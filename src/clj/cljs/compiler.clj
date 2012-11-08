;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(set! *warn-on-reflection* true)

(ns cljs.compiler
  (:refer-clojure :exclude [munge macroexpand-1])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cljs.tagged-literals :as tags]
            [cljs.analyzer :as ana]
            [cljs.js :as js])
  (:import java.lang.StringBuilder))

(declare munge)

(def js-reserved
  #{"abstract" "boolean" "break" "byte" "case"
    "catch" "char" "class" "const" "continue"
    "debugger" "default" "delete" "do" "double"
    "else" "enum" "export" "extends" "final"
    "finally" "float" "for" "function" "goto" "if"
    "implements" "import" "in" "instanceof" "int"
    "interface" "let" "long" "native" "new"
    "package" "private" "protected" "public"
    "return" "short" "static" "super" "switch"
    "synchronized" "this" "throw" "throws"
    "transient" "try" "typeof" "var" "void"
    "volatile" "while" "with" "yield" "methods"})

(def ^:dynamic *position* nil)
(def ^:dynamic *provided* nil)
(def ^:dynamic *lexical-renames* {})
(def cljs-reserved-file-names #{"deps.cljs"})

(defonce ns-first-segments (atom '#{"cljs" "clojure"}))

(defn munge
  ([s] (munge s js-reserved))
  ([s reserved]
    (if (map? s)
      ; Unshadowing
      (let [{:keys [name field] :as info} s
            depth (loop [d 0, {:keys [shadow]} info]
                    (cond
                      shadow (recur (inc d) shadow)
                      (@ns-first-segments (str name)) (inc d)
                      :else d))
            renamed (*lexical-renames* (System/identityHashCode s))
            munged-name (munge (cond field (str "self__." name)
                                     renamed renamed
                                     :else name)
                               reserved)]
        (if (or field (zero? depth))
          munged-name
          (symbol (str munged-name "__$" depth))))
      ; String munging
      (let [ss (string/replace (str s) #"\/(.)" ".$1") ; Division is special
            ss (apply str (map #(if (reserved %) (str % "$") %)
                               (string/split ss #"(?<=\.)|(?=\.)")))
            ms (clojure.lang.Compiler/munge ss)]
        (if (symbol? s)
          (symbol ms)
          ms)))))

(defn- comma-sep [xs]
  (interpose "," xs))

(defmulti emit :op)

(defn emits [& xs]
  (doseq [x xs]
    (cond
      (nil? x) nil
      (map? x) (emit x)
      (seq? x) (apply emits x)
      (fn? x)  (x)
      :else (do
              (let [s (print-str x)]
                (when *position*
                  (swap! *position* (fn [[line column]]
                                      [line (+ column (count s))])))
                (print s)))))
  nil)

(defn emitln [& xs]
  (apply emits xs)
  ;; Prints column-aligned line number comments; good test of *position*.
  ;(when *position*
  ;  (let [[line column] @*position*]
  ;    (print (apply str (concat (repeat (- 120 column) \space) ["// " (inc line)])))))
  (println)
  (when *position*
    (swap! *position* (fn [[line column]]
                        [(inc line) 0])))
  nil)

(defn ^String emit-str [expr]
  (with-out-str (emit expr)))

(defn emit-provide [sym]
  (when-not (or (nil? *provided*) (contains? @*provided* sym))
    (swap! *provided* conj sym)
    (emitln "goog.provide('" (munge sym) "');")))

(defn emit-source [node]
  (emits (js/to-source node)))

(defmulti constant-node class)

(defmethod constant-node :default [x]
  (js/nodify x))

(defn stringify [x]
  (cond
    (keyword? x) (str \uFDD0 \'
                      (if (namespace x)
                        (str (namespace x) "/") "")
                      (name x))
    (symbol? x)  (str \uFDD1 \'
                      (if (namespace x)
                        (str (namespace x) "/") "")
                      (name x))
    :else (str x)))

(defmethod constant-node clojure.lang.Keyword [x]
  (js/string (stringify x)))

(defmethod constant-node clojure.lang.Symbol [x]
  (js/string (stringify x)))

(defn- wrap-meta [x node]
  (if (meta x)
    (js/call 'cljs.core.with_meta node (constant-node meta))
    node))

(defmethod constant-node clojure.lang.PersistentList$EmptyList [x]
  (wrap-meta x
    (js/nodify 'cljs.core.List.EMPTY)))

(defmethod constant-node clojure.lang.PersistentList [x]
  (wrap-meta x
    (js/apply 'cljs.core.list x)))

(defmethod constant-node clojure.lang.Cons [x]
  (wrap-meta x
    (js/apply 'cljs.core.list x)))

(defmethod constant-node clojure.lang.IPersistentVector [x]
  (wrap-meta x
    (js/call 'cljs.core.vec (apply js/array x))))

(defmethod constant-node clojure.lang.IPersistentMap [x]
  (wrap-meta x
    (js/apply 'cljs.core.hash_map (map constant-node (apply concat x)))))

(defmethod constant-node clojure.lang.PersistentHashSet [x]
  (wrap-meta x
    (js/call 'cljs.core.set (apply js/array (map constant-node x)))))

(defn emit-constant [x]
  (emit-source (constant-node x)))

(defn emit-block
  [statements ret]
  (when statements
    (emits statements))
  (emit ret))

(defn transpile-block [{:keys [statements ret]}]
  (js/block (if statements statements []) ret))

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (emits "return "))
     (let [x# (do ~@body)]
       (when (js/node? x#)
         (emit-source x#)))
     (when-not (= :expr (:context env#)) (emitln ";"))))

(defn transpile-wrap [env node]
  (if (= :return (:context env))
    (js/return node)
    node))

(defmulti transpile :op)

;;TODO: This is a dirty hack being in this file
(defmethod js/nodify :default [x]
  (cond
    (map? x) (transpile x)
    (seq? x) (map js/nodify x)
    :else x))

(defmethod emit :default [ast]
  (-> ast transpile emit-source))

(defmethod transpile :no-op [ast] (js/empty))

(defmethod transpile :var
  [{:keys [env info]}]
  (if (= :statement (:context env))
    (js/empty)
    (let [n (:name info)
          n (if (= (namespace n) "js")
              (name n)
              info)]
      (transpile-wrap env (js/name (munge n))))))

(defmethod transpile :meta
  [{:keys [env expr meta]}]
  (transpile-wrap env
    (js/call 'cljs.core.with_meta expr meta)))

(def ^:private array-map-threshold 16)
(def ^:private obj-map-threshold 32)

(defmethod transpile :map
  [{:keys [env simple-keys? keys vals]}]
  (transpile-wrap env
    (cond
      (zero? (count keys))
      (js/name 'cljs.core.ObjMap.EMPTY)

      (and simple-keys? (<= (count keys) obj-map-threshold))
      (let [keys* (map stringify (map :form keys))]
        (js/call 'cljs.core.ObjMap.fromObject
                 (apply js/array keys*)
                 (apply js/object (interleave keys* vals))))

      :else
      (js/call (if (<= (count keys) array-map-threshold)
                 'cljs.core.PersistentArrayMap.fromArrays
                 'cljs.core.PersistentHashMap.fromArrays)
               (apply js/array keys)
               (apply js/array vals)))))

(defmethod transpile :vector
  [{:keys [env items]}]
  (transpile-wrap env
    (if (empty? items)
      (js/name 'cljs.core.PersistentVector.EMPTY)
      (js/call 'cljs.core.PersistentVector.fromArray
               (apply js/array items)
               true))))

(defmethod transpile :set
  [{:keys [env items]}]
  (transpile-wrap env
    (if (empty? items)
      (js/name 'cljs.core.PersistentHashSet.EMPTY)
      (js/call 'cljs.core.PersistentHashSet.fromArray
               (apply js/array items)))))

(defmethod transpile :constant
  [{:keys [env form]}]
  (if (= :statement (:context env))
    (js/empty)
    (transpile-wrap env (constant-node form))))

(defn get-tag [e]
  (or (-> e :tag)
      (-> e :info :tag)))

(defn infer-tag [e]
  (if-let [tag (get-tag e)]
    tag
    (case (:op e)
      :let (infer-tag (:ret e))
      :if (let [then-tag (infer-tag (:then e))
                else-tag (infer-tag (:else e))]
            (when (= then-tag else-tag)
              then-tag))
      :constant (case (:form e)
                  true 'boolean
                  false 'boolean
                  nil)
      nil)))

(defn safe-test? [e]
  (let [tag (infer-tag e)]
    (or (#{'boolean 'seq} tag)
        (when (= (:op e) :constant)
          (let [form (:form e)]
            (not (or (and (string? form) (= form ""))
                     (and (number? form) (zero? form)))))))))

(defmethod transpile :if
  [{:keys [test then else env unchecked]}]
  (let [ifop (if (= :expr (:context env)) js/hook js/if)
        test* (if (not (or unchecked (safe-test? test)))
                (js/call 'cljs.core.truth_ test)
                test)]
    (if else
      (ifop test* then else)
      (ifop test* then))))

(defmethod transpile :throw
  [{:keys [throw env]}]
  (let [throw* (js/throw throw)]
    (if (= :expr (:context env))
      (js/scope throw*)
      throw*)))

(defmethod transpile :def
  [{:keys [name init env doc export]}]
  (if init
    (let [mname (munge name)
          assign (js/assign mname init)]
      (if export
        (js/comma assign
          (js/call 'goog.exportSymbol (js/string (str (munge export))) mname))
        assign))
    (js/empty)))

(defn- transpile-apply-to
  [{:keys [name params]}]
  (let [arglist (gensym "arglist__")
        delegate-name (symbol (str (munge name) "__delegate"))
        params (map munge params)]
    (js/lambda [arglist]
      (for [[i param] (map-indexed vector (butlast params))]
        (js/var param (js/call 'cljs.core.first
                               (nth (iterate #(js/call 'cljs.core.next %) arglist) i))))
      (if (< 1 (count params))
        (js/var (last params) (js/call 'cljs.core.rest
                                       (nth (iterate #(js/call 'cljs.core.next %) arglist)
                                            (- (count params) 2))))
        (js/var (last params) (js/call 'cljs.core.seq arglist)))
      (js/return (js/apply delegate-name params)))))

(defn transpile-fn-method
  [{:keys [type name params env recurs] :as ast}]
  (transpile-wrap env
    (js/function (js/name (munge name)) (map munge params)
      (if type (js/var 'self__ (js/this)) [])
      (let [body (transpile-block ast)]
        (if recurs
          (js/while true body (js/break))
          body)))))

(defn- transpile-variadic-fn-method
  [{:keys [type name variadic params env recurs max-fixed-arity] :as f}]
  (transpile-wrap env
    (let [name (or name (gensym))
          mname (munge name)
          params (map munge params)
          delegate-name (symbol (str mname "__delegate"))
          body (transpile-block f)]
      (js/scope
        (js/var delegate-name (js/lambda params
                                (if recurs
                                  (js/while true body (js/break))
                                  body)))
        (js/var mname (js/lambda (if variadic
                                   (concat (butlast params) ['var_args])
                                   params)
                        (if type (js/var 'self__ (js/this)) [])
                        (if variadic
                          [(js/var (last params) (js/null))
                           (js/if (js/call 'goog.isDef 'var_args)
                             (js/assign (last params)
                                        (js/call 'cljs.core.array_seq
                                                 (js/call 'Array.prototype.slice.call 'arguments (dec (count params)))
                                                 0)))]
                          [])
                        (js/return (js/apply (js/dot delegate-name 'call) (cons (js/this) params)))))
        (js/assign (symbol (str mname ".cljs$lang$maxFixedArity")) max-fixed-arity)
        (js/assign (symbol (str mname ".cljs$lang$applyTo")) (transpile-apply-to (assoc f :name name)))
        (js/assign (symbol (str mname ".cljs$lang$arity$variadic")) delegate-name)
        (js/return mname)))))

(defn- transpile-multi-arity-fn
  [{:keys [name env methods max-fixed-arity variadic]}]
  (let [has-name? (and name true)
        name (or name (gensym))
        mname (munge name)
        maxparams (map munge (apply max-key count (map :params methods)))
        mmap (into {}
               (map (fn [method]
                      [(munge (symbol (str mname "__" (count (:params method)))))
                       method])
                    methods))
        ms (sort-by #(-> % second :params count) (seq mmap))]
    (transpile-wrap env
      (js/scope
        (js/var mname (js/null))
        (for [[n meth] ms]
          (js/var n (if (:variadic meth)
                      (transpile-variadic-fn-method meth)
                      (transpile-fn-method meth))))
        (js/assign mname (js/lambda (if variadic
                                      (concat (butlast maxparams) ['var_args])
                                      maxparams)
                                    (if variadic (js/var (last maxparams) 'var_args) [])
                                    (js/switch 'arguments.length
                                      (for [[n meth] ms]
                                        (if (:variadic meth)
                                          (js/default
                                              (js/return (js/apply (js/dot n 'cljs$lang$arity$variadic)
                                                                   (concat (butlast maxparams)
                                                                           (if (> (count maxparams) 1)
                                                                             [(js/call 'cljs.core.array_seq 'arguments, max-fixed-arity)]
                                                                             [])))))
                                          (let [pcnt (count (:params meth))]
                                            (js/case pcnt
                                              (js/apply (js/dot n 'call)
                                                        (js/this)
                                                        (if (zero? pcnt)
                                                          [(js/null)]
                                                          (take pcnt maxparams))))))))
                                      (js/throw (js/new 'Error (js/+ "Invalid arity: " 'arguments.length)))))
        (if variadic
          [(js/assign (js/dot mname 'cljs$lang$maxFixedArity) max-fixed-arity)
           (js/assign (js/dot mname 'cljs$lang$applyTo) (js/dot (some #(let [[n m] %] (when (:variadic m) n)) ms) 'cljs$lang$applyTo))]
          [])
        (if has-name?
          (for [[n meth] ms]
            (let [c (count (:params meth))]
              (if (:variadic meth)
                (js/assign (js/dot mname 'cljs$lang$arity$variadic) (js/dot n 'cljs$lang$arity$variadic))
                (js/assign (js/dot mname (symbol (str "cljs$lang$arity$" c))) n))))
          [])
        (js/return mname)))))

(defmethod transpile :fn
  [{:keys [name env methods variadic recur-frames loop-lets] :as f}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (if (= :statement (:context env))
    (js/empty)
    (let [loop-locals (->> (concat (mapcat :params (filter #(and % @(:flag %)) recur-frames))
                                   (mapcat :params loop-lets))
                           (map munge)
                           seq)
          node (if (= 1 (count methods))
                 (if variadic
                   (transpile-variadic-fn-method (assoc (first methods) :name name))
                   (transpile-fn-method (assoc (first methods) :name name)))
                 (transpile-multi-arity-fn f))]
      (if loop-locals
        (transpile-wrap env
          (js/apply (js/lambda loop-locals node) loop-locals))
        node))))

(defmethod transpile :do
  [{:keys [env] :as ast}]
  (let [block (transpile-block ast)]
    (if (= :expr (:context env))
      (js/scope block)
      block)))

(defmethod transpile :try*
  [{:keys [env try catch name finally]}]
  (when finally
    (assert (not= :constant (-> finally :ret :op)) "finally block cannot contain constant"))
  (let [node (js/try (transpile-block try)
                     (when catch (js/catch (munge name) (transpile-block catch)))
                     (when finally (transpile-block finally)))]
    (if (= :expr (:context env))
      (js/scope node)
      node)))

(defmethod transpile :let
  [{:keys [bindings env loop] :as ast}]
  (let [context (:context env)]
    (binding [*lexical-renames* (into *lexical-renames*
                                      (when (= :statement context)
                                        (map #(vector (System/identityHashCode %)
                                                      (gensym (str (:name %) "-")))
                                             bindings)))]
      (let [vars (js/block (map (fn [{:keys [init] :as binding}]
                             (js/var (munge binding) init))
                           bindings))
            body (transpile-block ast)
            block (if loop
                   (js/while true
                     (js/block vars body (js/break)))
                   (js/block vars body))]
        (if (= :expr context)
          (js/scope block)
          block)))))

(defmethod transpile :recur
  [{:keys [frame exprs env]}]
  (let [temps (vec (take (count exprs) (repeatedly gensym)))
        params (map munge (:params frame))]
    (js/block (concat (map js/var temps exprs)
                      (map js/assign params temps))
              (js/continue))))

(defmethod transpile :letfn
  [{:keys [bindings env] :as ast}]
  (let [context (:context env)
        node (js/block
               (for [{:keys [init] :as binding} bindings]
                 (js/var (munge binding) init))
               (transpile-block ast))]
    (if (= :expr context)
      (js/scope node)
      node)))

(defn protocol-prefix [psym]
  (symbol (str (-> (str psym) (.replace \. \$) (.replace \/ \$)) "$")))

(def unary-ops
  {'- js/-, '+ js/+, 'delete js/delete, '! js/!}) ; omits ['~ js/bit-not]

(def binary-ops
  {'* js/*, '/ js/div, '% js/mod, '+ js/+, '- js/-,
   '< js/<, '> js/>, '<= js/<= '>= js/>=,
   '<< js/<<, '>> js/>>, '>>> js/>>>, '& js/&, '| js/|, '&& js/&&, '|| js/||
   'instanceof js/instanceof, 'in js/in, '== js/==, '!= js/!=, '=== 'js/===}) ; omits ^

;;TODO: xor and bitwise not
;; mod seems wierd too... these operator symbols seems to be a failed experiment...

(defmethod transpile :invoke
  [{:keys [f args env] :as expr}]
  (let [info (:info f)
        nm (:name info)
        sym (when nm (symbol (name nm)))
        fn? (and ana/*cljs-static-fns*
                 (not (:dynamic info))
                 (:fn-var info))
        protocol (:protocol info)
        proto? (let [tag (infer-tag (first (:args expr)))]
                 (and protocol tag
                      (or ana/*cljs-static-fns*
                          (:protocol-inline env))
                      (or (= protocol tag)
                          (when-let [ps (:protocols (ana/resolve-existing-var (dissoc env :locals) tag))]
                            (ps protocol)))))
        opt-not? (and (= (:name info) 'cljs.core/not)
                      (= (infer-tag (first (:args expr))) 'boolean))
        ns (:ns info)
        js? (= ns 'js)
        goog? (when ns
                (or (= ns 'goog)
                    (when-let [ns-str (str ns)]
                      (= (get (string/split ns-str #"\.") 0 nil) "goog"))))
        keyword? (and (= (-> f :op) :constant)
                      (keyword? (-> f :form)))
        [f variadic-invoke]
        (if fn?
          (let [arity (count args)
                variadic? (:variadic info)
                mps (:method-params info)
                mfa (:max-fixed-arity info)]
            (cond
             ;; if only one method, no renaming needed
             (and (not variadic?)
                  (= (count mps) 1))
             [f nil]

             ;; direct dispatch to variadic case
             (and variadic? (> arity mfa))
             [(update-in f [:info :name]
                             (fn [name] (symbol (str (munge name) ".cljs$lang$arity$variadic"))))
              {:max-fixed-arity mfa}]

             ;; direct dispatch to specific arity case
             :else
             (let [arities (map count mps)]
               (if (some #{arity} arities)
                 [(update-in f [:info :name]
                             (fn [name] (symbol (str (munge name) ".cljs$lang$arity$" arity)))) nil]
                 [f nil]))))
          [f nil])]
    (transpile-wrap env
      (cond
       opt-not?
       (js/! (first args))

       proto?
       (let [pimpl (str (munge (protocol-prefix protocol))
                        (munge (name (:name info))) "$arity$" (count args))]
         (js/apply (js/dot (first args) (js/name pimpl)) args))

       keyword?
       (js/apply (js/dot (js/new 'cljs.core.Keyword f) 'call) nil args)

       variadic-invoke
       (let [[fixed-args variable-args] (split-at (:max-fixed-arity variadic-invoke) args)]
         (js/apply f (conj (mapv transpile fixed-args)
                           (js/call 'cljs.core.array_seq (apply js/array variable-args) 0))))

       (and js? (= (count args) 1) (unary-ops sym))
       (apply (unary-ops sym) args)

       (and js? (= (count args) 2) (binary-ops sym))
       (apply (binary-ops sym) args)

       (and js? (= (count args) 3) (= sym '?))
       (apply js/hook args)

       (or fn? js? goog?)
       (js/apply f args)

       :else
       (if (and ana/*cljs-static-fns* (= (:op f) :var))
         (let [fprop (js/dot f (js/name (str "cljs$lang$arity$" (count args))))]
           (js/hook fprop
             (js/apply fprop args)
             (js/apply (js/dot fprop 'call) nil args)))
         (js/apply (js/dot f 'call) nil args))))))

(defmethod transpile :new
  [{:keys [env ctor args]}]
  (transpile-wrap env
    (apply js/new ctor args)))

(defmethod transpile :set!
  [{:keys [env target val]}]
  (transpile-wrap env
    (js/assign target val)))

(defmethod transpile :ns
  [{:keys [name requires uses requires-macros]}]
  (swap! ns-first-segments conj (first (string/split (str name) #"\.")))
  (js/block
    (js/call 'goog.provide (munge (str name)))
    (for [lib (distinct (concat (when (not= name 'cljs.core) ['cljs.core])
                                (vals requires) (vals uses)))]
      (js/call 'goog.require (munge (str lib))))))

(defn provide! [sym]
  (if (or (nil? *provided*) (contains? @*provided* sym))
    []
    (do
      (swap! *provided* conj sym)
      (js/call 'goog.provide (munge sym)))))

(defmethod transpile :deftype*
  [{:keys [t fields pmasks]}]
  (let [fields (map munge fields)]
    (provide! t)
    ;TODO: JSType annotations
    ;(emitln "/**")
    ;(emitln "* @constructor")
    ;(emitln "*/")
    (js/assign (munge t) (js/lambda fields
      (for [fld fields]
        (js/assign (js/dot (js/this) fld) fld))
      (for [[pno pmask] pmasks]
        (js/assign (symbol (str "this.cljs$lang$protocol_mask$partition" pno "$")) pmask))))))

(defmethod transpile :defrecord*
  [{:keys [t fields pmasks]}]
  (let [fields (concat (map munge fields) '[__meta __extmap])]
    (provide! t)
    ;TODO: JSType annotations
    ;(emitln "/**")
    ;(emitln "* @constructor")
    ;(doseq [fld fields]
    ;  (emitln "* @param {*} " fld))
    ;(emitln "* @param {*=} __meta ")
    ;(emitln "* @param {*=} __extmap")
    ;(emitln "*/")
    (js/assign (munge t) (js/lambda fields
      (for [fld fields]
        (js/assign (js/dot (js/this) fld) fld))
      (for [[pno pmask] pmasks]
        (js/assign (symbol (str "this.cljs$lang$protocol_mask$partition" pno "$") pmask)))
      (js/if (js/> 'arguments.length (- (count fields) 2))
        (js/block
          (js/assign 'this.__meta '__meta)
          (js/assign 'this.__extmap '__extmap))
        (js/block
          (js/assign 'this.__meta (js/null))
          (js/assign 'this.__extmap (js/null))))))))

(defmethod transpile :dot
  [{:keys [env target field method args]}]
  (transpile-wrap env
    (let [dot (js/dot target (munge (or field method) #{}))]
      (if field dot (js/call dot)))))

(defmethod transpile :js
  [{:keys [env jsop args]}]
  (transpile-wrap env
    (apply (find-var (symbol "cljs.js" (str jsop))) args)))

(defn forms-seq
  "Seq of forms in a Clojure or ClojureScript file."
  ([f]
     (forms-seq f (clojure.lang.LineNumberingPushbackReader. (io/reader f))))
  ([f ^java.io.PushbackReader rdr]
     (if-let [form (binding [*ns* ana/*reader-ns*] (read rdr nil nil))]
       (lazy-seq (cons form (forms-seq f rdr)))
       (.close rdr))))

(defn rename-to-js
  "Change the file extension from .cljs to .js. Takes a File or a
  String. Always returns a String."
  [file-str]
  (clojure.string/replace file-str #"\.cljs$" ".js"))

(defn mkdirs
  "Create all parent directories for the passed file."
  [^java.io.File f]
  (.mkdirs (.getParentFile (.getCanonicalFile f))))

(defmacro with-core-cljs
  "Ensure that core.cljs has been loaded."
  [& body]
  `(do (when-not (:defs (get @ana/namespaces 'cljs.core))
         (ana/analyze-file "cljs/core.cljs"))
       ~@body))

(defn compile-file* [src dest]
  (with-core-cljs
    (with-open [out ^java.io.Writer (io/make-writer dest {})]
      (binding [*out* out
                ana/*cljs-ns* 'cljs.user
                ana/*cljs-file* (.getPath ^java.io.File src)
                *data-readers* tags/*cljs-data-readers*
                *position* (atom [0 0])
                *provided* (atom #{})]
        (loop [forms (forms-seq src)
               ns-name nil
               deps nil]
          (if (seq forms)
            (let [env (ana/empty-env)
                  ast (ana/analyze env (first forms))]
              (do (emit ast)
                  (if (= (:op ast) :ns)
                    (recur (rest forms) (:name ast) (merge (:uses ast) (:requires ast)))
                    (recur (rest forms) ns-name deps))))
            {:ns (or ns-name 'cljs.user)
             :provides [ns-name]
             :requires (if (= ns-name 'cljs.core) (set (vals deps)) (conj (set (vals deps)) 'cljs.core))
             :file dest}))))))

(defn requires-compilation?
  "Return true if the src file requires compilation."
  [^java.io.File src ^java.io.File dest]
  (or (not (.exists dest))
      (> (.lastModified src) (.lastModified dest))))

(defn compile-file
  "Compiles src to a file of the same name, but with a .js extension,
   in the src file's directory.

   With dest argument, write file to provided location. If the dest
   argument is a file outside the source tree, missing parent
   directories will be created. The src file will only be compiled if
   the dest file has an older modification time.

   Both src and dest may be either a String or a File.

   Returns a map containing {:ns .. :provides .. :requires .. :file ..}.
   If the file was not compiled returns only {:file ...}"
  ([src]
     (let [dest (rename-to-js src)]
       (compile-file src dest)))
  ([src dest]
     (let [src-file (io/file src)
           dest-file (io/file dest)]
       (if (.exists src-file)
         (if (requires-compilation? src-file dest-file)
           (do (mkdirs dest-file)
               (compile-file* src-file dest-file))
           {:file dest-file})
         (throw (java.io.FileNotFoundException. (str "The file " src " does not exist.")))))))

(comment
  ;; flex compile-file
  (do
    (compile-file "/tmp/hello.cljs" "/tmp/something.js")
    (slurp "/tmp/hello.js")

    (compile-file "/tmp/somescript.cljs")
    (slurp "/tmp/somescript.js")))

(defn path-seq
  [file-str]
  (->> java.io.File/separator
       java.util.regex.Pattern/quote
       re-pattern
       (string/split file-str)))

(defn to-path
  ([parts]
     (to-path parts java.io.File/separator))
  ([parts sep]
     (apply str (interpose sep parts))))

(defn to-target-file
  "Given the source root directory, the output target directory and
  file under the source root, produce the target file."
  [^java.io.File dir ^String target ^java.io.File file]
  (let [dir-path (path-seq (.getAbsolutePath dir))
        file-path (path-seq (.getAbsolutePath file))
        relative-path (drop (count dir-path) file-path)
        parents (butlast relative-path)
        parent-file (java.io.File. ^String (to-path (cons target parents)))]
    (java.io.File. parent-file ^String (rename-to-js (last relative-path)))))

(defn cljs-files-in
  "Return a sequence of all .cljs files in the given directory."
  [dir]
  (filter #(let [name (.getName ^java.io.File %)]
             (and (.endsWith name ".cljs")
                  (not= \. (first name))
                  (not (contains? cljs-reserved-file-names name))))
          (file-seq dir)))

(defn compile-root
  "Looks recursively in src-dir for .cljs files and compiles them to
   .js files. If target-dir is provided, output will go into this
   directory mirroring the source directory structure. Returns a list
   of maps containing information about each file which was compiled
   in dependency order."
  ([src-dir]
     (compile-root src-dir "out"))
  ([src-dir target-dir]
     (let [src-dir-file (io/file src-dir)]
       (loop [cljs-files (cljs-files-in src-dir-file)
              output-files []]
         (if (seq cljs-files)
           (let [cljs-file (first cljs-files)
                 output-file ^java.io.File (to-target-file src-dir-file target-dir cljs-file)
                 ns-info (compile-file cljs-file output-file)]
             (recur (rest cljs-files) (conj output-files (assoc ns-info :file-name (.getPath output-file)))))
           output-files)))))

(comment
  ;; compile-root
  ;; If you have a standard project layout with all file in src
  (compile-root "src")
  ;; will produce a mirrored directory structure under "out" but all
  ;; files will be compiled to js.
  )

(comment

;;the new way - use the REPL!!
(require '[cljs.compiler :as comp])
(def repl-env (comp/repl-env))
(comp/repl repl-env)
;having problems?, try verbose mode
(comp/repl repl-env :verbose true)
;don't forget to check for uses of undeclared vars
(comp/repl repl-env :warn-on-undeclared true)

(test-stuff)
(+ 1 2 3)
([ 1 2 3 4] 2)
({:a 1 :b 2} :a)
({1 1 2 2} 1)
(#{1 2 3} 2)
(:b {:a 1 :b 2})
('b '{:a 1 b 2})

(extend-type number ISeq (-seq [x] x))
(seq 42)
;(aset cljs.core.ISeq "number" true)
;(aget cljs.core.ISeq "number")
(satisfies? ISeq 42)
(extend-type nil ISeq (-seq [x] x))
(satisfies? ISeq nil)
(seq nil)

(extend-type default ISeq (-seq [x] x))
(satisfies? ISeq true)
(seq true)

(test-stuff)

(array-seq [])
(defn f [& etc] etc)
(f)

(in-ns 'cljs.core)
;;hack on core


(deftype Foo [a] IMeta (-meta [_] (fn [] a)))
((-meta (Foo. 42)))

;;OLD way, don't you want to use the REPL?
(in-ns 'cljs.compiler)
(import '[javax.script ScriptEngineManager])
(def jse (-> (ScriptEngineManager.) (.getEngineByName "JavaScript")))
(.eval jse cljs.compiler/bootjs)
(def envx {:ns (@namespaces 'cljs.user) :context :expr :locals '{ethel {:name ethel__123 :init nil}}})
(analyze envx nil)
(analyze envx 42)
(analyze envx "foo")
(analyze envx 'fred)
(analyze envx 'fred.x)
(analyze envx 'ethel)
(analyze envx 'ethel.x)
(analyze envx 'my.ns/fred)
(analyze envx 'your.ns.fred)
(analyze envx '(if test then else))
(analyze envx '(if test then))
(analyze envx '(and fred ethel))
(analyze (assoc envx :context :statement) '(def test "fortytwo" 42))
(analyze (assoc envx :context :expr) '(fn* ^{::fields [a b c]} [x y] a y x))
(analyze (assoc envx :context :statement) '(let* [a 1 b 2] a))
(analyze (assoc envx :context :statement) '(defprotocol P (bar [a]) (baz [b c])))
(analyze (assoc envx :context :statement) '(. x y))
(analyze envx '(fn foo [x] (let [x 42] (js* "~{x}['foobar']"))))

(analyze envx '(ns fred (:require [your.ns :as yn]) (:require-macros [clojure.core :as core])))
(defmacro js [form]
  `(emit (ana/analyze {:ns (@ana/namespaces 'cljs.user) :context :statement :locals {}} '~form)))

(defn jscapture [form]
  "just grabs the js, doesn't print it"
  (with-out-str
    (emit (analyze {:ns (@namespaces 'cljs.user) :context :expr :locals {}} form))))

(defn jseval [form]
  (let [js (jscapture form)]
    ;;(prn js)
    (.eval jse (str "print(" js ")"))))

;; from closure.clj
(optimize (jscapture '(defn foo [x y] (if true 46 (recur 1 x)))))

(js (if a b c))
(js (def x 42))
(js (defn foo [a b] a))
(js (do 1 2 3))
(js (let [a 1 b 2 a b] a))

(js (ns fred (:require [your.ns :as yn]) (:require-macros [cljs.core :as core])))

(js (def foo? (fn* ^{::fields [a? b c]} [x y] (if true a? (recur 1 x)))))
(js (def foo (fn* ^{::fields [a b c]} [x y] (if true a (recur 1 x)))))
(js (defn foo [x y] (if true x y)))
(jseval '(defn foo [x y] (if true x y)))
(js (defn foo [x y] (if true 46 (recur 1 x))))
(jseval '(defn foo [x y] (if true 46 (recur 1 x))))
(jseval '(foo 1 2))
(js (and fred ethel))
(jseval '(ns fred (:require [your.ns :as yn]) (:require-macros [cljs.core :as core])))
(js (def x 42))
(jseval '(def x 42))
(jseval 'x)
(jseval '(if 42 1 2))
(jseval '(or 1 2))
(jseval '(fn* [x y] (if true 46 (recur 1 x))))
(.eval jse "print(test)")
(.eval jse "print(cljs.user.Foo)")
(.eval jse  "print(cljs.user.Foo = function (){\n}\n)")
(js (def fred 42))
(js (deftype* Foo [a b-foo c]))
(jseval '(deftype* Foo [a b-foo c]))
(jseval '(. (new Foo 1 2 3) b-foo))
(js (. (new Foo 1 2 3) b))
(.eval jse "print(new cljs.user.Foo(1, 42, 3).b)")
(.eval jse "(function (x, ys){return Array.prototype.slice.call(arguments, 1);})(1,2)[0]")

(macroexpand-1 '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] b) (ethel [x] c) Ethel (foo [] d)))
(-> (macroexpand-1 '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] b) (ethel [x] c) Ethel (foo [] d)))
    last last last first meta)

(macroexpand-1 '(cljs.core/extend-type Foo Fred (fred ([x] a) ([x y] b)) (ethel ([x] c)) Ethel (foo ([] d))))
(js (new foo.Bar 65))
(js (defprotocol P (bar [a]) (baz [b c])))
(js (. x y))
(js (. "fred" (y)))
(js (. x y 42 43))
(js (.. a b c d))
(js (. x (y 42 43)))
(js (fn [x] x))
(js (fn ([t] t) ([x y] y) ([ a b & zs] b)))

(js (. (fn foo ([t] t) ([x y] y) ([a b & zs] b)) call nil 1 2))
(js (fn foo
      ([t] t)
      ([x y] y)
      ([ a b & zs] b)))

(js ((fn foo
       ([t] (foo t nil))
       ([x y] y)
       ([ a b & zs] b)) 1 2 3))


(jseval '((fn foo ([t] t) ([x y] y) ([ a b & zs] zs)) 12 13 14 15))

(js (defn foo [this] this))

(js (defn foo [a b c & ys] ys))
(js ((fn [x & ys] ys) 1 2 3 4))
(jseval '((fn [x & ys] ys) 1 2 3 4))
(js (cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] a)  (ethel [x] c) Ethel (foo [] d)))
(jseval '(cljs.core/deftype Foo [a b c] Fred (fred [x] a) (fred [x y] a)  (ethel [x] c) Ethel (foo [] d)))

(js (do
           (defprotocol Proto (foo [this]))
           (deftype Type [a] Proto (foo [this] a))
           (foo (new Type 42))))

(jseval '(do
           (defprotocol P-roto (foo? [this]))
           (deftype T-ype [a] P-roto (foo? [this] a))
           (foo? (new T-ype 42))))

(js (def x (fn foo [x] (let [x 42] (js* "~{x}['foobar']")))))
(js (let [a 1 b 2 a b] a))

(doseq [e '[nil true false 42 "fred" fred ethel my.ns/fred your.ns.fred
            (if test then "fooelse")
            (def x 45)
            (do x y y)
            (fn* [x y] x y x)
            (fn* [x y] (if true 46 (recur 1 x)))
            (let* [a 1 b 2 a a] a b)
            (do "do1")
            (loop* [x 1 y 2] (if true 42 (do (recur 43 44))))
            (my.foo 1 2 3)
            (let* [a 1 b 2 c 3] (set! y.s.d b) (new fred.Ethel a b c))
            (let [x (do 1 2 3)] x)
            ]]
  (->> e (analyze envx) emit)
  (newline)))
