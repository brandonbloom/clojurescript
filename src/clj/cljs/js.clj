(ns cljs.js
  (:refer-clojure :exclude (name boolean empty while apply case
                            + - * / mod > >= == <= <))
  (:import [com.google.javascript.rhino Token Node IR])
  (:import com.google.javascript.jscomp.CodePrinter$Builder))

;; Reflection voo-doo. This is only necessary until the entire emit phase is done.

(let [ctor (.getDeclaredConstructor
             com.google.javascript.jscomp.CodePrinter$Builder
             (into-array [com.google.javascript.rhino.Node]))]
  (.setAccessible ctor true)
  (defn- printer-builder [node]
    (.newInstance ctor (into-array [node]))))

(defn- call-private* [obj method-name & argpairs]
  (let [classes (into-array Class (take-nth 2 argpairs))
        args (into-array (take-nth 2 (next argpairs)))
        method (doto (.getDeclaredMethod
                       (if (instance? Class obj) obj (class obj))
                       (clojure.core/name method-name)
                       classes)
                 (.setAccessible true))]
    (.invoke method obj args)))

(defmacro call-private [obj method-name & argpairs]
  `(call-private* ~obj '~method-name ~@argpairs))

(defn to-source [node]
  (-> (printer-builder node)
    (call-private setOutputTypes Boolean/TYPE true)
    (call-private build)))

(defn- statement? [node]
  (and (instance? Node node)
       (clojure.core/boolean (call-private IR mayBeStatement Node node))))

;; Begin reasonable interface

(defn node? [x]
  (instance? Node x))

(defmulti nodify class)

(defn statementize [x]
  (let [node (nodify x)]
    (if (statement? node)
      node
      (IR/exprResult node))))

(defn empty []
  (IR/empty))

(defn null []
  (IR/nullNode))

(defn this []
  (IR/thisNode))

(defmethod nodify nil [x]
  (null))

;; Can't name a function 'true :-(
(defn yes []
  (IR/trueNode))

;; or 'false :-(
(defn no []
  (IR/falseNode))

(defn boolean [x]
  (if x (yes) (no)))

(defmethod nodify Boolean [x]
  (boolean x))

(defn number [x]
  (IR/number (double x)))

(defmethod nodify Long [x] (number x))
(defmethod nodify Integer [x] (number x))
(defmethod nodify Double [x] (number x))

(defn string
  ([x] (IR/string x))
  ([x & ys] (string (clojure.core/apply str x ys))))

(defmethod nodify String [x]
  (string x))

(defmethod nodify Character [x]
  (string (str x)))

(defn regexp
  ([pattern]
    (if (instance? java.util.regex.Pattern pattern)
      (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" (str pattern))]
        (if (empty? flags)
          (regexp pattern)
          (regexp pattern flags)))
      (IR/regexp (string pattern))))
  ([pattern flags]
   (IR/regexp (string pattern) (string flags))))

(defmethod nodify java.util.regex.Pattern [x]
  (regexp x))

(defn dot [target prop]
  (let [prop* (if (symbol? prop) (str prop) prop)]
    (IR/getprop (nodify target) (nodify prop*))))

(defn name [x]
  (loop [[obj & props] (clojure.string/split (str x) #"\.")
         node (if (= obj 'this) (this) (IR/name obj))]
    (if props
      (recur props (dot node (IR/string (first props))))
      node)))

(defmethod nodify clojure.lang.Symbol [x]
  (name x))

(defn call [target & args]
  (IR/call (nodify target) (into-array Node (map nodify args))))

(defn apply [target & args]
  (clojure.core/apply clojure.core/apply call target args))

(defn array [& elements]
  (IR/arraylit (into-array Node (map nodify elements))))

(defn object [& keyvals]
  (let [propdefs (->> (partition 2 keyvals)
                      (map (fn [[k v]]
                             (IR/propdef (IR/stringKey (str k)) (nodify v)))))]
    (IR/objectlit (into-array Node propdefs))))

(defn block [& statements]
  (IR/block (into-array Node (map statementize (flatten statements)))))

(defn if
  ([test then]
   (IR/ifNode (nodify test) (block then)))
  ([test then else]
   (IR/ifNode (nodify test) (block then) (block else))))

(defn hook
  ([test then] (hook test then nil))
  ([test then else]
   (IR/hook (nodify test) (nodify then) (nodify else))))

(defn switch [x & cases]
  (IR/switchNode (nodify x) (into-array Node (flatten cases))))

(defn case [x & body]
  (IR/caseNode (nodify x) (block body)))

(defn default [& body]
  (IR/defaultCase (block body)))

(defn throw [x]
  (IR/throwNode (nodify x)))

(defn param-list [params]
  (IR/paramList (into-array Node (map nodify params))))

(defn function [name params & body]
  (IR/function (nodify name)
               (if (and (node? params) (.isParamList params))
                 params
                 (param-list params))
               (block body)))

(defn lambda [params & body]
  (clojure.core/apply function (name "") params body))

(defn scope [& body]
  (call (clojure.core/apply lambda [] body)))

(defn assign [target value]
  (IR/assign (nodify target) (nodify value)))

(defn comma [left right]
  (IR/comma (nodify left) (nodify right)))

(defn new [ctor & args]
  (IR/newNode (nodify ctor) (into-array Node (map nodify args))))

(defn return
  ([] (IR/returnNode))
  ([x] (IR/returnNode (nodify x))))

(defn break []
  (IR/breakNode))

(defn continue []
  (IR/continueNode))

;;TODO: Why doesn't IR have a whileNode method?
(defn while [test & body]
  (Node. Token/WHILE (nodify test) (block body)))

(defn var
  ([name]
   (IR/var (nodify name)))
  ([name val]
   (IR/var (nodify name) (nodify val))))

(defn try [try catch finally]
  (cond
    (and catch finally) (IR/tryCatchFinally try catch finally)
    catch (IR/tryCatch try catch)
    finally (IR/tryFinally try finally)
    :else try))

(defn catch [name & body]
  (IR/catchNode (nodify name) (block body)))

(defn delete [x]
  (Node. Token/DELPROP (nodify x)))

(defn ! [x]
  (IR/not (nodify x)))

(defn +
  ([x] (IR/pos (nodify x)))
  ([x y] (IR/add (nodify x) (nodify y))))

(defn -
  ([x] (IR/neg (nodify x)))
  ([x y] (IR/sub (nodify x) (nodify y))))

(defn * [x y]
  (Node. Token/MUL (nodify x) (nodify y)))

;;TODO: Clojure 1.5 will allow /
(defn div [x y]
  (Node. Token/DIV (nodify x) (nodify y)))

(defn mod [x y]
  (Node. Token/MOD (nodify x) (nodify y)))

(defn == [x y]
  (IR/eq (nodify x) (nodify y)))

(defn === [x y]
  (IR/sheq (nodify x) (nodify y)))

(defn != [x y]
  (Node. Token/NE (nodify x) (nodify y)))

(defn !== [x y]
  (Node. Token/SHNE (nodify x) (nodify y)))

(defn > [x y]
  (Node. Token/LT (nodify x) (nodify y)))

(defn >= [x y]
  (Node. Token/LE (nodify x) (nodify y)))

(defn <= [x y]
  (Node. Token/GE (nodify x) (nodify y)))

(defn < [x y]
  (Node. Token/GT (nodify x) (nodify y)))

(defn instanceof [x c]
  (Node. Token/INSTANCEOF (nodify x) (nodify c)))

(defn in [x c]
  (Node. Token/IN (nodify x) (nodify c)))

(defn | [x y]
  (Node. Token/BITOR (nodify x) (nodify y)))

(defn || [x y]
  (IR/or (nodify x) (nodify y)))

(defn & [x y]
  (Node. Token/BITAND (nodify x) (nodify y)))

(defn && [x y]
  (IR/and (nodify x) (nodify y)))

(defn << [x y]
  (Node. Token/LSH (nodify x) (nodify y)))

(defn >> [x y]
  (Node. Token/RSH (nodify x) (nodify y)))

(defn >>> [x y]
  (Node. Token/URSH (nodify x) (nodify y)))
