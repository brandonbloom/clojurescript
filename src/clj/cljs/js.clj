(ns cljs.js
  (:refer-clojure :exclude (name boolean))
  (:import com.google.javascript.rhino.Node)
  (:import com.google.javascript.rhino.IR)
  (:import com.google.javascript.jscomp.CodePrinter$Builder))

;; Reflection voo-doo. This is only necessary until the entire emit phase is done.

(let [ctor (.getDeclaredConstructor
             com.google.javascript.jscomp.CodePrinter$Builder
             (into-array [com.google.javascript.rhino.Node]))]
  (.setAccessible ctor true)
  (defn- printer-builder [node]
    (.newInstance ctor (into-array [node]))))

(defn- unboxed-class [obj]
  (cond
    (instance? Boolean obj) Boolean/TYPE
    ;;TODO: Other primitives
    :else (class obj)))

(defn- call-private* [obj method-name & args]
  (let [method (doto (.getDeclaredMethod
                       (class obj)
                       (clojure.core/name method-name)
                       (into-array Class (map unboxed-class args)))
                 (.setAccessible true))]
    (.invoke method obj (into-array args))))

(defmacro call-private [obj method-name & args]
  `(call-private* ~obj '~method-name ~@args))

(defn to-source [node]
  (-> (printer-builder node)
    (call-private setPrettyPrint true)
    (call-private build)))


;; Begin reasonable interface

(defn node? [x]
  (instance? Node x))

(defmulti nodify class)

(defmethod nodify :default [x] x)

(defn null []
  (IR/nullNode))

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
  ([x & ys] (string (apply str x ys))))

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

(defn name [x]
  (loop [[obj & props] (clojure.string/split (str x) #"\.")
         node (IR/name obj)]
    (if props
      (recur props (IR/getprop node (IR/string (first props))))
      node)))

(defmethod nodify clojure.lang.Symbol [x]
  (name x))

(defn call [target & args]
  (IR/call (nodify target) (into-array Node (map nodify args))))

(defn array [& elements]
  (IR/arraylit (into-array Node (map nodify elements))))


(comment

(defn go [node]
  (print (to-source node)))

(go (IR/block [(IR/breakNode)
               (IR/continueNode)]))

)
