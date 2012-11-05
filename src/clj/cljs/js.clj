(ns cljs.js
  (:refer-clojure :exclude (boolean))
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
                       (name method-name)
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

(defn null []
  (IR/nullNode))

;; Can't name a function 'true :-(
(defn yes []
  (IR/trueNode))

;; or 'false :-(
(defn no []
  (IR/falseNode))

(defn boolean [x]
  (if x (yes) (no)))

(defn number [x]
  (IR/number (double x)))

(defn string [x]
  (IR/string x))


(comment

(defn go [node]
  (print (to-source node)))

(go (IR/block [(IR/breakNode)
               (IR/continueNode)]))

)
