(ns cljs.js
  (:import com.google.javascript.rhino.Node)
  (:import com.google.javascript.rhino.IR)
  (:import com.google.javascript.jscomp.CodePrinter$Builder))


;; Reflection voo-doo. This is only necessary until the entire emit phase is done.

(def :private Builder com.google.javascript.jscomp.CodePrinter$Builder)

(let [ctor (.getDeclaredConstructor Builder
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



(comment

(defn go [node]
  (print (to-source node)))

(go (IR/block [(IR/breakNode)
               (IR/continueNode)]))

)
