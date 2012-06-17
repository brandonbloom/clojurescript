(require '[clojure.java.io :as io])

(defn load-benchmark-results [path]
  (with-open [rdr (io/reader path)]
    (loop [engine nil
           results {}
           lines (line-seq rdr)]
      (if-let [line (first lines)]
        (recur (if-let [[_ engine'] (re-find #"Benchmarking with (.*)" line)]
                 engine' engine)
               (if-let [[_ bindings expr iterations duration] (re-find #"(.*), (.*), (\d+) runs, (\d+) msecs" line)]
                 (assoc results {:engine engine :bindings (read-string bindings)
                                 :expr (read-string expr) :iterations (read-string iterations)}
                        (read-string duration))
                 results)
               (next lines))
        results))))

(defn compare-benchmarks [before-path after-path]
  (let [before (load-benchmark-results before-path)
        after (load-benchmark-results after-path)
        together (into {} (for [[benchmark before-duration] before
                                :let [after-duration (get after benchmark)]
                                :when after-duration]
                            (let [difference (- before-duration after-duration)]
                              [benchmark {:before before-duration
                                          :after after-duration
                                          :difference difference
                                          :change (float (/ difference (max before-duration 1)))}])))]
    (doseq [[{:keys [engine bindings expr] :as benchmark}
             {:keys [change before after difference] :as result}]
            (sort-by #(-> % second :change Math/abs) together)]
      (println (str (format "%10.2f" (* change 100)) "%   "
                    (format "%5d" before) "ms  -> " (format "%5d" after) "ms   "
                    bindings " " expr " " engine)))))

(compare-benchmarks "./bench-master" "./bench-hashed")
