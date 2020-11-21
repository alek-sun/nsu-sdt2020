(ns clojurelabs.task3-1
  (:use [clojure.test]))


(defn local-partition
  "Check if collection non-empty and drop it by chunk-size"
  ([chunk-size c]
   (when-let [s (seq c)]
       (cons (take chunk-size s) (local-partition chunk-size (drop chunk-size s)))
       )))

(defn partition-filter
  "Consequent filter on partition results"
  [cond coll chunk]
  (->>
    coll
    (local-partition chunk)
    (mapcat #(filter cond %))
    (doall)))

(defn parallel-filter
  "Parallel filter on partition results"
  [cond coll chunk]
  (->>
    coll
    (local-partition chunk)
    (map #(future (doall (filter cond %))))
    (doall)
    (mapcat deref)
))


(defn condition
  "Heavy calculations..."
  ([x]
   (Thread/sleep 1000)
   (even? x)))


(deftest local-partition-test
  (is (= '((0 1 2 3) (4 5 6 7) (8 9)) (local-partition 4 (range 10)))))

(deftest my-parallel-filter-test
  (is (= '(0 2 4 6 8) (parallel-filter condition (range 10) 3))))


(defn -main
  []
  (local-partition-test)
  (my-parallel-filter-test)

  (println "Common filter ")
  (time (doall (filter condition (range 10))))

  (println "Common filter + partition")
  (time (partition-filter condition (range 10) 1))

  (println "Parallel filter ")
  (time (parallel-filter condition (range 10) 1))
  (time (parallel-filter condition (range 10) 5))
  )