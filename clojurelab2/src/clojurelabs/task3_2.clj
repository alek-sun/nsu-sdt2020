(ns clojurelabs.task3-2
  (:use [clojure.test]))


(defn local-partition
  "Check if collection non-empty and drop it by chunk-size and wrap it in lazy seq"
  ([chunk-size c]
   (lazy-seq
     (when-let [s (seq c)]
       (cons (take chunk-size s) (local-partition chunk-size (drop chunk-size s)))
       ))))

(defn execute-task
  "execute filter on part of collection and return lazy sequence of result"
  [cond c]
    (->>
      c
      (map #(future (doall (filter cond %))))
      (doall)
      (map deref)
      (lazy-seq)
    ))

(defn parallel-filter-lazy
  "drop collection by 'chunk' size and then by 'task' size for parallel future execution
  after that concatenate it and remove empty (filtered) elements"
  [cond coll chunk task]
  (->>
    coll
    (local-partition chunk)
    (local-partition task)
    (map #(execute-task cond %))
    (mapcat identity)
    (mapcat identity)
    )
  )


(defn condition
  "Heavy calculations..."
  ([x]
   (Thread/sleep 200)
   (even? x)))


(deftest local-partition-test
  (is (= '((0 1 2 3) (4 5 6 7) (8 9)) (local-partition 4 (range 10)))))

(deftest parallel-filter-lazy-test
  (is (= '(0 2 4 6 8) (take 5 (parallel-filter-lazy condition (range) 5 2)))))


(defn -main
  []
  (local-partition-test)
  (parallel-filter-lazy-test)

  (println "Parallel lazy filter ")
  (time (parallel-filter-lazy condition (range 20) 5 1))
  (time (take 10 (parallel-filter-lazy condition (range) 5 1)))
  (time (take 10 (parallel-filter-lazy condition (range) 5 2)))
  (time (take 10 (parallel-filter-lazy condition (range) 5 4)))
  )
