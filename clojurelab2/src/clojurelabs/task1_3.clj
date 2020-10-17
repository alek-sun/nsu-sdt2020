(ns clojurelabs.task1_3)

(defn my-map [fun coll]
  (reduce (fn [last-result it] (concat last-result [(fun it)])) (conj coll []))
  )

(defn my-filter [fun coll]
  (reduce (fn [last-result it] (if (fun it) (concat last-result [it]) last-result)) (conj coll []))
  )

(defn -main [& args]
  (println (my-map dec '(1 2 3 4 5 6)))
  (println (my-filter (fn [x]
                        (= (mod x 2) 0)) '(1 2 3 4 5 6)))
  )

