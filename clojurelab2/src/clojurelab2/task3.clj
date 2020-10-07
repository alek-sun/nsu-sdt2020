(ns clojurelab2.task3)

(defn my-map [fun coll]
  (reduce (fn [last-result it] (concat last-result (list (fun it)))) (cons '() coll))
  )

(defn my-filter [fun coll]
  (reduce (fn [last-result it] (if (fun it) (concat last-result (list it)))) (cons '() coll))
  )

(defn -main [& args]
  (println (my-map dec '(1 2 3 4 5 6)))
  (println (my-filter (fn [x]
                        (>= x 3)) '(1 2 3 4 5 6)))
  )

