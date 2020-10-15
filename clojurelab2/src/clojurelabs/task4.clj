(ns clojurelabs.task4)

(defn exclude-repeated [symbol items]
  (filter (fn [it] (not= symbol (first it))) items))

(defn increment-one [symbol items]
  (map (fn [coll] (conj coll symbol)) (exclude-repeated symbol items)))

(defn increment-symbols [alphabet items]
  (mapcat (fn [symbol] (increment-one symbol items)) alphabet))

(defn iterate-n [n alphabet]
  (nth (iterate (increment-symbols alphabet '(())) '(())) n))

(defn to-strings [items]
  (map (fn [it] (apply str it)) items))

(defn -main [& args]
  ;(println (generate '("a" (:b 1) ['c 'd]) 3))
  ;(println (generate '("a" "b" "c" "d") 3))
  (println (iterate-n 3 '("a" "b" "c" "d")))
  )

