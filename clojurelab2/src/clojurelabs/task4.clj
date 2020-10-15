(ns clojurelabs.task4)

(defn exclude-repeated [symbol items]
  (filter (fn [it] (not= symbol (first it))) items))

(defn increment-one [symbol items]
  (map (fn [coll] (conj coll symbol)) (exclude-repeated symbol items)))

(defn increment-symbols [alphabet items]
  (mapcat (fn [symbol] (increment-one symbol items)) alphabet))

(defn iterate-n [n alphabet]
  (nth (iterate (partial increment-symbols alphabet) '(())) n))

(defn to-strings [items]
  (map (fn [it] (apply str it)) items))

(defn -main [& args]
  (println (iterate-n 3 '("a" "b" "c" "d")))
  (println (iterate-n 3 '("a" (:b 1) ['c 'd])))
  (println (iterate-n 5 '("a" "b" "c" "d")))
  )

