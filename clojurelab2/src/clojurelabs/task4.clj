(ns clojurelabs.task4)

(defn exclude-repeated [symbol items]
  (filter (fn [it] (not= symbol (first it))) items))

(defn increment-one [symbol items]
  (map (fn [coll] (conj coll symbol)) (exclude-repeated symbol items)))

(defn increment-one-to-all [alphabet items]
  (map (fn [symbol] (increment-one symbol items)) alphabet))

(defn increment-symbols [alphabet items]
  (reduce concat `() (increment-one-to-all alphabet items)))

(defn recur-n-times
  [items n alphabet]
  (if (= n 0)
    items
    (recur (increment-symbols alphabet items) (dec n) alphabet)
    ))

(defn init-alph [alph]
  (map (fn [symbol] (list symbol)) alph))

(defn to-strings [items]
  (map (fn [it] (apply str it)) items))

(defn generate [alphabet n]
  (if (> n 0)
    (to-strings (recur-n-times (init-alph alphabet) (dec n) alphabet))
    '()))

(defn -main [& args]
  (println (generate '("a" (:b 1) ['c 'd]) 3))
  (println (generate '("a" "b" "c" "d") 3))
  )

