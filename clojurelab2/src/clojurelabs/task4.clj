(ns clojurelabs.task4)

(defn exclude-repeated [symbol items]
  (filter (fn [it] (not= symbol (subs it 0 1))) items))

(defn increment-one [symbol items]
  (map (fn [coll] (str symbol coll)) (exclude-repeated symbol items)))

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

(defn generate [alphabet n]
  (if (> n 0)
    (recur-n-times alphabet (dec n) alphabet)
    '()))

(defn -main [& args]
  (println (generate '("a" "b" "c" "d") 5))
  (println (generate '("a" "b" "s") 4))
  )

