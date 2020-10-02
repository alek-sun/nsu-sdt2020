(ns clojurelab2.core)

(defn item-alph-tail
  "concatenate all alphabet symbols with item, if symbol non-equal with item's first symbol"
  [item alphabet acc]
  (if (= (count alphabet) 0)
    acc
    (let [symbol (first alphabet), item-first (first item)]
      (if (not= symbol item-first)
        (recur item (rest alphabet) (cons (cons symbol item) acc))
        (recur item (rest alphabet) acc)))))

(defn alph-alph-tail
  "concatenate all alphabet symbols with all alphabet symbols"
  ([[item & alph-rest] alphabet acc]
   (let [item (item-alph-tail item alphabet ())]
     (if (= (count alph-rest) 0)
       (concat acc item)
       (recur alph-rest alphabet (concat acc item))))))

(defn recur-n-times
  [item n alphabet]
  (if (= n 0)
    item
    (recur (alph-alph-tail item alphabet ()) (dec n) alphabet)))

(defn to-strings
  [[first & rest] acc]
  (let [first-result (apply str first)]
    (if (= (count rest) 0)
      (concat acc (list first-result))
      (recur rest (concat acc (list first-result)))
      )))

(defn generate
  [alphabet n]
  (let [initial-alph (recur-n-times '() 1 alphabet)]
    (if (= n 1)
      initial-alph
      (to-strings (recur-n-times initial-alph (dec n) alphabet) ()))))

(defn -main [& args]
  (println (generate '("a" (:b 1) ['c 'd]) 3))
  (println (generate '("a" "b" "c" "d") 5)))