(ns clojurelab1.core)

(defn item-alph
  "concatenate all alphabet symbols with item, if symbol non-equal with item's first symbol"
  [item alphabet]
  (if (= (count alphabet) 0)
    ()
    (let [symbol (first alphabet), item-first (first item), result-rest (item-alph item (rest alphabet))]
      (if (not= symbol item-first)
        (cons (cons symbol item) result-rest)
        result-rest))))

(defn alph-alph
  "concatenate all alphabet symbols with all alphabet symbols"
  ([[first & alph-rest] alphabet]
   (let [item (item-alph first alphabet)]
     (if (= (count alph-rest) 0)
       item
       (concat item (alph-alph alph-rest alphabet))))))

(defn recur-n-times
  [item n alphabet]
  (if (= n 0)
    item
    (recur-n-times (alph-alph item alphabet) (dec n) alphabet)))

(defn to-strings
  [[first & rest]]
  (let [first-result (apply str first)]
    (if (= (count rest) 0)
      (list first-result)
      (cons first-result (to-strings rest)))))

(defn generate
  [alphabet n]
  (let [initial-alph (recur-n-times '() 1 alphabet)]
    (if (= n 1)
      initial-alph
      (recur-n-times initial-alph (dec n) alphabet))))

(defn -main [& args]
  (println (generate '("a" (:b 1) ['c 'd]) 2))
  (println (generate '("a" "b" "c" "d") 7)))