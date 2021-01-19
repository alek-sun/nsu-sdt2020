(ns clojurelabs.task4-tests
  (:require [clojure.test :as test]
            [clojurelabs.task4 :as logic]))

(test/deftest test-dnf
  (let [x (logic/variable :x)
        y (logic/variable :y)
        z (logic/variable :z)
        t (logic/variable :t)]

    (test/testing
      "((x | !y) & z) -> (t & !(y -> z)) ==== ((!x & y) | !z) | (t & (y & !z))"
      (test/is (= (logic/to-dnf
                    (logic/implication (logic/conjunction (logic/disjunction x (logic/invert y)) z)
                                       (logic/conjunction t (logic/invert (logic/implication y z)))))

                  (logic/disjunction (logic/disjunction (logic/conjunction (logic/invert x) y)
                                                        (logic/invert z))
                                     (logic/conjunction t (logic/conjunction y (logic/invert z))))
                  )))))

(test/deftest test-vals-vars
  (let [x (logic/variable :x)
        y (logic/variable :y)
        x-disj-y (logic/disjunction x y)]

      (test/is (= (logic/signify-var x-disj-y x true)
                  (logic/disjunction (logic/const true)
                                     y)))
      ))

(test/run-tests 'clojurelabs.task4-tests)