(ns clojurelabs.task4-tests
  (:require [clojure.test :as test]
            [clojurelabs.task4 :as logic]))

(test/deftest constants-test
  (test/testing "Constants test"
    (test/is (logic/constant? (logic/constant true)))
    (test/is (= false (logic/constant-value (logic/constant false))))
    ))


(test/deftest variables-test
  (test/testing "Variables test"
    (let [var (logic/variable :name)]
    (test/is (logic/variable? var))
    (test/is (logic/equal-var? var var))
    (test/is (= :name (logic/var-name var)))
    (test/is (not (logic/equal-var? var (logic/variable :other))))
    )
    ))

(test/deftest operators-test
  (let [var1 (logic/variable :name)
        var2 (logic/variable :other)
        var3 (logic/constant true)]

    (test/testing "Conjunction test"
      (test/is (logic/conjunction? (logic/conjunction var1 var2)))
      (test/is (logic/conjunction? (logic/conjunction var1 var3)))
      )

    (test/testing "Disjunction test"
      (test/is (logic/disjunction? (logic/disjunction var1 var2)))
      (test/is (logic/disjunction? (logic/disjunction var1 var3))))

    (test/testing "Inversion test"
      (test/is (logic/invert? (logic/invert var1)))
      (test/is (logic/invert? (logic/invert var3))))

    (test/testing "Implication test"
      (test/is (logic/implication? (logic/implication var1 var2)))
      (test/is (logic/implication? (logic/implication var1 var3)))))
  )

(test/deftest test-recur-trans
  (let [var1 (logic/variable :name)
        var2 (logic/variable :other)
        c-true (logic/constant true)]

      (let [res1 (logic/recur-trans c-true)]
        (test/is (= res1 c-true)))

      (let [c1 (logic/conjunction var1 var2)
            c2 (logic/conjunction var1 c-true)
            res1 (logic/recur-trans c1)
            res2 (logic/recur-trans c2)]
        (test/is (= res1 c1))
        (test/is (= res2 c2)))

      (let [inv1 (logic/invert c-true)
            inv2 (logic/invert var1)

            res1 (logic/recur-trans inv1)
            res2 (logic/recur-trans inv2)]
        (test/is (= res1 inv1))
        (test/is (= res2 inv2)))

      ; var1 -> var2 == !var1 ^ var2
        (let [impl (logic/implication var1 var2)
              res (logic/recur-trans impl)
              [arg1 arg2] (logic/args res)]
          (test/is (logic/disjunction? res))
          (test/is (= arg1 (logic/invert var1)))
          (test/is (= arg2 var2)))
      ))

(test/run-tests 'clojurelabs.task4-tests)
