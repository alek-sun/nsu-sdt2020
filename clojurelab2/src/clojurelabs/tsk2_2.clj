(ns clojurelabs.tsk2-2
  (:use [clojure.test]))

(defn trapeze-area
  [f x1 x2]
  (* (- x2 x1) (/ (+ (f x1) (f x2)) 2.0))
  )

(defn integrate [f step]
  (let [steps (map (partial * step) (range))
        sequent (reductions + 0
                            (map (partial trapeze-area f) steps (rest steps)))]
    (fn [b]
      (let [r (mod b step)
            n-step (int (/ (- b r) step))]
        (if (= n-step 0)
          0
          (nth
            sequent
            n-step)
          )
        )
      )
    )
  )

(defn little-diff [real expected]
  (< (Math/abs ^float (- real expected)) 0.0000001)
  )

(deftest trapeze-area-test
  (is (= (trapeze-area (fn [x] x) 3 6) 13.5))
  (is (= (trapeze-area (fn [x] 1) 1 5) 4.0))
  )

(deftest integrate-test
  (let [step 0.5
        result (integrate (fn [x] x) step)]
    (is (little-diff (result 3) 4.5))
    (is (little-diff (result 6) 18.0))
    (is (little-diff (result 9) 40.5))
    )
  )

(defn -main
  []
  (let [step 0.05
        integrator (integrate (fn [x] x) step)]
    (time (integrator 200))
    (time (integrator 201))
    (time (integrator 201))
    (time (integrator 199))
    (time (integrator 199))
    )

  (trapeze-area-test)
  (integrate-test)
  )