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

(defn -main
  []
  (let [step 0.05
        integrator (integrate (fn [x] x) step)]
    (time (integrator 200))
    (time (integrator 201))
    (time (integrator 199))
    ;(time (integrate (fn [x] x) step 200))
    ;(time (integrate (fn [x] x) step 200))
    ;(time (integrate (fn [x] x) step 199))
    ;(time (integrate (fn [x] x) step 1.5))
    ;(time (integrate (fn [x] x) step 3.7))
    ;(println (integrate (fn [x] x) step 2))
    ;(println (integrate (fn [x] x) 7 9))
    )
  )