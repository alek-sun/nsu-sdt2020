(ns clojurelabs.tsk2-1
  (:use [clojure.test]))

(defn trapeze-area
  [fx1 fx2 h]
  (* h (/ (+ fx1 fx2) 2.0))
  )

(defn increment-one-step [fun step n-step mem-increment-one-step]
  (if (= n-step 0)
    0
    (let [fx1 (fun (* step (dec n-step)))
          fx2 (fun (* step n-step))
          step-area (trapeze-area fx1 fx2 step)]
      (+ step-area (mem-increment-one-step fun step (dec n-step) mem-increment-one-step))
      )
    )
  )

(defn memoized [fun step]
  (let [memoization (memoize increment-one-step)]
    (fn [x]
      (let [rest (mod x step)]
        (increment-one-step fun step (int (/ (- x rest) step)) memoization)
        )
      )
    )
  )

(defn little-diff [real expected]
  (< (Math/abs ^float (- real expected)) 0.0000001)
  )

(deftest trapeze-area-test
  (is (= (trapeze-area 10 6 2) 16.0))
  (is (= (trapeze-area 5 4 0.5) 2.25))
  (is (= (trapeze-area 15.7 4 2.5) 24.625))
  )

(deftest increment-one-test
  (is (little-diff (increment-one-step (fn [x] x) 0.1 20 increment-one-step) 2.0))
  (is (little-diff (increment-one-step (fn [x] x) 0.2 10 increment-one-step) 2.0))
  )

(deftest memoized-test
  (let [step 0.5
        result (memoized (fn [x] x) step)]
    (is (little-diff (result 3) 4.5))
    (is (little-diff (result 6) 18.0))
    (is (little-diff (result 9) 40.5))
    )
  )

(defn -main
  []
  (let [step 0.05
        mem-integrate (memoized (fn [x] x) step)]
    (println "compute 0----2")
    (time (mem-integrate 2))
    (time (mem-integrate 2))
    (time (mem-integrate 2))
    (time (mem-integrate 2))
    (println (mem-integrate 2))
    (println "compute nothing 0--1")
    (time (mem-integrate 1))
    (time (mem-integrate 1))
    (time (mem-integrate 1))
    (println)
    (println "compute nothing 0-1.1")
    (time (mem-integrate 1.1))
    (println)
    (println "compute nothing 0-0.9")
    (time (mem-integrate 0.9))
    (println)
    (println "compute 2-2.1")
    (time (mem-integrate 2.1))
    (time (mem-integrate 2.1))
    (println)
    (println "compute 2.1-2.3")
    (time (mem-integrate 2.3))
    (time (mem-integrate 2.3))
    )

  (trapeze-area-test)
  (increment-one-test)
  (memoized-test)
  )





