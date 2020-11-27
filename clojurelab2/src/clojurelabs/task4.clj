(ns clojurelabs.task4)

; Define constants

(defn constant [value]
  {:pre [(boolean? value)]}
  (list ::const value))

(defn constant? [expr]
  (= (first expr) ::const))

(defn constant-value [const]
  (second const))



; Define variables, it's names, equality

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn var-name [var]
  (second var))

(defn equal-var? [var1 var2]
  {:pre [(and (variable? var1) (variable? var2))]}
  (=
    (var-name var1)
    (var-name var2)
    ))


; Conjunction

(defn conjunction [expr & rest]
  (cons ::conj (cons expr rest))
  )

(defn conjunction? [expr]
  (= (first expr) ::conj)
  )

; Disjunction

(defn disjunction [expr & rest]
  (cons ::disj (cons expr rest))
  )

(defn disjunction? [expr]
  (= (first expr) ::disj)
  )

; Invert

(defn invert [expr]
  (cons ::inv (list expr))
  )

(defn invert? [expr]
  (= ::inv (first expr)))


; Implication

(defn implication [left right]
  (cons ::impl (list left right))
  )

(defn implication? [expr]
  (= (first expr) ::impl)
  )

(defn args [expr]
  (rest expr))


(defn is-atom?
  [expr]
  ((some-fn constant?
            variable?)
   expr))

; Step 1: Translation to base items by translation table

(defn translate-by-table
  [expr table & vars]
  (if-let [transform
           (some
             (fn [[_ rule]]
               (if ((first rule) expr)
                 (second rule)
                 false
                 )
               )
             (map-indexed list table))]
    (transform expr vars)
    )
  )

(declare table)

(defn recur-trans
  [expr]
  (translate-by-table expr table))

(def table
  (list
    [(fn [expr] (is-atom? expr))
     (fn [expr _] expr)]

    [(fn [expr] (conjunction? expr))
     (fn [expr _] (let [[arg1 arg2] (args expr)]
                    (conjunction (recur-trans arg1)
                                 (recur-trans arg2))))]

    [(fn [expr] (disjunction? expr))
     (fn [expr _] (let [[arg1 arg2] (args expr)]
                    (disjunction (recur-trans arg1)
                                 (recur-trans arg2))))]

    [(fn [expr] (invert? expr))
     (fn [expr _] (let [[arg] (args expr)]
                    (invert (recur-trans arg))))]

    [(fn [expr] (implication? expr))
     (fn [expr _] (let [[arg1 arg2] (args expr)]
                    (disjunction (invert (recur-trans arg1))
                                 (recur-trans arg2))))]
    )
  )

; Step 2: Provide inversion

(declare recur-provide-inversion)

(def provide-inversion-table
  (list
    [(fn [expr] (invert? expr)
       (fn [expr [to-be-inverted]] (let [[arg] (args expr)]
                                     (recur-provide-inversion arg (not to-be-inverted)))))]

    [(fn [expr] (is-atom? expr))
     (fn [expr [to-be-inverted]] (if to-be-inverted
                                   (invert expr)
                                   expr))]

    [(fn [expr] (conjunction? expr))
     (fn [expr [to-be-inverted]] (let [[arg1 arg2] (args expr)
                                       inverted (if to-be-inverted disjunction conjunction)]
                                   (inverted (recur-provide-inversion arg1 to-be-inverted)
                                       (recur-provide-inversion arg2 to-be-inverted))))]

    [(fn [expr] (disjunction? expr))
     (fn [expr [to-be-inverted]] (let [[arg1 arg2] (args expr)
                                       inverted (if to-be-inverted conjunction disjunction)]
                                   (inverted (recur-provide-inversion arg1 to-be-inverted)
                                       (recur-provide-inversion arg2 to-be-inverted))))]
    )
  )

(defn recur-provide-inversion
  [expr to-be-inverted]
  (translate-by-table expr provide-inversion-table to-be-inverted))

(defn provide-inversion-to-atoms
  [expr]
  (recur-provide-inversion expr false))                     ; by default it's not necessity of inversion