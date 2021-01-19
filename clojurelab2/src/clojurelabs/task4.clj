(ns clojurelabs.task4)

; Define constants

(defn const [value]
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

(defn oper-type [expr]
  (when
    ((some-fn conjunction?
                  disjunction?
                  invert?)
         expr)
    (cond
      (conjunction? expr) conjunction
      (disjunction? expr) disjunction
      (invert? expr) invert)))

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

    [(fn [expr] (invert? expr))
     (fn [expr [to-be-inverted]] (let [[arg] (args expr)]
                                   (recur-provide-inversion arg (not to-be-inverted))))]))

(defn recur-provide-inversion
  [expr to-be-inverted]
  (translate-by-table expr provide-inversion-table to-be-inverted))

(defn provide-inversion-to-atoms
  [expr]
  (recur-provide-inversion expr false))

; Step 3: Distribution rules
(declare recur-distribution)

(def distribution-table
  (list
    [(fn [expr]
       (or
         (is-atom? expr)
         (and (invert? expr)
              (if-let [[arg] (args expr)]
                (is-atom? arg)
                false)
              ))
       )
     (fn [expr _] expr)]

    ; (x v y) ^ z => (x ^ z) v (y ^ z)
    [(fn [expr] (and (conjunction? expr)
                     (disjunction? (first (args expr)))))
     (fn [expr _] (let [[x-disjunction-y z] (args expr)
                        [x y] (args x-disjunction-y)]
                    (disjunction (conjunction (recur-distribution x)
                                              (recur-distribution z))
                                 (conjunction (recur-distribution y)
                                              (recur-distribution z)))))]

    ;x ^ (y v z) => (x ^ y) v (x ^ z)
    [(fn [expr] (and (conjunction? expr)
                     (disjunction? (second (args expr)))))
     (fn [expr _] (let [[x y-disjunction-z] (args expr)
                        [y z] (args y-disjunction-z)]
                    (disjunction (conjunction (recur-distribution x)
                                              (recur-distribution y))
                                 (conjunction (recur-distribution x)
                                              (recur-distribution z)))))]

    [(fn [expr] (disjunction? expr))
     (fn [expr _] (let [[arg1 arg2] (args expr)]
                    (disjunction (recur-distribution arg1)
                                 (recur-distribution arg2))))]

    [(fn [expr] (conjunction? expr))
     (fn [expr _] (let [[arg1 arg2] (args expr)]
                    (conjunction (recur-distribution arg1)
                                 (recur-distribution arg2))))]))

(defn recur-distribution
  [expr]
  (translate-by-table expr distribution-table))


; signify vars
(declare vars-to-values)

(def sign-table
  (list
    [(fn [expr] (constant? expr))
     (fn [expr _] expr)]

    [(fn [expr] (variable? expr))
     (fn [expr [var val]] (if (equal-var? expr var)
                            (const val)
                            expr))]

    [(fn [expr] (conjunction? expr))
     (fn [expr [var val]] (let [[arg1 arg2] (args expr)]
                            (conjunction (vars-to-values arg1 var val)
                                         (vars-to-values arg2 var val))))]

    [(fn [expr] (disjunction? expr))
     (fn [expr [var val]] (let [[arg1 arg2] (args expr)]
                            (disjunction (vars-to-values arg1 var val)
                                         (vars-to-values arg2 var val))))]

    [(fn [expr] (invert? expr))
     (fn [expr [var val]] (let [[arg] (args expr)]
                            (invert (vars-to-values arg var val))))]))

(defn vars-to-values
  [expr var val]
  (translate-by-table expr sign-table var val))

(defn signify-var
  [expr var val]
  {:pre [(variable? var)
         (boolean? val)]}
  (vars-to-values expr var val))

; Step 4: simplifying
(declare simplify)

(defn equal-expect-inv [e1 e2]
  (and (invert? e1)
       (let [[arg] (args e1)]
         (= arg e2))))

(def simplify-table
  (list
    ; arg1 ^ false == false
    [(fn [expr] (and (conjunction? expr)
                     (let [[arg1 arg2] (args expr)]
                       (or (= arg1 (const false))
                           (= arg2 (const false))))))
     (fn [_ _] (const false))]

    ; arg1 ^ arg1 == arg1
    [(fn [expr] (and (conjunction? expr)
                     (let [[arg1 arg2] (args expr)]
                       (= arg1 arg2))))
     (fn [expr _] (simplify (first (args expr))))]

    ; arg1 v false == arg1
    [(fn [expr] (and (disjunction? expr)
                     (let [[arg1 arg2] (args expr)]
                       (or (= arg1 (const false))
                           (= arg2 (const false))))))
     (fn [expr _] (let [[arg1 arg2] (args expr)]
                    (simplify (if (= arg1 (const false)) arg2 arg1))))]

    ; arg1 v arg1 == arg1
    [(fn [expr] (and (disjunction? expr)
                     (let [[arg1 arg2] (args expr)]
                       (= arg1 arg2))))
     (fn [expr _] (simplify (first (args expr))))]


    ; arg1 ^ true == arg1
    [(fn [expr] (and (conjunction? expr)
                     (let [[arg1 arg2] (args expr)]
                       (or (= arg1 (const true))
                           (= arg2 (const true))))))
     (fn [expr _] (let [[arg1 arg2] (args expr)]
                    (simplify (if (= arg1 (const true)) arg2 arg1))))]


    ; arg1 v true == true
    [(fn [expr] (and (disjunction? expr)
                     (let [[arg1 arg2] (args expr)]
                       (or (= arg1 (const true))
                           (= arg2 (const true))))))
     (fn [_ _] (const true))]

    ; arg1 ^ !arg1 / !arg1 ^ arg1 == false
    [(fn [expr] (and (conjunction? expr)
                     (let [[arg1 arg2] (args expr)]
                       (or (equal-expect-inv arg1 arg2)
                           (equal-expect-inv arg2 arg1)))))
     (fn [_ _] (const false))]

    ; arg1 v !arg1 / !arg1 v arg1== true
    [(fn [expr] (and (disjunction? expr)
                     (let [[arg1 arg2] (args expr)]
                       (or (equal-expect-inv arg1 arg2)
                           (equal-expect-inv arg2 arg1)))))
     (fn [_ _] (const true))]

    [(fn [expr] ((some-fn conjunction?
                          disjunction?
                          invert?)
                 expr))
     (fn [expr _] (let [oper-type (oper-type expr)
                        args (args expr)]
                    (apply oper-type (map simplify args))))]

    [(fn [expr] (is-atom? expr))
     (fn [expr _] expr)]
    ))

(defn simplify
  [expr]
  (translate-by-table expr simplify-table))

(defn to-dnf
  [expr]
  (->>
    expr
    (recur-trans)
    (provide-inversion-to-atoms)
    (recur-distribution)
    (simplify)))