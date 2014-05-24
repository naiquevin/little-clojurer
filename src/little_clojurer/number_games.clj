(ns little-clojurer.number-games)

;; Some Scheme functions used in the chapter map to their equivalent
;; Clojure functions as follows:
;;
;; add1    --> inc
;; sub1    --> dec
;; zero?   --> zero?
;; number? --> number?


(defn +
  "Adds the two numbers `n` and `m`."
  [n m]
  (if (zero? m)
    n
    (inc (+ n (dec m)))))


(defn -
  "Subtracts the second number from the first."
  [n m]
  (if (zero? m)
    n
    (dec (- n (dec m)))))


(defn addtup
  "Adds all the numbers in a tuple (vector)."
  [tup]
  (if (empty? tup)
    0
    (+ (first tup) (addtup (rest tup)))))


(defn x
  "Multiplies two numbers."
  [n m]
  (if (zero? m)
    0
    (+ n (x n (dec m)))))


(defn tup+
  "Builds a tuple from two tuples by adding corresponding numbers in
  the them in the same position. Assumes that tup1 and tup2 are of the
  same length."
  [tup1 tup2]
  (if (and (empty? tup1) (empty? tup2))
    nil
    (conj (tup+ (rest tup1) (rest tup2))
          (+ (first tup1) (first tup2)))))

;; (tup+ '(3 6 9 11 4) '(8 5 2 0 7))


(defn tup+*
  "Builds a tuple from two tuples by adding corresponding numbers in
  the them in the same position. The length of the two tuples may be
  different."
  [tup1 tup2]
  (cond (and (empty? tup1) (empty? tup2)) nil
        (empty? tup1) tup2
        (empty? tup2) tup1
        :else (conj (tup+* (rest tup1) (rest tup2))
                    (+ (first tup1) (first tup2)))))

;; (tup+* [3 7 8 1] [4 6])

(defn >
  "Checks whether the first number is greater than the second."
  [n m]
  (cond (zero? n) false
        (zero? m) true
        :else (> (dec n) (dec m))))

;; (> 4 5)  => false
;; (> 14 5) => true
;; (> 5 5)  => false


(defn <
  "Checks whether the first number is less than the second."
  [n m]
  (cond (zero? m) false
        (zero? n) true
        :else (< (dec n) (dec m))))

;; (< 5 5)  => false
;; (< 15 5) => false
;; (< 5 15) => true


(defn =
  "Check if the two numbers are equal."
  [n m]
  (cond (zero? n) (zero? m)
        (zero? m) false
        :else (= (dec n) (dec m))))

(defn =*
  "Alternate version to check if two numbers are equal"
  [n m]
  (not (or (< m n) (> m n))))


(defn exp
  "Returns the value of n raised to m"
  [n m]
  (if (zero? m) 1 (× n (exp n (dec m)))))


(defn ÷
  "Returns the quotient obtained upon dividing n by m"
  [n m]
  (if (< n m)
    0
    (inc (÷ (- n m) m))))


(defn length
  "Returns the number of items in the list"
  [xs]
  (if (empty? xs)
    0
    (+ 1 (length (rest xs)))))


(defn pick
  "Picks an item at position `n` in list `xs`."
  [n xs]
  (when (> n 0)
    (if (zero? (dec n))
      (first xs)
      (pick (dec n) (rest xs)))))

;; (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) => macaroni
;; (pick 0 '(lasagna))                                     => nil


(defn rempick
  "Picks all items in the list `xs` except the one at position
  `n`. Assumes that (< n (length xs)"
  [n xs]
  (if (zero? (dec n))
    (rest xs)
    (conj (rempick* (dec n) (rest xs)) (first xs))))


(defn rempick*
  "Alternate version of rempick that works for all values of positive
  values of `n`."
  [n xs]
  (cond (empty? xs) nil
        (zero? (dec n)) (rest xs)
        :else (conj (rempick* (dec n) (rest xs)) (first xs))))

;; (rempick 3 '(hotdogs with hot mustard)) => (hotdogs with mustard)
;; (rempick* 3 '(hotdogs))                 => (hotdogs)


(defn no-nums
  "Returns all atoms from a list that are not numbers."
  [xs]
  (cond (empty? xs) nil
        (number? (first xs)) (no-nums (rest xs))
        :else (conj (no-nums (rest xs)) (first xs))))

;; (no-nums '(5 pears 6 prunes 9 dates)) => (pears prunes dates)


(defn all-nums
  "Returns a tuple of numbers form a list of atoms."
  [xs]
  (cond (empty? xs) nil
        (number? (first xs)) (conj (all-nums (rest xs)) (first xs))
        :else (all-nums (rest xs))))

;; (no-nums '(5 pears 6 prunes 9 dates)) => (5 6 9)

;; eqan? is actually the same as clojure.core/=
(def eqan? clojure.core/=)

(defn occur
  "Returns the number of times the atom `a` appears in the list `xs`"
  [a xs]
  (cond (empty? xs) 0
        (eqan? (first xs) a) (+ 1 (occur a (rest xs)))
        :else (occur a (rest xs))))

;; (occur 4 '(1 4 2 3 4 5 4 6 4 4)) => 5
;; (occur 9 '(1 4 2 3 4 5 4 6 4 4)) => 0


(defn one?
  "Checks if value of `n` is number equal to one"
  [n]
  (zero? (dec n)))

(defn one*?
  [n] (= n 1))

(defn rempick**
  "Alternate version of rempick that uses `one?`"
  [n xs]
  (cond (empty? xs) nil
        (one? n) (rest xs)
        :else (conj (rempick** (dec n) (rest xs)) (first xs))))


;; (rempick** 3 '(lemon meringue salty pie)) => (lemon meringue pie)
