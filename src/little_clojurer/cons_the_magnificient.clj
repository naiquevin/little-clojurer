(ns little-clojurer.cons_the_magnificient)

;; This chapter is about `cons` but in Clojure it makes sense to use
;; `conj` which is a common abstraction for adding items to all kinds
;; of collections. The behaviour depending upon the type of
;; collection. In case of lists, it works just like `cons` (ie. the
;; item is added to the front of the list) but with two differences:
;;
;;   1. The order of the arguments is reversed.
;;   2. Besides the list, it takes any number of additional args and
;;      in case of lists, the 'cons'-ing happens in the order they are
;;      defined.


(defn rember
  "Returns a new list with the first occurence of the atom `a` removed
  from the list `xs`"
  [a xs]
  (cond (empty? xs) nil
        (= (first xs) a) (rest xs)
        :else (conj (rember a (rest xs)) (first xs))))


;; (rember 'and '(bacon lettuce and tomato))
;; (rember 'sauce '(soy and tomato sauce))


(defn firsts
  "Returns a list of all the first items from each list in a list of
  list of atoms `xs`"
  [xs]
  (if (empty? xs)
    nil
    (conj (firsts (rest xs)) (first (first xs)))))

(defn firsts*
  [xs]
  (when (seq xs)
    (conj (firsts* (rest xs)) (first (first xs)))))

;; (firsts* '((apple peach pumpkin) (plum pear cherry)
;;            (grape raisi n pea ) (bean carrot eggplant)))


(defn insert-r
  "Returns a new list with atom `new` added to the right of the first
  occurrence `old` in the list `xs`"
  [new old xs]
  (when (seq xs)
    (if (= (first xs) old)
      (conj (rest xs) new old)
      (conj (insert-r new old (rest xs)) (first xs)))))


;; (insert-r 'topping 'fudge '(ice cream with fudge for dessert))
;; (insert-r 'jalapeno 'and '(tacos tamales and salsa))
;; (insert-r 'e 'd '(a b c d f g d h))


(defn insert-l
  "Returns a new list with atom `new` added to the left of the first
  occurrence of `old` in the list `xs`"
  [new old xs]
  (when (seq xs)
    (if (= (first xs) old)
      (conj (rest xs) old new)
      (conj (insert-l new old (rest xs)) (first xs)))))


;; (insert-l 'jalapeno 'and '(tacos tamales and salsa))


(defn subst
  "Returns a list with the first occurrence of `old` replaced with
  `new` in list `xs`"
  [new old xs]
  (when (seq xs)
    (if (= (first xs) old)
      (conj (rest xs) new)
      (conj (subst new old (rest xs)) (first xs)))))

;; (subst 'topping 'fudge '(icecream with fudge for dessert))


(defn subst2
  "Returns a list with the first occurrence of either `o1` or `o2`
  with `new` in the list `xs`"
  [new o1 o2 xs]
  (when (seq xs)
    (let [olds #{o1 o2}]
      (if (olds (first xs))
        (conj xs new)
        (conj (subst2 new o1 o2 (rest xs)) (first xs))))))

;; (subst2 'vanilla 'chocolate' 'banana
;;         '(banana ice cream with chocolate topping))


(defn multi-rember
  "Returns a list with all occurrences of `a` removed from the input
  list `xs`"
  [a xs]
  (when (seq xs)
    (if (= (first xs) a)
      (multi-rember a (rest xs))
      (conj (multi-rember a (rest xs)) (first xs)))))

;; (multi-rember 'cup '(coffee cup tea cup and hick cup))


(defn multi-insert-r
  "Returns a list `new` inserted after every occurrence of `old` in
  the original list `xs`"
  [new old xs]
  (when (seq xs)
    (if (= (first xs) old)
      (conj (multi-insert-r new old (rest xs)) new old)
      (conj (multi-insert-r new old (rest xs)) (first xs)))))

;; (multi-insert-r 'curry 'fish '(fish is the same as fish))


(defn multi-insert-l
  "Returns a list `new` inserted before (left of) every occurrence of
  `old` in the original list `xs`"
  [new old xs]
  (when (seq xs)
    (if (= (first xs) old)
      (conj (multi-insert-l new old (rest xs)) old new)
      (conj (multi-insert-l new old (rest xs)) (first xs)))))

;; (multi-insert-l 'fried 'fish '(chips and fish or fish and fried))


(defn multi-subst
  "Returns a list with all occurrences of `old` substituted with `new`
  in the original list `xs`"
  [new old xs]
  (when (seq xs)
    (if (= (first xs) old)
      (conj (multi-subst new old (rest xs)) new)
      (conj (multi-subst new old (rest xs)) (first xs)))))


;; (multi-subst 'yogurt 'fudge
;;              '(fudge icecream with extra fudge for dessert))
