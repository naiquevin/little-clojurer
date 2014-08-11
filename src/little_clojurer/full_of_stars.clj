(ns little-clojurer.full-of-stars)


(defn rember*
  "Removes all elements that are equal to `a` from nested collection
  `xs`."
  [a xs]
  (when (seq xs)
    (if (not (coll? (first xs)))
      (if (= (first xs) a)
        (rember* a (rest xs))
        (cons (first xs) (rember* a (rest xs))))
      (cons (rember* a (first xs))
            (rember* a (rest xs))))))


;; > (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
;; ((coffee) ((tea)) (and (hick)))


(defn insert-r*
  "Inserts the element `new` to the right of every occurrence of the
  element `old` in the nested collection `xs`."
  [new old xs]
  (when (seq xs)
    (if (not (coll? (first xs)))
      (if (= (first xs) old)
        (conj (insert-r* new old (rest xs)) new old)
        (conj (insert-r* new old (rest xs)) (first xs)))
      (conj (insert-r* new old (rest xs))
            (insert-r* new old (first xs))))))

;; > (insert-r* 'roast 'chuck '((how much (wood))
;;                              could
;;                              ((a (wood) chuck))
;;                              (((chuck)))
;;                              (if (a) ((wood chuck)))
;;                              could chuck wood))
;; ((how much (wood))
;;  could
;;  ((a (wood) chuck roast))
;;  (((chuck roast)))
;;  (if (a) ((wood chuck roast)))
;;  could chuck roast wood)


(defn occur*
  "Counts the total number of occurrence of the element `a` in nested
  collection `xs`."
  [a xs]
  (cond (empty? xs) 0
        (not (coll? (first xs))) (if (= (first xs) a)
                                   (+ 1 (occur* a (rest xs)))
                                   (occur* a (rest xs)))
        :else (+ (occur* a (first xs)) (occur* a (rest xs)))))

;; >  (occur* 'banana '((banana)
;;                     (split ((((banana ice)))
;;                             (crea m ( banana))
;;                             sherbet))
;;                     (banana)
;;                     (bread)
;;                     (banana brandy)))
;; 5


(defn insert-l*
  "Inserts the element `new` to the left of every occurrence of the
  element `old` in the nested collection `xs`."
  [new old xs]
  (when (seq xs)
    (if (not (coll? (first xs)))
      (if (= (first xs) old)
        (conj (insert-l* new old (rest xs)) (first xs) new)
        (conj (insert-l* new old (rest xs)) (first xs)))
      (conj (insert-l* new old (rest xs))
            (insert-l* new old (first xs))))))

;; > (insert-l* 'pecker 'chuck '((how much (wood))
;;                               could
;;                               ((a (wood) chuck))
;;                               (((chuck)))
;;                               (if (a) ((wood chuck)))
;;                               could chuck wood))
;; ((how much (wood))
;;  could
;;  ((a (wood) pecker chuck))
;;  (((pecker chuck)))
;;  (if (a) ((wood pecker chuck)))
;;  could pecker chuck wood)


(defn subst*
  "Substitutes all occurrences of `old` with `new` value in nested
  list `xs`."
  [new old xs]
  (when (seq xs)
    (if (not (coll? (first xs)))
      (if (= (first xs) old)
        (conj (subst* new old (rest xs)) new)
        (conj (subst* new old (rest xs)) (first xs)))
      (conj (subst* new old (rest xs))
            (subst* new old (first xs))))))


;; > (subst* 'orange 'banana '((banana)
;;                             (split ((((banana ice)))
;;                                     (cream (banana))
;;                                     sherbet))
;;                             (banana)
;;                             (bread)
;;                             (banana brandy)))
;; ((orange)
;;  (split ((((orange ice)))
;;          (cream (orange))
;;          sherbet))
;;  (orange)
;;  (bread)
;;  (orange brandy))


(defn insert-l*
  "Inserts `new` to the left of all occurrences of `old` in the nested
  list `xs`."
  [new old xs]
  (when (seq xs)
    (if (not (coll? (first xs)))
      (if (= (first xs) old)
        (conj (insert-l* new old (rest xs)) old new)
        (conj (insert-l* new old (rest xs)) (first xs)))
      (conj (insert-l* new old (rest xs))
            (insert-l* new old (first xs))))))


;; > (insert-l* 'pecker 'chuck '((how much (wood)) could
;;                               ((a (wood) chuck))
;;                               (((chuck)))
;;                               (if (a) ((wood chuck)))
;;                               could chuck wood))
;;   ((how much (wood)) could
;;    ((a (wood) pecker chuck))
;;    (((pecker chuck)))
;;    (if (a) ((wood pecker chuck)))
;;    could pecker chuck wood)


(defn member*
  "Checks if the atom `a` is present in nested list `xs`."
  [a xs]
  (and (not (empty? xs))
       (if (not (coll? (first xs)))
         (or (= (first xs) a) (member* a (rest xs)))
         (or (member* a (first xs)) (member* a (rest xs))))))

;; > (member* 'chips '((potato) (chips ((with) fish) (chips))))
;;   true
;; > (member* 'potato '((potato) (chips ((with) fish) (chips))))
;;   true
;; > (member* 'onion '((potato) (chips ((with) fish) (chips))))
;;   false


(defn leftmost
  "Returns the first atomic value in nested list `xs`."
  [xs]
  (when (seq xs)
    (if (not (coll? (first xs)))
      (first xs)
      (leftmost* (first xs)))))

;; > (leftmost '((potato) (chips ((with) fish) (chips))))
;;   potato
;; > (leftmost '(((hot) (tuna (and))) cheese))
;;   hot
;; > (leftmost '(((() four)) 17 (seventeen)))
;;   nil
;; > (leftmost (quote ()))
;;   nil
