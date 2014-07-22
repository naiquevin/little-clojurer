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
