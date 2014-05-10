(ns little-clojurer.do-it-again)


(defn lat?
  "Checks if it's a list of atoms"
  [xs]
  (cond (empty? xs) true
        (coll? (first xs)) false
        :else (lat? (rest xs))))


(defn lat*?
  [xs]
  (or (empty? xs)
      (and (not (coll? (first xs)))
           (lat*? (rest xs)))))


(defn member?
  "Checks if `a` is a member of the list `xs`"
  [a xs]
  (cond (empty? xs) false
        (= (first xs) a) true
        :else (member? a (rest xs))))


(defn member*?
  [a xs]
  (and (not (empty? xs))
       (or (= (first xs) a)
           (member*? a (rest xs)))))






