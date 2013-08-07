(ns fourclojure.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn compress [s]
  (map first
       (partition-by identity s)))
  ;(reduce (fn [a b]
                     ;(if (= a b)
                       ;a
                       ;(cons a b)))
                   ;s))

(defn maxi [& args]
  (reduce #(if (> %1 %2)
             %1
             %2)
          args))

(defn weave [a b]
  ((fn [a b accum]
     (if (or (= (count a) 0)
             (= (count b) 0))
       accum
       (let [new-accum (conj
                         (conj accum (first a))
                         (first b))]
         (recur (rest a) (rest b) new-accum))))
     a
     b
     []))

(defn my-range [begin end]
  (take (- end begin)
        (iterate (fn [x]
                   (inc x))
                 begin)))

(defn has-nil? [k m]
  (and (contains? m k)
       (nil? (k m))))

(defn infix-calc [& args]
  (if (= (count args) 1)
    (first args)
    (let [a (first args)
          b (nth args 2)
          operator (second args)
          result (operator a b)
          new-args (conj (rest (rest (rest args)))
                         result)]
      (recur new-args))))

(defn default-map [default ks]
  "Problem 156"
  (zipmap ks (repeat default)))

(defn my-compare [f a b]
  (let [f-a (f a b)
        f-b (f b a)]
    (if (= false f-a f-b)
      :eq
      (if (and (= true f-a)
               (= false f-b))
        :lt
        :gt))))
(defn pascal [row]

  (if (<= row 1)
    [1]
    (let [prev-row (pascal (dec row))
          indicies (range row)
          new-row (map (fn [index]
                         ((fn pascal-cell [index prev-row]
    (let [cell-value (+
                       (nth prev-row (- index 1) 0)
                       (nth prev-row (- index 0) 0))]
      cell-value)) index prev-row))
                       indicies)]
      new-row)))

(defn my-interpose [spacer s]
  (flatten (reduce (fn [a b]
                     (list a spacer b))
                   s)))

(defn my-gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))

(defn my-closure [x]
  (fn [n]
    (int (Math/pow n x))))

(defn my-intersection [a b]
  (set (filter #(contains? b %1) a)))

(defn my-split [n s]
  (list
    (take n s)
    (drop n s)))

(defn read-binary [x]
  (int (reduce +
               (map-indexed (fn [idx itm]
                              (let [n (read-string (str itm))]
                                (* n (Math/pow 2 idx))))
                            (reverse x)))))

;(defn flatten-sets
  ;"Like flatten, but pulls elements out of sets instead of sequences."
  ;[v]
  ;(filter (complement set?)
          ;(rest (tree-seq set? seq (set v)))))


(defn pairwise-disjoint [sets]
  (let [flattened (filter (complement set?)
                          (rest (tree-seq set? seq (set sets))))]
    (if (>
          (reduce max
                  (map (fn [m]
                         (count (last m)))
                       (seq (group-by identity flattened))))
          1)
      false
      true)))

(defn my-iterate [f n]
  (cons n
    (lazy-seq (my-iterate f (f n)))))

(defn my-zipmap [ks vs]
  (reduce merge
    (map #(assoc {} %1 %2) ks vs)))

(defn drop-every-nth [s n]
  (remove nil?
          (map-indexed (fn [idx itm]
                         (let [nice-index (inc idx)]
                           (if (= 0 (mod nice-index n))
                             nil
                             itm)))
                       s)))
