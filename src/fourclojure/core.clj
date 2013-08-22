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
                            (let [cell-value (+ (nth prev-row (- index 1) 0)
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

(defn half-truth [& args]
  (= 2 (count (group-by identity args))))

(defn dot-product [a b]
  (reduce +
          (map #(* %1 %2)
               a b)))

(defn cartesian-product [a b]
  (set (for [x a
             y b]
         [x y])))

(defn product-digits [a b]
  (map #(read-string (str %1))
       (seq (str (* a b)))))

(defn group-sequence [f s]
  (reduce
    #(merge-with concat %2 %1)
              (map (fn [pair]
                     (let [k (first pair)
                           v (last pair)]
                       {v [k]}))
                   (zipmap s (map f s)))))

(defn sum-squares [nums]
  (count
    (filter true?
            (map (fn [n]
                   (let [digits (map #(read-string (str %1)) (seq (str n)))
                         squares (map #(* %1 %1) digits)
                         sum-square (reduce + squares)
                         smaller-than-sum-square (< n sum-square)]
                     smaller-than-sum-square))
                 nums))))

(defn is-binary-tree? [t]
  (if (nil? t)
    true
    (if (and (coll? t)
             (= (count t) 3))
      (and (is-binary-tree? (second t))
           (is-binary-tree? (last t)))
      false)))

(defn is-symmetric-tree? [t]
  (letfn [(mirror-tree [t]
          (if (nil? t)
            nil
            [(first t)
             (mirror-tree (last t))
             (mirror-tree (second t))]))]

    (let [value (first t)
          left (second t)
          right (last t)]
      (if (nil? t)
        true
        (if (or (= left right)
                (= left (mirror-tree right)))
          true
          false)))))

(defn rec-playing-card [card-string]
  (let [suits {\D :diamond
               \H :heart
               \C :club
               \S :spade}
        ranks {\2 0
               \3 1
               \4 2
               \5 3
               \6 4
               \7 5
               \8 6
               \9 7
               \T 8
               \J 9
               \Q 10
               \K 11
               \A 12}]
    {:suit (get suits (first (seq card-string)))
     :rank (get ranks (last (seq card-string)))}))

(defn my-map [f s]
  (if (or (nil? s)
          (empty? s))
    nil
    (cons (f (first s))
          (lazy-seq (my-map f (rest s))))))

(defn lcm-pair [a b]
  (/ (* a b)
     (my-gcd a b)))

(defn product [pair]
  (* (first pair) (last pair)))

(defn next-accum [accum]
 (let [products (map product accum)
       min-number (reduce min products)
       ; min-index (.indexOf products min-number)
       ]
   (println "products was: " products)
   (println "min for: " products " was: " min-number)
   (map (fn [x]
          (if (= (product x) min-number)
            [(inc (first x)) (last x)]
            x))
        accum)))

(defn lcm [& args]
  (defn recursive-lcm [accum]
    (let [products (map product accum)]
      (if (and (> (count products) 0)
               (= (count (set products)) 1))
        (first products)
        (do
          (println "recurring with accum: " accum)
          (recur (next-accum accum))))))
  (let [prepped-accum (map (fn [x]
                             [1 x])
                           args)
        ]
    (recursive-lcm prepped-accum)))

(defn set-difference [a b]
  (clojure.set/union
    (clojure.set/difference a b)
    (clojure.set/difference b a)))

(defn pascal-trapezoid [v]
  (iterate (fn [previous]
             (flatten (conj (list (last previous))
                     (map-indexed (fn [index element]
                                    (+' (nth previous (- index 1) 0)
                                       (nth previous (- index 0) 0)))
                                  previous))))
           v))

(defn index-sequence [s]
  (map-indexed (fn [idx e]
                 [e idx])
                  s))

(defn tree-to-table [t]

  )
