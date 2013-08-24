(ns fourclojure.core
  (:require [taoensso.timbre :as timbre])
  )

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

;(defn lcm-pair [a b]
  ;(/ (* a b)
     ;(my-gcd a b)))

(defn lcm [& args]
  (let [product (fn [pair]
                  (* (first pair) (last pair)))

        next-accum (fn [accum]
                     (let [products (map product accum)
                           min-number (reduce min products)]
                       (map (fn [x]
                              (if (= (product x) min-number)
                                [(inc (first x)) (last x)]
                                x))
                            accum)))

        recursive-lcm (fn [accum]
                        (let [products (map product accum)]
                          (if (and (> (count products) 0)
                                   (= (count (set products)) 1))
                            (first products)
                            (recur (next-accum accum)))))

        prepped-accum (map (fn [x]
                             [1 x])
                           args)]
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

(defn naive-leven [a b]
  (println "Iterating with A of: " a " and B of: " b)
  (if (or (nil? a)
          (= "" a))
    (count b)
    (if (or (nil? b)
            (= "" b))
      (count a)
      (if (= a b)
        0
        (let [last-char-cost (if (= (last (seq a))
                                    (last (seq b)))
                               0
                               1)]
          (reduce min [(inc (naive-leven (butlast a) b))
                       (inc (naive-leven a (butlast b)))
                       (+ last-char-cost (naive-leven (butlast a) (butlast b)))]))))))

(def memo-naive-leven (memoize naive-leven))

(defn initialize-matrix [m n]
  (defn recur-row [rows i]
    (if (= i -1)
      rows
      (let [prev-row (last rows)
            new-row (conj
                      (take (dec (count prev-row)) (repeatedly (fn [] 0)))

                      (inc (first prev-row))
                      )]
      (recur (conj rows new-row) (dec i)))))

  (let [first-row (take n (iterate inc 0))]
    (recur-row [first-row] m)))

(defn bottom-up-leven [a b]
  nil)

  ;(let [initial-matrix (initialize-matrix (count a) (count b))
        ;]
    ;)

  ;(last (last result m) n))

; int LevenshteinDistance(char s[1..m], char t[1..n]) {
;
;   for j from 1 to n {
;       for i from 1 to m {
;           if s[i] = t[j] then  ;going on the assumption that string indexes are 1-based in your chosen language<!-- not: s[i-1] = t[j-1] -->
;                                ;else you will have to use s[i-1] = t[j-1] Not doing this might throw an IndexOutOfBound error
;             d[i, j] := d[i-1, j-1]       ; no operation required
;           else
;             d[i, j] := minimum
;                     (
;                       d[i-1, j] + 1,  ; a deletion
;                       d[i, j-1] + 1,  ; an insertion
;                       d[i-1, j-1] + 1 ; a substitution
;                     )
;         }
;     }
;
;   return d[m, n]
; }
;
(defn levenshtein [x y]
         (time (naive-leven x y)))

(defn primes [x]
  (let [recur-primes (fn [universe p]
          (if (= (last universe) p)
            universe
            (let [new-universe (remove (fn [x]
                                         (and
                                           (not (= x p))
                                           (= 0 (mod x p))))
                                       universe)
                  new-p        (first (filter (fn [x]
                                                (> x p))
                                              new-universe))]
              (recur new-universe new-p))))
        universe (take 1000 (iterate inc 2))
        prime-list (recur-primes universe 2)]
    (take x prime-list)))

(defn my-distinct [s]
  (let [recur-distinct (fn [symbols remaining accum]
                         (if (= (count symbols)
                                (count accum))
                           accum
                           (let [new-accum (conj accum (first remaining))
                                 new-remaining (remove (fn [x]
                                                         (contains? (set new-accum) x))
                                                       remaining)]
                             (recur symbols new-remaining new-accum))))]

    (recur-distinct (set s) s [])))

(defn split-by-type [s]
  nil
  )

(defn tic-tac-toe [board]
  (defn check-horizontal [board]
    (reduce
      (fn [a b]
        (or a b))
      (map
        (fn [row]
          (if (and (= 1 (count (set row)))
                   (not (= :e (first row))))
            (first row)
            nil))
        board)))
  (defn check-vertical [board]
    (reduce
      (fn [a b]
        (or a b))
      (map
        (fn [a b c]
          (if (and (not (= :e a))
                   (= a b c))
            a
            nil))
        (first board)
        (second board)
        (last board))))
  (defn check-diagonal [board]
    (reduce
      (fn [a b]
        (or a b))
      (map
        (fn [diagonal]
          (if (and (= 1 (count (set diagonal)))
                   (not (= :e (first diagonal))))
            (first diagonal)
            nil))
        [
         [(ffirst board)
          (second (second board))
          (last (last board))]

         [(last (first board))
          (second (second board))
          (first (last board))]
         ])))
  (or
    (check-horizontal board)
    (check-vertical board)
    (check-diagonal board)))
