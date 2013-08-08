(ns fourclojure.core-test
  (:use midje.sweet
        fourclojure.core))

(fact "I can compress a sequence"
      (+ 1 1) => 2

      (apply str (compress "Leeeeeerrroyyy")) => "Leroy"
      (compress [1 1 2 3 3 2 2 3]) => '(1 2 3 2 3)
      (compress [[1 2] [1 2] [3 4] [1 2]]) => '([1 2] [3 4] [1 2])

      )

(fact "I can find the maximum in a sequence"
      (maxi 1 8 3 4) => 8
      (maxi 30 20) => 30
      (maxi 45 67 11) => 67
      )

(fact "I can weave two sequences together"
      (weave [1 2 3] [:a :b :c]) => '(1 :a 2 :b 3 :c)
      (weave [1 2 3 4] [5]) => [1 5]
      (weave [1 2] [3 4 5 6]) => '(1 3 2 4)
      (weave [30 20] [25 15]) => [30 25 20 15]
      )

(fact "I can implement range"
      (my-range 1 4)  => '(1 2 3)
      (my-range -2 2) => '(-2 -1 0 1)
      (my-range 5 8)  => '(5 6 7))

(fact "I can check for an existing nil value"
      (has-nil? :c {:a nil :b 2}) => false
      (has-nil? :b {:a nil :b 2}) => false
      (has-nil? :a {:a nil :b 2}) => true
      )

(fact "I can make an infix calculator"
      (infix-calc 2 + 5)                            => 7
      (infix-calc 38 + 48 - 2 / 2)                  => 42
      (infix-calc 10 / 2 - 1 * 2)                  => 8
      (infix-calc 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)  => 72
      )

(fact "I can make default map keys. Prob156"
      (default-map 0 [:a :b :c])        => { :a 0 :b 0 :c 0}
      (default-map "x" [1 2 3])         => { 1 "x" 2 "x" 3 "x"}
      (default-map [:a :b] [:foo :bar]) => { :foo [:a :b] :bar [:a :b] }
      )

(fact "I can write my own comparison. Prob166"
      (my-compare < 5 1) => :gt
      (my-compare (fn [x y]
                    (< (count x) (count y)))
                  "pear" "plum") => :eq
      (my-compare (fn [x y]
                    (< (mod x 5) (mod y 5)))
                  21 3)         => :lt
      (my-compare > 0 2) => :gt
      )

(fact "I can produce pascal's triangle. Prob97"
      (pascal 1) => [1]
      (map pascal (range 1 6)) => [[1]
                                   [1 1]
                                   [1 2 1]
                                   [1 3 3 1]
                                   [1 4 6 4 1]]
      (pascal 11) => [1 10 45 120 210 252 210 120 45 10 1]
      )

(fact "I can interpose a seq"
      (my-interpose 0 [1 2 3]) => [1 0 2 0 3]
      (apply str (my-interpose ", " ["one" "two" "three"])) => "one, two, three"
      (my-interpose :z [:a :b :c :d]) => [:a :z :b :z :c :z :d]
      )

(fact "I can find the GCD of two numbers"
      (my-gcd 2 4) => 2
      (my-gcd 10 5) => 5
      (my-gcd 5 7) => 1
      (my-gcd 1023 858) => 33
      )

(fact "I can find the GCD of two numbers"
      ((my-closure 2) 16) => 256
      ((my-closure 8) 2)  => 256

      (map (my-closure 3) [1 2 3 4])        => [1 8 27 64]
      (map #((my-closure %) 2) [0 1 2 3 4]) => [1 2 4 8 16]
)

(fact "I can perform intersection of two sets"
      (my-intersection #{0 1 2 3} #{2 3 4 5}) => #{2 3}
      (my-intersection #{0 1 2} #{3 4 5}) => #{}
      (my-intersection #{:a :b :c :d} #{:c :e :a :f :d}) => #{:a :c :d}
      )


(fact "I can split a sequence. Prob49"
      (my-split 3 [1 2 3 4 5 6])       => [[1 2 3] [4 5 6]]
      (my-split 1 [:a :b :c :d])       => [[:a] [:b :c :d]]
      (my-split 2 [[1 2] [3 4] [5 6]]) => [[[1 2] [3 4]] [[5 6]]]
      )


(fact "I can read a binary number. Prob122"
      (read-binary "0")                     => 0
      (read-binary "111")                   => 7
      (read-binary "1000")                  => 8
      (read-binary "1001")                  => 9
      (read-binary "11111111")              => 255
      (read-binary "10101010101")           => 1365
      (read-binary "1111111111111111")      => 65535
      )

(fact "I can detect pairwise disjoint sets"
      (pairwise-disjoint #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}}) => true

      (pairwise-disjoint #{#{:a :b :c :d :e}
                           #{:a :b :c :d}
                           #{:a :b :c}
                           #{:a :b}
                           #{:a}}) => false

      (pairwise-disjoint #{#{[1 2 3] [4 5]}
                           #{[1 2] [3 4 5]}
                           #{[1] [2] 3 4 5}
                           #{1 2 [3 4] [5]}}) => true

      (pairwise-disjoint #{#{'a 'b}
                           #{'c 'd 'e}
                           #{'f 'g 'h 'i}
                           #{''a ''c ''f}}) => true

      (pairwise-disjoint #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                           #{#{:x :y :z} #{:x :y} #{:z} #{}}
                           #{'[:x :y :z] [:x :y] [:z] [] {}}}) => false

      (pairwise-disjoint #{#{(= "true") false}
                           #{:yes :no}
                           #{(class 1) 0}
                           #{(symbol "true") 'false}
                           #{(keyword "yes") ::no}
                           #{(class '1) (int \0)}}) => false

      (pairwise-disjoint #{#{distinct?}
                           #{#(-> %) #(-> %)}
                           #{#(-> %) #(-> %) #(-> %)}
                           #{#(-> %) #(-> %) #(-> %)}}) => true

      (pairwise-disjoint #{#{(#(-> *)) + (quote mapcat) #_ nil}
                           #{'+ '* mapcat (comment mapcat)}
                           #{(do) set contains? nil?}
                           #{, , , #_, , empty?}}) => false
      )

(fact "I can re-implement iterate. Prob62"
      (take 100
            (my-iterate inc 0)) => (take 100 (range))
      (take 5 (my-iterate #(* 2 %) 1)) => [1 2 4 8 16]
      (take 9 (my-iterate #(inc (mod % 3)) 1)) => (take 9 (cycle [1 2 3]))
      )

(fact "I can re-implement zipmap. Prob61"
      (my-zipmap [:a :b :c] [1 2 3]) => {:a 1, :b 2, :c 3}
      (my-zipmap [1 2 3 4] ["one" "two" "three"]) => {1 "one", 2 "two", 3 "three"}
      (my-zipmap [:foo :bar] ["foo" "bar" "baz"]) => {:foo "foo", :bar "bar"}
      )

(fact "I can drop every Nth item. Prob41."
      (drop-every-nth [1 2 3 4 5 6 7 8] 3) => [1 2 4 5 7 8]
      (drop-every-nth [:a :b :c :d :e :f] 2) => [:a :c :e]
      (drop-every-nth [1 2 3 4 5 6] 4) => [1 2 3 5 6]
      )

(fact "I can find half-truths. Prob83"
      (half-truth false false) => false
      (half-truth true false) => true
      (half-truth true) => false
      (half-truth false true false) => true
      (half-truth true true true) => false
      (half-truth true true true false) => true
      )

;(fact "Looking glass. Prob126"
  ;(let [x Class]
    ;(and (= (class x)
            ;x)
         ;x)) => true
  ;)

(fact "Can calculate the dot product. Prob143"
      (dot-product [0 1 0] [1 0 0]) => 0
      (dot-product [1 1 1] [1 1 1]) => 3
      (dot-product [1 2 3] [4 5 6]) => 32
      (dot-product [2 5 6] [100 10 1]) => 256
      )

(fact "I know advanced destructuring. Prob51."
      (let [[a b & c :as d] [1 2 3 4 5]]
        [a b c d]) => [1 2 [3 4 5] [1 2 3 4 5]]
      )



(fact "Cartesian Product. Prob90"
      (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"}) =>
      #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
        ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
        ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}

      (cartesian-product #{1 2 3} #{4 5}) =>
      #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}

      (count (cartesian-product (into #{} (range 10))
                                (into #{} (range 30)))) => 300
      )

(fact "Product Digits. Prob99"
      (product-digits 1 1) => [1]
      (product-digits 99 9) => [8 9 1]
      (product-digits 999 99) => [9 8 9 0 1]
      )

(fact "Group a Sequence. Prob63"
      (group-sequence #(> % 5)
                      [1 3 6 8])                     => {false [1 3],
                                                         true [6 8]}
      (group-sequence #(apply / %)
                      [[1 2] [2 4] [4 6] [3 6]]) => {1/2 [[1 2] [2 4] [3 6]],
                                                     2/3 [[4 6]]}
      (group-sequence count
                      [[1] [1 2] [3] [1 2 3] [2 3]])    => {1 [[1] [3]],
                                                            2 [[1 2] [2 3]],
                                                            3 [[1 2 3]]}
      )


(fact "Can find the sum of squares. Prob120"
  (sum-squares (range 10))   => 8
  (sum-squares (range 30))   => 19
  (sum-squares (range 100))  => 50
  (sum-squares (range 1000)) => 50
)

(fact "Detect a binary tree. Prob95"
      (is-binary-tree? '(:a (:b nil nil) nil))                  => true
      (is-binary-tree? '(:a (:b nil nil)))                      => false
      (is-binary-tree? [1 nil [2 [3 nil nil] [4 nil nil]]])     => true
      (is-binary-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]]) => false
      (is-binary-tree? [1 [2 [3 [4 nil nil] nil] nil] nil])     => true
      (is-binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil])   => false
      (is-binary-tree? '(:a nil ()))                            => false
      )

(fact "Recognize a playing card. Prob128."
      (rec-playing-card "DQ") => {:suit :diamond :rank 10}
      (rec-playing-card "H5") => {:suit :heart   :rank 3}
      (rec-playing-card "CA") => {:suit :club    :rank 12}
      (map (comp :rank rec-playing-card str) '[S2 S3 S4 S5 S6 S7 S8 S9 ST SJ SQ SK SA]) => (range 13)
      )

(fact "I can re-implement Map. Prob118."
      (my-map inc [2 3 4 5 6]) => [3 4 5 6 7]
      (my-map (fn [_] nil) (range 10)) => (repeat 10 nil)
      (->> (my-map inc (range)) (drop (dec 1000000)) (take 2)) => [1000000 1000001]
      )
