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
