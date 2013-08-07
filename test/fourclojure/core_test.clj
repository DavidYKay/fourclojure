(ns fourclojure.core-test
  (:use midje.sweet
        fourclojure.core))

; (fact "I can compress a sequence"
;       (+ 1 1) => 2
; 
;       (apply str (compress "Leeeeeerrroyyy")) => "Leroy"
;       (compress [1 1 2 3 3 2 2 3]) => '(1 2 3 2 3)
;       (compress [[1 2] [1 2] [3 4] [1 2]]) => '([1 2] [3 4] [1 2])
; 
;       )

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
