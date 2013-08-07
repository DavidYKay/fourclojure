(ns fourclojure.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn compress [s]
  ;(mapcat (fn [a b]
            ;(if (= a b) a
              ;[a b]))
          ;s))
  ;(mapcat (fn [a b] (if (= a b) a [a b])) s))
  (reduce (fn [a b]
            (if (= a b)
              a
              (flatten [a b])))
          s))

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
