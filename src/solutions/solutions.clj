(ns solutions.solutions)

;; 1
(= true true)

;; 2
(= (- 10 (* 2 3)) 4)

;; 3
(= "HELLO WORLD" (.toUpperCase "hello world"))

;; 4
(= (list :a :b :c) '(:a :b :c))

;; 5
(= '(1 2 3 4) (conj '(2 3 4) 1))
(= '(1 2 3 4) (conj '(3 4) 2 1))

;; 6
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

;; 7
(= [1 2 3 4] (conj [1 2 3] 4))
(= [1 2 3 4] (conj [1 2] 3 4))

;; 8
(= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d)))
(= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d}))

;; 9
(= #{1 2 3 4} (conj #{1 4 3} 2))

;; 10
(= 20 ((hash-map :a 10, :b 20, :c 30) :b))
(= 20 (:b {:a 10, :b 20, :c 30}))

;; 11
(= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3]))

;; 12
(= 3 (first '(3 2 1)))
(= 3 (second [2 3 4]))
(= 3 (second [2 3 4]))

;; 13
(= [20 30 40] (rest [10 20 30 40]))

;; 14
(= 8 ((fn add-five [x] (+ x 5)) 3))
(= 8 ((fn [x] (+ x 5)) 3))
(= 8 (#(+ % 5) 3))
(= 8 ((partial + 5) 3))

;; 15
(= ((partial * 2) 2) 4)
(= ((partial * 2) 3) 6)
(= ((partial * 2) 11) 22)
(= ((partial * 2) 7) 14)

;; 16
(= (#(str "Hello, " % "!") "Dave") "Hello, Dave!")
(= (#(str "Hello, " % "!") "Jenn") "Hello, Jenn!")
(= (#(str "Hello, " % "!") "Rhea") "Hello, Rhea!")

;; 17
(= '(6 7 8) (map #(+ % 5) '(1 2 3)))

;; 18
(= '(6 7) (filter #(> % 5) '(3 4 5 6 7)))

;; 19
(= (#(first (reverse %)) [1 2 3 4 5]) 5)
(= (#(first (reverse %)) '(5 4 3)) 3)
(= (#(first (reverse %)) ["b" "c" "d"]) "d")

;; 20
(= (#(second (reverse %)) (list 1 2 3 4 5)) 4)
(= (#(second (reverse %)) ["a" "b" "c"]) "b")
(= (#(second (reverse %)) [[1 2] [3 4]]) [1 2])

;; 21
(= (#(last (take (+ 1 %2) %1)) '(4 5 6 7) 2) 6)
(= (#(last (take (+ 1 %2) %1)) [:a :b :c] 0) :a)
(= (#(last (take (+ 1 %2) %1)) [1 2 3 4] 1) 2)
(= (#(last (take (+ 1 %2) %1)) '([1 2] [3 4] [5 6]) 2) [5 6])

;; 22
(= (reduce (fn[r _] (inc r)) 0 '(1 2 3 3 1)) 5)
(= (reduce (fn[r _] (inc r)) 0 "Hello World") 11)
(= (reduce (fn[r _] (inc r)) 0 [[1 2] [3 4] [5 6]]) 3)
(= (reduce (fn[r _] (inc r)) 0 '(13)) 1)
(= (reduce (fn[r _] (inc r)) 0 '(:a :b :c)) 3)

;; 23
(= ((fn[l] (reduce #(conj %1 %2) '() l)) [1 2 3 4 5]) [5 4 3 2 1])
(= ((fn[l] (reduce #(conj %1 %2) '() l)) (sorted-set 5 7 2 7)) '(7 5 2))
(= ((fn[l] (reduce #(conj %1 %2) '() l)) [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])

;; 24
(= (apply + [1 2 3]) 6)
(= (apply + (list 0 -2 5 5)) 8)
(= (apply + #{4 2 1}) 7)
(= (apply + '(0 0 -1)) -1)
(= (apply + '(1 10 3)) 14)

;; 25
(= (filter odd? #{1 2 3 4 5}) '(1 3 5))
(= (filter odd? [4 2 1 6]) '(1))
(= (filter odd? [2 2 4 6]) '())
(= (filter odd? [1 1 1 3]) '(1 1 1 3))

;; 26
(= (#(take % ((fn fib [a b] (lazy-seq (cons a (fib b (+ a b))))) 1 1)) 3) '(1 1 2))
(= (#(take % ((fn fib [a b] (lazy-seq (cons a (fib b (+ a b))))) 1 1)) 6) '(1 1 2 3 5 8))
(= (#(take % ((fn fib [a b] (lazy-seq (cons a (fib b (+ a b))))) 1 1)) 8) '(1 1 2 3 5 8 13 21))

;; 27
(defn palindrome? [l]
  (if (= 0 (count l)) true
      (if (= (first l) (last l)) (palindrome? (rest (butlast l)))
          false)))
(false? (palindrome? '(1 2 3 4 5)))
(true? (palindrome? "racecar"))
(true? (palindrome? [:foo :bar :foo]))
(true? (palindrome? '(1 1 3 3 1 1)))
(false? (palindrome? '(:a :b :c)))

;; 28
(defn flat [s]
  (if (sequential? s)
    (mapcat flat s)
    [s]))
(= (flat '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (flat ["a" ["b"] "c"]) '("a" "b" "c"))
(= (flat '((((:a))))) '(:a))

;; 29
(= (#(apply str (re-seq #"[A-Z]" %1)) "HeLlO, WoRlD!") "HLOWRD")
(empty? (#(apply str (re-seq #"[A-Z]" %1)) "nothing"))
(= (#(apply str (re-seq #"[A-Z]" %1)) "$#A(*&987Zf") "AZ")

;; 30
(= (apply str (#(map first (partition-by identity %)) "Leeeeeerrroyyy")) "Leroy")
(= (#(map first (partition-by identity %)) [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (#(map first (partition-by identity %)) [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

;; 31
(= (#(partition-by identity %) [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (#(partition-by identity %) [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (#(partition-by identity %) [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))

;; 32
(= (mapcat #(vector % %) [1 2 3]) '(1 1 2 2 3 3))
(= (mapcat #(vector % %) [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (mapcat #(vector % %) [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (mapcat #(vector % %) [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))

;; 33
(= ((fn[l r] (mapcat #(repeat r %) l)) [1 2 3] 2) '(1 1 2 2 3 3))
(= ((fn[l r] (mapcat #(repeat r %) l)) [:a :b] 4) '(:a :a :a :a :b :b :b :b))
(= ((fn[l r] (mapcat #(repeat r %) l)) [4 5 6] 1) '(4 5 6))
(= ((fn[l r] (mapcat #(repeat r %) l)) [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
(= ((fn[l r] (mapcat #(repeat r %) l)) [44 33] 2) [44 44 33 33])

;; 34
(= (#(take (- %2 %1) (iterate (partial + 1) %1)) 1 4) '(1 2 3))
(= (#(take (- %2 %1) (iterate (partial + 1) %1)) -2 2) '(-2 -1 0 1))
(= (#(take (- %2 %1) (iterate (partial + 1) %1)) 5 8) '(5 6 7))

;; 35
(= 7 (let [x 5] (+ 2 x)))
(= 7 (let [x 3, y 10] (- y x)))
(= 7 (let [x 21] (let [y 3] (/ x y))))

;; 36
(= 10 (let [z 1 y 3 x 7] (+ x y)))
(= 4 (let [z 1 y 3 x 7] (+ y z)))
(= 1 (let [z 1 y 3 x 7] z))

;; 37
(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

;; 38
(= ((fn[& args] (reduce #(if (> %1 %2) %1 %2) args)) 1 8 3 4) 8)
(= ((fn[& args] (reduce #(if (> %1 %2) %1 %2) args)) 30 20) 30)
(= ((fn[& args] (reduce #(if (> %1 %2) %1 %2) args)) 45 67 11) 67)

;; 39
(= (#(flatten (into '() (zipmap %1 %2))) [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (#(flatten (into '() (zipmap %1 %2))) [1 2] [3 4 5 6]) '(1 3 2 4))
(= (#(flatten (into '() (zipmap %1 %2))) [1 2 3 4] [5]) [1 5])
(= (#(flatten (into '() (zipmap %1 %2))) [30 20] [25 15]) [30 25 20 15])

;; 40
(= (#(butlast (interleave %2 (repeat (count %2) %1))) 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (#(butlast (interleave %2 (repeat (count %2) %1))) ", " ["one" "two" "three"])) "one, two, three")
(= (#(butlast (interleave %2 (repeat (count %2) %1))) :z [:a :b :c :d]) [:a :z :b :z :c :z :d])

;; 41
(= (#(mapcat (partial take (dec %2))(partition-all %2 %1)) [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (#(mapcat (partial take (dec %2))(partition-all %2 %1)) [:a :b :c :d :e :f] 2) [:a :c :e])
(= (#(mapcat (partial take (dec %2))(partition-all %2 %1)) [1 2 3 4 5 6] 4) [1 2 3 5 6])

;; 42
(= (#(reduce * (take % (iterate dec %))) 1) 1)
(= (#(reduce * (take % (iterate dec %))) 3) 6)
(= (#(reduce * (take % (iterate dec %))) 5) 120)
(= (#(reduce * (take % (iterate dec %))) 8) 40320)

;; 43
(defn zip-seq [l n]
  (if (= n (count (first l)))
    []
    (lazy-seq (cons (map #(nth % n) l) (zip-seq l (inc n))))))
(defn zip-by [l z]
  (zip-seq (partition-all z l) 0))
(= (zip-by [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (zip-by (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (zip-by (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

;; 44
(defn rewind-seq [l r n]
  (if (= n (count l))
    []
    (lazy-seq (cons (nth l (rem (+ r n) (count l))) ; new index
                    (rewind-seq l r (inc n))
                    ))))
(defn rewind [n l]
  (rewind-seq l (+ (count l) (rem n (count l))) 0)) ; transform negative rewind to positive

(= (rewind 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (rewind -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (rewind 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (rewind 1 '(:a :b :c)) '(:b :c :a))
(= (rewind -4 '(:a :b :c)) '(:c :a :b))

;; 45
(= '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1)))

;; 46
(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

(= 3 ((flip nth) 2 [1 2 3 4 5]))
(= true ((flip >) 7 8))
(= 4 ((flip quot) 2 8))
(= [1 2 3] ((flip take) [1 2 3 4 5] 3))

;; 47
(contains? #{4 5 6} 4)
(contains? [1 1 1 1 1] 4)
(contains? {4 :a 2 :b} 4)
(not (contains? [1 2 4] 4))

;;48
(= 6 (some #{2 7 6} [5 6 7 8]))
(= 6 (some #(when (even? %) %) [5 6 7 8]))

;; 49
(defn split-custom-at [n l]
  (let [all (partition-all n l)]
    (cons (first all)
          (vector (mapcat identity (rest all))))))

(= (split-custom-at 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (split-custom-at 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (split-custom-at 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])

;; 50
(= (set ((fn[l] (vals (group-by type l )))  [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set ((fn[l] (vals (group-by type l ))) [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set ((fn[l] (vals (group-by type l ))) [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

;; 51
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))

;; 52
(= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e]))

;; 53
(defn increasing-sub-seq [s] (->>
                              s
                              (partition 2 1)
                              (partition-by (fn[[f s]] (- s f) ))
                              (filter #(= (inc (first (first %)))
                                          (second (first %))))
                              (sort-by count #(compare %2 %1))
                              (first)
                              (flatten)
                              (partition-by identity)
                              (map first)))

(= (increasing-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (increasing-sub-seq [5 6 1 3 2 7]) [5 6])
(= (increasing-sub-seq [2 3 3 4 5]) [3 4 5])
(= (increasing-sub-seq [7 6 5 4]) [])

;; 54
(defn part [n c]
  (if (> n (count c))
    []
    (cons (take n c) (part n (drop n c)))))

(= (part 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (part 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (part 3 (range 8)) '((0 1 2) (3 4 5)))

;; 55
(defn freqs [s]
  (->>
   s
   (group-by identity)
   (vals)
   (map (fn [g] [(first g) (count g)]))
   (into {})))
(= (freqs [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
(= (freqs [:b :a :b :a :b]) {:a 2, :b 3})
(= (freqs '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})

;; 56
(defn dist [s]
  (reduce #(if (seq (filter (partial = %2) %1))
             %1
             (conj %1 %2)) [] s))

(= (dist [1 2 1 3 1 2 4]) [1 2 3 4])
(= (dist [:a :a :b :b :c :c]) [:a :b :c])
(= (dist '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
(= (dist (range 50)) (range 50))

;; 57
(= '(5 4 3 2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))

;; 58
(defn compose [& fs]
  (fn [& args] (first(reduce #(vector (apply %2 %1)) args (reverse fs)))))

(= [3 2 1] ((compose rest reverse) [1 2 3 4]))
(= 5 ((compose (partial + 3) second) [1 2 3 4]))
(= true ((compose zero? #(mod % 8) +) 3 5 7 9))
(= "HELLO" ((compose #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

;; 59
(defn juxta [& fs]
  (fn [& args]
    (map #(apply % args) fs)))

(= [21 6 1] ((juxta + max min) 2 3 5 1 6 4))
(= ["HELLO" 5] ((juxta #(.toUpperCase %) count) "hello"))
(= [2 6 4] ((juxta :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;; 60
(defn red
  ([f c] (red f (first c) (rest c)))
  ([f g c]
   (if (seq c)
     (lazy-seq (cons g (red f (f g (first c)) (rest c))))
     [g])))

(= (take 5 (red + (range 10))) [0 1 3 6 10])
(= (red conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last (red * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

;; 61
(= (#(into {} (map vec (partition-all 2 (interleave %1 %2)))) [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
(= (#(into {} (map vec (partition-all 2 (interleave %1 %2)))) [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
(= (#(into {} (map vec (partition-all 2 (interleave %1 %2)))) [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})

;; 62
(defn it[f s]
  (lazy-seq (cons s (it f (apply f [s])))))

(= (take 5 (it #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (it inc 0)) (take 100 (range)))
(= (take 9 (it #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

;; 63
(defn grouping [f s]
  (apply merge-with concat (map (fn[e] {(f e) [e]}) s)))

(= (grouping #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
(= (grouping #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
(= (grouping count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})

;; 64
(= 15 (reduce + [1 2 3 4 5]))
(=  0 (reduce + []))
(=  6 (reduce + 1 [2 3]))

;; 65
(defn type-info [c]
  (cond
    (= (get (conj c {:a 1}) :a) 1) :map
    (= (count (conj c :a)) (count (conj (conj c :a) :a))) :set
    (= (last (conj (conj c :a) :b)) :b) :vector
    :else :list))
(= :map (type-info {:a 1, :b 2}))
(= :list (type-info (range (rand-int 20))))
(= :vector (type-info [1 2 3 4 5 6]))
(= :set (type-info #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map type-info [{} #{} [] ()]))

;; 66
(defn gcd [x y]
  (loop [a (max x y)
         b (min x y)]
    (let [r (rem a b)]
      (if (= r 0)
        b
        (recur b r )))))

(= (gcd 2 4) 2)
(= (gcd 10 5) 5)
(= (gcd 5 7) 1)
(= (gcd 1023 858) 33)

;; 67
(defn primes [n]
  (take n ((fn sieve
             ([] (sieve 2))
             ([n]
              (lazy-seq (cons n (filter #(< 0 (rem % n)) (sieve (inc n))))))))))

(= (primes 2) [2 3])
(= (primes 5) [2 3 5 7 11])
(= (last (primes 100)) 541)

;; 68
(= [7 6 5 4 3]
   (loop [x 5
          result []]
     (if (> x 0)
       (recur (dec x) (conj result (+ 2 x)))
       result)))

;; 69
(defn merge-w [f & c]
  (reduce (fn[g [k v]] (update-in g [k] #(if (nil? %)
                                           v
                                           (f % v))))
          {}
          (mapcat identity (map vec c))))


(= (merge-w * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})
(= (merge-w - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})
(= (merge-w concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})

;; 70
(defn word-sort [s]
  (sort #(compare (.toLowerCase %1) (.toLowerCase %2)) (clojure.string/split s #"\W+")))

(= (word-sort  "Have a nice day.")
   ["a" "day" "Have" "nice"])
(= (word-sort  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])
(= (word-sort  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])

;; 71
(= (last (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))
   5)

;; 72
(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   1)

;; 73
(defn winning
  [c]
  (->> c
       (partition-by identity)
       (filter #(and (= (count %) (count c)) (not=(first %) :e)))
       (first)))
(defn rows [c] c)
(defn cols [c] (apply map vector c))
(defn diags [c]
  (let [diag (fn [x] (reduce #(conj %1 (nth %2 (count %1))) [] x))]
    [(diag c) (diag (reverse c))]))

(defn tic-tac-toe-winner [m]
  (->>
   (concat (rows m) (cols m) (diags m))
   (filter winning)
   (first)
   (first)))

(= nil (tic-tac-toe-winner [[:e :e :e]
                            [:e :e :e]
                            [:e :e :e]]))
(= :x (tic-tac-toe-winner [[:x :e :o]
                           [:x :e :e]
                           [:x :e :o]]))
(= :o (tic-tac-toe-winner [[:e :x :e]
                           [:o :o :o]
                           [:x :e :x]]))
(= nil (tic-tac-toe-winner [[:x :e :o]
                            [:x :x :e]
                            [:o :x :o]]))
(= :x (tic-tac-toe-winner [[:x :e :e]
                           [:o :x :e]
                           [:o :e :x]]))
(= :o (tic-tac-toe-winner [[:x :e :o]
                           [:x :o :e]
                           [:o :e :x]]))
(= nil (tic-tac-toe-winner [[:x :o :x]
                            [:x :o :x]
                            [:o :x :o]]))

;; 74
(defn perfect-squares
  [s]
  (->> (clojure.string/split s #",")
       (map #(Integer/parseInt %))
       (map #(vector (int (Math/sqrt %)) %))
       (filter #(= (* (first %) (first %)) (second %)))
       (map #(second %))
       (clojure.string/join ",")))

(= (perfect-squares "4,5,6,7,8,9") "4,9")
(= (perfect-squares "15,16,25,36,37") "16,25,36")

;; 75
(defn euler-totient
  [x]
  (let [gcd (fn gcd2 [x y]
              (loop [a (max x y)
                     b (min x y)]
                (let [r (rem a b)]
                  (if (= r 0)
                    b
                    (recur b r )))))]
    (if (= x 1) 1 (->> (range 1 x)
                       (filter #(= (gcd % x) 1))
                       (count)))))
(= (euler-totient 1) 1)
(= (euler-totient 10) (count '(1 3 7 9)) 4)
(= (euler-totient 40) 16)
(= (euler-totient 99) 60)

;; 76
(= [1 3 5 7 9 11]
   (letfn
       [(foo [x y] #(bar (conj x y) y))
        (bar [x y] (if (> (last x) 10)
                     x
                     #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

;; 77
(defn anagrams
  [c]
  (set (filter #(<= 2 (count %)) (map set (vals (group-by #(set (seq %)) c))))))

(= (anagrams ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
(= (anagrams ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

;; 78
(defn tramp
  [f & args]
  (let [result (apply f args)]
    (if (instance? clojure.lang.IFn result) (tramp result) result)))

(= (letfn [(triple [x] #(sub-two (* 3 x)))
           (sub-two [x] #(stop?(- x 2)))
           (stop? [x] (if (> x 50) x #(triple x)))]
     (tramp triple 2))
   82)

(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
           (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
     (map (partial tramp my-even?) (range 6)))
   [true false true false true false])

;; 79
(defn left-subtree
  [t]
  (map butlast (next t)))
(defn right-subtree [t]
  (map next (next t)))
(defn leaf? [t]
  (= 1 (count t)))
(defn root-val [t]
  (first (first t)))
(defn triangle-min-path
  [t]
  (let [v (root-val t)]
    (if (leaf? t)
      v
      (min (+ v (triangle-min-path (left-subtree t)))
           (+ v (triangle-min-path (right-subtree t)))))))

(= 7 (triangle-min-path '([1]
                          [2 4]
                          [5 1 4]
                          [2 3 4 5])))

(= 20 (triangle-min-path '([3]
                           [2 4]
                           [1 9 3]
                           [9 9 2 4]
                           [4 6 6 7 8]
                           [5 7 3 5 1 4])))

;; 80
(defn divisors
  [n]
  (= n (reduce + (filter #(= 0 (rem n %))(range 1 n)))))

(= (divisors 6) true)
(= (divisors 7) false)
(= (divisors 496) true)
(= (divisors 500) false)
(= (divisors 8128) true)

;; 81
(defn intersect
  [s1 s2]
  (->> (clojure.set/union s1 s2)
       (filter (fn[x] (contains? s1 x)))
       (filter (fn[x] (contains? s2 x)))
       set))

(= (intersect #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (intersect #{0 1 2} #{3 4 5}) #{})
(= (intersect #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

;; 82
(defn ops-count [s1 s2]
  (cond (empty? s1) (count s2)
        (empty? s2) (count s1)
        (= (first s1) (first s2)) (ops-count (next s1) (next s2))
        :else (inc (min (ops-count (next s1) s2)
                        (ops-count s1 (next s2))
                        (ops-count (next s1) (next s2))))))

;; TODO

;; 83
(= false ((fn[& args] (true? (and (some true? args) (some false? args)))) false false))
(= true ((fn[& args] (true? (and (some true? args) (some false? args)))) true false))
(= false ((fn[& args] (true? (and (some true? args) (some false? args)))) true))
(= true ((fn[& args] (true? (and (some true? args) (some false? args)))) false true false))
(= false ((fn[& args] (true? (and (some true? args) (some false? args)))) true true true))
(= true ((fn[& args] (true? (and (some true? args) (some false? args)))) true true true false))

;; 84
(defn transitive-relations [x s]
  (when (seq s)
    (let [rels (filter (fn [[a _]] (= a x)) s)
          in-rel (map second rels)
          other (remove (set rels) s)]
      (lazy-seq (apply concat
                       in-rel
                       (map #(transitive-relations % other) in-rel)
                       )))))

(defn all-transitive-rels [rs]
  (->> rs
       (map first)
       (mapcat (fn[r] (map #(vector r %) (transitive-relations r rs))))
       set))

(let [divides #{[8 4] [9 3] [4 2] [27 9]}]
  (= (all-transitive-rels divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))
(let [more-legs
      #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
  (= (all-transitive-rels more-legs)
     #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
       ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))
(let [progeny
      #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
  (= (all-transitive-rels progeny)
     #{["father" "son"] ["father" "grandson"]
       ["uncle" "cousin"] ["son" "grandson"]}))

;; 85
(defn power-set [s]
  (cond
    (= 0 (count s)) #{#{}}
    (= 1 (count s)) #{#{} s}
    :else (let [first (first s)
                sub-sets (power-set (set (rest s)))]
            (clojure.set/union sub-sets
                               (set (map #(conj % first) sub-sets))))))

(= (power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
(= (power-set #{}) #{#{}})
(= (power-set #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
(= (count (power-set (into #{} (range 10)))) 1024)

;; 86
(defn to-digits [n]
  (if (= n 0)
    []
    (lazy-seq (cons (rem n 10) (to-digits (quot n 10))) )))
(defn happy?
  ([n] (happy? n #{}))
  ([n visited]
   (let [sums (reduce + 0 (map #(* % %) (to-digits n)))]
     (cond
       (= 1 sums) true
       (contains? visited sums) false
       :else (happy? sums (conj visited sums))))))
(= (happy? 7) true)
(= (happy? 986543210) true)
(= (happy? 2) false)
(= (happy? 3) false)

;; 88
(defn sym-diff [s1 s2]
  (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1)))
(= (sym-diff #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (sym-diff #{:a :b :c} #{}) #{:a :b :c})
(= (sym-diff #{} #{4 5 6}) #{4 5 6})
(= (sym-diff #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})

;; 89
;; TODO

;; 90
(defn cart [s1 s2]
  (set (mapcat #(map vector (repeat (count s2) %) s2) s1)))

(= (cart #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
(= (cart #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
(= 300 (count (cart (into #{} (range 10))
                    (into #{} (range 30)))))

;; 91 (Hard)
;; TODO

;; 92 (Hard)
;; TODO

;; 93
(defn flatten-custom
  [xs]
  (cond
    (and (coll? xs) (not (coll? (first xs)))) [xs]
    (coll? xs) (mapcat flatten-custom xs)
    :else [xs]))
(= (flatten-custom [["Do"] ["Nothing"]])
   [["Do"] ["Nothing"]])
(= (flatten-custom [[[[:a :b]]] [[:c :d]] [:e :f]])
   [[:a :b] [:c :d] [:e :f]])
(= (flatten-custom '((1 2)((3 4)((((5 6)))))))
   '((1 2)(3 4)(5 6)))

;; 94 (Hard)
;; TODO

;; 95
(defn is-tree?[v]
  (or (nil? v) (and
                (coll? v)
                (= 3 (count v))
                (is-tree? (second v))
                (is-tree? (second (next v))))))
(= (is-tree? '(:a (:b nil nil) nil))
   true)
(= (is-tree? '(:a (:b nil nil)))
   false)
(= (is-tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)
(= (is-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)
(= (is-tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)
(= (is-tree? [1 [2 [3 [4 false nil] nil] nil] nil])
   false)
(= (is-tree? '(:a nil ()))
   false)

;; 96
(defn sym-tree?
  ([[_ l r]] (sym-tree? l r))
  ([[v1 l1 r1] [v2 l2 r2]]
   (if (and (nil? v1) (nil? v2))
     true
     (and (= v1 v2)
          (sym-tree? r1 l2)
          (sym-tree? l1 r2)))))

(= (sym-tree? '(:a (:b nil nil) (:b nil nil))) true)
(= (sym-tree? '(:a (:b nil nil) nil)) false)
(= (sym-tree? '(:a (:b nil nil) (:c nil nil))) false)
(= (sym-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
               [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)
(= (sym-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
               [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)
(= (sym-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
               [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)

;; 97
(defn pascal-triangle [n]
  (if (= n 1)
    [1]
    (lazy-seq (concat (cons 1 (map #(+ (first %) (second %)) (partition 2 1 (pascal-triangle (dec n))))) [1]))))

(= (pascal-triangle 1) [1])
(= (map pascal-triangle (range 1 6))
   [     [1]
    [1 1]
    [1 2 1]
    [1 3 3 1]
    [1 4 6 4 1]])
(= (pascal-triangle 11)
   [1 10 45 120 210 252 210 120 45 10 1])

;; 98
(defn equiv
  [f xs]
  (set (map set (vals (group-by f xs)))))
(= (equiv #(* % %) #{-2 -1 0 1 2})
   #{#{0} #{1 -1} #{2 -2}})
(= (equiv #(rem % 3) #{0 1 2 3 4 5 })
   #{#{0 3} #{1 4} #{2 5}})
(= (equiv identity #{0 1 2 3 4})
   #{#{0} #{1} #{2} #{3} #{4}})
(= (equiv (constantly true) #{0 1 2 3 4})
   #{#{0 1 2 3 4}})

;; 99
(defn product-digits
  [n1 n2]
  (map #(- (int %) 48) (str (* n1 n2))))
(= (product-digits 1 1) [1])
(= (product-digits 99 9) [8 9 1])
(= (product-digits 999 99) [9 8 9 0 1])

;; 100
(defn lcm
  [& args]
  (letfn [(gcd [x y]
            (loop [a (max x y)
                   b (min x y)]
              (let [r (rem a b)]
                (if (= r 0)
                  b
                  (recur b r)))))]
    (reduce #(/ (* %1 %2) (gcd %1 %2)) args)))

(== (lcm 2 3) 6)
(== (lcm 5 3 7) 105)
(== (lcm 1/3 2/5) 2)
(== (lcm 3/4 1/6) 3/2)
(== (lcm 7 5/7 2 3/5) 210)

;; 101 (Hard)
;; TODO

;; 102
(defn into-camel-case
  [s]
  (let [parts (clojure.string/split s #"-")]
    (clojure.string/join (cons (first parts) (map clojure.string/capitalize (rest parts))))))

(= (into-camel-case "something") "something")
(= (into-camel-case "multi-word-key") "multiWordKey")
(= (into-camel-case "leaveMeAlone") "leaveMeAlone")

;; 103
(defn comb [n v]
  (let [xs (vec v)]
    (if (= 1 n)
      (into #{} (map #(hash-set %) xs))
      (set
       (mapcat
        (fn[i]
          (into #{} (map #(conj % (nth xs i)) (comb (dec n) (drop (inc i) xs)))))
        (range (inc (- (count xs) n))))))))


;; 104
(defn roman
  [n]
  (let [signs {1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C" 90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I"}
        nums  [1000 900 500 400 100 90 50 40 10 9 5 4 1]]
    (apply str (second
                (reduce
                 (fn [[x l] c]
                   [(rem x c) (conj l (apply str (repeat (quot x c) (get signs c))))])
                 [n []] nums)))))
(= "I" (roman 1))
(= "XXX" (roman 30))
(= "IV" (roman 4))
(= "CXL" (roman 140))
(= "DCCCXXVII" (roman 827))
(= "MMMCMXCIX" (roman 3999))
(= "XLVIII" (roman 48))

;; 105
(defn keys-vals [xs]
  (second (reduce (fn[[c m] x]
                    (if (keyword? x)
                      [x (assoc m x [])]
                      [c (update-in m [c] #(conj % x))]))
                  [nil {}]
                  xs)))

(= {} (keys-vals []))
(= {:a [1]} (keys-vals [:a 1]))
(= {:a [1], :b [2]} (keys-vals [:a 1, :b 2]))
(= {:a [1 2 3], :b [], :c [4]} (keys-vals [:a 1 2 3 :b :c 4]))

;; 106 (Hard)
;; TODO

;; 107
(defn expo [pow]
  (fn[x] (reduce * (repeat pow x))))

(= 256 ((expo 2) 16),
   ((expo 8) 2))
(= [1 8 27 64] (map (expo 3) [1 2 3 4]))
(= [1 2 4 8 16] (map #((expo %) 2) [0 1 2 3 4]))

;; 108
(defn min-common
  [& xs]
  (if (apply = (map first xs))
    (ffirst xs)
    (let [minimum (apply min (map first xs))]
      (apply min-common (map (partial drop-while #(>= minimum %)) xs)))))

(= 3 (min-common [3 4 5]))
(= 4 (min-common [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(= 7 (min-common (range) (range 0 100 7/6) [2 3 5 7 11 13]))
(= 64 (min-common (map #(* % % %) (range)) ;; perfect cubes
                  (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                  (iterate inc 20))) ;; at least as large as 20

;; 110
(defn pronunciations
  [xs]
  (rest (iterate #(vec (mapcat (fn[p] [(count p) (first p)])
                               (partition-by identity %)))
                 xs)))

(= [[1 1] [2 1] [1 2 1 1]] (take 3 (pronunciations [1])))
(= [3 1 2 4] (first (pronunciations [1 1 1 4 4])))
(= [1 1 1 3 2 1 3 2 1 1] (nth (pronunciations [1]) 6))
(= 338 (count (nth (pronunciations [3 2]) 15)))

;; TODO
;; 111 (Hard)

;; 112
