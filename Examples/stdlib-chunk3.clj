;; String functions
(defn starts-with? [s prefix]
  (= (subs s 0 (count prefix)) prefix))

(defn ends-with? [s suffix]
  (let [offset (- (count s) (count suffix))]
    (if (< offset 0) false
      (= (subs s offset) suffix))))

(defn blank? [s]
  (or (null? s) (= (trim s) "")))

(defn pad-left [s n ch]
  (let [deficit (- n (count s))]
    (if (<= deficit 0) s
      (str (join "" (map (fn [_] ch) (range 0 deficit))) s))))

(defn pad-right [s n ch]
  (let [deficit (- n (count s))]
    (if (<= deficit 0) s
      (str s (join "" (map (fn [_] ch) (range 0 deficit)))))))

(defn capitalize [s]
  (if (blank? s) s
    (str (upper-case (subs s 0 1)) (lower-case (subs s 1)))))

;; Very useful for parsing GreyScript output:
(defn lines [s]
  (split s "\n"))

(defn unlines [coll]
  (join coll "\n"))

;; Collection utilities
;; Clojure: (partition 3 [1 2 3 4 5 6]) -> ((1 2 3) (4 5 6))
(defn partition [n coll]
  (loop [remaining coll acc []]
    (if (empty? remaining)
      acc
      (recur (drop n remaining)
             (conj acc (take n remaining))))))

;; (group-by even? [1 2 3 4]) -> {true [2 4] false [1 3]}
(defn group-by [f coll]
  (reduce
    (fn [m x]
      (let [k (f x)]
        (assoc m k (conj (get m k []) x))))
    {}
    coll))

;; (frequencies [:a :b :a :c :a :b]) -> {:a 3 :b 2 :c 1}
(defn frequencies [coll]
  (reduce
    (fn [m x] (assoc m x (inc (get m x 0))))
    {}
    coll))

;; (zip [1 2 3] [:a :b :c]) -> [[1 :a] [2 :b] [3 :c]]
(defn zip [a b]
  (map (fn [pair] pair)
       (map list a b)))

;; (zipmap [:a :b] [1 2]) -> {:a 1 :b 2}
(defn zipmap [keys vals]
  (reduce
    (fn [m pair] (assoc m (first pair) (second pair)))
    {}
    (zip keys vals)))

;; (distinct [1 2 1 3 2]) -> [1 2 3]
(defn distinct [coll]
  (reduce
    (fn [acc x]
      (if (some? (fn [y] (= x y)) acc)
        acc
        (conj acc x)))
    []
    coll))

;; (flatten-1 [[1 2] [3 4]]) — one level only, unlike flatten
(defn flatten-1 [coll]
  (reduce concat [] coll))

;; (index-by :name [{:name "a"} {:name "b"}])
;; -> {"a" {:name "a"} "b" {:name "b"}}
(defn index-by [f coll]
  (reduce (fn [m x] (assoc m (f x) x)) {} coll))

;; (sorted-by :age [{:age 3} {:age 1}])
(defn sorted-by [f coll]
  (let [pairs (map (fn [x] [(f x) x]) coll)]
    ;; bubble sort — good enough for small GreyHack lists
    (map second
         (reduce
           (fn [acc _]
             (loop [xs acc result []]
               (if (< (count xs) 2)
                 (concat result xs)
                 (if (<= (first (first xs)) (first (second xs)))
                   (recur (rest xs) (conj result (first xs)))
                   (recur (cons (first xs) (rest (rest xs)))
                          (conj result (second xs)))))))
           pairs
           (range 0 (count pairs))))))
