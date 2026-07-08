;; Map operations
;; (select-keys {:a 1 :b 2 :c 3} [:a :c]) -> {:a 1 :c 3}
(defn select-keys [m ks]
  (reduce (fn [acc k]
            (if (contains? m k)
              (assoc acc k (get m k))
              acc))
          {} ks))

;; (rename-keys {:a 1} {:a :b}) -> {:b 1}
(defn rename-keys [m kmap]
  (reduce (fn [acc [old-k new-k]]
            (if (contains? acc old-k)
              (-> acc
                  (assoc new-k (get acc old-k))
                  (dissoc old-k))
              acc))
          m
          (zip (keys kmap) (vals kmap))))

;; (update {:count 0} :count inc)
(defn update [m k f & args]
  (assoc m k (apply f (cons (get m k) args))))

;; (merge {:a 1} {:b 2} {:a 99}) -> {:a 99 :b 2}
(defn merge [& maps]
  (reduce (fn [acc m]
            (reduce (fn [a k] (assoc a k (get m k)))
                    acc
                    (keys m)))
          {}
          maps))

(defn memoize [f]
  (let [cache (hash-map)]
    (fn [& args]
      (let [k (str args)]
        (if (contains? cache k)
          (get cache k)
          (let [result (apply f args)]
            (set! cache (assoc cache k result))
            result))))))

;; Atoms!
(defn atom [init]
  (hash-map :value init))

(defn deref [a]
  (get a :value))

(defn swap! [a f & args]
  (let [new-val (apply f (cons (deref a) args))]
    (set! a (assoc a :value new-val))
    new-val))

(defn reset! [a v]
  (set! a (assoc a :value v))
  v)

;; Transducers
;; Single-pass map+filter+reduce
(defn transduce [xforms f init coll]
  (let [xf (apply comp xforms)]
    (reduce (xf f) init coll)))

(defn mapping [f]
  (fn [rf]
    (fn [acc x] (rf acc (f x)))))

(defn filtering [pred]
  (fn [rf]
    (fn [acc x] (if (pred x) (rf acc x) acc))))

(defn taking [n]
  (let [count (atom 0)]
    (fn [rf]
      (fn [acc x]
        (if (< @count n)
          (do (set! count (inc @count)) (rf acc x))
          acc)))))

(defn keep [f coll]
  (filter (fn [x] (not (null? x)))
          (map f coll)))

(defn mapcat [f coll]
  (flatten-1 (map f coll)))

;; Error handling
;; Wrap a value as ok or err, no exceptions to catch at call site
(defn ok [v]   (hash-map :ok true  :value v))
(defn err [msg](hash-map :ok false :error msg))
(defn ok? [r]  (get r :ok))

;; Greyhack specific functions
(defn open-ports [computer]
  (filter (fn [p] (not (.is_closed p)))
          (.get_ports computer)))

(defn root? []
  (= (active_user) "root"))

(defn try-connect [ip port user pass]
  (try
    (let [sh (.connect_service (get_shell) ip port user pass)]
      (if (null? sh)
        (throw "null shell")
        sh))
    (catch [e] null)))

(defn get-in [m path]
  (reduce (fn [cur k]
            (if (null? cur) null
              (get cur k)))
          m path))

(defn assoc-in [m path v]
  (if (= (count path) 1)
    (assoc m (first path) v)
    (assoc m (first path)
             (assoc-in (get m (first path) {})
                       (rest path) v))))

(defn update-in [m path f & args]
  (assoc-in m path
    (apply f (cons (get-in m path) args))))

(defn retry [n f & args]
  (loop [attempts n last-err null]
    (if (= attempts 0)
      (throw (str "retry: exhausted after " n " attempts: " last-err))
      (let [result (try (apply f args)
                        (catch [e] e))]
        (if (null? (:message result))
          result
          (recur (dec attempts) (:message result)))))))
