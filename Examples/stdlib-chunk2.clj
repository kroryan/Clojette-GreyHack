;; Math utils
(defn even? [n] (= (% n 2) 0))
(defn odd? [n] (not (even? n)))
(defn zero? [n] (= n 0))
(defn pos? [n] (> n 0))
(defn neg? [n] (< n 0))
(defn inc [n] (+ n 1))
(defn dec [n] (- n 1))

(defn max [x & xs]
  (reduce
    (fn [a b]
      (if (> a b) a b))
    x
    xs))

;; Function utils
(defn identity [x] x)
(defn constantly [x] (fn [& _] x))
(defn complement [f] (fn [& args] (not (apply f args))))
(defn comp [f g] (fn [x] (f (g x))))

;; I/O
(defn slurp [path]
  (let [computer (.host_computer (get_shell))
        file (.File computer path)]
    (if (null? file)
      (throw (str "slurp: no such file: " path))
      (let [content (.get_content file)]
        (if (null? content)
          (throw (str "slurp: cannot read file (binary or no permission): " path))
          content)))))

(defn spit [path content]
  (let [computer (.host_computer (get_shell))
        existing (.File computer path)]
    (if (null? existing)
      ;; File doesn't exist, so we create it first
      (let [parent (parent_path path)
            fname  (last (split path "/"))
            result (.touch computer parent fname)]
        (if (string? result)
          (throw (str "spit: could not create file: " result))
          (.set_content (.File computer path) content)))
      (.set_content existing content))))

(defn spit-append [path content]
  (let [existing (try (slurp path) (catch [_] ""))]
    (spit path (str existing content))))

(defn file-seq [path]
  (let [computer (.host_computer (get_shell))
        entry    (.File computer path)]
    (if (null? entry)
      (throw (str "file-seq: path not found: " path))
      (if (.is_folder entry)
        (let [files   (.get_files   entry)
              folders (.get_folders entry)]
          (concat
            [entry]
            (reduce concat []
              (map (fn [f] (file-seq (.path f))) folders))
            (if (null? files) [] files)))
        [entry]))))

(defn file-exists? [path]
  (let [computer (.host_computer (get_shell))]
    (not (null? (.File computer path)))))

(defn make-parents [path]
  (let [computer (.host_computer (get_shell))
        segments (filter (fn [s] (not (= s ""))) (split path "/"))]
    (loop [parts segments current ""]
      (when (not (empty? parts))
        (let [next-path (str current "/" (first parts))]
          (when (null? (.File computer next-path))
            (.create_folder computer current (first parts)))
          (recur (rest parts) next-path))))))

(defn path-join [& parts]
  (let [joined (join "/" parts)]
    (replace joined "//" "/")))

(defn file-name [path]
  (last (split path "/")))

(defn file-ext [path]
  (let [name (file-name path)
        dot  (last-index-of name ".")]
    (if (< dot 0) "" (subs name (inc dot)))))

(defn strip-ext [path]
  (let [dot (last-index-of path ".")]
    (if (< dot 0) path (subs path 0 dot))))
