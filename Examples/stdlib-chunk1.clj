;;   Copyright (C) 2026 lattiahirvio
;;
;;   This file is part of Clojette.
;;   Clojette is licensed under GPLv3 with a special linking/importing exception.
;;   See LICENSE for details.
;;
;;   Clojette is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   any later version.
;;
;;   Clojette is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with Clojette. If not, see <https://www.gnu.org/licenses/>.

;; List operations - stack-safe via loop/recur
(defn map [f coll]
  (loop [remaining coll acc []]
    (if (empty? remaining)
      (reverse acc)
      (recur (cdr remaining) (cons (f (car remaining)) acc)))))

(defn filter [pred coll]
  (loop [remaining coll acc []]
    (if (empty? remaining)
      (reverse acc)
      (if (pred (car remaining))
        (recur (cdr remaining) (cons (car remaining) acc))
        (recur (cdr remaining) acc)))))

(defn reduce [f init coll]
  (loop [remaining coll acc init]
    (if (empty? remaining)
      acc
      (recur (cdr remaining) (f acc (car remaining))))))

(defn every? [pred coll]
  (loop [remaining coll]
    (if (empty? remaining)
      true
      (if (pred (car remaining))
        (recur (cdr remaining))
        false))))

(defn some? [pred coll]
  (loop [remaining coll]
    (if (empty? remaining)
      false
      (if (pred (car remaining))
        true
        (recur (cdr remaining))))))

(defn nth [coll n]
  (loop [remaining coll i n]
    (if (= i 0)
      (car remaining)
      (recur (cdr remaining) (- i 1)))))

(defn last [coll]
  (loop [remaining coll]
    (if (empty? (cdr remaining))
      (car remaining)
      (recur (cdr remaining)))))

(defn reverse [coll]
  (loop [remaining coll acc []]
    (if (empty? remaining)
      acc
      (recur (cdr remaining) (cons (car remaining) acc)))))

(defn range [start end]
  (loop [i start acc []]
    (if (>= i end)
      acc
      (recur (+ i 1) (cons i acc)))))

(defn concat [a b]
  (loop [remaining (reverse a) acc b]
    (if (empty? remaining)
      acc
      (recur (cdr remaining) (cons (car remaining) acc)))))

(defn flatten [coll]
  (loop [remaining coll acc []]
    (if (empty? remaining)
      (reverse acc)
      (if (list? (car remaining))
        (recur (concat (car remaining) (cdr remaining)) acc)
        (recur (cdr remaining) (cons (car remaining) acc))))))

(defn take [n coll]
  (loop [remaining coll i n acc []]
    (if (or (= i 0) (empty? remaining))
      (reverse acc)
      (recur (cdr remaining) (- i 1) (cons (car remaining) acc)))))

(defn drop [n coll]
  (loop [remaining coll i n]
    (if (or (= i 0) (empty? remaining))
      remaining
      (recur (cdr remaining) (- i 1)))))

(defn first [xs]
  (if (empty? xs)
    null
    (car xs)))

(defn seq [x]
  (cond
    (null? x) null

    (list? x)
    (if (empty? x)
      null
      x)

    (string? x)
    (let [len (count x)]
      (loop [i 0
             acc []]
        (if (>= i len)
          (if (empty? acc) null acc)
          (recur (+ i 1)
                 (conj acc (subs x i (+ i 1)))))))

    :else
    null))
