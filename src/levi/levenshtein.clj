(ns levi.levenshtein
  (:require [clojure.pprint :as pp]
            [clojure.core.matrix :as m]
            [clatrix.core :as cl]))

(defn slow-distance
  "Calculates the Levenshtein Distance between two strings using a naive
  recursive implementation.
  [https://en.wikipedia.org/wiki/Levenshtein_distance]"
  ([s t]
   (slow-distance s (count s) t (count t)))

  ([s len_s t len_t]
  (cond
    ;; base cases
    (= 0 len_s) len_t
    (= 0 len_t) len_s

    ;; recursive
    :else
    (let [schar (.charAt s (dec len_s))
          tchar (.charAt t (dec len_t))
          cost (if (= schar tchar) 0 1)]
      (min (inc (slow-distance s (dec len_s) t len_t))
           (inc (slow-distance s len_s t (dec len_t)))
           (+ cost (slow-distance s (dec len_s) t (dec len_t))))))))

(defn source-prefixes
  [coords v]
  (if (and (> (first coords) 0)
           (= 0 (second coords)))
    (first coords)
    v))

(defn target-prefixes
  [coords v]
  (if (and (> (second coords) 0)
           (= 0 (first coords)))
    (second coords)
    v))

(defn find-cheapest-op
  [s t d i j v]
  ;; ignore 0,0
  (let [cost (if (= (get s (dec i)) (get t (dec j))) 0 1)
        deletion (inc (m/mget d (dec i) j))
        insertion (inc (m/mget d i (dec j)))
        substitution (+ cost (m/mget d (dec i) (dec j)))
        least (min deletion insertion substitution)
        _ (m/mset! d i j least)]
    (m/mget d i j)))

(defn matrix-distance
  "Wagner-Fischer algorithm implementation."
  [s t]
  (cond
    ;; degenerate cases
    (= s t) 0
    (= 0 (count s)) (count t)
    (= 0 (count t)) (count s)
    :else
    ;; TODO iterate only over first row and first column for prefixes
    (let [d (m/new-matrix :double-array (inc (count s)) (inc (count t)))
          d (m/emap-indexed source-prefixes d)
          d (m/emap-indexed target-prefixes d)
          _ (for [i (range 1 (inc (count s)))]
              (for [j (range 1 (inc (count t)))]
                (find-cheapest-op s t d i j (m/mget d i j))))]
      (pp/pprint _)
      (int (m/mget d (count s) (count t))))))













