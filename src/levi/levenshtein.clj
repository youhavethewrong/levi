(ns levi.levenshtein
  (:require [clojure.core.matrix :as matrix]
            [clatrix.core :as cl]))


;; TODO, make loop/recur so we can memoize maybe
(defn slow-distance
  "Calculates the Levenshtein Distance between two strings using a naive
  recursive implementation.
  [https://en.wikipedia.org/wiki/Levenshtein_slow-distance]"
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

(defn matrix-distance
  [s t]
  (let [d (matrix/new-matrix :clatrix (inc (count s))  (inc (count t)))
        ;; first => m/i
        d (matrix/emap-indexed #(if (= 0 (second %1)) (first %1) %2) d)
        ;; second => n/j
        d (matrix/emap-indexed #(if (= 0 (first %1)) (second %1) %2) d)]
    (matrix/emap-indexed
     (fn [k v]
       (let [i (first k)
             j (second k)]
         (if (and (> i 0)
                  (> j 0))
           (if (= (.charAt s (dec i))
                  (.charAt t (dec j)))
             (nth (nth d (dec i)) (dec j))
             (let [di (dec i)
                   dj j
                   ii i
                   ij (dec j)
                   si (dec i)
                   sj (dec j)
                   deletion (inc (nth (nth d di) dj))
                   insertion (inc (nth (nth d ii) ij))
                   substitution (inc (nth (nth d si) sj))]
               (min deletion insertion substitution)))
         v)))
     d)))
