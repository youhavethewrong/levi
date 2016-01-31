(ns levi.levenshtein-test
  (:require [levi.levenshtein :as sut]
            [clojure.test :as t]))

(t/deftest slow-test
  (t/testing "Test the slow implementation of this algorithm"
    (t/is (= 3 (sut/slow-distance "kitten" "sitting")))
    (t/is (= 4 (sut/slow-distance "taco" "")))
    (t/is (= 5 (sut/slow-distance "" "snail")))
    (t/is (= 0 (sut/slow-distance "sunday" "sunday")))))


(t/deftest matrix-test
  (t/testing "Test the matrix implementation of this algorithm"
    (t/is (= 3 (sut/matrix-distance "kitten" "sitting")))
    (t/is (= 4 (sut/matrix-distance "taco" "")))
    (t/is (= 5 (sut/matrix-distance "" "snail")))
    (t/is (= 0 (sut/matrix-distance "sunday" "sunday")))))
