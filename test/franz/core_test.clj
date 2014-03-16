(ns franz.core-test
  (:require [clojure.test :refer :all]
            [franz.core :refer :all]))

(deftest pairs-ordered
  (testing "Edge cases"
    (is (= [[4 4]] (ordered-pairs 4 4 0)))
    (is (= []      (ordered-pairs 1 3 3)))
    (is (= []      (ordered-pairs 4 4))))
  (testing "Usual behaviour"
    (is (= #{[3 3] [3 4] [3 5] [4 4] [4 5] [5 5]} (set (ordered-pairs 3 5 0))))
    (is (= #{            [3 5]                  } (set (ordered-pairs 3 5 2))))
    (is (= #{      [3 4] [3 5]       [4 5]      } (set (ordered-pairs 3 5))))))

(deftest pairs-overlapping
  (is (= #{[[3 4] [4 5]]
           [[3 4] [4 6]]
           [[3 5] [4 6]]
           [[3 5] [5 6]]
           [[4 5] [5 6]]}
         (set (overlapping-pairs 3 6))))
  (is (= #{[[3 4] [4 5]]
           [[3 4] [4 6]]
           [[3 5] [4 5]]
           [[3 5] [4 6]]
           [[3 5] [5 6]]
           [[3 6] [4 6]]
           [[3 6] [5 6]]
           [[4 5] [5 6]]
           [[4 6] [5 6]]}
         (set (overlapping-pairs 3 6 :min-udiff 0)))))
