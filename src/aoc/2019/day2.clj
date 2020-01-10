(ns aoc.2019.day2
  (:require [aoc.core :refer [read-ic-input]]
            [aoc.2019.intcode :refer [run]]
            [clojure.test :refer [testing is]]))

(def input
  (read-ic-input 2019 2))

(defn change-and-run [mem x y]
  (-> mem
      (assoc 1 x)
      (assoc 2 y)
      (run)
      (first)))

(defn part-1 []
  (change-and-run input 12 2))

(defn part-2 []
  (first
    (for [x (range 100)
          y (range 100)
          :when (= 19690720 (change-and-run input x y))]
      (+ y (* 100 x)))))

(testing "Part-1"
  (is (= (run [1,0,0,0,99]) [2,0,0,0,99]))
  (is (= (run [2,3,0,3,99]) [2,3,0,6,99]))
  (is (= (run [2,4,4,5,99,0]) [2,4,4,5,99,9801]))
  (is (= (run [1,1,1,4,99,5,6,0,99]) [30,1,1,4,2,5,6,0,99]))
  (is (= (part-1) 11590668)))

(testing "Part-2"
  (is (= (part-2) 2254)))
