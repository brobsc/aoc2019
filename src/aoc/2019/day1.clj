(ns aoc.2019.day1
  (:require [aoc.core :refer [read-input]]
            [clojure.test :refer [testing is]]))
(def input
  (->> (read-input 2019 1)
       (map #(Integer/parseInt %))))

(defn fuel [i]
  (let [result (-> i (/ 3.0) (- 2) (biginteger))]
    (if (pos? result) result 0)))

(defn calc-fuel [i]
  (->> i
       (iterate fuel)
       (next)
       (take-while pos?)
       (apply +)))

(defn part-1 []
  (->> input
       (map fuel)
       (apply +)))

(defn part-2 []
  (->> input
       (map calc-fuel)
       (apply +)))

(testing "Part 1"
  (is (= (fuel 14) 2))
  (is (= (fuel 1969) 654))
  (is (= (fuel 100756) 33583))
  (is (= (part-1) 3320816)))

(testing "Part 2"
  (is (= (calc-fuel 14) 2))
  (is (= (calc-fuel 1969) 966))
  (is (= (calc-fuel 100756) 50346))
  (is (= (part-2) 4978360)))
