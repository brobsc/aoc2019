(ns aoc.2019.day1
  (:require [aoc.core :refer [read-input]]))

(defn calc-fuel [i]
  (let [sum (-> i
                (/ 3.0)
                (Math/floor)
                (- 2)
                (biginteger))]
    (if (< sum 0) 0 sum)))

(defn part-1 []
  (->> (read-input 2019 1)
       (map #(Integer/parseInt %))
       (map calc-fuel)
       (apply +)))

(defn calc-fuel' [i]
  (loop [fuel i
         t 0]
    (let [extra (calc-fuel fuel)
          total (+ t extra)]
      (if (< 0 extra)
        (recur extra total)
        total))))

(defn part-2 []
  (->> (read-input 2019 1)
       (map #(Integer/parseInt %))
       (map calc-fuel')
       (apply +)))

