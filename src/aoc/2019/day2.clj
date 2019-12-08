(ns aoc.2019.day2
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]))

(defn input []
  (vec (map #(Integer/parseInt %)
        (-> (read-input 2019 2)
            (first)
            (string/split #",")))))

(defn run [xs]
  (loop [xs xs
         start 0]
    (let [op (nth xs start)]
      (if (= 99 op)
        xs
        (let [s1 (nth xs (inc start))
              s2 (nth xs (+ start 2))
              d (nth xs (+ start 3))
              v1 (nth xs s1)
              v2 (nth xs s2)]
          (recur
            (case op
              1 (assoc xs d (+ v1 v2))
              2 (assoc xs d (* v1 v2)))
            (+ 4 start)))))))

(defn part-1' [x y]
  (-> (input)
      (assoc 1 x)
      (assoc 2 y)
      (run)
      (first)))

(defn part-1 []
  (part-1' 12 2))

(defn combinations []
  (for [x (range 100)
        y (range 100)]
    [x y]))

(defn answer [x y]
  (+ y (* 100 x)))

(defn part-2 []
  (->> (combinations)
       (map (juxt #(apply part-1' %) identity))
       (filter (fn [[r _]] (= 19690720 r)))
       (first)
       (#(nth % 1))
       (apply answer)))

