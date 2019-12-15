(ns aoc.2019.day8
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]))

(def sample "123456789012")

(defn decode-image [w h data]
  (let [int-data (string/split data #"")]
    (->> (partition (* w h) int-data)
         (map #(partition w %)))))

(defn check [w h data]
  (let [image (decode-image w h data)]
    (->> image
         (map #(mapcat identity %))
         (map (juxt identity (comp count #(filter #{"0"} %))))
         (apply min-key second)
         (first)
         (vector)
         (apply (juxt #(filter #{"1"} %) #(filter #{"2"} %)))
         (map count)
         (apply *))))

(def input (-> (read-input 2019 8) first))

(defn part-1 []
  (check 25 6 (-> (read-input 2019 8) first)))

(def sample2 "0222112222120000")

(defn combine-rows [r1 r2]
  (map #(if (= %1 "2") %2 %1) r1 r2))

(defn combine-layers [l1 l2]
  (map combine-rows l1 l2))

(defn make-image [w h data]
  (let [img (decode-image w h data)]
    (reduce combine-layers img)))

(defn number->pixel [n]
  (if (= "0" n)
    "░"
    "▓"))

(defn part-2 []
  (->> input
       (make-image 25 6)
       (map #(map number->pixel %))
       (map #(apply str %))
       (map println)))

