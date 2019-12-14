(ns aoc.2019.day5
  (:require [aoc.core :refer [read-input]]
          [clojure.string :as string]
          [aoc.2019.intcode :as ic]))

(defn part-1 []
  (ic/run (mapv #(Integer/parseInt %)
              (-> (read-input 2019 5)
                (first)
                (string/split #",")))))

