(ns aoc.2019.day7
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]
            [aoc.2019.intcode :as ic]))

(def sample [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn run-with [inputs]
  (binding [*in* (apply str (interleave inputs (repeat "\n")))]
    (ic/run sample)))
