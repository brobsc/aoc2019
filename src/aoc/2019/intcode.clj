(ns aoc.2019.intcode
  (:require [aoc.core :refer [read-input]]
          [clojure.string :as string]))

(defn extract-op [i]
  [(mod i 100)
   (mod (int (/ i 100)) 10)
   (mod (int (/ i 1000)) 10)
   (mod (int (/ i 10000)) 10)])

(defn get-value [i mode xs]
  (if (zero? mode)
    (get xs i)
    i))

(defn op4 [xs s]
  (println s)
  xs)

(defn run [xs]
  (loop [xs xs
         start 0]
    (let [full_op (nth xs start)
          [op s1-mode s2-mode d-mode] (extract-op full_op)]
      (if (= 99 op)
        xs
        (let [s1 (get xs (inc start))
              s2 (get xs (+ start 2))
              d (get xs (+ start 3))
              v1 (get-value s1 s1-mode xs)
              v2 (get-value s2 s2-mode xs)
              size (case op
                     (1 2) 4
                     (3 4) 2
                     0)]
          (recur
            (case op
              1 (assoc xs d (+ v1 v2))
              2 (assoc xs d (* v1 v2))
              3 (assoc xs s1 (read))
              4 (op4 xs v1)
              (throw (Exception. (str op "/" s1 "/" s2 "/" xs))))
            (+ size start)))))))
