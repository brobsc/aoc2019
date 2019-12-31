(ns aoc.2019.intcode
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]
            [clojure.core.async :refer [go go-loop <! >! chan
                                        <!! >!!]]))

(defn extract-op [i]
  [(mod i 100)
   (mod (int (/ i 100)) 10)
   (mod (int (/ i 1000)) 10)
   (mod (int (/ i 10000)) 10)])

(defn get-value [i mode rbase xs]
  (case mode
    0 (get xs i 0)
    1 i
    2 (get xs (+ rbase i) 0)))

(defn op4 [xs s]
  (println s)
  xs)

(defn safe-assoc [xs i value]
  (if (neg? i)
    (assoc xs 0 value)
    (if (< (count xs) i)
      (safe-assoc (assoc xs (count xs) 0) i value)
      (assoc xs i value))))

(defn inc-rbase [rbase value]
  (let [sum (+ rbase value)]
    (if (neg? sum)
      0
      sum)))

(defn get-literal-value [s mode rbase]
  (case mode
    2 (+ s rbase)
    s))

(defn run [xs]
  (loop [xs xs
         start 0
         rbase 0]
    (let [full-op (nth xs start)
          [op s1-mode s2-mode d-mode] (extract-op full-op)]
      (if (= 99 op)
        xs
        (let [s1 (get xs (inc start))
              s2 (get xs (+ start 2))
              s3 (get xs (+ start 3))
              v1 (get-value s1 s1-mode rbase xs)
              v2 (get-value s2 s2-mode rbase xs)
              v1' (get-literal-value s1 s1-mode rbase)
              d (get-literal-value s3 d-mode rbase)
              size (case op
                     (1 2 7 8) 4
                     (5 6) 3
                     (3 4 9) 2
                     0)
              pc-plus (+ size start)]
          (recur
            (case op
              1 (safe-assoc xs d (+ v1 v2))
              2 (safe-assoc xs d (* v1 v2))
              3 (safe-assoc xs v1' (read))
              4 (op4 xs v1)
              (5 6) xs
              7 (safe-assoc xs d (if (< v1 v2) 1 0))
              8 (safe-assoc xs d (if (= v1 v2) 1 0))
              9 xs
              (throw (Exception. (str op "/" s1 "/" s2 "/" xs))))
            (case op
              5 (if (not= 0 v1) v2 pc-plus)
              6 (if (zero? v1) v2 pc-plus)
              pc-plus)
            (case op
              9 (inc-rbase rbase v1)
              rbase)))))))





;;;

(defn op4' [xs s out]
  (go (>! out s))
  xs)


(defn run' [xs in out]
  (go-loop [xs xs
            start 0
            rbase 0]
    (let [full-op (nth xs start)
          [op s1-mode s2-mode d-mode] (extract-op full-op)]
      (if (= 99 op)
        xs
        (let [s1 (get xs (inc start))
              s2 (get xs (+ start 2))
              s3 (get xs (+ start 3))
              v1 (get-value s1 s1-mode rbase xs)
              v2 (get-value s2 s2-mode rbase xs)
              v1' (get-literal-value s1 s1-mode rbase)
              d (get-literal-value s3 d-mode rbase)
              size (case op
                     (1 2 7 8) 4
                     (5 6) 3
                     (3 4 9) 2
                     0)
              pc-plus (+ size start)]
          (recur
            (case op
              1 (safe-assoc xs d (+ v1 v2))
              2 (safe-assoc xs d (* v1 v2))
              3 (safe-assoc xs v1' (<! in))
              4 (op4' xs v1 out)
              (5 6) xs
              7 (safe-assoc xs d (if (< v1 v2) 1 0))
              8 (safe-assoc xs d (if (= v1 v2) 1 0))
              9 xs
              (throw (Exception. (str op "/" s1 "/" s2 "/" xs))))
            (case op
              5 (if (not= 0 v1) v2 pc-plus)
              6 (if (zero? v1) v2 pc-plus)
              pc-plus)
            (case op
              9 (inc-rbase rbase v1)
              rbase)))))))
