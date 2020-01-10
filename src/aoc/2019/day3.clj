(ns aoc.2019.day3
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]
            [clojure.set :as c-set]))

;; move wire
;; given an initial position and a move, move wire returning a vector
;; [final_pos [all_points]]
;; calc all known coordinates

(defn move [[x y] mv]
  (let [dir (first mv)
        qt (->> mv next (apply str) (#(Integer/parseInt %)))]
    (case dir
      \R (for [x' (range (inc x) (+ x (inc qt)))] [x' y])
      \L (for [x' (range (dec x) (- x (inc qt)) -1)] [x' y])
      \U (for [y' (range (inc y) (+ y (inc qt)))] [x y'])
      \D (for [y' (range (dec y) (- y (inc qt)) -1)] [x y']))))

(defn move-and-join [xs mv]
  (->> mv
      (move (last xs))
      (concat xs)))

(defn move-all [mvs]
  (reduce move-and-join
          [[0 0]] mvs))

(defn m-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

;; very slow solution
(defn solve-1 [s]
  (->> s
      (map #(string/split % #","))
      (map (comp set move-all))
      (apply c-set/intersection)
      (filter #(not (= [0 0] %)))
      (map m-distance)
      (apply min)))

(defn part-1 []
  (->> (read-input 2019 3)
       (solve-1)))

(defn steps [wire p]
  (.indexOf wire p))

(defn solve-2 [s]
  (let [[w1 w2] (->> s
                    (map #(string/split % #","))
                    (map move-all))
        inters (->> (c-set/intersection (set w1) (set w2))
                    (filter #(not (= [0 0] %))))]
    (apply min (map + (map #(steps w1 %) inters)
                     (map #(steps w2 %) inters)))))

(defn part-2 []
  (->> (read-input 2019 3)
       (solve-2)))
