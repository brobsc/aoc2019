(ns aoc.2019.day11
  (:require [aoc.core :refer [read-input]]
            [aoc.2019.intcode :as ic]
            [clojure.string :as string]
            [clojure.core.async :refer [close! go go-loop <! >! chan
                                        >!!  <!!]]))

(defn turn [[x y] current hand]
  (case [current hand]
    ([:down :right] [:up :left]) [(dec x) y :left]
    ([:up :right] [:down :left]) [(inc x) y :right]
    ([:left :right] [:right :left]) [x (inc y) :up]
    ([:right :right] [:left :left]) [x (dec y) :down]))

(defn paint-bot [in out a initial-command]
  (go-loop [grid {}
            pos [0 0]
            facing :up]
    (let [now (get grid pos :black)
          w (>! out (if (= grid {})
                      initial-command
                      (if (= :black now) 0 1)))
          paint (<! in)
          hand (<! in)]
      (if (and w paint)
        (let [[x y f] (turn pos facing (if (zero? hand) :left :right))
              pos' [x y]]
          (recur (assoc grid pos (if (zero? paint) :black :white))
                 pos' f))
        (>! a grid)))))

(def sample-xs [3 0
                104 1 104 0 3 0
                104 0 104 0 3 0
                104 1 104 0 3 0
                104 1 104 0 3 0
                104 0 104 1 3 0
                104 1 104 0 3 0
                104 1 104 0 3 0
                99])

(defn run-paint-bot [xs initial]
  (let [ic-chan (chan 2)
        pb-chan (chan 2)
        a-chan (chan)]
    (paint-bot ic-chan pb-chan a-chan initial)
    (ic/run' xs pb-chan ic-chan)
    (<!! a-chan)))

(defn render-hull [m]
  (let [c (map (fn [[k _]] k) m)
        miny (->> c (map second) (apply min))
        maxy (->> c (map second) (apply max))
        minx (->> c (map first) (apply min))
        maxx (->> c (map first) (apply max))]
    (doseq [y (range maxy (dec miny) -1)
            x (range minx (inc maxx))]
      (let [display (if (= maxx x) println print)]
        (if (= :black (get m [x y] :black))
          (display " ")
          (display "█"))))))

(defn part-1 []
  (count (run-paint-bot (mapv #(BigInteger. %) (-> (read-input 2019 11)
                                                 (first)
                                                 (string/split #",")))
          0)))

(defn part-2 []
  (-> (mapv #(BigInteger. %) (-> (read-input 2019 11)
                                 (first)
                                 (string/split #",")))
      (run-paint-bot 1)
      (render-hull)))
