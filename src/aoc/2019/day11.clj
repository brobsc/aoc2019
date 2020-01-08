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

(defn paint-bot [in out a]
  (go-loop [grid {}
            pos [0 0]
            facing :up]
    (let [now (get grid pos :black)
          w (>! out (if (= :black now) 0 1))
          paint (<! in)
          hand (<! in)]
      (if (and w paint)
        (let [[x y f] (turn pos facing (if (zero? hand) :left :right))
              pos' [x y]]
          (println "[commands] out:" (if (= :black now) 0 1) "in:" paint hand)
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

(defn run-paint-bot [xs]
  (let [ic-chan (chan 2)
        pb-chan (chan 2)
        a-chan (chan)]
    (paint-bot ic-chan pb-chan a-chan)
    (ic/run' xs pb-chan ic-chan)
    (read)
    (close! ic-chan)
    (close! pb-chan)
    (<!! a-chan)))

(defn render-hull [m]
  (doseq [y (range 2 -3 -1)
          x (range -2 3)]
    (let [display (if (= 2 x) println print)]
      (if (= :black (get m [x y] :black))
        (display ". ")
        (display "# ")))))


(defn part-1 []
  (run-paint-bot (mapv #(BigInteger. %) (-> (read-input 2019 11)
                                          (first)
                                          (string/split #",")))))
