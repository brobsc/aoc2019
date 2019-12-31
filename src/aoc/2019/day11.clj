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

(defn paint-bot [in out]
  (go-loop [grid {}
            pos [0 0]
            facing :up]
    (let [now (get grid pos :black)
          paint (<! in)
          hand (<! in)]
      (if paint
        (let [[x y f] (turn pos facing (if (zero? hand) :left :right))
              pos' [x y]]
          (>! out (if (= :black now) 0 1))
          (recur #p (-> grid
                        (assoc pos (if (zero? paint) :black :white)))
                 pos' f))
        (>! out grid)))))

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
        pb-chan (chan 2)]
    (>!! pb-chan 0)
    (ic/run' xs pb-chan ic-chan)
    (paint-bot ic-chan pb-chan)
    (Thread/sleep 5000)
    (close! ic-chan)
    (count (<!! pb-chan))))
