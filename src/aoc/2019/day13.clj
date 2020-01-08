(ns aoc.2019.day13
  (:require [aoc.core :refer [read-ic-input]]
            [aoc.2019.intcode :refer [run']]
            [clojure.core.async :refer [<! >!
                                        <!! >!!
                                        chan
                                        go-loop]]))
(def sample [104,1,
             104,2,
             104,3,
             104,6,
             104,5,
             104,4,
             104,2,
             104,3,
             104,2,
             104,1,
             104,1,
             104,2,
             99])

(def tile-ids
  {0 :empty
   1 :wall
   2 :block
   3 :paddle
   4 :ball})

(defn mapply [max-min pos coll]
  (if (seq coll)
    (->> coll
      (map pos)
      (apply max-min))
    0))

(defn move-cursor-up []
  (print "\r\033[2K\033[F\r\033[2K\033[F\r"))

(defn clear-screen []
  (repeatedly 80 move-cursor-up))

(defn game [in]
  (go-loop [state {}]
    (if-let [x (<! in)]
      (let [y (<! in)
            tile (<! in)]
        (recur (assoc state [x y] (get tile-ids tile))))
     state)))

(defn render-screen [m]
  (clear-screen)
  (let [c (map (fn [[k _]] k) m)
        miny (mapply min second c)
        maxy (mapply max second c)
        minx (mapply min first c)
        maxx (mapply max first c)]
    (doseq [y (range miny (inc maxy))
            x (range minx (inc maxx))]
      (let [display (if (= maxx x) println print)]
        (display
          (case (get m [x y] :empty)
            :wall "|"
            :block "x"
            :paddle "_"
            :ball "o"
            " "))))))

(defn start-game [xs]
  (let [ochan (chan 2)
        ichan (chan)]
    (run' xs ichan ochan)
    (let [state (<!! (game ochan))]
      (render-screen state)
      state)))

(defn part-1 []
  (->> (read-ic-input 2019 13)
       (start-game)
       (filter (fn [[k v]] (= :block v)))
       (count)))
