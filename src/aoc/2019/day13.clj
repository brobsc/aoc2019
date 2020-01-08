(ns aoc.2019.day13
  (:require [aoc.core :refer [read-ic-input]]
            [aoc.2019.intcode :refer [run']]
            [clojure.core.async :refer [<! >!
                                        offer!
                                        <!! >!!
                                        chan sliding-buffer
                                        timeout
                                        go
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

(defn clear-screen []
  #_(print (str (char 27) "[2J")) ; clear screen
  (print (str (char 27) "[;H"))) ; move cursor to the top left corner of the screen

(defn obj-pos [obj m]
  (or (some (fn [[k v]]
              (when (= v obj)
                k))
       m)
    [0 0]))

(defn score [m]
  (or (some (fn [[k v]]
             (when (= k [-1 0])
                v))
       m)
    0))

(defn render-screen [m]
  (clear-screen)
  (let [c (map (fn [[k _]] k) m)
        miny (mapply min second c)
        maxy (mapply max second c)
        minx (mapply min first c)
        maxx (mapply max first c)]
    (println)
    (println "score:" (score m) "->" (obj-pos :ball m)
             [maxx maxy])
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

(defn direction [n]
  (case n
    -1 "<"
     1 ">"
     0 "."))

(defn game [in out]
  (go-loop [state {}]
    (if-let [x (<! in)]
      (let [y (<! in)
            tile (<! in)
            [ballx bally] (obj-pos :ball state)
            [padx _] (obj-pos :paddle state)
            dir-p (compare ballx padx)]
        (do (render-screen state)
            (>! out dir-p))
        (recur (-> state
                  (assoc [x y] (if (= [x y]
                                      [-1 0])
                                 tile
                                 (get tile-ids tile))))))
     (do (println)
         state))))

(defn start-game [xs]
  (let [ochan (chan 2)
        ichan (chan (sliding-buffer 2))]
    (run' xs ichan ochan)
    (let [state (<!! (game ochan ichan))]
      state)))

(defn part-1 []
  (->> (read-ic-input 2019 13)
       (start-game)
       (filter (fn [[k v]] (= :block v)))
       (count)))

(defn part-2 []
  (->> (read-ic-input 2019 13)
       (#(assoc % 0 2))
       (start-game)
       (score)))

(defn -main []
  (clear-screen)
  (println (part-2)))
