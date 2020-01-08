(ns aoc.2019.day12
  (:require [aoc.core :refer [read-input]]
            [clojure.math.numeric-tower :refer [lcm]]))

(def sample-1
  [{:name :i
    :pos [-1 0 2]
    :vel [0 0 0]}
   {:name :e
    :pos [2 -10 -7]
    :vel [0 0 0]}
   {:name :g
    :pos [4 -8 8]
    :vel [0 0 0]}
   {:name :c
    :pos [3 5 -1]
    :vel [0 0 0]}])

(def sample-2
  [{:name :i
    :pos [-8 -10 0]
    :vel [0 0 0]}
   {:name :e
    :pos [5 5 10]
    :vel [0 0 0]}
   {:name :g
    :pos [2 -7 3]
    :vel [0 0 0]}
   {:name :c
    :pos [9 -8 -3]
    :vel [0 0 0]}])

(def input
  [{:name :i
    :pos [6 -2 -7]
    :vel [0 0 0]}
   {:name :e
    :pos [-6 -7 -4]
    :vel [0 0 0]}
   {:name :g
    :pos [-9 11 0]
    :vel [0 0 0]}
   {:name :c
    :pos [-3 -4 6]
    :vel [0 0 0]}])

(defn all-others [x coll]
  (when (seq coll)
    (if (not= x (first coll))
      (lazy-seq (cons (first coll) (all-others x (next coll))))
      (all-others x (next coll)))))

(defn simulate-step [moons]
  (for [m moons]
    (let [v (->> (all-others m moons)
                 (map :pos)
                 (map #(map compare %2 %1) (repeat (:pos m)))
                 (apply map +)
                 (map + (:vel m)))]
      (-> m
           (assoc :pos (map + v (:pos m)))
           (assoc :vel v)))))

(defn calc-energy [m k]
  (->> m
       (k)
       (map #(Math/abs %))
       (apply +)))

(defn total-energy [m]
  (* (calc-energy m :vel) (calc-energy m :pos)))

(defn total-system-energy [steps moons]
  (->> (iterate simulate-step moons)
       (take (inc steps))
       (last)
       (map total-energy)
       (apply +)))

(defn dimension-repetition [dimension moons]
  (reduce (fn [steps new-step]
            (let [dimensions (map (fn [m]
                                    [((comp dimension :pos) m)
                                     ((comp dimension :vel) m)])
                              new-step)]
              (if (contains? steps dimensions)
                (reduced (count steps))
                (conj steps dimensions))))
    #{}
    (iterate simulate-step moons)))

(defn find-repetition [moons]
  (->> moons
    (repeat 3)
    (map dimension-repetition [first second last])
    (reduce lcm)))

(defn part-1 []
  (->> input
       (total-system-energy 1000)))

(defn part-2 []
  (->> input
       (find-repetition)))
