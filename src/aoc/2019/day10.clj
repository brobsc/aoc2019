(ns aoc.2019.day10
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]))

(def sample ".#..#
.....
#####
....#
...##")

;; .#..#
;; .....
;; #####
;; ....#
;; ...##

(defn treat-input [s]
  (apply mapv vector (->> s
                          (mapv #(string/split % #"")))))

(defn factors [x]
  (conj (vec (for [y (range 2 (inc (/ x 2)))
                   :when (zero? (mod x y))]
               y))
    x))

(defn mdc [a b]
  (let [mdc' (->> b
                factors
                (take-while (set (factors a)))
                (last))]
    (if (or (nil? mdc') (zero? mdc'))
      1
      mdc')))

(defn debug [x]
  (println x)
  x)

(defn can-see? [[x1 y1] [x2 y2] asteroids]
  (if (or (= "." (get-in asteroids [x2 y2] "."))
          (= "." (get-in asteroids [x1 y1] "."))
          (= [x1 y1] [x2 y2]))
    false
    (let [[rel-x rel-y] [(Math/abs (- x2 x1)) (Math/abs (- y2 y1))]
          d (mdc rel-x rel-y)
          [a-x a-y] (map #(/ % d) [rel-x rel-y])
          blockers (->> (map vector (range (min (inc x1) (inc x2)) (max x1 x2) a-x)
                            (range (min (inc y1) (inc y2)) (max y1 y2) a-y))
                       (map #(= "#" (get-in asteroids % ".")))
                       (some identity))]
        (not blockers))))

(defn all-coords [asteroids]
  (let [max-x (count asteroids)
        max-y (count (first asteroids))]
    (for [x (range max-x)
          y (range max-y)]
      [y x])))

(defn check-all-others [coords asteroids]
  (->> (filter #(can-see? coords % asteroids) (all-coords asteroids))
       count))

(defn test1 []
  (let [asteroids (treat-input (string/split-lines sample))
        coords (all-coords asteroids)]
    (->> coords
         (map (juxt identity #(check-all-others % asteroids)))
         (apply max-key second))))

(defn safe-div [a b]
  (if (zero? b)
    0
    (if (zero? a)
      (if (neg? b) Double/MIN_VALUE Double/MAX_VALUE)
      (/ a b))))

(defn tan [coords1 coords2]
  (->> coords2
      (map - coords1)
      (apply safe-div)
      (Math/tan)))

(def in-los
  (memoize
    (fn [coords1 coords2 asteroids]
      (if (or (= coords1 coords2)
              (= "." (get-in asteroids coords1))
              (= "." (get-in asteroids coords2)))
        false
        (if (pos? (compare coords1 coords2))
          (in-los coords2 coords1 asteroids)
          (let [between (->> asteroids
                             all-coords
                             (drop-while (complement #{coords1}))
                             next
                             (take-while (complement #{coords2})))]
            (->> between
                 (map (juxt identity #(tan coords1 %)))
                 (filter #(#{(tan coords1 coords2)} (second %)))
                 (map first)
                 (map #(get-in asteroids %))
                 (some #{"#"})
                 (not))))))))

(defn check-all [coords asteroids]
  (count (filter #(in-los coords % asteroids) (all-coords asteroids))))

(defn part-1 []
  (let [asteroids (treat-input (read-input 2019 10))
        coords (all-coords asteroids)]
    (->> coords
         (map (juxt identity #(check-all % asteroids)))
         (map second)
         #_(apply max-key second))))


