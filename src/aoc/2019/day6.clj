(ns aoc.2019.day6
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]))

(def sample (string/split-lines "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"))

(def orbits
  (memoize
    (fn [c p os]
      (if (= p c)
        0
        (let [other (->> os
                         (some #(when (string/includes? % (str ")" p)) %))
                         (#(string/split % #"\)"))
                         (some #(when ((complement #{p}) %) %)))]
          (inc (orbits c other os)))))))

(def orbits' (partial orbits "COM"))

(defn total-orbits [os]
  (let [ps (->> os
               (map #(string/split % #"\)"))
               (mapcat identity)
               (set))]
    (->> ps
         (map #(orbits' % os))
         (apply +))))

(defn part-1 []
  (->> (read-input 2019 6)
       (total-orbits)))

(defn debug [s]
  (println s)
  s)

(defn keys-in
  "Returns a sequence of all key paths in a given map using DFS walk."
  [m]
  (letfn [(children [node]
            (let [v (get-in m node)]
              (if (map? v)
                (map (fn [x] (conj node x)) (keys v))
                [])))
          (branch? [node] (-> (children node) seq boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch? children %)))))

(defn path [p o]
  (when (seq o)
    (some->> o
       (keys-in)
       (filter #(some #{p} %))
       (filter #(= p (last %)))
       (seq)
       (reduce #(if (< (count %2) (count %1)) %1 %2))
       (butlast))))

(defn nest [t root]
  (loop [cur (seq t)
         res {}]
    (if (seq cur)
      (let [[parent children] (-> cur seq first)
            p (path parent res)
            recurring (if p
                        (-> res (dissoc parent) (update-in p assoc children))
                        (assoc res parent children))
            next-n (if p
                     (next cur)
                     (seq (conj (vec (next cur)) (first cur))))]
        (debug res)
        (if (= parent root)
          (recur (next cur)
                 {parent children})
          (recur
            (if (read) next-n [])
            recurring)))
      res)))

(defn tree [os]
  (loop [cur os
         res {}]
    (if (seq cur)
      (let [[center orbitting] (-> (first cur)
                                   (string/split #"\)"))]

        (recur (next cur)
               (update-in res [center] assoc orbitting {})))
      (nest res "COM"))))



(def path-to-root
  (memoize
    (fn [root node os]
      (if (= node root)
        [root]
        (let [other (->> os
                         (some #(when (string/includes? % (str ")" node)) %))
                         (#(string/split % #"\)"))
                         (some #(when ((complement #{node}) %) %)))]
          (concat (path-to-root root other os) [node]))))))

(defn common-p [p1 p2]
  (last (take-while (set p1) p2)))


(defn path [node1 node2 root os]
  (let [p1 (path-to-root root node1 os)
        p2 (path-to-root root node2 os)
        p (common-p p1 p2)
        r1 (path-to-root p node1 os)
        r2 (path-to-root p node2 os)]
    (debug r1)
    (debug r2)
    (- (+ (count r1) (count r2)) 4)))


(defn part-2 []
  (->> (read-input 2019 6)
       (path "YOU" "SAN" "COM")))

