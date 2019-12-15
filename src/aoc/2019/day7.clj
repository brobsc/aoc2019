(ns aoc.2019.day7
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]
            [aoc.2019.intcode :as ic]
            [clojure.core.async :refer [go <! >! chan
                                        <!! >!!]]))

(def sample [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn run-with [inputs program]
  (let [inp (apply str (interleave inputs (repeat "\n")))]
    (Integer/parseInt
      (first (string/split-lines (with-out-str
                                  (with-in-str inp (ic/run program))))))))

(defn run-sequence [inputs program]
  (loop [i inputs
         r 0]
    (if (seq i)
      (recur (next i)
             (run-with [(first i) r] program))
      r)))

(defn find-optimal [program]
  (->> (range 5)
       (permutations)
       (map (juxt identity #(run-sequence % program)))
       (apply max-key second)))

(defn part-1 []
  (let [program (mapv #(Integer/parseInt %)
                  (-> (read-input 2019 7)
                      (first)
                      (string/split #",")))]
    (find-optimal program)))


(def sample2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
              27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

(defn run-sequence-loop [[p1 p2 p3 p4 p5] program]
  (let [a-in (chan 2)
        b-in (chan 2)
        c-in (chan 2)
        d-in (chan 2)
        e-in (chan 2)
        _ (do
            (>!! a-in p1)
            (>!! a-in 0)
            (>!! b-in p2)
            (>!! c-in p3)
            (>!! d-in p4)
            (>!! e-in p5))
        a (ic/run' a-in b-in program)
        b (ic/run' b-in c-in program)
        c (ic/run' c-in d-in program)
        d (ic/run' d-in e-in program)
        e (ic/run' e-in a-in program)]
    a-in))

(defn find-optimal' [program]
  (->> (range 5 10)
       (permutations)
       (mapv (juxt identity #(run-sequence-loop % program)))
       (map (juxt first (comp <!! second)))
       (apply max-key second)))

(defn part-2 []
  (let [program (mapv #(Integer/parseInt %)
                  (-> (read-input 2019 7)
                      (first)
                      (string/split #",")))]
    (find-optimal' program)))
