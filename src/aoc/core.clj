(ns aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-input [y s]
  (-> (str y "/" "day" s ".txt")
    (io/resource)
    (slurp)
    (string/split #"\n")))

(defn read-ic-input [y s]
  (mapv #(BigInteger. %) (-> (read-input y s)
                             (first)
                             (string/split #","))))
