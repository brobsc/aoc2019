(ns aoc.2019.day14
  (:require [aoc.core :refer [read-input]]
            [clojure.string :as string]))

(def sample-1 "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(def sample-2 "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

(def sample-3 "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

(def sample-5 "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX")


(defn name-qt [s]
  (-> s
      (string/trim)
      (string/split #" ")
      ((fn [[qt name]]
          [(keyword name)
           (Integer/parseInt qt)]))))

(defn recipe [[inputs output]]
  (let [[out-name out-qt] (name-qt output)
        inputs (->>
                 (string/split inputs #", ")
                 (map name-qt))]
    {out-name
     {:result out-qt
      :inputs (into {} inputs)}}))


(defn recipes [str]
  (->> str
      (string/split-lines)
      (map #(string/split % #"=>"))
      (map recipe)
      (reduce merge)))

(defn div [a b]
  (+ (quot a b)
     (if (zero? (rem a b))
      0
      1)))

(defn produce
  ([ele qt m]
   (produce ele qt m [{} {}]))
  ([ele qt m box]
   (if (not (contains? m ele))
     (update-in box [0 ele] (fnil + 0) qt)
     (let [inputs (->> (ele m) :inputs)
           output (->> (ele m) :result)
           current (get-in box [1 ele] 0)
           needed (if (< qt current)
                   0
                   (- qt current))
           times (div needed output)
           total (* times output)
           excess (- total qt)]

          ; (println total "/" qt ele "->" current)
          ; (clojure.pprint/pprint (second box))

          (-> box
            (#(reduce (fn [b [i iqt]]
                        (produce i (* times iqt) m b))
                      %
                     inputs))
            (update-in [1 ele] (fnil + 0) excess))))))

(defn produce-fuel [qt s]
  (let [production (->> s
                    recipes
                    (partial produce :FUEL qt))]
    (production [{} {}])))

(defn iterate-full-produce [upto s]
  (let [production (->> s
                    recipes
                    (partial produce :FUEL 1))]
    (->> [{:ORE 0} {}]
        (iterate production)
        (take-while (fn [[o _]] (< (:ORE o) upto)))
        count
        dec)))

(defn ore [r]
  (->> r
       first
       :ORE))

(defn ore-need [qt-fuel s]
  (->> (produce-fuel qt-fuel s)
       (ore)))

(defn full-produce [upto s]
  (loop [lower-bound (quot upto (ore-need 1 s))
         upper-bound (* 100 lower-bound)]
    (let [ore-need-low (ore-need lower-bound s)
          ore-need-up (ore-need upper-bound s)
          middle (quot (+ lower-bound upper-bound) 2)
          ore-need-mid (ore-need middle s)]
      (cond
        (or (= middle lower-bound) (= upper-bound middle)) middle
        (< upto ore-need-mid) (recur lower-bound middle)
        (< ore-need-mid upto) (recur middle upper-bound)
        true middle))))

(defn solve [s]
  (->> s
       (produce-fuel 1)
       (first)
       (:ORE)))

(defn part-1 []
  (-> (read-input 2019 14)
      (#(string/join "\n" %))
      (solve)))

(defn part-2 []
  (->> (read-input 2019 14)
       (string/join "\n")
       (full-produce 1000000000000)))
