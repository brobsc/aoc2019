(ns aoc.2019.intcode
  (:require [clojure.core.async :refer [go go-loop <! >!
                                        close!]]))

(defn extract-op [i]
  (lazy-seq
    (cons (rem i 100)
      (->> (quot i 100)
         ((fn get-digits [i]
            (lazy-seq (cons (rem i 10) (get-digits (quot i 10))))))
         (take 3)))))

(defn get-value [i mode dest? rbase xs]
  (if dest?
    (case mode
      2 (+ i rbase)
      i)
    (case mode
      0 (get xs i 0)
      2 (get xs (+ rbase i) 0)
      i)))

(defn safe-assoc [xs i value]
  (if (neg? i)
    (assoc xs 0 value)
    (if (< (count xs) i)
      (safe-assoc (assoc xs (count xs) 0) i value)
      (assoc xs i value))))

(defn add [s1 s2 ds xs]
  (safe-assoc xs ds (+ s1 s2)))

(defn mul [s1 s2 ds xs]
  (safe-assoc xs ds (* s1 s2)))

(defn rin
  ([ds xs] (safe-assoc xs ds (read)))
  ([ds xs in] (go (safe-assoc xs ds (<! in)))))

(defn put
  ([s1 xs] (println s1) xs)
  ([s1 xs out] (go (>! out s1) xs)))

(defn jit [s1 ds]
  (when (not= 0 s1)
    ds))

(defn jif [s1 ds]
  (when (zero? s1)
    ds))

(defn slt [s1 s2 ds xs]
  (safe-assoc xs ds (if (< s1 s2) 1 0)))

(defn stq [s1 s2 ds xs]
  (safe-assoc xs ds (if (= s1 s2) 1 0)))

(defn rbs [s1 rbase]
 (let [sum (+ rbase s1)]
    (if (neg? sum)
      0
      sum)))

(defn nop [& _]
  nil)

(def ops {1 {:fun add
             :args [false false true]
             :pcplus nop}
          2 {:fun mul
             :args [false false true]
             :pcplus nop}
          3 {:fun rin
             :args [true]
             :pcplus nop}
          4 {:fun put
             :args [false]
             :pcplus nop}
          5 {:fun nop
             :args [false false]
             :pcplus jit}
          6 {:fun nop
             :args [false false]
             :pcplus jif}
          7 {:fun slt
             :args [false false true]}
          8 {:fun stq
             :args [false false true]}
          9 {:fun nop
             :args [false]
             :rbase-fn rbs}
          99 {:fun :hal
              :args 0}})

(defn extract-params [[op & rest] xs start rbase]
  (mapv #(get-value %1 %2 %3 rbase xs)
       (->> xs
            (drop start)
            (next)
            (take (-> (get ops op) (:args) count)))
       rest
       (-> (get ops op) (:args))))

(defn run [xs]
  (loop [xs xs
         start 0
         rbase 0]
    (let [op (->> start (nth xs) (extract-op))
          op-code (first op)]
      (if (= 99 op-code)
        xs
        (let [params (extract-params op xs start rbase)
              {:keys [fun pcplus rbase-fn args]
               :or {fun nop pcplus nop rbase-fn nop}} (get ops op-code)]
          (recur
            (or (apply fun (conj params xs)) xs)
            (or (apply pcplus params) (+ start 1 (count args)))
            (or (apply rbase-fn (conj params rbase)) rbase)))))))

(def running-chars (atom ["/" "-" "\\" "|"]))

(defn print-running []
  (print "[" (first @running-chars) "]" "\r")
  (reset! running-chars (take 4 (next (cycle @running-chars)))))

(defn run' [xs in out]
  (go-loop [xs xs
            start 0
            rbase 0]
    #_(print-running)
    (let [op (->> start (nth xs) (extract-op))
          op-code (first op)]
      (if (= 99 op-code)
        (do
          #_(print "       \r")
          (close! in)
          (close! out)
          xs)
        (let [params (extract-params op xs start rbase)
              {:keys [fun pcplus rbase-fn args]
               :or {fun nop pcplus nop rbase-fn nop}} (get ops op-code)
              result (apply fun (if (some #{op-code} [3 4])
                                 (conj params xs (if (= op-code 3)
                                                   in
                                                   out))
                                 (conj params xs)))]
          (recur
            (or (try (<! result)
                     (catch Exception _ result))
                xs)
            (or (apply pcplus params) (+ start 1 (count args)))
            (or (apply rbase-fn (conj params rbase)) rbase)))))))
