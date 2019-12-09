            [clojure.string :as string]
            [clojure.set :as c-set]

(defn no-dec? [s]
  (= (seq s) (sort (seq s))))

(defn consecutives? [s]
  (->> s
    (partition-by identity)
    (some #(< 1 (count %)))))

(defn consecutives?' [s]
  (->> s
    (partition-by identity)
    (some #(= 2 (count %)))))


(defn valid? [i]
  (->> (str i)
       (#((every-pred consecutives?
                 no-dec?) %))))

(defn valid?' [i]
  (->> (str i)
       (#((every-pred consecutives?'
                 no-dec?) %))))

(defn part-1 []
  (->> (range 128392 643281)
       (filter valid?)
       (count)))

(defn part-2 []
  (->> (range 128392 643281)
       (filter valid?')
       (count)))

