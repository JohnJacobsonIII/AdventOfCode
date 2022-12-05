(ns advent.day4
  (:require [clojure.string :as str]
            [advent.utils :as utils]
            [clojure.set :as set]))

(def sample-set
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-set [data]
  (->> data
       (str/split-lines)
       (map (fn [line]
              (as-> line it
                (str/split it #",")
                (map (fn [num-range]
                       (as-> num-range it
                         (str/split it #"-")
                         (map #(Integer/parseInt %) it))) it))))))

(def parsed-sample-set (parse-set sample-set))

(def big-set
  (->> (utils/read-input "day4.txt")
       (parse-set)))

(defn range-contains-range? [[[a1 b1] [a2 b2]]]
  (or
   (and (<= a1 a2) (<= b2 b1))
   (and (<= a2 a1) (<= b1 b2))))

(defn num-fully-contained [data]
  (->> data
       (map range-contains-range?)
       (filter true?)
       (count)))

(num-fully-contained parsed-sample-set)
(num-fully-contained big-set)

(defn ranges-overlapping? [[[a1 b1] [a2 b2]]]
  (let [set1 (set (range a1 (inc b1)))
        set2 (set (range a2 (inc b2)))]
    (seq (set/intersection set1 set2))))

(defn num-overlapping [data]
  (->> data
       (map ranges-overlapping?)
       (filter #(not (nil? %)))
       (count)))

(num-overlapping parsed-sample-set)
(num-overlapping big-set)
