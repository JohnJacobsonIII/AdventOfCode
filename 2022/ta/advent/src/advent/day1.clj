(ns advent.day1
  [:require
   [advent.utils :as utils]
   [clojure.string :as str]])

(def test-data
  [[1000 2000 300]
   [4000]
   [5000 6000]
   [7000 8000 9000]
   [10000]])

(defn read-grouped-input [input]
  (as-> input in
    (str/split in #"\n\n")
    (map #(str/split-lines %) in)
    (map (fn [group] (map #(Integer/parseInt %) group)) in)))

(def big-set
  (read-grouped-input (utils/read-input "day1.txt")))
; Day 1 - Part 1
(defn max-calories
  [elves]
  (apply max (map (partial apply +) elves)))

(max-calories test-data)
(max-calories big-set)

; Day 1 - Part 2
(defn sum-top-three-cals 
  [elves]
  (->> elves
    (map (partial apply +))
    (sort-by #(- %))
    (take 3)
    (apply +)))

(sum-top-three-cals test-data)
(sum-top-three-cals big-set)