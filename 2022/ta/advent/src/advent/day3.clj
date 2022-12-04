(ns advent.day3
  (:require [clojure.set :as set]
            [advent.utils :as utils]
            [clojure.string :as str]))

(def sample-set
  ["vJrwpWtwJgWrhcsFMMfFFhFp"
   "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
   "PmmdzqPrVvPwwTWBwg"
   "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
   "ttgJtRGJQctTZtZT"
   "CrZsJsPPZsGzwwsLwLmpwMDw"])

(def big-set
  (-> (utils/read-input "day3.txt")
      (str/split-lines)))

(def priority-map
  (reduce into {}
          (map
           (fn [c p] (assoc {} (char c) p))
           (concat
            (range (int \a) (inc (int \z)))
            (range (int \A) (inc (int \Z))))
           (range 1 53))))

(defn priority [ch]
  (priority-map ch))

(defn item-in-both-compartments [rucksack]
  (->> (split-at (/ (count rucksack) 2) rucksack)
       (map set)
       (reduce set/intersection)
       (first)))

(item-in-both-compartments (sample-set 0))

(defn priority-sum [rucksacks]
  (->> rucksacks
       (map item-in-both-compartments)
       (map priority)
       (reduce +)))

(priority-sum sample-set)
(priority-sum big-set)

(defn common-in-threes [rucksacks]
  (->> rucksacks
       (partition 3)
       (map #(->> %
                  (map set)
                  (reduce set/intersection)
                  (first)
                  (priority)))
       (reduce +)))

(common-in-threes sample-set)
(common-in-threes big-set)
