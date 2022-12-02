(ns advent.day2
  [:require
   [advent.utils :as utils]
   [clojure.string :as str]])

; Data
(def sample-set
  [["A" "Y"]
   ["B" "X"]
   ["C" "Z"]])

(def big-set
  (as-> (utils/read-input "day2.txt") in
    (str/split-lines in)
    (map #(str/split % #" ") in)))

;Day 2 - Part 1
(defn op-str->rps [s]
  (case s
    "A" :rock
    "B" :paper
    "C" :scissors))

(defn my-str->rps [s]
  (case s
    "X" :rock
    "Y" :paper
    "Z" :scissors))

(defn points-for-round [round]
  (case round
    [:rock :paper] 6
    [:scissors :rock] 6
    [:paper :scissors] 6
    [:rock :rock] 3
    [:paper :paper] 3
    [:scissors :scissors] 3
    [:paper :rock] 0
    [:scissors :paper] 0
    [:rock :scissors] 0))

(defn points-for-choice [[_ my-choice]]
  (case my-choice
    :rock 1
    :paper 2
    :scissors 3))

(defn points-for-strategy [strat]
  (->> strat
       (map (fn [[op me]] [(op-str->rps op) (my-str->rps me)]))
       (map (fn [round] (+ (points-for-round round) (points-for-choice round))))
       (reduce +)))

(points-for-strategy sample-set)
(points-for-strategy big-set)

;Day 2 - Part 2

(defn str->result [s]
  (case s
    "X" :lose
    "Y" :draw
    "Z" :win))

(defn make-result [op desired-res]
  (case [op desired-res]
    [:rock :lose] :scissors
    [:rock :draw] :rock
    [:rock :win] :paper
    [:paper :lose] :rock
    [:paper :draw] :paper
    [:paper :win] :scissors
    [:scissors :lose] :paper
    [:scissors :draw] :scissors
    [:scissors :win] :rock))

(defn points-for-strategy2 [strat]
  (->> strat
       (map (fn [[op me]]
              (let [op-move (op-str->rps op)]
                [op-move (make-result op-move (str->result me))])))
       (map (fn [round] (+ (points-for-round round) (points-for-choice round))))
       (reduce +)))

(points-for-strategy2 sample-set)
(points-for-strategy2 big-set)