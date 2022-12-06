(ns advent.day6 
  (:require [advent.utils :as utils]))

(def sample-input1
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def sample-input2
  "bvwbjplbgvbhsrlpgdmjqwftvncz")

(defn marker-position [stream]
  (loop [lst (partition 4 1 stream)
         idx 4]
    (if (= (-> lst first set count) 4)
      idx
      (recur (rest lst) (inc idx)))))

(marker-position sample-input1)
(marker-position sample-input2)

(def big-input 
  (utils/read-input "day6.txt"))

(marker-position big-input)

(defn start-of-message-position [stream]
  (loop [lst (partition 14 1 stream)
         idx 14]
    (if (= (-> lst first set count) 14)
      idx
      (recur (rest lst) (inc idx)))))

(start-of-message-position sample-input1)
(start-of-message-position sample-input2)

(start-of-message-position big-input)