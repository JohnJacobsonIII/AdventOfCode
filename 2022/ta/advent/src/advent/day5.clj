(ns advent.day5
  (:require [advent.utils :as utils]
            [clojure.string :as str]))

(def sample-start-state
  [nil
   (list \N \Z)
   (list \D \C \M)
   (list \P)])

(def sample-commands
  [[1 2 1]
   [3 1 3]
   [2 2 1]
   [1 1 2]])

(defn execute-cmd [state cmd]
  (let [[amnt from to] cmd]
    (as-> state st
      (update st to #(apply conj % (take amnt (st from))))
      (update st from #(drop amnt %)))))

(defn execute-commands [cmds state]
  (reduce execute-cmd state cmds))

(defn tops-of-state [state]
  (->> state
       (drop 1)
       (map first)
       (reduce str)))

(tops-of-state (execute-commands sample-commands sample-start-state))

(def big-start-state
  [nil
   (list \F \R \W)
   (list \P \W \V \D \C \M \H \T)
   (list \L \N \Z \M \P)
   (list \R \H \C \J)
   (list \B \T \Q \H \G \P \C)
   (list \F \Z \L \W \C \G)
   (list \C \G \J \Z \Q \L \V \W)
   (list \C \V \T \W \F \R \N \P)
   (list \V \S \R \G \H \W \J)])

(def big-commands
  (->> (utils/read-input "day5.txt")
       (str/split-lines)
       (map #(str/split % #" "))
       (map (fn [[_ amnt _ from _ to]]
              (map #(Integer/parseInt %) [amnt from to])))))

(tops-of-state (execute-commands big-commands big-start-state))

(defn execute-cmd-9001 [state cmd]
  (let [[amnt from to] cmd]
    (as-> state st
      (update st to #(apply conj % (reverse (take amnt (st from)))))
      (update st from #(drop amnt %)))))

(defn execute-commands-9001 [cmds state]
  (reduce #(do
             (println %1)
             (execute-cmd-9001 %1 %2)) state cmds))

(tops-of-state (execute-commands-9001 sample-commands sample-start-state))
(tops-of-state (execute-commands-9001 big-commands big-start-state))