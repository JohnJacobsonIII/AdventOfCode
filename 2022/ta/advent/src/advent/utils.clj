(ns advent.utils
  (:require [clojure.java.io]
            [clojure.string :as str]))

(defn read-input
  "Takes a file `day` from the resources folder and returns its contents."
  [day]
  (slurp (clojure.java.io/resource day)))

(defn read-numbers
  "Takes a string of numbers separated by newlines and returns a array of parsed integers."
  [data]
  (->> data
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

(defn read-string+nums 
  "Takes a string containing lines with a word followed by a number and parses it into a array of string integer pairs."
  [data]
  (as-> data $
    (str/split $ #"\r\n")
    (map #(str/split % #" ") $)
    (map (fn [[word n]] [word (Integer/parseInt n)]) $)))
