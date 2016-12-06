(ns advent-of-code.2016.06
  (:require [clojure.string :as str]))

(defn read-data []
  (str/split-lines (slurp "resources/2016/day06.txt")))

(defn read-cols [lines]
  (let [line-length (count (first lines))]
    (map (fn [i] (apply str (map #(nth %1 i) lines))) (range line-length))))

(defn count-letters [word]
  (let [reducer (fn [m letter] (if (contains? m letter) (assoc m letter (inc (m letter))) (assoc m letter 1)))]
    (reduce reducer {} word)))

(defn most-common-letter [word]
  (->> word
       (count-letters)
       (sort-by second)
       (reverse)
       (first)
       (first)))

(defn least-common-letter [word]
  (->> word
       (count-letters)
       (sort-by second)
       (first)
       (first)))

(defn solve1 []
  (prn (->> (read-data)
            (read-cols)
            (map most-common-letter)
            (apply str))))

(defn solve2 []
  (prn (->> (read-data)
            (read-cols)
            (map least-common-letter)
            (apply str))))
