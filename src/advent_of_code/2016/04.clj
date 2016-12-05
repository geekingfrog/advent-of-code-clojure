(ns advent-of-code.2016.04
  (:require [instaparse.core :as insta])
  (:require [clojure.string :as str])
  )

(def grammar "
  <line> = room-name <'-'> sector-id <'['> hash <']'>
  room-name = (letter | '-')+
  sector-id = digit+
  hash = letter+
  <letter> = #'[a-z]'
  <letter-and-dashes> = #'[a-z]'
  <digit> = #'[0-9]'
  ")

(def line-parser (insta/parser grammar))

(defn parse-line [l]
  (let [transform-map
        {:sector-id (fn [& digits] (read-string (apply str digits)))
         :room-name (fn [& letters] (apply str (filter (partial not= "-") letters)))
         :hash str
         }]
    (insta/transform transform-map (line-parser l))
    ))

(defn common-letters [s]
  (->> s
      (group-by identity)
      (reduce-kv #(assoc %1 %2 (count %3)) {})
      (sort-by first)
      (reverse)
      (sort-by second)
      (reverse)
      (take 5)
      (keys)
    ))

(defn real-room? [[room-name _ room-hash]]
  (.equals (apply str (common-letters room-name)) room-hash))

(defn read-data []
  (->> "resources/2016/day04.txt"
       (slurp)
       (str/split-lines)
       (map parse-line)))

(defn solve1 []
  (prn (->> (read-data)
            (filter real-room?)
            (map second)
            (reduce + 0)
            )))

;; Inspired from http://rosettacode.org/wiki/Rot-13#Clojure

(def A (into #{} (map char (range (int \a) (+ (int \a) 26)))))

(defn rot [n s]
  (let [Am (->> (cycle A) (drop n) (take 26) (zipmap A))]
    (apply str (map Am s))))

(defn decrypt-name [[room-name sector-id _]]
  (rot sector-id room-name))

(defn solve2 []
  (let [rooms (->> (read-data) (filter real-room?))
        names-and-sector (map (fn [x] [(decrypt-name x) (second x)]) rooms)]
    (prn (filter (fn [[n _]] (= n "northpoleobjectstorage")) names-and-sector))))
