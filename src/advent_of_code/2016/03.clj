(ns advent-of-code.2016.03
    (:require [clojure.string :as str]))

(defn read-data []
  (let [raw (slurp "resources/2016/day03.txt")
        lines (str/split-lines raw)
        parse (fn [s] (map read-string (filter not-empty (str/split s #"\s+"))))]
  (map parse lines)))


;; (group n seq) returns a new sequence of n-tuple
(defn group [n lst]
  (loop [l lst acc '()]
    (if (empty? l)
      acc
      ;; (recur (drop 3 l) (list (first l) (nth l 1) (nth l 2))))))
      (let [[a b c & stuff] l]
        (recur stuff (cons (list a b c) acc))))))



(defn read-data2 []
  (let [raw (slurp "resources/2016/day03.txt")
        symbols (filter not-empty (str/split raw #"\s+"))
        parsed (map read-string symbols)
        col1 (group 3 (take-nth 3 parsed))
        col2 (group 3 (take-nth 3 (drop 1 parsed)))
        col3 (group 3 (take-nth 3 (drop 2 parsed)))]
    (concat col1 col2 col3)
    ))

(def temp '(101 301 501 102 302 502 103 303 503 201 401 601 202 402 602 203 403 603))

(defn triangle? [sides]
  (let [[a b c] (sort sides)]
    (> (+ a b) c)))

(defn solve1 []
  (let [data (read-data)
        triangles (filter triangle? data)]
    (prn (count triangles))))

(defn solve2 []
  (let [parsed (read-data2)
        triangles (filter triangle? parsed)]
    (prn (count triangles))))
