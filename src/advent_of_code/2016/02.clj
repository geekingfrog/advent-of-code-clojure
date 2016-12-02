(ns advent-of-code.2016.02
  (:require [clojure.string :as str]))

(defn read-data []
  (str/split-lines (slurp "resources/2016/day02.txt")))

(defn coord-to-key [[x y]]
  (+ 1 (+ x (* y 3))))

(defn move [[x y] dir]
  (case dir
    \U [x (max 0 (- y 1))]
    \R [(min 2 (+ 1 x)) y]
    \D [x (min 2 (+ y 1))]
    \L [(max 0 (- x 1)) y]))

(defn move2 [[x y] dir]
  (let [[x' y']
        (case dir
          \U [x (- y 1)]
          \R [(+ x 1) y]
          \D [x (+ y 1)]
          \L [(- x 1) y]
          )
        dist (+ (Math/abs (- x' 2)) (Math/abs (- y' 2)))]
    (if (> dist 2) [x y] [x' y'])))

(defn coord-to-key2 [[x y]]
  (case [x y]
    [2 0] \1
    [1 1] \2
    [2 1] \3
    [3 1] \4
    [0 2] \5
    [1 2] \6
    [2 2] \7
    [3 2] \8
    [4 2] \9
    [1 3] \A
    [2 3] \B
    [3 3] \C
    [2 4] \D))

(defn process-line [start line]
  reduce move start line)

(defn process-code [lines]
  ;; Why (reductions process-line [1 1] lines) doesn't work???
  (let [codes (rest (reductions (fn [start line] (reduce move start line)) [1 1] lines))]
   (map coord-to-key codes)))

(defn process-code2 [lines]
  (let [codes (rest (reductions (fn [start line] (reduce move2 start line)) [0 2] lines))]
    (map coord-to-key2 codes)))

(defn solve1 [] (prn (process-code (read-data))))
(defn solve2 [] (prn (process-code2 (read-data))))
