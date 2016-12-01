(ns advent-of-code.2016.01
  (:require [clojure.string :as str]))

(def input "R4, R4, L1, R3, L5, R2, R5, R1, L4, R3, L5, R2, L3, L4, L3, R1, R5, R1, L3, L1, R3, L1, R2, R2, L2, R5, L3, L4, R4, R4, R2, L4, L1, R5, L1, L4, R4, L1, R1, L2, R5, L2, L3, R2, R1, L194, R2, L4, R49, R1, R3, L5, L4, L1, R4, R2, R1, L5, R3, L5, L4, R4, R4, L2, L3, R78, L5, R4, R191, R4, R3, R1, L2, R1, R3, L1, R3, R4, R2, L2, R1, R4, L5, R2, L2, L4, L2, R1, R2, L3, R5, R2, L3, L3, R3, L1, L1, R5, L4, L4, L2, R5, R1, R4, L3, L5, L4, R5, L4, R5, R4, L3, L2, L5, R4, R3, L3, R1, L5, R5, R1, L3, R2, L5, R5, L3, R1, R4, L5, R4, R2, R3, L4, L5, R3, R4, L5, L5, R4, L4, L4, R1, R5, R3, L1, L4, L3, L4, R1, L5, L1, R2, R2, R4, R4, L5, R4, R1, L1, L1, L3, L5, L2, R4, L3, L5, L4, L1, R3")

;; Orientation
;; North: 0
;; East: 1
;; South: 2
;; West: 3

;; Direction:
;; R: rotate clockwise: +1
;; L: rotate cclockwise: -1

(defn rotate [orientation direction]
  (case direction
    \R (mod (+ orientation 1) 4)
    \L (mod (- orientation 1) 4)
    orientation
    ))

(def parsedInput (str/split input #", "))

(defn walk [[orientation x y] rawDirection]
  (let [direction (first rawDirection)
        length (read-string (.substring rawDirection 1))
        newOrientation (rotate orientation direction)]
    (case newOrientation
      0 [newOrientation x (+ y length)]
      1 [newOrientation (+ x length) y]
      2 [newOrientation x (- y length)]
      3 [newOrientation (- x length) y])))

(defn solve1 []
  (let [[finalOrientation x y] (reduce walk '[0 0 0] parsedInput)]
    (println (+ (Math/abs x) (Math/abs y) ))))


(defn walkThorough [[[_ x0 y0] [o x1 y1]]]
  (case o
    0 (map (fn [n] [x0 (+ y0 n)]) (range 1 (+ 1 (- y1 y0))))
    1 (map (fn [n] [(+ x0 n) y0]) (range 1 (+ 1 (- x1 x0))))
    2 (map (fn [n] [x0 (- y0 n)]) (range 1 (+ 1 (- y0 y1))))
    3 (map (fn [n] [(- x0 n) y0]) (range 1 (+ 1 (- x0 x1))))
    ))

(defn solve2 []
  (let [states (reductions walk '[0 0 0] parsedInput)
        ;; all states, in a nested structure
        temp (map walkThorough (map vector states (rest states)))
        allStates (apply concat temp)
        bunnyLocation (loop [visitedStates #{[0 0]} remaining allStates]
                        (if (contains? visitedStates (first remaining))
                          (first remaining)
                          (recur (conj visitedStates (first remaining)) (rest remaining))
                          ))
        [bunnyX bunnyY] bunnyLocation]
    (println (+ (Math/abs bunnyX) (Math/abs bunnyY)))
    )
  )
