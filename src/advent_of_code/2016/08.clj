(ns advent-of-code.2016.08
  (:require [clojure.string :as str])
  (:require [instaparse.core :as insta]))

(def input-src "resources/2016/day08.txt")
(def W 50)
(def H 6)
(def initial-grid (apply vector (take (* W H) (repeat 0))))

(def grammar "
  <instruction> = rect | rotate-row | rotate-column
  rect = <'rect '> num <'x'> num
  rotate-row = <'rotate row y='> num <' by '> num
  rotate-column = <'rotate column x='> num <' by '> num
  num = #'[0-9]+'
  ")

(def line-parser (insta/parser grammar))

(defn parse-line [l]
  (let [transform-map
        {:num (fn [& digits] (apply read-string digits))}]
    (insta/transform transform-map (line-parser l))
    )
  )

(defn parse-data []
  (apply concat (map parse-line (str/split-lines (slurp input-src)))))


(defn apply-rect [grid tx ty]
  (let [idx' [(for [x (range tx) y (range ty)] [(+ x (* y W)) 1])]
        idx (apply concat idx')]
    (apply assoc grid (apply concat idx))))

(defn apply-row [grid tx ty]
  (let [idx (map #(+ (* tx W) %1) (range W))
        offseted-idx (map #(mod (- %1 ty) W) (range W))
        adjusted-idx (map #(+ (* tx W) %1) offseted-idx)
        new-vals (map grid adjusted-idx)
        update-vector (apply concat (map vector idx new-vals))
        ]
    (apply assoc grid update-vector)))

(defn apply-col [grid tx ty]
  (let [idx (map #(+ tx (* W %1)) (range H))
        offset (* ty W)
        val-idx (map #(+ tx (- (* W %1) offset)) (range H))
        adjusted-idx (map #(mod %1 (* W H)) val-idx)
        new-vals (map grid adjusted-idx)
        update-vector (apply concat (map vector idx new-vals))
        ]
    (apply assoc grid update-vector)))


(defn apply-transfo [grid [ttype tx ty]]
  (case ttype
    :rect (apply-rect grid tx ty)
    :rotate-row (apply-row grid tx ty)
    :rotate-column (apply-col grid tx ty)
    ((prn ttype "-" tx "," ty)
     (throw (Exception. "boom"))
     )
    ))

(defn compute-grid []
  (->> (parse-data)
       (reduce apply-transfo initial-grid)
       ))

(defn solve1 []
    (prn (count (filter #(= 1 %1) (compute-grid))))
  )


(defn display-grid [grid]
  (let [symbols (map #(if (= 1 %1) \# \.) grid)
        lines (partition W symbols)
        display (map #(apply str %1) lines)]
    (doseq display prn)))


(defn solve2 []
  (display-grid (compute-grid)))
