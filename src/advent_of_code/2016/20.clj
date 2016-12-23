(ns advent-of-code.2016.20
  (:require [clojure.string :as str]))

(defn get-data []
  (->>
    (slurp "resources/2016/day20.txt")
    (str/split-lines)
    (map #(map read-string (str/split % #"-")))
    (sort-by second)
    (sort-by first)
    )
  )

(defn find-lowest [ranges]
  (loop [rs ranges lowest 0]
    (if (empty? rs)
      lowest
      (let [[low high] (first rs)]
        (if (>= lowest low)
          (recur (rest rs) (max lowest (inc high)))
          lowest
        )
      )
    )
  ))

;; (defn solve1 [] (println (find-lowest ['(0 2) '(4 7) '(5 8)])))
(defn solve1 []
  (->>
    (get-data)
    (find-lowest)
    (println)
    )
  )


(defn find-allowed [ranges]
  (loop [rs ranges lowest 0 acc 0]
    (if (empty? rs)
      acc
      (let [[low high] (first rs)]
        (if (>= lowest low)
          (recur (rest rs) (max lowest high) acc)
          (recur (rest rs) high (+ acc (- low lowest 1)))
          )
        )
      )
    ))

(defn solve2 []
  (->>
    (get-data)
    (find-allowed)
    (println)
    )
  )
