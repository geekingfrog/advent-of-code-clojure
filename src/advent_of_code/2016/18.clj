(ns advent-of-code.2016.18
  (:require [clojure.string :as str]))

(def input ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^....")
(def input-test ".^^.^.^^^^")

(defn nth' [coll idx]
  (cond
    (< idx 0) \.
    (>= idx (count coll)) \.
    :else (nth coll idx)
    )
  )

;; (defn trap? [[left mid right]]
(defn trap? [triple]
  (let [[left mid right] triple]
    (or
      (and (= \^ left) (= \^ mid) (not= \^ right))
      (and (not= \^ left) (= \^ mid) (= \^ right))
      (and (= \^ left) (not= \^ mid) (not= \^ right))
      (and (not= \^ left) (not= \^ mid) (= \^ right))
      )
    )
  )

(defn next-row [row]
  (let [idxs (range (count row))
        triples (map #(vec
                         [(nth' row (dec %))
                         (nth row %)
                         (nth' row (inc %))
                         ]) idxs)
        traps (map trap? triples)
        res (map #(if % \^ \.) traps)
        ]
    (apply str res)
    )
  )

(defn count-safe [row]
  (reduce + 0 (map #(if (= \. %) 1 0) row))
  )

(defn solve1 []
  (let [rows (iterate next-row input)
        room (take 40 rows)
        safe-count (reduce + 0 (map count-safe room))
        ]
    (prn safe-count)
    )
  )

(defn solve2 []
  (let [rows (iterate next-row input)
        room (take 400000 rows)
        safe-count (reduce + 0 (map count-safe room))
        ]
    (prn safe-count)
    )
  )

