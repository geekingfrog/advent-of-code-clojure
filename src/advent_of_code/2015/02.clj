(ns advent-of-code.2015.02
  (:require [clojure.string :as str])
  )

(defn parseLine [l]
  (sort (map read-string (str/split l #"x")))
  )

(defn parse-data []
  (with-open [rdr (clojure.java.io/reader "resources/2015/day02.txt")]
    (doall (map parseLine (line-seq rdr)))
    )
  )

(defn wrap-area [[l w h]]
  (let [area (* 2 (+ (* l w) (* w h) (* l h)))
        slack (* l w)]
    (+ area slack)
    )
  )

(defn ribbon-length [[l w h]]
  (let [wrap (* 2 (+ l w))
        bow (* l w h)]
    (+ wrap bow)
  ))

(defn solve1 []
  (println (reduce + (map wrap-area (parse-data))))
  )

(defn solve2 []
  (println (reduce + (map ribbon-length (parse-data))))
  )
