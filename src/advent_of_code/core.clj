(ns advent-of-code.core
  (:gen-class)
  (:require [advent-of-code.2015.01 :as P01])
  (:require [advent-of-code.2015.02 :as P02])
  )

(defn solve [day problemNum]
  (let [d (read-string day),
        p (read-string problemNum)]

    (case (+ (* 10 d) p)
      11 (P01/solve1)
      12 (P01/solve2)
      21 (P02/solve1)
      22 (P02/solve2)
      )
    )
  )

(defn printUsage []
  (println "Usage: <progname> <day> <1|2>"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (< (count args) 2) (printUsage) (solve (nth args 0) (nth args 1))))
