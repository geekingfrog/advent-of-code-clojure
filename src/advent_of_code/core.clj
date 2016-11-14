(ns advent-of-code.core
  (:gen-class)
  (:require [advent-of-code.2015.01 :as P01])
  (:require [advent-of-code.2015.02 :as P02])
  (:require [advent-of-code.2015.03 :as P03])
  (:require [advent-of-code.2015.04 :as P04])
  (:require [advent-of-code.2015.05 :as P05])
  )

(defn solve [day problemNum]
  (let [d (read-string day),
        p (read-string problemNum)]

    (case (+ (* 10 d) p)
      11 (P01/solve1)
      12 (P01/solve2)
      21 (P02/solve1)
      22 (P02/solve2)
      31 (P03/solve1)
      32 (P03/solve2)
      41 (P04/solve1)
      42 (P04/solve2)
      51 (P05/solve1)
      52 (P05/solve2)
      )
    )
  )

(defn printUsage []
  (println "Usage: <progname> <day> <1|2>"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (< (count args) 2) (printUsage) (solve (nth args 0) (nth args 1))))
