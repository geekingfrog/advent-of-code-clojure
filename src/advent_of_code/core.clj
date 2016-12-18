(ns advent-of-code.core
  (:gen-class)
  (:require [advent-of-code.2015.01 :as P15_01])
  (:require [advent-of-code.2015.02 :as P15_02])
  (:require [advent-of-code.2015.03 :as P15_03])
  (:require [advent-of-code.2015.04 :as P15_04])
  (:require [advent-of-code.2015.05 :as P15_05])
  (:require [advent-of-code.2015.06 :as P15_06])
  (:require [advent-of-code.2016.01 :as P16_01])
  (:require [advent-of-code.2016.02 :as P16_02])
  (:require [advent-of-code.2016.03 :as P16_03])
  (:require [advent-of-code.2016.04 :as P16_04])
  (:require [advent-of-code.2016.05 :as P16_05])
  (:require [advent-of-code.2016.06 :as P16_06])
  (:require [advent-of-code.2016.07 :as P16_07])
  (:require [advent-of-code.2016.08 :as P16_08])
  (:require [advent-of-code.2016.09 :as P16_09])
  (:require [advent-of-code.2016.10 :as P16_10])
  (:require [advent-of-code.2016.17 :as P16_17])
  (:require [advent-of-code.2016.18 :as P16_18])
  )

(defn solve [year day problemNum]
  (let [idx (+ (* 10 day) problemNum)]

    (case year
      2015 (case idx
        11 (P15_01/solve1)
        12 (P15_01/solve2)
        21 (P15_02/solve1)
        22 (P15_02/solve2)
        31 (P15_03/solve1)
        32 (P15_03/solve2)
        41 (P15_04/solve1)
        42 (P15_04/solve2)
        51 (P15_05/solve1)
        52 (P15_05/solve2)
        61 (P15_06/solve1)
        62 (P15_06/solve2)
        (println "Not done yet"))
      2016 (case idx
        11 (P16_01/solve1)
        12 (P16_01/solve2)
        21 (P16_02/solve1)
        22 (P16_02/solve2)
        31 (P16_03/solve1)
        32 (P16_03/solve2)
        41 (P16_04/solve1)
        42 (P16_04/solve2)
        51 (P16_05/solve1)
        52 (P16_05/solve2)
        61 (P16_06/solve1)
        62 (P16_06/solve2)
        71 (P16_07/solve1)
        72 (P16_07/solve2)
        81 (P16_08/solve1)
        82 (P16_08/solve2)
        91 (P16_09/solve1)
        92 (P16_09/solve2)
        101 (P16_10/solve1)
        102 (P16_10/solve2)
        171 (P16_17/solve1)
        172 (P16_17/solve2)
        181 (P16_18/solve1)
        182 (P16_18/solve2)
        (println "Not done yet"))
      (println (str "Not a valid year " year))
    )
  )
  )

(defn printUsage []
  (println "Usage: <progname> <2015|2016> <day> <1|2>"))

(defn -main
  [& args]
  (if (< (count args) 3) (printUsage)
    (let [year (read-string (nth args 0))
          day (read-string (nth args 1))
          problemNumber (read-string (nth args 2))]
      (solve year day problemNumber))))
