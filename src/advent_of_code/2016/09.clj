(ns advent-of-code.2016.09
  (:require [clojure.string :as str]))

(defn read-data [] (first (str/split-lines (slurp "resources/2016/day09.txt"))))
;; (defn read-data [] "(27x12)(20x12)(13x14)(7x10)(1x12)A")
;; (defn read-data [] "A(2x2)BCD(2x2)EFG")


(defn parse-marker [s]
  (let [[full c r] (re-find #"(\d+)x(\d+)" s)]
    [(read-string c) (read-string r)]))

(defn decompress [compressed]
  (loop [s compressed acc ""]
    (if (empty? s)
      acc
      (let [[before raw-marker after] (str/split s #"[\(\)]" 3)]
        (if (nil? raw-marker)
          (recur "" (str acc before))
          (let [ [char-count repeat-count] (parse-marker raw-marker)
                [to-repeat remaining] (map #(apply str %1) (split-at char-count after))
                repeated (apply str (repeat repeat-count to-repeat))
                ]
            (recur remaining (str acc before repeated))
            )
          )
        )
      )
    )
  )


(defn decompress'
  " Only keep track of length, otherwise heap space error since string too big"
  [compressed]
  (loop [s compressed acc 0]
    (if (empty? s)
      acc
      (let [[before raw-marker after] (str/split s #"[\(\)]" 3)]
        (if (nil? raw-marker)
          (+ acc (.length before))
          (let [[char-count repeat-count] (parse-marker raw-marker)
                inner-str-count (decompress' (.substring after 0 char-count))
                repeated (* repeat-count inner-str-count)
                remaining (.substring after char-count)
                ]
            (recur remaining (+ acc (.length before) repeated))
            )
          )
        )
      )
    )
  )


(defn testing [s]
  (let [[before raw after] (str/split s #"[\(\)]" 3)]
    (list before raw after)))

(defn solve1 []
  (->> (read-data)
       (decompress)
       (count)
       (prn)
       )
  )

(defn solve2 []
  (->> (read-data)
       (decompress')
       (prn)
       )
  )
