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


(defn chunk-compressed [s]
  (prn "chunking " s)
  (let [[before raw-marker after] (str/split s #"[\(\)]" 3)]
    (if (nil? raw-marker)
      [before]
      (let [[char-count repeat-count] (parse-marker raw-marker)
            chunked (.substring 0 char-count (str raw-marker after))
            remaining (.substring char-count (str raw-marker after))]
        (apply conj [before chunked] (chunk-compressed remaining))
       )
      )
    )
  )


(defn decompress' [compressed]
  (loop [s compressed acc ""]
    (prn "decompressing " s)
    (if (empty? s)
      acc
      (let [[before raw-marker after] (str/split s #"[\(\)]" 3)]
        ;; (prn (str "triplet: " before " - " raw-marker " - " after))
        (if (nil? raw-marker)
          (str acc before)
          (let [[char-count repeat-count] (parse-marker raw-marker)
                inner-str (decompress' (.substring after 0 char-count))
                repeated (apply str (repeat repeat-count inner-str))
                remaining (.substring after char-count)
                ]
            ;; (prn "inner str: " inner-str)
            ;; (prn "repeated: " repeated)
            (str before repeated)
            ;; (recur remaining (str acc before repeated))
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
       (chunk-compressed)
       ;; (decompress')
       ;; (count)
       (prn)
       )
  )
  ;; (prn (decompress' "X(8x2)(3x3)ABCY")))
;; (->> "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
