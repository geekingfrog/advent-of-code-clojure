(ns advent-of-code.2016.05
  (:require [clojure.string :as str])
  (:require [digest :as digest]))

(def input "reyedfim")

(defn hash-with-prefix [prefix n]
  (digest/md5 (str prefix n)))

(defn solve1 []
  (let [hashes (map (partial hash-with-prefix input) (range))
        hits (filter (fn [h] (str/starts-with? h "00000")) hashes)
        code (map #(nth %1 5) hits)
        ]
    (prn (apply str (take 8 code)))))

;; For some reason, the (->>) version takes ages (or maybe just doesn't terminate,
;; haven't checked. This is weird.
  ;; (->> (range)
  ;;      (filter (fn [h] (str/starts-with? h "00000")))
  ;;      (map #(nth %1 5))
  ;;      (take 8)
  ;;      (apply str)
  ;;      (prn)
  ;;      ))

(defn digit-in-range? [c]
  (and (>= (int c) 48) (<= (int c) 55)))

(defn search-from [prefix start]
  (loop [n start]
    (let [hashed (hash-with-prefix prefix n)]
      (if (and (str/starts-with? hashed "00000") (digit-in-range? (nth hashed 5)))
        (list n hashed)
        (recur (inc n))
        )
      )
    )
  )

(defn search-digits [prefix]
  (loop [n 0 found-digits {} pos-to-find 0]
    (if (> pos-to-find 7)
      found-digits
      (if (contains? found-digits pos-to-find)
        (recur n found-digits (inc pos-to-find))
        (let [[n' hashed] (search-from prefix n)
              pos (read-string (str (nth hashed 5)))
              d (nth hashed 6)]
          (prn "got hit" pos d (keys found-digits) pos-to-find)
          (if (contains? found-digits pos)
            ;; ignore this hash, digit already found
            (recur (inc n') found-digits pos-to-find)

            ;; remember the digit for this position and continue searching
            (recur (inc n') (assoc found-digits pos d) pos-to-find)
            )
          )
        )
      )
    )
  )


(defn solve2 []
  (prn
    (->> (search-digits input)
         (sort)
         (map second)
         (apply str)
         )
    )
  )
