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

(defn find-first [f coll]
  (first (filter f coll)))

(defn search-digit [hashes pos]
  (let [okHash (find-first (fn [h] (= pos (nth h 5))) hashes)]
    (prn "okHash for pos " pos " - " okHash)
    (nth okHash 6)))

(defn search-digits [prefix]
  (let [hashes (map (partial hash-with-prefix prefix) (range))
        hits (filter (fn [h] (str/starts-with? h "00000")) hashes)]
    (map #(search-digit hits %1) [\0 \1 \2 \3 \4 \5 \6 \7])))


(defn solve2 []
  (prn (apply str (search-digits input))))
