(ns advent-of-code.2016.07
  (:require [clojure.string :as str]))

(defn read-data []
  (str/split-lines (slurp "resources/2016/day07.txt")))

(defn split-hypernet [s]
  (let [splitted (str/split s #"[\[\]]")
        supernets (take-nth 2 splitted)
        hypernets (take-nth 2 (rest splitted))]
    [supernets hypernets]))

(defn any? [coll]
  (loop [c coll]
    (if (empty? c)
      false
      (or (first c) (recur (rest c))))))

(defn valid-abba-match? [s]
  (if (nil? s)
    false
    (not= (nth s 1) (nth s 2))))

(defn has-abba? [s]
  (valid-abba-match? (re-find #"(\w)(\w)\2\1" s)))

(defn support-tls? [s]
  (let [[supernets hypernets] (split-hypernet s)
        abba-super (any? (map has-abba? supernets))
        abba-hyper (any? (map has-abba? hypernets))]
    (and (not abba-hyper) abba-super)
    )
  )

(defn solve1 []
  (->> (read-data)
       (filter support-tls?)
       (count)
       (prn)))

(defn valid-aba? [s]
  (if (nil? s)
    false
    (let [[_ a b] s]
      (not= a b)
      )
    )
  )

;; regex version doesn't work when aba overlaps. Example: zazbz
;; (defn find-all-aba [s]
;;   (map first (filter valid-aba? (re-seq #"(\w)(\w)\1" s))))

(defn find-all-aba [s]
  (let [triplets (map list s (rest s) (drop 2 s))
        str-triplets (map #(apply str %1) triplets)
        abas (filter #(and (= (first %1) (nth %1 2)) (not= (first %1) (nth %1 1))) str-triplets)]
    abas
    )
  )

(defn corresponding-bab [aba]
  (let [[a b _] aba]
    (apply str [b a b])))

(defn support-ssl? [s]
  (let [[supernets hypernets] (split-hypernet s)
        abas (apply concat (map find-all-aba supernets))
        babs (map corresponding-bab abas)
        valid-babs (filter (fn [bab] (any? (map #(str/includes? %1 bab) hypernets))) babs)]
    (not (empty? valid-babs))
    )
  )


(defn solve2 []
  (->> (read-data)
       (filter support-ssl?)
       (count)
       (prn)))
