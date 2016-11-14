(ns advent-of-code.2015.04
  (:require [digest])
  )

(def secret-key "yzbqklnj")

(defn mine [prefix start]
  (loop [n start]
    (let [computed-hash (digest/md5 (str secret-key n))]
      (if (.startsWith computed-hash prefix)
        n
        (recur (inc n))
        )
      )))

(defn solve1 []
  (let [prefix (apply str (repeat 5 \0))]
    (println (mine prefix 0))
    ))

(defn solve2 []
  (let [prefix (apply str (repeat 6 \0))]
    (println (mine prefix 0))
    ))

