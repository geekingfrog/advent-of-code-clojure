(ns advent-of-code.2016.14
  (:require [digest]))

(def input "abc")
;; (def input "jlmsuwbz")

(defn md5 [s] (digest/md5 (str input s)))

(defn stretched-md5 [s]
  (nth (iterate digest/md5 (digest/md5 (str input s))) 2016))

(defn has-triplet? [s]
  (let [triples (map list s (rest s) (drop 2 s))
        eq-triples (filter #(apply = %) triples)
        ]
    (when (not-empty eq-triples) (first (first eq-triples)))
    )
  )


(defn has-next-hash? [hash-fn i c]
  (let [substr (str c c c c c)
        stuff (->> (range (inc i) (+ i 1000))
                (map hash-fn)
                (some #(.contains % substr))
                )
        ]
    (when stuff (println (str "got key " i)))
    stuff
    )
  )


(defn solve1 []
  (->> (range)
    (map #(vector %1 (has-triplet? (md5 %))))
    (filter #(not= nil (second %)))
    (filter #(apply (partial has-next-hash? md5) %))
    (take 64)
    ;; (last)
    (println)
    )
  )


(defn has-quint? [s]
  (let [fives (map list s (rest s) (drop 2 s) (drop 3 s) (drop 4 s))
        eq-fives (filter #(apply = %) fives)
        ]
    (when (not-empty eq-fives) (first (first eq-fives)))
    )
  )

(defn prev-hash? [i c]
  (let [start (max 0 (- i 1000))
        idxs (range start i)
        substr (str c c c)
        hashes (map #(vector %1 (stretched-md5 %1)) idxs)
        ok-hashes (filter #(.contains (second %) substr) hashes)
        ]
    (println i c)
    (when (not-empty ok-hashes) (println (str "got key " (first (first ok-hashes)))))
    (first ok-hashes)
    )
  )

(defn solve2 []
  (->>
    (range)
    (map #(vector %1 (has-triplet? (stretched-md5 %))))
    (filter #(not= nil (second %)))
    (filter #(apply (partial has-next-hash? stretched-md5) %))
    (take 64)
    (println)
    ))

;; (defn solve2 []
;;   (->>
;;     (range)
;;     (map #(vector %1 (has-quint? (stretched-md5 %1))))
;;     (filter #(not= nil (second %)))
;;     ;; (map #(apply prev-hash? %))
;;     ;; (filter #(not= nil %))
;;     (take 2)
;;     ;; (map #(vector %1 (has-triplet? (stretched-md5 %))))
;;     ;; (filter #(not= nil (second %)))
;;     ;; (filter #(apply (partial has-next-hash? stretched-md5) %))
;;     ;; (take 64)
;;     ;; (last)
;;     (map #(vector %1 (stretched-md5 (first %1))))
;;     (println)
;;     )
;;   )

;; 10 25

;; 158236 too high
