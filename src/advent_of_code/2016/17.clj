(ns advent-of-code.2016.17
  (:require [digest]))

(def input "lpvhkcbi")
(def testinput "hijkl")

;; hack to avoid loops
(def limit 30)

(defn open? [c]
  (some #(= c %) "bcdef"))


(defn move-possible? [pos dir]
  (let [[x y] pos]
    (or
      (and (= :up dir) (> y 0))
      (and (= :down dir) (< y 3))
      (and (= :left dir) (> x 0))
      (and (= :right dir) (< x 3))
      )
    )
  )


(defn move [pos dir]
  (let [[x y] pos]
    (case dir
      :up [x (dec y)]
      :down [x (inc y)]
      :left [(dec x) y]
      :right [(inc x) y]
      )
    )
  )


(defn shortest-path [puzzle-key]
  (loop [paths [[[0 0] ""]]]
    (if (empty? paths)
      (do
        (prn "No paths to vault")
        nil
        )
      (let [[pos path] (first paths)]
        (cond
          (= pos [3 3]) path
          :else
          (let
            [next-hash (digest/md5 (str puzzle-key path))
             move-up
             (when (and (move-possible? pos :up) (open? (nth next-hash 0)))
               [(move pos :up) (str path \U)])
             move-down
             (when (and (move-possible? pos :down) (open? (nth next-hash 1)))
               [(move pos :down) (str path \D)])
             move-left
             (when (and (move-possible? pos :left) (open? (nth next-hash 2)))
               [(move pos :left) (str path \L)])
             move-right
             (when (and (move-possible? pos :right) (open? (nth next-hash 3)))
               [(move pos :right) (str path \R)])
             next-paths (filter #(not= nil %) [move-up move-down move-left move-right])
             ]
            (recur (apply conj (subvec paths 1) next-paths))
            )
          )
        )
      )
    )
  )

(defn all-paths [puzzle-key]
  (loop [paths [[[0 0] ""]] valid-paths []]
    (if (empty? paths)
      valid-paths
      (let [[pos path] (first paths)
            remaining (subvec paths 1)]
        (cond
          (= pos [3 3]) (recur remaining (conj valid-paths path))
          :else
          (let
            [next-hash (digest/md5 (str puzzle-key path))
             move-up
             (when (and (move-possible? pos :up) (open? (nth next-hash 0)))
               [(move pos :up) (str path \U)])
             move-down
             (when (and (move-possible? pos :down) (open? (nth next-hash 1)))
               [(move pos :down) (str path \D)])
             move-left
             (when (and (move-possible? pos :left) (open? (nth next-hash 2)))
               [(move pos :left) (str path \L)])
             move-right
             (when (and (move-possible? pos :right) (open? (nth next-hash 3)))
               [(move pos :right) (str path \R)])
             next-paths (filter #(not= nil %) [move-up move-down move-left move-right])
             ]
            (recur (apply conj remaining next-paths) valid-paths)
            )
          )
        )
      )
    )
  )





(defn solve1 []
  (let [path (shortest-path input)]
    (prn path)
    (when (not= nil path) (prn (.length path)))
    )
  )

(defn solve2 []
  (let [paths (all-paths input)
        lengths (map #(.length %) paths)]
        ;; longest (min-key count paths)]
    (prn (apply max lengths))
    )
  )
