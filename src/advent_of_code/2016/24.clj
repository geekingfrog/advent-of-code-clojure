(ns advent-of-code.2016.24
  (:require [clojure.string :as str]))

(defn base-matrix [w h]
  (loop [j 0 row (transient [])]
    (if (>= j h)
      (persistent! row)
      (let [col (loop [i 0 c (transient [])]
                  (if (>= i w)
                    (persistent! c)
                    (recur (inc i) (conj! c (if (= i j) 0 java.lang.Integer/MAX_VALUE)))
                    ))
            ]
        (recur (inc j) (conj! row col))
        )
      )
    )
  )

;; (defn graph-dist [g [i j i' j']]
;;   (let [row (g j)
;;         col (row i)
;;         a ((g j) i)
;;         b ((g j') i')
;;         ]
;;     (when (and (not= \# a) (not= \# b))
;;       1
;;       )
;;     ))

(defn nodes [g]
  ;; ignore the borders of the graphs
  (println "nodes g")
  (let [h (count g)
        w (count (first g))
        ]
    (do (for [i (range 1 (dec w))
              j (range 1 (dec h))
              :when (not= \# ((g j) i))
              ]
          (list i j)
          )
        )
    )
  )

(defn dist [i j i' j']
  (+ (Math/abs (- i' i)) (Math/abs (- j' j)))
  )


(defn graph-dist [g [i j] [i' j']]
  (cond
    (and (= i i') (= j j')) 0
    (< (dist i j i' j') 2) 1
    :else java.lang.Integer/MAX_VALUE
  ))


(defn floyd-warshall [g dists [k i j]]
  (let [dij (dists (list i j))
        dik (dists (list i k))
        dkj (dists (list k j))
        new-dist (+ dik dkj)
        ]
    (if (> dij new-dist)
      (assoc dists (list i j) new-dist)
      dists
      )
    )
  )


(defn base-dists [g]
  (println "base dists")
  (let [ns (nodes g)
        idxs (do (for [i ns j ns ]
                   (list i j)
                   ))
        ]
    (println "base-dists, number of steps: " (count idxs))
    (reduce #(assoc %1 %2 (apply graph-dist g %2)) {} idxs))
  )


(defn shortest-paths [g]
  (let
    [h (count g)
     w (count (first g))
     idxs (do (for [k (nodes g)
                    i (nodes g)
                    j (nodes g)
                    ]
                (list k i j)
                ))
     base (base-dists g)
     ]
    (println "number of steps: " (count idxs))
    (reduce (partial floyd-warshall g) base idxs)
    )
  )

(defn reduce-target [g m [i j]]
  (let [a ((g j) i)]
    (if (and (not= \. a) (not= \# a))
      (assoc m a (list i j))
      m
      )
    )
  )

(defn find-targets [g]
  (reduce (partial reduce-target g) {} (nodes g)))

;; (def testarr
;;   [(vec "#####")
;;    (vec "#0..#")
;;    (vec "#..1#")
;;    (vec "#####")
;;    ]
;;   )

(def testarr
  [(vec "###########")
   (vec "#0.1.....2#")
   (vec "#.#######.#")
   (vec "#4.......3#")
   (vec "###########")
   ]
  )

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn parse-data [path]
  (->>
    (slurp path)
    (str/split-lines)
    (map vec)
    (vec)
    ))

(defn path-length [dists targets path]
  (let [tuples (map list path (rest path))
        coords (map #(list (targets (first %)) (targets (second %))) tuples)
        segments (map #(get dists %) coords)
        ]
    ;; (println dists)
    ;; (println targets)
    ;; (println path)
    ;; (println coords)
    ;; (println segments)
    (reduce + 0 segments)
    )
  )

(defn solve1 []
  (let [graph (parse-data "resources/2016/day24.txt")
        targets (find-targets graph)
        dists (shortest-paths graph)
        possible-paths (filter #(= \0 (first %)) (permutations (keys targets)))
        all-lengths (map #(path-length dists targets %) possible-paths)
        ]

    (println all-lengths)
    (println (apply min all-lengths))
  ))


(defn solve2 []
  (let [graph (parse-data "resources/2016/day24.txt")]
    (println "start")
    (base-dists graph)
    (println "done")
    ))

;; (defn solve2 [] (println "not yet"))
