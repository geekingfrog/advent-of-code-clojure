(ns advent-of-code.2016.24
  (:require [clojure.string :as str]))

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

(defn dist [[i j] [i' j']] (+ (Math/abs (- i' i)) (Math/abs (- j' j))))

(defn parse-data [path]
  (->>
    (slurp path)
    (str/split-lines)
    (map vec)
    (vec)
    ))


(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))


(defn nodes [g]
  ;; ignore the borders of the graphs
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


(defn reduce-target [g m [i j]]
  (let [a ((g j) i)]
    (if (and (not= \. a) (not= \# a))
      (assoc m a (vector i j))
      m
      )
    )
  )

(defn find-targets [g]
  (reduce (partial reduce-target g) {} (nodes g)))

(defn adjacent-nodes [g [i j] visited]
  (->>
    [[(dec i) j] [(inc i) j] [i (dec j)] [i (inc j)]]
    (filter #(not= \# ((g (second %)) (first %))))
    (filter #(not (contains? visited %)))
    ))

(defn should-add-to-openset? [open-set [n d]]
  (or
    (not (contains? open-set n))
    (< d (open-set n))
    ))


(defn pretty-dir [prev cur]
  (let [[i j] prev
        [i' j'] cur
        ]
    (cond
      (= (inc i) i') \→
      (= (dec i) i') \←
      (= (dec j) j') \↑
      (= (inc j) j') \↓
      )
    )
  )

(defn reconstruct-path [path target]
  (loop [current target prev (path target) acc '()]
    (if (nil? prev)
      acc
      (recur prev (path prev) (conj acc (pretty-dir prev current)))
    )
  ))


(defn shortest' [g source target]
  "A* algo"
  (loop
    [open-set {source 0}
     closed-set #{}
     came-from {}
     ]

    (when
      (empty? open-set)
      (throw (Exception. (str "No path from " source " to " target))))

    (let [[n nscore] (apply min-key second open-set)
          open-set' (dissoc open-set n)
          ;; _ (println "openset: " open-set)
          ;; _ (println "n " n)
          ;; _ (println "closedset: " closed-set)
          next-nodes (adjacent-nodes g n closed-set)
          ;; _ (println "next nodes: " next-nodes)
          next-scores (repeat (inc nscore))
          next-open-nodes (map list next-nodes next-scores)
          ;; _ (println "wtf ?" next-open-nodes)
          filtered-nodes (filter #(should-add-to-openset? open-set %) next-open-nodes)
          ;; _ (println filtered-nodes)
          open-set'' (reduce #(apply assoc %1 %2) open-set' filtered-nodes)
          came-from' (reduce #(assoc %1 (first %2) n) came-from filtered-nodes)
          ]
      (if (= n target)
        (reconstruct-path came-from' target)
        (recur
          open-set''
          (conj closed-set n)
          came-from'
          )
        )
      )
    )
  )

(def shortest (memoize shortest'))

(defn path-length [g targets stops]
  (->>
    (map list stops (rest stops))
    (map #(list (targets (first %)) (targets (second %))))
    (map #(apply shortest g %))
    (map count)
    (reduce + 0)
    (#(vector % stops))
    )
  )


(defn solve1 []
  (let [graph (parse-data "./resources/2016/day24.txt")
        targets (find-targets graph)
        possible-stops (map #(conj % \0) (permutations (keys (dissoc targets \0))))
        all-lengths (map #(path-length graph targets %) possible-stops)
        ]
    (println (apply min-key first all-lengths))
    )
  )

(defn solve2 []
  (let [graph (parse-data "./resources/2016/day24.txt")
        targets (find-targets graph)
        possible-stops (map #(conj % \0) (permutations (keys (dissoc targets \0))))
        stops-with-returns (map #(concat %1 '(\0)) possible-stops)
        all-lengths (map #(path-length graph targets %) stops-with-returns)
        ]
    (println (apply min-key first all-lengths))
    )
  )
