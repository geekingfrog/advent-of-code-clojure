(ns advent-of-code.2016.13)

(def input 1350)

(defn binary [n]
  (reverse
    (loop [i n acc []]
      (if (= 0 i)
        acc
        (let [d (int (/ i 2))
              r (mod i 2)]
          (recur d (conj acc r))
          )
        )
      )
    )
  )


(defn floor? [x y]
  (if (or (< x 0) (< y 0))
    false
    (let
      [a (+ (* x x) (* 3 x) (* 2 x y) y (* y y))
       b (+ a input)
       bin (binary b)
       ones (count (filter #(= 1 %) bin))
       ]
      (even? ones)
    ))
  )


(defn next-path [visited x y dir]
  (when
    (and
      (floor? x y)
      (not (contains? visited [x y]))
      )
    [x y dir]
    )
  )

(defn shortest-path [x0 y0 x1 y1]
  "path from (x0 y0) to (x1 y1)"
  (loop [paths [[x0 y0 ""]] visited #{}]
    (if (empty? paths)
      (do (println "no path to target") nil)
      (let
        [[x y path] (first paths)
         next-paths (filter #(not= nil %)
                            [(next-path visited (inc x) y (str path \R))
                             (next-path visited (dec x) y (str path \L))
                             (next-path visited x (inc y) (str path \D))
                             (next-path visited x (dec y) (str path \U))
                             ]
                            )
         ]
        (if (and (= x1 x) (= y1 y))
          path
          (recur (concat (rest paths) next-paths) (conj visited [x y]))
        )
      )
    )
  ))


(defn all-paths [x0 y0 limit]
  "path from (x0 y0) to (x1 y1)"
  (loop [paths [[x0 y0 ""]] visited #{}]
    (if (empty? paths)
      visited
      (let
        [[x y path] (first paths)
         next-paths (filter #(not= nil %)
                            [(next-path visited (inc x) y (str path \R))
                             (next-path visited (dec x) y (str path \L))
                             (next-path visited x (inc y) (str path \D))
                             (next-path visited x (dec y) (str path \U))
                             ]
                            )
         ]
        (if (> (.length path) limit)
          (recur (rest paths) visited)
          (recur
            (concat (rest paths) next-paths)
            (conj visited [x y]))
          )
      )
    )
  ))




(defn solve1 []
  (let [path (shortest-path 1 1 31 39)]
    (println path)
    (println (count path))
    )
  )

(defn solve2 []
  (let [locations (all-paths 1 1 50)]
    (println (count locations))
    )
  )
