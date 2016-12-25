(ns advent-of-code.2016.22
  (:require [instaparse.core :as insta]))


(def grammar "
  <data> = <header> <'\n'> (fdline)*
  header = <'Filesystem              Size  Used  Avail  Use%'>
  fdline = sector <spaces> specs
  specs = size <spaces>
          used <spaces>
          avail <spaces>
          use <'\n'>
  sector = <'/dev/grid/node-x'> num <'-y'> num
  spaces = ' '*
  size = num <'T'>
  used = num <'T'>
  avail = num <'T'>
  use = num <'%'>
  num = #'[0-9]+'
  ")

(def parser (insta/parser grammar))

(defn parse [input]
  (let [transform-map
        {:num (fn [& digits] (read-string (apply str digits)))
         :spaces (fn [& args] nil)
         :sector list
         :specs (fn [& args] (reduce #(apply assoc %1 %2) {} args))
         :fdline (fn [& args] args)
         }
        intermediate (insta/transform transform-map (parser input))
        ]
    intermediate
    (reduce #(apply assoc %1 %2) {} intermediate)
    ))

(defn viable? [a b]
  (and
    (not= 0 (:used a))
    (<= (:used a) (:avail b))
    )
  )

(defn pairs [in]
  (let [ks (keys in)]
    (for [a ks
          b ks
          :when (not= a b)
          :when (viable? (get in a) (get in b))]
          (list a b)
      )
    )
  )

(defn solve1 []
  (->>
    (slurp "./resources/2016/day22.txt")
    (parse)
    (pairs)
    (count)
    (println)
    )
  )

;; 1995 too high

(defn solve2 [] (println "nope"))
