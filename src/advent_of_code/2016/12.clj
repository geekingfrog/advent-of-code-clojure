(ns advent-of-code.2016.12
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def grammar "
  <instruction> = copy | inc | dec | jump
  copy = <'cpy '> (register | num) <' ' > register
  inc = <'inc '> register
  dec = <'dec '> register
  jump = <'jnz '> (register | num) <' '> num
  num = #'[-0-9]+'
  register = 'a' | 'b' | 'c' | 'd'
  ")

(def line-parser (insta/parser grammar))

(defn parse-line [l]
  (let
    [transform-map
     {:num (fn [& digits] (read-string (apply str digits)))
      :register keyword
      }]
    (insta/transform transform-map (line-parser l))
    )
  )

(defn parse-input [path]
  (->>
    (slurp path)
    (str/split-lines)
    (map parse-line)
    (apply concat)
    )
  )

(defn exec-copy [env [v reg]]
  (let [regval (if (keyword? v) (v env) v)]
    (assoc env reg regval)
    )
  )


(defn exec-inc [env reg]
  (assoc env reg (inc (reg env))))

(defn exec-dec [env reg]
  (assoc env reg (dec (reg env))))

(defn exec-jump [env [reg v]]
  (let [{c :counter regs :regs} env
        regval (if (keyword? reg) (reg regs) reg)
        ]
    (if (not= 0 regval)
      (+ v c)
      (inc c)
      )
    ))

(defn execute [instructions initial-env]
  (loop [env initial-env]
    (let
      [
       counter (:counter env)
       regs (:regs env)
       [ins-type & args] (nth instructions counter)
       regs' (cond
               (= :copy ins-type) (exec-copy regs args)
               (= :inc ins-type) (exec-inc regs (first args))
               (= :dec ins-type) (exec-dec regs (first args))
               (= :jump ins-type) regs
               :else (do
                       (prn (nth instructions counter))
                       (throw (Exception. "invalid instruction"))
                       )
               )
       counter' (cond
                  (= :jump ins-type) (exec-jump env args)
                  :else (inc counter)
                  )
       env' (assoc env :regs regs' :counter counter')
       ]
      ;; (prn (:regs env'))
      (if (>= counter' (count instructions))
        env'
        (recur env')
        )
      )
    )
  )

(defn solve1 []
  (let [instructions (parse-input "resources/2016/day12.txt")
        initial-env
        {:regs {:a 0 :b 0 :c 0 :d 0}
         :counter (int 0)
         }
        final-env (execute instructions initial-env)
        ]
    (println final-env)
    (println (get-in final-env [:regs :a]))
    )
  )

(defn solve2 []
  (let [instructions (parse-input "resources/2016/day12.txt")
        initial-env
        {:regs {:a 0 :b 0 :c 1 :d 0}
         :counter (int 0)
         }
        final-env (execute instructions initial-env)
        ]
    (println final-env)
    (println (get-in final-env [:regs :a]))
    )
  )
