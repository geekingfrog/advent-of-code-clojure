(ns advent-of-code.2016.23
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def grammar "
  <instruction> = copy | inc | dec | jump | toggle
  copy = <'cpy '> (register | num) <' ' > register
  inc = <'inc '> register
  dec = <'dec '> register
  jump = <'jnz '> (register | num) <' '> (register | num)
  toggle = <'tgl '> (register | num)
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
    (vec)
    )
  )

(defn exec-copy [env [v reg]]
  ;; reg may not be a register because of a toggle
  (cond (keyword? reg)
        (let [regval (if (keyword? v) (v env) v)]
          (assoc env reg regval)
          )
        )
  )


(defn exec-inc [env reg]
  (assoc env reg (inc (reg env))))

(defn exec-dec [env reg]
  (assoc env reg (dec (reg env))))

(defn exec-jump [env [reg v]]
  (let [{c :counter regs :regs} env
        regval (if (keyword? reg) (reg regs) reg)
        valval (if (keyword? v) (v regs) v)
        ]
    (if (not= 0 regval)
      (+ valval c)
      (inc c)
      )
    ))


(defn toggle-instruction [[ins-type & ins-args]]
  (if (= 1 (count ins-args))
    (if (= :inc ins-type)
      (apply vector :dec ins-args)
      (apply vector :inc ins-args)
    )
    (if (= :jump ins-type)
      (apply vector :copy ins-args)
      (apply vector :jump ins-args)
      )
    )
  )

(defn exec-toggle [is env reg]
  (let [{c :counter regs :regs} env
        regval (if (keyword? reg) (regs reg) reg)
        target-idx (+ c regval)
        ]
    (if (or (< target-idx 0) (>= target-idx (count is)))
      ;; toggle outside of program -> no-op
      is
      (assoc is target-idx (toggle-instruction (nth is target-idx)))
      )
    )
  )


(defn exec-add [regs [x y]]
  ;; x and y must be registers here
  (let [xval (regs x)
        yval (regs y)
        ]
    (assoc regs x (+ xval yval))
    )
  )

(defn exec-mult [regs [x y]]
  ;; x and y must be registers here
  (let [xval (regs x)
        yval (regs y)
        ]
    (assoc regs x (* xval yval))
    )
  )

(defn optimize-add? [[_ i0 i1 i2]]
  (let [t0 (first i0)
        t1 (first i1)
        t2 (first i2)
        ]
    (and
      (= :jump t2) ;; jump
      (= (last i2) -2) ;; jump 2 steps back
      (keyword? (first i2))
      (or (= :inc t0) (= :dec t0))
      (or (= :inc t1) (= :dec t1))
      )
    )
  )

(defn optimize-mult? [[_ i0 i1 i2 i3 i4 i5]]
  (let
    [t0 (first i0)
     t1 (first i1)
     t2 (first i2)
     t3 (first i3)
     t4 (first i4)
     t5 (first i5)
     ]
    ;; very restrictif test
    (and
      (= :copy t0)
      (= :add t1)
      (= :copy t2)
      (= :nop t3)
      (= :dec t4)
      (= :jump t5)
      (= -5 (nth i5 2))
      )
    )
  )


(defn transform-add [is [idx [_ r0] [_ r1] [_ rj _]]]
  (let
    [target-reg (if (= r0 rj) r1 r0)
     other-reg (if (= target-reg r0) r1 r0)
     transformed [
                  [:add target-reg other-reg]
                  [:copy 0 other-reg]
                  [:nop]
                  ]
     ]
    (vec (concat (subvec is 0 idx) transformed (subvec is (+ 3 idx))))
  ))


;; dirty optim for this special case
(defn transform-mult
  [is
   [idx
    [_ source-reg tmp-reg] ;; copy source tmp
    [_ target-reg _] ;; add final tmp
    [_ _ _] ;; copy 0 tmp
    [_] ;; nop
    [_ flag] ;; dec flag
    [_ _ _] ;; jnz flag -5
    ]
   ]
  (let [transformed [[:copy source-reg tmp-reg]
                     [:mult tmp-reg flag]
                     [:add target-reg tmp-reg]
                     [:copy 0 flag]
                     [:copy 0 tmp-reg]
                     [:nop]
                     ]]
    (vec (concat (subvec is 0 idx) transformed (subvec is (+ 6 idx))))
    )
  )

(defn optimize-add [instructions]
  (let
    [tmp (map list (range) instructions (rest instructions) (drop 2 instructions))
     add-targets (vec (filter optimize-add? tmp))
     ]
    (vec (reduce transform-add instructions add-targets))
    )
  )

(defn optimize-mult [instructions]
  (let
    [tmp (map list
              (range)
              instructions
              (rest instructions)
              (drop 2 instructions)
              (drop 3 instructions)
              (drop 4 instructions)
              (drop 5 instructions)
              )
     mult-targets (vec (filter optimize-mult? tmp))
     ]
    (vec (reduce transform-mult instructions mult-targets))
    )
  )

(defn optimize [instructions]
  ;; only transform inc x; dec y; jnz y -2; into add x y; copy 0 y; nop
  (->>
    instructions
    (optimize-add)
    (optimize-mult)
    )
  )

  ;; (let
  ;;   [tmp (map list (range) instructions (rest instructions) (drop 2 instructions))
  ;;    add-targets (vec (filter optimize-add? tmp))
  ;;    ]
  ;;   (vec (reduce transform-add instructions add-targets))
  ;;   )
  ;; )


(defn execute [instructions initial-env]
  (loop [is instructions optimized (optimize instructions) env initial-env]
    (let
      [
       counter (:counter env)
       regs (:regs env)
       [ins-type & args] (nth optimized counter)
       regs' (case ins-type
               :copy (exec-copy regs args)
               :inc (exec-inc regs (first args))
               :dec (exec-dec regs (first args))
               :jump regs
               :toggle regs
               :add (exec-add regs args)
               :mult (exec-mult regs args)
               :nop regs
               (do
                 (prn (nth is counter))
                 (println env)
                 (println is)
                 (throw (Exception. "invalid instruction"))
                 )
               )
       counter' (cond
                  (= :jump ins-type) (exec-jump env args)
                  :else (inc counter)
                  )
       env' (assoc env :regs regs' :counter counter')
       is' (if (= :toggle ins-type)
             (exec-toggle is env (first args))
             is
             )

       ;; changing the bytecode may invalidate previous optims
       optimized' (if (= :toggle ins-type)
                    (optimize is')
                    optimized
                    )
       ]
      (if (>= counter' (count is))
        env'
        (recur is' optimized' env')
        )
      )
    )
  )

(defn initial-env [a]
  {:regs {:a a :b 0 :c 0 :d 0}
   :counter (int 0)
   })


(defn solve1 []
  (let [instructions (parse-input "resources/2016/day23test2.txt")
        final-env (execute instructions (initial-env 7))
        ]
    (println final-env)
    )
  )

(defn solve2 []
  (let [instructions (parse-input "resources/2016/day23.txt")]
    (println (execute instructions (initial-env 12)))
    )
  )
