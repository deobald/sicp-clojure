(ns sec1-3.ex1-42)

(defn incr [num]
  (+ num 1))

(defn square [num]
  (* num num))

(defn compose [fn1 fn2]
  #(fn1 (fn2 %)))

((compose square incr) 6)

