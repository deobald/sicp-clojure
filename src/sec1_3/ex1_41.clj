(ns sec1-3.ex1-41)

(defn dub [proc]
  #(proc (proc %)))

(defn incr [num]
  (+ num 1))

(((dub (dub dub)) incr) 5)
; => 21
