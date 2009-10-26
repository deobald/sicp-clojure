(ns sec1-3.ex1-41)

(defn incr [num]
  (+ num 1))

(defn dub [proc]
  #(proc (proc %)))

(((dub (dub dub)) incr) 5)
; => 21
