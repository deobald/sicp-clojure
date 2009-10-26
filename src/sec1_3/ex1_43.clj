(ns sec1-3.ex1-43)

(defn square [num]
  (* num num))

(defn compose [fn1 fn2]
  #(fn1 (fn2 %)))

(defn repeated [proc times]
  (if (= times 2)
    (compose proc proc)
    (compose proc (repeated proc (- times 1)))))

((repeated square 2) 5)
; => 625
