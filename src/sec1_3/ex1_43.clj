(ns sec1-3.ex1-43)

(defn square [num]
  (* num num))

(defn compose [f g]
  #(f (g %)))

(defn repeated-inside [original-f f times]
  (if (= times 1)
    f
    (recur original-f (comp original-f f) (dec times))))

(defn repeated [f times]
  (repeated-inside f f times))

((repeated square 2) 5)
; => 625
