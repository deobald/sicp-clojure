(ns sec2_2.ex2_23)

(defn for-each [fn items]
  (fn (first items))
  (if (empty? items)
    true
    (recur fn (rest items))))

(for-each #(println %) '(57 321 88))


