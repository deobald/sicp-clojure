
(ns ch4.scheme-helpers)

(defn car [x] (first x))
(defn cdr [x] (next x))
(defn cadr [x] (second x))
(defn caddr [x] (first (next (next x))))
(defn cdddr [x] (next (next (next x))))
(defn caddr [x] (first (next (next x))))
(defn cadddr [x] (first (next (next (next x)))))
(defn null? [x] (nil? x))


