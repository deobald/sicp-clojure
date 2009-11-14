(ns sec2-1.ex2-2)

(defn make-point [x y]
  {:x x :y y})

(defn make-segment [start end]
  {:start start :end end})

(defn print-point [p]
  (print "(")
  (print (:x p))
  (print ", ")
  (print (:y p))
  (print ")"))

(defn print-segment [segment]
  (print newline)
  (print-point (:start segment))
  (print " - ")
  (print-point (:end segment)))

(defn midpoint-segment [segment]
  (let [midpoint-x (average (:x (:start segment)) (:x (:end segment)))
        midpoint-y (average (:y (:start segment)) (:y (:end segment)))]
  (make-point midpoint-x midpoint-y)))

(def steves-segment
  make-segment (make-point 2 2) (make-point 10 10))

(print-segment steves-segment)