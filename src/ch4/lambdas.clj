
(ns ch4.lambdas
  ;(:require )
  ;(:use )
  ;(:import )
  )


(defn lambda-parameters [exp] (second exp))

(defn lambda-body [exp] (rest (rest exp)))

(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))

