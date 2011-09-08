
(ns msl.math-funcs
  (:refer-clojure)
  (:refer msl))

(defn logistic [x c]
  (/ 1.0 (+ 1.0 (Math/exp (* (- c) x)))))
(defn minus-exp [x c]
  (Math/exp (* (- c) x))) 