(ns msl.rand
  (:refer-clojure)
  (:refer msl)
  (:refer clojure.contrib.math)
  (:import java.util.Random))

(defn rand-seq [bounds]
  (map (fn [[min max]] (rand max)) bounds))
(defn rgen-coll [bounds]
  (map (fn [[min max]] (+ min (rand (- max min)))) bounds))


  
(defn rand-distinct-ints [n from till]
  (let [d (- (dec till)  from)]

    (loop [i n
	   ret []]
      
      (if (zero? i)
	ret
	(let [r (round (+ (* (rand) d) from))]
	  (if (some #(= % r) ret)
	    (recur i ret)
	    (recur (dec i) (conj ret r))))))))	  

(defn rand-coll [coll]
  (nth coll (int (rand (count coll)))))
(defn rand-element [coll]
  (nth coll (int (rand (count coll)))))
(defn rand-take [coll n]
  (take-indexes coll (rand-distinct-ints n 0 (count coll))))
(defn rand-drop 
  ([coll] (drop-ith (int (rand (count coll))) coll))
  ([coll n] (reduce (fn [c _] (rand-drop c)) coll (range n))))

  
     

;; sampling from a sequence with given probabilities
(defn rand-prob-coll [probs events]
  (let [r (rand)]
    (loop [[p & ps] probs
	   [e & es] events
	   s 0]
      (if (< r (+ s p))
	e
	(recur ps es (+ s p))))))

;; sampling from a sequence < 1.0  and default given
(defn rand-prob-coll-default [probs events default]
  (rand-prob-coll (conj probs (- 1.0 (reduce + probs))) 
		  (conj events default)))
     



     
     

(def jrand (new java.util.Random))
(defn rand-normal 
  ([] (. jrand nextGaussian))
  ([mean var] (+ (* (. jrand nextGaussian) var) mean)))
(defn prob-normal 
  ([x] (prob-normal x 0.0 1.0))
  ([x mean var2] (* (/ 1 (Math/sqrt (* 2 Math/PI var2)))
		    (Math/exp (- (/ (Math/pow (- x mean) 2)
				    (* 2 var2)))))))
     



