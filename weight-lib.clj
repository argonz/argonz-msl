;; WEIGHT LIB
(ns msl.weight-lib
  (:refer-clojure)
  (:refer msl)
  (:refer msl.seq-numerical))
   
;; inverse 
;; (defn inv-weights [wseq]
;;   (seq-div-n wseq (sum wseq)))
;; (defn inv-weights-zero-check [wseq]
;;   (let [wsum (reduce + wseq)]
;;     (map (fn [w] (if (zero? w)
;; 		   1.0
;; 		   (/ wsum w)))
;; 	 wseq)))
;; should work on it!!!

;; average and other weight functions
(defn wavg [seq wseq]
  (/ (sum (seq-*-seq seq wseq)) (sum wseq)))
(defn wavg-seqs [seq-seq wseq]
  (seq-div-n (seqs-sum (map #'seq-*-n seq-seq wseq)) (sum wseq)))

;; merging weighted seqs - a wavg-ba siman mehetne - multimethodnak csinalni? :O
(defn merge-wseqs [[seq0 wseq0] [seq1 wseq1]]
  [(concat seq0 seq1) (concat wseq0 wseq1)])

;; sampling from a weighted list
(defn rand-wcoll [wcoll vcoll]
  (let [s (reduce + wcoll)
	r (rand s)]
    (loop [[w & ws] wcoll
	   [v & vs] vcoll
	   s 0]
      (if (< r (+ s w))
	v
	(recur ws vs (+ s w))))))

(defn wrand-element [wcoll vcoll]	
  (let [s (reduce + wcoll)
	r (rand s)]
    (loop [[w & ws] wcoll
	   [v & vs] vcoll
	   s 0]
      (if (< r (+ s w))
	v
	(recur ws vs (+ s w))))))
;; (defn wirand-element [wcoll vcoll]
;;   (wrand-element (inv-weights-zero-check wcoll) vcoll))

;; weighted random index of the weight
(defn wrand-index [wcoll]
  (let [sum (reduce + wcoll)
	r (rand sum)]
    (loop [[w & ws] wcoll
	   i 0
	   s 0]
      (if (< r (+ s w))
	i
	(recur ws (inc i) (+ s w))))))