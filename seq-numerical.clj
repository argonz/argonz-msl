(ns msl.seq-numerical
  (:refer-clojure)
  (:refer msl)
  (:use clojure.contrib.math))

;; ezeket igazabol macroval meg lehetne csinalni :)
;; sequence operators
(defn seq-*-n [seq n]
  (map #(* % n) seq))
(defn seq-div-n [seq n]
  (if (zero? n) (println "ZERO" seq))
  (if (zero? n)
    seq
    (map #(/ % n) seq)))
(defn seq-+-n [seq n]
  (map #(+ % n) seq))
(defn seq-sub-n [seq n]
  (map #(- % n) seq))
(defn seq-pow-n [seq n]
  (map #(Math/pow % n) seq))

(defn seq-+-seq [& seqs]
  (apply map + seqs))
(defn seq-sub-seq [& seqs]
  (apply map - seqs))
(defn seq-*-seq [& seqs]
  (apply map * seqs))
(defn seq-div-seq [& seqs]
  (apply map / seqs))
;; (defn seq-pow-seq [& seqs]
;;   (apply (fn [& ss] (reduce Math/pow (first ss) (rest ss))) seqs))
(defn seq-pow-seq [seq0 seq1]
  (map #(Math/pow %1 %2) seq0 seq1))

(defn seq-abs [seq]
  (map abs seq))
(defn summa [seq]
  (apply + seq))
(defn sum [seq]
  (apply + seq))



(defn avg [seq]
  (/ (apply + seq) (count seq)))
(defn mean [seq]
  (/ (apply + seq) (count seq)))

(defn variance [seq]
  (avg (map (fn [s] (Math/abs s)) (seq-sub-n seq (avg seq)))))
(defn deviation [seq]
  (avg (map (fn [s] (Math/abs s)) (seq-sub-n seq (avg seq)))))
;; (defn standard-deviation [seq]
;;   (map (Math
;;   (avg (map (fn [s] (Math/abs s)) (seq-sub-n seq (avg seq)))))



(defn exp-var [seq]
  (Math/pow (sum (seq-pow-n (seq-sub-n seq (avg seq)) 2))
	    (/ 1 2)))
(defn mss [seq]				;mean square sum :D
  (/ (apply + (map (fn [s] (Math/pow s 2)) seq))
     (count seq)))
(defn mse [seq0 seq1]
  (/ (apply + (map (fn [s] (Math/pow s 2)) (seq-sub-seq seq0 seq1)))
     (count seq0)))

(defn seqs-sum [seqs]
  (apply map + seqs))
(defn seqs-avg-seq [seqs]
  (seq-div-n (seqs-sum seqs) (count seqs)))

;; distance
(defn seq-magnitude-n [seq n]
  (expt (apply + (map #(expt % n) (map abs seq)))
	(/ 1.0 n)))
(defn seq-distance-n [seq0 seq1 n]
  (seq-magnitude-n (map - seq0 seq1) n))

(defn seq-magnitude [seq]
  (Math/sqrt (apply + (map #(* % %) seq))))
(defn seq-normalize [seq]
  (seq-div-n seq (seq-magnitude seq)))
(defn seq-unitize [seq]
  (seq-div-n seq (seq-magnitude seq)))  

(defn seq-scalar-prod [seq0 seq1]
  (apply + (map * seq0 seq1)))


(defn seq-distance [seq0 seq1]
  (seq-magnitude (seq-sub-seq seq0 seq1)))
(defn seq-displacement [seq0 seq1]
  (seq-sub-seq seq1 seq0))
(defn seq-direction [seq0 seq1]
  (seq-unitize (seq-displacement seq0 seq1)))


;; kullbach leibler distance
(defn self-inf [p0 p1]
  (. Math log (/ p0 p1)))  
(defn seq-dkl [seq0 seq1]
  (apply + (map #(* %1 (self-inf %1 %2)) seq0 seq1)))
(defn bin-prob-dkl [p0 p1]
  (seq-dkl (list p0 (- 1 p0)) (list p1 (- 1 p1))))
(defn bin-prob-seq-dkl [seq0 seq1]
  (apply + (map abs (map #(bin-prob-dkl %1 %2) seq0 seq1))))
  

;; symmetric absolute dist
(defn seq-sdkl [seq0 seq1]
  (/ (+ (seq-dkl seq0 seq1) (seq-dkl seq1 seq0))
     2.0))
(defn bin-prob-sdkl [p0 p1]
  (seq-sdkl (list p0 (- 1 p0)) (list p1 (- 1 p1))))
(defn bin-prob-seq-sdkl [seq0 seq1]
  (apply + (map abs (map #(bin-prob-sdkl %1 %2) seq0 seq1))))
(defn bin-prob-seq-2norm-sdkl [seq0 seq1]
  (seq-distance (map #(bin-prob-sdkl %1 %2) seq1 seq0)
		(map #(bin-prob-sdkl %1 %2) seq0 seq1)
		2.0))



;; equations about lines
(defn points->equation-of-line [p0 p1]
  [(seq-direction p0 p1) p0])
(defn equations-of-line->common-point [[a0 b0] [a1 b1]]
  (seq-div-seq (seq-sub-seq b1 b0) (seq-sub-seq a0 a1)))