(in-ns 'msl)

;; implication  false 1 0
(defn imp 
  ([] true)
  ([x] x)
  ([x & next] (if (and x (not (first next)))
		false 
		(apply imp (cons true (rest (rest next)))))))
			     

;; xor          false 1 1, 0 0
(defn xor
  ([] true)
  ([x] x)
  ([x & next] (if (or (and x (first next))
		      (and (not x) (not (first next))))
		false
		(apply xor (cons true (rest (rest next)))))))


;; eqvivalence   false: 1 0, 0 1
(defn eqv
  ([] true)
  ([x] x)
  ([x & next] (if (or (and x (not (first next)))
		      (and (not x) (first next)))
		false
		(apply eqv (cons true (rest (rest next)))))))
		