;; MATE STANDARD LIB - SEQUENCE OPERATIONS
(in-ns 'msl)


(defn ranges-distinct [& ns]
  (reverse (second (reduce (fn [[i rs] n] [(+ i n) (cons (range i (+ n i)) rs)])
			   [0 []]
			   ns))))
(defn index 
  ([coll e] (index coll e =))
  ([coll e pred] (loop [[c & cs] coll
			i 0]
		   (if c
		     (if (pred c e)
		       i
		       (recur cs (inc i)))))))
(defn ortolize [seqs]
  (apply map vector seqs))
(defn transpose [seqs]
  (apply map vector seqs))

(defn last-nth [n coll]
  (take-last n coll))
(defn drop-nth [n coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (concat (take (dec n) s) (drop-nth n (drop n s)))))) 
(defn drop-ith [i coll]
  (concat (take i coll) (drop (inc i) coll)))

(defn concat-distinct [& seqs]
  (distinct (apply concat seqs)))

(defn member? 
  ([coll e] (member? coll e identical?))
  ([coll e pred] (some (fn [c] (pred c e)) coll)))

;; hash
(defn keys->vals [h keys]
  (map get (repeat h) keys))
(defn key-vals->has [keys vals]
  (apply hash-map (flatten (map (fn [k v] [k v]) keys vals))))
(defn keys-vals->hash [keys vals]
  (apply hash-map (flatten (map (fn [k v] [k v]) keys vals))))


;; substitue elements in coll to e if predicate holds
(defn substitute [coll e pred]
  (for [c coll]
    (if (pred c)
      e
      c)))
;; element dependent substitue
(defn substitute-edep [coll e pred]
  (for [c coll]
    (if (pred c e)
      e
      c)))     
;; substitue if predicate-holds in coll0 to coll1
(defn substitute-coll [coll0 coll1 pred]
  (loop [[c1 & acs1] coll1
	 ncoll0 coll0]			;the replaced coll0
    (if c1
      (recur acs1 (substitute ncoll0 c1 pred))
      ncoll0)))


	    
(defn find-first [pred coll]
  (loop [c coll]
    (if (seq c)
      (if (pred (first c))
	(first c)
	(recur (rest c))))))
(defn pick [pred coll]			;= find-first
  (loop [c coll]
    (if (seq c)
      (if (pred (first c))
	(first c)
	(recur (rest c))))))
(defn remove-first [pred coll]
  (loop [bc []
	 ac coll]
    (if (seq ac)
      (if (pred (first ac))
	(concat bc (rest ac))
	(recur (conj bc (first ac)) (rest ac)))
      coll)))
 
(defn remove-element 
  ([coll e pred] (remove (fn [i] (pred e i)) coll))
  ([ coll e] (remove-element coll e identical?)))
(defn remove-elements
  ([coll es pred] (remove (fn [i] (some (fn [j] (pred i j)) es)) coll))
  ([coll es] (remove-elements coll es identical?)))

     
     


(defn pos [coll element]
  (loop [cs coll 
	 i 0]
    (if (empty? cs)
      nil
      (if (= (first cs) element)
	i
	(recur (rest cs) (+ i 1))))))

(defn tupelize 
  ([coll] (tupelize coll 2))
  ([coll n] (reverse (loop [c coll r (list)]
		       (if (empty? c)
			 r
			 (recur (nthnext c n) (conj r (take n c))))))))

(defn take-indexes [coll indexes]
  (map (fn [i] (nth coll i)) indexes))


(defn replace-index [coll index replace]
  (concat (take index coll) [replace] (drop (inc index) coll)))
(defn replace-indexes [coll indexes replaces]
  (reduce (fn [c i r] (replace-index c i r)) coll indexes replaces))
(defn replace-nth [coll n e]
  (concat (take n coll) [e] (drop (inc n) coll)))
(defn replace-nths [coll ns es] 
  (reduce (fn [c n r] (replace-nth c n r)) coll ns es))

(defn replace-first [coll pred e]
  (loop [[c & cs] coll
	 ret []]
    (if (pred c)
      (concat (conj ret e) cs)
      (recur cs (conj ret c)))))
(defn replace-all [coll pred e]
  (map #(if (pred %) e %) coll))

;; groupie - yippie :D
(defn group [pred coll]
  (loop [[c & cs] coll
	 gs []]
    ;; going through coll
    (if c      
      (recur cs
	     ;; inserting subgroup
	     (loop [[g & gs] gs
		    rs []]
	       (if g
		 (if (pred c (first g))
		   (doall (concat rs [(conj g c)] gs))
		   (recur gs (conj rs g)))
		 (conj rs [c]))) )
      
      ;; returning the groups
      gs)))
		      
      
(defn sort-groups 
  ([comp gs] (sort (fn [[g0 & _] [g1 & _]] (comp g0 g1))
		   (map (fn [g] (sort comp g)) 
			gs)))
  ([gs] (sort-groups < gs)))
      
;; 
(defn lref [seq & refs]
  (loop [s seq
	 rs refs] 
    (if (empty? rs)
      s
      (recur (nth s (first rs)) (rest rs)))))
      
(defn seq-translate [item seq0 seq1]
  (loop [s0 seq0
	 s1 seq1]
    (if (= (first s0) item)
      (first s1)
      (recur (rest s0) (rest s1)))))


;; (defn list-to-vec [list]
;;   (if (list? list)
;;     (

;; (defn deep-apply-on-col [f coll]
;;   (if (coll? coll)
;;     (doseq [c coll] (deep-apply-on-col f c))
;;     (apply f coll)))


;; ugly as hell -> 1d vector for tensors 
;; definetely should be a java class :)
;; (ns tensor 
;;   (:refer-clojure))
;; (defn init [dims]
;;   {:dims dims
;;    :vec (vector (range )})
;; (defn aref [tensor & indexes]
;;   (nth (:vec tensor) (reduce + (map * 
