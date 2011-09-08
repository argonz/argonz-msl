
(ns spc.work
  (:refer-clojure)
  (:require clojure.contrib.string))


;; REGISTRY - of spacs
(def *spc-def-reg* (agent {}))
(def *spc-imp-reg* (agent {}))


;; ;; LOAD
;; ;; obtaining the sources
;; (defn src->parseds [f]
;;   (load-string (str "(quote ("
;; 		    (.replaceAll (re-matcher #"\|" (slurp f)) " | ")
;; 		    "))")))
;; (defn srcs->parseds [fs]
;;   (apply concat (map src->parseds fs)))


;; init primitive cells
(defn init-sym-def-cell [sym code]
  {:sym sym
   :val? nil
   :code code})
(defn init-sym-val-cell [sym val]
  {:sym sym
   :val? true
   :val val})
(defn init-exp-def-cell [sym bnd code]
  {:sym sym
   :bnd bnd			       	;if there is binding that's means this is an expressing
   :val? nil
   :code code})
(defn init-exp-val-cell [sym bnd val]
  {:sym sym
   :bnd bnd
   :val? true
   :val val})

;; PARSING
;; decide the body is expression or not
(defn bdy-expression? [bdy]
  (if (coll? bdy)
    (symbol? (first (flatten bdy)))
    (symbol? bdy)))
	
;; decide the def part is a function or just symbol
(defn def-expr? [def]
  (if (seq? def)
    (< 1 (count def))))
(defn def-noparam-expr? [def]
  (if (seq? def)
    (= (count def) 1)))
(defn def-sym? [def]
  (symbol? def))
(defn def-multi-sym? [def]
  (vector? def))



;; DEF-PAIR
(defn def-pair->init-cell [def bdy]
  ;; (println def (seq? def))
  ;;   (cond (def-sym? def) (println "defsym")
  ;; 	  (def-expr? def)  (println "defexpr")
  ;; 	  (def-noparam-expr? def) (println "defnoparam")
  ;; 	  true (println ":NNYYYII"))
	
  (cond (def-sym? def) (if (bdy-expression? bdy)
			 (init-sym-def-cell def bdy)
			 (init-sym-val-cell def bdy))
	(def-expr? def) (let [[sym & bnd] def]
			  (if (bdy-expression? bdy)
			    (init-exp-def-cell sym (apply vector bnd) bdy)
			    (init-exp-val-cell sym (apply vector bnd) bdy)))
	(def-noparam-expr? def) (let [sym (first def)]
				  (init-exp-def-cell sym [] bdy))))

;; DEPENDENT SYMBOLS
;; parsing bindings - what are the symbols which are not required
;; predefs

;; dealing with bindings
(defn binding-symbols [b]
  (if (symbol? b)
    ;; if symbol
    (if (or (keyword? b) (= "&" (name b)))
      nil
      [b])

    ;; if collection
    (if (empty? b)
      nil
      (distinct (apply concat (map binding-symbols b))))))

	
;; parsing a definition cases
(defn non-local-symbols [c locals])
;; sym
(defn symbol-non-local-symbols [c locals]
  (if (some #(= % c) locals)		;should check (name c)  instead??? :O
    nil
    [c]))
(defn quote? [c]
  (if (coll? c)
    (if (not-empty c)
      (= "quote" (name (first c))))))
(defn empty-coll? [c]
  (if (coll? c)
    (empty? c)))

;; (fn [a b & c] ...)
(defn fn-binding-struct? [c]
  (let [[c0 & cs] c] 
    (or (= (name c0) "fn")
	(= (name c0) "fn*"))))
(defn fn-binding-non-local-symbols [c locals]
  (let [[c0 b & cs] c
	nlocals (concat locals (binding-symbols b))]
    (distinct (apply concat (map non-local-symbols
				 cs
				 (repeat nlocals))))))
;; vector [a b (+ 11 1)]
(defn coll-non-local-symbols [c locals]
  (distinct (apply concat (map non-local-symbols
			       c
			       (repeat locals)))))
  
;; (let [[a b] (list 1 2) ..] ..)
(defn let-loop-binding-struct? [c]
  (let [[c0 & cs] c] 
    (or (= (name c0) "let")
	(= (name c0) "loop"))))
(defn let-loop-binding-non-local-symbols [c locals]
  (let [[c0 b & cs] c
	bss (take-nth 2 b)
	bcs (take-nth 2 (rest b))
	[new-ls non-ls] (reduce (fn [[new-ls non-ls] [b c]]
				  [(distinct (concat new-ls (binding-symbols b)))
				   (distinct (concat non-ls (non-local-symbols c new-ls)))])
				[locals []]
				(map vector bss bcs))]

    ;; adding up the from the bindings and from the code
    (distinct (concat non-ls
		      (apply concat (map non-local-symbols
					 cs
					 (repeat new-ls)))))))

  
(defn non-local-symbols [c locals]	;locals - local symbols
  ;; (println "NONL" c locals)
  ;; (cond (nil? c) (println "nil?") 
  ;; 	(keyword? c) (println "keword?")
  ;; 	(symbol? c) (println "symbol?")
  ;; 	(vector? c) (println "vector?")
  ;; 	(empty-coll? c) (println "emp-coll?")
  ;; 	(quote? c) (println "qoutoe?")
  ;; 	(map? c) (println "map?") 
  ;; 	(seq? c)  (cond (fn-binding-struct? c)    (println "fn-bin?")
  ;; 			 (let-loop-binding-struct? c)     (println "let-bin?")
  ;; 			 true (println "expr"))
  ;; 	true (println "FFFFAIILL"))

  (cond (nil? c) nil
	(keyword? c) nil	
	(symbol? c) (symbol-non-local-symbols c locals)
	(vector? c) (coll-non-local-symbols c locals)
	(empty-coll? c) nil
	(quote? c) nil
	(map? c) (coll-non-local-symbols (vals c) locals)
	(seq? c) (cond (fn-binding-struct? c) (fn-binding-non-local-symbols c locals)
			(let-loop-binding-struct? c) (let-loop-binding-non-local-symbols c locals)
			true (coll-non-local-symbols c locals))))


;; add non-local symbols to cell
(defn cell->add-nonloc-syms [c]
  (if (not (:val? c))
    (if (:bnd c)
      (assoc c :nonloc-syms (non-local-symbols (:code c)
					       []))
      (assoc c :nonloc-syms (non-local-symbols (:code c)
					       (binding-symbols (:bnd c)))))
    c))
(defn def-pair->cell [def bdy]
  (cell->add-nonloc-syms (def-pair->init-cell def bdy)))
(defn def-pair->add-to-ch [def bdy ch]
  (let [c (def-pair->cell def bdy)]

    ;; compile feedback
    (if (some #(= % (:sym c)) (keys ch))
      (println "WARNING: "  (:sym c) " double defined"))

    ;; merging the values
    (assoc ch (:sym c) c)))

(defn def-pairs->ch [defs]
  (loop [[def bdy & defs] defs
	 ch {}]
    (if def
      (recur defs (def-pair->add-to-ch def bdy ch))
      ch)))


;; FUNCTION CODE for spc fields
(defn sym->bind-pair [sym ch]
  (let [c (gensym)
	f (gensym)]

    ;; checking self
    (if (= sym 'self)

      ;; self symbol is exception
      `(~sym ~ch)
    
      ;; if not the self is the symbol
      `([~ch ~sym] (let [~c (get ~ch '~sym)]

		     ;; if value present
		     (if (get ~c :val?)
		       [~ch (get ~c :val)]

		       ;; if function to create value / transform ch present
		       (if (get ~c :fnc)
			 ((get ~c :fnc) ~ch)

			 ;; if no function there is only code to eval - the case when it's masked
			 (let [~f (spc.work/cell->fnc ~c ~ch)]
			   (~f (assoc ~ch '~sym (assoc ~c :fnc ~f)))))))))))
(defn syms->bind-pairs [syms chsym] 
  (apply vector (reduce concat (map sym->bind-pair syms (repeat chsym)))))

;; full let return clause
(defn syms-bdy->sym-req-bdy [sym syms bdy chsym]
  (let [v (gensym)
	c (gensym)]

    ;; non local sym bindings 
    `(let ~(syms->bind-pairs syms chsym)

       ;;values 
       (let [~v ~bdy
	     ~c (assoc (get ~chsym '~sym)
		  :val? true
		  :val ~v)]

	 ;; return
	 [(assoc ~chsym '~sym ~c) ~v]))))

;; (clojure.core/let [[ch b] (clojure.core/let [G__5761 (clojure.core/get ch (quote b))]
;; 					    (if (clojure.core/get G__5761 :val?)
;; 					      [ch (clojure.core/get G__5761 :val)]
;; 					      (if (clojure.core/get G__5761 :fnc)
;; 						((clojure.core/get G__5761 :fnc) ch)
;; 						(clojure.core/let [G__5762 (spc.work/cell->fnc G__5761 ch)]
;; 								  (G__5762 (clojure.core/assoc ch (quote b)
;; 											       (clojure.core/assoc G__5761 :fnc G__5762)))))))
;; 		   [ch c] (clojure.core/let [G__5763 (clojure.core/get ch (quote c))]
;; 					    (if (clojure.core/get G__5763 :val?)
;; 					      [ch (clojure.core/get G__5763 :val)]
;; 					      (if (clojure.core/get G__5763 :fnc)
;; 						((clojure.core/get G__5763 :fnc) ch)
;; 						(clojure.core/let [G__5764 (spc.work/cell->fnc G__5763 ch)] (G__5764 (clojure.core/assoc ch (quote c) (clojure.core/assoc G__5763 :fnc G__5764)))))))]
;; 		  (clojure.core/let [G__5759 (+ 1 (f0 2 b) c)
;; 				     G__5760 (clojure.core/assoc (clojure.core/get ch (quote a)) :val? true :val G__5759)]
;; 				    [(clojure.core/assoc ch (quote a) G__5760) G__5759]))

;; function or sym request
(defn syms-bdy->sym-fnc-bdy [sym syms bdy]
  (let [ch (gensym)]
    `(fn [~ch]
       ~(syms-bdy->sym-req-bdy sym
			       syms
			       bdy
			       ch))))
(defn syms-bdy->exp-fnc-bdy [sym syms bnd bdy]
  (let [ch (gensym)]
    `(fn [~ch]
       ~(syms-bdy->sym-req-bdy sym
			       syms
			       `(fn ~bnd ~bdy)
			       ch))))

;; ON THE SPOT COMPILE
(defn cell->covered-nonlocal-syms [c ch]
  (let [ss (:nonloc-syms c)]
    (filter (fn [s] (some #(= s %) ss)) (conj (keys ch) 'self))))
(defn cell->fnc [c ch]
  (if (:bnd c)
    (eval (syms-bdy->exp-fnc-bdy (:sym c) (cell->covered-nonlocal-syms c ch) (:bnd c) (:code c)))
    (eval (syms-bdy->sym-fnc-bdy (:sym c) (cell->covered-nonlocal-syms c ch) (:code c)))))


;; adding the fnc to the cells
(defn cell->add-fnc [c ch]
  (assoc c :fnc (cell->fnc c ch)))
(defn cellh->add-fncs [ch]
  (reduce (fn [ch c] (assoc ch (:sym c) (cell->add-fnc c ch)))
	  ch
	  (vals ch)))


;; RUNTIME SPAC 
;; registering a spac
(defn reg-spc-def [nam ans defs]
  (do (await *spc-def-reg*)
      (send *spc-def-reg* assoc nam {:nam nam
				     :ans ans
				     :cells (def-pairs->ch defs)})
      (await *spc-imp-reg*)
      (send *spc-imp-reg* assoc nam nil)))
(defn reg-imp-def [nam imp]
  (do ;; (await *spc-imp-reg*)
      (send *spc-imp-reg* assoc nam imp)))

;; cell hash from the definitions
(defn name->defreg-cellh [n]
  (loop [ns [n]
	 vis []
	 ch {}]
    
    (if (not-empty ns)
      (let [[n & ns] ns
	    r (get @*spc-def-reg* n)]

	(if (not-any? #(= n %) vis)
	  (recur (concat ns (:ans r))
		 (conj vis n)
		 (merge (:cells r) ch))
	  (recur ns
		 vis
		 ch)))
      ch)))

;; cell hash add the fncs etc..
(defn name->cellh [n]
  (let [imp (get @*spc-imp-reg* n)]
    (if imp
      imp
      (let [nimp (cellh->add-fncs (name->defreg-cellh n))]
	(do (reg-imp-def n nimp)
	    nimp)))))

;; asking cellh with defs masking
(defn name-defs->cellh [n defs]
  (merge (name->cellh n) 
	 (def-pairs->ch defs)))		;no compiled fncs -> on time they will compile it..



;; MASKING RELATED 
;; backward dependency infer -> what's there should be treated
(defn cellh->affected-syms [ch syms]
  (loop [[s & ss] syms
	 ret []]
    (if s
      (if (some #(= s %) ret)
	(recur ss ret)			;PROBLEM AT NONLOC SYMBOL... WTF :D
	(recur (concat ss (:nonloc-syms (get ch s))) (conj ret s)))
      ret)))
(defn cellh->deval-affected-cells [ch syms]
  (reduce (fn [ch s]
	    (assoc ch s (assoc (get ch s)
			  :val? nil
			  :fnc nil)))
	  ch
	  syms)) 
(defn cellh->mask-deval-cellh [ch syms]
  (cellh->deval-affected-cells ch (cellh->affected-syms ch syms)))			       
(defn def-pairs->mask-def-syms [defs]
  (map #(if (coll? %)
	  (first %)
	  %)
       (take-nth 2 defs)))
;; the problem with masking -> you should recompute all the values :O - not so bad..
;; oh baaad so baaad :D



;; INTERFACE 
(ns spc
  (:refer-clojure))

;; MACRO for registering spacs
(defmacro _defsp [nam ans & defs]
  (spc.work/reg-spc-def nam ans defs))
(defmacro _defch [nam ans & defs]
  (spc.work/reg-spc-def nam ans defs))

;; creating an spc
(defn _is [name & defs]
  (spc.work/name-defs->cellh name defs))

;; masking
(defn _ms [cellh & defs]
  (merge (spc.work/cellh->mask-deval-cellh cellh (spc.work/def-pairs->mask-def-syms defs))
	 (spc.work/def-pairs->ch defs)))
	
;; joining/merging spcs
(defn _js [spc & spcs]
  )

;; ask for a field
;; here should be
;; 1. vector (field = [f f f])
;; 2. cellh = symbol  than using static functions
(defn _fs->eval-cell->cellh-value [cellh c field]
    (if (:val? c)
      [cellh (:val c)]
		 
      ;; fnc 
      (if (:fnc c)
	((:fnc c) cellh)
				      
	;; compile
	(let [f (spc.work/cell->fnc c cellh)]
	  (f (assoc cellh field (assoc c :fnc f)))))))


;; REQUESTING FIELD!!!
(defn _fs [cellh field & args]

  ;; check cellh is symbolic??
  (let [cellh (if (symbol? cellh)            ;check 
		(spc.work/name->cellh cellh) ;crate 
		cellh)]
    ;; the cell
    (let [c (get cellh field)]

      ;; symbol or vector of symbols
      (if (symbol? field)

	;; SYMBOL
	(let [[ch v] (_fs->eval-cell->cellh-value cellh c field)]
	  ;; (println field v (:bnd c) args)
	  ;; func or value
	  (if (:bnd c)
	    [ch (apply v args)]
	    [ch v]))

	;; VECTOR of VALUES
	(reduce (fn [[ch vs] f]
		  (let [[ch v] (apply _fs ch f args)]
		    [ch (conj vs v)])) 
		[cellh []]
		field)))))
	

;; just compute a field
(defn _cs [cellh field & args]
  (let [[_ v] (apply _fs cellh field args)]
    v))



;; Another interesting thing would be if these objects would be agents -> so I just send them what objects I want to have, and they modify their states according to that.. it's possible although have to say it's what.. we will see :)

 
;; ;; probe spaces
;; (_defch s0 []
;;   a 1
;;   b 2
;;   c (+ a b)
;;   (j [p0 p1 p2]) [(f0 p0 p1) a [a p2]]
;;   z [a b (f0 1 1)]
;;   (init a b) (_ms self 'a a 'b b)
;;   (f0 a b) (+ a b c)
;;   (f1 a b) (+ (f0 a b) c)

;;   (rrr) (* (rand) 5)
;;   (rir) (doall (map (fn [_] (rrr)) (range 5)))
;;   (rar n) (doall (map (fn [_] (rrr)) (range n)))
;;   )

;; (_defch s1 [s0]
;;   a 5
;;   ;; c would be okay again??
;;   (f0 a) (+ a b c)
;;   (f1 a b) (+ (f0 a) b))








