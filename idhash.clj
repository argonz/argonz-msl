;; element hash
(ns msl.idhash
  (:refer-clojure))

(defn init []
  {:hash (hash-map)
   :i -1})

(defn next-i [h]
  (let [nh (assoc h :i (+ (:i h) 1))]
    [(:i nh) nh]))
(defn add-e [e h]
  (let [[i nh] (next-i h)
	ne (assoc e :id i)]
    [ne (assoc nh :hash (assoc (:hash nh) i ne))]))
(defn get-e [id h]
  (id (:hash h)))

(defn last-e [h]
  ((:i h) (:hash h)))
  
