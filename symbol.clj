(ns msl.symbol
  (:refer-clojure)
  (:refer msl))

;; concat-string (str "Sfad" "Fdsafda")
(defn concat-syms [& syms]
  (symbol (apply str (map #(.getName %) syms))))


(defn concat-keywords [& keywords]
  (keyword (apply str (reduce concat (map rest (map str keywords))))))
(defn concat-symbols [& symbols]
  (symbol (apply str (reduce concat (map str symbols)))))