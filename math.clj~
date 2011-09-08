;;; math.clj
;;;
;;; This is a tiny math library for Clojure.  It wraps most of
;;; java.lang.Math in non-reflective Clojure functions, and adds a few
;;; extra functions for convenience.
;;;
;;; by Stuart Sierra, mail@stuartsierra.com
;;;
;;; THE ACCOMPANYING PROGRAM IS PROVIDED UNDER THE TERMS OF THE COMMON
;;; PUBLIC LICENSE. ANY USE, REPRODUCTION OR DISTRIBUTION OF THE
;;; PROGRAM CONSTITUTES RECIPIENT'S ACCEPTANCE OF THIS AGREEMENT.
;;;
;;; Full text of the Common Public License is included with the
;;; Clojure distribution, and is available at
;;; http://www.opensource.org/licenses/cpl1.0.php


(in-namespace 'math)

;; Pi
(def PI (. java.lang.Math PI))

;; Euler's number
(def E (. java.lang.Math E))

;; Absolute value.
(defn abs [x]
  (if (< x 0) (- x) x))

;; java.lang.Math static functions taking one Double argument.  For
;; overloaded functions, always uses the Double version.
(defmacro one-double [& names]
  `(do ~@(map (fn [name] 
		  `(defn ~name [x#] (. java.lang.Math 
				       (~name #^java.lang.Double x#))))
	      names)))

(one-double acos asin atan cbrt ceil cos cosh exp expm1 floor log
	    log10 log1p rint sin sinh sqrt tan tanh toDegrees
	    toRadians round signum)

;; java.lang.Math static functions taking two Double arguments.  For
;; overloaded functions, always uses the Double version.
(defmacro two-doubles [& names]
  `(do ~@(map (fn [name] 
		  `(defn ~name [x# y#] 
		     (. java.lang.Math (~name #^java.lang.Double x# 
					      #^java.lang.Double y#))))
	      names)))

(two-doubles atan2 hypot IEEEremainder pow copySign)

;; Random number between zero and one, inclusive.
(defn random [] (. java.lang.Math (random)))

;; Nth root of a real number.
(defn nthroot [n real] (pow real (/ n)))

;; Logarithm to an arbitrary base, using the change-of-base formula.
(defn logb [base real]
  (/ (log real) (log base)))

;; Base-2 logarithm.
(defn log2 [x] (logb 2 x))

(def *exports* '(math PI E abs acos asin atan cbrt ceil cos cosh exp
	    expm1 floor log log10 log1p rint sin sinh sqrt tan tanh
	    toDegrees toRadians round signum atan2 hypot IEEEremainder
	    pow copySign random nthroot logb log2))
