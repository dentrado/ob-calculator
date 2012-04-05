(ns ob-calculator.core
  (:import (java.util Random))
  (:use clojure.contrib.math))

(defn debug [n] (println n) n)

;; Random:
(def randgen (Random.))

(defn roll []
  (+ 1 (.nextInt randgen 6)))

(defn ob-roll []
  (let [r (roll)]
    (if (= r 6)
      (+ (ob-roll) (ob-roll))
      r)))

(defn ob-stats [sample-size]
  (->> (repeatedly ob-roll)
       (take sample-size)
       (frequencies)
       (sort)))

(defn stat-table [rolls sample-size]
  (doseq [[k v] rolls]
    (println (format "%d\t| %.4f" k (double (/ v sample-size))))))


;; Exact:
(defn prob-1t6 [n]
  (if (< 0 n 7)
    1/6
    0))

(def prob-Ob1t6
     (memoize (fn [n] 
		(let [probs (map #(* (prob-Ob1t6 %1) (prob-Ob1t6 %2))
				 (range 1 n)		 ; 1,2,...,n
				 (range (dec n) 0 -1))	 ; n,n-1,...,1
		      sum (/ (reduce + probs) 6)]
		  (if (< 0 n 6)
		    (+ 1/6 sum)
		    sum)))))

(def multi-prob
     (memoize
      (fn [die-fn dice num]
	(if (= dice 1)
	  (die-fn num)
	  (let [probs (map #(* (die-fn %1)
			       (multi-prob die-fn (dec dice) %2))
			   (range 1 num)	    ; 1,2,...,n
			   (range (dec num) 0 -1))] ; n,n-1,...,1
	    (reduce + probs))))))

(defn less-multi-prob
  "calculates the probability of getting num or less with 'dice' nr of dice"
  [die-fn dice num]
  (reduce + (map #(multi-prob die-fn dice %) (range 1 (inc num)))))

(defn more-multi-prob
  "calculates the probability of getting num or more with 'dice' nr of dice"
  [die-fn dice num]
  (- 1 (less-multi-prob die-fn dice (dec num))))

(def prob-nt6 (partial multi-prob prob-1t6))
(def less-prob-nt6 (partial less-multi-prob prob-1t6))
(def more-prob-nt6 (partial more-multi-prob prob-1t6))

(def prob-Obnt6 (partial multi-prob prob-Ob1t6))
(def less-prob-Obnt6 (partial less-multi-prob prob-Ob1t6))
(def more-prob-Obnt6 (partial more-multi-prob prob-Ob1t6))

(defn print-table [fn dice num]
  (print "n  dice:|")
  (doseq [die (range 1 (inc dice))]
    (print (str die "\t|")))
  (println)
  (dotimes [n (* 11 dice)] (print "-"))
  (println)
  (doseq [n (range 1 (inc num))]
    (print (str  n "\t|"))
    (doseq [die (range 1 (inc dice))]
      (print (format "%.4f\t|" (double (fn die n)))))
    (println)))

(defn print-latex-table [fn dice num]
  (println (str "\\begin{tabular}{l | *{" dice "}{c} }"))
  (print "n \\hspace{1mm} \\slash \\hspace{1mm} t: & ")
  (doseq [die (range 1 dice)]
    (print (str die " & ")))
  (println (str dice " \\\\"))
  (println "\\hline")
  (doseq [n (range 1 (inc num))]
    (print (str  n " & "))
    (doseq [die (range 1 dice)]
      (print (format "%.4f & " (double (fn die n)))))
    (println (format "%.4f \\\\ \\hline" (double (fn dice n)))))
  (println "\\end{tabular}"))

(comment
  (def prob-Obnt6
       (memoize
	(fn [dice num]
	  (cond
	   (= dice 1) (prob-Ob1t6 num)
	   :else (let [probs (map #(* (prob-Ob1t6 %1)
				      (prob-Obnt6 (dec dice) %2))
				  (range 1 num)		 ; 1,2,...,n
				  (range (dec num) 0 -1))] ; n,n-1,...,1

		   (println probs)
		   
		   (reduce + probs)))))))