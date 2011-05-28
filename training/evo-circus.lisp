(defpackage "EVO-CIRCUS"
  (:use "COMMON-LISP"
	"TENNISDB"
	"PROBABILITY-LIB"
	"MATCH-PREDICT"
	"MATCH-DEDUCT"
	"BETTING-STRATEGY"
	"TRAINING-SET"
	"GRAPH-EVO"
	"WEIGHT-LIB"))

(in-package "EVO-CIRCUS")


;; (defun overbet? (amount overbet-th)
;;   (> amount overbet-th))
;; (defun penalty-for-overbet (odd amount max-money)
;;   (- (* 2 odd amount max-money)))
(defun betting (tcase max-money pms &optional print)
  (let* ((penv (initialize-penv (training-case-date tcase) 
				(getf (training-case-match-conditions tcase) :surface)
				(training-case-db tcase) 
				pms))
	 (predict (average-predicted-probability (training-case-pid0 tcase) 
						 (training-case-pid1 tcase)
						 (training-case-odds tcase) 
						 penv)))

    (multiple-value-bind (choosen amount) (bet? (training-case-odds tcase) predict pms)

      (if print 
	  (format t "~&CASE: ~a~%predict: ~a  odds: ~a~%choosen: ~a amount: ~a" tcase predict (odds->probability (training-case-odds tcase)) choosen amount))

      (if choosen
	  (if (= choosen 0)
		  (evaluate-bet (training-case-pid0 tcase) (* max-money amount) tcase)
		  (evaluate-bet (training-case-pid1 tcase) (* max-money amount) tcase))
	  0))))
		

(defun simulate-series (series params &optional print)
  (let ((pass-bound (* (list-length series) 7/8)))
    (labels ((rec (cases money max-money passed pms)
	       (if (> money max-money passed)
		   (setf max-money money))
	       
	       (if (> passed pass-bound)
		   (- (expt 1.4 (list-length series)))

		   (if (<= money 0)
		       (- max-money (expt 1.25 (list-length cases)))

		       (if cases
			   (let ((bet (betting (car cases) max-money pms print)))

			     ;; printing
			     (if print
				 (format t "~&money: ~a max-money: ~a~%bet: ~a"money max-money bet))

			     (if (zerop bet)
				 (rec (cdr cases) money max-money (1+ passed) pms)
				 (rec (cdr cases) (+ money bet) max-money passed pms)))
			   money)))))
		
      ;; normalize for power 
      (let ((result (rec series 1.0 1.0 0 (apply #'initialize-pms params))))
	(if (> result 0.0)
	    (expt result (/ 1 (list-length series)))
	    (- (expt (abs result) (/ 1 (list-length series)))))))))

(defun simulate-training-series (series-list params)
  (labels ((rec (sl retfit retweight)
	     (if sl
		 (rec (cdr sl) 
		      (cons (simulate-series (car sl) params) retfit) 
		      (cons (list-length (car sl)) retweight))
		 (list retfit retweight))))
    (let ((result (rec series-list nil nil)))
      (weighted-average (car result) (cadr result)))))    
	       
(defun make-test-fitness-f (training-series)
  (let* ((worst most-negative-single-float)
	 (best worst))
    (lambda (sol) (if (check-params sol)
		      (let ((result (simulate-training-series training-series sol)))
			(if (> result best)
			    (progn 
			      (format t "~&money: ~a - sol:~{ ~a~}" result sol)
			      (setf best result)))
			result)
		      worst))))

(defun make-description (ndesc n-match min-date max-date)
  (loop repeat ndesc collect `(,n-match ,min-date ,max-date)))



;; money: 1.0088798 - sol: 0.018010749 0.6873965 0.40681553 0.002214776 105.56745 0.00985587 0.9724453 0.0012182222 1.740857 0.009006927 0.9605761 0.07994462 1.219439+++
;; money: 1.0090421 - sol: 0.020891538 0.74816 0.44264263 0.001513708 109.99901 0.005487041 0.9692368 0.001292327 2.0713775 0.005424517 0.9630523 0.06844906 1.2166659+
;; money: 1.0001603 - sol: 0.015090737 0.74529743 0.44229126 0.0017002057 105.8491 0.009460704 0.96678734 0.0013380933 1.8703922 0.005338065 0.961125 0.060878284 1.2432874
;; money: 1.0039657 - sol: 0.019035082 0.7322588 0.46684858 0.002263965 101.572655 0.006016649 0.972225 0.0010112496 1.921037 0.0059638205 0.96167284 0.05061381 1.2101158+++
;; money: 0.99994886 - sol: 0.019322544 0.71054876 0.53930396 0.0018631011 110.45759 0.008468645 0.9724084 0.001045061 2.122877 0.007578435 0.97120297 0.1094358 1.217335
;; money: 1.0023913 - sol: 0.019502154 0.7140048 0.46143943 0.0017890405 111.499054 0.0051741763 0.9513289 0.0013188149 1.817964 0.008848367 0.9655201 0.13213748 1.1878469

(defparameter *p* (list 0.019 0.732 0.4668 0.0022 101.572 0.006 0.972 0.001 1.92 0.006 0.961 0.19 0.000000101))
(defun in-interval? (p min max)
  (< min p max))
(defun check-params (params)
  (and (in-interval? (nth 0 params) 0.000001 1.0)
       (in-interval? (nth 1 params) 0.000001 1.0)
       (in-interval? (nth 2 params) 0.000001 1.0)

       ;; wl-count
       (in-interval? (nth 3 params) 0.000001 1.0)
       (in-interval? (nth 4 params) 0.0 most-positive-single-float)
       (in-interval? (nth 5 params) 0.000001 1.0)
       (in-interval? (nth 6 params) 0.000001 1.0)

       ;; exch
       (in-interval? (nth 7 params) 0.000001 1.0)
       (in-interval? (nth 8 params) 0.0 most-positive-single-float)
       (in-interval? (nth 9 params) 0.000001 1.0)
       (in-interval? (nth 10 params) 0.000001 1.0)

       ;; bet
       (in-interval? (nth 11 params) 0.000001 100.0)
       (in-interval? (nth 12 params) 0.000001 100.0)))

(defparameter *db* (initialize-db-from-dif "/home/mate/prog/lisp/tennis-pred/data/atp2008.dif"
					   "/home/mate/prog/lisp/tennis-pred/data/atp2007.dif"))

(defparameter *training-set* (make-training-set 
			      (append (make-description 1 200 (translate-date 2008 0 1) (translate-date 2009 0 1))
;;				      (make-description 3 15 (translate-date 2008 0 1) (translate-date 2009 0 1))
;;					(make-description 2 20 (translate-date 2008 0 1) (translate-date 2009 0 1)) 
    ;; 					(make-description 1 60 (translate-date 2008 0 1) (translate-date 2009 0 1)))
				      )
			      1.0
			      *db*))

(defun make-starting-circus (n type)
  (let ((params (list :fitness-f (make-test-fitness-f *training-set*)
		      :eject-f (lambda (s f i) (progn (if (= (mod i 25) 0)
							  (format t "+"))
						      nil))
		      :fitness-compare-f #'>
		      :starting-data-set (graph-evo::make-starting-data-set n   
									    ;; deduct
									    '(0.015 0.02)
									    '(0.7 0.75)
									    '(0.4 0.5)
								      
									    ;; wlcount
									    '(0.0015 0.0025)
									    '(100.0 110.0)
									    '(0.005 0.01)
									    '(0.96 0.98)
								      
									    ;; exch
									    '(0.001 0.0015)
									    '(1.8 2.2)
									    '(0.005 0.01)
									    '(0.96 0.97) 
									    
									    ;; bet
									    '(0.05 0.1)
									    '(1.2 1.25)))))

    (if (equal type :asexual)
	(apply #'graph-evo::make-asexual-graph-evo (append (list :CR 0.5
								 :spreadco 0.9
								 :starting-neighbour-set (graph-evo::make-starting-neighbour-set n '(1 -1)))
							   params))
	(apply #'graph-evo::make-diff-graph-evo (append (list :starting-neighbour-set (graph-evo::make-starting-neighbour-set n '(-2 -1 1 2))) params)))))


(defun start-circus (n type)
  (let ((c (make-starting-circus n type)))
    (loop repeat 1000000 do (graph-evo::step-graph-evo c))))
							

