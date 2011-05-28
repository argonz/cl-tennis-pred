(in-package #:training)
(defparameter +db+ (initialize-db-from-dif "/home/mate/prog/lisp/tennis-pred/data/atp2008.dif" 
					   "/home/mate/prog/lisp/tennis-pred/data/atp2007.dif" 
					   "/home/mate/prog/lisp/tennis-pred/data/atp2006.dif"))
(defparameter +dbo+ (copy-tennisdb +db+))
(defparameter +db-stat+ (copy-tennisdb-date +db+ :max-date (- +current-date+ 50)))
(defparameter +cases+ (make-training-series 300 (- +current-date+ 50) +current-date+ 1.0 +db+))

(defparameter +pms+ (initialize-pms 0.0035 0.0035))
;; ROSSZ A BAYES -> 5 settesre TELJESEN IDIOTA ERTEKEKET KAP!!! -> osszevonni 3set es 5setet -es inkabb 3set penzeset hszn.

(defun choose-pid (odds probs pid0 pid1)
  (let ((ev0 (estimated-value (car probs) (car odds)))
	(ev1 (estimated-value (cadr probs) (cadr odds))))
    (if (> ev0 ev1)
	(if (> ev0 1.45)
	    pid0)
	(if (> ev1 1.45)		;megvan cserelve!!!!
	    pid1))))
	    
;;FELCXS
(defun evaluate-cases (cases pms os &optional print)
  (labels ((rec (cs mon)
	     (if cs
		 (let* ((c (car cs))
			(p (predict (training-case-pid0 c) 
				    (training-case-pid1 c) 
				    (initialize-penv (training-case-date c)  
						     (getf (training-case-match-conditions c) :surface)
						     pms
						     os
						     (training-case-db c)))))
		   (if p
		       (let ((pid (choose-pid (training-case-odds c)
					      (list p (- 1.0 p)) 
					      (training-case-pid0 c)
					      (training-case-pid1 c))))
			 
			 (if pid
			     (let ((evbet (evaluate-bet pid 1.0 c)))
			       (if print (format t "~&prediction: ~a  odds: ~a  choosen: ~a  ev:  ~a  mon: ~a~%" 
						 p (car (odds->prob (training-case-odds c))) pid evbet mon))
			       (rec (cdr cs) (+ evbet mon)))
			     (rec (cdr cs) mon)))
		       (rec (cdr cs) mon)))
		 mon)))
    
    (rec cases 0.0)))