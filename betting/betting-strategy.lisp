(defpackage "BETTING-STRATEGY" 
  (:use "COMMON-LISP"
	#:masd
	"PROBABILITY-LIB"
	"TENNISDB"
	"MATCH-DEDUCT")
  (:export "BET?"))
(in-package "BETTING-STRATEGY")

(defun prize->bet-amount (prize odd)
  (/ prize odd))
(defun bet? (odds predicted-prob pms)
  (if predicted-prob
      (let ((d (- (car (odds->probability odds)) (car predicted-prob))))
	(if (> (abs d) (pms-bet-minimal-diff pms))
	    (if (minusp d)
		(values 0
			(prize->bet-amount (funcall (pms-bet-amount-f pms) (- (abs d) (pms-bet-minimal-diff pms)))
					   (car odds)))
		(values 1 
			(prize->bet-amount (funcall (pms-bet-amount-f pms) (- (abs d) (pms-bet-minimal-diff pms)))
					   (cadr odds))))))))
				   
	    