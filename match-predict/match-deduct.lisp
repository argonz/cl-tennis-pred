(defpackage "MATCH-DEDUCT"
  (:use "COMMON-LISP"
	#:masd
	"PROBABILITY-LIB"
	"TENNISDB"
	"CURVE-FUNCTIONS"
;	"CONDITION-EXCHANGE"
;	"SET-STATISTICS"
	"SET-STATISTICS-BUILD"
	"WEIGHT-LIB")
  (:export "DEDUCT-MATCH->RATIO-WEIGHT"))

(in-package "MATCH-DEDUCT")
	
(defun deduct-match->timeweight (match penv)
  (funcall (pms-deduct-match-timeweight-f (penv-pms penv)) (- (penv-date penv) (match-date match))))
(defun deduct-ratio (playerid match penv)
  (list (odds->ratio (odds-avg-according playerid match))
	1.0))
;;   (let ((wl-count-rat (wl-ratio playerid (match-surface match) penv))) ;mennyiszer nyer azon a terepen

;;     (if (match-odds-avg match)
;; 	(let ((section (pms-deduct-odds-section (penv-pms penv))))
;; 	  (list (weighted-average (list (* (match->set-ratio playerid match) ;a sectionnek megf. szamitjak be az oddst
;; 					   (car wl-count-rat))
;; 					(odds->ratio (odds-avg-according playerid match)))
;; 				  (list (- 1 section) section))
;; 		(cadr wl-count-rat)))
	  
;; 	(list (* (match->set-ratio playerid match) (car wl-count-rat))
;; 	      (* (cadr wl-count-rat) (pms-deduct-noodds-weight-multp (penv-pms penv))))))) ;nincs odds buntetes

(defun exchange-deducted-ratio (deducted-ratio playerid match penv)
  (let ((exc (exchange playerid :surface (match-surface match) (penv-surface penv) penv)))
    (list (* (car deducted-ratio) (car exc))
	  (* (cadr deducted-ratio) (cadr exc)))))
 
(defun deduct-match->ratio-weight (match playerid penv)
  (let ((p (match->average-prob match)))
    (if (won? playerid match) 
	(list (probability->ratio p)  (deduct-match->timeweight match penv))
	(list (probability->ratio (- 1 p)) (match->average-prob match) (deduct-match->timeweight match penv)))))

 ;;  (let* ((oppid (opponent playerid match))
;; 	 (r0 (exchange-deducted-ratio (deduct-ratio playerid match penv) playerid match penv)) ;a megkapottat at is valtj.
;; 	 (r1 (exchange-deducted-ratio (deduct-ratio oppid match penv) oppid match penv)))

;;     (list (weighted-average (list (car r0) (/ 1 (car r1)))
;; 			    (list (cadr r0) (cadr r1)))
;; 	  (* (weighted-average (list (cadr r0) (cadr r0)) ;hat ennel gazabb nincs a kodban :D
;; 			       (list (Cadr r0) (cadr r0)))
;; 	     (deduct-match->timeweight match penv)))))
	 

