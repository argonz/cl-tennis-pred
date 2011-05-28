(defpackage "CONDITION-EXCHANGE"
  (:use "COMMON-LISP"
	#:masd
	"TENNISDB"
	"MATCH-LIB"
	"PARAMETERS"
	"PENV")
  (:export "INITIALIZE-EXCH"
	   "EXCHANGE"))
(in-package "CONDITION-EXCHANGE")

;; letesztelni - bizonytalan vagyok hogy sul el.
(defmacro time->weight (time penv)
  `(funcall (pms-exch-match-timeweight-f (penv-pms ,penv)) ,time))
(defmacro match->weight (match penv)
  `(time->weight (- (penv-date ,penv) (match-date ,match)) ,penv))
(defmacro weightsum->final-weight (weightsum penv)
  `(funcall (pms-exch-weight-f (penv-pms ,penv)) ,weightsum))
(defun matches->ratio-weight (playerid type subtype penv)
  (labels ((rec (ms won lost)
	     (if ms
		 (if (won? playerid (car ms))
		     (rec (cdr ms) (+ won (match->weight (car ms) penv)) lost)
		     (rec (cdr ms) won (+ lost (match->weight (car ms) penv))))
		 (list (/ won lost) (weightsum->final-weight (+ won lost) penv)))))

    (rec (filter-matches-and-f (playerid->matches playerid (penv-db penv)) type subtype)
	 (time->weight (pms-exch-match-start-w-d0 (penv-pms penv)) penv)
	 (time->weight (pms-exch-match-start-w-d0 (penv-pms penv)) penv))))
	 

;; the structure of the exchange
;; id -> type -> subt0 subt1 -> (win-ratio weight)
(defun playerid->type-hash (playerid penv)
  (gethash-or-make playerid (penv-exch penv) (make-hash-table :test #'equal)))
(defun type->subtype-hash (type type-hash)
  (gethash-or-make type type-hash (make-hash-table :test #'equal)))
(defun subtype->ratio-weight-or-make (subtype subtype-hash type playerid penv)
  (gethash-or-make subtype 
		   subtype-hash  
		   (matches->ratio-weight playerid type subtype penv)))


(defun subtypes->ratio-weight (stype0 stype1 type-hash type playerid penv)
  (if (equal stype0 stype1)
      (list 1.0 1.0)
      (let ((s0 (subtype->ratio-weight-or-make stype0 (type->subtype-hash type type-hash) type playerid penv))
	    (s1 (subtype->ratio-weight-or-make stype1 (type->subtype-hash type type-hash) type playerid penv)))
	(list (/ (car s1) (car s0)) (min (cadr s0) (cadr s1))))))


(defun exchange (playerid type subtype0 subtype1 penv)
  (subtypes->ratio-weight subtype0 
			  subtype1  
			  (type->subtype-hash type (playerid->type-hash playerid penv))
			  type
			  playerid 
			  penv))
