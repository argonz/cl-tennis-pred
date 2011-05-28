(defpackage "CURVE-FUNCTIONS"
  (:use "COMMON-LISP"
	#:masd)
  (:export "MAKE-TAIL-F"
	   "MAKE-COMPLEMENTER-TAIL-F"
	   "MAKE-SQUARE-X"
	   "MAKE-REFINER-F"
	   "MAKE-TAIL-ITERATE-F"))

(in-package "CURVE-FUNCTIONS")


;; TAIL FUNCTIONS -> THE EXPONENTS
(defun make-tail-f (p &key (at0 1.0) (atinf 0.0) (base 0.5))
  (assert (and (> at0 atinf) (< 0 base 1)))
  (if (and (= atinf 0.0) (= at0 1.0)) 
      (lambda (x) (expt base (* p x)))
      (let ((diff (abs (- at0 atinf))))
	(lambda (x) (+ atinf (* diff (expt base (* p x))))))))

(defun make-complementer-tail-f (p &key (at0 0.0) (atinf 1.0) (base 0.5))
  (assert (and (< at0 atinf) (< 0 base 1)))
  (if (and (= atinf 1.0) (= at0 0.0)) 
      (lambda (x) (- 1 (expt base (* p x))))
      (let ((diff (abs (- atinf at0))))
	(lambda (x) (+ atinf (* diff (- 1 (expt base (* p x)))))))))
  
(defun make-square-x (p)
  (lambda (x) (expt x p)))		;igazabol lefele hajlonak kepzelem el -> p E [0 1]

(defun make-refiner-f (p)		;1.0 tol valo kilengest csokkenti
  (let ((fn (make-square-x p)))
    (lambda (x) 
      (if (> x 1.0)
	    (+ 1.0 (funcall fn (- x 1.0)))
	    (/ 1 (+ 1.0 (funcall fn (- (/ 1 x) 1.0))))))))

;; minel jobban eltavolodik az 1.0 tol annal jelentektelenebb
(defun make-1.0-weight-f (p)
  (let ((fn (make-tail-f p)))
    (lambda (x) 
      (if (> x 1.0)
	  (funcall fn (- x 1.0))
	  (funcall fn (- (/ 1 x) 1.0))))))
	

;; a monte-carlo-sampling hasonlo - mivel idovel egyre kisebb a delta, ezt hasznalja
(defun make-tail-iterate-f (max min n-iterate)
  (assert (<= min max))
  (let* ((shift (log max 0.5))
	 (p (/ (- (log min 0.5) 
		  shift) 
	       (1- n-iterate))))	;nullarol kezdodik az iteralas
    (lambda (x) (expt 0.5 (+ (* x p) shift)))))

