;; (require "GRAPH-EVO")
(in-package "RATIO-NET")


;; TESTING for find the correct parameters for the iteration numbers 
(defun valid-connect? (c exist-connects)
  (and (not (equal (car c) (cadr c))) 
       (not (or (member c exist-connects :test #'equal)
		(member (reverse c) exist-connects :test #'equal)))))
(defun make-random-connects-ids (n-connect n-node)
  (labels ((rec (n ret)
	     (if (zerop n)
		 ret
		 (let ((c (list (random n-node) (random n-node))))
		   (if (valid-connect? c ret)
		       (rec (1- n) (cons c ret))
		       (rec n ret))))))
    (rec n-connect nil)))  
(defun make-test-case (n-node n-connect connect-min-max)
  (let ((net (initialize-net)))
    (loop for i from 0 repeat n-node do (insert-initialize-node i net))
    (loop 
       for i from 0 repeat n-connect
       for ids in (make-random-connects-ids n-connect n-node) 
       do 
	 (insert-initialize-connect (car ids) 
				    (cadr ids) 
				    (+ (car connect-min-max) (random (float (- (cadr connect-min-max)
									       (car connect-min-max))))) 
				    (random 1.0)
				    net))
    net))

;; cases-el kapcsolatban 
;; (defparameter *cases* (list (make-test-case 20 50 '(0.2 8.0))
;; 			    (make-test-case 40 150 '(0.2 8.0)) 
;; 			    (make-test-case 80 450 '(0.2 8.0))
;; 			    ))
;; fitness: 28.832178  -  params: (1.6529486 41.4617 0.47211388 0.0067126676)++
;; fitness: 14.503  -  params: (1.5393888 39.803493 0.4297058 0.0050685667
;; szerintem -> 1.55 60 0.75 0.025  -  ezek szerintem jo beallitasok

(defparameter *p* (list 1.65 55 0.45 0.0035))
(defparameter *cases* (list
;		       (make-test-case 15 40 '(0.2 8.0))
		       (make-test-case 30 120 '(0.2 8.0)) 
		       (make-test-case 60 380 '(0.2 8.0))
		       ;; 			    (make-test-case 140 1200 '(0.1 10.0)) 
			    ))

(defun square-error-of-min-max (values power)
  (expt (abs (- (max-list values) (min-list values))) power))

;; params (power-it multip-it delta-start delta-end)
(defun test-a-case (n params net)
  (loop repeat n 
     collect
       (compare-represents 1 0 (set-influences-of-connects (car params) (cadr params) (caddr params) (cadddr params) (copy-net net)))))

(defun in-interval? (p min max)
  (< min p max))
(defun valid-params? (params)
  (and (in-interval? (car params) 1.0 1.9)
       (in-interval? (cadr params) 10 120)
       (in-interval? (caddr params) 0.1 0.9)
       (in-interval? (cadddr params) 0.00001 0.2)))

(defun fitness-of-test (ep mp errp)
  (lambda (sol) (if (valid-params? sol)
		    (+ (expt (car sol) ep) ;expt part
		       (expt (cadr sol) mp) ;multip part
		       (expt (/ (loop for c in *cases* 
				   sum
				     (let ((e (* 100 (square-error-of-min-max (test-a-case 6 sol c) 1))))
				       (if (< e 1.0)
					   1.0
					   e)))
				(list-length *cases*))
			     errp))
		    10000000000)))

(defun start-evo (npop)
  (let ((g (make-diff-graph-evo :fitness-f (fitness-of-test 2.5 0.25 3.25)
				:eject-f (let ((best 1000000))
					   (lambda (s f i) (progn (if (= (mod i 1000) 0) (format t "+"))
								  (if (< f best)
								      (progn
									(setf best f)
									(format t "~&fitness: ~a  -  params: ~a" f s))))))
				:starting-data-set (make-starting-data-set npop '(1.5 1.65) '(35 45) '(0.35 0.45) '(0.005 0.025))
				:starting-neighbour-set (make-starting-neighbour-set npop '(-2 -1 1 2)))))
    (loop repeat 10000000 do (step-graph-evo g))))
				
				
