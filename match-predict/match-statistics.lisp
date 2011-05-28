(defpackage "MATCH-STATISTICS"
  (:use "COMMON-LISP"
	#:masd
	"PROBABILITY-LIB"
	"TENNISDB"
	"PARAMETERS")
  (:export "MAKE-RATIO-HASH"
	   "RATIO-FROM-RATIO-HASH"
	   "PROBABILITY-FROM-RATIO"
	   "PROBABILITY-FROM-RATIO-HASH"
	   "PROBABILITY-FROM-RAIO-HASH-GEOMETRICAL"))

(in-package "MATCH-STATISTICS")
;; USE
;; make-ratio hash from a db
;; receive probability of winning giving the sets and a ratio-hash (the statistics


;; (defun set-diff (set)
;;   (if (> (car set) (cadr set))
;;       (/ (car set) (+ 0.5 (cadr set)))
;;       (/ (cadr set) (+ 0.5 (car set)))))
;; (defun order-sets (sets)
;;   (sort (copy-list sets) (lambda (s1 s2) (< (set-diff s1) (set-diff s2)))))
(defmacro nr-of-sets (sets)
  `(list-length ,sets))

(defparameter +winning-sets+ '((6 0) (6 1) (6 2) (6 3) (6 4) (7 5) (7 6)))
(defparameter +sets+ (append +winning-sets+ (mapcar #'reverse +winning-sets+)))

(defun insert-match (h0 sets)
;;   (loop for s in (order-sets sets)
;;        for i from 1 upto 5
  (loop for s in sets
       do
       (progn
	 (let ((rwin (gethash s h0))) ;winnning side
	   (setf (gethash s h0) (list (1+ (car rwin)) (cadr rwin))))
	 (let ((rlos (gethash (reverse s) h0))) ;losing side
	   (setf (gethash (reverse s) h0) (list (car rlos) (1+ (cadr rlos))))))))

;;(setf d (build-tennisdb "/home/mate/prog/lisp/tennis-pred/data/atp2008.dif" "/home/mate/prog/lisp/tennis-pred/data/atp2007.dif"))
(defun make-ratio-hash (matches)
  (let ((h0 (make-hash-table :test #'equal)))

    ;; for every set-length
    (loop for k in +sets+ 
       do 
	 (setf (gethash k h0) (list 0 0)))

    ;; insert set-results
    (loop for m in matches 
       do
	 (insert-match h0 (match-sets m)))
    
    ;; normalize
    (loop for k being the hash-key of h0
       for v being the hash-value of h0
       do
;;	 (setf (gethash k h0) (/ (car v) (+ (car v) (cadr v)))))   ;; ha szazalekot szeretnenk
	 (setf (gethash k h0) (/ (car v) (cadr v)))) ; ha aranyt szeretnenk

    h0))

;; (defun probability-from-ratio-hash-geometrical (sets ratio-hash)
;;   (expt (product (mapcar (lambda (s) (gethash s ratio-hash)) sets))
;; 	(/ 1 (nr-of-sets sets))))

(defun probability-from-ratio-hash (sets ratio-hash)
  (probability-from-ratio (/ (sum (mapcar (lambda (s) (gethash s ratio-hash)) sets))
			     (nr-of-sets sets))))
(defun ratio-from-ratio-hash (sets ratio-hash)
  (/ (sum (mapcar (lambda (s) (gethash s ratio-hash)) sets))
     (nr-of-sets sets)))


;; (defun initialize-winprob (db)
;;   (let ((cs (categorize-matches (get-all-matches db) :carpet)))
;;     (make-winprob :all (make-ratio-hash (get-all-matches db))
;; 		  :clay (make-ratio-hash (getf cs :clay))
;; 		  :carpet (make-ratio-hash (getf cs :carpet))
;; 		  :hard (make-ratio-hash (getf cs :hard))
;; 		  :grass (make-ratio-hash (getf cs :grass)))))
   

;; printing
(defun print-ratio-hash (ratio-hash)
  (format nil 
	  "~{~{~& Sets: ~a  -  Prob: ~f~}~}"
	  (loop 
	     for k0 being the hash-key of ratio-hash
	     for v0 being the hash-value of ratio-hash
	     collect
	       (list k0 v0))))

;; (defun print-winprob (wp)
;;   (format nil 
;; 	  "~% Clay:~%~a~%~% Carpet:~%~a~%~% Hard:~%~a~%~% Grass:~%~a~%~% All:~%~a~%"
;; 	  (print-ratio-hash (winprob-clay wp))
;; 	  (print-ratio-hash (winprob-carpet wp))
;; 	  (print-ratio-hash (winprob-hard wp))
;; 	  (print-ratio-hash (winprob-grass wp))
;; 	  (print-ratio-hash (winprob-all wp))))
		 
		 
	

