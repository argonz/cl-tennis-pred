(defpackage #:masd-bayes-statistics
  (:use #:cl
	#:masd
	#:iterate
	#:weight-lib)
  (:export #:make-spectrum
	   #:*spectrums
	   #:/spectrums
	   #:.spectrum
	   #:+spectrum
	   #:-spectrum
	   #:^spectrum
	   #:sum-spectrum
	   #:norm-spectrum
	   #:avg-spectrums
	   #:wavg-spectrums
	   #:infer-spectrums))

(in-package #:masd-batyes-statistics) 


(defmacro make-spectrum (size &key (initial-element 1.0))
  `(make-list ,size :initial-element ,initial-element))
(defmacro *spectrums (spectrum0 spectrum1)
  (oncesyms (spectrum0 spectrum1)
    (gensyms (s0 s1)
      `(iterate (for ,s0 in ,spectrum0) 
		(for ,s1 in ,spectrum1)
		(collect (* ,s0 ,s1))))))
(defmacro /spectrums (spectrum0 spectrum1)
  (oncesyms (spectrum0 spectrum1)
    (gensyms (s0 s1)
      `(iterate (for ,s0 in ,spectrum0) 
		(for ,s1 in ,spectrum1)
		(collect (/ ,s0 ,s1))))))
(defmacro .spectrum (scalar spectrum)
  (oncesyms (scalar spectrum)
    (gensyms (s)
      `(iterate (for ,s in ,spectrum)
		(collect (* ,scalar ,s))))))
(defmacro +spectrums (spectrum0 spectrum1)
  (oncesyms (spectrum0 spectrum1)
    (gensyms (s0 s1)
      `(iterate (for ,s0 in ,spectrum0) 
		(for ,s1 in ,spectrum1)
		(collect (+ ,s0 ,s1))))))
(defmacro -spectrums (spectrum0 spectrum1)
  (oncesyms (spectrum0 spectrum1)
    (gensyms (s0 s1)
      `(iterate (for ,s0 in ,spectrum0) 
		(for ,s1 in ,spectrum1)
		(collect (- ,s0 ,s1))))))
(defmacro ^spectrum (power spectrum)
  (oncesyms (spectrum power)
    (gensyms (s)
      `(iterate (for ,s in ,spectrum) 
		(collect (expt ,s ,power))))))
(defmacro sum-spectrum (spectrum)
  (oncesyms (spectrum)
    (gensyms (s)
      `(iterate (for ,s in ,spectrum) (sum ,s)))))
(defmacro norm-spectrum (spectrum &optional *norm)
  (oncesyms (spectrum)
    (gensyms (s ssum)
      `(let* ((,ssum ,(if *norm
			  `(* (/ 1.0 ,*norm) (sum-spectrum ,spectrum))
			  `(sum-spectrum ,spectrum))))
	 (iterate (for ,s in ,spectrum) (collect (/ ,s ,ssum)))))))
(defmacro inverse-*spectrum (*spectrum)
  (oncesyms (*spectrum)
    (gensyms (s)
      `(iter (for ,s in ,*spectrum)
	     (collect (/ 1.0 ,s))))))
(defmacro avg-spectrums (spectrums)
  (oncesyms (spectrums)
    (gensyms (s)
      `(apply #'mapcar (lambda (&rest ,s) (average ,s)) ,spectrums))))
(defmacro wavg-spectrums (spectrums weights)
  (gensyms (v v0 v1 w)
    `(weighted-average ,spectrums ,weights 
		       :*valwgtop (lambda (,v ,w) (.spectrum ,w ,v))
		       :+valop (lambda (&rest ,v) (reduce (lambda (,v0 ,v1) (+spectrums ,v0 ,v1)) ,v))
		       :/valwgtop (lambda (,v ,w) (.spectrum (/ 1.0 ,w) ,v)))))
(defmacro infer-spectrums (spectrums)
  (oncesyms (spectrums)
    (gensyms (s s0 s1)
      `(iter (for ,s in ,spectrums)
	     (reducing ,s by (lambda (,s0 ,s1) (*spectrums ,s0 ,s1)))))))
(defun inferprint-spectrums (spectrums)
  (iter (for s in spectrums)
	(reducing s by (lambda (s0 s1) (progn (print "INFER") (print s0) (print s1) (*spectrums s0 s1))))))
(defmacro infernorm-spectrums (spectrums &optional (*norm 1.0))
  (oncesyms (spectrums)
    (gensyms (s s0 s1)
      `(iter (for ,s in ,spectrums)
	     (reducing ,s by (lambda (,s0 ,s1) (norm-spectrum (*spectrums ,s0 ,s1) ,*norm)))))))


;; property-statistics
;; direct association -> if a what's the probabilities of b
(defstruct-export 
    (defstruct direct
      a-cues
      b-cues
      a-chash
      b-chash
      a-b-ar))
(defmacro direct-a-b (a b direct)
  `(aref (direct-a-b-ar ,direct) (gethash ,a (direct-a-chash ,direct)) (gethash ,b (direct-b-chash ,direct))))
(defmacro direct-set-a-b (a b value direct)
  `(setf (direct-a-b ,a ,b ,direct) ,value))
(defmacro direct-add-a-b (a b value direct)
  `(direct-set-a-b ,a ,b (+ (direct-a-b ,a ,b ,direct) ,value) ,direct))
(defmacro direct-a->spectrum (a direct)
  (oncesyms (a direct)
    (gensyms (b)
      `(iterate (for ,b in (direct-b-cues ,direct))
		(collect (direct-a-b ,a ,b ,direct))))))
(defmacro direct-b->spectrum (b direct)
  (oncesyms (b direct)
    (gensyms (a)
      `(iterate (for ,a in (direct-a-cues ,direct))
		(collect (direct-a-b ,a ,b ,direct))))))
(defun direct-set-normed-a0 (a *norm direct)
  (iter (for s in (norm-spectrum (direct-a->spectrum a direct) *norm))
	(for b in (direct-b-cues direct))
	(direct-set-a-b a b s direct)))
(defun direct-set-normed-a (direct &optional (*norm 1.0))
  (iter (for a in (direct-a-cues direct))
	(direct-set-normed-a0 a *norm direct)))
(defun direct-set-normed-b0 (b *norm direct)
  (iter (for s in (norm-spectrum (direct-b->spectrum b direct) *norm))
	(for a in (direct-a-cues direct))
	(direct-set-a-b a b s direct)))
(defun direct-set-normed-b (direct &optional (*norm 1.0))
  (iter (for b in (direct-b-cues direct))
	(direct-set-normed-b0 b *norm direct)))
(defun initialize-direct (a-cues b-cues norms abw-observes)
  (let ((dir (make-direct :a-cues a-cues
			  :b-cues b-cues 
			  :a-chash (make-hash-table-initialized :test #'equal 
								:key-value-pairs (iterate (for ac in a-cues)
											  (for i first 0 then (1+ i))
											  (collect (list ac i))))
			  :b-chash (make-hash-table-initialized :test #'equal
								:key-value-pairs (iterate (for bc in b-cues)
											  (for i first 0 then (1+ i))
											  (collect (list bc i))))
			  :a-b-ar (make-array (list (list-length a-cues) (list-length b-cues)) 
					      :initial-element least-positive-double-float)))); least-positive-single-float))))

    (iter (for (a b w) in abw-observes)
	  (direct-add-a-b a b w dir))
    (iter (for n in norms)
	  (if (listp n)
	      (cond ((equal (car n) :a) (direct-set-normed-a dir (cadr n)))
		    ((equal (car n) :b) (direct-set-normed-b dir (cadr n))))
	      (cond ((equal n :a) (direct-set-normed-a dir))
		    ((equal n :b) (direct-set-normed-b dir)))))
    dir))
	

(defun direct->string (dir)
  (format nil 
	  "~&~{~{key: ~a ~%spectrum: ~%~{~{key: ~a  prob: ~a~%~}~}~%~}~}" 
	  (iter (for a in (direct-a-cues dir))
		(collect (list a
			       (iter (for s in (direct-a->spectrum a dir))
				     (for b in (direct-b-cues dir))
				     (collect (list b s))))))))
       
(defstruct-export
    (defstruct asc
      a-cues
      b-cues
      a-dir
      b-dir))
(defmacro asc-insert-ab (a b w asc)
  (oncesyms (a b w asc)
    `(progn (asc-set-a-b ,a ,b (+ (asc-a-b ,a ,b ,asc) ,w) ,asc)
	    (asc-set-b-a ,b ,a (+ (asc-b-a ,b ,a ,asc) ,w) ,asc))))
(defmacro asc-a->spectrum (a asc)
  `(direct-a->spectrum ,a (asc-a-dir ,asc)))
(defmacro asc-b->spectrum (b asc)
  `(direct-b->spectrum ,b (asc-b-dir ,asc)))
(defmacro asc-adir-a-b (a b asc)
  `(direct-a-b ,a ,b (asc-a-dir ,asc)))
(defmacro asc-bdir-a-b (a b asc)
  `(direct-a-b ,a ,b (asc-b-dir ,asc)))
(defun initialize-asc (a-cues b-cues adir-norms bdir-norms &optional abw-observes)
  (let ((asc (make-asc :a-cues a-cues
		       :b-cues b-cues
		       :a-dir (initialize-direct a-cues b-cues adir-norms abw-observes)
		       :b-dir (initialize-direct a-cues b-cues bdir-norms abw-observes)))) ;nem rakjuk forditva bele csak lek.
    asc))
			      




(defun print-spectrum (keys values)
  (format nil "~&~{~{key: ~a  prob:  ~a~%~}~}" (iterate (for k in keys) (for v in values) (collect (list k v)))))
(defun print-a->spectrum (a asc)
  (print-spectrum (asc-b-cues asc) (asc-a->spectrum a asc)))
(defun print-b->spectrum (b asc)
  (print-spectrum (asc-a-cues asc) (asc-b->spectrum b asc)))
(defun print-asc (asc)
  (format t 
	  "~&printing asc:~%~%adir:~{~{~&KEY: ~a ~%~%spect:~%~a~}~%~}~%~%bdir:~%~{~{KEY: ~a ~%~%spect:~%~a~}~%~}"
	  (iterate (for a in (asc-a-cues asc)) 
		   (collect (list a (print-a->spectrum a asc))))
	  (iterate (for b in (asc-b-cues asc))
		   (collect (list b (print-b->spectrum b asc))))))
