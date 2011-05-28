(in-package #:match-predict)
(defparameter +db+ (initialize-db-from-dif "/home/mate/prog/lisp/tennis-pred/data/atp2008.dif" 
					   "/home/mate/prog/lisp/tennis-pred/data/atp2007.dif" 
					   "/home/mate/prog/lisp/tennis-pred/data/atp2006.dif"))
;;(defparameter +dbl+ (initialize-db-from-dif "/home/mate/prog/lisp/tennis-pred/data/atp2008.dif"))

(defparameter +sl+ (list-length +sets+)) ;list-length of the spectrums
(defparameter +stat-time-wf+ (make-tail-f 0.0004))  
(defun match->stat-tw (match)
  (funcall +stat-time-wf+ (- +current-date+ (match-date match))))


(defun match->set-set (match)
  (labels ((rec (s0 s1 ret)
	     (if s1
		 (rec s0 (cdr s1) (cons (list (car s0) (car s1)) ret))
		 (if (cdr s0)
		     (rec (cdr s0) (cddr s0) ret)
		     ret))))
    (append (rec (match-sets match) (cdr (match-sets match)) nil)
	    (rec (reverse (match-sets match)) (cdr (reverse (match-sets match))) nil))))
(defun match->set-set-tw (match)
  (iter (for ss in (match->set-set match))
	(collect (append ss (list (match->stat-tw match))))))

;; kiprobalni hogy a sulyba beszamitjuk, a settek szamat ->
;; 3 szett van akkor 1/3 hogy egy bizonyos set bekovetkezik... 

(defparameter +set-sets+  (iter (for s0 in +sets+)
				(appending (iter (for s1 in +sets+)
						 (collect (list s0 s1))))))

;; a-spectrum - if has a set which other sets very probable (has an association with)
;; b-spectrum - in which set.. not to use
;; +set-set--set+  -et  nemj
(defparameter +set--set+ (initialize-direct +sets+ 
					    +sets+ 
					    '(:a)
					    (iter (for m in (all-matches +db+))
						  (appending (append (match->set-set-tw m)
								     (match->set-set-tw (reverse-match m)))))))

(defmacro sets->spectrum (sets)
  (oncesyms (sets)
    (gensyms (sp s s0 s1)
      `(let ((,sp (/ 1.0 (list-length ,sets))))
	 (iter (for ,s in ,sets)
	       (reducing (.spectrum ,sp (direct-a->spectrum ,s +set--set+))
			 by (lambda (,s0 ,s1) (+spectrums ,s0 ,s1))))))))
(defmacro match->spectrum (pid match)
  `(sets->spectrum (sets-according ,pid ,match)))
(defun matches->spectrums (pid matches)
  (iter (for m in matches)
	(collect (match->spectrum pid m))))


(defun sets->spectrum0 (sets)
  (iter (for s in sets)
	(print (wavg-spectrums (list (direct-a->spectrum s +set--set+)
					(make-spectrum 14))
				  (list (/ 1.0 (list-length sets))
					(- 1.0 (/ 1.0 (list-length sets))))))
	(reducing (wavg-spectrums (list (direct-a->spectrum s +set--set+)
					(make-spectrum 14))
				  (list (/ 1.0 (list-length sets))
					(- 1.0 (/ 1.0 (list-length sets)))))
		  by (lambda (s0 s1) (*spectrums s0 s1)))))
	

(defun pwin (spectrum)
  (apply #'+ (subseq spectrum 0 (/ (list-length spectrum) 2))))

;; infered - * all
(defun matches->inf-spectrum (pid matches)
  (infer-spectrums (matches->spectrums pid matches)))
(defun matches->avg-spectrum (pid matches &optional (*norm 1.0))
  (norm-spectrum (avg-spectrums (matches->spectrums pid matches)) *norm))

;; distorted
(defmacro match->spectrum-wf (pid match weightf)
  (oncesyms (match)
    `(^spectrum (funcall ,weightf ,match) (match->spectrum ,pid ,match))))
(defun matches->spectrums-wf (pid matches weightf)
  (iter (for m in matches)
	(collect (match->spectrum-wf pid m weightf))))
(defun matches->inf-spectrum-wf (pid matches weightf)
  (infer-spectrums (matches->spectrums-wf pid matches weightf)))

;; weighted-average
(defun matches->wavg-spectrum (pid matches weightf)
  (wavg-spectrums (matches->spectrums pid matches)
		  (iter (for m in matches)
			(collect (funcall weightf m)))))

(defun spectrum->string (spectrum)
  (format nil "~&~{~{set: ~a  prob: ~a~%~}~}" (iter (for s in spectrum)
						     (for n in +sets+)
						     (collect (list n s)))))

  
;; gathering for inference
(defun 2match->daysbetween (m0 m1)
  (abs (- (match-date m0) (match-date m1)))) 
(defun 3match->daysbetween (m0 m1 m2)
  (max (2match->daysbetween m0 m1) (2match->daysbetween m0 m2) (2match->daysbetween m1 m2)))
(defun 3match->samesurface? (m0 m1 m2)
  (and (equal (match-surface m0) (match-surface m1))
       (equal (match-surface m1) (match-surface m2))))
  
(defun gather-triangles (maxdays db)
  (labels ((rec2 (i0 i1 is2 ret)
	     (if is2
		 (let ((0-1 (matches-between i0 i1 db))
		       (0-2 (matches-between i0 (car is2) db))
		       (1-2 (matches-between i1 (car is2) db)))
		   (if (and 0-1 0-2 1-2)
		       (rec2 i0 
			     i1 
			     (cdr is2) 
			     (append ret (iter (for (0m 1m 2m) in (combinatorics:combinations-from-groups 0-1 0-2 1-2))
					       (if (and (3match->samesurface? 0m 1m 2m)
							(< (3match->daysbetween 0m 1m 2m) maxdays))
						   (collect (list 0m 1m 2m))))))
		       (rec2 i0 i1 (cdr is2) ret)))
		   ret)))

    (labels ((rec1 (i0 is1 ret)
	       (if (cdr is1)
		   (rec1 i0 (cdr is1) (rec2 i0 (car is1) (cdr is1) ret))
		   ret)))

      (labels ((rec0 (is0 ret)
		 (if (cdr is0)
		     (rec0 (cdr is0) (rec1 (car is0) (cdr is0) ret))
		     ret)))
	
	(rec0 (all-playerids db) nil)))))
	       
(defun 3m->possible-3ms (3m) 
  (let ((vars (append 3m (mapcar #'reverse-match 3m))))
    (iter (for m0 in vars)
	  (appending (iter (for m1 in vars)
			   (if (and (= (match-loser m0) (match-winner m1))
				    (not (= (match-winner m0) (match-loser m1))))
			       (appending (iter (for m2 in vars)
						(if (and (= (match-winner m2) (match-loser m1))
							 (= (match-loser m2) (match-winner m0)))
						    (collect (list m0 m1 m2)))))))))))

(defparameter +stat-infer-twf+ (make-tail-f 0.125))  
;; a-b b-c c-a  -t kovetnek a 3m-nek beallitott meccsek
(defun 3m->sssw (3m)
  (let* ((ab (nth 0 3m))
	 (bc (nth 1 3m))
	 (ca (nth 2 3m))
	 (w (funcall +stat-infer-twf+ (3match->daysbetween ab bc ca))))

    (iter (for s0 in +sets+)
	  (for sb1 in (reverse-sets (match-sets ab)))
	  (appending (iter (for s1 in +sets+)
			   (for sb2 in (match-sets bc))
			   (appending (iter (for s2 in +sets+)
					    (for sa5 in (reverse-sets (match-sets ca)))
					    (collect (list (list sb1 sb2) sa5 w)))))))))

;; egy max date-t adni -> pl 30 -> utana lefele haladva a dat
;; valamilyen self-filterhez hasonloan ki kell evolvalni mik azok a paramok, amelyek "kvazi" jol adjak vissza
(defparameter +t45+ (gather-triangles 45 +db+))
(defparameter +set-set--infsets+ (initialize-direct +set-sets+ 
						    +sets+ 
						    `((:a ,(list-length +sets+)))
						    (iter (for 3m in +t45+)
							  (appending (iter (for 3mp in (3m->possible-3ms 3m))
									   (appending (3m->sssw 3mp)))))))
 
;; utanuk normat kene belole vonni? - szoval erdekes, hogy ilyen magas-hajmereszto ertekeket kapnak meg

;; - fontos, hogy 1.0 -ra normalt spektrumot kapjanak meg
;; az a gond, hogy a norma nagyon lekicsinyli a kulonbsegeket :O - valahogy mashogy kene.. wavg az nem felt. jo.. :O
(defun bspectrums->infer (ba-spectrum bc-spectrum)
  (infernorm-spectrums (iter (for bap in ba-spectrum)
			 (for bas in +sets+)
			 (appending (iter (for bcp in bc-spectrum)
					  (for bcs in +sets+)
					  (collect (.spectrum (* bap bcp) 
							      (direct-a->spectrum (list bas bcs) +set-set--infsets+))))))))


(defun bsets->infer (ba-sets bc-sets)
  (bspectrums->infer (norm-spectrum (sets->spectrum ba-sets)) (norm-spectrum (sets->spectrum bc-sets))))
(defun bsets->infer (ba-sets bc-sets)
  (infer-spectrums (iter (for c in (combinatorics:combinations-from-groups ba-sets bc-sets))
			 (collect (direct-a->spectrum c +set-set--infsets+)))))


;; ez nem direkt -> egy atlagolt spektrumot kell visszakapnunk.. 
;; (defparameter +set-set--infer-mult+ (initialize-direct +set-sets+ 

;; test
;; (setf p3m (list (make-match :winner 0 :loser 1 :sets '((6 4) (6 3)))
;; 		(make-match :winner 1 :loser 2 :sets '((6 4) (6 3)))
;; 		(make-match :winner 0 :loser 2 :sets '((6 2) (6 1))))) 