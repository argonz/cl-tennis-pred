(in-package #:match-predict)

(defparameter +db+ (initialize-db-from-dif "/home/mate/prog/lisp/tennis-pred/data/atp2008.dif" 
					   "/home/mate/prog/lisp/tennis-pred/data/atp2007.dif" 
					   "/home/mate/prog/lisp/tennis-pred/data/atp2006.dif"))
(defparameter +stat-twf+ (make-tail-f 0.0005))
(defparameter +stat-mbwf+ (make-complementer-tail-f 0.8))
(defun empired-matches (db)
  (labels ((rec0 (i is)
	     (if is
		 (let ((ms (matches-between i (car is) db)))
		   (append (iter (for m in (cdr (sort ms (lambda (m0 m1) (< (match-date m0) (match-date m1))))))
				 (for i first 1 then (1+ i))
				 (collect (list m (* (funcall +stat-mbwf+ i)
						     (funcall +stat-twf+ (- +current-date+ (match-date m)))))))
			   (rec0 i (cdr is)))))))

    (labels ((rec1 (is)
	       (if is
		   (append (rec0 (car is) (cdr is)) (rec1 (cdr is))))))

      (rec1 (all-playerids db)))))
(defparameter +ems+ (empired-matches +db+))
(defun dist-exp (p0 p1) 
  (abs (- (log (prob->rat (+ p0 1e-5))) (log (prob->rat (+ p1 1e-5))))))
;; (defun mws->ocs (mws)
;;   (mapcar (lambda (mw) (list (rat->prob (exp (car mw))) (cadr mw)))
;; 	  (sort (ica 23
;; 		     (apply #'append (mapcar (lambda (mw) (if (match-odds-avg (car mw))
;; 							      (let ((r (odds->rat (match-odds-avg (car mw)))))
;; 								(list (list (log r) (cadr mw))
;; 								      (list (log (/ 1 r)) (cadr mw))))))
					     
;; 					     mws))
;; 		     :metricf (lambda (pw0 pw1) (abs (- (car pw0) (car pw1))))
;; 		     :joinf (lambda (pw0 pw1) (list (wavg (list (car pw0) (car pw1))
;; 							  (list (cadr pw0) (cadr pw1)))
;; 						    (+ (cadr pw0) (cadr pw1)))))
;; 		(lambda (pw0 pw1) (< (car pw0) (car pw1))))))
;; (defparameter ocs (mws->ocs +ems+))
  
;; length - 21
(defparameter +ocs+ '(0.0 0.015 0.0375 0.078 0.127 0.1875 0.29 0.3285
		      0.5
		      0.6715 0.71 0.8125 0.873 0.922 0.9625 0.985 1.0))
		       

;; SULLYOZNI LEHETNE MENNYIRE KOZEPEN VAN A CLASS
(defun find-odds-class (prob)
  (labels ((rec (ocs)
	     (if (<= (car ocs) prob (cadr ocs))
		 (list (list (car ocs) (dist-exp (car ocs) prob))
		       (list (cadr ocs) (dist-exp (cadr ocs) prob)))
		 (rec (cdr ocs)))))
    (let* ((r (rec +ocs+))
	   (s (+ (cadar r) (cadadr r))))
      (list (list (caar r) (/ (cadar r) s))
	    (list (caadr r) (/ (cadadr r) s))))))
(defun match->odds-class (match)
  (find-odds-class (car (odds->prob (match-odds-avg match)))))

;; set-set
(defun match->set-set (match)
  (labels ((rec (s0 s1 ret)
	     (if s1
		 (rec s0 (cdr s1) (cons (list (car s0) (car s1)) ret))
		 (if (cdr s0)
		     (rec (cdr s0) (cddr s0) ret)
		     ret))))
    (append (rec (match-sets match) (cdr (match-sets match)) nil)
	    (rec (reverse (match-sets match)) (cdr (reverse (match-sets match))) nil))))

;; bele kell rakni, hogy az odds-a es az elozmenye mennyire esik egybe!!!
(defun mw->set-set-oc (mw)
  (let* ((m (car mw))
	 (ocs (match->odds-class m))
	 (sss (match->set-set m))
	 (w (* (cadr mw) (/ 1.0 (list-length sss)))))

    (iterate (for ss in sss)
	     (appending (list (list ss (caar ocs) (* (cadar ocs) w))
			      (list ss (caadr ocs) (* (cadadr ocs) w)))))))
(defun sets->set-set->spectrum (sets ssodir)
  (infer-spectrums (iter (for ss in (comb sets 2))
			 (collect (direct-a->spectrum ss ssodir)))))

;; sr-set
(defun sets->gr-set (sets)
  (let ((gr (sets->game-result sets)))
    (iter (for s in sets)
	  (collect (list gr s)))))
(defun mw->gr-set-oc (mw)
  (let* ((m (car mw))
	 (ocs (match->odds-class m))
	 (grs (sets->gr-set (match-sets m)))
	 (w (* (cadr mw) (/ 1.0 (list-length grs)))))
    (iterate (for gs in grs)
	     (appending (list (list gs (caar ocs) (* (cadar ocs) w))
			      (list gs (caadr ocs) (* (cadadr ocs) w)))))))
(defun sets->gr-set->spectrum (sets srsodir)
  (infer-spectrums (iter (for srs in (sets->gr-set sets))
			 (collect (direct-a->spectrum srs srsodir)))))

;; wl -> how big win percent the oc classes has
(defparameter +wl+ (list :won :lost))
(defun mw->oc-wl (mw)
  (let* ((m (car mw))
	 (w (cadr mw))
	 (ocs (match->odds-class m))
	 (wl? (if (or (equal (sets->game-result (match-sets m)) :swin)
		      (equal (sets->game-result (match-sets m)) :twin))
		  :won
		  :lost)))
    (list (list (caar ocs) wl? (* (cadar ocs) w))
	  (list (caadr ocs) wl? (* (cadadr ocs) w)))))
 
(defparameter +set-sets+ (iter (for s0 in +sets+)
			       (appending (iter (for s1 in +sets+)
						(collect (list s0 s1))))))
(defparameter +gr-sets+ (iter (for s in +sets+)
			      (appending (iter (for g in +game-results+)
					       (collect (list g s))))))
(defstruct ocstat
  ocs
  ss-oc
  grs-oc
  oc-wp
  ococ-oc-infer)
;; GAME RATIO-T KELL HASZNALNI -> megszamolni osszes nyert/vesztett es ebbol is csinalni v. 10 classt
(defun find-spectrum-value (class spectrum sclasses)
  (iter (for c in sclasses)
	(for v in spectrum)
	(if (equal class c)
	    (leave v))))


(defun sets->ocspectrum (sets os)		  
  (norm-spectrum (infer-spectrums (list (sets->set-set->spectrum sets (ocstat-ss-oc os))
					(sets->gr-set->spectrum sets (ocstat-grs-oc os))))))
(defun ocspectrum->wp (spectrum os)
  (iter	(for s in spectrum)
	(for oc in (ocstat-ocs os))
	(sum (* s (car (direct-a->spectrum oc (ocstat-oc-wp os)))))))
(defun sets->wp (sets os)
  (ocspectrum->wp (sets->ocspectrum sets os) os))
(defun ocspectrum->theoretic-wp (spectrum os)
  (iter (for s in spectrum)
	(for oc in (ocstat-ocs os))
	(sum (* s (average oc)))))
(defun sets->theoretic-wp (sets os)
  (ocspectrum->theoretic-wp (sets->ocspectrum sets os) os))



;; egy max date-t adni -> pl 30 -> utana lefele haladva a dat
;; valamilyen self-filterhez hasonloan ki kell evolvalni mik azok a paramok, amelyek "kvazi" jol adjak vissza

(defun initialize-ocstat (mws)
  (let ((mws (apply #'append (mapcar (lambda (mw) (if (and (3set-game? (car mw))
							   (match-odds-avg (car mw)))
						      (list mw
							    (list (reverse-match (car mw)) (cadr mw)))))
				     mws))))

    (make-ocstat :ocs +ocs+
		 :ss-oc (initialize-direct +set-sets+ +ocs+ '(:a) 
					   (apply #'append (mapcar (lambda (mw) (mw->set-set-oc mw)) mws)))
		 :grs-oc (initialize-direct +gr-sets+ +ocs+ '(:a)
					    (apply #'append (mapcar (lambda (mw) (mw->gr-set-oc mw)) mws)))
		 :oc-wp (initialize-direct +ocs+ +wl+ '(:a) 
					   (apply #'append (mapcar (lambda (mw) (mw->oc-wl mw)) mws))))))
(defparameter +os+ (initialize-ocstat +ems+))

;; ;; INFERENCE STAT
;; (defun 2match->daysbetween (m0 m1)
;;   (abs (- (match-date m0) (match-date m1)))) 
;; (defun 3match->daysbetween (m0 m1 m2)
;;   (max (2match->daysbetween m0 m1) (2match->daysbetween m0 m2) (2match->daysbetween m1 m2)))
;; (defun 3match->samesurface? (m0 m1 m2)
;;   (and (equal (match-surface m0) (match-surface m1))
;;        (equal (match-surface m1) (match-surface m2))))
  
;; (defun gather-triangles (maxdays db)
;;   (labels ((rec2 (i0 i1 is2 ret)
;; 	     (if is2
;; 		 (let ((0-1 (matches-between i0 i1 db))
;; 		       (0-2 (matches-between i0 (car is2) db))
;; 		       (1-2 (matches-between i1 (car is2) db)))
;; 		   (if (and 0-1 0-2 1-2)
;; 		       (rec2 i0 
;; 			     i1 
;; 			     (cdr is2) 
;; 			     (append ret (iter (for (0m 1m 2m) in (comb-from-groups 0-1 0-2 1-2))
;; 					       (if (and (3match->samesurface? 0m 1m 2m)
;; 							(< (3match->daysbetween 0m 1m 2m) maxdays))
;; 						   (collect (list 0m 1m 2m))))))
;; 		       (rec2 i0 i1 (cdr is2) ret)))
;; 		   ret)))

;;     (labels ((rec1 (i0 is1 ret)
;; 	       (if (cdr is1)
;; 		   (rec1 i0 (cdr is1) (rec2 i0 (car is1) (cdr is1) ret))
;; 		   ret)))

;;       (labels ((rec0 (is0 ret)
;; 		 (if (cdr is0)
;; 		     (rec0 (cdr is0) (rec1 (car is0) (cdr is0) ret))
;; 		     ret)))
	
;; 	(rec0 (all-playerids db) nil)))))
	       
;; (defun 3m->possible-3ms (3m) 
;;   (let ((vars (append 3m (mapcar #'reverse-match 3m))))
;;     (iter (for m0 in vars)
;; 	  (appending (iter (for m1 in vars)q
;; 			   (if (and (= (match-looser m0) (match-winner m1))
;; 				    (not (= (match-winner m0) (match-looser m1))))
;; 			       (appending (iter (for m2 in vars)
;; 						(if (and (= (match-winner m2) (match-looser m1))
;; 							 (= (match-looser m2) (match-winner m0)))
;; 						    (collect (list m0 m1 m2)))))))))))


;; (defparameter +stat-infer-twf+ (make-tail-f 0.085))
;; (defun 3m->oc-oc-oc (ab bc ca os)	;b0 b1 a2   az infer !!!
;;   (list (list (find-odds-class (sets->wp (reverse-sets (match-sets ab)) os))
;; 	      (find-odds-class (sets->wp (match-sets bc) os)))
;; 	(find-odds-class (sets->wp (reverse-sets (match-sets ca)) os))
;; 	1))


;; (defun initialize-infer-direct (3ms os)
;;   (initialize-direct (comb-from-groups +ocs+ +ocs+)
;; 		     +ocs+
;; 		     '(:a)
;; 		     (iter (for 3m in 3ms)
;; 			   (appending (iter (for 3mp in (3m->possible-3ms 3m))
;; 					    (collect (3m->oc-oc-oc (car 3mp) (cadr 3mp) (caddr 3mp) os)))))))


;; (defun complete-ocstat-with-3minfer (3ms ocstat)
;;   (setf (ocstat-ococ-oc-infer ocstat) (initialize-infer-direct 3ms ocstat))
;;   ocstat)

;; ;; ab bc ca
;; (defun oc-oc->inferred-oc (ba bc ocstat)
;;   (direct-a->spectrum (list ba bc) (ocstat-ococ-oc-infer ocstat)))
;; (defun oc-oc->inferred-wp (ba bc ocstat)
;;   (ocspectrum->wp (oc-oc->inferred-oc ba bc ocstat) ocstat))
;; (defun wp-wp->inferred-wp (wba wbc ocstat)
;;   (oc-oc->inferred-wp (find-odds-class wba) (find-odds-class wbc) ocstat))

;; (defparameter +t60+ (gather-triangles 60 +db+))
;; (defun initialize-complete-ocstat ()
;;   (complete-ocstat-with-3minfer  
;;    +t60+ 
;;    (initialize-ocstat-sf 2 (delete-if-not #'match-odds-avg 
;; 					  (filter-matches-and (all-matches +db+) :gametype :3set)))))

 
;; (defparameter +os+ (initialize-complete-ocstat))




			     
;; ;; (defun pids->abbccas (pids)
;; ;;   (iter (for p0 in pids)
;; ;; 	(appending (let ((ps0 (remove p0 pids)))
;; ;; 		     (iter (for p1 in ps0)
;; ;; 			   (appending (let ((ps1 (remove p1 ps0)))
;; ;; 					(iter (for p2 in ps1)
;; ;; 					      (collect (list (list p0 p1)
;; ;; 							     (list p1 p2)
;; ;; 							     (list p2 p0)))))))))))
;; ;; (defun abbcca->bba (abbcca)
;; ;;   (list (reverse (car abbcca))
;; ;; 	(cadr abbcca)
;; ;; 	(reverse (caddr abbcca)))) 
;; ;; (defun pids->bba (pids)
;; ;;   (iter (for a in (pids->abbccas pids))
;; ;; 	(collect (abbcca->bba a))))