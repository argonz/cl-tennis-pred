(in-package #:match-predict)
	 
(defun m->tw (match penv)
  (funcall (pms-deduct-match-timeweight-f (penv-pms penv)) (- (penv-date penv) (match-date match))))

;; NODE - RATIO BETWEEN
(defun set-node (ii snet)
  (insert-snnode ii 
		 nil 
		 (lambda (ivafs) (rearrange-ivas (apply #'append (mapcar #'funcall ivafs))))
		 nil
		 snet))

;; EVIDENCES
(defun m->iva (m penv)
  (make-iva :sourceid (list (match-winner m) (match-looser m))
	    :val (sets->wp (match-sets m) (penv-os penv))
	    :am (m->tw m (penv-date penv))))
(defun ms->iva (ms penv)
  (unite-ivas (iter (for m in ms)
		    (collect (m->iva m penv)))))

;; a stressnet sajat id-jen tarolja a dolgokat - de a hatasokat a representtel jeloljuk!!!!
(defun insert-evidence-iva (iva snet)
  (let ((n (repr->snnode (iva-sourceid iva) snet)))
    (setf (snnode-value n) (cons iva (snnode-value n)))
    (insert-stress (iva-sourceid iva) (lambda () (list iva)) snet)))
(defun ms->set-evidences (ms snet penv)
  (insert-evidence-iva (ms->iva ms penv) snet)
  (insert-evidence-iva (ms->iva (mapcar #'reverse-match ms) penv) snet))

;; INFERENCE STRESSES
;; ii -re akarunk kovetkeztetni
(defun set-ii-infers (ii pids snet penv)
  (iter (for i in (remove-if (lambda (p) (member p ii :test #'equal)) pids))
	(let ((bn0 (repr->snnode (list i (car ii)) snet))
	      (bn1 (repr->snnode (list i (cadr ii)) snet)))
	  
	  (insert-stress ii  
			 (lambda () 
			   (let ((b0ivas (filter-ivas (list ii (reverse ii)) (snnode-value bn0)))
				 (b1ivas (filter-ivas (list ii (reverse ii)) (snnode-value bn1))))
			     (if (and b0ivas b1ivas)
				 (let ((b0vw (ivas-wavg-wsum b0ivas))
				       (b1vw (ivas-wavg-wsum b1ivas)))
				   (let ((nv (wp-wp->inferred-wp (car b0vw) (car b1vw) (penv-os penv))) ;optimazalhato let
					 (na (* 0.5 (min (cadr b0vw) (cadr b1vw)))))
				     (reamount-ivas (revalue-ivas (join-ivas b0ivas b1ivas) nv) na))))))
			 snet))))


;; PREDICTION
(defun initialize-snet-for-predict (pid0 pid1 penv)
  (let ((pids (pids-connected pid0 pid1 (pms-depth (penv-pms penv)) (penv-db penv))))
    (if pids
	(let ((mss (pids->matches pids (penv-db penv)))
	      (snet (initialize-snet)))
	  (iter (for ii in (variations pids 2))
		(set-node ii snet))
	  (iter (for ms in mss)
		(ms->set-evidences ms snet penv))
	  (iter (for ii in (variations pids 2))
		(set-ii-infers ii pids snet penv))
	  snet))))
	 
;; netlink -> winprob -> ratio-net -> prediction
(defun predict (pid0 pid1 penv)
  (let ((sn0 (initialize-snet-for-predict pid0 pid1 penv))
	(sn1 (initialize-snet-for-predict pid1 pid0 penv)))

    (if (and sn0 sn1)
	(progn 
	  (simulate-snet (pms-nstep (penv-pms penv)) sn0)
	  (simulate-snet (pms-nstep (penv-pms penv)) sn1)

	  (let ((v0 (ivas-wavg (snnode-value (repr->snnode (list pid0 pid1) sn0))))
		(v1 (- 1.0 (ivas-wavg (snnode-value (repr->snnode (list pid1 pid0) sn1))))))
	    (print v0)
	    (print v1)
	    (average (list v0 v1)))))))

    			   
			     