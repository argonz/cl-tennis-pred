(in-package #:match-predict)
	 
(defun m->ded-tw (match penv)
  (funcall (pms-deduct-match-timeweight-f (penv-pms penv)) (- (penv-date penv) (match-date match))))

;; NODE - RATIO BETWEEN
(defun set-node (i snet)
  (insert-snnode i 
		 1.0 
		 (lambda (fs) (iter (for (v w) in (mapcar #'funcall fs))
				       (collect v into vs)
				       (collect w into ws)
				       (finally (return (wavg vs ws)))))
		 nil 
		 snet))

;; (defun m->rat (m penv)
;;   (* (prob->rat (sets->wp (match-sets m) (penv-os penv)))
;;      (wavg (list (pid-surfaces->w/w (match-winner m) (list (penv-surface penv) (match-surface m)) penv)
;; 		 (/ 1.0 (pid-surfaces->w/w (match-looser m) (list (penv-surface penv) (match-surface m)) penv)))
;; 	   (list 0.5 0.5))))
;; az utso valtozat
(defun m->rat (m penv)
  (* (odds->rat (match-odds-avg m))
     (wavg (list (pid-surfaces->w/w (match-winner m) (list (penv-surface penv) (match-surface m)) penv)
		 (/ 1.0 (pid-surfaces->w/w (match-looser m) (list (penv-surface penv) (match-surface m)) penv)))
	   (list 0.5 0.5))))
      

(defun set-evidences (ms snet penv)
  (let ((i (car (matches->participants ms)))
	(o (cadr (matches->participants ms))))
    (let ((i/o-w (iter (for m in ms)
		       (collect (m->rat m penv) into vs)
		       (collect (m->ded-tw m penv) into ws)
		       (finally (return (wavg-wsum vs ws))))))
      (let ((in (repr->snnode i snet))
	    (on (repr->snnode o snet)))
	(insert-stress i 
		       (lambda () (let ((iv (snnode-value in))
					(ov (snnode-value on)))
				    (list (wavg (list (* (car i/o-w) ov) iv) ;milyennek kene lennie in-nek on ism.
						(list 0.5 0.5))
					  (cadr i/o-w))))
		       snet)))))


  
;; PREDICTION
(defun initialize-snet-for-predict (pid0 pid1 penv)
  (let ((pids (pids-connected pid0 pid1 (pms-depth (penv-pms penv)) (penv-db penv))))
    (if pids
	(let ((mss (pids->matches pids (penv-db penv)))
	      (snet (initialize-snet)))

	  (iter (for i in pids)
		(set-node i snet))

	  (iter (for ms in mss)
		(set-evidences ms snet penv)
		(set-evidences (mapcar #'reverse-match ms) snet penv))
	  snet))))
	 
;; netlink -> winprob -> ratio-net -> prediction
(defun predict (pid0 pid1 penv)
  (let ((sn0 (initialize-snet-for-predict pid0 pid1 penv)))
    (if sn0
	(progn 
	  (simulate-snet (pms-nstep (penv-pms penv)) sn0)
	  (let ((r0 (/ (snnode-value (repr->snnode pid0 sn0))
		       (snnode-value (repr->snnode pid1 sn0)))))
	    (rat->prob r0))))))

    			   
			     