(in-package #:match-predict)

;; USE
;; gather players for the playerids
;; gather matchlinks according to this

(defun pids-connected (pid0 pid1 depth db)
  (labels ((rec (id d ret)
	     (if (= id pid1)
		 (cons id ret)
		 
		 (if (zerop d)
		     nil
		     (remove-duplicates 
		      (apply #'append
			     (iter (for o in (set-difference (opponents id (playerid->matches id db)) 
							     ret))
				   (collect (rec o 
						 (1- d) 
						 (cons id ret))))))))))

    (rec pid0 depth nil)))
(defun pids->matches (pids db)
  (labels ((rec0 (id ids)
	     (if ids
		 (let ((ms (direct-matches id (matches-between id (car ids) db))))
		   (if ms
		       (cons ms (rec0 id (cdr ids)))
		       (rec0 id (cdr ids))))
		 nil)))
    (labels ((rec1 (ids)
	       (if (cdr ids) 
		   (append (rec0 (car ids) (cdr ids)) (rec1 (cdr ids)))
		   nil)))

      (rec1 pids))))
(defun matches->participants (ms)
  (list (match-winner (car ms)) (match-looser (car ms))))

		  
 
;; printing
(defun print-playerids (playerids)
  (format t "~&playerids:~{ ~a ~}" playerids)) 
