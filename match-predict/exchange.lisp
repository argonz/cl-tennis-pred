(in-package #:match-predict)

(defun match->exch-tw (match penv)
  (funcall (pms-exch-match-timeweight-f (penv-pms penv)) (- (penv-date penv) (match-date match))))

;; atlagos-spectrum kene most :O
;; the ratio of m0 / m1
;; (defun matches->s/s (pid matches0 matches1)
;;   (/spectrums (norm-spectrum (matches->wavg-spectrum pid matches0 #'match->exch-tw)) 
;; 	      (norm-spectrum (matches->wavg-spectrum pid matches1 #'match->exch-tw)))) 

;; ;; ehash - playerid - type - (stype0 stype1)
;; (defun pid-surfaces->s/s (playerid surfaces ehash db)
;;   (gethash-or-make surfaces 
;; 		   (gethash-or-make playerid ehash (make-hash-table :test #'equal)) 
;; 		   (matches->s/s playerid
;; 				 (filter-matches-and (playerid->matches playerid db) :surface (car surfaces))
;; 				 (filter-matches-and (playerid->matches playerid db) :surface (cadr surfaces)))))

;; exchange-t meg letesztelni

;; (defun matches->wp (pid ms penv)
;;   (iter (for m in (direct-matches pid ms))
;; 	(collect (sets->wp (match-sets m) (penv-os penv)) into vs)
;; 	(collect (match->exch-tw m penv) into ws)
;; 	(finally (return (wavg vs ws)))))
(defun matches->wp (pid ms penv)
  (iter (for m in (direct-matches pid ms))
	(collect (car (odds->prob (match-odds-avg m))) into vs)
	(collect (match->exch-tw m penv) into ws)
	(finally (return (wavg vs ws)))))


(defun matches->w/w (pid matches0 matches1 penv)
  (if (and matches0 matches1)
      (/ (matches->wp pid matches0 penv)
	 (matches->wp pid matches1 penv))
      1.0))


(defun pid-surfaces->w/w (playerid surfaces penv)
  (gethash-or-make surfaces  
		   (gethash-or-make playerid (penv-exch penv) (make-hash-table :test #'equal))
		   (matches->w/w playerid
				 (filter-matches-and (playerid->matches playerid (penv-db-all penv)) 
						     :surface (car surfaces))
				 (filter-matches-and (playerid->matches playerid (penv-db-all penv)) 
						     :surface (cadr surfaces))
				 penv)))

 
