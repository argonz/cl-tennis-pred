(in-package "TENNISDB")

(defun date? (str)
  (= (count #\/ str) 2))
(defun date-proxy? (str)
  (equal "###" str))
;; a very primitve and lame - for the spider
(defmacro translate-date (year month day)
  `(float (/ (encode-universal-time 0 0 0 ,day ,month ,year) ,(* 60 60 24))))

(defun count-days (d-m-y)
  (translate-date 
   (read-from-string (caddr d-m-y))
   (read-from-string (car d-m-y))
   (read-from-string (cadr d-m-y))))
   
(defun collect-a-data (line stream)
  (cond ((member line '("TABLE" "TUPLES" "DATA" "VECTORS"))
	 line)

	;; if number
	((equal (elt line 0) #\0)
	 (read-line stream nil)
	 (if (date? (subseq line 2))
	     (count-days (split-by-character (subseq line 2) #\/)) ;!!!! FOR THE TENNIS
	     (read-from-string (subseq line 2))))

	;; if string
	((equal (elt line 0) #\1)
	 (read-from-string (read-line stream nil)))

	;; if special
	((equal (elt line 0) #\-) 	;because there isn't any other variation
	 (progn
	   (read-line stream nil)
	   'bot))))
           
(defun translate-to-keyword (string)
  (cond ((equal string "Hard") :hard)
	((equal string "Clay") :clay)
	((equal string "Grass") :grass)
	((equal string "Carpet") :carpet)
	((equal string "Indoor") :indoor)
	((equal string "Outdoor") :outdoor)))
	
(defun load-from-dif-a-table (dif-with-path)
  (with-open-file (input-stream dif-with-path)
    (loop for line1 = (read-line input-stream nil) then (read-line input-stream nil)
       while line1
	 collect
	 (collect-a-data line1 input-stream))))

(defun format-row (row)
  (list :place (nth 1 row)
	:date (nth 3 row)
	:door (translate-to-keyword (nth 5 row))
	:surface (translate-to-keyword (nth 6 row))
	:winner (nth 9 row)
	:looser (nth 10 row)
	:sets (delete-if (lambda (l) (or (member "" l :test #'equal) (member " " l :test #'equal)))
			 (grouping-by-period (subseq row 15 25) 2))
	:odds (delete-if (lambda (l) (or (member "" l :test #'equal) (member " " l :test #'equal)))
			 (grouping-by-period (subseq row 28 38) 2))))

				
;; (formatted-load "/z/prog/lisp/tennis-pred/data/2008.dif") 
(defun formatted-load (path)
  (loop for m in (butlast (cddr (split-by-element (load-from-dif-a-table path) 'bot)))
       collect
       (format-row m)))

(defun collect-player-names-from-dif-match-list (ml)
  (mapcar (lambda (l) (list :name l)) 
	  (remove-duplicates (apply 'append (loop for e in ml collect (list (getf e :winner) (getf e :looser))))
			     :test #'equal)))

;; (formatted-load-players-matches "/z/prog/lisp/tennis-pred/data/2008.dif") 
;; ha nincs date akkor kipotolja az elozo + 1 el, megcsinalni
(defun formatted-load-players-matches (path)
  (let ((l (formatted-load path)))
    (list :matches l 
	  :players (collect-player-names-from-dif-match-list l))))
	    