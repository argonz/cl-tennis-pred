(in-package "TENNISDB")

(defparameter +winning-sets+ '((6 0) (6 1) (6 2) (6 3) (6 4) (7 5) (7 6)))
(defparameter +sets+ (append +winning-sets+ (reverse (mapcar #'reverse +winning-sets+))))
(defparameter +3wsr+ '((2 0) (2 1)))
(defparameter +3set-ratios+ (append +3wsr+ (reverse (mapcar #'reverse +3wsr+))))
(defparameter +5wsr+ '((3 0) (3 1) (3 2)))
(defparameter +5set-ratios+ (append +5wsr+ (reverse (mapcar #'reverse +5wsr+))))
(defparameter +set-ratios+ (append +3set-ratios+ +5set-ratios+))
(defparameter +game-results+ (list :swin :twin :tloose :sloose))

(defstruct-export 
    (defstruct match
      id
      winner
      looser
      sets

      odds-ext				;odds szelso ertek
      odds-avg				;odds kozep

      place
      date
      door
      surface))

(defmacro opponent (playerid match)
  `(if (= (match-winner ,match) ,playerid)
       (match-looser ,match)
       (match-winner ,match))) 
(defmacro opponents (playerid matches)
  (let ((m (gensym)))
    `(mapcar (lambda (,m) (opponent ,playerid ,m)) ,matches)))
(defmacro nr-of-sets (match)
  `(list-length (match-sets ,match)))
(defmacro count-set (set match)
  `(count ,set (match-sets ,match) :test #'equal))
(defmacro reverse-sets (sets)
  (oncesyms (sets)
    (gensyms (s)
      `(iter (for ,s in ,sets) (collect (reverse ,s))))))
(defmacro sets-according (playerid match)
  `(if (= ,playerid (match-winner ,match))
       (match-sets ,match)
       (reverse-sets (match-sets ,match))))
(defmacro odds-ext-according (playerid match)
  `(if (= ,playerid (match-winner ,match))
       (match-odds-ext ,match)
       (reverse (match-odds-ext ,match))))
(defmacro odds-avg-according (playerid match)
  `(if (= ,playerid (match-winner ,match))
       (match-odds-avg ,match)
       (reverse (match-odds-avg ,match))))
(defun reverse-match (match)
  (make-match :winner (match-looser match) ;ez gond!!! megvaltoztatni
	      :looser (match-winner match) 
	      :sets (reverse-sets (match-sets match))
	      :odds-avg (odds-avg-according (match-looser match) match)
	      :odds-ext (odds-ext-according (match-looser match) match)
	      :surface (match-surface match) 
	      :date (match-date match)
	      :door (match-door match)))
(defun direct-matches (wpid ms)
  (iter (for m in ms)
	(collect (if (= (match-winner m) wpid)
		     m
		     (reverse-match m)))))
(defun match-according (match playerid)
  (if (= (match-winner match) playerid)
      match
      (reverse-match match)))

(defmacro won? (playerid match)
  `(= ,playerid (match-winner ,match)))
(defmacro lost? (playerid match)
  `(= ,playerid (match-looser ,match)))
(defmacro set-won? (set)
  `(> (car ,set) (cadr ,set)))
(defmacro sets->wonlost (sets)
  (gensyms (s)
    `(iterate (for ,s in ,sets) (collect (set-won? ,s)))))
(defmacro match->wonlost (match)
  `(sets->wonlost (match-sets ,match)))
(defmacro sets->setratio (sets)
  (gensyms (wl)
    `(let ((,wl (sets->wonlost ,sets)))
       (list (count t ,wl) (count nil ,wl)))))
(defmacro match->setratio (match)
  `(sets->setratio (match-sets ,match)))

(defun 3set-game-sets? (sets)
  (or (= (list-length sets) 2)
      (and (= (list-length sets) 3)
	   (let ((wl (sets->wonlost sets)))
	     (and (member nil wl) (member t wl))))))
(defmacro 3set-game? (match)
  `(3set-game-sets? (match-sets ,match)))
(defmacro sets-tight? (sets)
  (oncesyms (sets)
    `(if (3set-game-sets? ,sets)
	 (= (list-length ,sets) 3)
	 (= (list-length ,sets) 2))))
(defmacro tight? (match)
  `(sets-tight? (match-sets ,match)))

(defmacro sets-won? (sets)
  (gensyms (wl)
    `(let ((,wl (sets->setratio ,sets)))
       (> (car ,wl) (cadr ,wl)))))
(defun sets->game-result (sets)
  (if (sets-tight? sets)
      (if (sets-won? sets)
	  :twin
	  :tloose)
      (if (sets-won? sets)
	  :swin
	  :sloose)))
	  

;; preprocessing related
(defun complete-ws-ls (ws ls)
  (cond ((and (>= ws 7) (= ls 6)) (list 7 6))
	((and (>= ws 7) (<= ls 5)) (list 7 5))
	((and (= ws 6) (= ls 5)) (list 6 4))
	((and (<= ws 6) (<= ls 4)) (list 6 ls))))
(defun complete-set (set)
  (if (> (car set) (cadr set))
      (complete-ws-ls (car set) (cadr set))
      (complete-ws-ls (cadr set) (car set))))
(defun valid-sets? (sets)
  (if (every (lambda (s) (member s +sets+ :test #'equal)) sets)
      (if (3set-game-sets? sets)
	  (member 2 (sets->setratio sets))
	  (member 3 (sets->setratio sets)))))


;; FILTER
(defmacro valid-min-date? (min-date match)
  `(> (match-date ,match) ,min-date))
(defmacro valid-max-date? (max-date match)
  `(< (match-date ,match) ,max-date))
(defmacro participant? (playerid match)
  `(or (= ,playerid (match-winner ,match)) (= ,playerid (match-looser ,match))))
(defmacro between? (playerids match)
  `(and (member (match-winner ,match) ,playerids) (member (match-looser ,match) ,playerids)))
(defmacro equal-surface? (surface match)
  `(equal (match-surface ,match) ,surface))
(defmacro equal-door? (door match)
  `(equal (match-door ,match) ,door))
(defmacro oddspercent-between? (oddsperc match)
  (gensyms (p o)
    `(let ((,p (odds->probability (match-odds-avg ,match)))
	   (,o ,oddsperc))
       (or (< (car ,o) (car ,p) (cadr ,o))
	   (< (car ,o) (cadr ,p) (cadr ,o))))))
(defmacro valid-gametype? (gametype match)
  `(if (equal ,gametype :3set)
       (3set-game? ,match)
       (not (3set-game? ,match))))
(defmacro valid-nr-of-sets? (nset match)
  `(= (nr-of-sets ,match) ,nset))
(defmacro valid-set-ratio? (set-ratio match)
  (gensyms (sr)
    `(let ((,sr (match->setratio ,match)))
       (or (equal ,sr ,set-ratio) (equal (reverse ,sr) ,set-ratio)))))

(defun filter (matches type param)
  (remove-if-not (lambda (m) (cond ((equal type :surface) (equal-surface? param m))
				   ((equal type :participant) (participant? param m))
				   ((equal type :between) (between? param m))
				   ((equal type :odds-percent-between) (oddspercent-between? param m))
				   ((equal type :max-date) (valid-max-date? param m))
				   ((equal type :min-date) (valid-min-date? param m))
				   ((equal type :set-ratio) (valid-set-ratio? param m))
				   ((equal type :gametype) (valid-gametype? param m))
				   ((equal type :nr-of-sets) (valid-nr-of-sets? param m))
				   ((equal type :door) (equal-door? param m))))
		 matches))
(defun filter-matches-and (matches &rest filters)
  (let ((args (split filters '(:odds-percent-between :between :max-date :min-date :participant :surface :door :gametype :set-ratio :nr-of-sets) :test #'equal)))
    (reduce (lambda (ms0 arg) 
	      (reduce (lambda (ms1 p) (filter ms1 (car arg) p))
		      (cons ms0 (cdr arg))))
	    (cons matches args))))



;; CATEGORIZATION
(defmacro categorize (match-list match-access-f)
  (let ((l1 (gensym))
	(l2 (gensym)))
    `(group ,match-list (lambda (,l1 ,l2) (equal (,match-access-f ,l1) (,match-access-f ,l2))))))

(defmacro labels-categorized (categorized-match-list category)
  (let ((c (gensym)))
    `(apply #'append
	    (loop for ,c in ,categorized-match-list
	       collect
		 (list (,(cond ((equal category :surface) 'match-surface)
			       ((equal category :door) 'match-door)
			       ((equal category :place) 'match-place))
			 (car ,c))
		       ,c)))))

(defmacro categorize-matches (matches category)
  `(labels-categorized (categorize ,matches ,(cond ((equal category :surface) 'match-surface)
						   ((equal category :door) 'match-door)
						   ((equal category :place) 'match-place)))
		       ,category))



;; FILTER DEPRECATED 

;; (defmacro filter (matches type param)
;;   (let ((m (gensym)))
;;     `(remove-if-not (lambda (,m) (,(cond ((equal type :max-date) 'valid-max-date?)
;; 					 ((equal type :min-date) 'valid-min-date?)
;; 					 ((equal type :participant) 'participant?)
;; 					 ((equal type :between) 'between?)
;; 					 ((equal type :odds-percent-between) 'oddspercent-between?)
;; 					 ((equal type :gametype) 'valid-gametype?)
;; 					 ((equal type :nr-of-sets) 'valid-nr-of-sets?)
;; 					 ((equal type :surface) 'equal-surface?)
;; 					 ((equal type :door) 'equal-door?))
;; 				   ,param ,m))
;; 		    ,matches)))

;; ;; ennek kene egy funkcio valtozatanak is lennie!!
;; (defmacro filter-matches-and (matches &rest filters)
;;   (let ((args (apply #'append (mapcar (lambda (l) (list (car l) (cdr l)))
;; 				      (split filters '(:max-date :min-date :participant :odds-percent-between :nr-of-sets :surface :gametype :nr-of-sets :door) :test #'equal)))))

;;     (reduce (lambda (l1 l2) (list 'filter l1 (car l2) (cadr l2)))
;; 	    (apply #'append 
;; 		   `(,matches)
;; 		   (loop for p in (getf args :between) collect
;; 			`(:between ,p))
;; 		   (loop for p in (getf args :participant) collect
;; 			`(:participant ,p))
;; 		   (loop for p in (getf args :max-date) collect
;; 			`(:max-date ,p))
;; 		   (loop for p in (getf args :min-date) collect
;; 			`(:min-date ,p))
;; 		   (loop for p in (getf args :odds-percent-between) collect
;; 			`(:odds-percent-between ,p))
;; 		   (loop for p in (getf args :nr-of-sets) collect
;; 			`(:nr-of-sets ,p))
;; 		   (loop for p in (getf args :gametype) collect
;; 			`(:gametype ,p))
;; 		   (loop for p in (getf args :surface) collect
;; 			`(:surface ,p))
;; 		   (loop for p in (getf args :door) collect
;; 			`(:door ,p))
;; 		   ))))

;; (defmacro filter-matches-or (matches &rest filters)
;;   (let ((args (apply #'append (mapcar (lambda (l) (list (car l) (cdr l)))
;; 				      (split filters '(:max-date :min-date :between :participant :surface :door) :test #'equal)))))
;;     `(remove-duplicates (append ,@(append (loop for p in (getf args :between) collect
;; 					       `(filter ,matches :between ,p))
;; 					  (loop for p in (getf args :participant ) collect
;; 					       `(filter ,matches :participant ,p))
;; 					  (loop for p in (getf args :max-date) collect
;; 					       `(filter ,matches :max-date ,p))
;; 					  (loop for p in (getf args :min-date) collect
;; 					       `(filter ,matches :min-date ,p))
;; 					  (loop for p in (getf args :surface) collect
;; 					       `(filter ,matches :surface ,p))
;; 					  (loop for p in (getf args :door) collect
;; 					       `(filter ,matches :door ,p))))
;; 			:test #'equal)))
    
