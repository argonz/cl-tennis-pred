(in-package #:training)

(defstruct-export 
    (defstruct training-case
      pid0
      pid1
      odds
      date
      match-conditions
      db
      winner
      winner-odds))

(defun choose-matches (nchoose matches)
  (let ((len (list-length matches)))
    (labels ((rec (n ret)
	       (if (zerop n)
		   ret
		   (let ((i (random len)))
		     (if (member i ret)
			 (rec n ret)
			 (rec (1- n) (cons i ret)))))))
      (mapcar (lambda (i) (nth i matches)) (rec nchoose nil)))))

(defmacro softer-odd (odd p)
  `(+ 1 (* (- ,odd 1) ,p)))
(defun softer-odds (odds p)
  (list (softer-odd (car odds) p) (softer-odd (cadr odds) p)))
(defun initialize-training-case (match db betsoft)
  (let ((c (< (random 1.0) 0.5)))
    (make-training-case :pid0 (if c (match-winner match) (match-looser match))
			:pid1 (if c (match-looser match) (match-winner match))
			:odds (softer-odds (if c 
					       (match-odds-avg match) 
					       (reverse (match-odds-avg match)))
					   betsoft) ;mert amit en kapok az nem lesz ilyen jo :)
			:date (match-date match)
			:match-conditions (list :surface (match-surface match)
						:door (match-door match))
			:db (copy-tennisdb-date db :max-date (match-date match))
			:winner (match-winner match)
			:winner-odds (* (car (match-odds-avg match)) betsoft))))
  
;; training-series is a list of training case-s
(defun make-training-series (nchoose min-date max-date betsoft db)
  (loop for m in (choose-matches nchoose (remove-if-not #'match-odds-avg
							(filter-matches-and (all-matches db) :min-date min-date :max-date max-date)))
       collect
       (initialize-training-case m db betsoft)))

(defun make-training-set (series-desc-list betsoft db) ;series-desc (n-match min-date max-date)
  (loop for d in series-desc-list
       collect
       (apply #'make-training-series (append d (list betsoft db)))))


;; annual-training-set a real test
(defun make-chronological-training-series (min-date max-date betsoft db)
  (sort (lambda (c0 c1) (< (training-case-date c0) (training-case-date c1)))
	(loop for m in (filter-matches-and (all-matches db) :min-date min-date :max-date max-date)
	   collect
	     (initialize-training-case m db betsoft))))


;; evaluates
(defun evaluate-bet (pid bet case)
  (if (= pid (training-case-winner case))
      (* (- (training-case-winner-odds case) 1) bet)
      (- bet)))