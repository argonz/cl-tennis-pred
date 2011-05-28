(in-package "TENNISDB")
     
;; (setf d (build-tennisdb "/home/mate/prog/lisp/tennis-pred/data/atp2008.dif"))  
;; 			"/home/mate/prog/lisp/tennis-pred/data/atp2007.dif"

(defparameter +current-date+ (let ((l (multiple-value-list (decode-universal-time (get-universal-time)))))
			       (translate-date (nth 5 l) (nth 4 l) (nth 3 l))))

(defstruct-export 
    (defstruct player
      id
      name))

(defstruct-export 
    (defstruct db
      (players (initialize-idhash))
      (matches (initialize-idhash))
      (playerid->matchids (make-hash-table))
      (playerid->playername (make-hash-table))
      (playername->playerid (make-hash-table :test #'equal))))

;; ACCES FUNCTIONS
(defmacro id->player (playerid db)
  `(idhash-id->element ,playerid (db-players ,db)))
(defmacro playerid->playername (playerid db)
  `(gethash ,playerid (db-playerid->playername ,db)))
(defmacro playername->playerid (playername db)
  `(gethash ,playername (db-playername->playerid ,db)))
(defmacro playername->player (playername db)
  `(id->player (playername->playerid ,playername ,db) ,db))
(defmacro all-players (db)
  `(idhash-elements (db-players ,db)))
(defmacro all-playerids (db)
  `(hash-keys (db-playerid->playername ,db)))

(defmacro id->match (matchid db)
  `(idhash-id->element ,matchid (db-matches ,db))) 
(defmacro ids->matches (matchids db)
  (let ((i (gensym)))
    `(mapcar (lambda (,i) (id->match ,i ,db)) ,matchids)))
(defmacro playerid->matchids (playerid db)
  `(gethash ,playerid (db-playerid->matchids ,db))) 
(defmacro playerid->matches (playerid db)
  `(ids->matches (playerid->matchids ,playerid ,db) ,db))
(defmacro all-matches (db)
  `(idhash-elements (db-matches ,db)))
(defmacro matches-between (playerid0 playerid1 db)
  `(filter-matches-and (playerid->matches ,playerid0 ,db) :participant ,playerid1))
(defmacro has-played? (playerid0 playerid1 db)
  `(intersection (playerid->matchids ,playerid0 ,db) (playerid->matchids ,playerid1 ,db)))

(defmacro count-players (db)
  `(idhash-count (db-players ,db)))
(defmacro count-matches (db)
  `(idhash-count (db-matches ,db)))

;; INSERTS
;; player
(defmacro insert-playerid->playername (playerid playername db)
  `(setf (playerid->playername ,playerid ,db) ,playername))
(defmacro insert-playername->playerid (playername playerid db)
  `(setf (playername->playerid ,playername ,db) ,playerid))
(defmacro insert-player (player db)
  `(if (not (playername->playerid (player-name ,player) ,db))
       (progn (idhash-insert-set-id ,player player-id (db-players ,db))
	      (insert-playerid->playername (player-id ,player) (player-name ,player) ,db)
	      (insert-playername->playerid (player-name ,player) (player-id ,player) ,db))))
(defmacro insert-initialize-player (name db)
  (let ((p (gensym)))
    `(let ((,p (make-player :name ,name)))
       (insert-player ,p ,db))))

;; match
(defmacro insert-playerid->matchid (playerid matchid db)
  `(setf (playerid->matchids ,playerid ,db)
	 (cons ,matchid (playerid->matchids ,playerid ,db))))
(defmacro insert-match (match db)
  `(progn (idhash-insert-set-id ,match match-id (db-matches ,db))
	  (insert-playerid->matchid (match-winner ,match) (match-id ,match) ,db)     ;megnezni majd id-hasht
	  (insert-playerid->matchid (match-looser ,match) (match-id ,match) ,db)))
(defmacro insert-initialize-match (winner looser sets odds-ext odds-avg place date door surface db)
  (let ((m (gensym)))
    `(let ((,m (make-match :winner ,winner
			   :looser ,looser
			   :sets ,sets
			   :odds-ext ,odds-ext
			   :odds-avg ,odds-avg
			   :place ,place
			   :date ,date
			   :door ,door
			   :surface ,surface)))
       (insert-match ,m ,db))))
;; (defmacro remove-match (matchid db)
;;   (let ((m (gensym)))
;;     `(let ((m (id->match ,matchid ,db)))
;;        (idhash-remove ,matchid (db-matches ,db))
;;        (setf (


;; INITIALIZES DB
(defun match-params-valid? (params)
  (and (valid-sets? (getf params :sets))
       (getf params :odds)
       (getf params :winner)
       (getf params :looser)
       (getf params :surface)))

(defun initialize-db-from-dif (&rest paths)
  (let ((db (make-db)))
    (loop for p in paths 
	 do
	 (let ((loaded-dif (formatted-load-players-matches p)))

	   (loop for p in (getf loaded-dif :players)
		do
		(insert-initialize-player (getf p :name) db))
	   
	   ;; JELENLEG KIHAGYOK MINDEN MECCSET AMELYIKNEK NINCSEN ODDSA - EZ ROSSZ EZEN VALTOZTATNI!!!
	   (loop for p in (getf loaded-dif :matches)
		do
		(if (match-params-valid? p)
		    (insert-initialize-match (playername->playerid (getf p :winner) db)
					     (playername->playerid (getf p :looser) db)
					     (getf p :sets) 
					     (if (getf p :odds) (min-max-from-odds (getf p :odds)))
					     (if (getf p :odds) (average-of-odds (getf p :odds)))
					     (getf p :place)
					     (getf p :date)
					     (getf p :door)
					     (getf p :surface)
					     db)))))
    db))

	
(defun copy-tennisdb (db)
  (make-db :players (copy-idhash (db-players db))
	   :matches (copy-idhash (db-matches db))
	   :playerid->playername (copy-hash-table (db-playerid->playername db))
	   :playername->playerid (copy-hash-table (db-playername->playerid db))
	   :playerid->matchids (copy-hash-table (db-playerid->matchids db))))

(defmacro copy-tennisdb-date (db &key min-date max-date)
  (gensyms (d p m)
    `(let ((,d (make-db)))
      (loop for ,p in (all-players ,db)
	 do (insert-initialize-player (player-name ,p) ,d))
      (loop for ,m in (filter-matches-and (all-matches ,db) ,@(cond ((and min-date max-date)
								     (list :min-date min-date :max-date max-date))
								    (min-date
								     (list :min-date min-date))
								    (max-date
								     (list :max-date max-date))))
	 do (insert-initialize-match (match-winner ,m)
				     (match-looser ,m)
				     (match-sets ,m)
				     (match-odds-ext ,m)
				     (match-odds-avg ,m) 
				     (match-place ,m) 
				     (match-date ,m)
				     (match-door ,m)
				     (match-surface ,m)
				     ,d))
      ,d)))
	 


;; proba
;; (setf d (initialize-db-from-dif "/home/mate/prog/lisp/tennis-pred/data/atp2008.dif"))
