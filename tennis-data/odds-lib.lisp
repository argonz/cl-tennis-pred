(in-package "TENNISDB")

;; odds must be (list o1 o2)
(defun odds->rat (odds)
  (/ (1- (cadr odds))
     (1- (car odds))))			;backward -> that's the good ratio
(defmacro odds->prob (odds)
  (let ((p (gensym)))
    `(let ((,p (rat->prob (odds->rat ,odds))))
       (list ,p (- 1 ,p)))))
(defmacro reverse-odds (odds)
  `(list (cadr ,odds) (car ,odds)))
(defun min-max-from-odds (odds-list)
  (list (min-list (mapcar #'car odds-list))
	(max-list (mapcar #'cadr odds-list))))
(defun average-of-odds (odds-list)
  (list (average (mapcar #'car odds-list))
	(average (mapcar #'cadr odds-list))))


