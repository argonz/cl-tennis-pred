(defpackage #:infer-net
  (:use #:cl
	#:masd
	#:iterate
	#:weight-lib
	#:idhash))

(in-package #:infer-net)

(defstruct node
  id
  iocons				;input output connections - directed ...
  value
  weight)

(defstruct-export
    (defstruct inet
      nodes
      inferf
      id->repr
      repr->id)) 				;always n0 n1 -> n2 -s value
	
(defmacro repr->id (repr inet)
  `(gethash ,repr (inet-repr->id ,inet)))
(defmacro id->repr (id inet)
  `(gethash ,id (inet-id->repr ,inet)))

(defun insert-node-preiotr (repr iocons val weight inet)
  (progn (idhash-insert-set-id (make-node :iocons iocons :value val :weight weight) 
			       node-id 
			       (inet-nodes inet))
	 (let ((id (idhash-lastid (inet-nodes inet))))
	   (setf (id->repr id inet) repr)
	   (setf (repr->id repr inet) id))))
(defun translate-iocons (node inet)
  (setf (node-iocons node) (list (iter (for ic in (car (node-iocons node)))
				       (collect (repr->id ic inet)))
				 (iter (for oc in (cadr (node-iocons node)))
				       (collect (repr->id oc inet))))))
(defun insert-nodes (riovw-list inet)
  (iter (for (r io v w) in riovw-list)
	(insert-node-preiotr r io v w inet))
  (iter (for n in (idhash-elements (inet-nodes inet)))
	(translate-iocons n inet)))

(defun initialize-inet (inferf &optional riovw-list)
  (let ((inet (make-inet :nodes (initialize-idhash)
			 :inferf inferf
			 :id->repr (make-hash-table)
			 :repr->id (make-hash-table :test #'equal))))
    (insert-nodes riovw-list inet)
    inet))
    

;; ROSSZ EGY stress-netet kell csinalni -> definialni nodeok es a koztuk levo erot -> es ezt frissiteni
;; utana a trukkok, hogy bovitem (aztan ujabb halot csinalni rola

;; (defun infer (inet)



  
;; (setf n (initialize-inet nil '((10 ((1) (2)) 5 1) (11 ((1) (2)) 6 1) (1 ((10) nil) 4 2) (2 (nil nil) 0 0))))