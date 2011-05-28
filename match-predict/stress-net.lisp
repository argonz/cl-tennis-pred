(defpackage #:stress-net
  (:use #:cl
	#:masd
	#:iterate
	#:weight-lib
	#:idhash)
  (:export #:initialize-snet
	   #:repr->snnode
	   #:snnodes
	   #:insert-snnode
	   #:insert-stress
	   #:simulate-snet))
(in-package #:stress-net)

(defstruct-export
    (defstruct snnode
      id
      value
      affectf				;f (stressval-f  .. ) -> new-snnodevalue
      stresses))                	;f -> stressval
(defstruct-export
    (defstruct snet
      snnodes
      repr->id
      id->repr))
	
(defmacro id->snnode (id snet)
  `(idhash-id->element ,id (snet-snnodes ,snet)))
(defmacro snnodes (snet)
  `(idhash-elements (snet-snnodes ,snet)))
(defmacro repr->id (repr snet)
  `(gethash ,repr (snet-repr->id ,snet)))
(defmacro id->repr (id snet)
  `(gethash ,id (snet-id->repr ,snet)))
(defmacro repr->snnode (repr snet)
  (oncesyms (snet)
    `(id->snnode (repr->id ,repr ,snet) ,snet)))
(defmacro repr->value (repr snet)	;for request 
  (oncesyms (snet)
    `(snnode-value (id->snnode (repr->id ,repr ,snet) ,snet))))

(defun insert-snnode (repr value affectf stresses snet)
  (idhash-insert-set-id (make-snnode :value value
				   :affectf affectf
				   :stresses stresses)
			snnode-id 
			(snet-snnodes snet))
  (let ((id (idhash-lastid (snet-snnodes snet))))
    (setf (id->repr id snet) repr)
    (setf (repr->id repr snet) id)))
(defun insert-snnodes (snnodes-rvass snet)
  (iter (for (r v a s) in snnodes-rvass)
	(insert-snnode r v a s snet)))
 
(defun insert-stress (effected-repr effectf snet)
  (let ((n (repr->snnode effected-repr snet)))
    (setf (snnode-stresses n) (cons effectf (snnode-stresses n)))))
(defun insert-stresses (stress-res snet)
  (iter (for (r e) in stress-res)
	(insert-stress r e snet)))

(defun initialize-snet (&key snnodes-rvass stress-res)
  (let ((snet (make-snet :snnodes (initialize-idhash)
			 :repr->id (make-hash-table :test #'equal)
			 :id->repr (make-hash-table))))
    (if snnodes-rvass (insert-snnodes snnodes-rvass snet))
    (if stress-res (insert-stresses stress-res snet))
    snet))

(defun affect-snnodes (snet)
  (iter (for n in (snnodes snet))
	(collect (list (snnode-id n) (funcall (snnode-affectf n) (snnode-stresses n))) 
	  into ivs)
	(finally (iter (for (i v) in ivs)
		       (setf (snnode-value (id->snnode i snet)) v)))))
(defun simulate-snet (n snet)
  (iter (repeat n)
	(affect-snnodes snet)))
