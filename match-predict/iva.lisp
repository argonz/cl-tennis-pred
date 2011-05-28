(in-package #:match-predict)
(defstruct iva
  sourceid
  val
  am)

(defun filter-ivas (filterids ivas)
  (remove-if (lambda (iva) (member (iva-sourceid iva) filterids :test #'equal)) ivas))
(defun suma (ivas)
  (iter (for iva in ivas)
	(sum (iva-am iva))))
(defun sumv (ivas)
  (iter (for iva in ivas)
	(sum (iva-val iva))))
(defun ivas-wavg (ivas)
  (iter (for iva in ivas)
	(collect (iva-val iva) into vs)
	(collect (iva-am iva) into as)
	(finally (return (wavg vs as)))))
(defun ivas-wavg-wsum (ivas)
  (iter (for iva in ivas)
	(collect (iva-val iva) into vs)
	(collect (iva-am iva) into as)
	(finally (return (wavg-wsum vs as)))))
(defun unite-ivas (ivas)
  (let ((va (ivas-wavg-wsum ivas)))
    (make-iva :sourceid (iva-sourceid (car ivas))
	      :val (car va)
	      :am (cadr va))))
(defun join-ivas (ivas0 ivas1)
  (append ivas0 ivas1))
(defun rearrange-ivas (ivas)
  (iter (for g in (group ivas (lambda (i0 i1) (equal (iva-sourceid i0) (iva-sourceid i1)))))
	(collect (unite-ivas g))))
(defun reamount-ivas (ivas newsum)
  (let ((oldsum (suma ivas)))
    (iter (for iva in ivas)
	  (collect (make-iva :sourceid (iva-sourceid iva)
			     :val (iva-val iva)
			     :am (* newsum (/ (iva-am iva) oldsum)))))))
(defun revalue-ivas (ivas newvalue)
  (iter (for iva in ivas)
	(collect (make-iva :sourceid (iva-sourceid iva)
			   :val newvalue
			   :am (iva-am iva)))))