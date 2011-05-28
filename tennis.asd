;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "TENNIS-ASD"	
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "TENNIS-ASD")

(defsystem tennis
  :components ((:file "bet"))
  :depends-on ("masd" "tennis-data" "match-predict" "training"))







               
	       


