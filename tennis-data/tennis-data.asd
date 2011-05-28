;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "TENNIS-DATA-ASD"	
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "TENNIS-DATA-ASD")

(defsystem tennis-data
  :components ((:file "defpackage")
	       (:file "odds-lib" :depends-on ("defpackage"))
	       (:file "dif-loader" :depends-on ("defpackage"))
	       (:file "match-lib" :depends-on ("defpackage"))
	       (:file "tennisdb" :depends-on ("defpackage" "odds-lib" "dif-loader" "match-lib")))

  :depends-on ("masd" "idhash"))







               
	       


