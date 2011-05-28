;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "BETTING-ASD"	
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "BETTING-ASD")

(defsystem betting
  :components ((:file "betting-strategy"))
  :depends-on ("masd" "tennis-data" "match-predict"))







               
	       


