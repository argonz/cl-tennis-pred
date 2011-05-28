;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "TRAINING-ASD"	
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "TRAINING-ASD")

(defsystem training
  :components ((:file "defpackage")
	       (:file "training-set" :depends-on ("defpackage"))
;	       (:file "bench" :depends-on ("defpackage"))
; 	       (:file "evo-circus" :depends-on ("defpackage"))
	       )
  
  :depends-on ("masd" "idhash" "graph-evo" "tennis-data" "match-predict"))







               
	       


