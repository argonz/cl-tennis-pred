;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- 
(defpackage "MATCH-PREDICT-ASD"	
  (:use 
   "COMMON-LISP"
   "ASDF"))

(in-package "MATCH-PREDICT-ASD")

(defsystem match-predict
  :components ((:file "curve-functions")
	       (:file "stress-net" :depends-on ("curve-functions"))

	       (:file "defpackage" :depends-on ("stress-net"))
	       (:file "bayes-statistics" :depends-on ("defpackage"))
	       (:file "parameters" :depends-on ("defpackage"))
	       (:file "penv" :depends-on ("defpackage"))
	       (:file "bayes-results" :depends-on ("defpackage" "bayes-statistics"))
	       (:file "netlink" :depends-on ("defpackage"))
	       (:file "match-predict" :depends-on ("defpackage"))
	       
;;  	       (:file "condition-exchange" :depends-on ("parameters" "penv"))

	       )
  
  :depends-on ("masd" "iterate" "idhash" "tennis-data" "graph-evo"))







               
	       


