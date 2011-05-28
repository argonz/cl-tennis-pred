;; This is a minimal script to set up the asdf system to my standard library.
(load #p"/home/mate/prog/lisp/load.lisp")
(setf asdf:*central-registry*
      '(*default-pathname-defaults*
	#p"/usr/local/lib/slime/"
	#p"/home/mate/prog/lisp/msd/"
	#p"/home/mate/prog/lisp/msd/id-hash/"
	#p"/home/mate/prog/lisp/evo/graph-evo/"
	#p"/home/mate/prog/lisp/tennis-pred/"
	#p"/home/mate/prog/lisp/tennis-pred/tennis-data/"
	#p"/home/mate/prog/lisp/tennis-pred/match-predict/"
	#p"/home/mate/prog/lisp/tennis-pred/betting/"
	#p"/home/mate/prog/lisp/tennis-pred/training/"))


(in-package "CL-USER")
(asdf:operate 'asdf:load-op 'tennis)
(use-package #:masd)

	 

