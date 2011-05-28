(in-package #:match-predict)

(defstruct-export
    (defstruct penv
      date
      surface
      db
      db-all
      pms
      exch
      os))

(defun initialize-penv (date surface-demand pms os db)
  (make-penv :date date
	     :surface surface-demand
	     :db (copy-tennisdb-date db :min-date (- date (pms-lookback pms)))
	     :db-all db
	     :pms pms
	     :exch (make-hash-table  :test #'equal)
	     :os os))
	       