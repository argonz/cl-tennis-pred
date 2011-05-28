(in-package #:match-predict)

(defstruct-export
    (defstruct pms
      ;; parameters for ratio net
      depth
      nstep
      lookback

      ;; functions of prediction
      deduct-match-timeweight-f
      exch-match-timeweight-f))


(defun initialize-pms (dmatch-tw-p exch-tw-p)
  (make-pms :depth 4
	    :nstep 8
	    :lookback 150

	    :deduct-match-timeweight-f (make-tail-f dmatch-tw-p)
	    :exch-match-timeweight-f (make-tail-f exch-tw-p)))

