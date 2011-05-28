(defpackage "RATIO-NET"
  (:use "COMMON-LISP"
	#:masd
	"PROBABILITY-LIB"
	"CURVE-FUNCTIONS"
	"IDHASH"
	"ITERATE"
	"WEIGHT-LIB") 
  (:export "INITIALIZE-NET"
	   "INSERT-INITIALIZE-NODE"
	   "INSERT-INITIALIZE-CONNECT"
	   "COMPUTE-NR-OF-ITERATES"
	   "SET-INFLUENCES-OF-CONNECTS"
	   "PRINT-NET"
	   "COPY-NET"
	   "COMPARE-REPRESENTS"))

(in-package "RATIO-NET")

;; USE
;; insert nodes 
;; than insert evidences between the nodes 

;; egy wrapperekkel teli mutatas
(defstruct node				;variable what we want to sample-out
  id
  absolute-value)

(defstruct connect
  id
  nodeid0 
  nodeid1
  relative-value
  weight)

(defstruct net
  nodes
  connects
  represent->nodeid
  nodeid->represent)


(defun initialize-net (&key test-for-represent)
  (make-net :nodes (initialize-idhash)
	    :connects (initialize-idhash)
	    :represent->nodeid (if test-for-represent 
				    (make-hash-table :test test-for-represent)
				    (make-hash-table))
	    :nodeid->represent (if test-for-represent 
				   (make-hash-table :test test-for-represent)
				   (make-hash-table))))

(defmacro id->node (id net)
  `(idhash-id->element ,id (net-nodes ,net)))
(defmacro all-nodes (net)
  `(idhash-elements (net-nodes ,net))) 
(defmacro represent->nodeid (represent net)
  `(gethash ,represent (net-represent->nodeid ,net)))
(defmacro nodeid->represent (nodeid net)
  `(gethash ,nodeid (net-nodeid->represent ,net)))
(defmacro represent->node (represent net)
  `(id->node (represent->nodeid ,represent ,net) ,net))

(defmacro insert-node (node net)
  `(idhash-insert-set-id ,node node-id (net-nodes ,net)))
(defmacro insert-initialize-node (represent net &optional (absolute-value 1.0))
  (gensyms (nodeid)
    `(progn
       (insert-node (make-node :absolute-value ,absolute-value) ,net)
       (let ((,nodeid (idhash-lastid (net-nodes ,net))))
	 (setf (gethash ,represent (net-represent->nodeid ,net)) ,nodeid)
	 (setf (gethash ,nodeid (net-nodeid->represent ,net)) ,represent)))))
   
(defmacro insert-connect (connect net)
  `(idhash-insert-set-id ,connect connect-id (net-connects ,net)))
(defmacro insert-initialize-connect-by-nodeids (nodeid0 nodeid1 relative-value weight net)
  `(insert-connect (make-connect :nodeid0 ,nodeid0
				 :nodeid1 ,nodeid1
				 :relative-value ,relative-value
				 :weight ,weight)
		   ,net))
(defmacro insert-initialize-connect (represent0 represent1 relative-value weight net)
  `(insert-initialize-connect-by-nodeids (represent->nodeid ,represent0 ,net)
					 (represent->nodeid ,represent1 ,net) 
					 ,relative-value
					 ,weight
					 ,net))

(defmacro id->connect (id net)
  `(idhash-id->element ,id (net-connects ,net)))
(defmacro all-connects (net)
  `(idhash-elements (net-connects ,net)))

(defmacro count-nodes (net)
  `(idhash-count (net-nodes ,net)))
(defmacro count-connects (net)
  `(idhash-count (net-connects ,net)))

(defmacro has-connection-to-node? (nodeid connect)
  `(or (= ,nodeid (connect-nodeid0 ,connect)) 
       (= ,nodeid (connect-nodeid1 ,connect))))
(defmacro has-connection-to-represent? (represent connect net)
  `(has-connection-to-node? (represent->nodeid ,represent ,net) ,connect))
(defmacro connects-nodes? (nodeid0 nodeid1 connect)
  `(and (has-connection-to-node? ,nodeid0 ,connect) (has-connection-to-node? ,nodeid1 ,connect)))
(defmacro connects-represents? (represent0 represent1 connect net)
  `(connects-nodes? (represent->nodeid ,represent0 ,net) (represent->nodeid ,represent1 ,net) ,connect))

(defmacro all-connects-between-nodes (nodeid0 nodeid1 net)
  (let ((c (gensym)))
    `(delete-if-not (lambda (,c) (connects-nodes? ,nodeid0 ,nodeid1 ,c)) (all-connects ,net))))
(defmacro all-connects-between-represents (represent0 represent1 net)
  `(all-connects-between-nodes (represent->nodeid ,represent0) (represent->nodeid ,represent1) ,net))

;; comparisons 
(defun compare-nodes (nv0 nv1)
  (/ nv0 (+ nv0 nv1)))
(defun compare-nodes-exp (nv0 nv1)
  (ratio->probability (exp (- nv0 nv1))))
(defun compare-represents (represent0 represent1 net)
  (if (and (represent->node represent0 net) (represent->node represent1 net))
      (compare-nodes-exp (node-absolute-value (represent->node represent0 net))
			 (node-absolute-value (represent->node represent1 net)))))

;; influence-functions out-of-the-box
;; always returns two values - new av0, new av1
;; rv always in the right direction!!! - nodeids insert in the proper order
(defun influence-ratios-exp (av0 av1 rv delta)
  (let* ((ad (- av0 av1))
	 (rd (log rv))
	 (newd (weighted-average (list ad rd) (list (- 1.0 delta) delta)))
	 (av (/ (- (+ av0 av1) newd) 2)))
    (values av (+ av newd))))

(defmacro new-value-part (value0 value1 ratio delta)
  `(weighted-average (list (/ ,value0 (+ ,value0 ,value1)) (/ ,ratio (1+ ,ratio)))
		     (list (- 1.0 ,delta) ,delta)))


(defmacro influence-of-connect (connect delta net)
  (gensyms (n0 n1 n0a n1a nsum p)
    `(let* ((,n0 (id->node (connect-nodeid0 ,connect) ,net))
	    (,n1 (id->node (connect-nodeid1 ,connect) ,net))
	    (,n0a (node-absolute-value ,n0))
	    (,n1a (node-absolute-value ,n1))
	    (,nsum (+ ,n0a ,n1a))
	    (,p (new-value-part ,n0a ,n1a (connect-relative-value ,connect) ,delta)))
       
       (setf (node-absolute-value ,n0) (* ,nsum ,p))
       (setf (node-absolute-value ,n1) (* ,nsum (- 1 ,p))))))

(defmacro influence-of-connect-exp (connect delta net)
  (gensyms (n0 n1 n0a n1a wd nbase)
    `(let* ((,n0 (id->node (connect-nodeid0 ,connect) ,net))
	    (,n1 (id->node (connect-nodeid1 ,connect) ,net))
	    (,n0a (node-absolute-value ,n0))
	    (,n1a (node-absolute-value ,n1))
	    (,wd (weighted-average (list (- ,n0a ,n1a) (log (connect-relative-value ,connect))) 
				   (list (- 1.0 ,delta) ,delta)))
	    (,nbase (/ (- (+ ,n0a ,n1a) ,wd) 2)))
       
       (setf (node-absolute-value ,n0) (+ ,nbase ,wd))
       (setf (node-absolute-value ,n1) ,nbase))))

      

;; ide egy function kell majd 
(defun make-random-choose-connect-f (net)
  (let* ((connects (all-connects net))
	 (weights (mapcar (lambda (e) (connect-weight e)) connects)))
    (random-weighted-func-tree connects weights)))
(defun compute-nr-of-iterates (n-connects power-param multip-param)
  (round (* (expt n-connects power-param) multip-param)))

;; majd tesztelni melyik a jobb
(defun set-influences-of-connects (iteration-pp iteration-mp delta-start delta-end net)
  (if (plusp (count-nodes net))
      (let* ((connect-choose-f (make-random-choose-connect-f net))
	     (ni (compute-nr-of-iterates (list-length (all-connects net)) iteration-pp iteration-mp))
	     (delta-f (make-tail-iterate-f delta-start delta-end ni)))
	
	(iter (for i from 0 below ni)
	      (for c first (funcall connect-choose-f) then (funcall connect-choose-f))
	      (for d first (funcall delta-f i) then (funcall delta-f i))
	      (influence-of-connect-exp c d net))))
  net)

(defun copy-net (net)
  (let ((cnet (initialize-net :test-for-represent (hash-table-test (net-represent->nodeid net)))))
    (loop for n in (all-nodes net) do (insert-initialize-node (nodeid->represent (node-id n) net) 
							      cnet 
							      (node-absolute-value n)))
    (loop for c in (all-connects net) do (insert-initialize-connect (nodeid->represent (connect-nodeid0 c) net)
								    (nodeid->represent (connect-nodeid1 c) net)
								    (connect-relative-value c)  
								    (connect-weight c) 
								    cnet))
    cnet))

;; printing
(defun print-net (net)
  (format nil 
	  "~&nodes:~%~{~{represents: ~a - node-id: ~a - absolute-value: ~a~%~}~}~%~%connects~%~{~{connect-id: ~a - connects: ~a - weight: ~a - relative-value: ~a~%~}~}" 
	  (loop for n in (all-nodes net) collect (list (nodeid->represent (node-id n) net)
						       (node-id n)
						       (node-absolute-value n)))
	  (loop for c in (all-connects net) collect (list (connect-id c) 
							  (list (nodeid->represent (connect-nodeid0 c) net)
								(nodeid->represent (connect-nodeid1 c) net))
							  (connect-weight c)
							  (connect-relative-value c)))))

