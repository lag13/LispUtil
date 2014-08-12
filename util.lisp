(defun n-rand-unique (n max &optional exclude) 
  "Returns a list of n random unique numbers in the range [0, max) which aren't in the list exclude."
  (let* ((possible-nums (set-difference (loop for i below max collect i) exclude))
	 (len (length possible-nums))
	 (arr (make-array len :initial-contents possible-nums))
	 (lower-bound (- len n)))
    (loop for i from (1- len) downto lower-bound
       do (rotatef (aref arr i) (aref arr (random (1+ i))))
       collect (aref arr i))))

(defun shuffle (seq)
  (loop for i from (1- (length seq)) downto 1
     for j = (random (1+ i))
     do (rotatef (elt seq i) (elt seq j)))
  seq)

(defun random-seq-elem (seq)
  "Returns a random element from a list."
  (elt seq (random (length seq))))

(defun range (start end)
  "Returns a list containing integers [start, end]"
  (loop for i from start to end collect i))

;; In all my graph algorithms I pretend that graphs are a-lists.
;; So an example of a graph would be something like this:
;; ((0 1 5) (1 0 2) (2 1) (3 4 5) (4 3 5) (5 3 4))
;; So node 0 is adjacent to nodes 1 and 5 etc...
(defun adj-nodes (node graph) 
  "Returns the list of nodes adjacent to node."
  (cdr (assoc node graph)))

(defun within-one (node1 node2 graph)
  "Returns true if node2 is within one move away from node1."
  (member node2 (adj-nodes node1 graph)))

(defun within-two (node1 node2)
  "Returns true if node2 is within two moves away from node1."
  (or (within-one node1 node2)
      (some (lambda (u)
	      (within-one u node2))
	    (adj-nodes node1))))

(defun remove-smallest (seq &optional key)
  "Removes the smallest item from a list, returning the item and modifying the list in place."
  (let* ((min (reduce #'min seq :key key))
	 (result (find min seq :key key)))
    (values (delete min seq :count 1 :key key) result)))
    
(defun findpl (prop val p-lists)
  "Returns a p-list from a list of p-lists that matches the property and value."
  (find val p-lists :key (lambda (pl) (getf pl prop))))

(defun shortest-path1 (source dest graph)
  "Returns a list which is the shortest path from source to dest. The weight of each edge is just 1."
  (do* ((min-queue (let ((dist (length graph)))
		     (mapcar (lambda (x) (list :vert x :pred nil :shortest-est dist))
			     (remove source (mapcar #'car graph))))
		   (sort min-queue #'< :key (lambda (x) (getf x :shortest-est))))
	(seen (list (list :vert source :pred nil :shortest-est 0))
	      (cons (pop min-queue) seen))
	(u (getf (car seen) :vert) (getf (car seen) :vert)))
       ((or (equal u dest) 
	    (null min-queue)) (do ((w (findpl :vert dest seen)
				      (findpl :vert (getf w :pred) seen))
				   (path nil (cons (getf w :vert) path)))
				  ((null w) path)))
    (let ((new-dist (1+ (getf (car seen) :shortest-est))))
      (mapc (lambda (v) 
	      (let ((v (findpl :vert v min-queue)))
		(when (< new-dist (getf v :shortest-est))
		  (setf (getf v :shortest-est) new-dist)
		  (setf (getf v :pred) u))))
	    (set-difference (adj-nodes u graph)
			    (mapcar (lambda (x) (getf x :vert)) seen))))))

;; Another version of the one above.
(defun shortest-path2 (source dest graph)
  "Returns a list which is the shortest path from source to dest. The weight of each edge is just 1."
  (let ((not-seen (let ((dist (length graph)))
		    (mapcar (lambda (x) (list :vert x :pred nil :shortest-est dist))
			    (remove source (mapcar #'car graph)))))
	(seen (list (list :vert source :pred nil :shortest-est 0))))
    (loop for u = (getf (car seen) :vert)
       for new-dist = (1+ (getf (car seen) :shortest-est))
       while (and (not (equal u dest)) not-seen)
       do 
	 (mapc (lambda (v) 
		 (let ((v (findpl :vert v not-seen)))
		   (when (< new-dist (getf v :shortest-est))
		     (setf (getf v :shortest-est) new-dist)
		     (setf (getf v :pred) u))))
	       (set-difference (adj-nodes u graph)
			       (mapcar (lambda (x) (getf x :vert)) seen)))
	 (multiple-value-bind (x y) (remove-smallest not-seen 
						     (lambda (x) (getf x :shortest-est)))
	   (setf not-seen x)
	   (push y seen))
       finally
	 (return (do ((w (findpl :vert dest seen)
			 (findpl :vert (getf w :pred) seen))
		      (path nil (cons (getf w :vert) path)))
		     ((null w) path))))))
	
(defun collect-leaves1 (tree)
  "Flattens a tree into just a list of its leaves."
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(T (append (flatten (car tree)) (flatten (cdr tree))))))

(defun collect-leaves2 (tree)
  "Same as above but probably more efficient"
  (let ((result nil))
    (labels ((f (tree) (cond ((null tree))
			     ((atom tree) (push tree result))
			     (T (f (car tree)) (f (cdr tree))))))
      (f tree)
      (nreverse result))))

(defun tree-height (tree)
  "Returns the height of a tree"
  (labels ((f (height tree) (cond ((null tree) height)
				  ((atom tree) height)
				  (T (max (f (1+ height) (car tree))
					  (f height (cdr tree)))))))
    (f 0 tree)))

(defmacro copy-part-struct ((&rest accessor-fns) struct1 struct2)
  "Copies part of a structure from struct1 and the rest from struct2"
  (let ((result (gensym)))
    `(let ((,result (copy-structure ,struct2)))
       ,@(loop for a in accessor-fns collect `(setf (,a ,result) (,a ,struct1)))
       ,result)))

(defun explode-seq (delimiter seq &optional remove-empty-subseqs-p)
  "Breaks up a sequence into a list of tokens based on the delimiter character."
  (loop for i = 0 then (1+ j)
     for j = (position delimiter seq :start i)
     collect (subseq seq i j) into tokens
     while j
     finally
       (return 
	 (if remove-empty-subseqs-p
	     (remove-if (lambda (x) (zerop (length x))) tokens)
	     tokens))))

