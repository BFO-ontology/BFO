(defstruct (bfo (:print-function print-bfo))
  terms
  class-tree
  2-prop-tree
  3-prop-tree
  uris
  )



(defun print-bfo (bfo stream level)
  (format t "#<bfo ~a classes, ~a properties>"
	  (count-if 'symbolp (cdr (bfo-terms bfo)) :key 'car)
	  (* 2 (count-if 'consp (cdr (bfo-terms bfo)) :key 'car))))

(defun read-bfo2-reference-spec ()
  (with-open-file (f "bfo:src;tools;bfo2-reference.lisp")
    (let ((terms (read f))
	  (class-tree (read f))
	  (2-prop-tree (read f))
	  (3-prop-tree (read f))
	  (uris (eval-uri-reader-macro (read f))))
      (make-bfo :terms terms :class-tree class-tree :2-prop-tree 2-prop-tree :3-prop-tree 3-prop-tree :uris uris))))

(defvar *bfo2* (load-time-value (read-bfo2-reference-spec)))

(defun active-iris ()
  (let ((names
	  (loop for (name) in (cdr (bfo-terms *bfo2*))
	     when (symbolp name) collect name
	     when (consp name) collect (first name) and collect (second name))))
    (loop for name in names 
       for found = (assoc name (cdr (bfo-uris *bfo2*)))
       collect (list (or (third found)
			 `(hey! ,name))
		     (first found)
		     ))))

(defun tree-every (tree fn)
  (cond ((atom tree)
	 (funcall fn tree))
	(t (every (lambda(el) (tree-every el fn)) tree))))

(defun tree-replace (tree replacement-fn)
  (cond ((atom tree)
	 (funcall replacement-fn tree))
	(t (mapcar (lambda(el) (tree-replace el replacement-fn)) tree))))

  
(defun filter-schulz-axioms ()
  (let ((in-use (active-iris *bfo2*)))
    (with-open-file (i "bfo:src;tools;raw-schulz-axioms.lisp")
      (with-open-file (o "bfo:src;tools;filtered-schulz-axioms.lisp" :direction :output :if-does-not-exist :create :if-exists :supersede)
	(loop with auto
	   for ax = (eval-uri-reader-macro (read i nil :eof))
	   until (eq ax :eof)
	   if (tree-every ax (lambda(el) (or (not (uri-p el)) (assoc el in-use) (eq el !owl:Thing))))
	   collect (tree-replace ax (lambda(el) (or (second (assoc el in-use)) el)))
	   into ok
	   else collect (tree-replace ax (lambda (el) (or (car (find el (cdr (bfo-uris *bfo2*)) :key 'third)) el)))
	   into notok
	   finally
	   (let ((*print-case* :downcase))
	     (loop for ax in ok
		if (or (and (eq (first ax) 'sub-class-of) (symbolp (second ax)) (symbolp (third ax)))
		       (and (eq (first ax) 'sub-object-property-of) (symbolp (second ax)) (symbolp (third ax)))
		       (and (eq (first ax) 'inverse-object-properties) (symbolp (second ax)) (symbolp (third ax)))
		       (and (eq (first ax) 'disjoint-classes))
		       (and (eq (first ax) 'equivalent-classes) (symbolp (second ax)) (consp (third ax)) (eq (first (third ax)) 'object-union-of)))
		do (push ax auto)
		else do
		  (print ax o) (terpri o))
	     (terpri o) (terpri o)
	     (format o ";; The following axioms are generated directly from structures in bfo2-reference.lisp~%~%")
	     (loop for ax in auto do (format o ";;~a~%~%" ax))
	     (format o ";; The following axioms use URIs not in the current spec~%~%")
	     (loop for ax in notok do (format o ";;~a~%~%" ax)))
	   (return (list ok notok)))))))

  