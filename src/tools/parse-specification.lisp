(defstruct (bfo (:print-function print-bfo))
  terms
  class-tree
  2-prop-tree
  3-prop-tree
  uris
  class2subclass
  2prop2subprop
  3prop2subprop
  terms-with-md-siblings
  term2annotation
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
      (let ((struct (make-bfo :terms terms :class-tree class-tree :2-prop-tree 2-prop-tree :3-prop-tree 3-prop-tree :uris uris)))
	(parse-bfo2-tree (second 2-prop-tree) #'(setf bfo-2prop2subprop) struct)
	(parse-bfo2-tree (second 3-prop-tree) #'(setf bfo-3prop2subprop) struct)
	(parse-bfo2-tree (second class-tree) #'(setf bfo-class2subclass) struct)
	struct
	))))


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

;; walk through lines. State is stack of terms forming ancestry, with top the term to accumulate siblings.
;; If at same level of of stack as previous emit pair: parent, sub
;; If reducing indent, pop stack difference in level
;; If increasing indent, can only be 1 more. First emit parent, term, then push term on ancestor stack

(defun parse-bfo2-tree (spec result-setter bfo-struct)
  (let ((parent-stack nil)
	(stream (make-string-input-stream spec))
	(parent2child (make-hash-table :test 'equal)))
    (read-line stream)
    (flet ((emit (child parent)
					;(let ((emit (list child parent))) (print-db emit))
	     (and child parent
		  (push child (gethash parent parent2child)))
	     (unless (and child (gethash child parent2child))
	       (setf (gethash child parent2child) nil))))
      (loop with stack = nil and indent = 0 and lastchild = nil
	 for line = (read-line stream nil :eof)
	 until (eq line :eof)
	 for ((depth name flags)) = (all-matches line "(-*)([_0-9a-z-]+)(\\(([a-z]+)\\)){0,1}" 1 2 4)
	 for new-indent = (length depth)
	 for name-symbol = (intern (string-upcase name))
	 do
;;;					(print-db line depth name flags indent new-indent name-symbol lastchild stack)
	 (when (find #\d flags) (push name-symbol (bfo-terms-with-md-siblings bfo-struct)))
	 (cond ;((null stack)		; first line
		;(setq stack (list name-symbol)))
	       ((= indent new-indent) ;; add another sibling and replace lastchild
		(emit name-symbol (first stack))
		(setq lastchild name-symbol))
	       ((= (1+ indent) new-indent) ;; go one level deeper
		(and lastchild (push lastchild stack))
		(emit name-symbol (first stack))
		(setq indent new-indent)
		(setq lastchild name-symbol)))
	 (when (< new-indent indent) ; going up a level
;;;	   (print-db (- new-indent indent))
	   (loop repeat (- indent new-indent) do  (pop stack))
	   (emit name-symbol (first stack))
	   (setq lastchild name-symbol)
	   (setq indent new-indent)))
      (funcall result-setter parent2child bfo-struct)))
  bfo-struct)
  
(defvar *bfo2* (load-time-value (read-bfo2-reference-spec)))
