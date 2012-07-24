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
  anntag2term
  term2clif
  term2annotation
  fol-expressions
  ontology-annotations
  )



(defun print-bfo (bfo stream level)
  (format stream "#<bfo ~a classes, ~a properties>"
	  (count-if 'symbolp (cdr (bfo-terms bfo)) :key 'car)
	  (* 2 (count-if 'consp (cdr (bfo-terms bfo)) :key 'car))))

(defun read-bfo2-reference-spec ()
  (with-open-file (f "bfo:src;ontology;owl-group;specification;bfo2-reference.lisp")
    (with-open-file (g "bfo:src;ontology;owl-group;specification;bfo2-uris.lisp")
      (let ((terms (read f))
	    (class-tree (read f))
	    (2-prop-tree (read f))
	    (3-prop-tree (read f))
	    (tag2term (read f))
	    (clif-term-exceptions (read f))
	    (uris (eval-uri-reader-macro (read g)))
	    (fol (read-fol-clif)))
	(let* ((tag2term-filled
		(loop for (ann term) in (cdr tag2term)
		   collect (list (intern (string-upcase (#"replaceAll" ann "[ _]" "-")))
				 (or term (intern (string-upcase (#"replaceAll" ann "[ _]" "-")))))))
	       (struct (make-bfo :terms terms :class-tree class-tree :2-prop-tree 2-prop-tree
				   :3-prop-tree 3-prop-tree :uris uris :anntag2term tag2term-filled :fol-expressions fol))
	       (term2clif (loop for (tag term) in tag2term-filled
			     for exception = (second (assoc tag (cdr clif-term-exceptions) :test 'equalp))
			     collect (list term (or exception
						    (let ((tentative (apply 'concatenate 'string 
									    (mapcar 'string-capitalize
										    (split-at-char (string tag) #\-) ))))
						      (if (eq :unary (bfo-term-arity term struct))
							  tentative
							  (progn
							    (setf (char tentative  0) (char-downcase (char tentative 0)))
							    (if (eq :ternary (bfo-term-arity term struct))
								(concatenate 'string tentative "At")
								tentative
							  )))))))))
	    (setf (bfo-term2clif struct) term2clif)
	    (parse-bfo2-tree (second 2-prop-tree) #'(setf bfo-2prop2subprop) struct)
	    (parse-bfo2-tree (second 3-prop-tree) #'(setf bfo-3prop2subprop) struct)
	    (parse-bfo2-tree (second class-tree) #'(setf bfo-class2subclass) struct)
	    struct
	    )))))


(defun active-iris (bfo2)
  (let ((names
	  (loop for (name) in (cdr (bfo-terms bfo2))
	     when (symbolp name) collect name
	     when (consp name) collect (first name) and collect (second name))))
    (loop for name in names 
       for found = (assoc name (cdr (bfo-uris bfo2)))
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
  
;(defvar *bfo2* (load-time-value (read-bfo2-reference-spec)))

(defun read-fol-clif ()
  (with-open-file (f "bfo:src;ontology;fol-ressler;2012-07-20;BFO-FOL-alpha-2012-07-20.clif")
    (let ((*readtable* (let ((r (copy-readtable *readtable*))) (setf (readtable-case r) :preserve) r))
	  (table (make-hash-table :test 'equal)))
      (loop for (precomment expression postcomment) in (read-bfo-clif-pieces f)
	 until (null expression)
	 for id = (or (and precomment (caar (last (all-matches precomment "\\[(\\d{3}-\\d{3})\\]" 1))))
		      (and postcomment (caar (all-matches postcomment"\\[(\\d{3}-\\d{3})\\]" 1))))
	 do (setf (gethash (make-uri nil (format nil "obo:bfo/axiom/~a" id)) table) (format nil "~a // axiom label in BFO2 CLIF: [~a] " expression id)))
      table)))

(defun read-bfo-clif-pieces (stream)
  (loop with pre-peek 
     for nextchar = (or pre-peek  (peek-char t stream nil :eof))
     until (eq nextchar :eof) 
     for comment = (if (char= nextchar #\/)
		       (progn
			 (if pre-peek (setq pre-peek nil)
			     (read-char stream))
			 (if (char= (peek-char nil stream) #\*)
			     (read-slash-*-comment stream)
			     (error "Trouble parsing /* clif position: ~a before: ~s" (file-position stream)
				    (read-line stream))))
		       )
     for form = (read stream nil :eof)
     for trailing = (and (equal (peek-char t stream nil :eof) #\/)
			 (read-char stream)
			 (if  (equal (peek-char nil stream nil :eof) #\/)
			      (progn (read-char stream) (read-line stream nil :eof))
			      (progn (setq pre-peek #\/) nil)
			      ))
     when (and form (neq form :eof)) collecting (list comment form trailing)))
	 

(defun read-slash-*-comment (stream)
  (with-output-to-string (s)
    (loop for char =  (read-char stream) do (write-char char s) until (and (equal char #\*) (equal (peek-char nil stream nil :eof) #\/)))
    (read-char stream nil nil)
    ))

(defun bfo-term-arity (symbol bfo2)
  (let ((entry (find symbol (cdr (bfo-terms bfo2))
		     :test (lambda(a el)
			     (if (atom (car el)) (eq a (car el)) 
				 (member a (car el)))))))
    (cond ((member :unary entry) :unary)
	  ((member :binary entry) :binary)
	  ((member :ternary entry) :ternary))))

(defmacro with-bfo-uris (spec &body body)
  `(progv (mapcar 'first (cdr (bfo-uris ,spec))) (mapcar 'third (cdr (bfo-uris ,spec)))
     ,@body))

