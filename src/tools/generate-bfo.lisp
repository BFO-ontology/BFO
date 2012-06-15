(defmacro with-obo-metadata-uris (&body body)
  (let ((props '(
;;;	     ("ready-for-release" !obo:IAO_0000122)
;;;	     ("metadata-complete" !obo:IAO_0000120)
;;;	     ("metadata-incomplete" !obo:IAO_0000123)
;;;	     ("pending-final-vetting" !obo:IAO_0000125)
;;;	     ("uncurated" !obo:IAO_0000124)
;;;	     ("placeholder" !obo:IAO_0000121)
		 ("definition" !obo:IAO_0000115)
		 ("definition-source" !obo:IAO_0000119)
		 ("term-editor" !obo:IAO_0000117)
		 ("preferred-term" !obo:IAO_0000111)
		 ("alternative-term" !obo:IAO_0000118)
		 ("example-of-usage" !obo:IAO_0000112)
;;;	     ("has-curation-status" !obo:IAO_0000114)
		 ("editor-note" !obo:IAO_0000116)
		 ("curator-note" !obo:IAO_0000232)
;;;	     ("curation-status" !obo:IAO_0000078)
;;;	     ("imported-from" !obo:IAO_0000412)
		 )))
    `(let-uri ,props
       ,@(loop for p in (list !obo:IAO_0000112 !obo:IAO_0000118 !obo:IAO_0000111 !obo:IAO_0000117 !obo:IAO_0000232
			      !obo:IAO_0000119 !obo:IAO_0000115 #|!obo:IAO_0000412 !obo:IAO_0000114|# !obo:IAO_0000116)
	    collect `(annotation-property ,p))
;;;	    ,@(loop for i in (list !obo:IAO_0000122 !obo:IAO_0000120 !obo:IAO_0000123 !obo:IAO_0000125 !obo:IAO_0000124 !obo:IAO_0000121) 
;;;		 collect `(individual ,i (type !curation-status)))
       ,@(loop for (name uri) in props collect
	      (list 'annotation-assertions !rdfs:label uri (substitute #\space #\- name :test 'char=)))
       ,@body
       )))

(defun generate-bfo2 (bfo2)
    (with-ontology bfo2-ont (:ontology-iri !obo:bfo.owl #|:versionIRI !obo:bfo/dev/bfo.owl|# :base !obo: :collecting t)
	((as (generate-declarations bfo2))
	 (as (generate-hierarchy bfo2))
	 (as (generate-property-inverses bfo2))
	 ;(as (parse-annotations bfo2))
	 ;(as (add-annotations bfo2))
	 )
      (write-rdfxml bfo2-ont "bfo:src;ontology;owl-group;bfo.owl")))

(defmacro with-bfo-uris (spec &body body)
  `(progv (mapcar 'first (cdr (bfo-uris ,spec))) (mapcar 'third (cdr (bfo-uris ,spec)))
     ,@body))

(defun generate-declarations (bfo2)
  (let ((seen (make-hash-table))
	(axs nil))
    (with-bfo-uris bfo2
      (loop for (table type) in
	   '((bfo-class2subclass class) (bfo-2prop2subprop object-property) (bfo-3prop2subprop Object-property))
	   do
	   (maphash (lambda(c sc) 
		      (loop for el in (cons c sc)
			 do
			 (unless (gethash el seen)
			   (push `(declaration (,type ,(eval el))) axs)
			   (push `(annotation-assertion !rdfs:label ,(eval el) ,(nice-guy-poor-label el))
				 axs)
			   (setf (gethash el seen) t))))
		    (funcall table bfo2)))
      axs)))

(defun nice-guy-poor-label (handle)
  (loop with now = (string-downcase (string handle)) for (match replace) in
       '(("_at$" "-at-all-times")
	 ("_st$" "-at-some-time")
	 ("^c-" "continuant-")
	 ("^o-" "occurrent-")
	 ("^t-" "temporal-")
	 ("^p-" "process-")
	 ("-s(-|$)" "-spatial-")
	 ("-t-" "-temporal-")
	 ("\\bcf\\b" "continuant-fiat")
	 ("^s-d" "specifically-d")
	 ("^g-" "generically-")
	 ("^ic" "independent continuant")
	 ("^sdc" "specifically-dependent-continuant")
	 ("^gdc" "generically-dependent-continuant")
	 ("^st-" "spatiotemporal-")
	 ("\\bst\\b" "spatiotemporal")
	 ("^s-" "spatial-")
	 ("-ppart-" "-proper part-")
	 ("-dep-" "-dependent-")
	 ("([0-3])d-" "$1\\-dimensional ")
	 ("-f-" "-function-")
	 ("^r-q" "relational-q")
	 ("\\br\\b" "region")
	 ("\\bq\\b" "quality")
	 ("\\bf\\b" "function")
	 ("\\bd\\b" "disposition")
	 ("\\bg\\f" "generic")
	 ("-t$" "-time")
	 ("^(\\\w+\\b) has" "has $1")
	 ("-" " "))
       do
       (setq now (#"replaceAll" now match replace))
     finally (return now)))

(defun generate-hierarchy (bfo2)
  (let ((axs nil))
    (with-bfo-uris bfo2
      (loop for (table relation) in
	   '((bfo-class2subclass subclassof) (bfo-2prop2subprop subobjectpropertyof) (bfo-3prop2subprop subobjectpropertyof))
	   do
	   (maphash (lambda(super subs) 
		      (loop for sub in subs
			 do (and sub super (push `(,relation ,(eval sub) ,(eval super)) axs))))
		    (funcall table bfo2))))
    axs))     

(defun generate-property-inverses (bfo2)
  (with-bfo-uris bfo2
    (loop with uris = (cdr (bfo-uris bfo2))
       for (name . keys) in (cdr (bfo-terms bfo2))
       for temporal = (find-if (lambda(el) (and (consp el) (eq (car el) :temporal))) keys )
       do (print-db name keys)
       when (and (consp name) (member :binary keys))
       collect `(inverse-object-properties ,(eval (first name)) ,(eval (second name)))
       when (and (consp name) (member :ternary keys))
	 if (and (assoc (st-term (first name )) uris)
		 (assoc (st-term (second name )) uris))
	 collect `(inverse-object-properties ,(eval (st-term (first name))) ,(eval (st-term (second name))))
	 )))

(defun st-term (name)
  (intern (format nil "~a_ST" (string name))))

(defun at-term (name)
  (intern (format nil "~a_AT" (string name))))

(defun generate-disjoints (bfo2)
  (with-bfo-uris bfo2
    (loop for class in (bfo-terms-with-md-siblings bfo2)
	 for children = (gethash class (bfo-class2subclass bfo2))
	 collect `(disjoint-classes ,@(mapcar 'eval children)))))

(defun parse-annotations (bfo2)
  (with-obo-metadata-uris
    (loop for rawa = (extract-raw-bfo-annotations)
       for table = (make-hash-table :test 'equalp)
       for (annotation-type term text) = rawa
       do
       ;; as|at|a|axiom|domain|range|note|example
       (cond ((equal annotation-type "note")
	      (push (cons !editor-note (format nil "BFO 2 Reference: ~a" (#"replaceAll" (#"replaceAll" text "^\\s+" "") "\\s+$" "")))
		    (gethash (uri-for-reference-doc-term term) table)))
	     ((equal annotation-type "example")
	      (push (cons !example-of-usage  (#"replaceAll" (#"replaceAll" text "^\\s+" "") "\\s+$" ""))
		    (gethash (uri-for-reference-doc-term term) table)))
	     ((member annotation-type '("as" "at" "a") :test 'equalp)
	      (loop for (prop text) in (parse-as text)
		 do 
		 (push (cons prop text)
		       (gethash (uri-for-reference-doc-term term) table))))
	     (t nil))
	 finally (setf (bfo-term2annotation bfo2) table))))


    


	     
  
