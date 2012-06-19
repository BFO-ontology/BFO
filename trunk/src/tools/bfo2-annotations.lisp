(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bfo2-ontprops* 
    '(("definition" !obo:IAO_0000115)
      ("elucidation" !obo:IAO_0000600)
      ("definition-source" !obo:IAO_0000119)
      ("term-editor" !obo:IAO_0000117)
      ("preferred-term" !obo:IAO_0000111)
      ("alternative-term" !obo:IAO_0000118)
      ("example-of-usage" !obo:IAO_0000112)
      ("imported-from" !obo:IAO_0000412)
      ("editor-note" !obo:IAO_0000116)
      ("curator-note" !obo:IAO_0000232)
      ("bfo-spec-label" !obo:BFO_0000179)
      ("axiom-nl" !obo:IAO_0000601)
      ("axiom-fol" !obo:IAO_0000602)
      ("axiomid" !obo:IAO_0010000)
      )))

(defparameter *bfo2-anntag-to-prop* 
  '(("Synonym" "alternative-term")
    ("Example" "example-of-usage")
    ("Range" "editor-note")
    ("Domain" "editor-note")
    ("Theorem" "editor-note")
    ("Definition" "definition")
    ("EXAMPLES" "example-of-usage")
    ("Elucidation" "elucidation")
    ("Axiom" "axiom-nl")))

(defmacro with-obo-metadata-uris (&body body)
  `(let-uri ,*bfo2-ontprops*
     ,@body
     ))

(defun generate-ontology-properties (bfo2)
  `((annotation !rdfs:comment "This is an early version of BFO version 2 and has not yet been extensively reviewed by the project team members. Please see the project site http://code.google.com/p/bfo/ , the bfo2 owl discussion group http://groups.google.com/group/bfo-owl-devel , the bfo2 discussion group http://groups.google.com/group/bfo-devel, the tracking google doc http://goo.gl/IlrEE, and the current version of the bfo2 reference http://bfo.googlecode.com/svn/trunk/docs/bfo2-reference/BFO2-Reference.docx . This ontology is generated from a specification at http://bfo.googlecode.com/svn/trunk/src/tools/bfo2-reference.lisp and http://bfo.googlecode.com/svn/trunk/src/tools/bfo2-uris.lisp , with the code that generates the OWL version residing in the same directory.")))

(defun generate-ontology-annotation-property-defs (bfo2)
  (let ((om (load-ontology "https://information-artifact-ontology.googlecode.com/svn/trunk/src/ontology/ontology-metadata.owl")))
    (list*
     `(declaration (annotation-property ,!rdfs:isDefinedBy))
     `(declaration (annotation-property ,!bfo-spec-label))
     `(annotation-assertion !rdfs:label ,!bfo-spec-label "bfo specification label")
     `(sub-annotation-property-of ,!bfo-spec-label ,!rdfs:label)
     (loop for (here-label prop) in (eval-uri-reader-macro *bfo2-ontprops*)
	for label = (entity-label prop om)
	collect `(declaration (annotation-property ,prop))
	when label collect `(annotation-assertion !rdfs:label ,prop ,label)
	when label collect `(annotation-assertion !rdfs:isDefinedBy ,prop ,!obo:iao.owl)))))

(defun a-better-lousy-label (handle)
  (loop with now = (string-downcase (string handle)) for (match replace) in
       '(("_at$" "-at-all-times")
	 ("_st$" "-at-some-time")
	 ("^c-" "continuant-")
	 ("^o-" "occurrent-")
	 ("^t-" "temporal-")
	 ("^p-" "process-")
	 ("-s-dep" "-specifically-dep")
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
	 ("([0-3])d-" "$1.dimensional ");; I don't know why a "-" in the replacement works here. So change to "." and use another rule to replace with "-"
	 ("-f-" "-function-")
	 ("^r-q" "relational-q")
	 ("\\br\\b" "region")
	 ("\\bq\\b" "quality")
	 ("\\bf\\b" "function")
	 ("\\bd\\b" "disposition")
	 ("\\bg\\f" "generic")
	 ("-t$" "-time")
	 ("^(\\\w+\\b) has" "has $1")
	 ("-" " ")
	 ("^material$" "material entity")
	 ("^immaterial$" "immaterial entity")
	 ("\\." "-"))
       do
       (setq now (replace-all (#"replaceAll" now match replace) "^(\\d)-"
			      (lambda(s) (format nil "~r-" (parse-integer s))) 1))
     finally (return now)))

(defun generate-label-annotations (bfo2)
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
			   (push `(annotation-assertion !rdfs:label ,(eval el) ,(a-better-lousy-label el))
				 axs)
			   (push `(annotation-assertion !bfo-spec-label ,(eval el) ,(string-downcase (string el)))
				 axs)
			   (setf (gethash el seen) t))))
		    (funcall table bfo2)))
      axs)))

(defun parse-reference-annotations (bfo2)
  (with-obo-metadata-uris
    (loop with rawa = (extract-raw-bfo-annotations)
       with table = (make-hash-table :test 'equalp)
       for (annotation-type term text) in rawa
       do
	 (setq text (#"replaceAll" (#"replaceAll" text "^\\s+" "") "\\s+$" ""))
       ;; as|at|a|axiom|domain|range|note|example
       (cond ((equalp annotation-type "note")
	      (push (cons !editor-note (format nil "BFO 2 Reference: ~a" text))
		    (gethash (uri-for-reference-doc-term term bfo2) table)))
	     ((#"matches" annotation-type "(?i)example.*")
	      (push (cons !example-of-usage  text)
		    (gethash (uri-for-reference-doc-term term bfo2) table)))
	     ((member annotation-type '("as" "at" "a") :test 'equalp)
	      (loop for (prop text) in (parse-as text)
		 do 
		   (push (cons prop text)
		       (gethash (uri-for-reference-doc-term term bfo2) table))))
	     (t nil))
	 finally (progn (setf (bfo-term2annotation bfo2) table) (return table)))))

(defun uri-for-reference-doc-term (tag bfo2)
  (let ((found (or (assoc tag (cdr (bfo-anntag2term bfo2)) :test 'equalp)
		   (assoc (#"replaceAll" tag "[ _]" "-") (cdr (bfo-anntag2term bfo2)) :test 'equalp))))
    (unless found
      (error "What's this tag: ~a ?" tag))
    (let* ((found-or-translated
	    (or (second found)
		(intern (string-upcase (#"replaceAll" tag "[ _]" "-")))))
	   (uri (third (assoc found-or-translated (cdr (bfo-uris bfo2)) :test 'equalp))))
      (or found-or-translated
	  (progn (warn "No URI found for ~a ?" found-or-translated) nil)))))

(defun parse-as (text)
  (let ((them (all-matches text "\\b(\\w+):(.*)" 1 2)))
    (when (> (length them) 1) (print them))
    them))

(defun generate-reference-annotations (bfo2 &aux axs)
  (parse-reference-annotations bfo2)
  (with-bfo-uris bfo2
    (maphash (lambda (term as)
	       (loop for (ann . rawtext) in as
		  for (text axiomid) = (multiple-value-list (clean-reference-annotation-text rawtext ann term))
		  for prop = (second (assoc ann *bfo2-anntag-to-prop* :test 'equalp))
		  for uri = (or (and (uri-p ann) ann)
				(second (assoc prop *bfo2-ontprops* :test 'equalp)))
		  when (not (boundp term))
		    do (setq axs (append (maybe-generate-annotation-for-ternary-property bfo2 term text axiomid prop uri)
				      axs))
		  unless(not (boundp term))
		  if  (equal prop "editor-note") do
		  (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))  ,uri ,(eval term) ,(format nil "BFO2 Reference: ~a" text)) axs)
		  else if (equal prop "example-of-usage")
		    do (loop for one in (split-at-regex text "\\s*\\\\[;,]\\s*")
			  do (push `(annotation-assertion ,uri ,(eval term) ,one) axs))
		  else 
		    do (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid))) ,uri ,(eval term) ,text) axs)
		  ))
	     (bfo-term2annotation bfo2)))
  axs)

(defun clean-reference-annotation-text (text ann term)
  (when (null text) (print-db ann term) (break))
  (let ((clean (#"replaceAll" (string-trim " " text) "&amp;" "&")))
    (let ((axiomid (caar (all-matches clean "\\[+(\\d+-\\d+)\\]*" 1))))
      (if axiomid
	  (progn
	    (setq clean (#"replaceAll" clean "\\[+(\\d+-\\d+)\\]*" "(axiom label in BFO2 Reference: [$1])"))
	    (values clean (make-uri nil (format nil "obo:bfo/axiom/~a" axiomid))))
	  clean))))

(defun maybe-generate-annotation-for-ternary-property (bfo2 term text axiomid prop uri)
  (let ((uris (cdr (bfo-uris bfo2)))
	(axs nil))
    (let* ((at (assoc (intern (concatenate 'string (string term) "_AT")) uris))
	   (st (assoc (intern (concatenate 'string (string term) "_ST")) uris)))
;      (print-db at st)
      ;; lazy ass cut and paste. Factor out
      (when st
	(if  (equal prop "editor-note") 
	     (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))  ,uri ,(third st) ,(format nil "BFO2 Reference: ~a" text)) axs)
	     (if (equal prop "example-of-usage")
		 (loop for one in (split-at-regex text "\\s*\\\\[;,]\\s*")
		    do (push `(annotation-assertion ,uri ,(third st) ,one) axs))
		 (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))
					      ,uri ,(third st) ,text) axs)))
	(push `(annotation-assertion !editor-note ,(third st) ,(format nil "Alan Ruttenberg: This is a binary version of a ternary time-indexed, instance level, relation. The BFO reading of the binary relation '~a' is: exists t,  exists_at(x,t) & exists_at(y,t) & '~a'(x,y,t)" (a-better-lousy-label (car st)) (a-better-lousy-label term))) axs))
      (when at
	(if  (equal prop "editor-note") 
	     (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))  ,uri ,(third at) ,(format nil "BFO2 Reference: ~a" text)) axs)
	     (if (equal prop "example-of-usage")
		 (loop for one in (split-at-regex text "\\s*\\\\[;,]\\s*")
		    do (push `(annotation-assertion ,uri ,(third at) ,one) axs))
		 (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))
					      ,uri ,(third at) ,text) axs)))
	(push `(annotation-assertion !editor-note ,(third at) ,(format nil "Alan Ruttenberg: This is a binary version of a ternary time-indexed, instance-level, relation. The BFO reading of the binary relation '~a' is: forall(t) exists_at(x,t) -> exists_at(y,t) and '~a(x,y,t)'" (a-better-lousy-label (car at)) (a-better-lousy-label term))) axs)
	axs))))

      