(defparameter *bfo2-anntag-to-prop* 
  '(("Synonym" "alternative-term")
    ("Example" "example-of-usage")
    ("Range" "editor-note")
    ("Domain" "editor-note")
    ("Definition" "definition")
    ("EXAMPLES" "example-of-usage")
    ("Elucidation" "elucidation")
    ("Axiom" "axiom-nl")
    ("Theorem" "axiom-nl")))

(defun generate-ontology-properties (bfo2)
  (with-open-file (f "bfo:src;ontology;owl-group;specification;ontology-properties.lisp")
    (loop for entry = (read f nil :eof)
	 until (eq entry :eof)
	 if (consp (car entry)) collect (car entry) else collect entry)))

(defun generate-ontology-annotation-property-defs (bfo2)
  (let ((om (load-ontology "https://information-artifact-ontology.googlecode.com/svn/trunk/src/ontology/ontology-metadata.owl")))
    (list*
     `(declaration (annotation-property ,!rdfs:isDefinedBy))
     `(declaration (annotation-property ,!rdfs:seeAlso))
     `(declaration (annotation-property ,!foaf:homepage))
     `(declaration (annotation-property ,!bfo-owl-spec-label))
     `(declaration (annotation-property ,!bfo-fol-spec-label))
     (loop for (here-label prop) in (eval-uri-reader-macro *bfo2-ontprops*)
	for label = (entity-label prop om)
	collect `(declaration (annotation-property ,prop))
	when label collect `(annotation-assertion !rdfs:label ,prop ,label)
	when label collect `(annotation-assertion !rdfs:isDefinedBy ,prop ,!obo:iao.owl)))))

(defun a-better-lousy-label (handle)
  (or (a-handcrafted-label handle)
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
	     ("-ppart" "-proper part")
	     ("-dep-" "-dependent-")
	     ("([0-3])d-" "$1.dimensional ") ;; I don't know why a "-" in the replacement works here. So change to "." and use another rule to replace with "-"
	     ("-f-" "-function-")
	     ("^r-q" "relational-q")
	     ("\\br-l" "l")
	     ("at-r" "at")
	     ("\\br\\b" "role")
	     ("\\bq\\b" "quality")
	     ("\\bf\\b" "function")
	     ("\\bd\\b" "disposition")
	     ("\\bg\\f" "generic")
	     ("-t$" "-time")
	     ("-" " ")
	     ("^(\\\w+\\b) has part" "has $1 part")
	     ("^(\\\w+\\b) part of" "part of $1")
	     ("^material$" "material entity")
	     ("^immaterial$" "immaterial entity")
	     ("\\b(\\w+)\\sproper" "proper $1")
					;	 ("\\b(continuant)\\s((proper ){0,1})}has" "has $1 $2")
	     ("\\." "-"))
	   do
	   (setq now (replace-all (#"replaceAll" now match replace) "^(\\d)-"
				  (lambda(s) (format nil "~r-" (parse-integer s))) 1))
	   finally (return now))))

(defun a-handcrafted-label (handle)
  (let ((temporal (caar (all-matches (string handle) "_([AS])T$" 1))))
    (when temporal (setq handle (intern (#"replaceAll" (string handle) "_.T$" ""))))
    (let ((matched 
	   (getf '(st-projects-onto-s "projects onto spatial region"
		   st-projects-onto-t "projects onto temporal region"
		   s-projection-of-st "spatial projection of"
		   t-projection-of-st "temporal projection of"
		   s-depends-on "specifically depends on"
		   has-s-dep "has specific dependent"
		   g-depends-on "generically depends on"
		   has-g-dep "has generic dependent"
		   o-part-of "part of occurrent"
		   c-part-of "part of continuant"
		   t-part-of "temporal part of"
		   m-part-of "member of"
		   o-ppart-of "proper part of occurrent"
		   c-ppart-of "proper part of continuant"
		   t-ppart-of "proper temporal part of"
		   o-has-part "has occurrent part"
		   c-has-part "has continuant part"
		   t-has-part "has temporal part"
		   m-has-part "has member"
		   o-has-ppart "has proper occurrent part"
		   c-has-ppart "has proper continuant part"
		   t-has-ppart "has proper temporal part")
		 handle)))
      (and matched
	   (concatenate 
	    'string  matched
	    (if temporal
		(if (equal temporal "a") " at all times" " at some times")
		""))))))

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
			   (push `(annotation-assertion !bfo-owl-spec-label ,(eval el) ,(string-downcase (string el)))
				 axs)
			   (let ((clifterm (or (second (assoc el (bfo-term2clif bfo2)))
					       (second (assoc (intern (#"replaceAll" (string el) "_(S|A)T$" ""))
							      (bfo-term2clif bfo2))
						       ))))
			     (when clifterm 
			       (push `(annotation-assertion !bfo-fol-spec-label ,(eval el) ,clifterm)
				 axs)))
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
  (let ((found (or (assoc (intern (string-upcase tag)) (bfo-anntag2term bfo2) :test 'equalp)
		   (assoc (intern (string-upcase (#"replaceAll" tag "[ _]" "-"))) (bfo-anntag2term bfo2) :test 'equalp))))
    (unless found
      (error "What's this tag: ~a ?" tag))
    (let* ((found-or-translated
	    (or (second found)
		(intern (string-upcase (#"replaceAll" tag "[ _]" "-")))))
	   (uri (third (assoc found-or-translated (cdr (bfo-uris bfo2)) :test 'equalp))))
      (or found-or-translated
	  (progn (warn "No URI found for ~a ?" found-or-translated) nil))
      )))

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
		  do (setq axs (append (maybe-generate-annotation-for-temporal-property bfo2 term text axiomid prop uri)
				       axs))
		  unless(not (boundp term))
		  do (setq axs (setq axs (append axs (generate-annotations-for-one-entry bfo2 prop axiomid uri (eval term) text))))))
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

(defun generate-annotations-for-one-entry (bfo2 prop axiomid annotation-property-uri subject text &aux axs)
  (when (and axiomid (gethash axiomid (bfo-fol-expressions bfo2)))
    (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))  ,!axiom-fol ,subject ,(gethash axiomid (bfo-fol-expressions bfo2))) axs))
  (if  (equal prop "editor-note") 
       (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))  ,annotation-property-uri ,subject ,(format nil "BFO2 Reference: ~a" text)) axs)
       (if (equal prop "example-of-usage")
	   (loop for one in (split-at-regex text "\\s*\\\\[;,]\\s*")
	      do (push `(annotation-assertion ,annotation-property-uri ,subject ,one) axs))
	   (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))
					,annotation-property-uri ,subject ,text) axs)))
  axs)


(defun maybe-generate-annotation-for-temporal-property (bfo2 term text axiomid prop uri)
  (let ((uris (cdr (bfo-uris bfo2)))
	(axs nil))
    (let* ((at (assoc (at-term term) uris))
	   (st (assoc (st-term term) uris)))
;      (print-db at st)
      ;; lazy ass cut and paste. Factor out
      (when st
	(setq axs (append axs (generate-annotations-for-one-entry bfo2 prop axiomid uri (third st) text)))
	(push `(annotation-assertion !editor-note ,(third st) ,(format nil "Alan Ruttenberg: This is a binary version of a ternary time-indexed, instance level, relation. The BFO reading of the binary relation '~a' is: exists t,  exists_at(x,t) & exists_at(y,t) & '~a'(x,y,t)" (a-better-lousy-label (car st)) (a-better-lousy-label term))) axs))
      (when at 	
	(setq axs (append axs (generate-annotations-for-one-entry bfo2 prop axiomid uri (third at) text)))
	(push `(annotation-assertion !editor-note ,(third at) ,(format nil "Alan Ruttenberg: This is a binary version of a ternary time-indexed, instance-level, relation. The BFO reading of the binary relation '~a' is: forall(t) exists_at(x,t) -> exists_at(y,t) and '~a(x,y,t)'." (a-better-lousy-label (car at)) (a-better-lousy-label term))) axs))
	axs)))

      
(defun gather-non-reference-annotations (bfo2)
  (with-bfo-uris bfo2
      (with-open-file (f "bfo:src;ontology;owl-group;specification;non-reference-annotations.lisp")
	(loop for entry = (read f nil :eof)
	   until (eq entry :eof)
	     collect (generate-from-lispy-axiom bfo2 entry)))))

(defun eval-bfo-uris (form bfo2)
  (with-bfo-uris bfo2
      (cond ((and (symbolp form) (boundp form))
	     (symbol-value form))
	    ((atom form) form)
	    (t (mapcar (lambda(el) (eval-bfo-uris el bfo2)) form)))))


;; FIXME!!- for the life of me I can't figure out why I need the (car ...)
(defun read-bfo-specific-annotation-properties (bfo2)
  (car
   (eval-uri-reader-macro
    (with-open-file (f "bfo:src;ontology;owl-group;specification;bfo-specific-annotation-properties.lisp")
      (with-bfo-uris bfo2
	(with-obo-metadata-uris 
	  (loop for form = (read f nil :eof)
	     until (eq form :eof)
	     collect form)))))))

			      
