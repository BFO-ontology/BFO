(defparameter *bfo2-anntag-to-prop* 
  '(("Synonym" "alternative-term")
    ("Example" "example-of-usage")
    ("Range" "editor-note")
    ("Domain" "editor-note")
    ("Definition" "definition")
    ("EXAMPLES" "example-of-usage")
    ("EXAMPLE" "example-of-usage")
    ("Elucidation" "elucidation")
    ("Axiom" "axiom-nl")
    ("Theorem" "axiom-nl")))

(defun generate-ontology-properties (bfo2)
  (append (bfo-ontology-annotations bfo2) 
	  (with-open-file (f "bfo:src;ontology;owl-group;specification;ontology-properties.lisp")
	    (loop for entry = (read f nil :eof)
	       until (eq entry :eof)
	       if (consp (car entry)) collect (car entry) else collect entry))))

(defun generate-ontology-annotation-property-defs (bfo2)
  (let ((om (or (ignore-errors (load-ontology (truename "bfo:src;ontology;local;ontology-metadata.owl")))
		(ignore-errors
		  (load-ontology "http://purl.obolibrary.org/obo/iao/ontology-metadata.owl")))
		  ))
    (assert om () "Didn't load ontology metadata")
    (list*
     `(declaration (annotation-property ,!rdfs:isDefinedBy))
     `(declaration (annotation-property ,!dc:license))
     `(declaration (annotation-property ,!rdfs:seeAlso))
     `(declaration (annotation-property ,!dc:publisher))
     `(declaration (annotation-property ,!dc:contributor))
;     `(declaration (annotation-property ,!dc:member))
     `(declaration (annotation-property ,!foaf:homepage))
     `(declaration (annotation-property ,!foaf:mbox))
;     `(declaration (annotation-property ,!bfo-owl-spec-label))
;     `(declaration (annotation-property ,!bfo-fol-spec-label))
     (loop for (here-label prop) in (eval-uri-reader-macro *bfo2-ontprops*)
	for label = (entity-label prop om)
	collect `(declaration (annotation-property ,prop))
	when label collect `(annotation-assertion !rdfs:label ,prop ,(concatenate 'string label "@en"))
	when label collect `(annotation-assertion !rdfs:isDefinedBy ,prop ,!obo:iao.owl)))))

(defun a-better-lousy-label (handle &optional (langtag t))
  (concatenate
   'string 
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
	      ("-dep-" "-dependent-")
	      ("([0-3])d-" "$1.dimensional ") ;; I don't know why a "-" in the replacement works here. So change to "." and use another rule to replace with "-"
	      ("-f-" "-function-")
	      ("^r-q" "relational-q")
	      ("\\br-l" "l")
	      ("at-r" "at")
;	      ("profile-of" "process-profile-of")
	      ("\\br\\b" "role")
	      ("\\bq\\b" "quality")
	      ("\\bf\\b" "function")
	      ("\\bd\\b" "disposition")
	      ("\\bg\\f" "generic")
	      ("-t$" "-time")
	      ("-" " ")
	      ("^material$" "material entity")
	      ("^immaterial$" "immaterial entity")
	      ("\\." "-"))
	    do
	    (setq now (replace-all (#"replaceAll" now match replace) "^(\\d)-"
				   (lambda(s) (format nil "~r-" (parse-integer s))) 1))
	    finally (return now)))
   "@en"))

(defun a-handcrafted-label (handle)
  (if (eq handle 'c-part-of-object_at)
      "part of continuant at all times that whole exists" 
      (if (eq handle 'c-has-part-object_at)
	  "has continuant part at all times that part exists" 
	  (let ((temporal (caar (all-matches (string handle) "_([AS])T$" 1))))
	    (when temporal (setq handle (intern (#"replaceAll" (string handle) "_.T$" ""))))
;	    (print-db handle)
	    (let ((matched 
		   (getf '(st-projects-onto-s "projects onto spatial region"
			   st-projects-onto-t "projects onto temporal region"
			   s-projection-of_st "spatial projection of"
			   t-projection-of_st "temporal projection of"
			   s-depends-on "specifically depends on"
			   has-s-dep "has specific dependent"
			   g-depends-on "generically depends on"
			   has-g-dep "has generic dependent"
			   o-part-of "part of occurrent"
			   c-part-of "part of continuant"
			   t-part-of "temporal part of"
			   member-part-of "member part of"
			   o-ppart-of "proper part of occurrent"
			   c-ppart-of "proper part of continuant"
			   t-ppart-of "proper temporal part of"
			   o-has-part "has occurrent part"
			   c-has-part "has continuant part"
			   has-t-part "has temporal part"
			   has-member-part "has member part"
			   o-has-ppart "has proper occurrent part"
			   c-has-ppart "has proper continuant part"
			   has-t-ppart "has proper temporal part"
			   realizable "realizable entity"
			   located-at-r "occupies spatial region"
			   r-location-of "has spatial occupant"
			   occupies "occupies spatiotemporal region"
			   occupied-by "has spatiotemporal occupant"
			   spans "occupies temporal region"
			   span-of "has temporal occupant" 
			   fiat-object "fiat object part"
			   )
			 handle)))
	      (and matched
		   (concatenate 
		    'string  matched
		    (if temporal
			(if (equal temporal "A") " at all times" " at some time")
			""))))))))

(defun generate-label-annotations (bfo2)
  (let ((seen (make-hash-table))
	(axs nil))
    (with-bfo-uris bfo2
      (loop for (table type) in
	   '((bfo-class2subclass class) (bfo-2prop2subprop object-property) #+temporal (bfo-3prop2subprop Object-property))
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
	 (setq text (#"replaceAll" text "\\s*/\\*.*?\\*/\\s*" " ")) ; remove in line comments
       ;; as|at|a|axiom|domain|range|note|example
       (cond ((equalp annotation-type "note")
	      (if (equalp term "ontology") 
		  (push `(annotation !editor-note ,(format nil "BFO 2 Reference: ~a" text))
			(bfo-ontology-annotations bfo2))
		  (push (cons !editor-note (format nil "BFO 2 Reference: ~a" text))
			(gethash (uri-for-reference-doc-term term bfo2) table))))
	     ((#"matches" annotation-type "(?i)example.*")
	      (loop for one in (split-at-regex text "\\s*\\\\[,;.]\\s*")
		   do (push (cons !example-of-usage  one)
			 (gethash (uri-for-reference-doc-term term bfo2) table))))
	     ((member annotation-type '("as" "at" "a") :test 'equalp)
	      (loop for (prop text) in (parse-as text)
		 do 
		   (push (cons prop text)
		       (gethash (uri-for-reference-doc-term term bfo2) table))))
	     (t nil))
	 finally (progn (setf (bfo-term2annotation bfo2) table) (return table)))))

(defun uri-for-reference-doc-term (tag bfo2)
;  (print-db tag)
  (let ((found (or (assoc (intern (string-upcase tag)) (bfo-anntag2term bfo2) :test 'equalp)
		   (assoc (intern (string-upcase (#"replaceAll" tag "[ _]" "-"))) (bfo-anntag2term bfo2) :test 'equalp))))
    (unless found
      (warn "What's this tag: ~a ?" tag))
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
	      do (push `(annotation-assertion ,annotation-property-uri ,subject ,(concatenate 'string one "@en")) axs))
	   (push `(annotation-assertion ,@(and axiomid (list (list 'annotation !axiomid axiomid)))
					,annotation-property-uri ,subject ,(concatenate 'string text "@en")) axs)))
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
	(push `(annotation-assertion !editor-note ,(third st) ,(format nil "Alan Ruttenberg: This is a binary version of a ternary time-indexed, instance level, relation. The BFO reading of the binary relation '~a' is: exists(t) exists_at(x,t) & exists_at(y,t) & '~a'(x,y,t)"  (a-better-lousy-label (car st) nil) (a-better-lousy-label term nil))) axs))
      (when at 	
	(setq axs (append axs (generate-annotations-for-one-entry bfo2 prop axiomid uri (third at) text)))
	(push `(annotation-assertion !editor-note ,(third at) ,(format nil "Alan Ruttenberg: This is a binary version of a ternary time-indexed, instance-level, relation. The BFO reading of the binary relation '~a' is: forall(t) exists_at(x,t) -> exists_at(y,t) and '~a(x,y,t)'." (a-better-lousy-label (car at) nil) (a-better-lousy-label term nil))) axs))
	axs)))

      
(defun gather-non-reference-annotations (bfo2)
  (with-obo-metadata-uris
    (with-bfo-uris bfo2
      (with-open-file (f "bfo:src;ontology;owl-group;specification;non-reference-annotations.lisp")
	(loop for entry = (eval-uri-reader-macro (read f nil :eof))
	   until (eq entry :eof)
	   append (generate-from-lispy-axiom bfo2 entry))))))

(defun gather-extra-references-annotations (bfo2)
  (with-obo-metadata-uris
    (with-bfo-uris bfo2
      (with-open-file (f "bfo:src;ontology;owl-group;specification;extra-references-annotations.lisp")
	(loop for entry = (eval-uri-reader-macro (read f nil :eof))
	   until (eq entry :eof)
	   append (generate-from-lispy-axiom bfo2 entry))))))

(defun eval-bfo-uris (form bfo2)
  (with-bfo-uris bfo2
      (cond ((and (symbolp form) (boundp form))
	     (symbol-value form))
	    ((atom form) form)
	    (t (mapcar (lambda(el) (eval-bfo-uris el bfo2)) form)))))


(defun read-bfo-specific-annotation-properties (bfo2)
   (eval-uri-reader-macro
    (with-open-file (f "bfo:src;ontology;owl-group;specification;bfo-specific-annotation-properties.lisp")
      (with-bfo-uris bfo2
	(with-obo-metadata-uris 
	  (loop for form = (read f nil :eof)
	     until (eq form :eof)
	     collect form))))))

			      
(defun generate-inverse-annotations-duplicates (bfo2 partial-bfo)
  (with-obo-metadata-uris
    (let ((ont (if (v3kb-p partial-bfo) partial-bfo (load-ontology (namestring (truename partial-bfo))))))
      (loop for (fromprop toprop annprop value) in
	   (sparql '(:select (?p ?pi ?a ?v) (:distinct t) 
		     (?a !rdf:type !owl:AnnotationProperty)
		     (:union
		      ((?p !rdf:type !owl:ObjectProperty) 
		       (?pi !owl:inverseOf ?p))
		      ((?p !rdf:type !owl:ObjectProperty) 
		       (?p !owl:inverseOf ?pi)))
		     (?p ?a ?v)
		     (:filter (or (equal ?a !editor-note) (equal ?a !elucidation) (equal ?a !example-of-usage) (equal ?a !definition))
		      ))
		   :kb ont :use-reasoner :none :use-reasoner :pellet)
	   ;do (print-db fromprop toprop annprop value (format nil "(from inverse property - ~a) ~a" (car (rdfs-label fromprop ont)) value))
	   collect
	   `(annotation-assertion ,annprop ,toprop ,(format nil "[copied from inverse property '~a'] ~a"
							    (car (rdfs-label fromprop ont)) value))
	   ))))
							
