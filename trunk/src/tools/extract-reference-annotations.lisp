(in-package :cl-user)


(defun extract-raw-bfo-annotations (&optional (path "bfo:docs;bfo2-reference;BFO2-Reference.docx"))
  "Syntax in document: one of: a(label)[one colon-tagged annotation], a(label)[multiple colon-tagged annotations], axioms(label) [textual description of an axiom]. Output list of triples of tag, label, text"
  (let* ((zip  (new 'java.util.zip.ZipFile (new 'file (namestring (truename path)))))
	 (entry (#"getEntry" zip  "word/document.xml"))
	 (zstream (#"getInputStream" zip entry))
	 (isr (new 'InputStreamReader zstream "UTF-8"))
	 (reader (new 'BufferedReader isr))
	 ;; this next is sneaky. Bum off the jar stream implementation, after reading some implementation code.
	 (stream (new 'org.armedbear.lisp.Stream 'system:jar-stream reader))
	 (xml (progn (read-line stream) (read-line stream))))
    (let ((extracted-text 
	   (with-output-to-string (s)
	     (loop for match in  (all-matches xml "<w:t[^>]*>(.*?)</w:t>" 1)
		do (write-string (#"replaceAll" (car match) "</{0,1}.*?>" "") s) 
		)
	     )))
      (all-matches extracted-text  "(?i)(as|a|axiom|domain|range|note|example)\\((.*?)\\)\\[(.*?)\\]" 1 2 3))))

;; simpler, but doesn't see hidden text
;; (#"parseToString" (new 'tika) (new 'file (translate-logical-pathname "bfo:docs;bfo2-reference;BFO2-Reference.docx")))

; (remove-duplicates (mapcar 'car (extract-raw-bfo-annotations)) :test 'string-equal) -> ("axiom" "range" "as" "a" "note" "example")
; (remove-duplicates (mapcar 'second (extract-raw-bfo-annotations)) :test 'string-equal)

(defun known-in-reference ()
  (remove-duplicates (mapcar 'second (extract-raw-bfo-annotations)) :test 'string-equal))

(defun extract-uri-label-schulz ()
  (let ((schulz  (load-ontology (namestring (translate-logical-pathname "bfo:src;ontology;owl-schulz;bfo.owl")))))
    (let ((them nil))
      (maphash (lambda (k v)(push  (list (car v) k) them)) (rdfs-labels schulz))
      (sort them 'string-lessp :key 'car))))

(defun extract-uri-label-ruttenberg ()
  (let ((rutt  (load-ontology (concatenate 'string (namestring (translate-logical-pathname "bfo:src;ontology;owl-ruttenberg;")) "bfo2_all.owl"))))
;    (print-db (loaded-documents rutt))
    (let ((them nil))
      (maphash (lambda (k v)(push  (list (car v) k) them)) (rdfs-labels rutt))
      (setq them (sort them 'string-lessp :key 'car))
      them)))

(defun three-way-compare ()
  (let ((ignore '("alternative term" "axiom id" "Author" "comment" "editor note" "topObjectProperty" "definition"
		  "editor preferred term" "BFOWL 2 Working Draft" "string")))
    (flet ((normalize (list)
	     (set-difference (mapcar (lambda(e)
				       (#"replaceFirst"
					(#"replaceFirst"
					 (#"replaceFirst"
					  (#"replaceAll" e "[_-]" " ") "^is " "") 
					 "^g " "generically ")
					"^s " "specifically "))
				       list)
			     ignore :test 'string-equal)))
      (let ((reference (normalize (known-in-reference)))
	    (ruttenberg (normalize (mapcar 'car (extract-uri-label-ruttenberg))))
	    (schulz (normalize (mapcar 'car (extract-uri-label-schulz)))))
	(format t "In reference and not ruttenberg: ~%")
	(print (sort (set-difference reference ruttenberg :test 'string-equal) 'string-lessp))
	(format t "~%~%In ruttenberg and not reference: ~%")
	(print (sort (set-difference ruttenberg reference :test 'string-equal) 'string-lessp))
	(format t "~%~%In reference and not schulz: ~%")
	(print (sort (set-difference reference schulz :test 'string-equal) 'string-lessp))
	(format t "~%~%In schulz and not reference: ~%")
	(print (sort (set-difference schulz reference :test 'string-equal) 'string-lessp))
	nil))))


#|
Found in the reference:

"bearer_of" 
"concretization" 
"continuant" 
"continuant fiat boundary" 
"continuant_part_of" 
"disposition" 
"disposition_of" 
"entity" 
"exists_at" 
"fiat object part" 
"function" 
"function_of" 
"g-depends on" 
"generically dependent continuant" 
"has_continuant_part" 
"has_disposition" 
"has_function" 
"has_material_basis" 
"has_occurrent_part" 
"has_participant" 
"has_role" 
"immaterial entity" 
"independent continuant" 
"inheres_in" 
"located_at" 
"located_in" 
"material entity" 
"member_part_of" 
"object" 
"object aggregate" 
"occupies" 
"occurrent" 
"occurrent_part_of" 
"one-dimensional continuant fiat boundary" 
"one-dimensional spatial region" 
"one-dimensional temporal region" 
"process" 
"process boundary" 
"projects_onto" 
"proper_continuant_part_of" 
"proper_occurrent_part_of" 
"quality" 
"quality_of" 
"realizable entity" 
"realization" 
"relational quality" 
"role" 
"role_of" 
"s-depends on" 
"site" 
"spatial region" 
"spatiotemporal region" 
"specifically dependent continuant" 
"temporal region" 
"temporal_part_of" 
"three-dimensional spatial region" 
"two-dimensional continuant fiat boundary" 
"two-dimensional spatial region" 
"zero-dimensional continuant fiat boundary" 
"zero-dimensional spatial region" 
"zero-dimensional temporal region" 
|#


#|

Latest comparison run:

In reference and not ruttenberg: 

("concretization" "continuant fiat boundary" "continuant part of" "disposition of" "exists at" "has continuant part" "has disposition" "has material basis" "has occurrent part" "immaterial entity" "member part of" "occupies" "occurrent part of" "one dimensional continuant fiat boundary" "one dimensional spatial region" "one dimensional temporal region" "projects onto" "proper continuant part of" "proper occurrent part of" "realization" "relational quality" "temporal part of" "three dimensional spatial region" "two dimensional continuant fiat boundary" "two dimensional spatial region" "zero dimensional continuant fiat boundary" "zero dimensional spatial region" "zero dimensional temporal region") 

In ruttenberg and not reference: 

("aggregate of" "begins to exist during" "ceases to exist during" "concretization of" "concretizes" "connected spatiotemporal region" "connected temporal region" "course of" "dependent continuant" "example of usage" "fiat part of" "fiat process part" "granular part of" "granular part of process" "has course" "has granular part" "has granular process part" "has part" "has participant beginning to exist" "has participant ceasing to exist" "has quality" "has site of" "immediately preceded by" "immediately precedes" "object boundary" "occurs in" "one dimensional region" "part of" "participates in" "preceded by" "precedes" "process aggregate" "realized by" "realizes" "scattered spatiotemporal region" "scattered temporal region" "spatiotemporal instant" "spatiotemporal interval" "temporal instant" "temporal interval" "three dimensional region" "two dimensional region" "zero dimensional region") 

In reference and not schulz: 

("concretization" "one dimensional spatial region" "realization" "three dimensional spatial region" "two dimensional spatial region" "zero dimensional spatial region") 

In schulz and not reference: 

("boundary dependent on" "concretization of" "concretizes" "connected spatiotemporal region" "connected temporal region" "contained in" "continuant boundary of" "dependent continuant" "depends on" "example of usage" "fiat process part" "has continuant boundary" "has continuant proper part" "has material part" "has member part" "has occurrent boundary" "has occurrent proper part" "has part" "has process profile" "has proper part" "has quality" "has temporal part" "inv depends on" "inv generically depends on" "inv located at" "inv located in" "inv specifically depends on" "label" "material basis of" "material part of" "object boundary" "occupied by" "occurrent boundary of" "occurs in" "one dimensional region" "part of" "participates in" "PlainLiteral" "preceded by" "precedes" "process aggregate" "process profile" "process profile of" "processual context" "processual entity" "proper part of" "proper temporal part of" "realized by" "realizes" "scattered spatiotemporal region" "scattered temporal region" "spatiotemporal instant" "spatiotemporal interval"  "three dimensional region" "two dimensional region" "zero dimensional region") 
|#

