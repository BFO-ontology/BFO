(defparameter *bfo2-relations*
  '((is-part-of !obo:BFO_0000050)
    (has-part !obo:BFO_0000051)
    (inheres-in !obo:BFO_0000052)
    (is-bearer-of !obo:BFO_0000053)
    (is-realized-by !obo:BFO_0000054)
    (realizes !obo:BFO_0000055)
    (participates-in !obo:BFO_0000056)
    (has-participant !obo:BFO_0000057)
    (is-concretization-of !obo:BFO_0000058)
    (concretizes !obo:BFO_0000059)
    (is-immediately-preceded-by !obo:BFO_0000060)
    (immediately-precedes !obo:BFO_0000061)
    (is-preceded-by !obo:BFO_0000062)
    (precedes !obo:BFO_0000063)
    (is-course-of !obo:BFO_0000064)
    (has-course !obo:BFO_0000065)
    (occurs-in !obo:BFO_0000066)
    (has-site-of !obo:BFO_0000067)
    (begins-to-exist-during !obo:BFO_0000068)
    (ceases-to-exist-during !obo:BFO_0000069)
    ))

(defun id-for-bfo-axiom (i)
  (make-uri nil (format nil "obo:BFO_~7,'0d" (+ i 10000))))

(defmacro with-label-vars-from (ont &body body)
  (let ((ontvar (make-symbol "ONT"))
	(classes (make-symbol "CLASSES")))
    `(let* ((,ontvar (load-ontology ,ont))
	   (,classes (mapcar (lambda(e) 
			       (list (intern (string-upcase (substitute #\- #\space (second e))))
				     (first e)))
			     (sparql '(:select (?class ?label) ()
				       (?class !rdf:type !owl:Class) 
				       (?class !rdfs:label ?label))
				     :kb ,ontvar :use-reasoner :none))))
       (progv (mapcar 'first ,classes) (mapcar 'second ,classes)
	 ,@body))))

(defun nax (id ax &optional label)
  "named axiom - id is a number. label is optional."
  `(,(first ax)  (annotation !obo:IAO_0010000 ,(id-for-bfo-axiom id))
     ,@(and label `((annotation !rdfs:label ,(format nil "~a@en" label))))
     ,@(cdr ax)))

(defun bfo2-relations ()
  (let ((definition !obo:IAO_0000115)
	(alternative-term !obo:IAO_0000118)
	(example-of-usage !obo:IAO_0000112)
	(editor-preferred-label !obo:IAO_0000111)
	(editor-note  !obo:IAO_0000116)
	(axiom-id !obo:IAO_0010000))
    (labels ((@en (s) (format nil "~a@en" s))
	     (label (thing s)  `(annotation-assertion !rdfs:label ,thing ,(@en s)))
	     (alternative-label (thing s)  `(annotation-assertion ,alternative-term ,thing ,(@en s)))
	     (definition (thing s)  `(annotation-assertion ,definition ,thing ,(@en s))))
      (with-ontology bfo-2-relations (:about !obo:bfo/core-relations.owl :collecting t)
	  ((progv (mapcar 'car *bfo2-relations*) (mapcar 'second *bfo2-relations*)
	     (with-label-vars-from "~/repos/bfo/trunk/src/ontology/bfo2.owl"  
	       (asq (imports !obo:bfo/core-classes.owl))
	       (loop for (var uri) in *bfo2-relations* do
		    (as `(declaration (object-property ,uri))
			(label uri (substitute #\space #\- (string-downcase (string var)))))
		    (when (find #\- (string var))
		      (as (alternative-label uri (substitute #\_ #\- (#"replaceFirst" (string-downcase (string var)) "is-" "")))))
		    (if (#"matches" (string-downcase (string var)) "^is-.*")
			(as (alternative-label uri (substitute #\space #\- (#"replaceFirst" (string-downcase (string var)) "is-" ""))))
		    ))
	       (as
		`(declaration (annotation-property ,axiom-id))
		`(annotation-assertion !rdfs:label ,axiom-id "axiom id")
		`(declaration (annotation-property ,example-of-usage))
		`(declaration (annotation-property ,editor-preferred-label))
		`(declaration (annotation-property ,alternative-term))
		`(declaration (annotation-property ,definition))
		`(declaration (annotation-property ,editor-note)))
	       (as
		(nax 1 `(object-property-domain ,is-part-of ,entity) "domain of part of")
		(nax 2 `(object-property-range ,is-part-of ,entity))
		(nax 3 `(inverse-object-properties ,is-part-of ,has-part))
		(nax 4 `(object-property-domain ,has-participant ,process))
		(nax 5 `(object-property-range ,has-participant ,continuant))
		(nax 6 `(inverse-object-properties ,has-participant ,participates-in))
		(nax 7 `(object-property-domain ,is-bearer-of ,independent-continuant))
		(nax 8 `(object-property-range ,is-bearer-of ,dependent-continuant))
		(nax 9 `(inverse-object-properties ,inheres-in ,is-bearer-of))
		(nax 10 `(object-property-domain ,has-course ,continuant))
		(nax 11 `(object-property-range ,has-course ,process))
		(nax 12 `(inverse-object-properties ,has-course ,is-course-of))
		(nax 13 `(transitive-object-property ,has-part))
		(nax 14 `(functional-object-property ,inheres-in))
		(nax 15 `(object-property-domain ,immediately-precedes ,process))
		(nax 16 `(object-property-range ,immediately-precedes ,process))
		(nax 17 `(inverse-object-properties ,immediately-precedes ,is-immediately-preceded-by))

		(nax 19 `(object-property-domain ,precedes ,process))
		(nax 20 `(object-property-range ,precedes ,process))
		(nax 21 `(inverse-object-properties ,precedes ,is-preceded-by))
		(nax 22 `(sub-object-property-of ,is-immediately-preceded-by ,is-preceded-by))
		(nax 23 `(sub-object-property-of ,immediately-precedes ,precedes))
		(nax 24 `(object-property-domain ,occurs-in ,process))
		(nax 25 `(object-property-range ,occurs-in ,independent-continuant))
		(nax 26 `(inverse-object-properties ,occurs-in ,has-site-of))
		#|(nax 27 |# `(sub-object-property-of (object-property-chain ,is-part-of ,occurs-in) ,occurs-in); ) ;"part a process that occurs in C occurs in C")
		#|(nax 18 |# `(sub-object-property-of (object-property-chain ,occurs-in ,is-part-of) ,occurs-in) ;) ;"if p occurs in c, then p occurs in anything that c is part of")

		(nax 28 `(transitive-object-property ,precedes))
		(nax 29 `(transitive-object-property ,is-preceded-by))
		(nax 30 `(functional-object-property ,is-immediately-preceded-by))
		(nax 31 `(functional-object-property ,immediately-precedes))
		(nax 32 `(object-property-domain ,concretizes ,generically-dependent-continuant))
		(nax 33 `(object-property-range ,concretizes ,specifically-dependent-continuant))
		(nax 34 `(inverse-object-properties ,concretizes ,is-concretization-of))
		(nax 35 `(object-property-domain ,realizes ,process))
		(nax 36 `(object-property-range ,realizes ,realizable-entity))
		(nax 37 `(inverse-object-properties ,realizes ,is-realized-by))
		#|(nax 38 |# `(sub-object-property-of (object-property-chain ,realizes ,inheres-in) ,has-participant) ; "bearers of realizables participate in their realizaton")  ; note OWLAPI bug.

                (nax 39 `(inverse-object-properties ,begins-to-exist-during ,ceases-to-exist-during))
		     ))))
	(write-rdfxml bfo-2-relations "~/repos/bfo/trunk/src/ontology/bfo2-relations.owl")))))
		
;(bfo2-relations)
	     
(def-metadata concretizes bfo2-relations
  :editor-note
  "A generically dependent continuant may inhere in more than one entity. It does so by virtue of the fact that there is, for each entity that it inheres, a specifically dependent *concretization* of the generically dependent continuant that is specifically dependent. For instance, consider a story, which is an information artifact that inheres in some number of books. Each book bears some quality that carries the story. The relation between this quality and the generically dependent continuant is that the former is the concretization of the latter."
  :definition
  "a relationship between a generically dependent continuant and at least one specifically dependent continuant upon which it existentially depends"
  :definition-editor
  "Alan Ruttenberg"
  :definition-editor
  "Barry Smith")


;(def-axiom 1 bfo 
;  `(object-property-domain ,is-part-of ,entity)
;  "domain of part of")



;(with-label-vars-from "~/repos/bfo/trunk/src/ontology/bfo2.owl"  entity) -> !obo:BFO_0000001

;; these axioms violate global constaints
;		(nax 18 `(irreflexive-object-property ,is-immediately-preceded-by)) 
;		(nax 19 `(irreflexive-object-property ,immediately-precedes))
;		(nax 20 `(asymmetric-object-property ,is-immediately-preceded-by))
;		(nax 21 `(asymmetric-object-property ,immediately-precedes))
	   
;		(nax 24 `(irreflexive-object-property ,is-preceded-by)) 
;		(nax 25 `(irreflexive-object-property ,precedes))
;		(nax 26 `(asymmetric-object-property ,is-preceded-by))
;		(nax 27 `(asymmetric-object-property ,precedes))
