(defparameter *bfo-id-counter* 9)
(defparameter *bfo-id-map* (make-hash-table))

(defun id-for-bfo-class (class)
  (let ((mapping *bfo-id-map*))
    (cond ((eq !bfo:Entity class) !obo:BFO_0000001)
	  ((eq !snap:Continuant class) !obo:BFO_0000002)
	  ((eq !span:Occurrent class) !obo:BFO_0000003)
	  ((eq !snap:IndependentContinuant class) !obo:BFO_0000004)
	  ((eq !snap:DependentContinuant class) !obo:BFO_0000005)
	  ((eq !snap:SpatialRegion class) !obo:BFO_0000006)
	  ((eq !span:Process class) !obo:BFO_0000007)
	  ((eq !span:TemporalRegion class) !obo:BFO_0000008)
	  ((eq !span:SpatioTemporalRegion class) !obo:BFO_0000009)
	  (t
	   (or (gethash class mapping)
	       (prog1  (setf (gethash class mapping) (make-uri  nil (format nil "obo:BFO_~7,'0d" *bfo-id-counter*)))
		 (incf *bfo-id-counter*)))))))

(with-ontology bfo (:collecting t)
    ((let* ((bfo (load-ontology "~/repos/bfo/trunk/bfo.owl"))
	    (definition !obo:IAO_0000115)
	    (alternative-term !obo:IAO_0000118)
	    (example-of-usage !obo:IAO_0000112)
	    (editor-preferred-label !obo:IAO_0000111)
	    )
       (flet ((@en (s) (format nil "~a@en" (substitute #\space #\_ s))))
	 (as `(annotation-assertion !rdfs:label ,alternative-term ,(@en "alternative term"))
	     `(annotation-assertion !rdfs:label ,example-of-usage ,(@en "example of usage"))
	     `(annotation-assertion !rdfs:label ,editor-preferred-label ,(@en "editor preferred term"))
	   `(declaration (annotation-property ,example-of-usage))
	   `(declaration (annotation-property ,editor-preferred-label))
	   `(declaration (annotation-property ,alternative-term))
	   )
	 (loop for (class parent label doc)  in
	      (sparql '(:select (?class ?parent ?label ?documentation) () (?class !rdf:type !owl:Class) 
			(:optional (?class !rdfs:subClassOf ?parent) (:filter (not (isblank ?parent))))
			(:optional (?class !skos:definition ?documentation))
			(?class !skos:prefLabel ?label)
			(:filter (not (isblank ?class)))
			)
		      :kb bfo :use-reasoner :none)
	      for classid = (id-for-bfo-class class)
	      for parentid = (id-for-bfo-class parent)
	      do
	      (as `(declaration (class ,classid)))
	      (when parent (as `(subclass-of ,classid ,parentid)))
	      (when doc
		(as `(annotation-assertion ,definition ,classid ,(@en (#"replaceAll" doc "\\[.*?\\]" "")))))
	      (as
	       `(annotation-assertion !rdfs:label ,classid ,(@en label))
	       `(annotation-assertion ,editor-preferred-label ,classid ,(@en label))
	       )
	      (loop for (example) in 
		   	      (sparql `(:select (?example) ()
					(,class !skos:example ?example))
				      :kb bfo :use-reasoner :none)
		   do
		   (as `(annotation-assertion ,example-of-usage ,classid ,(@en example))))
	      (loop for (syn) in 
		   	      (sparql `(:select (?syn) ()
					(,class !skos:altLabel ?syn))
				      :kb bfo :use-reasoner :none)
		   do
		   (as `(annotation-assertion ,alternative-term ,classid ,(@en syn))))
	      ))))
  (write-rdfxml bfo "~/repos/bfo/trunk/src/ontology/bfo2.owl"))



      