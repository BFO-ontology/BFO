(defun comment-obo-ids-in-owl-file (in-path out-path)
  (let ((kb (load-ontology in-path)))
    (let ((labels (rdfs-labels kb)))
      (with-open-file (in in-path :external-format :utf-8)
	(with-open-file (out out-path :direction :output :if-does-not-exist :create :if-exists :supersede :external-format :utf-8)
	  (loop for line = (read-line in nil :eof)
	     until (eq line :eof)
	     for prop-replaced = (replace-all line  "<obo:(\\w+_\\d+)"
					      (lambda(id)
						(format nil "<!-- ~a --><obo:~a" 
							(car
							 (gethash
							  (make-uri (format nil "http://purl.obolibrary.org/obo/~a" id)
								    ) labels))
							id)) 1)
	     for entity-replaced = (replace-all prop-replaced  "(rdf:(about|resource)=\"http://purl.obolibrary.org/obo/(\\w+_\\d+)\"/{0,1}>)"
						(lambda(whole id)
						  (format nil "~a<!-- ~a -->"  
				      
							  whole
							  (car (gethash
								(make-uri (format nil "http://purl.obolibrary.org/obo/~a" id)
									  ) labels))))
						1 3)
	     
	     do
	     (write-string entity-replaced out)
	     (terpri out)))))))