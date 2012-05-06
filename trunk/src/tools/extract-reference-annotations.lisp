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
		do (write-string (car match) s) 
		)
	     )))
      (all-matches extracted-text  "(?i)(as|a|axiom)\\((.*?)\\)\\[(.*?)\\]" 1 2 3))))

;; simpler, but doesn't see hidden text
;; (#"parseToString" (new 'tika) (new 'file (translate-logical-pathname "bfo:docs;bfo2-reference;BFO2-Reference.docx")))

