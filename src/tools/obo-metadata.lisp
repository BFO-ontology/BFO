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
    ("bfo-owl-spec-label" !obo:BFO_0000179)
    ("bfo-fol-spec-label" !obo:BFO_0000180)
    ("axiom-nl" !obo:IAO_0000601)
    ("axiom-fol" !obo:IAO_0000602)
    ("axiomid" !obo:IAO_0010000)
    ))

(defmacro with-obo-metadata-uris (&body body)
  `(let-uri ,*bfo2-ontprops*
     ,@body
     ))
