;; annotations defining the annotations used specifically for BFO development

(annotation-assertion !rdfs:label !bfo-owl-spec-label "BFO OWL specification label@en")
(annotation-assertion !definition !bfo-owl-spec-label "Relates an entity in the ontology to the name of the variable that is used to represent it in the code that generates the BFO OWL file from the lispy specification.@en")
(annotation-assertion !definition-source !bfo-spec-label "Person:Alan Ruttenberg")
(annotation-assertion !curator-note !bfo-owl-spec-label "Really of interest to developers only@en")
(sub-annotation-property-of !bfo-owl-spec-label !rdfs:label)

(annotation-assertion !rdfs:label !bfo-fol-spec-label "BFO CLIF specification label@en")
(annotation-assertion !definition !bfo-fol-spec-label "Relates an entity in the ontology to the term that is used to represent it in the the CLIF specification of BFO2@en")
(annotation-assertion !definition-source !bfo-fol-spec-label "Person:Alan Ruttenberg")
(annotation-assertion !curator-note !bfo-fol-spec-label "Really of interest to developers only@en")
(sub-annotation-property-of !bfo-fol-spec-label !rdfs:label)

