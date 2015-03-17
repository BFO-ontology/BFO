;; Not finished


;; <Project xmlns="http://usefulinc.com/ns/doap#"
;;         xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
;;         xmlns:foaf="http://xmlns.com/foaf/0.1/">

;;         <name>DOAP</name>
;;         <homepage rdf:resource="http://usefulinc.com/doap" />
;;         <created>2004-05-04</created>
;;         <shortdesc xml:lang="en">
;;             Tools and vocabulary for describing community-based
;;             software projects.
;;         </shortdesc>
;;         <description xml:lang="en">
;;             DOAP (Description of a Project) is an RDF vocabulary and
;;             associated set of tools for describing community-based software
;;             projects.  It is intended to be an interchange vocabulary for
;;             software directory sites, and to allow the decentralized
;;             expression of involvement in a project.
;;         </description>
;;         <maintainer>
;;             <foaf:Person>
;;                 <foaf:name>Edd Dumbill</foaf:name>
;;                 <foaf:homepage rdf:resource="http://usefulinc.com/edd" />
;;             </foaf:Person>
;;         </maintainer>
;; </Project>

;; expects the variables 

(Declaration (class !doap:Project))
(AnnotationAssertion !rdfs:isDefinedBy !doap:Project <http://usefulinc.com/ns/doap>)
(Declaration (class !doap:SVNRepository))
(AnnotationAssertion !rdfs:isDefinedBy !doap:SVNRepository <http://usefulinc.com/ns/doap>)
(Declaration (class !doap:Version))
(AnnotationAssertion !rdfs:isDefinedBy !doap:Version <http://usefulinc.com/ns/doap>)
(Declaration ObjectProperty(!doap:browse))
(AnnotationAssertion !rdfs:isDefinedBy !doap:browse <http://usefulinc.com/ns/doap>)
(Declaration ObjectProperty(!doap:bug-database))
(AnnotationAssertion !rdfs:isDefinedBy !doap:bug-database <http://usefulinc.com/ns/doap>)
(Declaration ObjectProperty(!doap:file-release))
(AnnotationAssertion !rdfs:isDefinedBy !doap:file-release <http://usefulinc.com/ns/doap>)
(Declaration ObjectProperty(!doap:homepage))
(AnnotationAssertion !rdfs:isDefinedBy !doap:homepage <http://usefulinc.com/ns/doap>)
(Declaration ObjectProperty(!doap:mailing-list))
(AnnotationAssertion !rdfs:isDefinedBy !doap:mailing-list <http://usefulinc.com/ns/doap>)
(Declaration ObjectProperty(!doap:release))
(AnnotationAssertion !rdfs:isDefinedBy !doap:release <http://usefulinc.com/ns/doap>)
(Declaration ObjectProperty(!doap:repository))
(AnnotationAssertion !rdfs:isDefinedBy !doap:repository <http://usefulinc.com/ns/doap>)
(Declaration ObjectProperty(!doap:wiki))
(AnnotationAssertion !rdfs:isDefinedBy !doap:wiki <http://usefulinc.com/ns/doap>)
(Declaration DataProperty(!doap:revision))
(AnnotationAssertion !rdfs:isDefinedBy !doap:revision <http://usefulinc.com/ns/doap>)

(ClassAssertion !doap:Project <http://purl.obolibrary.org/obo/bfo/project>)

; (ObjectPropertyAssertion !doap:browse <http://purl.obolibrary.org/obo/bfo/project> <http://purl.obolibrary.org/obo/bfo/browse>)
(ObjectPropertyAssertion !doap:bug-database <http://purl.obolibrary.org/obo/bfo/project> <http://purl.obolibrary.org/obo/bfo/tracker>)
(ObjectPropertyAssertion !doap:homepage <http://purl.obolibrary.org/obo/bfo/project> <http://purl.obolibrary.org/obo/bfo>)
(ObjectPropertyAssertion !doap:mailing-list <http://purl.obolibrary.org/obo/bfo/project> <mailto:bfo-discuss@googlegroups.com>)
;(ObjectPropertyAssertion !doap:release <http://purl.obolibrary.org/obo/bfo/project> <http://purl.obolibrary.org/obo/bfo/version-__VERSION_GOES_HERE__>)
(ObjectPropertyAssertion !doap:repository <http://purl.obolibrary.org/obo/bfo/project> <http://purl.obolibrary.org/obo/bfo/repository>)
(ObjectPropertyAssertion !doap:wiki <http://purl.obolibrary.org/obo/bfo/project> <http://purl.obolibrary.org/obo/bfo/wiki>)

(ClassAssertion !doap:SVNRepository <http://purl.obolibrary.org/obo/bfo/repository>)

(AnnotationAssertion !rdfs:label <http://purl.obolibrary.org/obo/bfo/version-__VERSION_GOES_HERE__> \"__PRETTY_NAME_GOES_HERE__\")

(AnnotationAssertion !rdfs:seeAlso
	 <http://purl.obolibrary.org/obo/bfo/version-__VERSION_GOES_HERE__>
	 <http://purl.obolibrary.org/obo/bfo/wiki/Releases/__VERSION_GOES_HERE__>)

(ClassAssertion !doap:Version <http://purl.obolibrary.org/obo/bfo/version-__VERSION_GOES_HERE__>)

(ObjectPropertyAssertion !doap:file-release <http://purl.obolibrary.org/obo/bfo/version-__VERSION_GOES_HERE__> <http://purl.obolibrary.org/obo/bfo/__VERSION_GOES_HERE__/bfo.owl>)

;(ObjectPropertyAssertion !doap:file-release <http://purl.obolibrary.org/obo/bfo/version-__VERSION_GOES_HERE__> <http://purl.obolibrary.org/obo/bfo.owl>)

;(DataPropertyAssertion !doap:revision <http://purl.obolibrary.org/obo/bfo/version-__VERSION_GOES_HERE__> \"SVN $Revision: 2469 $\")

