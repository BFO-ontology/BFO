(defmacro private-axioms (&body axs)
  (loop for ax in axs
     if (eq (car ax) 'comment)
     collect `(comment ,(cadr ax) (,(caaddr ax) (annotation !rdfs:comment (:literal "to support reasoning only" "@en")) ,@(cdaddr ax)))
     else 
     collect `(,(car ax) (annotation !rdfs:comment "to support reasoning only") ,@(cdr ax))
     ))

(defmacro comment (comment &body axiom)
  `(,(caar axiom) (annotation !rdfs:comment (:literal ,comment "@en")) ,@(cdar axiom)))

(defmacro declare-labeled (label form &body comment)
  `((declaration ,form) (annotation-assertion !rdfs:label ,(second form) (:literal ,label "@en"))
	  ,@(if comment `((annotation-assertion !rdfs:comment ,(second form) (:literal ,(car comment) "@en"))))
	  ))
  


(with-ontology ()
    (
     ;; from BFO
     (declare-labeled "continuant" (class !continuant))
     (declare-labeled "occurrent" (class !occurrent))
     (disjointclasses !continuant !occurrent)
     (declare-labeled "process" (class !process))
     (sub-class-of !process !occurrent)
     (declare-labeled "participates in" (object-property !participates_in))
     (declare-labeled "has participant" (object-property !has_participant))
     (object-property-domain !has_participant !process)
     (object-property-range !has_participant !continuant)
     (inverse-object-properties !participates_in !has_participant)

     (declare-labeled "course" (class !course)
       "At each moment a continuant is located in a spatial region. The sum across time of these spatial regions amounts to a spatiotemporal region. The course of a continuant is the sum of all processes that are located in that spatiotemporal region. These processes may be wholly located, or may be parts of other processes cut so that the part's location is wholly located.")
     (subclass-of !course !process)
     (declare-labeled "has course" (object-property !has_course) "relates a continuant to a course to a unique course")
     (declare-labeled "course of" (object-property !course_of) "relates a course to unique continuant")
     (inverse-object-properties !has_course !course_of)
     (functionalobject-property !has_course)
     (inverse-functional-object-property !has_course)
     (object-property-domain !has_course !continuant)
     (object-property-range !has_course !course)
     (inverse-object-properties !has_course !course_of)

     (private-axioms
       (declare-labeled "(temporal part of course) of" (object-property !temporal_part_of_course_of) "relates a temporal part of a course to unique continuant. A temporal part is one which has no spatial projection outside a temporal region, and whose spatial projection within the temporal region is exactly the spatial projection of the course.")
       (object-property-domain !temporal_part_of_course_of (object-some-values-from !part_of_process !course)) 
       (object-property-range !temporal_part_of_course_of !continuant)
       (functional-object-property !temporal_part_of_course_of))

     (private-axioms

       ;; define the "man-man" properties

       (declare-labeled "self c"  (object-property !self_c)
	 "A property that relates every continuant instance to itself")
       (comment "make it self loop"
	 (sub-class-of !continuant (object-has-self !self_c)))

       (object-property-domain !self_c !c) ; necessary?
       (object-property-range !self_c !c)  ; necessary?

       (declare-labeled "self p" (object-property !self_p)
	 "A property that relates every process instance to itself")

       (comment "enforce that self_p only relates processes"
	 (object-property-domain !self_p !p))
       (comment "enforce that self_p only relates processes"
	 (object-property-range !self_p !p))

       (comment "make it loop"
	 (sub-class-of !process (object-has-self !self_p))

     ))

    ;; axioms specific to parthood

     (declare-labeled "has part at all times" (object-property !has_part_at_all_times)
       "c1 has part at all times c2 means forall(t) if exists(c1,t) then exists(c2,t) and part_of(c1,c2,t)")

     (declare-labeled "has part at some time" (object-property !has_part_at_some_time)
       "c1 has part at some time c2 means there is some t such that exists(c1,t) and exists(c2,t) and part_of(c1,c2,t)")

     (declare-labeled "has part during"  (object-property !has_part_during)
       "c1 has part during p2 means forall(t) if t is in the temporal projection of p2, and if instance_of(p2,course) and course_of(c2,p2) then part_of(c1,c2,t) or if temporal_part_of_course_of(p2,p2c) and course_of(c2,p2c) then part_of(c1,c2,t)")
     (object-property-domain !has_part_during !continuant)
     (object-property-range !has_part_during (object-union-of !course (object-some-values-from !temporal_part_of_course_of !course)))
       
     (declaration (object-property !part_of_at_all_times))
     (declaration (object-property !part_of_at_some_time))
     (declaration (object-property !part_of_during))
     (sub-object-property-of !self_c !has_part_at_all_times)
     (sub-object-property-of !self_c !part_of_at_all_times)


     (declare-labeled "is part of process" (object-property !part_of_process) 
       "relates processes to other processes that they are part of")

     (declare-labeled "has part that is process"  (object-property !process_has_part)
       "relates processes to their process parts")

     (comment "since process parthood is atemporal, these are inverse still"
	 (inverse-object-properties !process_has_part !part_of_process))
     (comment "process parthood is transitive"
       (transitive-object-property !part_of_process))
     (comment "process parthood is transitive"
       (transitive-object-property !process_has_part))
     
     (private-axioms
       (comment "every process is part of itself"
	 (sub-object-property-of !self_p !part_of_process)))

     (object-property-domain !part_of_process !process)
     (object-property-range !part_of_process !process)

     ;; now the terms that give us some further entailments via chaining
     (comment "if you 
     (sub-object-property-of (object-property-chain !has_part_during !course_of) !has_part_at_some_time)
     (sub-object-property-of (object-property-chain !part_of_during !course_of) !part_of_at_some_time)
     (sub-object-property-of (object-property-chain !part_of_during !part_of_process) !part_of_during)
     (sub-object-property-of (object-property-chain !has_part_during !part_of_process) !has_part_during)


     ;; proper part of experiment
     (declaration (object-property !has_proper_part_at_all_times))
     (declaration (object-property !proper_part_of_processs))
     (disjoint-object-properties !has_proper_part_at_all_times !self_c)
     (disjoint-object-properties !proper_part_of_processs !self_p)
     (disjoint-object-properties !self_c !has_proper_part_at_all_times)
     (disjoint-object-properties !self_p !proper_part_of_processs)




       (sub-object-property-of (object-property-chain !general_part_of !self_c) !part_of_at_all_times)
       (sub-object-property-of (object-property-chain !general_part_of !self_p) !part_of_process)
       (sub-object-property-of (object-property-chain !general_has_part !self_p) !process_has_part))
       (sub-object-property-of !self_p !general_has_part)
       (sub-object-property-of !self_p !general_part_of)
       (sub-object-property-of !self_c !general_has_part)
       (sub-object-property-of !self_c !general_part_of)






     (annotation-assertion !rdfs:label !has_part_at_all_times "has part at all times"@en)
     (sub-object-property-of !has_part_at_all_times !has_part_at_some_time)
     (transitive-object-property !has_part_at_all_times)

     (annotation-assertion !rdfs:label !has_part_at_some_time "has part at some time"@en)
     (annotation-assertion !rdfs:label !has_part_during "has part during"@en)
     (object-property-domain !has_part_during !continuant)
     (object-property-range !has_part_during !occurrent)

     (annotation-assertion !rdfs:label !has_proper_part_at_all_times "has proper part at all times"@en)
     (sub-object-property-of !has_proper_part_at_all_times !has_part_at_all_times)
     (asymmetricobject-property !has_proper_part_at_all_times)
     (annotation-assertion !rdfs:label !part_of_at_all_times "part of at all times")
     (sub-object-property-of !part_of_at_all_times !part_of_at_some_time)
     (transitive-object-property !part_of_at_all_times)
     (object-property-domain !part_of_at_all_times !continuant)
     (object-property-range !part_of_at_all_times !continuant)
     (annotation-assertion !rdfs:label !part_of_at_some_time "part of at some time")
     (object-property-domain !part_of_at_some_time !continuant)
     (object-property-range !part_of_at_some_time !continuant)

     (annotation-assertion !rdfs:label !part_of_during "part of during")
     (object-property-domain !part_of_during !continuant)
     (object-property-range !part_of_during !occurrent)

     (annotation-assertion !rdfs:label !part_of_process "part of process")


     (annotation-assertion !rdfs:label !process_has_part "process has part"@en)

     (annotation-assertion !rdfs:label !proper_part_of_processs "proper part of processs"@en)
     (sub-object-property-of !proper_part_of_processs !part_of_process)


     ;; small tests.

     (differentindividuals !c1 !c2 !c3 !o1 !o2 !o3)
     (declaration (named-individual !c1))
     (annotation-assertion !rdfs:label !c1 "c1")
     (classassertion !c !c1)
     (object-property-assertion !part_of_at_all_times !c1 !c2)
     (declaration (named-individual !c2))
     (annotation-assertion !rdfs:label !c2 "c2")
     (classassertion !c !c2)
     (declaration (named-individual !c3))
     (annotation-assertion !rdfs:label !c3 "c3"@en)
     (declaration (named-individual !o1))
     (annotation-assertion !rdfs:label !o1 "o1")
     (classassertion !o !o1)
     (object-property-assertion !part_of_process !o1 !o2)
     (declaration (named-individual !o2))
     (annotation-assertion !rdfs:label !o2 "o2")
     (classassertion !o !o2)
     (declaration (named-individual !o3))
     (annotation-assertion !rdfs:label !o3 "o3"@en)


     ;; general part of, to not have to worry about continuant/occurrent distinction

     (sub-class-of !continuant (object-all-values-from !general_part_of !continuant))
     (sub-class-of !occurrent (object-all-values-from !general_part_of !occurrent))
     (declaration (object-property !general_has_part))
     (annotation-assertion !rdfs:label !general_has_part "general has part"@en)
     (declaration (object-property !general_part_of))
     (annotation-assertion !rdfs:label !general_part_of "general part of")
     (transitive-object-property !general_part_of)
     (object-property-domain !general_part_of (objectunionof !occurrent !continuant))
     (object-property-range !general_part_of (objectunionof !occurrent !continuant))
     (object-property-assertion !general_part_of !continuant3 !continuant2)
     (object-property-assertion !general_part_of !occurrent3 !occurrent2)
     (sub-class-of (object-some-values-from !general_part_of !continuant) !continuant)
     (sub-class-of (object-some-values-from !general_part_of !occurrent) !occurrent)

     ))