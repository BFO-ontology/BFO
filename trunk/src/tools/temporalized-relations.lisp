(defmacro @en (string)
  `(concatenate 'string ,string "@en"))

(defmacro private-axioms (&body axs)
  (list 'quote 
	(loop for ax in axs
	   if (eq (car ax) 'comment)
	   collect `(comment ,(cadr ax) (,(caaddr ax) (annotation !rdfs:comment ,(@en "to support reasoning only")) ,@(cdaddr ax)))
	   else if (eq (car ax) 'declare-labeled)
	   collect `(declare-labeled ,(second  ax) ,(third ax) ,(fourth ax) (annotation !rdfs:comment ,(@en "to support reasoning only")))
	   else 
	   collect `(,(car ax) (annotation !rdfs:comment ,(@en "to support reasoning only")) ,@(cdr ax))
	   )))

(defmacro comment (comment &body axiom)
  `'((,(caar axiom) (annotation !rdfs:comment ,(@en comment)) ,@(cdar axiom))))

(defmacro declare-labeled (label form &optional comment annotation)
  `'((declaration ,@(if annotation (list annotation) nil) ,form) (annotation-assertion ,@(if annotation (list annotation) nil) !rdfs:label ,(second form) ,(@en label))
	  ,@(if comment `((annotation-assertion ,@(if annotation (list annotation) nil)!rdfs:comment ,(second form) ,(@en comment))))
	  ))

(def-uri-alias "continuant" !obo:BFO_0000002)
(def-uri-alias "occurrent" !obo:BFO_0000003)
(def-uri-alias "independent_continuant" !obo:BFO_0000004)
(def-uri-alias "process" !obo:BFO_0000007)
(def-uri-alias "temporal_region" !obo:BFO_0000008)
(def-uri-alias "material_entity" !obo:BFO_0000040)
(def-uri-alias "site" !obo:BFO_0000029)
(def-uri-alias "definition" !obi:IAO_0000115)
(def-uri-alias "definition-source" !obi:IAO_0000119)
(def-uri-alias "definition-editor" !obi:IAO_0000117)
(def-uri-alias "preferred-term" !obi:IAO_0000111)
(def-uri-alias "alternative-term" !obi:IAO_0000118)
(def-uri-alias "example-of-usage" !obi:IAO_0000112)
(def-uri-alias "curation-status" !obi:IAO_0000078)
(def-uri-alias "editor-note" !obo:IAO_0000116)
(def-uri-alias "curator-note" !obo:IAO_0000232)

(defparameter participant-class !continuant)

(with-ontology temporalized-relations (:collecting t :base "http://purl.obolibrary.org/obo/example/")
    ( ;; from BFO
     (as
      (declare-labeled "continuant" (class !continuant))
      (declare-labeled "occurrent" (class !occurrent))
      '(disjoint-classes !continuant !occurrent)
      (declare-labeled "process" (class !process))
      '(sub-class-of !process !occurrent)
      (declare-labeled "material entity" (class !material_entity))
      '(sub-class-of !material_entity !continuant)
      (declare-labeled "temporal region" (class !temporal_region))
      '(sub-class-of !temporal_region !occurrent)
      (declare-labeled "site" (class !site))
      '(sub-class-of !site !continuant)

      ;;****************************************************************
      ;; participation

      (declare-labeled "participates in at some time" (object-property !participates_in_at_some_time))
      (declare-labeled "has participant at some time" (object-property !has_participant_at_some_time))
      '(inverse-object-properties !participates_in_at_some_time !has_participant_at_some_time)
      '(annotation-assertion !alternative-term !participates_in_at_some_time  "participates in" )
      '(annotation-assertion !alternative-term !has_participant_at_some_time  "has participant" )

      '(annotation-assertion !editor-note !has_participant_at_some_time
			    "This corresponds to the quantification over time as was stated for the class relation definition in Relations in Biomedical Ontology

A suitable target for participates_in relations in the OBO to OWL conversion")

      '(annotation-assertion !definition !has_participant_at_some_time "A relation between a process and a continuant where there exists some temporal region that is part of the temporal projection of the process at which the continant participates in the process. ")

      '(annotation-assertion !definition !participates_in_at_some_time "A relation between a continuant and a process where there exists some temporal region that is part of the temporal projection of the process at which the continant participates in the process. ")

      '(annotation-assertion !editor-note !participates_in_at_some_time "This corresponds to the quantification over time as was stated for the class relation definition in Relations in Biomedical Ontology

A suitable target for participates_in relations in the OBO to OWL conversion")

      '(object-property-domain !has_participant_at_some_time !process)

      `(object-property-range !has_participant_at_some_time ,participant-class)

      `(object-property-domain (annotation !editor-note "bfo2 reference has independent continuant as domain but hopefull will change")
	!participates_in_at_some_time ,participant-class)

      '(object-property-range (annotation !editor-note "bfo2 reference has occurrent as target but hopefull will change")
	!participates_in_at_some_time !process)

      '(inverse-object-properties !participates_in_at_some_time !has_participant_at_some_time)

      ;; ****************************************************************
      ;; Courses and segments
 
      (declare-labeled "course" (class !course)
		       "At each moment a material entity is located in a spatial region. The stitching together of these across time amounts to a spatiotemporal region. The course of a material entity is the sum of all processes (fiat or bona fide) that are located in that spatiotemporal region. These processes may be wholly located, or may be parts of other processes cut so that the part's location is wholly located(so fiat). Each material entity is in a 1:1 correspondence with its course.")

      (declare-labeled "course segment" (class !course_segment)
		       "A temporal part of a course")

      '(subclass-of !course !course_segment)
      '(subclass-of !course (object-all-values-from !has_temporal_part !course_segment))
      '(subclass-of !course_segment !process)
      '(equivalent-classes !course_segment (object-some-values-from !temporal_part_of !course))

      (declare-labeled "has course" (object-property !has_course) "relates a material entity to it's unique course")
      (declare-labeled "is course of" (object-property !course_of) "relates a course to unique material entity")

      '(inverse-object-properties !has_course !course_of)
      '(functional-object-property !has_course)
      '(functional-object-property !course_of)
      '(object-property-domain !has_course !material_entity)
      '(object-property-range !has_course !course)
      '(object-property-domain !has_course !material_entity)
      '(object-property-range !has_course !course)
      '(object-property-range !course_of !material_entity)
      '(object-property-domain !course_of !course)

      '(annotation-assertion !editor-note "Should there be courses of populations (which can include progeny)? It seems yes, BUT, if we do that then we have a problem mapping course_of uniquely back to a continuant because: imagine first organism course. Part of that course would map to either the original or the population, ambiguously. So we have a potential problem with uniquely mapping course parts.")
       
      '(annotation-assertion !editor-note !course "Sometimes called the life of an independent continuant, or the existence (process) of an independent continuant. The 1:1 relation has-course relates the continuant to the course.

We do not extend courses to dependent continuants for now. Given the current definition and the BFO definition of qualities having the same spatial region as their bearers, this definition contradicts the 1:1 nature of the relation. Something may be done about this - there are several discussions occurring at the moment. We do believe that this mapping is 1:1. 

Options: 

1) Say something about the participation of the continuant in the processes of the course. Downside: might be hard to do. There is still disagreement on how to understand participation.

2) Reexamin BFO's definition of the spatial region of dependent continuants

3) Restrict course to be processes associated with material entities and look for another solution for dependent continuants

...")

      '(annotation-assertion !editor-note  "Another formulation of the definition: A course is the unique process associated with an independent continuant which the sum total of [fiat or bona fide] processes whose spatial projection at any time is part of the spatial region occupied by the independent continuant.")

     (declare-labeled "is segment of course of" (object-property !segment_of_course_of) "relates a temporal part of a course to the unique material entity whose course the segment is temporal part of.")

     '(object-property-domain !segment_of_course_of !course_segment) 

     '(object-property-range !segment_of_course_of !material_entity)

     '(functional-object-property !segment_of_course_of)

     '(sub-object-property-of !course_of !segment_of_course_of)

     (declare-labeled "is temporal part of" (object-property !temporal_part_of) "a is temporal part of b if a and b are occurrents, and there is no part of b that doesn't overlap a and occupies a temporal region that overlaps the temporal region of a.")

     (declare-labeled "has temporal part" (object-property !has_temporal_part) "")
     '(object-property-domain !temporal_part_of !occurrent)
     '(object-property-range !temporal_part_of !occurrent)
     '(object-property-range !has_temporal_part !occurrent)
     '(object-property-domain !has_temporal_part !occurrent)
     '(transitive-object-property !temporal_part_of)
     '(transitive-object-property !has_temporal_part)
     '(sub-object-property-of !self_o !temporal_part_of)
     '(sub-object-property-of !self_o !has_temporal_part)
     '(sub-object-property-of (object-property-chain !part_of_occurrent !self_t) !temporal_part_of)
     '(sub-object-property-of (object-property-chain !occurrent_has_part !self_t) !has_temporal_part)


     (declare-labeled "is segment of course" (object-property !is_segment_of_course) "a is segment of course of b if a is a course segment and b is a course and a is a temporal part of b.")
     '(object-property-range !is_segment_of_course !course)
     '(object-property-domain !is_segment_of_course !course_segment)
     '(functional-object-property !is_segment_of_course)
     '(sub-object-property-of !is_segment_of_course !temporal_part_of)

;;; ****************************************************************
     ;; self properties

     ;; self continuant
     (declare-labeled "self c"  (object-property !self_c)
		      "A property that relates every continuant instance to itself")
     (comment "make it self loop"
	      (sub-class-of !continuant (object-has-self !self_c)))

     '(object-property-domain !self_c !continuant)   ; necessary?
     '(object-property-range !self_c !continuant)    ; necessary?

     ;; self process
     (declare-labeled "self p" (object-property !self_p)
		      "A property that relates every process instance to itself")

     (comment "enforce that self_p only relates processes"
	      (object-property-domain !self_p !process))
     (comment "enforce that self_p only relates processes"
	      (object-property-range !self_p !process))

     (comment "make it loop"
	      (sub-class-of !process (object-has-self !self_p))
	      )

     ;; self occurrent
     (declare-labeled "self o" (object-property !self_o)
		      "A property that relates every occurent instance to itself")

     (comment "enforce that self_o only relates occurrents"
	      (object-property-domain !self_o !occurrent))
     (comment "enforce that self_o only relates occurrents"
	      (object-property-range !self_o !occurrent))

     (comment "make it loop"
	      (sub-class-of !occurrent (object-has-self !self_o))
	      )


     ;; self temporal region
     (declare-labeled "self t" (object-property !self_t)
		      "A property that relates every temporal region to itself")

     (comment "enforce that self_t only relates occurrents"
	      (object-property-domain !self_t !temporal_region))
     (comment "enforce that self_t only relates occurrents"
	      (object-property-range !self_t !temporal_region))

     (comment "make it loop"
	      (sub-class-of !temporal_region (object-has-self !self_t))
	      )

;;; ****************************************************************

     ;; parthood

     ;; between continuants

     (declare-labeled "has part at all times" (object-property !has_part_at_all_times)
		      "c1 has part at all times c2 means forall(t) if exists(c1,t) then exists(c2,t) and part_of(c2,c1,t). This is known as permanent specific parthood in that the had part is the same particular throughout")

     (declare-labeled "has part at some time" (object-property !has_part_at_some_time)
		      "c1 has part at some time c2 means there is some t such that exists(c1,t) and exists(c2,t) and part_of(c2,c1,t)")

     (declare-labeled "has part during"  (object-property !has_part_during)
		      "c1 has part during p2 means forall(t) if t is in the temporal projection of p2, and if instance_of(p2,course) and course_of(c2,p2) then part_of(c1,c2,t) or if segment_of_course_of(p2,p2c) and course_of(c2,p2c) then part_of(c1,c2,t)")
     '(object-property-domain !has_part_during !material_entity)
     '(object-property-range !has_part_during  !course_segment)
       
     (declare-labeled "part of at all times" (object-property !part_of_at_all_times)
		      "c1 part of at all times c2 means forall(t) if exists(c1,t) then exists(c2,t) and part_of(c1,c2,t). This is known as permanent specific parthood in that the had part is the same particular throughout")

     (declare-labeled "part of at some time" (object-property !part_of_at_some_time)
		      "c1 part of at some time c2 means there is some t such that exists(c1,t) and exists(c2,t) and part_of(c1,c2,t)")

     '(sub-object-property-of (annotation !editor-note "local reflexivity") !self_c !has_part_at_all_times)
     '(sub-object-property-of (annotation !editor-note "local reflexivity") !self_c !part_of_at_all_times)

     '(object-property-domain !part_of_at_some_time !continuant)
     '(object-property-range !part_of_at_some_time !continuant)

     '(object-property-domain !part_of_at_all_times !continuant)
     '(object-property-range !part_of_at_all_times !continuant)

     '(sub-object-property-of !part_of_at_all_times !part_of_at_some_time)
     '(transitive-object-property !part_of_at_all_times)

     '(object-property-domain !part_of_at_all_times !continuant)
     '(object-property-range !part_of_at_all_times !continuant)

     ; between (indirectly) material entities

     (declare-labeled "has part during"  (object-property !has_part_during)
		      "c1 has part during p2 means forall(t) if t is part of the temporal projection of p2, and if instance_of(p2,course) and course_of(c2,p2) then has_part(c1,c2,t) or if segment_of_course_of(p2,p2c) and course_of(c2,p2c) has_part(c1,c2,t)")

     (declare-labeled "part of during"  (object-property !part_of_during)
		      "c1 part of during p2 means forall(t) if t is part of the temporal projection of p2, and if instance_of(p2,course) and course_of(c2,p2) then part_of(c1,c2,t) or if segment_of_course_of(p2,p2c) and course_of(c2,p2c) then part_of(c1,c2,t)")

     '(object-property-domain !part_of_during !material_entity)
     '(object-property-range !part_of_during  !course_segment)

     '(annotation-assertion !editor-note !part_of_during "Note that this relation has domain material entity since in order for this relation to be of use it needs to relate to a course segment, which only material entities have")

     '(annotation-assertion !editor-note !has_part_during "Note that this relation has domain material entity since in order for this relation to be of use it needs to relate to a course segment, which only material entities have")

     ;; between occurrents

     (declare-labeled "is part of occurrent" (object-property !part_of_occurrent) 
		      "relates occurrents to other occurrents that they are part of")

     (declare-labeled "has part that is occurrent"  (object-property !occurrent_has_part)
		      "relates occurrent to their occurrent parts")

     (comment "since occurrent parthood is atemporal, these are inverse"
	      (inverse-object-properties !occurrent_has_part !part_of_occurrent))
     
     (comment "occurrent parthood is transitive"
	      (transitive-object-property !part_of_occurrent))
     
     (comment "occurrent parthood is transitive"
	      (transitive-object-property !occurrent_has_part))
     
     (comment "every occurrent is part of itself"
	      (sub-object-property-of !self_o !part_of_occurrent))
     (comment "every occurrent has part itself"
	      (sub-object-property-of !self_o !occurrent_has_part))

     '(object-property-domain !part_of_occurrent !occurrent)
     '(object-property-range !part_of_occurrent !occurrent)

     '(sub-object-property-of !has_part_at_all_times !has_part_at_some_time)
     '(transitive-object-property !has_part_at_all_times)
     '(transitive-object-property !part_of_at_all_times)

     ;; now the terms that give us some further entailments via chaining

     '(sub-object-property-of (object-property-chain !has_part_during !course_of) !has_part_at_some_time)
     '(sub-object-property-of (object-property-chain !part_of_during !course_of) !part_of_at_some_time)
     '(sub-object-property-of (object-property-chain !has_part_during !segment_of_course_of) !has_part_at_some_time)
     '(sub-object-property-of (object-property-chain !part_of_during !segment_of_course_of) !part_of_at_some_time)

     '(sub-object-property-of (object-property-chain !part_of_during !has_temporal_part) !part_of_during)
     '(sub-object-property-of (object-property-chain !has_part_during !has_temporal_part) !has_part_during)

     ;; restrict part types
     ;;
     '(sub-class-of (object-some-values-from !part_of_occurrent !process) !process)
     '(sub-class-of (object-some-values-from !occurrent_has_part !process) !process)
     '(sub-class-of !process (object-all-values-from !occurrent_has_part !process))
     '(sub-class-of !process (object-all-values-from !part_of_occurrent !process))
     '(sub-class-of !material_entity (object-all-values-from !has_part_at_some_time (object-union-of !site !material_entity)))
     '(sub-class-of !material_entity (object-all-values-from !has_part_at_all_times (object-union-of !site !material_entity)))
     '(sub-class-of !material_entity (object-all-values-from !part_of_at_some_time !material_entity))
     '(sub-class-of !material_entity (object-all-values-from !part_of_at_all_times !material_entity))
     '(sub-class-of !site (object-all-values-from !part_of_at_all_times (object-union-of !material_entity !site)))
     '(sub-class-of !site (object-all-values-from !part_of_at_some_time (object-union-of !material_entity !site)))
     '(sub-class-of !site (object-all-values-from !has_part_at_all_times !site))
     '(sub-class-of !site (object-all-values-from !has_part_at_some_time !site))

     '(sub-class-of (object-some-values-from !part_of_at_some_time !material_entity) (object-union-of !material_entity !site))
     '(sub-class-of (object-some-values-from !has_part_at_all_times !material_entity) !material_entity)
     '(sub-class-of (object-some-values-from !has_part_during !process) (object-union-of !material_entity !site))
     '(sub-class-of (object-some-values-from !occurrent_has_part !process) !process)
     `(sub-class-of !temporal_region (object-all-values-from !occurrent_has_part !temporal_region))
     `(sub-class-of (object-some-values-from !occurrent_has_part !temporal_region) !temporal_region )
     `(sub-class-of !temporal_region (object-all-values-from !part_of_occurrent !temporal_region))
     `(sub-class-of (object-some-values-from !part_of_occurrent !temporal_region) !temporal_region )



     ;; proper part of 

     '(declaration (object-property !has_proper_part_at_all_times))
     '(declaration (object-property !proper_part_of_occurrent))
     '(disjoint-object-properties !has_proper_part_at_all_times !self_c)
     '(disjoint-object-properties !proper_part_of_occurrent !self_o)
     '(disjoint-object-properties !self_c !has_proper_part_at_all_times)
     '(disjoint-object-properties !self_o !proper_part_of_occurrent)

     ;; General part of

     (declare-labeled  "general has part" (object-property !general_has_part) "A has part relation that can be applied to either occurrents or continuants. for occurrents it is the timeless has part and for continuants it is  has permanent specific part")
     (declare-labeled "general part of" (object-property !general_part_of) "A part of relation that can be applied to either occurrents or continuants. for occurrents it is the timeless part of and for continuants it is permanent part of specific ")

     '(sub-object-property-of (object-property-chain !general_part_of !self_c) !part_of_at_all_times)
     '(sub-object-property-of (object-property-chain !general_part_of !self_o) !part_of_occurrent)
     '(sub-object-property-of (object-property-chain !general_has_part !self_o) !occurrent_has_part)
     '(sub-object-property-of (object-property-chain !general_has_part !self_c) !has_part_at_all_times)

     '(sub-object-property-of (annotation !editor-note "local reflexivity") !self_o !general_has_part)
     '(sub-object-property-of (annotation !editor-note "local reflexivity") !self_o !general_part_of)
     '(sub-object-property-of (annotation !editor-note "local reflexivity") !self_c !general_has_part)
     '(sub-object-property-of (annotation !editor-note "local reflexivity") !self_c !general_part_of)

     '(sub-class-of !continuant (object-all-values-from !general_part_of !continuant))
     '(sub-class-of !occurrent (object-all-values-from !general_part_of !occurrent))

     '(sub-class-of (object-some-values-from !general_part_of !continuant) !continuant )
     '(sub-class-of (object-some-values-from !general_part_of !occurrent) !occurrent)

     '(transitive-object-property !general_part_of)
     '(transitive-object-property !general_has_part)
     
     '(object-property-domain !general_part_of (objectunionof !occurrent !continuant))
     '(object-property-range !general_part_of (objectunionof !occurrent !continuant))
     '(object-property-domain !general_has_part (objectunionof !occurrent !continuant))
     '(object-property-range !general_has_part (objectunionof !occurrent !continuant))


;;; ****************************************************************
;;; Stages


      (declare-labeled "stage" (class !stage) "A stage of a continuant is a representation of the continuant that allows for predication over a temporal interval. Although an OWL individual, it does not represent an additional BFO particular.")
      '(annotation-assertion !alternative-term !stage "phase sortal")
      '(equivalent-classes !stage (object-some-values-from !stage_of !continuant))
      '(sub-class-of !stage !continuant)

      (declare-labeled "is stage of" (object-property !stage_of) "")
      '(functional-object-property !stage_of)
      '(object-property-domain !stage_of !stage)
      '(object-property-range !stage_of !continuant)
      
      (declare-labeled "has stage" (object-property !has_stage) "")
      '(object-property-domain !has_stage !continuant)
      '(object-property-range !has_stage !stage)

      (declare-labeled "transformation of" (object-property !transformation_of))
      '(object-property-domain !transformation_of !stage)
      '(object-property-range !transformation_of !stage)


;;; ****************************************************************

     '(annotation-assertion !rdfs:label !has_proper_part_at_all_times "has proper part at all times@en")
     '(sub-object-property-of !has_proper_part_at_all_times !has_part_at_all_times)
     '(asymmetric-object-property !has_proper_part_at_all_times)


     '(annotation-assertion !rdfs:label !proper_part_of_occurrent "is proper part of occurrent@en")
     '(sub-object-property-of !proper_part_of_occurrent !part_of_occurrent)


     ;; small tests.

     '(differentindividuals !c1 !c2 !c3 !o1 !o2 !o3)
     '(declaration (named-individual !c1))
     '(annotation-assertion !rdfs:label !c1 "c1")
     '(classassertion !continuant !c1)
     '(object-property-assertion !part_of_at_all_times !c1 !c2)
     '(declaration (named-individual !c2))
     '(annotation-assertion !rdfs:label !c2 "c2")
     '(classassertion !continuant !c2)
     '(declaration (named-individual !c3))
     '(annotation-assertion !rdfs:label !c3 "c3@en")
     '(declaration (named-individual !o1))
     '(annotation-assertion !rdfs:label !o1 "o1")
     '(classassertion !occurrent !o1)
     '(object-property-assertion !part_of_occurrent !o1 !o2)
     '(declaration (named-individual !o2))
     '(annotation-assertion !rdfs:label !o2 "o2")
     '(classassertion !occurrent !o2)
     '(declaration (named-individual !o3))
     '(annotation-assertion !rdfs:label !o3 "o3@en")
     '(object-property-assertion !general_part_of !continuant3 !continuant2)
     '(object-property-assertion !general_part_of !occurrent3 !occurrent2)



     ))
  (write-rdfxml temporalized-relations))

#|
(about !obo:example/course
       (!rdfs:label "course")
       (!obo:IAO_0000116 "Should there be courses of populations (which can include progeny)? It seems yes, BUT, if we do that then we have a problem mapping course_of uniquely back to a continuant because: imagine first organism course. Part of that course would map to either the original or the population, ambiguously. So we have a potential problem with uniquely mapping course parts.")
       (!obo:IAO_0000116 "Maybe trajectory is a better name. \"Vote\" says Barry.
Similary, we should vote as to whether this is a type in BFO.
Should we have \"course segment\".
")
       (!obo:IAO_0000116 "Sometimes called the life of an independent continuant, or the existence (process) of an independent continuant. The 1:1 relation has-course relates the continuant to the course.

We do not extend courses to dependent continuants for now. Given the current definition and the BFO definition of qualities having the same spatial region as their bearers, this definition contradicts the 1:1 nature of the relation. Something may be done about this - there are several discussions occurring at the moment. We do believe that this mapping is 1:1. 

Options: 

1) Say something about the participation of the continuant in the processes of the course. Downside: might be hard to do. There is still disagreement on how to understand participation.

2) Reexaming BFO's definition of the spatial region of dependent continuants

3) Restrict course to be processes associated with material entities and look for another solution for dependent continuants

...") (!obo:IAO_0000115 "A course is the unique process associated with an independent continuant which the sum total of [fiat or bona fide] processes whose spatial projection at any time is part of the spatial region occupied by the independent continuant.")
       ) 

(about !obo:example/has_proper_part_at_some_time
       (!obo:IAO_0000115 "For some time in temporal projection of the subject's course the subject has a proper part the object . ")
       (!rdfs:label "has proper part at some time")
       ) 

(about !obo:example/has_proper_part_at_all_times
       (!obo:IAO_0000116 "This relation will probably be removed - we can't define it as transitive as transitivity and disjoint properties together violate the OWL general constraints.
")
       (!rdfs:label "has proper part at all times")
       (!obo:IAO_0000115 "For all  times t in temporal projection of the subject's course the subject has a proper part the object at t")
       ) 

(about !obo:example/has_life
       (!obo:IAO_0000115 "relates an independent continuant to its unique course")
       (!rdfs:label "has course"))

(about !obo:example/has_part_at_some_time
       (!obo:IAO_0000116 "The relation is not transitive, as if a has part b at some t1 and b has part c and some time t2, the relation would only be transitive when t1 = t2, which isn't part of the definition.")
       (!rdfs:label "has part at some time")
       (!obo:IAO_0000115 "There exists some time instant in the temporal projection of the subject's course at which time the subject is part of the object. This implies the object exists at that time. ")
       ) 

 
(about !obo:example/has_part_during
       (!obo:IAO_0000116 "the constraint on temporal part of is that the part and the whole occupy the same spatial region at all times in the temporal projection of the whole. But the whole may extend longer, temporally")
       (!obo:IAO_0000115 "a continuant c has part during some process p iff p is a temporal segment of a course and for all times t in the temporal projection of the course c has part (course_of course) a t")
       (!rdfs:label "has part during")
       ) 

(about 
(!obo:example/general_part_of (!rdfs:label "general part of") (!obo:IAO_0000116 "Intended for use by the OBO to OWL translator, in order to avoid having to know which entities are occurrents or processes before generating the translation of the relation. Currently, assertion of general-part of will entail 'part of process' for processes, and 'part of at all times' for independent continuants. However the entailment in the other direction does not work. Asserting 'part of process' will not assert 'general part of' because the axioms needed (at least the ones I've tried) violate the global rules for OWL 2. For queries, therefore, of 'general part of' take the union of querying the independent continuant and process versions of the relation.") (!obo:IAO_0000115 "A relation intended to be be able to used to either assert part of at all times relations between either continuants or occurrents.")) 
(!obo:example/o2 (!rdfs:label "o2")) 
(!obo:example/self_ic (!obo:IAO_0000115 "A relation that holds for any independent continuant instance, relating the occurrent to itself. Identity.") (!obo:IAO_0000116 "Here only to support reasoning. It allows us define part of relations as locally reflexive, as well as proper parthood by excluding these self relations. Effectively identity, restricted to occurrents. If we defined part of relations to be globally reflexive, then we would have it applied to every entity, including those for which part_of might not make sense. As an example, it is not clear whether it makes sense to say realizable entities have parts.") (!rdfs:label "self ic")) 
(!obo:example/life_of (!rdfs:label "course of") (!obo:IAO_0000115 "relates a course to the unique continuant it is the course of") (!rdfs:comment "need a relation to go from course part back to unique continuant. course_of won't work right now because it is 1:1.

1 choice is to weaken course_of to course-part-of

2 course_segment-of is more broad")) 
(!obo:example/class_proxy (!obo:IAO_0000117 "Likely to go away but helping me thing at the moment.") (!rdfs:label "non rigid class proxy") (!obo:IAO_0000115 "A class proxy is a representational artifact that stands in place of a type for the purpose of reasoning in OWL. There is only a single instance that is a class proxy for each class.")) 
(!obo:example/c (!rdfs:label "continuant")) 
(!obo:example/temporal_segment_of (!rdfs:label "temporal segment of") (!rdfs:comment "1. Fix with Barry's definition
2. Consider whether we need this for more than course or course segments.
3. Make peace with finding a different label") (!obo:IAO_0000115 "a process p1 is a temporal part of process p2 iff 

the temporal projection of p1 is part of the temporal projection of p2 
and at all times t in the temporal projection of p1, 
   the spatial projection of p1 equals the spatial projection of p2
and every part of p2 
   whose temporal projection is part of the temporal projection of p1 
   is also a part of p2
and p2 has no parts that are not part of p1")) 
(!obo:example/o1 (!rdfs:label "o1")) 
(!obo:example/c2 (!rdfs:label "c2")) 
(!obo:example/course_segment_of (!obo:IAO_0000115 "relates a temporal segment of a course to the continuant that the course is of. ") (!obo:IAO_0000116 "This is many to one - all the temporal segments map back to the same continuant. The issue to watch out for is non-uniqueness - Barry raised the issue of cases where there is (effectively) a derivation, but practically people consider the life of the continuants to overlap. For example consider an embryo which at some point splits into \"identical\" twins. Do the first part of their courses - before the split - overlap? If so, then to what does the segment of the their common initial course map to - we say here that the relation is functional (and we need this property) but if we believed they shared common course then we have a problem.") (!rdfs:label "segment of course of")) 
(!obo:example/part_of_during (!obo:IAO_0000116 "the constraint on temporal part of is that the part and the whole occupy the same spatial region at all times in the temporal projection of the whole. But the whole may extend longer, temporally") (!rdfs:label "part of during") (!obo:IAO_0000115 "a continuant c is part of during some process p iff p is a temporal part of a course and for all times t in the temporal projection of the course c part of (course_of course) a t")) 
(!obo:example/o3 (!rdfs:label "o3")) 
(!obo:example/c1 (!rdfs:label "c1")) 
(!obo:example/process (!rdfs:label "process")) 
(!obo:example/general_has_part (!rdfs:label "general has part") (!obo:IAO_0000115 "A relation obtaining either between independent continuants when part of at all times or between processes when part of process") (!obo:IAO_0000116 "Intended for use by the OBO to OWL translator, in order to avoid having to know which entities are occurrents or processes before generating the translation of the relation. Currently, assertion of general has part will entail 'process has part' for processes, and 'has part of at all times' for independent continuants. This is accomplished by property chains associated with has part at all times and has process part.

However the entailment in the other direction does not work. Asserting 'part of process' will not assert 'general has part' because the axioms needed (at least the ones I've tried) violate the global rules for OWL 2. For queries, therefore, of 'general has part' take the union of querying the independent continuant and process versions of the relation.")) 
(!obo:example/proper_part_of_at_some_time (!rdfs:label "proper part of at some time") (!obo:IAO_0000115 "For some time in temporal projection of the subject's course the subject is a proper part the object . ")) 
(!obo:example/part_of_at_some_time (!rdfs:label "part of at some time") (!obo:IAO_0000115 "There exists some time instant in the temporal projection of the subject's course at which time the subject has part the object. This implies the object exists at that time. ")) 
(!obo:example/o (!rdfs:label "occurrent")) 
(!obo:example/type_of_during (!rdfs:label "type of during") (!obo:IAO_0000116 "Barry says to try 

ice water steam on some portion of h20.
Butterfly, larvae, pupae for some organism
foetus and newborn

Can I define transformation of?
")) 
(!obo:example/self_p (!rdfs:label "self p") (!obo:IAO_0000115 "A relation that holds for any process instance, relating the occurrent to itself. ") (!obo:IAO_0000116 "Here only to support reasoning. It allows us define part of relations as locally reflexive, as well as proper parthood by excluding these self relations. Effectively identity, restricted to occurrents. If we defined part of relations to be globally reflexive, then we would have it applied to every entity, including those for which part_of might not make sense. As an example, it is not clear whether it makes sense to say realizable entities have parts.")) 
(!obo:example/independent_continuant (!rdfs:label "independent continuant")) 

(!obo:example/c3 (!rdfs:label "c3")) 
(!obo:example/has_part_at_all_times (!rdfs:label "has part at all times") (!obo:IAO_0000115 "For all times in temporal projection of the subject's course the subject is part of the object. This implies the object exists at all times that the subject exists.")) 
(!obo:example/process_has_part (!rdfs:label "process has part") (!obo:IAO_0000115 "A primitive relation among occurrent p1 and p2, with the constraint that the spatiotemporal projection of p1 has part the spatiotemporal projection of  p2")) 
(!obo:example/part_of_process (!rdfs:label "part of process") (!obo:IAO_0000115 "p1 part of process p2 iff 
   p1 is a process and 
   p2 is a process and 
   p1 and p2 are related by the instance-level part of relation described in \"Relations in Biomedical Ontologies\".")) 

(!obo:example/part_of_at_all_times (!rdfs:label "part of at all times") (!obo:IAO_0000115 "For all times in temporal projection of the subject's course the subject has part the object. This implies the object exists at all times that the subject exists."))
|#