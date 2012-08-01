;;; Introduction:

;;; The strategy is to have this declarative skeleton which is used to
;;; generate the base part of BFO2. To it will be added the automatically
;;; extracted annotations from the reference, via that, the FOL axioms, as
;;; well as hand-crafted axioms in a different file.
;;; There are several tables of information currently

;;; 1) terms: This gives a symbol (syn:handle) for each term, as well
;;; as information about whether the term is a unary predicate (a
;;; class), a binary precitate (non-time-dependent relation) or
;;; temporal predicate (time-dependent relation)

;;; 2) class-hierarchy: Picture of the hierarchy of classes. Process
;;; to generate subclass relations.

;;; 3) binary-relation-hierarchy: Picture of the hierarchy of binary properties. Process
;;; to generate subproperty assertions

;;; 4) temporal-relation-hierarchy: Picture of the hierarchy of temporal properties. Process
;;; to generate subproperty assertions 

;;; 5) URI allocations, mapping, other metadata. specify URIs that
;;; have already been used, both in bfo2 protos and in bfo1. Give
;;; hints for mapping. Associate with issues.

;; define symbol and arity. We'll use the symbols for any
;; explicit assertion or reference from code. These are not
;; labels. Some labels will come from the reference and some from
;; elsewhere
;; syntax: 
;;   '(' class-symbol :unary props* ')' |
;;   '(' relation-symbol (:binary|:temporal) props*  ')'
;;   '(' relation-symbol inverse-symbol ')' ( :binary | :temporal ) props* ')' 

(terms
 (entity :unary)
 (continuant :unary)
 (occurrent :unary)
 (ic :unary)
 (sdc :unary)
 (gdc :unary)
 (process :unary)
 (history :unary)
 (p-boundary :unary)
 (t-region :unary)
 (st-region :unary)
 (s-region :unary)
 (material :unary)
 (immaterial :unary)
 (object :unary)
 (fiat-object :unary)
 (object-aggregate :unary)
 (quality :unary)
 (r-quality :unary)
 (realizable :unary)
 (disposition :unary)
 (function :unary)
 (role :unary)
 (process-profile :unary)
 (0d-t-region :unary)
 (1d-t-region :unary)
 (0d-s-region :unary)
 (1d-s-region :unary)
 (2d-s-region :unary)
 (3d-s-region :unary)
 (site :unary)
 (cf-boundary :unary)
 (0d-cf-boundary :unary)
 (1d-cf-boundary :unary)
 (2d-cf-boundary :unary)
 ((exists-at during-which-exists) :binary)
 ((o-part-of o-has-part) :binary )
 ((o-ppart-of o-has-ppart) :binary)
 ((t-part-of has-t-part) :binary )
 ((t-ppart-of has-t-ppart) :binary)
 ((occupies occupied-by)  :binary)
 ((occurs-in contains-process)  :binary)
 ((profile-of has-profile) :binary )
 ((realizes realized-in) :binary )

 ((c-part-of c-has-part) :ternary (:issue 49)
  (:temporal (:all :some) (:all :some) "as Mathias suggests"))

 ((c-ppart-of c-has-ppart) :ternary (:issue 49)
  (:temporal (:all :some) (:all :some) "Mathias suggest not parallel to part of, seems not to alan"))

 ((c-part-of-object c-has-part-object) :ternary 
  (:temporal (:all ) (:all)))

 ((member-part-of has-member-part) :ternary (:issue 49)
  (:temporal (:all :some) (:all :some) "Seems to be similar enough to part of (parts aren't permanent or defining except at an instant, so offer full set")
  )
 
 ((located-in has-location) :ternary  (:issue 49)
  (:temporal (:some :all) (:some :all))
  )

 ((located-at-r r-location-of) :ternary  (:issue 49)
  (:temporal (:some) (:some) "Include some some for now, but note that the all versions can be used to define frames")
  )

 ((inheres-in bearer-of) :ternary (:issue 49)
  (:temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too"))
 
 ((q-of has-q) :ternary (:temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (:issue 49))

 ((f-of has-f) :ternary (:temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (:issue 49))

 ((r-of has-r) :ternary (:temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (:issue 49))

 ((d-of has-d) :ternary (:temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (:issue 49))

 ((s-depends-on has-s-dep) :ternary (:temporal (:all :some) (:some :all) "specific dependents inhere in there bearers for their whole life. Buf the s-dependence of a process on a partcipant can hold only at some times. So both. I think Mathias leaves out the process case? (I would be happy to leave it out of BFO altogether") (:issue 49))
 ((g-depends-on has-g-dep) :ternary (:temporal (:some) (:some) "Define some times in both directions agreeing with Mathias") (:issue 49))


 ((has-material-basis material-basis-of) :ternary (:temporal (:all) (:some :all) "Follow Mathias as at all times for the forward relation, both for the reverse relation") (:issue 49))
 
 ((concretizes concretized-by) :ternary (:temporal (:some :all) (:some :all)) (:issue 49) "Unsure of this one - Alan. Include both for now")
 
 ((st-projects-onto-s s-projection-of-st) :ternary (:temporal (:some) (:some) "Things tend to move and change shape in time, so at some times in both directions") (:issue 49))
 
 ((st-projects-onto-t t-projection-of-st) :binary)
 
 ((has-participant participates-in) :ternary (:temporal (:some :all) (:some :all) "at some times is parallels the class-class definition. At all times is permanent participation, requested by Stefan") (:issue 49))
 
 ((history-of has-history) :binary)
 ((spans span-of) :binary)
 
 )

;; define the class hierarchy (unary symbols). This is done from the
;; figure. parenthesized properties are either "d" for disjoint
;; children, and/or "c" for covering axiom (the children instances completely
;; exhaust the parent instances).

(class-hierarchy "
entity(d)
-continuant(d)
--ic(d)
---material
----object
----fiat-object
----object-aggregate
---immaterial(d)
----site
----cf-boundary(d)
-----0d-cf-boundary
-----1d-cf-boundary
-----2d-cf-boundary
----s-region(d)
-----0d-s-region
-----1d-s-region
-----2d-s-region
-----3d-s-region
--sdc(d)
---quality
----r-quality
---realizable(d)
----disposition
-----function
----role
--gdc
-occurrent(d)
--process(d)
---process-profile
---history
--p-boundary
--st-region
--t-region(d)
---0d-t-region
---1d-t-region
")

;; define the binary propery hierarchy

(binary-property-hierarchy "
realizes
realized-in
o-part-of
-o-ppart-of
--t-ppart-of
-t-part-of
--t-ppart-of
o-has-part
-o-has-ppart
--has-t-ppart
-has-t-part
--has-t-ppart
profile-of
has-profile
occupies
occupied-by
exists-at
-spans
-st-projects-onto-t 
during-which-exists
-span-of
-t-projection-of-st
contains-process
-has-history
occurs-in
-history-of
")

;; define the temporal property hierarchy. You can't have a temporal
;; property be a sub of a binary or vice versa. Of course we can't
;; really have temporal property in owl, so if we agree on the
;; at-some-times, at-all-times thing then at-all-time is subprop of
;; at-some-time in each self case, whereas the at-all-times
;; superproperties are segregated from the at-some-times
;; superproperties (i.e. q-of at some time /-> inheres-in at all times)
;; Also consider: "during" variants of the properties, if we get histories.

(temporal-property-hierarchy "
located-at-r_st 
r-location-of_st
s-depends-on_st
-has-participant_st
-s-depends-on_at
--has-participant_at
--history-of
--inheres-in_at
---q-of_at 
---f-of_at 
---r-of_at 
---d-of_at 
has-s-dep_st
-participates-in_st
-bearer-of_st
--has-q_st
--has-r_st
--has-d_st
--has-f_st
-has-s-dep_at
--participates-in_at
---has-history
--bearer-of_at
---has-q_at
---has-f_at
---has-r_at 
---has-d_at
g-depends-on_st
has-g-dep_st
has-material-basis_at
material-basis-of_st
-material-basis-of_at
concretizes_st 
-concretizes_at 
concretized-by_st
-concretized-by_at
st-projects-onto-s_st 
s-projection-of-st_st
c-part-of-object_at 
c-has-part-object_at 
c-part-of_st
-c-part-of-object_at
-c-part-of_at
-c-ppart-of_st
--c-ppart-of_at
---member-part-of_at
--member-part-of_st
---member-part-of_at
c-part-of_at
-c-ppart-of_at
--member-part-of_at
c-has-part_st
-c-has-part-object_at
-c-has-part_at
-c-has-ppart_st
--c-has-ppart_at
---has-member-part_at
--has-member-part_st
---has-member-part_at
c-has-part_at
-c-has-ppart_at
--has-member-part_at
located-in_st 
-located-in_at
has-location_st
-has-location_at
")

;; Note: Uris moved to bfo2-uris.lisp so less likely to confuse bfo2 editors into thinking they need to use URIs.
;; Note that that file also now has relations that were proposed but are not yet included in the spec, as well as deprecated relations.

;; this section needs to be manged each time the spec is updated. It
;; maps the string used as an annotation tag to the short handle used
;; in the code. The function (known-in-reference) gives you the list
;; of tags that are used in the reference document. If the tag is the
;; same as the variable name, or there is only spaces or underscores
;; that should be translated to dashes you don't have to repeat it.
;; For temporal properties use the unsuffixed name and the code will dwim.

(reference-annotation-tag-to-variable
 ("entity")
 ("exists-at")
 ("occurrent-part-of" o-part-of)
 ("proper-continuant-part-of" c-ppart-of)
 ("proper-occurrent-part-of" o-ppart-of)
 ("has-continuant-part" c-has-part)
 ("has-occurrent-part" o-has-part)
 ("has-proper-continuant-part" c-has-ppart)
 ("has-proper-occurrent-part" o-has-ppart)
 ("continuant")
 ("independent-continuant" ic)
 ("object")
 ("object-aggregate")
 ("member-part-of")
 ("fiat-object-part" fiat-object)
 ("immaterial-entity" immaterial)
 ("continuant-fiat-boundary" cf-boundary)
 ("material-entity" material)
 ("zero-dimensional-continuant-fiat-boundary" 0d-cf-boundary)
 ("one-dimensional-continuant-fiat-boundary" 1d-cf-boundary)
 ("two-dimensional-continuant-fiat-boundary" 2d-cf-boundary)
 ("site")
 ("spatial-region" s-region)
 ("zero-dimensional-spatial-region" 0d-s-region)
 ("one-dimensional-spatial-region" 1d-s-region)
 ("two-dimensional-spatial-region" 2d-s-region)
 ("three-dimensional-spatial-region" 3d-s-region)
 ("occupies-spatial-region" located-at-r)
 ("continuant-part-of" c-part-of)
 ("located-in")
 ("occupies-temporal-region" spans)
 ("span of")
 ("specifically-dependent-continuant" sdc)
 ("inheres-in")
 ("bearer-of")
 ("s-depends-on")
 ("specifically depends on" s-depends-on)
 ("quality")
 ("quality-of" q-of)
 ("object-aggregate" object-aggregate)
 ("relational-quality" r-quality)
 ("realizable-entity" realizable)
 ("realizes")
 ("realized-in")
 ("role")
 ("disposition")
 ("function")
 ("role-of" r-of)
 ("disposition-of" d-of)
 ("function-of" f-of)
 ("has-role" has-r)
 ("has-disposition" has-d)
 ("has-function" has-f)
 ("has-material-basis")
 ("g-depends-on")
 ("generically-dependent-continuant" gdc)
 ("concretizes")
 ("projects-onto") ;; FIXME
 ("occupies-spatiotemporal-region" occupies)
 ("occurs-in")
 ("contains_process")
 ("process")
 ("process-profile")
 ("history")
 ("process-boundary" p-boundary)
 ("has-participant")
 ("participates-in")
 ("temporal-part-of" t-part-of)
 ("proper-temporal-part-of" t-ppart-of)
 ("process-profile-of" profile-of)
 ("spatiotemporal-region" st-region)
 ("occurrent")
 ("temporal-region" t-region)
 ("zero-dimensional-temporal-region" 0d-t-region)
 ("one-dimensional-temporal-region" 1d-t-region)
 ("history-of" history-of)
 ("has-history" has-history)
 )

;; this section needs to be manged each time the spec is updated. It
;; maps the string (only exceptions) used as an annotation tag to the
;; short handle used in the CLIF spec. The function
;; (known-in-reference) gives you the list of tags that are used in
;; the reference document. If the tag is the same as the variable
;; name, or there is only spaces or underscores that should be
;; translated to camel case you don't have to repeat it.  For temporal
;; properties use the unsuffixed name and the code will dwim.

(reference-annotation-tag-to-clif
 (located-at "locatedAt")
 (s-depends-on "specificallyDependsOn")
 (g-depends-on "genericallyDependsOn")
 )