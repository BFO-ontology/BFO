;;; Introduction:

;;; The strategy is to have this declarative skeleton which is used to
;;; generate the base part of BFO2. To it will be added the automatically
;;; extracted annotations from the reference, via that, the FOL axioms, as
;;; well as hand-crafted axioms in a different file.
;;; There are several tables of information currently

;;; 1) terms: This gives a symbol (syn:handle) for each term, as well
;;; as information about whether the term is a unary predicate (a
;;; class), a binary precitate (non-time-dependent relation) or
;;; ternary predicate (time-dependent relation)

;;; 2) class-hierarchy: Picture of the hierarchy of classes. Process
;;; to generate subclass relations.

;;; 3) binary-relation-hierarchy: Picture of the hierarchy of binary properties. Process
;;; to generate subproperty assertions

;;; 4) ternary-relation-hierarchy: Picture of the hierarchy of ternary properties. Process
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
;;   '(' relation-symbol (:binary|:ternary) props*  ')'
;;   '(' relation-symbol inverse-symbol ')' ( :binary | :ternary ) props* ')' 

(terms
 (entity :unary)
 (continuant :unary)
 (occurrent :unary)
 (ic :unary)
 (sdc :unary)
 (gdc :unary)
 (process :unary)
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
 ((exists-at during-which-exists)  :binary (-> entity t-region))
 ((o-part-of o-has-part) :binary (:locally-reflexive occurrent) :transitive (-> (st-region st-region) (process process) (p-boundary process) (p-boundary p-boundary)))
 ((o-ppart-of o-has-ppart) :binary :transitive (:locally-irreflexive occurrent) (-> (st-region st-region) (process process) (p-boundary process) (p-boundary p-boundary)))
 ((t-part-of has-t-part) :binary :transitive (:locally-reflexive t-region) (-> (t-region t-region) (0d-t-region 0d-t-region) (1d-t-region 1d-t-region)))
 ((occupies occupied-by)  :binary (:locally-reflexive st-region) (:locally-reflexive t-region))
 ((profile-of has-profile) :binary (-> process-profile process))
 ((realizes realized-in) :binary (-> process realizable))

 ((c-part-of c-has-part) :ternary (:issue 49)
  (:temporal (:all :some) (:all :some) "as Mathias suggests")
  (:local-reflexive continuant) :transitive-at-a-time)
 ((c-ppart-of c-has-ppart) :ternary (:issue 49)
  (:temporal (:all :some) (:all :some) "Mathias suggest not parallel to part of, seems not to alan")
  (:locally-irreflexive continuant) :transitive-at-a-time
  )

 ((member-part-of has-member-part) :ternary (:issue 49)
  (:temporal (:all :some) (:all :some) "Seems to be similar enough to part of (parts aren't permanent or defining except at an instant, so offer full set")
  )
 
 ((located-in has-location) :ternary  (:issue 49)
  (:temporal (:some :all) (:some :all))
  :transitive-at-a-time (:locally-reflexive continuant)
  )

 ((located-at-r r-location-of) :ternary  (:issue 49)
  (:temporal (:some) (:some) "Include some some for now, but note that the all versions can be used to define frames")
  (:locally-reflexive s-region)
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
 
 ((concretizes concretization-of) :ternary (:temporal (:some :all) (:some :all)) (:issue 49) "Unsure of this one - Alan. Include both for now")
 
 ((st-projects-onto-s s-projection-of-st) :ternary (:temporal (:some) (:some) "Things tend to move and change shape in time, so at some times in both directions") (:issue 49))
 
 ((st-projects-onto-t t-projection-of-st) :binary)
 
 ((has-participant participates-in) :ternary (:temporal (:some :all) (:some :all) "at some times is parallels the class-class definition. At all times is permanent participation, requested by Stefan") (:issue 49))
 
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
--process
---process-profile
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
-t-part-of
o-has-part
-o-has-ppart
-has-t-part
profile-of
has-profile
occupies
occupied-by
st-projects-onto-t 
t-projection-of-st
spans
span-of
exists-at
during-which-exists
")

;; define the ternary property hierarchy. You can't have a ternary
;; property be a sub of a binary or vice versa. Of course we can't
;; really have ternary property in owl, so if we agree on the
;; at-some-times, at-all-times thing then at-all-time is subprop of
;; at-some-time in each self case, whereas the at-all-times
;; superproperties are segregated from the at-some-times
;; superproperties (i.e. q-of at some time /-> inheres-in at all times)
;; Also consider: "during" variants of the properties, if we get histories.

(ternary-property-hierarchy "
located-at-r_st 
r-location-of_st
s-depends-on_st
-s-depends-on_at
--inheres-in_at
---q-of_at 
---f-of_at 
---r-of_at 
---d-of_at 
has-s-dep_st
-has-s-dep_at
--bearer-of_at
---has-q_at
---has-f_at
---has-r_at 
---has-d_at
g-depends-on_st
has-g-dep_st
participates-in_st
-participates-in_at
has-participant_st
-has-participant_at
has-material-basis_at
concretizes_st 
-concretizes_at 
concretization-of_st
-concretization-of_at
st-projects-onto-s_st 
s-projection-of-st_st
c-part-of_st
-c-part-of_at
c-ppart-of_st
-c-ppart-of_at
member-part-of_st
-member-part-of_at
located-in_st 
-located-in_at
--c-part-of_at 
---c-ppart-of_at 
---member-part-of_at
c-has-part_st 
-c-has-part_at 
c-has-ppart_st
-c-has-ppart_at
has-member-part_st
-has-member-part_at
has-location_st
-has-location_at
--c-has-part_at 
---c-has-ppart_at
----has-member-part_at
")

;; Note: Uris moved to bfo2-uris.lisp so less likely to confuse bfo2 editors into thinking they need to use URIs.
;; Note that that file also now has relations that were proposed but are not yet included in the spec, as well as deprecated relations.

;; this section needs to be manged each time the spec is updated. It
;; maps the string used as an annotation tag to the short handle used
;; in the code. The function (known-in-reference) gives you the list
;; of tags that are used in the reference document. If the tag is the
;; same as the variable name, or there is only spaces or underscores
;; that should be translated to dashes you don't have to repeat it.
;; For ternary properties use the unsuffixed name and the code will dwim.

(reference-annotation-tag-to-variable
 ("entity")
 ("exists-at")
 ("occurrent-part-of" o-part-of)
 ("proper-continuant-part-of" c-ppart-of)
 ("proper-occurrent-part-of" o-ppart-of)
 ("has-continuant-part" c-has-part)
 ("has-occurrent-part" o-has-part)
 ("continuant")
 ("independent-continuant" ic)
 ("object")
 ("object-aggregate")
 ("member-part-of" m-part-of)
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
 ("located-at")
 ("continuant-part-of" c-part-of)
 ("located-in")
 ("specifically-dependent-continuant" sdc)
 ("inheres-in")
 ("bearer-of")
 ("s-depends-on")
 ("quality")
 ("quality-of" q-of)
 ("relational-quality" r-quality)
 ("realizable-entity" realizable)
 ("realizes")
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
 ("occupies")
 ("process")
 ("process-boundary" p-boundary)
 ("has-participant")
 ("temporal-part-of" t-part-of)
 ("process-profile-of" profile-of)
 ("spatiotemporal-region" st-region)
 ("occurrent")
 ("temporal-region" t-region)
 ("zero-dimensional-temporal-region" 0d-t-region)
 ("one-dimensional-temporal-region" 1d-t-region)
 )
