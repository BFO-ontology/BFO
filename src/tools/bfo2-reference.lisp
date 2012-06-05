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
 ((exists-at has-existing-entities)  :binary)
 ((o-part-of o-has-part) :binary)
 ((t-part-of t-has-part) :binary)
 ((occupies occupied-by)  :binary)
 ((profile-of has-profile) :binary)
 ((c-part-of c-has-part) :ternary)
 ((c-ppart-of c-has-ppart) :ternary)
 ((member-part-of member-has-part) :ternary)
 ((located-in has-location) :ternary)
 ((located-at-r r-location-of) :ternary)
 ((inheres-in bearer-of) :ternary)
 ((s-depends-on has-s-dep) :ternary)
 ((g-depends-on has-g-dep) :ternary)
 ((q-of has-q) :ternary)
 ((f-of has-f) :ternary)
 ((r-of has-r) :ternary)
 ((d-of has-d) :ternary)
 ((realizes realized-in) :binary)
 ((has-material-basis material-basis-of) :ternary)
 ((concretizes concretization-of) :ternary)
 ((st-projects-onto-s s-projection-of-st) :ternary)
 ((st-projects-onto-t t-projection-of-st) :binary)
 ((has-participant participates-in) :ternary)
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
----cf-boundary(dc)
-----0d-cf-boundary
-----1d-cf-boundary
-----2d-cf-boundary
----s-region(dc)
-----0d-s-region
-----1d-s-region
-----2d-s-region
-----3d-s-region
--sdc(d)
---quality
----r-quality
---realizable
----disposition
-----function
----role
--gdc
-occurrent(d)
--process
---process-profile
--p-boundary
--st-region
--t-region
---0d-t-region
---1d-t-region
")

;; define the binary propery hierarchy

(binary-property-hierarchy "
realizes
o-part-of
-t-part-of
profile-of
occupies
st-projects-onto-t 
")

;; define the ternary property hierarchy. You can't have a ternary
;; property be a sub of a binary or vice versa. Of course we can't
;; really have ternary property in owl, so if we agree on the
;; at-some-times, at-all-times thing then at-all-time is subprop of
;; at-some-time in each self case, whereas the at-all-times
;; superproperties are segregated from the at-some-times
;; superproperties (i.e. q-of at some time /-> inheres-in at all times)
;; Also consider: "during" variants of the properties, if we get histories.

;; Note: These *really* don't make any sense without a temporal
;; reading, so for the purposes here we will have them all be
;; considered "at-all-times". BUT beware because that isn't the sense
;; of all the current class-level relations. has-participant, for
;; example, is at-some-time in the old RO.

(ternary-property-hierarchy "
located-at-r 
s-depends-on 
-inheres-in 
--q-of 
--f-of 
--r-of 
--d-of 
g-depends-on 
located-in 
located-at
has-participant
has-material-basis 
concretizes 
st-projects-onto-s 
spans
c-part-of 
-c-ppart-of 
-member-part-of 
")

;; URIs of terms both current in bfo2 reference as well as those used,
;; but not. Make sure not to use them again. 
;; syntax: (handle bfo1.1uri bfo2uri prop*)
;; if bfo1.1uri is nil then there isn't a term in BFO1.1 that corresponds
;; if bfo2uri is nil a URI still needs to be provided
;; Current props:
;; :obsolete - The term is obsolete in BFO2
;; :maybe - the term is in some prototype and is proposed for BFO2 but not yet accepted
;; (:issue n) - See issue n on the tracker for more information
;; (:super handle) - The term is obsolete but a mapping would have it be a subclass of handle
;; (:subs handles) - The term is obsolete but a mapping would have it be superclass of handles

(uris 

 ;; The sanctioned classes of BFO2. Update as necessary.

 (entity !bfo:Entity !obo:BFO_0000001)
 (continuant !snap:Continuant !obo:BFO_0000002)
 (occurrent !span:Occurrent !obo:BFO_0000003)
 (ic !snap:IndependentContinuant !obo:BFO_0000004)
 (sdc !snap:SpecificallyDependentContinuant !obo:BFO_0000020) 
 (gdc !snap:GenericallyDependentContinuant !obo:BFO_0000031) 
 (process !span:ProcessualEntity !obo:BFO_0000015) 
 (p-boundary !span:ProcessBoundary !obo:BFO_0000035) 
 (t-region !span:TemporalRegion !obo:BFO_0000008)
 (st-region !span:SpatioTemporalRegion !obo:BFO_0000011)
 (s-region !snap:SpatialRegion !obo:BFO_0000006 )
 (material !snap:MaterialEntity !obo:BFO_0000040) 
 (immaterial nil !obo:BFO_0000141)  
 (object !snap:Object !obo:BFO_0000030) 
 (fiat-object !snap:FiatObjectPart !obo:BFO_0000024) 
 (object-aggregate !snap:ObjectAggregate !obo:BFO_0000027) 
 (quality !snap:Quality !obo:BFO_0000019) 
 (r-quality nil !obo:BFO_0000145)  
 (realizable !snap:RealizableEntity !obo:BFO_0000017)  
 (disposition !snap:Disposition !obo:BFO_0000016) 
 (function !snap:Function !obo:BFO_0000034) 
 (role !snap:Role !obo:BFO_0000023)  
 (process-profile nil !obo:BFO_0000144) 
 (0d-t-region !span:TemporalInstant !obo:BFO_0000148) 
 (1d-t-region !span:TemporalInterval !obo:BFO_0000038)  
 (0d-s-region !snap:ZeroDimensionalRegion !obo:BFO_0000018) 
 (1d-s-region !snap:OneDimensionalRegion !obo:BFO_0000026 ) 
 (2d-s-region !snap:TwoDimensionalRegion !obo:BFO_0000009 ) 
 (3d-s-region !snap:ThreeDimensionalRegion !obo:BFO_0000028)
 (site !snap:Site !obo:BFO_0000029) 
 (cf-boundary nil !obo:BFO_0000140)  
 (0d-cf-boundary nil !obo:BFO_0000147)
 (1d-cf-boundary nil !obo:BFO_0000142)
 (2d-cf-boundary nil !obo:BFO_0000146)

 ;; The sanctioned relations in BFO2. Update as necessary.

 (o-part-of nil !obo:BFO_0000132)
 (o-has-part nil !obo:BFO_0000117)
 (t-part-of nil !obo:BFO_0000139)
 (t-has-part nil !obo:BFO_0000121)
 (occupies nil !obo:BFO_0000066)  
 (occupied-by nil !obo:BFO_0000126)
 (has-profile nil !obo:BFO_0000119)
 (profile-of nil !obo:BFO_0000133)
 (c-has-part nil !obo:BFO_0000110)
 (c-part-of nil !obo:BFO_0000105)
 (c-ppart-of nil !obo:BFO_0000137)
 (c-has-ppart nil !obo:BFO_0000111)
 (member-part-of nil !obo:BFO_0000129)
 (member-has-part nil !obo:BFO_0000115)
 (located-in nil !obo:BFO_0000082)
 (has-location nil !obo:BFO_0000124)
 (located-at-r nil !obo:BFO_0000083)
 (r-location-of nil !obo:BFO_0000123)
 (inheres-in nil !obo:BFO_0000052)
 (bearer-of nil !obo:BFO_0000053)
 (s-depends-on nil !obo:BFO_0000070) 
 (has-s-dep nil !obo:BFO_0000125) 
 (g-depends-on nil !obo:BFO_0000084)
 (has-g-dep nil !obo:BFO_0000101)
 (q-of nil !obo:BFO_0000080)
 (has-q nil !obo:BFO_0000086)  
 (f-of nil !obo:BFO_0000079)
 (has-f nil !obo:BFO_0000085)
 (r-of nil !obo:BFO_0000081)
 (has-r nil !obo:BFO_0000087)
 (d-of nil !obo:BFO_0000107) 
 (has-d nil !obo:BFO_0000112)
 (realized-in nil !obo:BFO_0000054 (:issue 39))
 (realizes nil !obo:BFO_0000055 (:issue 39))
 (has-material-basis nil !obo:BFO_0000113)  
 (material-basis-of nil !obo:BFO_0000127)  
 (concretizes nil !obo:BFO_0000059)
 (concretization-of nil !obo:BFO_0000058 )
 (st-projects-onto-s nil !obo:BFO_0000151 (:issue 41)) ; in version end of may, alan assigned
 (s-projection-of-st nil !obo:BFO_0000152 (:issue 41)) ; alan assigned
 (st-projects-onto-t nil !obo:BFO_0000153 (:issue 41)) ; alan assigned
 (t-projection-of-st nil !obo:BFO_0000154 (:issue 41)) ; alan assigned
 (spans nil !obo:BFO_0000155) ; in version end of may alan assigned
 (span-of nil !obo:BFO_0000156) ; in version end of may alan assigned
 (participates-in nil !obo:BFO_0000056)
 (has-participant nil !obo:BFO_0000057)
 (exists-at nil !obo:BFO_0000108) 
 (has-existing-entities nil !obo:BFO_0000157)  ; alan assigned

;; obsolete terms that were in BFO 1.1

 (part-of nil !obo:BFO_0000050 :obsolete (:issue 46))
 (has-part nil !obo:BFO_0000051 :obsolete (:issue 46))
 (dc !snap:DependentContinuant !obo:BFO_0000005 :obsolete (:issue 45) (:super continuant) (:subs sdc gdc))

 (scattered-t-region !span:ScatteredTemporalRegion !obo:BFO_0000032 :obsolete (:super t-region) (:issue 44))
 (connected-t-region !span:ConnectedTemporalRegion !obo:BFO_0000022 :obsolete (:super t-region) (:issue 44))
 (scattered-st-region :ScatteredSpatiotemporalRegion !obo:BFO_0000010 :obsolete (:super st-region) (:issue 44))
 (connected-st-region !span:ConnectedSpatiotemporalRegion !obo:BFO_0000013  :obsolete (:super st-region) (:issue 44))

 (object-boundary !snap:ObjectBoundary !obo:BFO_0000025 :obsolete (:super immaterial))

 (p-aggregate !span:ProcessAggregate !obo:BFO_0000014 :obsolete (:super process))
 (p-context !span:ProcessualContext !obo:BFO_0000037 :obsolete (:super occurrent))

 (st-interval !span:SpatiotemporalInterval !obo:BFO_0000036 :obsolete (:super st-region)) 
 (0d-st-region !span:SpatiotemporalInstant !obo:BFO_0000012 :obsolete (:super st-region)) 
 (fiat-p-part !span:FiatProcessPart !obo:BFO_0000033 :obsolete (:super process)) 
 (old-process !span:Process !obo:BFO_0000007 :obsolete (:super process))

;; prototype terms from owl-ruttenberg. Maybe they will get in, maybe not.

 (is-immediately-preceded-by nil !obo:BFO_0000060 (:maybe ruttenberg))
 (immediately-precedes nil !obo:BFO_0000061 (:maybe ruttenberg))
 (is-preceded-by nil !obo:BFO_0000062 (:maybe ruttenberg))
 (precedes nil !obo:BFO_0000063 (:maybe ruttenberg))

 ;; histories
 (is-course-of nil !obo:BFO_0000064 (:maybe ruttenberg) (:issue 43))
 (has-course nil !obo:BFO_0000065 (:maybe ruttenberg) (:issue 43))

 ;; relating a host to a site
 (has-site-of nil !obo:BFO_0000067 (:maybe ruttenberg) (:issue 36))

 ;; granularity
 (has-granular-part nil !obo:BFO_0000071 (:maybe ruttenberg) (:issue 52))
 (is-granular-part-of nil !obo:BFO_0000073 (:maybe ruttenberg) (:issue 52))
 (has-granular-process-part nil !obo:BFO_0000072 (:maybe ruttenberg) (:issue 52))
 (is-granular-part-of-process nil !obo:BFO_0000074 (:maybe ruttenberg) (:issue 52))

 ;; define fiat object part and object aggregate in terms of these
 (is-aggregate-of nil !obo:BFO_0000075 (:maybe ruttenberg))
 (is-fiat-part-of nil !obo:BFO_0000076 (:maybe ruttenberg))

 ;; coming and going out of existence
 (begins-to-exist-during nil !obo:BFO_0000068 (:maybe ruttenberg) (:issue 50))
 (ceases-to-exist-during nil !obo:BFO_0000069 (:maybe ruttenberg) (:issue 50))
 (has-participant-beginning-to-exist nil !obo:BFO_0000077 (:issue 50)) 
 (has-participant-ceasing-to-exist nil !obo:BFO_0000078 (:issue 50))

 ;; leftover terms from owl-schulz. Majority could be defined but are not in spec. 
 ;; submit issues where appropriate

 (depends-on nil !obo:BFO_0000106 (:maybe schulz))
 (inv-depends-on nil !obo:BFO_0000122 (:maybe schulz))
 (has-c-boundary nil !obo:BFO_0000109 (:maybe schulz))
 (has-o-boundary nil !obo:BFO_0000116 (:maybe schulz))
 (o-has-ppart nil !obo:BFO_0000118 (:maybe schulz))
 (o-ppart-of nil !obo:BFO_0000138 (:maybe schulz))
 (has-ppart nil !obo:BFO_0000120 :not (:issue 46))
 (ppart-of nil !obo:BFO_0000135 :not (:issue 46))
 (t-ppart-of nil !obo:BFO_0000136 (:maybe schulz) (:note "inverse was not defined"))
 (has-material-part nil !obo:BFO_0000114 (:maybe schulz) (:issue 33))
 (material-part-of nil !obo:BFO_0000128 (:maybe schulz) (:issue 33))
 (b-depends-on nil !obo:BFO_0000102 (:maybe schulz))
 (o-boundary-of nil !obo:BFO_0000131 (:maybe schulz))
 (c-boundary-of nil !obo:BFO_0000104 (:maybe schulz))
 (contained-in nil !obo:BFO_0000103 :obsolete)
 (1d-t-region-dup nil !obo:BFO_0000143 (:duplicate !obo:BFO_0000038))
 (projects-onto nil !obo:BFO_0000134 :not (:issue 41))
 (occupies-dup nil !obo:BFO_0000130 (:duplicate !obo:BFO_0000066))
)
