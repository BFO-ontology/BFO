
(sub-class-of occurrent (object-all-values-from s-depends-on (object-union-of occurrent ic))) 

(sub-class-of occurrent (object-all-values-from m-has-part occurrent)) 

(sub-class-of occurrent (object-all-values-from m-part-of occurrent)) 

(sub-class-of s-region (object-all-values-from c-part-of s-region)) 

(sub-class-of s-region (object-all-values-from c-has-part s-region)) 

(sub-class-of sdc (object-all-values-from s-depends-on ic)) 

(sub-class-of site (object-intersection-of (object-some-values-from located-at-r 3d-s-region) immaterial)) 

(sub-class-of p-boundary (object-some-values-from occupies 0d-t-region)) 

(sub-class-of p-boundary (object-all-values-from occupies t-region)) 

(sub-class-of material (object-all-values-from m-has-part object-aggregate)) 

(sub-class-of material (object-all-values-from m-part-of material)) 

(sub-class-of immaterial (object-some-values-from located-at-r 3d-s-region)) 

(sub-class-of immaterial (object-all-values-from m-has-part immaterial)) 

(sub-class-of immaterial (object-all-values-from m-part-of immaterial)) 

(object-property-domain inheres-in sdc) 

(object-property-domain bearer-of ic) 

(object-property-domain realized-in realizable) 

(object-property-domain realizes occurrent) 

(object-property-domain participates-in continuant) 

(object-property-domain has-participant occurrent) 

(object-property-domain concretization-of gdc) 

(object-property-domain concretizes sdc) 

(object-property-domain f-of function) 

(object-property-domain q-of quality) 

(object-property-domain r-of role) 

(transitive-object-property located-in) 

(object-property-domain located-in ic) 

(transitive-object-property located-at-r) 

(object-property-domain located-at-r ic) 

(object-property-domain g-depends-on gdc) 

(object-property-domain has-f ic) 

(object-property-domain has-q material) 

(object-property-domain has-r ic) 

(object-property-domain has-g-dep ic) 

(transitive-object-property c-part-of) 

(object-property-domain c-part-of continuant) 

(object-property-domain d-of disposition) 

(transitive-object-property c-has-part) 

(object-property-domain c-has-part continuant) 

(transitive-object-property c-has-ppart) 

(object-property-domain c-has-ppart continuant) 

(object-property-domain has-d ic) 

(object-property-domain has-material-basis disposition) 

(transitive-object-property o-has-part) 

(object-property-domain o-has-part occurrent) 

(transitive-object-property t-has-part) 

(object-property-domain t-has-part occurrent) 

(transitive-object-property r-location-of) 

(object-property-domain r-location-of s-region) 

(transitive-object-property has-location) 

(object-property-domain has-location ic) 

(object-property-domain occupied-by (object-union-of st-region t-region)) 

(object-property-domain material-basis-of material) 

(transitive-object-property o-part-of) 

(object-property-domain o-part-of occurrent) 

(object-property-domain profile-of process-profile) 

(transitive-object-property c-ppart-of) 

(object-property-domain c-ppart-of continuant) 

(transitive-object-property t-part-of) 

(object-property-domain t-part-of occurrent) 

(object-property-range t-part-of occurrent) 

(sub-object-property-of (object-property-chain participates-in t-part-of) participates-in) 

(sub-object-property-of (object-property-chain concretization-of g-depends-on) s-depends-on) 

(equivalent-classes (object-some-values-from has-s-dep role) (object-some-values-from has-r role)) 

(sub-object-property-of (object-property-chain c-part-of located-in) located-in) 

(sub-object-property-of (object-property-chain located-in c-part-of) located-in) 

(sub-object-property-of (object-property-chain bearer-of realized-in) participates-in) 


;;
(equivalent-classes (object-some-values-from has-s-dep disposition) (object-some-values-from has-d disposition)) 
(equivalent-classes (object-some-values-from has-r role) (object-some-values-from bearer-of role)) 
(equivalent-classes (object-some-values-from has-f function) (object-some-values-from bearer-of function)) 
(equivalent-classes (object-some-values-from has-s-dep quality) (object-some-values-from has-q quality)) 
(equivalent-classes (object-some-values-from has-d disposition) (object-some-values-from bearer-of disposition)) 
(equivalent-classes (object-some-values-from has-s-dep function) (object-some-values-from has-f function)) 
;;
(sub-object-property-of (object-property-chain located-at-r c-part-of r-location-of) located-in) 


;; The following axioms are generated directly from structures in bfo2-reference.lisp

;;(disjoint-classes 1d-cf-boundary 2d-cf-boundary 0d-cf-boundary)

;;(disjoint-classes 2d-s-region 0d-s-region 1d-s-region 3d-s-region)

;;(inverse-object-properties t-part-of t-has-part)

;;(sub-object-property-of t-part-of o-part-of)

;;(inverse-object-properties c-ppart-of c-has-ppart)

;;(sub-object-property-of c-ppart-of c-part-of)

;;(inverse-object-properties o-part-of o-has-part)

;;(inverse-object-properties material-basis-of has-material-basis)

;;(inverse-object-properties has-s-dep s-depends-on)

;;(inverse-object-properties located-in has-location)

;;(inverse-object-properties r-location-of located-at-r)

;;(inverse-object-properties t-part-of t-has-part)

;;(inverse-object-properties o-part-of o-has-part)

;;(inverse-object-properties material-basis-of has-material-basis)

;;(inverse-object-properties d-of has-d)

;;(sub-object-property-of has-d bearer-of)

;;(inverse-object-properties c-ppart-of c-has-ppart)

;;(sub-object-property-of c-has-ppart c-has-part)

;;(inverse-object-properties c-part-of c-has-part)

;;(inverse-object-properties d-of has-d)

;;(sub-object-property-of d-of inheres-in)

;;(inverse-object-properties c-part-of c-has-part)

;;(inverse-object-properties g-depends-on has-g-dep)

;;(inverse-object-properties r-of has-r)

;;(sub-object-property-of has-r bearer-of)

;;(inverse-object-properties q-of has-q)

;;(sub-object-property-of has-q bearer-of)

;;(inverse-object-properties f-of has-f)

;;(sub-object-property-of has-f has-d)

;;(inverse-object-properties g-depends-on has-g-dep)

;;(inverse-object-properties r-location-of located-at-r)

;;(inverse-object-properties located-in has-location)

;;(inverse-object-properties r-of has-r)

;;(sub-object-property-of r-of inheres-in)

;;(inverse-object-properties q-of has-q)

;;(sub-object-property-of q-of inheres-in)

;;(inverse-object-properties f-of has-f)

;;(sub-object-property-of f-of d-of)

;;(inverse-object-properties has-s-dep s-depends-on)

;;(inverse-object-properties concretizes concretization-of)

;;(inverse-object-properties concretizes concretization-of)

;;(inverse-object-properties has-participant participates-in)

;;(sub-object-property-of has-participant s-depends-on)

;;(inverse-object-properties has-participant participates-in)

;;(sub-object-property-of participates-in has-s-dep)

;;(inverse-object-properties realizes realized-in)

;;(inverse-object-properties realizes realized-in)

;;(inverse-object-properties bearer-of inheres-in)

;;(sub-object-property-of bearer-of has-s-dep)

;;(inverse-object-properties bearer-of inheres-in)

;;(sub-object-property-of inheres-in s-depends-on)

;;(disjoint-classes cf-boundary s-region)

;;(sub-class-of cf-boundary immaterial)

;;(equivalent-classes cf-boundary (object-union-of 1d-cf-boundary 2d-cf-boundary 0d-cf-boundary))

;;(sub-class-of material ic)

;;(sub-class-of p-boundary occurrent)

;;(disjoint-classes site s-region)

;;(sub-class-of role realizable)

;;(equivalent-classes sdc (object-union-of quality realizable))

;;(sub-class-of quality sdc)

;;(sub-class-of disposition realizable)

;;(sub-class-of st-region occurrent)

;;(disjoint-classes s-region site)

;;(sub-class-of s-region immaterial)

;;(sub-class-of s-region continuant)

;;(equivalent-classes s-region (object-union-of 2d-s-region 0d-s-region 1d-s-region 3d-s-region))

;;(sub-class-of ic continuant)

;;(equivalent-classes ic (object-union-of immaterial material))

;;(sub-class-of occurrent entity)

;;(sub-class-of continuant entity)

;; The following axioms use URIs not in the current spec

;;(equivalent-classes continuant (object-union-of dc ic))

;;(sub-class-of continuant (object-some-values-from exists-at 1d-t-region-dup))

;;(sub-class-of continuant (object-all-values-from part-of continuant))

;;(sub-class-of continuant (object-all-values-from has-part continuant))

;;(equivalent-classes occurrent (object-union-of old-process t-region st-region p-boundary process-profile))

;;(sub-class-of occurrent (object-some-values-from occupies-dup t-region))

;;(sub-class-of occurrent (object-some-values-from occupies-dup st-region))

;;(equivalent-classes dc (object-union-of gdc sdc))

;;(sub-class-of dc continuant)

;;(sub-class-of old-process occurrent)

;;(sub-class-of old-process (object-some-values-from has-participant material))

;;(sub-class-of old-process (object-some-values-from occupies 1d-t-region-dup))

;;(sub-class-of old-process (object-some-values-from s-depends-on ic))

;;(sub-class-of old-process (object-some-values-from t-has-part occurrent))

;;(sub-class-of old-process (object-all-values-from part-of old-process))

;;(sub-class-of old-process (object-all-values-from occupies 1d-t-region-dup))

;;(sub-class-of old-process (object-all-values-from s-depends-on ic))

;;(equivalent-classes t-region (object-intersection-of occurrent (object-union-of 0d-t-region 1d-t-region-dup) (object-all-values-from o-has-part t-region) (object-all-values-from o-ppart-of 1d-t-region-dup)))

;;(sub-class-of st-region (object-some-values-from projects-onto s-region))

;;(sub-class-of st-region (object-some-values-from projects-onto t-region))

;;(equivalent-classes realizable (object-intersection-of sdc (object-complement-of quality) (object-some-values-from inheres-in (object-union-of material site)) (object-all-values-from realized-in old-process)))

;;(equivalent-classes sdc (object-intersection-of (object-some-values-from s-depends-on ic) dc))

;;(sub-class-of sdc dc)

;;(sub-class-of site (object-all-values-from part-of (object-union-of site material)))

;;(sub-class-of site (object-all-values-from has-part site))

;;(equivalent-classes gdc (object-intersection-of (object-some-values-from g-depends-on ic) dc))

;;(sub-class-of gdc dc)

;;(sub-class-of gdc (object-all-values-from part-of gdc))

;;(sub-class-of gdc (object-all-values-from has-part gdc))

;;(sub-class-of p-boundary (object-complement-of (object-some-values-from o-has-ppart !owl:Thing)))

;;(sub-class-of p-boundary (object-some-values-from occupies-dup 0d-t-region))

;;(sub-class-of p-boundary (object-some-values-from o-boundary-of old-process))

;;(equivalent-classes material (object-some-values-from has-part material))

;;(sub-class-of material (object-all-values-from part-of material))

;;(sub-class-of material (object-all-values-from has-part (object-union-of material site)))

;;(sub-class-of cf-boundary (object-complement-of (object-some-values-from has-part s-region)))

;;(equivalent-classes immaterial (object-intersection-of (object-complement-of (object-some-values-from has-part material)) ic))

;;(sub-class-of 1d-t-region-dup t-region)

;;(sub-class-of 0d-t-region (object-intersection-of (object-complement-of (object-some-values-from o-has-ppart occurrent)) t-region))

;;(inverse-object-properties has-part part-of)

;;(inverse-object-properties has-part part-of)

;;(transitive-object-property has-part)

;;(inverse-object-properties is-preceded-by precedes)

;;(transitive-object-property is-preceded-by)

;;(object-property-domain is-preceded-by occurrent)

;;(inverse-object-properties is-preceded-by precedes)

;;(transitive-object-property precedes)

;;(object-property-domain precedes occurrent)

;;(object-property-domain occupies (object-union-of p-boundary old-process))

;;(sub-object-property-of s-depends-on depends-on)

;;(object-property-domain s-depends-on (object-union-of old-process dc))

;;(sub-object-property-of g-depends-on depends-on)

;;(sub-object-property-of has-g-dep inv-depends-on)

;;(object-property-domain contained-in material)

;;(sub-object-property-of c-boundary-of ppart-of)

;;(inverse-object-properties has-c-boundary c-boundary-of)

;;(object-property-domain c-boundary-of ic)

;;(sub-object-property-of c-part-of part-of)

;;(inverse-object-properties depends-on inv-depends-on)

;;(object-property-domain depends-on (object-union-of dc occurrent))

;;(sub-object-property-of has-c-boundary has-ppart)

;;(sub-object-property-of c-has-part has-part)

;;(sub-object-property-of c-has-ppart has-ppart)

;;(sub-object-property-of has-material-part c-has-part)

;;(inverse-object-properties has-material-part material-part-of)

;;(transitive-object-property has-material-part)

;;(object-property-domain has-material-part material)

;;(sub-object-property-of m-has-part has-ppart)

;;(sub-object-property-of has-o-boundary has-ppart)

;;(sub-object-property-of o-has-part has-part)

;;(sub-object-property-of o-has-ppart o-has-part)

;;(sub-object-property-of o-has-ppart has-ppart)

;;(inverse-object-properties o-has-ppart o-ppart-of)

;;(transitive-object-property o-has-ppart)

;;(object-property-domain o-has-ppart occurrent)

;;(sub-object-property-of has-profile o-has-ppart)

;;(object-property-domain has-profile old-process)

;;(sub-object-property-of has-ppart has-part)

;;(inverse-object-properties has-ppart ppart-of)

;;(sub-object-property-of t-has-part has-part)

;;(inverse-object-properties depends-on inv-depends-on)

;;(object-property-domain inv-depends-on ic)

;;(sub-object-property-of has-s-dep inv-depends-on)

;;(object-property-domain has-s-dep (object-union-of old-process continuant))

;;(inverse-object-properties occupied-by occupies-dup)

;;(sub-object-property-of material-part-of c-part-of)

;;(inverse-object-properties has-material-part material-part-of)

;;(transitive-object-property material-part-of)

;;(object-property-domain material-part-of material)

;;(sub-object-property-of m-part-of ppart-of)

;;(inverse-object-properties occupied-by occupies-dup)

;;(object-property-domain occupies-dup occurrent)

;;(sub-object-property-of o-boundary-of ppart-of)

;;(inverse-object-properties o-boundary-of has-o-boundary)

;;(object-property-domain o-boundary-of p-boundary)

;;(sub-object-property-of o-part-of part-of)

;;(sub-object-property-of profile-of o-ppart-of)

;;(object-property-domain projects-onto st-region)

;;(sub-object-property-of ppart-of part-of)

;;(inverse-object-properties has-ppart ppart-of)

;;(sub-object-property-of t-ppart-of t-part-of)

;;(object-property-domain t-ppart-of occurrent)

;;(sub-object-property-of c-ppart-of ppart-of)

;;(sub-object-property-of o-ppart-of o-part-of)

;;(sub-object-property-of o-ppart-of ppart-of)

;;(inverse-object-properties o-has-ppart o-ppart-of)

;;(transitive-object-property o-ppart-of)

;;(object-property-domain o-ppart-of occurrent)

;;(disjoint-classes old-process t-region st-region p-boundary process-profile)

;;(sub-object-property-of (object-property-chain contained-in part-of) contained-in)

;;(sub-object-property-of (object-property-chain part-of contained-in) contained-in)

