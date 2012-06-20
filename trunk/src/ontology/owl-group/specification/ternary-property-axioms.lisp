(r 
 (-> a b) = if x r y and x instanceof a then y instance of b

(domain r d) -> x r y -> x instance of d  = (r (-> d Thing))
(range r d) -> x r y -> y instance of d   = (r (-> Thing d))

sample: s-region c-part-of only s-region. s-region c-has-part only s-region.


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

(domain inheres-in sdc)
(range  inheres-in ic)
(domain realized-in realizable) 
(range realized-in process)
(domain participates-in continuant) 
(range participates-in process) 
(domain concretization-of gdc) 
(range concretization-of sdc)
(domain concretizes sdc) 
(domain f-of function) 
(range  f-of ic)
(domain q-of quality)
(range  f-of ic)
(domain r-of role) 
(domain located-in ic) 
(domain located-at-r ic) 
(domain g-depends-on gdc) 
(domain has-f ic) 
(domain has-q material) 
(domain has-r ic) 
(domain has-g-dep ic) 
(domain c-part-of continuant) 
(domain d-of disposition) 
(domain c-has-part continuant) 
(domain c-has-ppart continuant) 
(domain has-d ic) 
(domain has-material-basis disposition) 
(domain o-has-part occurrent) 
(domain t-has-part occurrent) 
(domain r-location-of s-region) 
(domain has-location ic) 
(domain occupied-by (object-union-of st-region t-region)) 
(domain material-basis-of material) 
(domain o-part-of occurrent) 
(domain profile-of process-profile) 
(domain c-ppart-of continuant) 
(domain t-part-of occurrent) 
(range t-part-of occurrent) 
