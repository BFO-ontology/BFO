;; axioms starting 500

(object-property :temporal
  (c-part-of c-has-part)
  (temporal (:all :some) (:all :some) "as Mathias suggests")
  (reflexive continuant :id 500)
  (transitive-at-a-time :id 501)
  (domain continuant :id 502)
  (range continuant :id 503)
  )

(object-property :temporal
  (c-ppart-of c-has-ppart)
  (issue 49)
  (temporal (:all :some) (:all :some) "Mathias suggest not parallel to part of, seems not to alan")
  (irreflexive :id 504)
  (transitive-at-a-time :id 505)
  (domain continuant :id 506)
  (range continuant :id 507)
  )

(object-property :temporal
  (member-part-of has-member-part)
  (issue 49)
  (temporal (:all :some) (:all :some) "Seems to be similar enough to part of (parts aren't permanent or defining except at an instant, so offer full set")
  (domain continuant :id 508)
  (range continuant :id 509)
  )
 
(object-property :temporal
  (located-in has-location) 
  (issue 49)
  (temporal (:some :all) (:some :all))
  (transitive-at-a-time :id 510)
  (reflexive continuant :id 511)
  (domain continuant :id 512)
  (range continuant :id 513)
  )

(object-property :temporal
  (located-at-r r-location-of) 
  (issue 49)
  (temporal (:some) (:some) "Include some some for now, but note that the all versions can be used to define frames")
  (reflexive s-region :id 514)
  (domain continuant :id 515)
  (range s-region :id 516)
  )

(object-property :temporal
  (inheres-in bearer-of)
  (issue 49)
  (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (domain sdc :id 517)
  (range ic :id 518))

 
 (object-property :temporal
   (q-of has-q)
   (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
   (issue 49)
   (domain quality :id 519)
   (range ic :id 520)
   )

(object-property :temporal
  (f-of has-f)
  (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (issue 49)
  (domain function :id 521)
  (range ic :id 522))

(object-property :temporal
  (r-of has-r)
  (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (issue 49)
  (domain role :id 523)
  (range ic :id 524))

 (object-property (d-of has-d)
   (:temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (issue 49)
  (domain disposition :id 525)
  (range ic :id 526))

 (object-property :temporal
   (s-depends-on has-s-dep)
   (temporal (:all :some) (:some :all) "specific dependents inhere in there bearers for their whole life. Buf the s-dependence of a process on a partcipant can hold only at some times. So both. I think Mathias leaves out the process case? (I would be happy to leave it out of BFO altogether")
   (issue 49)
   (domain (or process sdc) :id 527)
   (range (or ic sdc) :id 528)
   )

(object-property :temporal
  (g-depends-on has-g-dep)
  (temporal (:some) (:some) "Define some times in both directions agreeing with Mathias")
  (issue 49)
  (domain gdc :id 529)
  (range ic :id 530)
  )


(object-property :temporal
  (has-material-basis material-basis-of)
  (temporal (:all) (:some :all) "Follow Mathias as at all times for the forward relation, both for the reverse relation")
  (issue 49)
  (domain disposition :id 531)
  (range ic :id 532)
  )
 
 (object-property :temporal
   (concretizes concretization-of)
   (temporal (:some :all) (:some :all))
   (:issue 49   "Unsure of this one - Alan. Include both for now")
   (domain sdc :id 533)
   (range gdc :id 534)
)
 
(object-property :temporal
  (st-projects-onto-s s-projection-of-st)
  (temporal (:some) (:some) "Things tend to move and change shape in time, so at some times in both directions")
  (issue 49)
  (domain st-region :id 535)
  (range s-region :id 536)
  )
 
(object-property :temporal
  (has-participant participates-in)
  (temporal (:some :all) (:some :all) "at some times is parallels the class-class definition. At all times is permanent participation, requested by Stefan") (issue 49)
  (domain process :id 537)
  (range continuant :id 538)
  )
 
(object-property :temporal
  (spans span-of)
  (domain occurrent :id 539)
  (range t-region :id 540)
 )

