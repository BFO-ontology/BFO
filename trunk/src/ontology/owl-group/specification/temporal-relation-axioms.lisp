 ;; axioms starting 500

 (object-property :temporal
   (c-part-of c-has-part)
   (temporal (:all :some) (:all :some) "as Mathias suggests")
   (inverse-at-a-time :id 541)
   (reflexive continuant :id 500)
   (transitive-at-a-time :id 501)
   (domain continuant :id 502)
   (range continuant :id 503)
   (material -> material :id _)
   (quality -> (not realizable) :id 577)
   (realizable -> (not quality) :id 578)
   (disposition -> (not role) :id 579)
   (role -> (not disposition) :id 580)
   (immaterial <- immaterial :id 581)
   (< (0d-cf-boundary -> (not (or 1d-cf-boundary 2d-cf-boundary)) :id 591))
   (< (1d-cf-boundary -> (not (or 2d-cf-boundary)) :id 582))
   (< (0d-s-region -> (not (or 1d-s-region 2d-s-region 3d-s-region)) :id 583))
   (< (1d-s-region -> (not (or 2d-s-region 3d-s-region)) :id 584))
   (< (2d-s-region -> (not 3d-s-region) :id 592))
   (s-region <-> s-region :id 585)
   (< (cf-boundary -> cf-boundary :id 586))
   (sdc -> (not (or ic gdc)) :id 587)
   (gdc -> (not (or ic sdc)) :id 588)
   (ic -> (not (or sdc gdc)) :id 589)
   (< (immaterial -> (not material) :id 028-001))
   (cf-boundary -> (not s-region) :id 029-001)
   )

 (object-property :temporal
   (c-ppart-of c-has-ppart)
   (issue 49)
   (inverse-at-a-time :id 542)
   (temporal (:all :some) (:all :some) "Mathias suggest not parallel to part of, seems not to alan")
   (irreflexive :id 504)
   (transitive-at-a-time :id 505)
   (domain continuant :id 506)
   (range continuant :id 507)
   )

 (object-property :temporal
   (member-part-of has-member-part)
   (issue 49)
   (inverse-at-a-time :id 543)
   (temporal (:all :some) (:all :some) "Seems to be similar enough to part of (parts aren't permanent or defining except at an instant, so offer full set")
   (domain continuant :id 508)
   (range continuant :id 509)
   )

 (object-property :temporal
   (located-in has-location) 
   (issue 49)
   (inverse-at-a-time :id 544)
   (temporal (:some :all) (:some :all))
   (transitive-at-a-time :id 046-001)
   (reflexive ic :id 511)
   (domain ic :id 512)
   (range ic :id 513)
   )

 (object-property :temporal
   (located-at-r r-location-of) 
   (issue 49)
   (inverse-at-a-time :id 545)
   (temporal (:some) (:some) "Include some some for now, but note that the all versions can be used to define frames")
;   (reflexive s-region :id 042-001) not needed due to below self
   (domain ic :id 041-002)
   (range s-region :id 516)
   (ic +> s-region :id 539) ; translates to located-at. We want there to be one at all times but this is the best we can do I think.
   (s-region -> self :id 575)
   (< (s-region -> self :id 576))
   (cf-boundary -> (or 0d-s-region 1d-s-region 2d-s-region) :id 029-001) ; check if two points can make a cf-boundary.
   )

 (object-property :temporal
   (inheres-in bearer-of)
   (issue 49)
   (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
   (inverse-at-a-time :id 547)
   (domain sdc :id 517)
   (sdc +> (and ic (not s-region))  :id 548)
   (range (and ic (not s-region)) :id 518)
   )

  (object-property :temporal
    (q-of has-q)
    (inverse-at-a-time :id 549)
    (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
    (issue 49)
    (domain-narrowed (inheres-in bearer-of) :id 564)
    (domain quality :id 519)
    )

 (object-property :temporal
   (f-of has-f)
   (inverse-at-a-time :id 550)
   (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
   (issue 49)
   (domain function :id 521)
   (domain-narrowed (inheres-in bearer-of) :id 565))

(object-property :temporal
  (r-of has-r)
  (inverse-at-a-time :id 551)
  (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
  (issue 49)
  (domain-narrowed (inheres-in bearer-of) :id 566)
  (domain role :id 523))

 (object-property :temporal
   (d-of has-d)
   (inverse-at-a-time :id 552)
   (temporal (:all) (:some :all) "specific dependents inhere in their bearers for at all times they exist. So we defined all times for that direction and all and some times for the other. More broad than Mathias who suggests some times on the inverse too")
   (issue 49)
   (domain disposition :id 525)
   (domain-narrowed (inheres-in bearer-of) :id 567))

 (object-property :temporal
   (s-depends-on has-s-dep)
   (inverse-at-a-time :id 553)
   (transitive-at-at-time :id 594)
   (temporal (:all :some) (:some :all) "specific dependents inhere in there bearers for their whole life. Buf the s-dependence of a process on a partcipant can hold only at some times. So both. I think Mathias leaves out the process case? (I would be happy to leave it out of BFO altogether")
   (issue 49)
   (domain (or process sdc) :id 527)
   (range (or (and ic (not s-region)) sdc process) :id 528)
   (s (process +> material))
   (s (ic -> nothing :id 593))
   )

(object-property :temporal
  (g-depends-on has-g-dep)
  (inverse-at-a-time :id 554)
  (temporal (:some) (:some) "Define some times in both directions agreeing with Mathias")
  (issue 49)
  (domain gdc :id 529)
  (range (and ic (not s-region)) :id 530)
  (< (s (o has-s-dep_st concretizes_at :id 573)))
  (< (a (o has-s-dep_at concretizes_at :id 574)))
  )

(object-property :temporal
  (has-material-basis material-basis-of)
  (inverse-at-a-time :id 555)
  (temporal (:all) (:some :all) "Follow Mathias as at all times for the forward relation, both for the reverse relation")
  (issue 49)
  (domain disposition :id 531)
  (range material :id 532)
  )

(object-property :temporal
   (concretizes concretized-by)
   (inverse-at-a-time :id 556)
   (temporal (:some :all) (:some :all))
   (:issue 49   "Unsure of this one - Alan. Include both for now")
   (domain sdc :id 533)
   (range gdc :id 534)
   (< (s (gdc +> sdc :id 557)))
)
 
(object-property :temporal
  (st-projects-onto-s s-projection-of-st)
  (inverse-at-a-time :id 558)
  (temporal (:some) (:some) "Things tend to move and change shape in time, so at some times in both directions")
  (issue 49)
  (domain st-region :id 535)
  (s (st-region +> s-region :id 590))
  (range s-region :id 536)
  )
 
(object-property :temporal
  (has-participant participates-in)
  (temporal (:some :all) (:some :all) "at some times is parallels the class-class definition. At all times is permanent participation, requested by Stefan") (issue 49)
  (inverse-at-a-time :id 559)
  (domain process :id 537)
  (range (and continuant (not s-region)) :id 538)
  (a (o has-participant_at inheres-in_at :id 560)) ; if p has-participant sdc and sdc inheres-in m then p has-participant m 
  (s (o has-participant_st inheres-in_at :id 561 :cant "There is a cyclic dependency involving property has-participant_st"))
  (a (o bearer-of_at participates-in_at :id 562 :cant "There is a cyclic dependency involving property has-participant_at"))
  (< (s (o bearer-of_st participates-in_at :id 563)))  ; if m bearer-of sdc and sdc participates-in p then m participates-in p
  (< (a (o has-g-dep_at participates-in_at :id 571 :cant "There is a cyclic dependency involving property has-participant_at")))
  (< (s (o has-g-dep_st participates-in_at :id 572)))  
  )

((sub-object-property-of (object-property-chain realizes inheres-in_at) has-participant_st)
 :source "Alan Ruttenberg"
 :id 106-002)

((equivalent-classes r-quality (object-intersection-of quality (object-min-cardinality 2 inheres-in_at ic)))
 :id 568)

;(sub-object-property-of (object-property-chain located-at-r_st c-part-of_at r-location-of_at) located-in_st) 




 

;; r1 r-location-of y y c-part-of x, x located-at r, y located-at r1 -> r1 c-part-of r 043-001
 






