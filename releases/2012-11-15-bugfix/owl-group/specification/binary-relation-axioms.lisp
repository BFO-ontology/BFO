;; axioms starting 100

(object-property :binary
  (exists-at during-which-exists)
  (inverses :id 100)
  (domain entity :id 101)
;  (functional :id 169)
  (entity +> t-region :id 170)
  (p-boundary -> 0d-t-region :id 171)
  (range t-region :id 102))

(object-property :binary
  (o-part-of o-has-part)
  (inverses :id 103 )
  (domain occurrent :id 104)
  (range occurrent :id 105)
  (transitive :id 106)
  (reflexive occurrent :id 107)
  (history -> (not process-profile :id _))
  (process-profile +> process :id _)
  (process-profile -> process :id _)
  (st-region <-> st-region :id 108) ;; x o-part-of y and x :a st-region -> y :a st-region, x o-part-of y and y :a st-region -> x :a st-region
  (t-region <-> t-region :id 109) ;; x o-part-of y and x :a t-region -> y :a t-region
  (process -> process :id 110)  ;; x o-part-of y and x :a process -> y :a process    
  (< (process -> (or process p-boundary) :id 111)) ;;  o-has-part y and x :a process -> y :a process or process boundary
  (p-boundary -> (or process p-boundary) :id 112) ; x o-part-of y and x :a p-boundary -> y :a process or process boundary
  (< (p-boundary -> p-boundary :id 113)) ; x o-has-part y and x :a p-boundary -> y :a process or process boundary
  (occurrent -> self :cant "Non-simple property or its inverse appears in the Self restriction")
  (< (occurrent ->  self :cant "Non-simple property or its inverse appears in the Self restriction"))
  )

(object-property :binary
  (o-ppart-of o-has-ppart) 
  (domain occurrent :id 114)
  (range occurrent :id 115)
  (inverses :id 116)
  (transitive :id 117)
  (irreflexive :id 118)
;  (st-region -> st-region :id 119)
;  (t-region <-> t-region :id 120)
  (< (0d-t-region -> nothing :id 121))
;  (< (process -> (or process p-boundary) :id 122))
;  (process -> process  :id 123)
;  (p-boundary -> (or process p-boundary) :id 124)
;  (< (p-boundary -> p-boundary :id 125))
  )

(object-property :binary
  (t-part-of has-t-part)
  (inverses :id 126)
  (domain occurrent :id 127)
  (range occurrent :id 128)
  (transitive :id 129)
;  (reflexive t-region :id 130) ; owl can't have transitive and irreflexive
;  (st-region -> st-region :id 131)
;  (t-region -> t-region :id 132)
;  (< (process -> (or process p-boundary) :id 133))
;  (p-boundary -> (or p-boundary process) :id 134)
;  (< (t-region -> t-region :id 167))
  )

(object-property :binary
  (t-ppart-of has-t-ppart)
  (inverses :id 135)
  (domain occurrent :id 136)
  (range occurrent :id 137)
  (transitive :id 138)
;  (irreflexive :id 139) ; owl can't have transitive and irreflexive
;  (st-region -> st-region :id 140)
;  (t-region -> t-region :id 141)
;  (process -> (or process p-boundary) :id 142)
;  (< ; (p-boundary -> self :id 143) ; owl global restrictions?
;     (t-region -> t-region :id 144)
  (0d-t-region -> self :id 145 :cant "Non-simple property or its inverse appears in the Self restriction") 
  )

(object-property :binary 
   (occupies occupied-by)
   (inverses :id 146)
   (domain occurrent :id 147)
   (range st-region :id 148)
;   (reflexive st-region :id 149)  don't need it, below self axioms do it
   (p-boundary -> (and st-region (some t-part-of st-region)) :id 151)
   (st-region -> self :id 165)
   (< (st-region -> self :id 166))
   )

(object-property :binary
   (profile-of has-profile) 
   (inverses :id 152)
   (domain process-profile :id 153)
   (range process :id 154)
   (= (process-profile +> process :id 165))
   )

(object-property  :binary
   (realizes realized-in)
   (inverses :id 155)
   (domain process :id 156)
   (range realizable :id 157))

(object-property :binary
  (st-projects-onto-t t-projection-of-st) 
  (inverses :id 158)
  (domain st-region :id 159)
  (st-region +> t-region)
  (range t-region :id 160))

(object-property :binary
  (spans span-of)
  (domain occurrent :id 539)
  (range t-region :id 540)
  (inverses :id 161)
;  (reflexive t-region :id 162) - don't need, below does it
  (t-region -> self :id 163)
  (< (t-region -> self :id 164))
  (o occupies st-projects-onto-t :id 168 :cant "conflicts with the self properties. Recast the self properties using new relations to get around this")
 )

(object-property :binary
  (has-history history-of)
  (domain material :id 172)
  (range process :id 173)
  (inverses :id  174)
  (functional :id 175)
  (< (history +> continuant :is 176))
  (< (functional :id 177)))

(object-property :binary
  (occurs-in contains-process)
  (domain (or process p-boundary) :id 178)
  (range (or material site) :id 180)
  (inverses :id  181)
  )

  
