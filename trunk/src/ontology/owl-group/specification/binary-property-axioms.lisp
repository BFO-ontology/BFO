(object-property :binary
  (exists-at during-which-exists)
  (:inverses :id _)
  (entity -> t-region :id _))

(object-property :binary
  (o-part-of o-has-part)
  (:inverses :id _)
  (:locally-reflexive occurrent :id _)
  (:transitive id: _)
  (-> (st-region st-region :id _)
      (process process :id _)
      (p-boundary (union-of process p-boundary) :id _)))

(object-property :binary
  (o-ppart-of o-has-ppart) 
  (:inverses :id _)
  (:transitive :id _)
  (:irreflexive :id _)
  (->
   (st-region st-region :id _)
   (t-region t-region :id _)
   (0d-t-region nothing :id _)
   (process process :id _)
   (p-boundary (union-of process p-boundary) :id _)))

(object-property :binary
  (t-part-of has-t-part)
  (:inverses :id _)
  :transitive
  (:locally-reflexive t-region :id _)
  (-> t-part-of
      (t-region t-region)
      (0d-t-region self)
      (1d-t-region t-region)
      )
  (-> has-t-part
      (t-region t-region)
      (0d-t-region self)
      (1d-t-region t-region :id _ :note "Would like to exclude points that are not part of the region")
      ))

 (object-property :binary 
   (occupies occupied-by)
   (:inverses :id _)
   (:locally-reflexive st-region :id _)
   (:locally-reflexive t-region :id _)
   (-> occupies 
       (process (union-of st-region t-region) :id _)
       (process-boundary (union-of st-region t-region) :id _)
   (-> occupied-by
       (occurrent occurrent :id _))
   ))

 (object-property :binary
   (profile-of has-profile) 
   (:inverses :id _)
   (-> profile-of
       (process-profile process :id _))
   (-> has-profile
       (process process-profile :id _)))
   
 (object-property  :binary
   (realizes realized-in)
   (:inverses :id _)
   (-> realizes (process realizable :id _))
   (-> realized-by (realizable process) :id _)
   )
