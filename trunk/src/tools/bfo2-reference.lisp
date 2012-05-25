;; define symbol, arity and URI. We'll use the symbols for any
;; explicit assertion or reference from code. These are not
;; labels. Some labels will come from the reference and some from
;; elsewhere
;; syntax: 
;;   '(' class-symbol :unary props* uri ')' |
;;   '(' relation-symbol (:binary|:ternary) props* uri ')'
;;   '(' relation-symbol inverse-symbol ')' ( :binary | :ternary ) props* '(' uri inverse-uri ')' ')' 

(terms
 '((entity :unary)
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
   (exists-at  :binary)
   ((o-part-of o-has-part) :binary)
   ((t-part-of t-has-part) :binary)
   (("occupies" "occupied-by")  :binary)
   ((profile-of has-profile) :binary)
   ((c-part-of c-has-part) ternary)
   ((c-ppart-of c-has-ppart) ternary)
   ((m-part-of m-has-part) ternary)
   ((located-in has-location) ternary)
   ((located-at-r r-location-of) ternary)
   ((inheres-in bearer-of) ternary)
   ((s-dep-on has-s-dep) ternary)
   ((g-dep-on has-g-dep) ternary)
   ((q-of has-q) ternary)
   ((f-of has-f) ternary)
   ((r-of has-r) ternary)
   ((d-of has-d) ternary)
   ((realizes realized-in) :binary)
   ((has-material-basis material-basis-of) ternary)
   ((concretizes concretization-of) ternary)
   ((st-projects-onto-s s-projection-of-st) ternary)
   ((st-projects-onto-t t-projection-of-st) binary)
))

;; define the class hierarchy (unary symbols). This is done from the figure.
(class-hierarchy "
entity
-continuant
--ic
---object
---fiat-object
---object-aggregate
--sdc
---quality
----r-quality
---realizable
----disposition
-----function
----role
--gdc
--immaterial
---site
---cf-boundary
----0d-cf-boundary
----1d-cf-boundary
----2d-cf-boundary
---s-region
----0d-s-region
----1d-s-region
----2d-s-region
----3d-s-region
-occurrent
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
s-dep-on 
-inheres-in 
--q-of 
--f-of 
--r-of 
--d-of 
g-dep-on 
located-in 
located-at
has-participant
has-material-basis 
concretizes 
st-projects-onto-s 
c-part-of 
-c-ppart-of 
-m-part-of 
")

;; URIs of terms that didn't make it. Make sure not to use them again.

(obsolete-ruttenterms)
