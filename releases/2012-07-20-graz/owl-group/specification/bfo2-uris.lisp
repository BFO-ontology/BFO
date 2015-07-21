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
 (history nil !obo:BFO_0000182) ; alan allocated
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
 (has-t-part nil !obo:BFO_0000121)
 (t-ppart-of nil !obo:BFO_0000136)
 (has-t-ppart nil !obo:BFO_0000181) ;alanr assigned
 (occupies nil !obo:BFO_0000130)  
 (occupied-by nil !obo:BFO_0000126)
 (has-profile nil !obo:BFO_0000119)
 (profile-of nil !obo:BFO_0000133)
 (c-has-part_at nil !obo:BFO_0000110)
 (c-has-part_st nil !obo:BFO_0000178) ; alanr assigned
 (c-part-of_at nil !obo:BFO_0000177) ; alanr assigned
 (c-part-of_st nil !obo:BFO_0000176) ;alanr assigned
 (c-ppart-of_at nil !obo:BFO_0000137)
 (c-ppart-of_st nil !obo:BFO_0000175) ;alanr assigned
 (c-has-ppart_at nil !obo:BFO_0000111)
 (c-has-ppart_st nil  !obo:BFO_0000174) ;alanr assigned
 (member-part-of_st nil !obo:BFO_0000129)
 (member-part-of_at nil !obo:BFO_0000173) ;alanr assigned
 (has-member-part_st nil !obo:BFO_0000115)
 (has-member-part_at nil !obo:BFO_0000172) ;alanr assigned
 (located-in_at nil !obo:BFO_0000082)
 (located-in_st nil !obo:BFO_0000171) ;alanr assigned
 (has-location_st nil !obo:BFO_0000124)
 (has-location_at nil !obo:BFO_0000170)
 (located-at-r_st nil !obo:BFO_0000083)
 (r-location-of_st nil !obo:BFO_0000123)
 (inheres-in_at nil !obo:BFO_0000052)
 (bearer-of_st nil !obo:BFO_0000053) 
 (bearer-of_at nil !obo:BFO_0000158)  ; alan assigned
 (s-depends-on_at nil !obo:BFO_0000070) 
 (s-depends-on_st nil !obo:BFO_0000169)  ; alanr assigned
 (has-s-dep_st nil !obo:BFO_0000125) 
 (has-s-dep_at nil !obo:BFO_0000168)  ; alanr assigned
 (g-depends-on_st nil !obo:BFO_0000084)
 (has-g-dep_st nil !obo:BFO_0000101)
 (q-of_at nil !obo:BFO_0000080)
 (has-q_st nil !obo:BFO_0000086)  
 (has-q_at nil !obo:BFO_0000159)   ; alan assigned
 (f-of_at nil !obo:BFO_0000079)
 (has-f_st nil !obo:BFO_0000085)
 (has-f_at nil !obo:BFO_0000160) ; alanr assigned
 (r-of_at nil !obo:BFO_0000081)
 (has-r_st nil !obo:BFO_0000087)
 (has-r_at nil !obo:BFO_0000161) ;alanr assigned
 (d-of_at nil !obo:BFO_0000107) 
 (has-d_st nil !obo:BFO_0000112)
 (has-d_at nil !obo:BFO_0000162) ;alanr assigned
 (realized-in nil !obo:BFO_0000054 (:issue 39))
 (realizes nil !obo:BFO_0000055 (:issue 39))
 (has-material-basis_at nil !obo:BFO_0000113)  
 (material-basis-of_st nil !obo:BFO_0000127)  
 (material-basis-of_at nil !obo:BFO_0000163) ; alanr assigned
 (concretizes_st nil !obo:BFO_0000059)
 (concretizes_at nil !obo:BFO_0000164) ; alanr assignd
 (concretized-by_st nil !obo:BFO_0000058 )
 (concretized-by_at nil !obo:BFO_0000165) ; alanr assigned
 (st-projects-onto-s_st nil !obo:BFO_0000151 (:issue 41)) ; in version end of may, alan assigned
 (s-projection-of-st_st nil !obo:BFO_0000152 (:issue 41)) ; alan assigned
 (st-projects-onto-t nil !obo:BFO_0000153 (:issue 41)) ; alan assigned
 (t-projection-of-st nil !obo:BFO_0000154 (:issue 41)) ; alan assigned
 (spans nil !obo:BFO_0000155) ; in version end of may alan assigned
 (span-of nil !obo:BFO_0000156) ; in version end of may alan assigned
 (participates-in_st nil !obo:BFO_0000056)
 (participates-in_at nil !obo:BFO_0000166) ; alanr assigned
 (has-participant_st nil !obo:BFO_0000057)
 (has-participant_at nil !obo:BFO_0000167)
 (exists-at nil !obo:BFO_0000108) 
 (during-which-exists nil !obo:BFO_0000157)  ; alan assigned
 (o-has-ppart nil !obo:BFO_0000118)
 (o-ppart-of nil !obo:BFO_0000138)

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
 (has-ppart nil !obo:BFO_0000120 :not (:issue 46))
 (ppart-of nil !obo:BFO_0000135 :not (:issue 46))
 (has-material-part nil !obo:BFO_0000114 (:maybe schulz) (:issue 33))
 (material-part-of nil !obo:BFO_0000128 (:maybe schulz) (:issue 33))
 (b-depends-on nil !obo:BFO_0000102 (:maybe schulz))
 (o-boundary-of nil !obo:BFO_0000131 (:maybe schulz))
 (c-boundary-of nil !obo:BFO_0000104 (:maybe schulz))
 (contained-in nil !obo:BFO_0000103 :obsolete)
 (1d-t-region-dup nil !obo:BFO_0000143 (:duplicate !obo:BFO_0000038))
 (projects-onto nil !obo:BFO_0000134 :not (:issue 41))

 (occurs-in nil !obo:BFO_0000066)
 (contains-process nil !obo:BFO_0000183)
 (history-of nil !obo:BFO_0000184)
 (has-history nil !obo:BFO_0000185)
 (c-part-of-object_at nil !obo:BFO_0000186)
 (c-has-part-object_at nil !obo:BFO_0000187)

)
