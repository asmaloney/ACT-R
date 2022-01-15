(clear-all)

(define-model fan

  (sgp :v t :act nil :esc t :lf .63 :mas 1.6 :ga 1.0) 
  (sgp :style-warnings nil)
  
  (chunk-type comprehend-sentence relation arg1 arg2)
  (chunk-type meaning word)
  
  (add-dm
   (p1 ISA comprehend-sentence relation in arg1 hippie arg2 park)
   (p2 ISA comprehend-sentence relation in arg1 hippie arg2 church)
   (p3 ISA comprehend-sentence relation in arg1 hippie arg2 bank)
   (p4 ISA comprehend-sentence relation in arg1 captain arg2 park)
   (p5 ISA comprehend-sentence relation in arg1 captain arg2 cave)
   (p6 ISA comprehend-sentence relation in arg1 debutante arg2 bank)
   (p7 ISA comprehend-sentence relation in arg1 fireman arg2 park)
   (p8 ISA comprehend-sentence relation in arg1 giant arg2 beach)
   (p9 ISA comprehend-sentence relation in arg1 giant arg2 castle)
   (p10 ISA comprehend-sentence relation in arg1 giant arg2 dungeon)
   (p11 ISA comprehend-sentence relation in arg1 earl arg2 castle)
   (p12 ISA comprehend-sentence relation in arg1 earl arg2 forest)
   (p13 ISA comprehend-sentence relation in arg1 lawyer arg2 store)
   (guard ISA meaning word "guard")
   (beach ISA meaning word "beach")
   (castle ISA meaning word "castle")
   (dungeon ISA meaning word "dungeon")
   (earl ISA meaning word "earl")
   (forest ISA meaning word "forest")
   (giant ISA meaning word "giant")
   (hippie ISA meaning word "hippie")
   (park ISA meaning word "park")
   (church ISA meaning word "church")
   (captain ISA meaning word "captain")
   (cave ISA meaning word "cave")
   (debutante ISA meaning word "debutante")
   (bank ISA meaning word "bank")
   (fireman ISA meaning word "fireman")
   (lawyer ISA meaning word "lawyer")
   (store ISA meaning word "store")
   (in ISA meaning word "in"))
  
  (set-base-levels
   (guard 10) (beach 10) (castle 10) (dungeon 10) (earl 10) 
   (forest 10) (hippie 10) (park 10) (church 10) (bank 10) 
   (captain 10) (cave 10) (giant 10) (debutante 10) (fireman 10)
   (lawyer 10) (store 10) (in 10))
  


  (chunk-type sentence-goal arg1 arg2 state)
  
  (define-chunks (test isa chunk) (harvest-person isa chunk)
    (harvest-location isa chunk) (get-retrieval isa chunk)
    (goal isa sentence-goal state test))
  
  (goal-focus goal)
  
  (P start
     =goal>
        ISA         sentence-goal
        arg1        =person
        state       test   
     ==>
     =goal>
        state       harvest-person
     +retrieval>
        ISA         meaning
        word        =person)

  (P harvest-person
     =goal>
        ISA         sentence-goal
        arg2        =location
        state       harvest-person
     =retrieval>
     ==>
     =goal>
        arg1        =retrieval
        state       harvest-location
     +retrieval>
        ISA         meaning
        word        =location)

  (p harvest-location
     =goal>
        ISA         sentence-goal
        state       harvest-location
     =retrieval>
        ISA         meaning
     ==>
     =goal>
        arg2        =retrieval
        state       get-retrieval)

  (P retrieve-from-person
     =goal>
        ISA         sentence-goal
        arg1        =person
        state       get-retrieval
     ==>
     =goal>
        state       nil
     +retrieval>
        ISA         comprehend-sentence
        arg1        =person)

  (P retrieve-from-location
     =goal>
        ISA         sentence-goal
        arg2        =location
        state       get-retrieval
     ==>
     =goal>
        state       nil
     +retrieval>
        ISA         comprehend-sentence
        arg2        =location)

  (P respond-yes
     =goal>
        ISA         sentence-goal
        arg1        =person
        arg2        =location
        state       nil
     =retrieval>
        ISA         comprehend-sentence
        arg1        =person
        arg2        =location
     ==>
     =goal>
        state       "k")

  (P mismatch-person-no
     =goal>
        ISA         sentence-goal
        arg1        =person
        arg2        =location
        state       nil
     =retrieval>
        ISA         comprehend-sentence
      - arg1        =person
     ==>
     =goal>
        state       "d")

  (P mismatch-location-no
     =goal>
        ISA         sentence-goal
        arg1        =person
        arg2        =location
        state       nil
     =retrieval>
        ISA         comprehend-sentence
      - arg2        =location
     ==>
     =goal>
        state       "d")

  (spp mismatch-location-no :at .21)
  (spp mismatch-person-no :at .21)
  (spp respond-yes :at .21)
  (spp start :at .250)
  (spp harvest-person :at .285)
  )

