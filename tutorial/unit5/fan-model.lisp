(clear-all)

(define-model fan

  (sgp :v t :act nil :esc t :lf .63 :mas 1.6) 
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
  
 
  (P find-person
     ?visual-location>
        buffer      unrequested
     ?imaginal>
        state       free
     ==>
     +imaginal>
  
     +visual-location>
        ISA         visual-location
        screen-x    lowest)

  (P attend-visual-location
     =visual-location>
   
     ?visual-location>
        buffer      requested
     ?visual>
        state       free
     ==>
     +visual>
        cmd         move-attention
        screen-pos  =visual-location)

  (P retrieve-meaning
     =visual>
        ISA         visual-object
        value       =word
     ==>
     +retrieval>
        ISA         meaning
        word        =word)

  (P encode-person
     =retrieval>
   
     =imaginal>
        ISA         comprehend-sentence
        arg1        nil
     ==>
     =imaginal>
        arg1        =retrieval
     +visual-location>
        ISA         visual-location
        screen-x    highest)

  (P encode-location
     =retrieval>
   
     =imaginal>
        ISA         comprehend-sentence
        arg1        =arg
        arg2        nil
     ==>
     =imaginal>
        arg2        =retrieval)

  (P retrieve-from-person
     =imaginal>
        ISA         comprehend-sentence
        arg1        =person
        arg2        =location
     ?retrieval>
        state       free
        buffer      empty 
     ==>
     =imaginal>
     +retrieval>
        ISA         comprehend-sentence
        arg1        =person)

  (P retrieve-from-location
     =imaginal>
        ISA         comprehend-sentence
        arg1        =person
        arg2        =location
     ?retrieval>
        state       free
        buffer      empty 
     ==>
     =imaginal>
     +retrieval>
        ISA         comprehend-sentence
        arg2        =location)

  (P yes
     =imaginal>
        ISA         comprehend-sentence
        arg1        =person
        arg2        =location
     =retrieval>
        ISA         comprehend-sentence
        arg1        =person
        arg2        =location
     ?manual>   
        state       free   
     ==>
     +manual>
        cmd         press-key
        key         "k")

  (P mismatch-person
     =imaginal>
        ISA         comprehend-sentence
        arg1        =person
        arg2        =location
     =retrieval>
        ISA         comprehend-sentence
      - arg1        =person
     ?manual>   
        state       free   
     ==>
     +manual>
        ISA         press-key
        key         "d")

  (P mismatch-location
     =imaginal>
        ISA         comprehend-sentence
        arg1        =person
        arg2        =location
     =retrieval>
        ISA         comprehend-sentence
      - arg2        =location
     ?manual>   
        state       free   
     ==>
     +manual>
        cmd         press-key
        key         "d"))

