(clear-all)

(define-model sperling

(sgp :v t :declarative-finst-span 10)

(sgp :show-focus t :trace-detail low)

(sgp :seed (100 0))

(chunk-type read-letters location tone step upper-y lower-y)
(chunk-type report-row row)

(add-dm
 (attending isa chunk) 
 (medium isa chunk) 
 (find isa chunk) 
 (encode isa chunk)
 (goal isa read-letters step attending upper-y 400 lower-y 600))

(p detected-sound
   =aural-location>
   ?aural>
     state   free
   ==>
   +aural>
     event   =aural-location)

(p sound-respond-low
   =goal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  500
==>
   =goal>
     tone     low
     upper-y  500
     lower-y  520)

(p sound-respond-medium
   =goal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  1000
==>
   =goal>
     tone     medium
     upper-y  450
     lower-y  470)

(p sound-respond-high
   =goal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  2000
==>
   =goal>
     tone     high
     upper-y  400
     lower-y  420)

(p attend-low
   =goal>
     isa        read-letters
     step       attending
   =visual-location>
     isa        visual-location
   > screen-y   500
   < screen-y   520
   
   ?visual>
     state      free
   ==>
   =goal>
     location   low
     step       encode
   +visual>
     cmd        move-attention
     screen-pos =visual-location)

(p attend-medium
   =goal>
     isa        read-letters
     step       attending
   =visual-location>
     isa        visual-location
   > screen-y   450
   < screen-y   470
   
   ?visual>
     state      free
==>
   =goal>
     location   medium
     step       encode
   +visual>
     cmd        move-attention
     screen-pos =visual-location)

(p attend-high
   =goal>
     isa        read-letters
     step       attending
   =visual-location>
     isa        visual-location
   > screen-y   400
   < screen-y   420
   
   ?visual>
     state      free
==>
   =goal>
     location   high
     step       encode
   +visual>
     cmd        move-attention
     screen-pos =visual-location)

(p encode-row-and-find
   =goal>
     isa       read-letters
     location  =pos
     upper-y   =uy
     lower-y   =ly
   =visual>
==>
   =visual>
     status    =pos
   -visual>
   =goal>
     location  nil
     step      attending
   +visual-location>
     :attended nil
   > screen-y  =uy 
   < screen-y  =ly)

(P start-report
   =goal>
     isa     read-letters
     tone    =tone
   ?visual>
     state   free
   ==>
   +goal>
     isa     report-row
     row     =tone
   +retrieval>
     status  =tone)

(P do-report
   =goal>
     isa      report-row
     row      =tone
   =retrieval>
     status   =tone
     value    =val
   ?manual>
     state    free
   ==>
   +manual>              
     cmd      press-key     
     key      =val
   +retrieval>
     status   =tone
     :recently-retrieved  nil)

(p stop-report 
   =goal>
     isa     report-row
     row     =row
   ?retrieval>
     buffer  failure
   ?manual>
     state   free
==>
   +manual>              
     cmd     press-key       
     key     space
   -goal>)

(goal-focus goal)

(spp start-report :u -2)
(spp detected-sound  :u 10)
(spp sound-respond-low :u 10)
(spp sound-respond-medium :u 10)
(spp sound-respond-high :u 10)

)
