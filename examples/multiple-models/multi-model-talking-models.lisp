;;; This file contains three models which can talk to each other
;;; through the system provided by the multi-model-talking code.

(clear-all)


;;; Each model will output what it hears and from whom in 
;;; the trace.  M2 will start things off by saying "hello" 
;;; and M1 will respond to anything which it hears by 
;;; saying "back at you".  M3 doesn't do anything other 
;;; than listen.

;;; These models ignore their own speech by only having
;;; chunks which aren't "location self" stuffed into the
;;; audio-location buffer.

(define-model m1
    
  (chunk-type goal speaker)
  
  (set-audloc-default - location self :attended nil)
  
  (p detected-sound
     =aural-location>
       isa      audio-event
       location =who
     ?aural>
       state    free
     ==>
     +goal>
       speaker  =who
     +aural>
       isa      sound
       event    =aural-location)
  
  (p hear
     =goal>
       isa     goal
       speaker =who
     =aural>
       isa     sound
       content =x
     ?vocal>
       state   free
     ==>
     +vocal>
       isa     speak
       string  "Back at you"
     !output! (I heard =who say =x))
)


(define-model m2
    
  (chunk-type goal speaker)
  (set-audloc-default - location self :attended nil)
  
  (p speak
     ?goal>
       buffer empty
     ?vocal>
       state  free
     ==>
     +goal>
       speaker self
     +vocal>
       isa    speak
       string "Hello")
  
  
  (p detected-sound
     =aural-location>
       isa      audio-event
       location =who
     ?aural>
       state    free
     ==>
     +goal>
       speaker  =who
     +aural>
       isa      sound
       event    =aural-location)
  
  (p hear
     =goal>
       isa     goal
       speaker =who
     =aural>
       isa     sound
       content =x
     ==>
     !output!  (I heard =who say =x))
)

  
(define-model m3
   
  (chunk-type goal speaker)
  (set-audloc-default - location self :attended nil)
  
    
  (p detected-sound
     =aural-location>
       isa      audio-event
       location =who
     ?aural>
       state    free
     ==>
     +goal>
       speaker  =who
     +aural>
       isa      sound
       event    =aural-location)
  
  (p hear
     =goal>
       isa     goal
       speaker =who
     =aural>
       isa     sound
       content =x
     ==>
     !output!  (I heard =who say =x))
)
