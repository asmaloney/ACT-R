;;; This is the model which performs the tracking task to
;;; output the information as it is updated.

(clear-all)

(define-model simple-tracking
    
    (sgp :v t :needs-mouse nil :show-focus t :trace-detail high)
  
  
  (P attend
     "Attend the stuffed location" 
     =visual-location>
     ?visual>
      state       free
     ?goal>
      buffer empty
     ==>
     +goal>
     +visual>
      isa         move-attention
      screen-pos  =visual-location)
  
  (P track-letter
     "Once attended track it"
     =goal>
     =visual>
     ?visual>
      state       free
     ==>
     +visual>
      isa         start-tracking)
  
  (p report
     "Output the info as it updates"
     =goal>
     =visual-location>
      screen-x    =x
      screen-y    =y
     =visual>
      value       =val
     ==>
     !output! (=val is at =x =y))
  )