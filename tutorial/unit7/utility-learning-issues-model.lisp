(clear-all)

(define-model utility-learning-test
    
  (sgp :seed (100 2))
  (sgp :esc t :ul t :egs .5)
  
  (chunk-type choose choice)
  (chunk-type response answer)
         
  (define-chunks (initial-goal isa choose) (response isa response)
      (a isa chunk) (b isa chunk))
  
  (declare-buffer-usage imaginal response answer)
  
  (p choose-a
     =goal>
       isa choose
       choice nil
   ==>
     =goal>
       choice a)
  
  (p choose-b
     =goal>
       isa choose
       choice nil
   ==>
     =goal>
       choice b)
  
  (p response-matches
     =goal>
       isa choose
       choice =choice
     =imaginal>
       isa response
       answer =choice
   ==>
     -goal>)
  
  (p response-doesnt-match
     =goal>
       isa choose
       choice =choice
     =imaginal>
       isa response
       - answer =choice
       - answer nil
   ==>
     -goal>)
  
  (p unknown-response
     =goal>
       isa choose
       choice =choice
     =imaginal>
       isa response
       answer nil
   ==>
     -goal>)
  
  (spp response-matches :reward 4)
  (spp response-doesnt-match :reward 0)
         
  (schedule-event 0 "utility-learning-issues-choose" :output 'medium))
