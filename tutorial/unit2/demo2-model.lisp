

(clear-all)

(define-model demo2

(sgp :seed (123456 0))
(sgp :v t :show-focus t :trace-detail high)
  
(chunk-type read-letters step)
(chunk-type array letter)

(add-dm 
 (start isa chunk) (attend isa chunk)
 (respond isa chunk) (done isa chunk)
 (goal isa read-letters step start))

(P find-unattended-letter
   =goal>
      ISA         read-letters
      step        start
 ==>
   +visual-location>
      :attended    nil
   =goal>
      step        find-location
)

(P attend-letter
   =goal>
      ISA         read-letters
      step        find-location
   =visual-location>
   ?visual>
      state       free
==>
   +visual>
      cmd         move-attention
      screen-pos  =visual-location
   =goal>
      step        attend
)

(P encode-letter
   =goal>
      ISA         read-letters
      step        attend
   =visual>
      value       =letter
   ?imaginal>
      state       free
==>
   =goal>
      step        respond
   +imaginal>
      isa         array
      letter      =letter
)


(P respond
   =goal>
      ISA         read-letters
      step        respond
   =imaginal>
      isa         array
      letter      =letter
   ?manual>   
      state       free
==>
   =goal>
      step        done
   +manual>
      cmd         press-key
      key         =letter
)

(goal-focus goal)

)
