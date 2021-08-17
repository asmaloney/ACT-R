

(clear-all)

(define-model demo2

(sgp :seed (123456 0))
(sgp :v t :show-focus t :trace-detail high)
  
(chunk-type read-letters state)
(chunk-type array letter)

(add-dm 
 (start isa chunk) (attend isa chunk)
 (respond isa chunk) (done isa chunk)
 (goal isa read-letters state start))

(P find-unattended-letter
   =goal>
      ISA         read-letters
      state       start
 ==>
   +visual-location>
      :attended    nil
   =goal>
      state       find-location
)

(P attend-letter
   =goal>
      ISA         read-letters
      state       find-location
   =visual-location>
   ?visual>
      state       free
==>
   +visual>
      cmd         move-attention
      screen-pos  =visual-location
   =goal>
      state       attend
)

(P encode-letter
   =goal>
      ISA         read-letters
      state       attend
   =visual>
      value       =letter
   ?imaginal>
      state       free
==>
   =goal>
      state       respond
   +imaginal>
      isa         array
      letter      =letter
)


(P respond
   =goal>
      ISA         read-letters
      state       respond
   =imaginal>
      isa         array
      letter      =letter
   ?manual>   
      state       free
==>
   =goal>
      state       done
   +manual>
      cmd         press-key
      key         =letter
)

(goal-focus goal)

)
