
(clear-all)

(define-model zbrodoff
    
(sgp :v nil :esc t :lf 0.4 :bll 0.5 :ans 0.5 :rt 0 :ncnar nil)

(sgp :show-focus t)

(chunk-type problem arg1 arg2 result)
(chunk-type goal state count target next-let next-num)
(chunk-type number number next visual-rep vocal-rep)
(chunk-type letter letter next visual-rep vocal-rep)

(add-dm
 (zero  ISA number number zero next one visual-rep "0" vocal-rep "zero")
 (one   ISA number number one next two visual-rep "1" vocal-rep "one")
 (two   ISA number number two next three visual-rep "2" vocal-rep "two")
 (three ISA number number three next four visual-rep "3" vocal-rep "three")
 (four  ISA number number four next five visual-rep "4" vocal-rep "four")
 (five  isa number number five)
 (a ISA letter letter a next b visual-rep "a" vocal-rep "a")
 (b ISA letter letter b next c visual-rep "b" vocal-rep "b")
 (c ISA letter letter c next d visual-rep "c" vocal-rep "c")
 (d ISA letter letter d next e visual-rep "d" vocal-rep "d")
 (e ISA letter letter e next f visual-rep "e" vocal-rep "e")
 (f ISA letter letter f next g visual-rep "f" vocal-rep "f")
 (g ISA letter letter g next h visual-rep "g" vocal-rep "g")
 (h ISA letter letter h next i visual-rep "h" vocal-rep "h")
 (i ISA letter letter i next j visual-rep "i" vocal-rep "i")
 (j ISA letter letter j next k visual-rep "j" vocal-rep "j")
 (k isa letter letter k next l visual-rep "k" vocal-rep "k")
 (l isa letter letter l)
 (goal isa goal)
 (attending) (read) (count) (counting) (encode))

(set-visloc-default screen-x lowest)

(P attend
   =goal>
      ISA         goal
      state       nil
   =visual-location>
   ?visual>
       state      free
==>
   =goal>
      state       attending
   +visual>
      cmd         move-attention
      screen-pos  =visual-location
   )

(P read-first
   =goal>
     ISA         goal
     state       attending
   =visual>
     ISA         visual-object
     value       =char
   ?imaginal>
     buffer      empty
     state       free   
==>
   +imaginal>
   +retrieval>
     isa         letter
     visual-rep  =char
   =goal>
     state       nil
   +visual-location>
     ISA         visual-location
   > screen-x    current
     screen-x    lowest
   - value       "+"
   )

(p encode-first
   =goal>
     isa           goal
     state         attending
   =retrieval>
     letter        =let
     vocal-rep     =word
   =imaginal>
     arg1          nil
   ?vocal>
     preparation   free
   ==>
   +vocal>
     cmd           subvocalize
     string        =word
   =imaginal>
     arg1          =let
   =goal>
     state         read
   )

(P read-second
   =goal>
     ISA         goal
     state       read
   =visual>
     ISA         visual-object
     value       =char
   =imaginal>
     isa         problem
    - arg1       nil
     arg2        nil
   ==>
   =imaginal>
   +retrieval>
     isa         number
     visual-rep  =char
   
   =goal>
     state       nil
   
   +visual-location>
     ISA         visual-location
     screen-x    highest
   )

(p encode-second
   =goal>
     isa          goal
     state        attending
   =retrieval>
     number       =num
     vocal-rep    =word
   =imaginal>
    - arg1        nil
     arg2         nil
   ?vocal>
     preparation  free
   ==>
   +vocal>
     cmd          subvocalize
     string       =word
   =imaginal>
     arg2         =num
   =goal>
     state        read
   )

(P read-third
   =goal>
     ISA         goal
     state       read
   =imaginal>
     isa         problem
     arg1        =arg1
     arg2        =arg2
   =visual>
     ISA         visual-object
     value       =char
   ?visual>
     state       free
==>
   =imaginal>
   +retrieval>
     isa         letter
     visual-rep  =char
   =goal>
     state       encode
   +visual>
     cmd         clear
   )

(p encode-third
   =goal>
     isa          goal
     state        encode
     target       nil
   =retrieval>
     letter       =let
     vocal-rep    =word
   =imaginal>
     arg1         =a1
     arg2         =a2
   ?vocal>
     preparation  free
   ==>
   +vocal>
     cmd          subvocalize
     string       =word
   =goal>
     target       =let
     state        count
   =imaginal>
   )


(P start-counting
   =goal>
     ISA         goal
     state       count
   =imaginal>
     isa         problem
     arg1        =a
     arg2        =val
==>
   =imaginal>
     result      =a
   =goal>
     state       counting
   +retrieval>
     ISA         letter
     letter      =a
   )

(p initialize-counting
   =goal>
     isa         goal
     state       counting
     count       nil
   =retrieval>
     isa         letter
     letter      =l
     next        =new
     vocal-rep   =t
   ?vocal>
     state       free
   ==>
   +vocal>
     isa         subvocalize
     string      =t
   =goal>
     next-num    one
     next-let    =new
     count       zero
   +retrieval>
     isa         letter
     letter      =new
   )


(P update-result
   =goal>
     ISA         goal
     count       =val
     next-let    =let
     next-num    =n
   =imaginal>
     isa         problem
    - arg2       =val
   =retrieval>
     ISA         letter
     letter      =let
     next        =new
     vocal-rep   =txt
   ?vocal>
     state       free
   ==>
   =goal>
     next-let    =new
   +vocal>
     cmd         subvocalize
     string      =txt
   =imaginal>
     result      =let
   +retrieval>
     ISA         number
     number      =n
   )

(P update-count
   =goal>
     ISA         goal
     next-let    =let
     next-num    =n
   =retrieval>
     ISA         number
     number      =val
     next        =new
     vocal-rep   =txt
   ?vocal>
     preparation free
==>
   +vocal>
     cmd         subvocalize
     string      =txt
   =goal>
     count       =val
     next-num    =new
   +retrieval>
     ISA         letter
     letter      =let
   )


(P final-answer-yes
   =goal>
     ISA         goal
     target      =let
     count       =val
   =imaginal>
     isa         problem
     result      =let
     arg2        =val
   
   ?manual>
     state       free
   ==>
   +goal>
     
   +manual>
     cmd         press-key
     key         "k"
   )

(P final-answer-no
    =goal>
     ISA         goal
     count       =val
   - target      =let
   =imaginal>
     isa         problem
     result      =let
     arg2        =val
   
   ?manual>
     state       free
   ==>
   +goal>
     
   +manual>
     cmd         press-key
     key         "d"
   )

(set-all-base-levels 100000 -1000)
(goal-focus goal)
)
