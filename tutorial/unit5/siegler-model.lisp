(clear-all)

(define-model siegler
    
    (sgp :rt -.45 :esc t :v nil :act nil :ans 0.5 :mp 16)

  
(chunk-type plus-fact addend1 addend2 sum)
(chunk-type number aural-rep vocal-rep)

(add-dm
 (zero  ISA number aural-rep 0 vocal-rep "zero")
 (one   ISA number aural-rep 1 vocal-rep "one")
 (two   ISA number aural-rep 2 vocal-rep "two")
 (three ISA number aural-rep 3 vocal-rep "three")
 (four  ISA number aural-rep 4 vocal-rep "four")
 (five  ISA number aural-rep 5 vocal-rep "five")
 (six   ISA number aural-rep 6 vocal-rep "six")
 (seven ISA number aural-rep 7 vocal-rep "seven")
 (eight ISA number aural-rep 8 vocal-rep "eight")
 (nine  ISA number aural-rep 9 vocal-rep "nine")
 (f00 ISA plus-fact addend1 zero addend2 zero sum zero)
 (f01 ISA plus-fact addend1 zero addend2 one sum one)
 (f02 ISA plus-fact addend1 zero addend2 two sum two)
 (f03 ISA plus-fact addend1 zero addend2 three sum three)
 (f04 ISA plus-fact addend1 zero addend2 four sum four)
 (f05 ISA plus-fact addend1 zero addend2 five sum five)
 (f10 ISA plus-fact addend1 one addend2 zero sum one)
 (f11 ISA plus-fact addend1 one addend2 one sum two)
 (f12 ISA plus-fact addend1 one addend2 two sum three)
 (f13 ISA plus-fact addend1 one addend2 three sum four)
 (f14 ISA plus-fact addend1 one addend2 four sum five)
 (f15 ISA plus-fact addend1 one addend2 five sum six)
 (f20 ISA plus-fact addend1 two addend2 zero sum two)
 (f21 ISA plus-fact addend1 two addend2 one sum three)
 (f22 ISA plus-fact addend1 two addend2 two sum four)
 (f23 ISA plus-fact addend1 two addend2 three sum five)
 (f24 ISA plus-fact addend1 two addend2 four sum six)
 (f25 ISA plus-fact addend1 two addend2 five sum seven)
 (f30 ISA plus-fact addend1 three addend2 zero sum three)
 (f31 ISA plus-fact addend1 three addend2 one sum four)
 (f32 ISA plus-fact addend1 three addend2 two sum five)
 (f33 ISA plus-fact addend1 three addend2 three sum six)
 (f34 ISA plus-fact addend1 three addend2 four sum seven)
 (f35 ISA plus-fact addend1 three addend2 five sum eight)
 (f40 ISA plus-fact addend1 four addend2 zero sum four)
 (f41 ISA plus-fact addend1 four addend2 one sum five)
 (f42 ISA plus-fact addend1 four addend2 two sum six)
 (f43 ISA plus-fact addend1 four addend2 three sum seven)
 (f44 ISA plus-fact addend1 four addend2 four sum eight)
 (f45 ISA plus-fact addend1 four addend2 five sum nine)
 (f50 ISA plus-fact addend1 five addend2 zero sum five)
 (f51 ISA plus-fact addend1 five addend2 one sum six)
 (f52 ISA plus-fact addend1 five addend2 two sum seven)
 (f53 ISA plus-fact addend1 five addend2 three sum eight)
 (f54 ISA plus-fact addend1 five addend2 four sum nine))

(p hear-sound
   =aural-location>
     isa        audio-event
     location   external
   ?aural>
     state      free
  ==>
   +aural>
     event      =aural-location)

(p encode-digit 
   =aural>
     isa        sound
     content    =val
   ?retrieval>
     state      free
  ==>
   +retrieval>
     isa        number
     aural-rep  =val)

(p harvest-arg1
   =retrieval>
   ?imaginal>
     buffer     empty
     state      free
  ==>
   +imaginal>
     isa        plus-fact
     addend1    =retrieval)

(p harvest-arg2
   =retrieval>
   =imaginal>
     isa          plus-fact
     addend2      nil
   ?imaginal> 
     state        free
  ==>
   *imaginal>
     addend2      =retrieval)

(P retrieve-answer
   =imaginal>
     ISA          plus-fact
     addend1      =val1
     addend2      =val2
     sum          nil
   ?retrieval>
     state        free
     buffer       empty
  ==>
   =imaginal>
  
   +retrieval>
     ISA          plus-fact
     addend1      =val1
     addend2      =val2)

(P harvest-answer
   =retrieval>
      ISA         plus-fact
      sum         =number
   =imaginal>
      isa         plus-fact
   ?imaginal>
      state       free
  ==>
   *imaginal>
      sum         =number
   +retrieval>    =number)

(P respond
   =imaginal>
     isa         plus-fact
     sum         =result
   =retrieval>
     ISA         number
     vocal-rep   =name
   ?vocal>
     state       free
  ==>
   +vocal>
     cmd         speak
     string      =name)


;; The base level activation of the number chunks are
;; set high so that there are no errors in retrieving them -
;; the assumption is that the kids know the numbers and have  
;; no errors from hearing or speaking them.

(set-base-levels 
  (zero 10) (one 10) (two 10) (three 10) (four 10) (five 10)
  (six 10) (seven 10) (eight 10) (nine 10))

;; The similarity between the numbers are set with a simple
;; linear distance measure:
;; Mji =  -.1 * |val(i) - val(j)|


(Set-similarities
  (zero one -0.1) (one two -0.1) (two three -0.1) (three four -0.1) (four five -0.1)
  (zero two -0.2)(one three -0.2)(two four -0.2)(three five -0.2)
  (zero three -0.3)(one four -0.3) (two five -0.3)
  (zero four -0.4)(one five -0.4)
  (zero five -0.5))


;; Set the base-levels of the small facts (sum <= 5) slightly 
;; above the default of 0.0 because more errors occurred to low side  
;; of answer and other research has found that small addition
;; problems occur more frequently.
;; Use the same offset for all of them because the objective is not
;; to try and engineer every detail found in the data.


(set-base-levels 
 (f00 .65)(f01 .65)(f02 .65)(f03 .65)(f04 .65)
 (f10 .65)(f11 .65)(f12 .65)(f13 .65)
 (f20 .65)(f21 .65)(f22 .65)
 (f30 .65)(f31 .65)
 (f40 .65))

)
