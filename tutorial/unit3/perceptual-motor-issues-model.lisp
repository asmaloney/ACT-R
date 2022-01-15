(clear-all)

(define-model perceptual-motor-issues

    (sgp :seed (101 0))
    (sgp :v t :show-focus t :trace-detail medium :er t)

(chunk-type letter name next previous)
(chunk-type task letter)

(add-dm
 (a isa letter name "a" next "b" previous "z")
 (b isa letter name "b" next "c" previous "a")
 (c isa letter name "c" next "d" previous "b")
 (d isa letter name "d" next "e" previous "c")
 (e isa letter name "e" next "f" previous "d")
 (f isa letter name "f" next "g" previous "e")
 (g isa letter name "g" next "h" previous "f")
 (h isa letter name "h" next "i" previous "g")
 (i isa letter name "i" next "j" previous "h")
 (j isa letter name "j" next "k" previous "i")
 (k isa letter name "k" next "l" previous "j")
 (l isa letter name "l" next "m" previous "k")
 (m isa letter name "m" next "n" previous "l")
 (n isa letter name "n" next "o" previous "m")
 (o isa letter name "o" next "p" previous "n")
 (p isa letter name "p" next "q" previous "o")
 (q isa letter name "q" next "r" previous "p")
 (r isa letter name "r" next "s" previous "q")
 (s isa letter name "s" next "t" previous "r")
 (t isa letter name "t" next "u" previous "s")
 (u isa letter name "u" next "v" previous "t")
 (v isa letter name "v" next "w" previous "u")
 (w isa letter name "w" next "x" previous "v")
 (x isa letter name "x" next "y" previous "w")
 (y isa letter name "y" next "z" previous "x")
 (z isa letter name "z" next "a" previous "y"))


(add-dm 
 (start isa chunk) (attend isa chunk)
 (respond isa chunk) (done isa chunk))

(p find-letter
   =visual-location>
   ?visual>
     state       free
 ==>
   +visual>
     cmd         move-attention
     screen-pos  =visual-location
   +imaginal>
)

(p encode-letter
   =imaginal>
     isa         task
     letter      nil
   =visual>
     isa         visual-object
     value       =letter
 ==>
   =imaginal>
     letter      =letter
)

(p respond-next
   =imaginal>
     isa         task
     letter      =letter
   =visual>
     isa         visual-object
     value       "next"
   ?manual>
     state       busy
 ==>
   +retrieval>
     isa         letter
     previous    =letter
   +manual>
     cmd         press-key
     key         =letter
)

(p respond-previous
   =imaginal>
     isa         task
     letter      =letter
   =visual>
     isa         text
     value       "previous"
   ?manual>
     state       free
 ==>
   +retrieval>
     isa         letter
     next        =letter
   +manual>
     cmd         press-key
     key         =letter
)

(p respond-final
   =retrieval>
     isa         letter
     name        =letter
   ==>
   +manual>
     cmd         press-key
     key         =letter
))
