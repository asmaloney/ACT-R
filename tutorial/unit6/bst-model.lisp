
(clear-all)

(define-model bst

(sgp :v nil :esc t :egs 3 :show-focus t :ul t :ult t :needs-mouse t)

(chunk-type try-strategy strategy state)
(chunk-type encoding a-loc b-loc c-loc goal-loc length over under)

(define-chunks 
    (goal isa try-strategy state start)
    (start) (find-line) (looking) (attending)
    (encode-under) (encode-over) (choose-strategy)
    (calculate-difference) (consider-next) (check-for-done)
    (read-done) (evaluate-c) (prepare-mouse) (evaluate-a)
    (over) (under) (move-mouse) (wait-for-click))


(p start-trial
   =goal>
      isa      try-strategy
      state    start
   ?visual-location>
      buffer   unrequested
  ==>
   =goal>
      state    find-line)

(p find-next-line
   =goal>
      isa       try-strategy
      state     find-line
  ==>
   +visual-location>
      isa       visual-location
      :attended nil
      kind      line
      screen-y  lowest
   =goal>
      state     looking)


(p attend-line
   =goal>
      isa        try-strategy
      state      looking
   =visual-location>
   ?visual>
      state      free
  ==>
   =goal>
      state      attending
   +visual>
      isa        move-attention
      screen-pos =visual-location)

(p encode-line-a
   =goal>
      isa        try-strategy
      state      attending
   =visual>
      isa        line
      screen-pos =pos
   ?imaginal>
      buffer     empty
      state      free
  ==>
   +imaginal>
      isa        encoding
      a-loc      =pos
   =goal>
      state      find-line)

(p encode-line-b
   =goal>
      isa        try-strategy
      state      attending
   =imaginal>
      isa        encoding
      a-loc      =a
      b-loc      nil
   =visual>
      isa        line
      screen-pos =pos
  ==>
   =imaginal>
      b-loc      =pos
   =goal>
      state      find-line)

(p encode-line-c
   =goal>
      isa        try-strategy
      state      attending
   =imaginal>
      isa        encoding
      b-loc      =b
      c-loc      nil
   =visual>
      isa        line
      screen-pos =pos
  ==>
   =imaginal>
      c-loc      =pos
   =goal>
      state      find-line)

(p encode-line-goal
   =goal>
      isa        try-strategy
      state      attending
   =imaginal>
      isa        encoding
      c-loc      =c
      goal-loc   nil
   =visual>
      isa        line
      screen-pos =pos
      width      =length
   ?visual>
      state      free
  ==>
   =imaginal>
      goal-loc   =pos
      length     =length
   =goal>
      state      encode-under
   +visual>
      isa        move-attention
      screen-pos =c)

(p encode-under
   =goal>
      isa        try-strategy
      state      encode-under
   =imaginal>
      isa        encoding
      b-loc      =b
      length     =goal-len
   =visual>
      isa        line
      width      =c-len
   ?visual>
      state      free
  ==>
   !bind! =val   (- =goal-len =c-len)

   =imaginal>
      under      =val
   =goal>
      state      encode-over
   +visual>
      isa        move-attention
      screen-pos =b)

(p encode-over
   =goal>
      isa      try-strategy
      state    encode-over
   =imaginal>
      isa      encoding
      length   =goal-len
   =visual>
      isa      line
      width    =b-len
  ==>
   !bind! =val (- =b-len =goal-len)
   =imaginal>
      over     =val
   =goal>
      state    choose-strategy)

(p encode-line-current
   =goal>
      isa        try-strategy
      state      attending
   =imaginal>
      isa        encoding
      goal-loc   =goal-loc
   =visual>
      isa        line
      width      =current-len
   ?visual>
      state      free
  ==>
   =imaginal>
      length     =current-len
   =goal>
      state      calculate-difference
   +visual>
      isa        move-attention
      screen-pos =goal-loc)

(p calculate-difference
   =goal>
      isa      try-strategy
      state    calculate-difference
   =imaginal>
      isa      encoding
      length   =current-len
   =visual>
      isa      line
      width    =goal-len
  ==>
   !bind! =val (abs (- =current-len =goal-len))
   =imaginal>
      length   =val
   =goal>
      state    consider-next)

(p check-for-done
   =goal>
      isa       try-strategy
      state     consider-next
   =imaginal>
      isa       encoding
      length    0
  ==>
   =goal>
      state     check-for-done
   +visual-location>
      isa       visual-location
      value     "done")

(p find-done
   =goal>
      isa        try-strategy
      state      check-for-done
   =visual-location>
   ?visual>
      state      free
  ==>
   +visual>
      isa        move-attention
      screen-pos =visual-location
   =goal>
      state      read-done)

(p read-done
   =goal>
      isa    try-strategy
      state  read-done
   =visual>
      isa    text
      value  "done"
  ==>
   +goal>
      isa    try-strategy
      state  start)

(p consider-c
   =goal>
      isa        try-strategy
      state      consider-next
   =imaginal>
      isa        encoding
      c-loc      =c-loc
    > length     0
   ?visual>
       state     free
  ==>
   =imaginal>
   =goal>
      state      evaluate-c
   +visual>
      isa        move-attention
      screen-pos =c-loc)

(p choose-c
   =goal>
      isa       try-strategy
      state     evaluate-c
   =imaginal>
      isa       encoding
      length    =difference
   =visual>
      isa       line
   <= width     =difference
  ==>
   =imaginal>
   =goal>
      state     prepare-mouse
   +visual-location>
      isa       visual-location
      kind      oval
      value     "c")

(p consider-a
   =goal>
      isa        try-strategy
      state      evaluate-c
   =imaginal>
      isa        encoding
      a-loc      =a-loc
      length     =difference
   =visual>
      isa        line
    > width      =difference
   ?visual>
      state      free
  ==>
   =imaginal>
   =goal>
      state      evaluate-a
   +visual>
      isa        move-attention
      screen-pos =a-loc)

(p choose-a
   =goal>
      isa       try-strategy
      state     evaluate-a
   =imaginal>
      isa       encoding
      length    =difference
   =visual>
      isa       line
   <= width     =difference
  ==>
   =imaginal>
   =goal>
      state     prepare-mouse
   +visual-location>
      isa       visual-location
      kind      oval
      value     "a")

(p reset
   =goal>
      isa       try-strategy
      state     evaluate-a
   =imaginal>
      isa       encoding
      length    =difference
   =visual>
      isa       line
    > width     =difference
  ==>
   =imaginal>
   =goal>
      state     prepare-mouse
   +visual-location>
      isa       visual-location
      kind      oval
      value     "reset")

(p decide-over
   =goal>
      isa       try-strategy
      state     choose-strategy
      strategy  nil
   =imaginal>
      isa       encoding
      under     =under
      over      =over
   !eval! (< =over (- =under 25))
  ==>
   =imaginal>
   =goal>
      state     prepare-mouse
      strategy  over
   +visual-location>
      isa       visual-location
      kind      oval
      value     "b")

(p force-over
   =goal>
      isa       try-strategy
      state     choose-strategy
    - strategy  over
  ==>
   =goal>
      state     prepare-mouse
      strategy  over
   +visual-location>
      isa       visual-location
      kind      oval
      value     "b")

(p decide-under
   =goal>
      isa       try-strategy
      state     choose-strategy
      strategy  nil
   =imaginal>
      isa       encoding
      over      =over
      under     =under
   !eval! (< =under (- =over 25))
  ==>
   =imaginal>
   =goal>
      state     prepare-mouse
      strategy  under
   +visual-location>
      isa       visual-location
      kind      oval
      value     "c")

(p force-under
   =goal>
      isa       try-strategy
      state     choose-strategy
    - strategy  under
  ==>
   =goal>
      state     prepare-mouse
      strategy  under
   +visual-location>
      isa       visual-location
      kind      oval
      value     "c")

(p move-mouse
   =goal>
      isa        try-strategy
      state      prepare-mouse
   =visual-location>
   ?visual>
      state      free
   ?manual>
      state      free
  ==>
   +visual>
      isa        move-attention
      screen-pos =visual-location
   =goal>
      state      move-mouse
   +manual>
      isa        move-cursor
      loc        =visual-location)

(p click-mouse
   =goal>
      isa    try-strategy
      state  move-mouse
   ?manual>
      state  free
  ==>
   =goal>
      state  wait-for-click
   +manual>
      isa    click-mouse)

(p look-for-current
   =goal>
      isa       try-strategy
      state     wait-for-click
   ?manual>
      state     free
   =visual>
      - value "reset"
  ==>
   +visual-location>
      isa       visual-location
      kind      line
      screen-y  highest
   =goal>
      state     looking)

(p pick-another-strategy
   =goal>
      isa      try-strategy
      state    wait-for-click
   ?manual>
      state    free
   =visual>
      value    "reset"
  ==>
   =goal>
      state    choose-strategy)


(goal-focus goal)

(spp decide-over :u 13)
(spp decide-under :u 13)
(spp force-over :u 10)
(spp force-under :u 10)

(spp pick-another-strategy :reward 0)
(spp read-done :reward 20))
