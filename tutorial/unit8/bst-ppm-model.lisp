(clear-all)

(define-model bst-learn

  (sgp :v nil :esc t :egs 3 :show-focus t :trace-detail medium :ul t :ult t :needs-mouse t)
  (sgp :ppm 40 :sim-hook "bst-number-sims")
  (sgp :cache-sim-hook-results t)
  
(chunk-type try-strategy strategy state)
(chunk-type encoding a-loc b-loc c-loc goal-loc length goal-length b-len c-len difference)

(define-chunks start find-line looking attending choose-strategy
  consider-next check-for-done read-done evaluate-c prepare-mouse
  evaluate-a over under move-mouse wait-for-click)

(add-dm (goal isa try-strategy state start))

(declare-buffer-usage imaginal encoding difference length)

(p start-trial
    =goal>
      isa    try-strategy
      state  start
    ?visual-location>
      buffer unrequested
   ==>
    =goal>
      state  find-line)

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
      isa     try-strategy
      state   looking
    =visual-location>
    ?visual>
      state   free
   ==>
    =goal>
      state      attending
    +visual>
      cmd        move-attention
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
      isa      encoding
      a-loc    =pos
    =goal>
      state    find-line)

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
      width      =b-len
   ==>
    =imaginal>
      b-loc    =pos
      b-len    =b-len
    =goal>
      state    find-line)

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
      width      =c-len
   ==>
    =imaginal>
      c-loc    =pos
      c-len    =c-len
    =goal>
      state    find-line)

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
      goal-loc     =pos
      goal-length  =length
    =goal>
      state        choose-strategy)

(p encode-line-current
    =goal>
      isa      try-strategy
      state    attending
    =imaginal>
      isa      encoding
      goal-loc =goal-loc
    =visual>
      isa      line
      width    =current-len
    ?visual>
      state    free
    ?imaginal-action> 
      state    free   
  ==>
    =imaginal>
      length     =current-len
    +imaginal-action>
      action     "bst-compute-difference"
      simple     t
    =goal>
      state      consider-next
    +visual>
      cmd        move-attention
      screen-pos =goal-loc)

(p check-for-done
    =goal>
      isa         try-strategy
      state       consider-next
    =imaginal>
      isa         encoding
     > difference -1  
     < difference 1
    ?imaginal>
      state       free
   ==>
    =goal>
      state     check-for-done
    +visual-location>
      isa       visual-location
      value     "done")

(p find-done
    =goal>
      isa      try-strategy
      state    check-for-done
    =visual-location>
    ?visual>
      state    free
   ==>
    +visual>
      cmd        move-attention
      screen-pos =visual-location
    =goal>
      state      read-done)

(p read-done
    =goal>
      isa      try-strategy
      state    read-done
    =visual>
      isa      text
      value    "done"
==>
    +goal>
      isa      try-strategy
      state    start)

(p consider-c
    =goal>
      isa         try-strategy
      state       consider-next
    =imaginal>
      isa         encoding
      c-loc       =c-loc
     - difference 0 
    ?imaginal>
     state        free
    ?visual>
       state      free
   ==>
    =imaginal>
    =goal>
      state      evaluate-c
    +visual>
      cmd        move-attention
      screen-pos =c-loc)

(p choose-c
    =goal>
      isa        try-strategy
      state      evaluate-c
    =imaginal>
      isa        encoding
      difference =difference
    =visual>
      isa        line
    <= width     =difference
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
    +visual-location>
      isa      visual-location
      kind     oval
      value    "c")

(p consider-a
    =goal>
      isa        try-strategy
      state      evaluate-c
    =imaginal>
      isa        encoding
      a-loc      =a-loc
      difference =difference
    =visual>
      isa        line
    > width      =difference
    ?visual>
      state free
   ==>
    =imaginal>
    =goal>
      state      evaluate-a
    +visual>
      cmd        move-attention
      screen-pos =a-loc)

(p choose-a
    =goal>
      isa        try-strategy
      state      evaluate-a
    =imaginal>
      isa        encoding
      difference =difference
    =visual>
      isa        line
    <= width     =difference
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
    +visual-location>
      isa      visual-location
      kind     oval
      value    "a")

(p reset
    =goal>
      isa        try-strategy
      state      evaluate-a
    =imaginal>
      isa        encoding
      difference =difference
    =visual>
      isa        line
     > width     =difference
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
    +visual-location>
      isa      visual-location
      kind     oval
      value    "reset")

(p decide-over
    =goal>
      isa         try-strategy
      state       choose-strategy
     - strategy   over
    =imaginal>
      isa         encoding
      goal-length =d
      b-len       =d
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
      strategy over
    +visual-location>
      isa      visual-location
      kind     oval
      value    "b")

(p decide-under
    =goal>
      isa         try-strategy
      state       choose-strategy
    -  strategy   under
    =imaginal>
      isa         encoding
      goal-length =d
      c-len       =d
   ==>
    =imaginal>
    =goal>
      state    prepare-mouse
      strategy under
    +visual-location>
      isa      visual-location
      kind     oval
      value    "c")

(p move-mouse
    =goal>
      isa    try-strategy
      state  prepare-mouse
    =visual-location>
    ?visual>
      state  free
    ?manual>
      state  free
   ==>
    +visual>
      cmd        move-attention
      screen-pos =visual-location
    =goal>
      state      move-mouse
    +manual>
      cmd        move-cursor
      loc        =visual-location)

(p click-mouse
    =goal>
      isa     try-strategy
      state   move-mouse
    ?manual>
      state   free
   ==>
    =goal>
      state   wait-for-click
    +manual>
      cmd     click-mouse)

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
      isa       try-strategy
      state     wait-for-click
    ?manual>
      state     free
    =visual>
      value     "reset"
   ==>
    =goal>
      state choose-strategy)

(goal-focus goal)

(spp decide-over :u 10)
(spp decide-under :u 10)

(spp pick-another-strategy :reward 0)
(spp read-done :reward 20)
)
