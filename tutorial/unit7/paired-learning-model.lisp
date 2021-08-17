(clear-all)

(define-model paired-learning
    
(sgp :esc t :v nil :pct t :ul t :epl t :rt -1.7 :lf 0.2 :ans 0.4 :egs 0.1 :bll 0.5 :trace-detail low :iu 5 :alpha 0.2 :ncnar nil)

(chunk-type operator pre action arg1 arg2 post)
(chunk-type task state step)
(chunk-type args arg1 arg2)

(define-chunks ready start stimulus-read create read recalled
  fill filled associate wait respond test-arg2 response
  type new-trial complete-task retrieving-operator attending retrieving-response)

(add-dm
  (goal isa task state start step ready)
  (op1 isa operator pre start action read arg1 create post stimulus-read)
  (op2 isa operator pre stimulus-read action associate arg1 filled arg2 fill post recalled)
  (op3 isa operator pre recalled action test-arg2 arg1 respond arg2 wait)
  (op4 isa operator pre respond action type arg2 response post wait)
  (op5 isa operator pre wait action read arg2 fill post new-trial)
  (op6 isa operator pre new-trial action complete-task post start))

(set-all-base-levels 1000)

(p retrieve-operator
   =goal>           
     isa    task
     state  =state
     step   ready
  ==>
   +retrieval>
     isa    operator
     pre    =state
   =goal>
     step   retrieving-operator)

(p read-arg1
   =goal>
     isa        task
     step       retrieving-operator
   =retrieval>
     isa        operator
     action     read
     arg1       create
     post       =state
   =visual-location>
   ?visual>
     state      free
   ?imaginal>
     state      free
  ==>
   +imaginal>
     isa        args
     arg1       fill
   +visual>
     cmd        move-attention
     screen-pos =visual-location
   =goal>
     step       attending
     state      =state)

(p encode-arg1
   =goal>
     isa     task
     step    attending
   =visual>
     isa     text
     value   =val
   =imaginal>
     isa     args
     arg1    fill
   ?imaginal>
     state   free
  ==>
   *imaginal>
     arg1    =val
   =goal>
     step    ready)

(p retireve-associate
   =goal>
     isa     task
     step    retrieving-operator
   =imaginal>
     isa     args
     arg1    =stimulus
   =retrieval>
     isa     operator
     action  associate
     arg1    filled
     arg2    fill
     post    =state
   ?imaginal>
     state   free
  ==>
   +retrieval>
     isa     args
     arg1    =stimulus
   *imaginal>
     arg2    fill
   =goal>
     step    retrieving-response
     state   =state)

(p retrieval-arg2-unsuccessful
   =goal>
     isa     task
     step    retrieving-response
   =imaginal>
     isa     args
     arg2    fill
   ?retrieval>
     state   error
   ?imaginal>
     state  free
  ==>
   *imaginal>
     arg2   nil
   =goal>
     step   ready)

(p retrieval-arg2-successful
   =goal>
     isa    task
     step   retrieving-response
   =imaginal>
     isa    args
     arg2   fill
   =retrieval>
     isa    args
     arg2   =val
   ?imaginal>
     state  free
  ==>
   *imaginal>
     arg2   =val
   =goal>
     step   ready)

(p respond-to-retrieval-failure
   =goal>
     isa    task
     step   retrieving-operator
   =imaginal>
     isa    args
     arg2   nil
   =retrieval>
     isa    operator
     action test-arg2
     arg2   =state
   ?imaginal>
     state  free
  ==>
   =imaginal>
   =goal>
     state  =state
     step   ready)

(p respond-to-retrieval-success
   =goal>
     isa     task
     step    retrieving-operator
   =imaginal>
     isa     args
     arg2    =val
   =retrieval>
     isa     operator
     action  test-arg2
     arg1    =state
   ?imaginal>
     state   free
  ==>
   =imaginal>
   =goal>
     state   =state
     step    ready)

(p type-arg2
   =goal>
     isa     task
     step    retrieving-operator
   =imaginal>
     isa     args
     arg2    =val
   =retrieval>
     isa     operator
     action  type
     arg2    response
     post    =state
   ?imaginal>
     state   free
   ?manual>
     state   free
  ==>
   =imaginal>
   +manual>
     cmd     press-key
     key     =val
   =goal>
     state   =state
     step    ready)

(p read-arg2
   =goal>
     isa        task
     step       retrieving-operator
   =imaginal>
   =retrieval>
     isa        operator
     action     read
     arg2       fill
     post       =state
   =visual-location>
   ?visual>
     state      free
   ?imaginal>
     state      free
  ==>
   +visual>
     cmd        move-attention
     screen-pos =visual-location
   *imaginal>
     arg2       fill
   =goal>
     step       attending
     state      =state)

(p encode-arg2
   =goal>
     isa    task
     step   attending
   =imaginal>
     isa    args
     arg2   fill
   =visual>
     isa    text
     value  =val
   ?imaginal>
     state  free
  ==>
   *imaginal>
     arg2   =val
   =goal>
     step   ready)

(p complete-task
   =goal>
     isa     task
     step    retrieving-operator
   =retrieval>
     isa     operator
     action  complete-task
     post    =state
   ==>
   +goal>
     isa     task
     state   =state
     step    ready)


(spp complete-task :reward 20)

(goal-focus goal)
)
