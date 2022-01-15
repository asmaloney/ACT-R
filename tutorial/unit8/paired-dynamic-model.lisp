(clear-all)

(define-model paired-learning-dynamic
    
(sgp :esc t :v t :pct t :ul t :epl t :rt -1.7 :lf 0.2 :ans 0.4 :egs 0.1 :bll 0.5 :trace-detail low :iu 5 :alpha 0.2)
(sgp :do-not-harvest imaginal)

(chunk-type operator pre action arg1 arg2 post label required slot success failure)
(chunk-type task state step context)

(define-chunks (stimulus-read)(read)(start)(recalled)(number)(retrieve)(wait)
               (respond)(type)(new-trial)(complete-task)(ready)(retrieving-operator)(process)(retrieving-result))

(add-dm
 (op1 isa operator pre start         action read     label word    post stimulus-read)
 (op2 isa operator pre stimulus-read action retrieve required word label number post recalled)
 (op3 isa operator pre recalled      slot   number   success respond            failure wait)
 (op4 isa operator pre respond       action type     required number            post wait)
 (op5 isa operator pre wait          action read     label number  post new-trial)
 (op6 isa operator pre new-trial     action complete-task          post start))

(set-all-base-levels 1000)

(goal-focus-fct (car (define-chunks (isa task state start step ready))))

(p retrieve-operator
   =goal>           
     isa     task
     state   =state
     step    ready
  ==>
   +retrieval>
     isa     operator
     pre     =state
   =goal>
     step    retrieving-operator
     context nil)

(p read-and-create
   =goal>
     isa       task
     step      retrieving-operator
   =retrieval>
     isa       operator
     action    read
     label     =destination
     post      =state
   =visual-location>
   ?visual>
     state     free
   ?imaginal>
     buffer    empty
     state     free
  ==>
   +imaginal>
   +visual>
     isa        move-attention
     screen-pos =visual-location
   =goal>
    context    =destination
    step       process
    state      =state)

(p read
   =goal>
     isa        task
     step       retrieving-operator
   =retrieval>
     isa        operator
     action     read
     label      =destination
     post       =state
   =visual-location>
   ?visual>
     state      free
   ?imaginal>
     buffer     full
  ==>
   +visual>
     isa        move-attention
     screen-pos =visual-location
   =goal>
     context    =destination
     step       process
     state      =state)

(p encode
   =goal>
     isa         task
     step        process
     context     =which-slot
   =visual>
     isa         text
     value       =val
   =imaginal>
   ?imaginal>
     state       free
  ==>
   *imaginal>
     =which-slot =val
   =goal>
     step        ready)

(p retrieve-associate
   =goal>
     isa      task
     step     retrieving-operator
   =imaginal>
     =target  =stimulus
   =retrieval>
     isa      operator
     action   retrieve
     required =target
     label    =other
     post     =state
  ==>
   +retrieval>
     =target  =stimulus
   =goal>
     step     retrieving-result
     context  =other
     state    =state)

(p retrieval-unsuccessful
   =goal>
     isa      task
     step     retrieving-result
     context  =dest
   =imaginal>
   ?retrieval>
     buffer   failure
   ?imaginal>
     state    free
  ==>
   *imaginal>
     =dest    nil
   =goal>
     step     ready)

(p retrieval-successful
   =goal>
     isa      task
     step     retrieving-result
     context  =dest
   =imaginal>
   =retrieval>
     =dest    =val
   ?imaginal>
     state    free
  ==>
   *imaginal>
     =dest    =val
   =goal>
     step     ready)

(p unsuccessful-test
   =goal>
     isa     task
     step    retrieving-operator
   =imaginal>
     =dest   nil
   =retrieval>
     isa     operator
     slot    =dest
     failure =state
   ?imaginal>
     state   free
  ==>
   =goal>
     state   =state
     step    ready)

(p successful-test
   =goal>
     isa     task
     step    retrieving-operator
   =imaginal>
     =dest   =val
   =retrieval>
     isa     operator
     slot    =dest
     success =state
   ?imaginal>
     state   free
  ==>
   =goal>
     state   =state
     step    ready)

(p type
   =goal>
     isa      task
     step     retrieving-operator
   =imaginal>
     =slot    =val
   =retrieval>
     isa      operator
     action   type
     required =slot
     post     =state
   ?manual>
     state    free
  ==>
   +manual>
     cmd      press-key
     key      =val
   =goal>
     state    =state
     step     ready)

(p complete-task
   =goal>
     isa    task
     step   retrieving-operator
   =retrieval>
     isa    operator
     action complete-task
     post   =state
   ?imaginal>
     state  free
  ==>
   -imaginal>
   +goal>
     isa    task
     state  =state
     step   ready)

(spp complete-task :reward 20)
)
