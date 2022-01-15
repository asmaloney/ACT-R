(clear-all)

(define-model 1-hit-model 
    
  ;; do not change these parameters
  (sgp :esc t :bll .5 :ol t :sim-hook "1hit-bj-number-sims" 
       :cache-sim-hook-results t :er t :lf 0)
  
  ;; adjust these as needed
  (sgp :v nil :ans .2 :mp 10.0 :rt -60)
  
  ;; This type holds all the game info 
  
  (chunk-type game-state
     mc1 mc2 mc3 mstart mtot mresult oc1 oc2 oc3 ostart otot oresult state)
  
  ;; This chunk-type should be modified to contain the information needed
  ;; for your model's learning strategy
  
  (chunk-type learned-info mc1 action)
  
  ;; Declare the slots used for the goal buffer since it is
  ;; not set in the model defintion or by the productions.
  ;; See the experiment code text for more details.
  
  (declare-buffer-usage goal game-state :all)
  
  ;; Create chunks for the items used in the slots for the game
  ;; information and state
  
  (define-chunks win lose bust retrieving start results)
    
  ;; Provide a keyboard for the model's motor module to use
  (install-device '("motor" "keyboard"))
  
  (p start
     =goal>
       isa game-state
       state start
       MC1 =c
    ==>
     =goal>
       state retrieving
     +retrieval>
       isa learned-info
       MC1 =c
     - action nil)

  (p cant-remember-game
     =goal>
       isa game-state
       state retrieving
     ?retrieval>
       buffer  failure
     ?manual>
       state free
    ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key "s")
  
  (p remember-game
     =goal>
       isa game-state
       state retrieving
     =retrieval>
       isa learned-info
       action =act
     ?manual>
       state free
    ==>
     =goal>
       state nil
     +manual>
       cmd press-key
       key =act
     
     @retrieval>)
  
  
  (p results-should-hit
     =goal>
       isa game-state
       state results
       mresult =outcome
       MC1 =c
     ?imaginal>
       state free
    ==>
     !output! (I =outcome)
     =goal>
       state nil
     +imaginal>
       MC1 =c 
       action "h")

  (spp results-should-hit :u 10)

  
  (p results-should-stay
     =goal>
       isa game-state
       state results
       mresult =outcome
       MC1 =c
     ?imaginal>
       state free
    ==>
     !output! (I =outcome)
     =goal>
       state nil
     +imaginal>
       MC1 =c 
       action "s") 
  
  (p clear-new-imaginal-chunk
     ?imaginal>
       state free
       buffer full
     ==>
     -imaginal>)
  )
