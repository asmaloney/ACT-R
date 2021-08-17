
(clear-all)

;; Create one copy of the model code using defparameter so that
;; it gets updated if we reload -- defvar doesn't re-evaluate the
;; value after the first time it is loaded.

(defparameter *model-code*
  
  ;; A list of information just like one would put in a 
  ;; define-model call.
  
  '((sgp :v t :trace-detail high :er t)
    
    (chunk-type (player-loc (:include visual-location)) name position turn result)
    (chunk-type (player (:include visual-object)) name position turn result)
    
    (chunk-type goal my-name state)
    
    (define-chunks find play win lose player)
    (define-chunks-fct (list (current-model)))
    (define-chunks-fct (list (list 'goal 'my-name (current-model))))
    
    (goal-focus goal)
    
    (install-device '("motor" "keyboard"))
    
    (p start
       =goal>
         my-name =m
         state nil
       ==>
       =goal>
         state find
       !output! (My name is =m)
       
       +visual-location>
          name =m
       )
    
    (p attend
       =goal>
         state find
       =visual-location>
       ?visual>
         state free
     ==>
       +visual>
       isa move-attention
       screen-pos =visual-location
       =goal>
       state play)
    
    (p 1-step
       =goal>
       state play
       =visual>
       turn t
       position =pos
       ?manual>
       state free
     ==>
       =visual>
       !output! (I am at position =pos)
       +manual>
       isa press-key
       key "1")
    
    (p 2-step
       =goal>
       state play
       =visual>
       turn t
       position =pos
       ?manual>
       state free
     ==>
       =visual>
       !output! (I am at position =pos)
       +manual>
       isa press-key
       key "2")    
    
    (p result
       =goal>
       state play
       =visual>
       result =r
       ==>
       
       !output! (I =r)
       -goal>)))

;; Use the define-model function to create models named
;; model1 and model2 each using the same definition code.

(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)
