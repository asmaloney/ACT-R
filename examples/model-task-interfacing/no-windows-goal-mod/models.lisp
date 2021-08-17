
(clear-all)

;; Create one copy of the model code using defparameter.
;; The model is told its position in the my-pos slot and
;; can use that to access the value.

(defparameter *model-code*
  
  ;; A list of information just like one would put in a 
  ;; define-model call.
  
  '((sgp :v t :trace-detail high :er t)
    
    (chunk-type play my-pos p1 p2 state result)
    
    (define-chunks move win lose p1 p2 game-over)
    
    ;; Avoid any warnings about slot usage in the goal
    ;; buffer since the slots are being set in code
    
    (declare-buffer-usage goal play :all)
     
    (p 1-step
       =goal>
         my-pos =me
         =me =pos
         state move
     ==>
       !output! (I am at position =pos)
       
       ;; call the function to take the action
       !eval! ("make-move" 1)
       -goal>)
    
    (p 2-step
       =goal>
         my-pos =me
         =me =pos
         state move
     ==>
       !output! (I am at position =pos)
       
       ;; call the function to take the action
       !eval! ("make-move" 2)
       -goal>)
    
    (p result
       =goal>
         result =r
         state game-over
     ==>
       !output! (I =r)
       -goal>)))

;; Use the define-model function to create models named
;; model1 and model2 each using the same definition code.

(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)

