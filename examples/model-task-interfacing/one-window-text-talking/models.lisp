(clear-all)

(defparameter *model-code*
    '((sgp :v t :trace-detail high :needs-mouse nil :er t)
      
      (chunk-type play my-color my-name state)
      
      (set-audloc-default - location self :attended nil)
      (set-visloc-default color green)
      
      (define-chunks move say-done start)
      
      (declare-buffer-usage goal play :all)
      
      (p game-over
         =visual-location>
         ?visual>
           state free
       ==>
         +visual>
           cmd move-attention
           screen-pos =visual-location)
      
      (p winner
         =goal>
         my-name =name
         =visual>
         value =name
         ==>
         -goal>
         !output! (I am the winner))
      
      (p loser
         =goal>
         my-name =name
         =visual>
         - value =name
         value =winner
         ==>
         -goal>
         !output! (=winner was the winner))
      
      
      (p hear-something
         =goal>
         =aural-location>
         ?aural>
           state free
       ==>
         +aural>
           event =aural-location)
      
      (p I-start
         =goal>
           my-name =name
         =aural>
           content =name
       ==>
         =goal>
           state move)
      
      (p other-player-starts
         =goal>
           my-name =name
         =aural>
          - content =name
          - content "one"
          - content "two"
       ==>
         )
      
      (p other-player-went-one
         =goal>
         =aural>
           content "one"
       ==>
         =goal>
           state move)
      
      (p other-player-went-two
         =goal>
         =aural>
           content "two"
       ==>
         =goal>
           state move)

      (p 1-step
         =goal>
           state move
         ?vocal>
           state free
       ==>
         +vocal>
           cmd speak
           string "one"
         =goal>
           state nil)
      
      (p 2-step
         =goal>
           state move
         ?vocal>
           state free
       ==>
         +vocal>
           cmd speak
           string "two"
         =goal>
           state nil)      
      ))
      
(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)

