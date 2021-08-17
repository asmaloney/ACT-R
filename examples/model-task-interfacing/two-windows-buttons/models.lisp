         
(clear-all)


(defparameter *model-code*
    '((sgp :v t :trace-detail high :needs-mouse t)
      (chunk-type play my-color state)
      
      (define-chunks (goal isa play my-color red)
          (move) (click))
      
      (goal-focus goal)
      
      (set-visloc-default-fct '(kind oval - color white - color blue))
      
      (install-device '("motor" "cursor" "mouse"))
      (start-hand-at-mouse)
      
      (p my-turn
         =goal>
           state nil
         =visual-location>
           screen-x =x
           color red
       ==>
         !bind! =limit (+ =x 100)
         +visual-location>
           kind oval
           color white
          > screen-x =x
          < screen-x =limit
         =goal>
           state move)
      
      (p move
         =goal>
           state move
         =visual-location>
         ?manual>
           state free
         ==>
         +manual>
           isa move-cursor
           loc =visual-location
         =goal>
           state click)
      
      (p click
         =goal>
           state click
         ?manual>
           state free
         ==>
         +manual>
           isa click-mouse
         =goal>
         state nil)
      
      (p game-over
         =goal>
         =visual-location>
         color green
         ?visual>
         state free
         ==>
         -goal>
         +visual>
         isa move-attention
         screen-pos =visual-location)
      (p report-win
         =visual>
         value "1"
         ==>
         !output! (I win))
      (p report-lose
         =visual>
         value "2"
         ==>
         !output! (I lose))))
      
(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)
