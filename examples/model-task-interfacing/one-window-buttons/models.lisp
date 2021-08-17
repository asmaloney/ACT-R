(clear-all)

(define-model red-player
  (sgp :v t :trace-detail high :needs-mouse t :er t :show-focus red :needs-mouse red)
  
  ;; stuff buttons that aren't white or blue
  
  (set-visloc-default kind oval - color white - color blue)
      
  ;; Create a chunk with the player color and player label and make it the current goal
  (chunk-type play my-color my-number state)    
  (define-chunks (goal isa play my-color red my-number "1"))
  
  (goal-focus goal)

  (define-chunks move read check done click)

       
  (install-device '("motor" "cursor" "mouse"))
  
  (start-hand-at-mouse)
      
      (p my-turn-from-left
         =goal>
           state nil
           my-color red
         =visual-location>
           screen-x =x
           color red
       ==>
         ;; cheat and restrict location based on code
         !bind! =limit (+ =x 100)
         +visual-location>
           kind oval
           color white
          > screen-x =x
          < screen-x =limit
         =goal>
           state move)
      
      (p my-turn-from-right
         =goal>
           state nil
           my-color blue
         =visual-location>
           screen-x =x
           color blue
       ==>
         ;; cheat and restrict location based on code
         !bind! =limit (- =x 100)
         +visual-location>
           kind oval
           color white
          < screen-x =x
          >= screen-x =limit
         =goal>
           state move)
      
      (p game-over
         =goal>
           state nil
         =visual-location>
           screen-x =x
           screen-y =y
           color green
       ==>
         +visual-location>
           kind text
           screen-x =x
           screen-y =y
         =goal>
           state read)
      
      (p read
         =goal>
           state read
         =visual-location>
         ?visual>
           state free
       ==>
         =goal>
         state check
         +visual>
         isa move-attention
         screen-pos =visual-location)
      
      (p yeah
         =goal>
           state check
           my-number =num
         =visual>
           value =num
       ==>
         !output! (I won)
         =goal>
           state done)
      
      (p boo
         =goal>
           state check
           my-number =num
         =visual>
          - value =num
       ==>
         !output! (I lost)
         =goal>
           state done)
      

      (p move
         =goal>
           state move
         =visual-location>
         ?manual>
           state free
       ==>
         +manual>
           cmd move-cursor
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
           cmd click-mouse
         =goal>
         state nil))

(define-model blue-player
  (sgp :v t :trace-detail high :needs-mouse t :er t :show-focus blue :needs-mouse blue)
  
  ;; stuff buttons that aren't white or red
  
  (set-visloc-default kind oval - color white - color red)
      
  ;; Create a chunk with the player color and player label and make it the current goal
      
  (chunk-type play my-color my-number state)
  (define-chunks (goal isa play my-color blue my-number "2"))
  
  (goal-focus goal)

  (define-chunks move read check done click)

      
  (install-device '("motor" "cursor" "mouse"))
  
  (start-hand-at-mouse)
      
      (p my-turn-from-left
         =goal>
           state nil
           my-color red
         =visual-location>
           screen-x =x
           color red
       ==>
         ;; cheat and restrict location based on code
         !bind! =limit (+ =x 100)
         +visual-location>
           kind oval
           color white
          > screen-x =x
          < screen-x =limit
         =goal>
           state move)
      
      (p my-turn-from-right
         =goal>
           state nil
           my-color blue
         =visual-location>
           screen-x =x
           color blue
       ==>
         ;; cheat and restrict location based on code
         !bind! =limit (- =x 100)
         +visual-location>
           kind oval
           color white
          < screen-x =x
          >= screen-x =limit
         =goal>
           state move)
      
      (p game-over
         =goal>
           state nil
         =visual-location>
           screen-x =x
           screen-y =y
           color green
       ==>
         +visual-location>
           kind text
           screen-x =x
           screen-y =y
         =goal>
           state read)
      
      (p read
         =goal>
           state read
         =visual-location>
         ?visual>
           state free
       ==>
         =goal>
         state check
         +visual>
         isa move-attention
         screen-pos =visual-location)
      
      (p yeah
         =goal>
           state check
           my-number =num
         =visual>
           value =num
       ==>
         !output! (I won)
         =goal>
           state done)
      
      (p boo
         =goal>
           state check
           my-number =num
         =visual>
          - value =num
       ==>
         !output! (I lost)
         =goal>
           state done)
      

      (p move
         =goal>
           state move
         =visual-location>
         ?manual>
           state free
       ==>
         +manual>
           cmd move-cursor
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
           cmd click-mouse
         =goal>
         state nil))