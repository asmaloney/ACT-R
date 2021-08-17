;;; This is a simple example model to show the new options of
;;; clockwise and counterclockwise which can be used with the
;;; :nearest request parameter in a visual-location request.
;;; 
;;; The display is a clock face in an experiment window.  The
;;; model finds the top most number (12), attends to it, and
;;; then finds and attends to items clockwise until it returns
;;; to a number it has seen before.  Then, it switches to finding
;;; and attending to items counterclockwise until it again 
;;; returns to a previously seen number.

(clear-all)

(defun run-test ()
  (reset)
  
  ;; open a visible experiment window and install it as a device.
  ;; the window has the default width and height of 300 pixels each
  ;; and default location of 300,300.
  ;; Installing an AGI window device automatically sets the
  ;; default center point reference to the center of that window.
  
  (install-device (open-exp-window "Test"))
  
  ;; Draw the clock face, and since there's only one window don't need 
  ;; the window reference.
  
  (add-text-to-exp-window nil "12" :x 150 :y 50 :height 25)
  (add-text-to-exp-window nil "1" :x 200  :y 63 :height  25)
  (add-text-to-exp-window nil "2" :x 237  :y 100 :height 25)
  (add-text-to-exp-window nil "3" :x 250  :y 150 :height 25)
  (add-text-to-exp-window nil "4" :x 237  :y 200 :height 25)
  (add-text-to-exp-window nil "5" :x 200  :y 237 :height 25)
  (add-text-to-exp-window nil "6" :x 150  :y 250 :height 25)
  (add-text-to-exp-window nil "7" :x 100  :y 237 :height 25)
  (add-text-to-exp-window nil "8" :x  63  :y 200 :height 25)
  (add-text-to-exp-window nil "9" :x  50  :y 150 :height 25)
  (add-text-to-exp-window nil "10" :x  63 :y 100 :height 25)
  (add-text-to-exp-window nil "11" :x 100 :y 63 :height 25)       
  
  
  ;; If we wanted to set the reference point directly we
  ;; would do this to get the same point (window_x + width/2) 
  ;; and (window_y + height/2)
  ;;
  ;;(set-visual-center-point 450 450)
  
  (run 10 t))


(define-model test-rotation
    
    (sgp :show-focus t)
    
  (chunk-type goal start dir current)
  
  (p start
     ?goal>
       buffer empty
     ==>
     +visual-location>
       isa visual-location
       screen-y lowest
     +goal>
       isa goal
       dir clockwise)
    
  (p attend
     =goal>
     =visual-location>
     ?visual>
       state free
       buffer empty
     ==>
     +visual>
       isa move-attention
       screen-pos =visual-location)
  
  (p encode-and-record
     =goal>
       isa goal
       start nil
       dir =dir
     =visual>
       isa text
       value =val
     ==>
     =goal>
       start =val
       current =val)
  
  (p encode
     =goal>
       isa goal
      - start =val
      - start nil
       dir =dir
     =visual>
       isa text
       value =val
     ==>
     =goal>
       current =val)

  
  (p encode-and-switch
     =goal>
       isa goal
       start =val
       dir clockwise
     =visual>
       isa text
       value =val
     ==>
     =goal>
       dir counterclockwise
       current =val)
  
  (p encode-and-stop
     =goal>
       isa goal
       dir counterclockwise
       start =val
     =visual>
       isa text
       value =val
     ==>
     =goal>
       dir nil
       current =val)
  
  
  (p find-next
     =goal>
       isa goal
       dir =dir
       current =val
     ?visual-location>
       buffer empty
     ?visual>
       buffer empty
       state free
     ==>
     +visual-location>
       isa visual-location
      - value =val
       :nearest =dir)
  )
