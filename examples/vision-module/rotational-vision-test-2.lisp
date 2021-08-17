;;; This is a simple example model to show the new options of
;;; clockwise and counterclockwise which can be used with the
;;; :nearest request parameter in a visual-location request.
;;;
;;; The difference between this model and rotational-vision-test-1
;;; is that this model indicates the object around which it will
;;; rotate explicitly using the :center request parameter instead
;;; of using the point set with set-visual-center-point.
;;; 
;;; The display is a clock face in an experiment window.  The
;;; model finds the marker for the center of the clock (the red
;;; button). Then it finds the top most number (12), attends to 
;;; it, and then finds and attends to items clockwise until it 
;;; returns to a number it has seen before.  Then, it switches 
;;; to finding and attending to items counterclockwise until it
;;; again  returns to a previously seen number.  It then switches
;;; to the reference item outside of the clock (the green button),
;;; and repeats the process.
;;;
;;; 
;;; 

(clear-all)

(defun run-test ()
  (reset)
  
  ;; open a visible experiment window and install it as a device.
  ;; the window has the default width and height of 300 pixels each
  ;; and default location of 300,300.
  ;; Installing an AGI window device automatically sets the
  ;; default center point reference to the center of that window.
  
  (install-device (open-exp-window "Test"))
  
  ;; draw the clock face off to one side with a red button in
  ;; the center and a green button off to the right.
  
  (add-button-to-exp-window nil :x 100 :y 100 :height 25 :width 25 :color 'red)
  
  (add-text-to-exp-window nil "12" :x 100 :y 30 :height 25)
  (add-text-to-exp-window nil "1" :x 135  :y 39  :height 25)
  (add-text-to-exp-window nil "2"  :x 161  :y 65  :height 25)
  (add-text-to-exp-window nil "3"  :x 170  :y 100  :height 25)
  (add-text-to-exp-window nil "4"  :x 161  :y 135 :height 25)
  (add-text-to-exp-window nil "5"  :x 135  :y 161 :height 25)
  (add-text-to-exp-window nil "6"  :x 100  :y 170 :height 25)
  (add-text-to-exp-window nil "7"  :x 65 :y  161 :height 25)
  (add-text-to-exp-window nil "8" :x  39 :y  135 :height 25)
  (add-text-to-exp-window nil "9" :x  30 :y  100 :height 25)
  (add-text-to-exp-window nil "10" :x  39 :y  65 :height 25)
  (add-text-to-exp-window nil "11" :x  65 :y  39 :height 25)       
  
  (add-button-to-exp-window nil :x 250 :y 100 :height 25 :width 25 :color 'green)
    
  (run 15 t))


(define-model test-rotation
    
    (sgp :show-focus t)
    
  (chunk-type goal start dir center-loc current finish color)
  
  (p start
     ?goal>
       buffer empty
     ==>
     -visual-location>
     +goal>
       isa goal
       dir clockwise
       color red)
  
  (p find-ref
     =goal>
       color =c
       start nil
       center-loc nil
     ==>
     =goal>
       color nil
     +visual-location>
       color =c)
  
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
  
  (p encode-center
     =goal>
       isa goal
       start nil
       center-loc nil
       dir =dir
     =visual>
       - color black
       screen-pos =loc
     ==>
     =goal>
       ; we could use either =loc or =visual here because
       ; either a location or object can be used as the center
       center-loc =loc
     +visual-location>
       kind     text
       screen-y lowest)
  
  (p encode-and-record
     =goal>
       isa goal
       start nil
       center-loc =loc
       dir =dir
     =visual>
       isa text
       value =val
     ==>
     =goal>
       start =val
       current =val
     !output! (Attending to =val))
  
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
       current =val
     !output! (Attending to =val))

  
  (p encode-and-switch-dir
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
       current =val
     !output! (Attending to =val))
  
  (p encode-and-switch-ref
     =goal>
       isa goal
       dir counterclockwise
       start =val
       finish nil
     =visual>
       isa text
       value =val
     ==>
     =goal>
       dir clockwise
       center-loc nil
       start nil
       finish t
     +visual-location>
       color green
     !output! (Attending to =val))
  
  
  (p find-next
     =goal>
       isa goal
       dir =dir
       current =val
       center-loc =c-loc
     ?visual-location>
       buffer empty
     ?visual>
       buffer empty
       state free
     ==>
     +visual-location>
       isa visual-location
       - value =val
       kind text
       :nearest =dir
       :center =c-loc)
  )
