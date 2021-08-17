;;; This model is a simple demonstration of using the image AGI items.
;;; It performs the task created by the creating-an-image.lisp or
;;; creating_an_image.py file.
;;;
;;; The model attends to the items it sees in the window from
;;; top to bottom and for each one moves the mouse to a position
;;; slightly offset from the true location of the item and then
;;; clicks the mouse.
;;;

(clear-all)


(define-model image-testing-model

  (sgp :v t :show-focus t :needs-mouse t)
  
  (set-visloc-default screen-y lowest)
  
  (p attend
     "attend to anything placed into the visual-location buffer"
     =visual-location>
     ?visual>
       state free
   ==>
     +visual>
       cmd move-attention
     screen-pos =visual-location
     !eval! (pprint-chunks-fct (list =visual-location)))

  (p attend-and-instan-loc
     "Create a new chunk in the imaginal buffer with the location information of the attended item"
     =visual>
       screen-pos =loc
     ?imaginal>
       state free
   ==>
     +imaginal> =loc
     !eval! (pprint-chunks-fct (list =visual)))

  (p adjust
     "adjust the x,y coordinates in the imaginal buffer"
     =imaginal>
       screen-x =x
       screen-y =y
       color =color
   ==>
     !bind! =nx (- =x 5)
     !bind! =ny (+ =y 2)
     =imaginal>
       screen-x =nx
       screen-y =ny
       color nil)

  (p move
     "Move the mouse to the location in the imaginal buffer"
     =imaginal>
       color nil
     ?manual>
       state free
   ==>
     +manual>
       isa move-cursor
       loc =imaginal
     +goal>)
  
  (p click
     "Once the mouse is there click it and try to find another feature in the window"
     =goal>
     ?manual>
       state free
   ==>
     +manual>
       isa click-mouse
     +visual-location>
       > screen-y current
       :nearest current-y
     -goal>
     -imaginal>))


