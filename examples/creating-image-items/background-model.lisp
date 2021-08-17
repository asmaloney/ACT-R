;;; This model is a simple demonstration of using arbitrary
;;; visicon features in conjunction with the image AGI items.
;;; It performs the task created by the background-image.lisp or
;;; background_image.py file.
;;;
;;; The model attends to the items it sees in the window (except
;;; for the background image) from left to right and top to 
;;; bottom moving the mouse to each as it goes.  It also prints
;;; the visual chunks which it receives.
;;;


(clear-all)


(define-model background
    
  (define-chunks same-line next-line)
    
  (sgp :v t :show-focus t :needs-mouse t)
  
  (p find-start
     ?goal>
       buffer empty
     =visual-location>
     ==>
     +goal>
     +visual-location>
      - kind image
       screen-y lowest
       screen-x lowest)
  
  (p attend
     =goal>
     =visual-location>
     ?visual>
       state free
     ==>
     =goal>
       state nil
     +visual>
       cmd move-attention
       screen-pos =visual-location
     !eval! (pprint-chunks-fct (list =visual-location)))

  (p attend-and-move
     =visual>
       screen-pos =loc
     ?manual>
       state free
   ==>
     +manual> 
       isa move-cursor
       loc =loc
     !eval! (pprint-chunks-fct (list =visual)))

  (p find-next
     =goal>
       state nil
     ?visual-location>
       buffer empty
     ?visual>
       state free
       buffer empty
     ?manual>
       state free
     ==>
     =goal>
       state same-line
     +visual-location>
       screen-y current
      > screen-x current
       screen-x lowest
      - kind image)
  
  (p next-row
     =goal>
       state same-line
     ?visual-location>
       buffer failure
     ==>
     =goal>
       state next-line
     +visual-location>
      > screen-y current
       screen-y lowest
       screen-x lowest
      - kind image)
  
  (p no-more
     =goal>
       state next-line
     ?visual-location>
       buffer failure
     ==>
     -goal>))
  
  

