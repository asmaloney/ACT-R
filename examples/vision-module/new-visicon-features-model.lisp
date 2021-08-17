
(clear-all)

;;; The model is very simple in that it just repeatedly finds
;;; a location and then attends to the item there printing
;;; out the chunks in the visual-location and visual buffers
;;; after the corresponding requet completes.
;;;
;;; The visual-location requests are specified such that 
;;; there will only be one visual-location which matches
;;; the request.  
;;;
;;; The productions which make the visual-location requests
;;; describe the new features being used to perform the 
;;; request.

(define-model new-visual-features
    
    (sgp :v t :trace-detail low)
  
  (chunk-type (polygon-feature (:include visual-location)) regular)
  
  (chunk-type (polygon (:include visual-object)) sides)
  (chunk-type (square (:include polygon)) (sides 4) (square t))
  
  (chunk-type goal step)
  
  ;; Do this to avoid the warnings when the chunks are created
  
  (define-chunks true false square polygon)
  
  ;; Define this chunk for use in a nearest request
  
  (define-chunks (loc-100x100 isa visual-location screen-x 100 screen-y 100))
  
  (p find-oval
     ?goal>
      buffer empty
     ==>
     +goal>
      step 0
     
     ;; Start by finding the oval by specifying the
     ;; kind value which was filled in automatically.
     
     +visual-location>
      kind oval)
  
  
  (p find-regular
     =goal>
      step 1
     ?visual-location>
      buffer empty
     ?visual>
      buffer empty
      state free
     ==>
     
     ;; This request shows using one of the new
     ;; feature slots.
     
     +visual-location>
       screen-x lowest
       regular true)
    
  (p find-on-diagonal
     =goal>
      step 2
     ?visual-location>
      buffer empty
     ?visual>
      buffer empty
      state free
     ==>
     
     ;;; This request shows the use of a request variable.
     ;;; The visual-location requets consider any value 
     ;;; which starts with an & to be a variable in the
     ;;; same way that a production uses = in the LHS matching.
     ;;; Thus, this tests that the screen-x and screen-y values
     ;;; are the same.
     ;;;
     ;;; It's important to note that this is only the case
     ;;; for +visual-location requests - not a general feature
     ;;; of RHS requests.  Other modules could use other variable
     ;;; indicators if they wanted (or none at all).
     
     +visual-location>
      screen-x &x
      screen-y &x)
  
  (p find-fewer-sides-than-current
     =goal>
      isa goal
      step 3
     ?visual-location>
      buffer empty
     ?visual>
      buffer empty
      state free
     ==>
     
     ;;; This request shows the special marker current
     ;;; which can be used in any slot.
     
     +visual-location>
       < sides current
       regular false)
  
  
  (p find-fewest-sides
     =goal>
      isa goal
      step 4
     ?visual-location>
      buffer empty
     ?visual>
      buffer empty
      state free
     ==>
     
     ;; similarly lowest and highest can also
     ;; be used with any slot having numeric
     ;; values (features which don't have the slot
     ;; or have a non-numeric value will be ignored).
     
     +visual-location>
      sides lowest)
  
  (p shift-attention
     =goal>
     =visual-location>
     ?visual>
      state free
     ==>
     +visual>
      isa move-attention
      screen-pos =visual-location
     !output! (Here is the chunk in the visual-location buffer)
     !eval! (pprint-chunks-fct (list =visual-location)))
  
  (p attend-item
     =goal>
      isa goal
      step =step
     =visual>
     ==>
     !output! (Here is the chunk in the visual buffer)
     !eval! (pprint-chunks-fct (list =visual))
     !bind! =next-step (1+ =step)
     =goal>
      step =next-step)   
  )

  
  
