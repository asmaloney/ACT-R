;;; This model demonstrates the visual-location request to change the
;;; specification for the visual-location buffer stuffing in the same
;;; way that the set-visloc-default command can be used by the modeler.
;;; 
;;; The request to do so is just like a typical visual-location request
;;; to find a feature, except that it includes a slot named set-visloc-default
;;; with a value of t.  The chunk-type set-visloc-default is a subtype of
;;; visual-location and includes that slot as a default value.  Therefore
;;; a request which includes 'isa set-visloc-default' can be used instead
;;; of specifying the slot directly, and subtypes of that chunk-type can
;;; be created if desired.
;;;
;;; That request triggers an immediate reprocessing of the visual scene by
;;; the vision module to determine if there is a chunk to be stuffed into
;;; the visual-location buffer given the new specification.
;;;
;;; The visual-scene used here is the same one found in the new-visicon-features
;;; example.

(clear-all)

(defun test-visloc-defaults ()
  
  (reset)
  
  (let ((feats (add-visicon-features '(screen-x 0 screen-y 5 regular true)
                                     '(isa (visual-location oval) screen-x 12 screen-y 20 value (oval "the oval") height 10 width 40 color blue)
                                     '(isa (polygon-feature polygon) screen-x 50 screen-y 50 value (polygon "poly 1") height 20 width 40 color blue regular false sides (nil 7))
                                     '(isa (polygon-feature square) screen-x 10 screen-y 50 value (square "the square") height 30 width 30 color red regular true sides (nil 4))
                                     '(isa (polygon-feature polygon) screen-x 5 screen-y 70 value (polygon "poly 2") height 50 width 45 color green regular false sides (nil 5)))))
    
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    
    (run-n-events 3)
    (print-visicon)
    
    ;; Then just run the model until it's done (takes less than 1 second).
    (run 1)))


;;; The model is very simple in that it just repeatedly changes the
;;; defaults for buffer stuffing and then prints out the chunk that
;;; was stuffed into the buffer as a result.
;;;

(define-model set-visloc-default-request-test
    
    (sgp :v t :trace-detail medium)
  
  (chunk-type (polygon-feature (:include visual-location)) regular (poly-feat t))
  (chunk-type (polygon (:include visual-object)) sides)
  (chunk-type (square (:include polygon)) (square t))
  (chunk-type goal step (state task))
  (chunk-type wait step (state wait))
  
  ;; Define a subtype of the set-visloc-default chunk-type that
  ;; also inherits from the polygon-feature.
  
  (chunk-type (polygon-visloc-spec (:include polygon-feature)
                                   (:include set-visloc-default)))
  
  ;; Define the chunks used in the features and the model 
  
  (define-chunks square polygon true false wait task)
  
  (p start
     ?goal>
       buffer empty
     ==>
     +goal>
       isa wait
       step 0
     !output! (The default spec is leftmost :attended new))
  
  (p print-and-advance
     "Just print out the chunk in the
      visual-location buffer and advance the step"
     =goal>
       isa wait
       step =step
     =visual-location>
   ==>
     !bind! =next (1+ =step)
     !output! (Here is the chunk in the visual-location buffer)
     !eval! (pprint-chunks-fct (list =visual-location))
     +goal>
       isa goal
       step =next)
     
  (p find-oval
     =goal>
       isa goal
       step 1
     ?visual-location>
       buffer empty
    ==>
     +goal>
       isa wait
       step 1
     +visual-location>
       isa set-visloc-default
       kind oval
     !output! (The specification is now kind oval))
  
  (p find-regular
     =goal>
       isa goal
       step 2
     ?visual-location>
       buffer empty
    ==>
     +goal>
       isa wait
       step 2
     
     ;; This shows the use of the subtype to specify
     ;; new slots and the inclusion of default slots.
     
     +visual-location>
       isa polygon-visloc-spec
       regular true
     
     !output! (The specification is now for a regular polygon feature))

  
    (p find-on-diagonal
     =goal>
       isa goal
       step 3
     ?visual-location>
       buffer empty
    ==>
     +goal>
       isa wait
       step 3
     
       ;; This shows that the variables allowed in the
       ;; normal visual-location requests can also be used
       ;; in specifying the defaults.
     
     +visual-location>
       isa set-visloc-default
       screen-x &x
       screen-y &x
     !output! (The specification is now that the x and y coords are equal))
  
  (p restore-default
     =goal>
      isa goal
      step 4
     ?visual-location>
       buffer empty
    ==>
     +goal>
       isa wait
       step 4
     +visual-location>
       isa set-visloc-default
       screen-x lowest
       :attended new
         
     !output! (The specification is now back to default of screen-x lowest :attended new))
  )

  
  
