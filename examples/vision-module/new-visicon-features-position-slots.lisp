;;; This example demonstrates how one can change the slots used for 
;;; the position information in the visual features.

(defun run-example ()
  
  (reset)
  
 
  
  ;; For this example these chunk-types are defined in the model:
  
  ;; (chunk-type real-space x y z size)
  ;; (chunk-type other-space pos-1 pos-2 pos-3 size value)
  
  ;; We will add three features: one using the initial position slots
  ;; of screen-x, screen-y, and distance, one using the x, y, and z
  ;; slots, and one using pos-1, pos-2, and pos-3 slots.
  
  
  (add-visicon-features '(screen-x 0 screen-y 5 distance 1080))
  
  (set-default-vis-loc-slots x y z)
  
  (add-visicon-features '(x 10 y 15 z 1080))

  (set-default-vis-loc-slots-fct 'pos-1 'pos-2 'pos-3)
  
  (add-visicon-features '(pos-1 50 pos-2 15 pos-3 1080 value t))
                                     
    
  ;; Give the vision module a chance to process the display
  ;; before printing the visicon.
    
  (run-n-events 3)
  (print-visicon)
  
  ;; Then just run the model
  (run 10))
  
  

;;; The model is very simple in that it just repeatedly finds
;;; unattended locations and attends to them printing
;;; out the chunks in the visual-location and visual buffers
;;; after the corresponding requet completes.
;;;

(load-act-r-model "ACT-R:examples;vision-module;new-visicon-features-position-slots-model.lisp")

