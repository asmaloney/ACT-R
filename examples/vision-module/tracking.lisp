#|

There are two example functions here: one which uses the AGI to 
display a moving X and one which uses add- & modify- visicon-features
to move an arbitrary feature.

The same model found in tracking-model.lisp performs both tasks.

To run them call either agi-tracking or arbitrary-tracking
respectively to do the task.

|#

(load-act-r-model "ACT-R:examples;vision-module;tracking-model.lisp")

(defun agi-tracking () 
  
  (reset)
  
  ;; open a window and add the text
  
  (let* ((window (open-exp-window "Moving X" :visible t))
         (text (add-text-to-exp-window window "x" :x 10 :y 10))
         (y 10))
    
    
    (install-device window)
    
    ;; schedule an event to move it
    
    (schedule-periodic-event .5 (lambda () 
                                  (modify-text-for-exp-window text :y (incf y 10)))
                             :maintenance t
                             :details "moving object"
                             :initial-delay 1.0)
    
    ;; run the model in real time since it's a visible window
    
    (run 3 t)))


(defun arbitrary-tracking ()
  
  (reset)
  
  ;; First create the visual-location chunk
  
  (let ((feature (first (add-visicon-features '(screen-x 15 screen-y 20 value "x"))))
        (x 15))
    
    ;; schedule an event to move the item
    
    (schedule-periodic-event .5 (lambda () 
                                  (modify-visicon-features (list feature 'screen-x (incf x 10))))
                             :maintenance t
                             :details "moving object"
                             :initial-delay 1.0)
    
    ;; run the model
    
    (run 3)))




