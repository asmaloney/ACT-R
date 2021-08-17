
(load-act-r-model "ACT-R:tutorial;unit6;choice-model.lisp")

(defvar *unit6-choice-data* '(0.664 0.778 0.804 0.818))

(defvar *response* nil)

(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (setf *response* key))

(defun choice-person ()
  (when (visible-virtuals-available?)
    (let ((window (open-exp-window "Choice Experiment" :visible t)))
      
      (add-act-r-command "choice-response" 'respond-to-key-press "Choice task key response")
      (monitor-act-r-command "output-key" "choice-response")
      
      (add-text-to-exp-window window "choose" :x 50 :y 100)
      
      (setf *response* nil)
      
      (while (null *response*)        
        (process-events))
      
      (clear-exp-window window)
      
      (add-text-to-exp-window window (if (< (act-r-random 1.0) .9) "heads" "tails") :x 50 :y 100)
      
      (let ((start (get-time nil)))
        
        (while (< (- (get-time nil) start) 1000)
          (process-events)))
      
      (remove-act-r-command-monitor "output-key" "choice-response")
      (remove-act-r-command "choice-response")
      
      *response*)))

(defun choice-model ()
  )

(defun choice-data (n)
  )

  
 


