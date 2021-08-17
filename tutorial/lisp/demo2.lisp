(load-act-r-model "ACT-R:tutorial;unit2;demo2-model.lisp")

(defvar *response* nil)


(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (setf *response* key)
  (clear-exp-window))


(defun demo2-experiment (&optional human)
  
  (reset)
  
  (let* ((items (permute-list '("B" "C" "D" "F" "G" "H" "J" "K" "L" "M" "N" 
                                "P" "Q" "R" "S" "T" "V" "W" "X" "Y" "Z")))
         (text1 (first items))
         (window (open-exp-window "Letter recognition")))
    
    (add-text-to-exp-window window text1 :x 125 :y 150)
    
    (add-act-r-command "demo2-key-press" 'respond-to-key-press 
                       "Demo2 task output-key monitor")
    (monitor-act-r-command "output-key" "demo2-key-press")
    
    (setf *response* nil) 
    
    (if human
        (when (visible-virtuals-available?)
          (while (null *response*)
            (process-events)))
      
      (progn
        (install-device window)
        (run 10 t)))
    
    (remove-act-r-command-monitor "output-key" "demo2-key-press")
    (remove-act-r-command "demo2-key-press")
    
    *response*))

