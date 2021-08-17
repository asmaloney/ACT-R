(load-act-r-model "ACT-R:tutorial;unit2;unit2-assignment-model.lisp")

(defvar *response* nil)

(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (setf *response* key))

(defun unit2-experiment (&optional human)
  
  (reset)
  
  (let* ((items (permute-list '("B" "C" "D" "F" "G" "H" "J" "K" "L" "M" "N"
                                "P" "Q" "R" "S" "T" "V" "W" "X" "Y" "Z")))
         (target (first items))
         (foil (second items))
         (window (open-exp-window "Letter difference"))
         (text1 foil)
         (text2 foil)
         (text3 foil)
         (index (act-r-random 3)))       
    
    (case index
      (0 (setf text1 target))
      (1 (setf text2 target))
      (2 (setf text3 target)))
    
    (add-text-to-exp-window window text1 :x 125 :y 75)
    (add-text-to-exp-window window text2 :x 75 :y 175)
    (add-text-to-exp-window window text3 :x 175 :y 175)
    
    (setf *response* nil)
    
    (add-act-r-command "unit2-key-press" 'respond-to-key-press 
                       "Assignment 2 task output-key monitor")
    (monitor-act-r-command "output-key" "unit2-key-press")

    (if human 
        (if (visible-virtuals-available?)
            (while (null *response*)
              (process-events)))
      
      (progn
        (install-device window)
        (run 10 t)))
    
    (remove-act-r-command-monitor "output-key" "unit2-key-press")
    (remove-act-r-command "unit2-key-press")

    (if (string-equal *response* target)
        t
      nil)))
