(load-act-r-model "ACT-R:tutorial;unit4;paired-model.lisp")

(defvar *response* nil)
(defvar *response-time* nil)

(defvar *pairs* '(("bank" "0") ("card" "1") ("dart" "2") ("face" "3") ("game" "4")
                  ("hand" "5") ("jack" "6") ("king" "7") ("lamb" "8") ("mask" "9")
                  ("neck" "0") ("pipe" "1") ("quip" "2") ("rope" "3") ("sock" "4")
                  ("tent" "5") ("vent" "6") ("wall" "7") ("xray" "8") ("zinc" "9")))

(defvar *paired-latencies* '(0.0 2.158 1.967 1.762 1.680 1.552 1.467 1.402))
(defvar *paired-probability* '(0.000 .526 .667 .798 .887 .924 .958 .954))

(defun paired-task (size trials &optional human)
  
  (add-act-r-command "paired-response" 'respond-to-key-press 
                     "Paired associate task key press response monitor")
  (monitor-act-r-command "output-key" "paired-response")
  
  (prog1 
      
      (do-experiment size trials human)
    
    (remove-act-r-command-monitor "output-key" "paired-response")
    (remove-act-r-command "paired-response")))


(defun respond-to-key-press (model key)
 
  (setf *response-time* (get-time model))
  (setf *response* key))


(defun do-experiment (size trials human)
  
  (if (and human (not (visible-virtuals-available?)))
      (print-warning "Cannot run the task as a person without a visible window available.")
    (progn
      (reset)
      
      (let* ((result nil)
             (model (not human))
             (window (open-exp-window "Paired-Associate Experiment" :visible human)))
        
        (when model
          (install-device window))
        
        (dotimes (i trials) 
          (let ((score 0.0)
                (time 0.0))
            
            (dolist (x (permute-list (subseq *pairs* (- 20 size)))) 
              
              (clear-exp-window window)
              (add-text-to-exp-window window (first x) :x 150 :y 150)
              
              (setf *response* nil)                   
              (let ((start (get-time model)))
                
                (if model
                    (run-full-time 5)
                  (while (< (- (get-time nil) start) 5000)
                    (process-events)))
                
                (when (equal *response* (second x))      
                  (incf score 1.0)    
                  (incf time (- *response-time* start)))
                
                (clear-exp-window window)
                (add-text-to-exp-window window (second x) :x 150 :y 150)
                (setf start (get-time model))
                
                (if model
                    (run-full-time 5)
                  (while (< (- (get-time nil) start) 5000)
                    (process-events)))))
            
            (push (list (/ score size) (if (> score 0) (/ time score 1000.0) 0)) result)))
        
        (reverse result)))))
 

(defun paired-experiment (n)
  (let ((data nil))
    (dotimes (i n)
      (if (null data)
          (setf data (paired-task 20 8))
        (setf data (mapcar (lambda (x y)
                             (list (+ (first x) (first y))
                                   (+ (second x) (second y))))
                     data (paired-task 20 8)))))
    (output-data data n)))

(defun output-data (data n)
  (print-results (mapcar (lambda (x) (/ (second x) n)) data) *paired-latencies* "Latency")
  (print-results (mapcar (lambda (x) (/ (first x) n)) data) *paired-probability* "Accuracy"))

(defun print-results (predicted data label)
  (format t "~%~A:~%" label)
  (correlation predicted data)
  (mean-deviation predicted data)
  (format t "Trial    1       2       3       4       5       6       7       8~%")
  (format t "     ~{~8,3f~}~%" predicted))

