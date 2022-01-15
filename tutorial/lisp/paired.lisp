; ACT-R tutorial unit 4 paired associate task.
; This experiment runs several trials of presenting a 
; word prompt, and waiting 5 seconds for a response,
; then it displays the correct response (which will be
; a digit) and waits 5 seconds before starting the next
; trial.  The time to respond to the initial prompt and
; the correctness of the response are recorded for
; comparison to the humam data of the task that had
; 20 different pairs (the digits were each paired with
; two words) over 8 blocks (a randomized ordering of the
; 20 pairs).


; Load the corresponding model for the task

(load-act-r-model "ACT-R:tutorial;unit4;paired-model.lisp")

; Global variables to hold the participant's response, the time of
; that response, the possible stimuli, and the data from the 
; original experiment.

(defvar *response* nil)
(defvar *response-time* nil)

(defvar *pairs* '(("bank" "0") ("card" "1") ("dart" "2") ("face" "3") ("game" "4")
                  ("hand" "5") ("jack" "6") ("king" "7") ("lamb" "8") ("mask" "9")
                  ("neck" "0") ("pipe" "1") ("quip" "2") ("rope" "3") ("sock" "4")
                  ("tent" "5") ("vent" "6") ("wall" "7") ("xray" "8") ("zinc" "9")))

(defvar *paired-latencies* '(0.0 2.158 1.967 1.762 1.680 1.552 1.467 1.402))
(defvar *paired-probability* '(0.000 .526 .667 .798 .887 .924 .958 .954))

; paired-task takes two required parameters: the number of stimuli to
; present in a block and the number of blocks to run.  The optional 
; parameter if specified as true will run a person instead of the model.

(defun paired-task (size trials &optional human)
  
  ; Create a command for the respond-to-key-press function so that
  ; it can be used to monitor "output-key".
  
  (add-act-r-command "paired-response" 'respond-to-key-press 
                     "Paired associate task key press response monitor")
  (monitor-act-r-command "output-key" "paired-response")
  
  ; Run the function that does the actual experiment, return the result
  ; of that, and remove the monitor and command that were added.
  
  (prog1 
      
      (do-experiment size trials human)
    
    (remove-act-r-command-monitor "output-key" "paired-response")
    (remove-act-r-command "paired-response")))


; respond-to-key-press will record the time of a key press
; using get-time (which reports model time if passed a true
; value or real time if passed nil, and model will be nil
; if it is a person performing the task) and the key that 
; was pressed.

(defun respond-to-key-press (model key)
 
  (setf *response-time* (get-time model))
  (setf *response* key))

; do-experiment takes three parameters, the number of pairs to
; present per block, the number of blocks, and whether it is a
; person performing the task.  It runs the number of blocks
; requested collecting the correctness and timing data per
; block, and then returns a list with lists where each sublist
; represents a block with the first item being % correct and
; the second mean response time.

(defun do-experiment (size trials human)
  
  (if (and human (not (visible-virtuals-available?)))
      (print-warning "Cannot run the task as a person without a visible window available.")
    
    (progn
      
      (reset)
      
      ; create a varaible to hold the data, an indication of whether the
      ; model is performing the task, and an opened experiment window that
      ; is visible if a human is performing the task or virtual if it is
      ; a model.
      
      (let* ((result nil)
             (model (not human))
             (window (open-exp-window "Paired-Associate Experiment" :visible human)))
        
        (when model
          (install-device window))
        
        ; Loop over the number of blocks

        (dotimes (i trials) 
          (let ((score 0.0)
                (time 0.0))
            
            ; randomize the list of items to present which are
            ; taken from the possible pairs
            
            (dolist (x (permute-list (subseq *pairs* (- 20 size)))) 
              
              ; clear the window and display the prompt
              
              (clear-exp-window window)
              (add-text-to-exp-window window (first x) :x 150 :y 150)
              
              ; clear the response and record the time when the trial
              ; is started
              
              (setf *response* nil)                   
              (let ((start (get-time model)))
                
                ; If it's the model run it for exactly 5 seconds
                ; and if it's a person wait for 5 seconds of real
                ; time to pass.
                
                (if model
                    (run-full-time 5)
                  (while (< (- (get-time nil) start) 5000)
                    (process-events)))
                
                ; If there is a correct response increment the
                ; count of correct answers and the cumulative
                ; response times.
                
                (when (equal *response* (second x))      
                  (incf score 1.0)    
                  (incf time (- *response-time* start)))
                
                ; Clear the window and display the correct response
                
                (clear-exp-window window)
                (add-text-to-exp-window window (second x) :x 150 :y 150)
                (setf start (get-time model))
                
                ; If it's the model run it for exactly 5 seconds
                ; and if it's a person wait for 5 seconds of real
                ; time to pass.
                
                (if model
                    (run-full-time 5)
                  (while (< (- (get-time nil) start) 5000)
                    (process-events)))))
            
            ; push a list with the percentage correct and mean response time
            ; onto the result list.
            
            (push (list (/ score size) (if (> score 0) (/ time score 1000.0) 0)) result)))
        
        ; return the reversed result list so that it's in presentation order
        ; (since they were pushed onto the list).
        
        (reverse result)))))
 
; paired-experiment takes one required parameter which is the number of times
; to run a model through the original experiment (20 pairs for 8 trials each).
; It collects the data from those trials which is passed to the output-data
; function for averaging and comparison to the original data.

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

; output-data takes two required parameters which are a list of cumulative
; data items from the experiment and the number of experiment repetitions
; that were collected. It averages the results of the latency and accuracy
; data and calls print-results to display the comparison to the original
; data along with the average results.

(defun output-data (data n)
  (print-results (mapcar (lambda (x) (/ (second x) n)) data) *paired-latencies* "Latency")
  (print-results (mapcar (lambda (x) (/ (first x) n)) data) *paired-probability* "Accuracy"))

(defun print-results (predicted data label)
  (format t "~%~A:~%" label)
  (correlation predicted data)
  (mean-deviation predicted data)
  (format t "Trial    1       2       3       4       5       6       7       8~%")
  (format t "     ~{~8,3f~}~%" predicted))

