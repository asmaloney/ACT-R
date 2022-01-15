; ACT-R tutorial unit 3 demonstration task.
; The sperling experiment displays a block of 12
; letters in the window, with 3 lines of 4 letters
; per line for a brief time.  After the display has
; been presented a tone sound is generated to indicate
; which row of letters must be reported (the timing of
; the tone relative to the initial display of the 
; letters is variable).  After the letters go away
; the participant must press the keys to indicate which
; letters were in the target row, and press the spacebar
; to indicate completion.


; Load the corresponding tutorial model

(load-act-r-model "ACT-R:tutorial;unit3;sperling-model.lisp")

; Define some global variables: *responses* holds the list of
; keys pressed by the participant, *show-responses* indicates
; whether or not to print out the responses provided after a
; trial is run, and *sperling-exp-data* holds the results of
; the original experiment (number of correct responses from
; the target row based on the delay of the tone) for 
; comparison to the model's performance.

(defvar *responses* nil)
(defparameter *show-responses* t)

(defvar *sperling-exp-data* '(3.03 2.40 2.03 1.50))

; The sperling-trial function runs a single trial of the task
; and returns the number of correct responses.  It requires one
; parameter which is the time in seconds to delay the tone 
; after the items have been presented.

(defun sperling-trial (onset-time)
  
  ; Reset ACT-R and all models to initial state
  
  (reset)
  
  ; create some local variables to perform the trial:
  ;  letters: is a randomized list of letter strings
  ;  answers: will be the list of letters in the target row
  ;  row: a random number from 0-2 indicating which row will
  ;       be the target
  ;  window: an experiment window created to display the task
  ;  freq: will be set to the frequency of the tone to present
  
  (let* ((letters (permute-list '("B" "C" "D" "F" "G" "H" "J" 
                                  "K" "L" "M" "N" "P" "Q" "R" 
                                  "S" "T" "V" "W" "X" "Y" "Z")))
         (answers nil)   
         (row (act-r-random 3))
         (window (open-exp-window "Sperling Experiment" :visible t))
         freq)
    
    ; Show the first 12 letters from the list in the window in
    ; three rows of four and record which ones are in the target
    ; row in the answers variable.
    
    (dotimes (i 3)
      (dotimes (j 4)        
        (let ((txt (nth (+ j (* i 4)) letters)))
          (when (= i row)
            (push txt answers))
          (add-text-to-exp-window window txt :x (+ 75 (* j 50)) :y (+ 100 (* i 50))))))
    
    ; Tell the model to interact with that window
    
    (install-device window)
    
    ; Set the freq variable based on which row is the target
    
    (case row 
      (0 
       (setf freq 2000))
      (1 
       (setf freq 1000))
      (2 
       (setf freq 500)))
    
    ; Create a tone with frequency freq for .5 seconds
    ; starting at the indicated onset-time 
    
    (new-tone-sound freq .5 onset-time)
    
    ; To simulate the persistent visual memory for the model
    ; we will not clear the display until after a randomly
    ; chosen time between .9 and 1.1 seconds has passed.
    ; This is done by scheduling the clear-exp-window command
    ; to be called after that amount of time has passed.
    
    (schedule-event-relative (+ 900 (act-r-random 200)) "clear-exp-window" :params (list window) :time-in-ms t)
    
    ; clear the response variable 
    
    (setf *responses* nil)
    
    ; Add a command for our respond-to-key-press function so that
    ; ACT-R can call it.
    
    (add-act-r-command "sperling-response" 'respond-to-key-press 
                       "Sperling task key press response monitor")
    
    ; Monitor the output-key action so that our respond-to-key-press
    ; function is called when a key is pressed.
    
    (monitor-act-r-command "output-key" "sperling-response")
    
    ; Run the model for up to 30 seconds in real time mode.
    
    (run 30 t)
    
    ; Stop monitoring the output-key action and remove our command.
    
    (remove-act-r-command-monitor "output-key" "sperling-response")
    (remove-act-r-command "sperling-response")
    
    ; If the *show-responses* variable is non-nil then print out
    ; the correct answers and the responses that were provided
    
    (when *show-responses*
      (format t "~%~%answers: ~S~%responses: ~S~%" answers *responses*))
    
    ; Call the compute-score function to determine the number of
    ; correct responses and return the result.
    
    (compute-score answers)))

; The compute-score function counts how many of the correct answers
; were provided by the participant and returns that number.

(defun compute-score (answers)
  (let ((score 0))
    (dolist (x answers score)
      (when (member x *responses* :test 'string-equal)
        (incf score)))))
    

; This function is the one that will be called when the participant
; presses a key, and it just records the result in the *responses*
; list unless it is the space bar.

(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (unless (string-equal key "space")
    (push key *responses*)))

; The report-data function takes a list of the average number of items
; reported in the target row ordered by onset delay. It compares those
; to the original experiment's data based on correlation and mean deviation
; then calls print-results to display the data.

(defun report-data (data)
  
  (correlation data *sperling-exp-data*)
  (mean-deviation data *sperling-exp-data*)
  (print-results data))

; The print-results function takes a list of the average target row data
; and prints that in a table along with the original experiment's data.

(defun print-results (data)
  
  (format t "~%Condition    Current Participant   Original Experiment~%")
  (do ((condition '(0.00 0.15 0.30 1.00) (cdr condition))
       (temp1 data (cdr temp1))
       (temp2 *sperling-exp-data* (cdr temp2)))
      ((null temp1))
    (format t " ~4,2F sec.          ~6,2F                ~6,2F~%" 
              (car condition) (car temp1) (car temp2))))

; The one-block function runs a trial of the experiment at each
; of the experiment's tone onset conditions in a random order.
; It returns a list of the correct answer counts in the order
; of onset duration (lowest first).

(defun one-block ()
  (let ((result nil))
    
    (dolist (x (permute-list '(0.0 .15 .30 1.0)))
      (push (cons x (sperling-trial x)) result))
    (mapcar 'cdr (sort result '< :key 'car))))

; The sperling-experiment function takes one required parameter which
; is the number of blocks to run in the experiment (where each block
; is one trial at each of the 4 possible onset times).  It collects
; the data over blocks, averages the results, and passes that to the
; report-data function for display.

(defun sperling-experiment (n)
  (let ((results (list 0 0 0 0)))
    (dotimes (i n)
      (setf results (mapcar '+ results (one-block))))
    (report-data (mapcar (lambda (x) (/ x n)) results))))

