; ACT-R tutorial unit3 subitize experiment.
; This experiment displays a number of Xs on
; the screen and the participant must respond 
; with how many are there. A human participant
; must press a key from 0-9 (where 0 represents 
; 10 items) but a model must speak the count 
; (which is how the original experiment was
; performed).  The time of the response and its
; correctness are recorded.

; Load the corresponding ACT-R starting model.

(load-act-r-model "ACT-R:tutorial;unit3;subitize-model.lisp")

; Define some global variables to hold the response
; given and the time it occurred.

(defvar *response* nil)
(defvar *response-time* nil)

; A variable holding the data from the original experiment

(defvar *subitize-exp-data* '(.6 .65 .7 .86 1.12 1.5 1.79 2.13 2.15 2.58))

; Two functions for converting an integer to a string 
; as either the word e.g. "one" or the digits e.g. "1"
; for comparison to the response given by a model (spoken)
; or a person (keypress).

(defun number-to-word (n)
  (format nil "~r" n))

(defun number-to-string (n)
  (if (= n 10)
      "0"
    (princ-to-string n)))

; The subitize-trial function presents one trial of the task.
; It requires one parameter which is the number of items to
; present (should be an integer from 1-10), and has an optional
; parameter which indicates whether it is the model or a person
; performing the task.  It returns a list with the time it took
; the participant to respond and t if the response was correct 
; or the list (30 nil) if the response was incorrect or no 
; response was provided.

(defun subitize-trial (n &optional human)
  
  ; Reset ACT-R and all models to initial state
  
  (reset)
  
  ; create some local variables to perform the trial:
  ;  points: is a list of randomized x,y coordinates to display the Xs
  ;          from the generate-points function
  ;  window: an experiment window created to display the task
  ;  start: the current time at the start of the trial 
  ;         as given by the ACT-R get-time function which
  ;         needs to be provided whether it is the model or
  ;         a person doing the task to get the appropriate time
  ;  answer: a variable to hold the correct answer for the trial
  
  (let ((points (generate-points n))
        (window (open-exp-window "Subitizing Experiment"))
        (start (get-time (if human nil t)))
        answer)
    
    ; Display an x at each of the points
    
    (dolist (point points) 
      (add-text-to-exp-window window "x" :x (first point) :y (second point)))
    
    ; clear the response variables
    
    (setf *response* nil)
    (setf *response-time* nil)
    
    ; Run the trial
    
    (if human
        (when (visible-virtuals-available?)
          
          ; If a human is doing the task and there is a visible
          ; window available for them to interact with then
          ; add a command and monitor the output-key action
          
          (add-act-r-command "subitize-response" 'respond-to-key-press 
                             "Subitize task human response")
          (monitor-act-r-command "output-key" "subitize-response")
          
          ; Set the correct answer string for a key press
          
          (setf answer (number-to-string n))
          
          ; Wait until there is a response
          
          (while (null *response*)
            (process-events))
          
          ; Stop monitoring output-key and remove the command
          
          (remove-act-r-command-monitor "output-key" "subitize-response")
          (remove-act-r-command "subitize-response"))
      (progn
        
        ; If a model is doing the task add a command and monitor
        ; the output-speech action
        
        (add-act-r-command "subitize-response" 'record-model-speech 
                           "Subitize task model response")
        (monitor-act-r-command "output-speech" "subitize-response")
        
        ; Set the correct answer string for a spoken response
        
        (setf answer (number-to-word n))
        
        ; Tell the model to interact with the created window
        
        (install-device window)
        
        ; Run the model for up to 30 seconds in real time mode
        
        (run 30 t)
        
        ; Stop monitoring output-speech and remove the command
        
        (remove-act-r-command-monitor "output-speech" "subitize-response")
        (remove-act-r-command "subitize-response")))
    
    ; If a response is given and it matches the correct answer
    ; then return a list with the time since the trial started
    ; in seconds and t, otherwise return a list of 30 and nil
    
    (if (and *response* (string-equal answer *response*))
        (list (/ (- *response-time* start) 1000.0) t)
      (list 30 nil))))

; Subitize-experiment takes one optional parameter which indicates
; whether it is a human or model performing the task.  Then it 
; presents a trial for each of the counts from 1-10 in a randomized
; order, and passes the results to report-data sorted by item count.

(defun subitize-experiment (&optional human)
  (let (results)
    (dolist (items (permute-list '(10 9 8 7 6 5 4 3 2 1)))
      (push (list items (subitize-trial items human)) results))
    
    (setf results (sort results '< :key 'car))
    (report-data (mapcar 'second results))))

; Report-data compares the times in the provided data to
; the original experiment data and then passes it to print-results
; for output.

(defun report-data (data)
  (let ((rts (mapcar 'first data)))
    (correlation rts *subitize-exp-data*)
    (mean-deviation rts *subitize-exp-data*)
    (print-results data)))

; Print-results outputs a table with the times and correctness
; values from the current experiment along with the data from
; the origial experiment.

(defun print-results (data)
  (format t "Items    Current Participant   Original Experiment~%")
  (dotimes (i (length data))
    (format t "~3d        ~5,2f  (~3s)              ~5,2f~%" 
      (1+ i) (car (nth i data)) (second (nth i data))
      (nth i *subitize-exp-data*))))

; The next three functions: generate-points, new-distinct-point, and
; too-close are used to generate a list of random x,y lists for the
; coordinates to display the Xs so that they are within the bounds of
; the window and non-overlapping.

(defun generate-points (n)
   (let ((points nil))
    (dotimes (i n points)
      (push (new-distinct-point points) points))))

(defun new-distinct-point (points)
  (do ((new-point (list (+ (act-r-random 240) 20) (+ (act-r-random 240) 20))
                  (list (+ (act-r-random 240) 20) (+ (act-r-random 240) 20))))
      ((not (too-close new-point points)) new-point)))

(defun too-close (new-point points)
  (some (lambda (a) (and (< (abs (- (car new-point) (car a))) 40)
                         (< (abs (- (cadr new-point) (cadr a))) 40))) 
        points))                 


; Respond-to-key-press is monitoring output-key to record 
; the current time and key pressed when a human is performing
; the task.

(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (setf *response-time* (get-time nil))
  (setf *response* key))


; Record-model-speech is monitoring output-speech to record 
; the current time and word spoken when a model is performing
; the task.

(defun record-model-speech (model string)
  (declare (ignore model))
  (setf *response-time* (get-time t))
  (setf *response* string))

