; ACT-R tutorial unit8 building sticks experiment.
; This experiment displays three posssible sticks
; which can be used to create a given target stick's
; length.  It is an isomorph of Luchins water jug
; problem, and the experiment for the model is the
; one from: 
;
; Lovett, M. C., & Anderson, J. R. (1996).  History of success 
; and current context in problem solving: Combined influences
; on operator selection.  Cognitive Psychology, 31, 168-217.
;
; The task is presented with buttons to pick the sticks
; and a button to reset the current trial.


; Global variables to hold the information about the
; current trial information.

(defvar *target*)
(defvar *current-stick*)
(defvar *current-line*)
(defvar *done*)
(defvar *choice*)
(defvar *window* nil)
(defvar *visible* nil)

; The data from the experiment, the lengths of the sticks
; used in the experiment, and two example problems for 
; demonstration.

(defvar *bst-exp-data* '(20.0 67.0 20.0 47.0 87.0 20.0 80.0 93.0
                         83.0 13.0 29.0 27.0 80.0 73.0 53.0))

(defvar *exp-stims* '((15  250  55  125)(10  155  22  101)
                      (14  200  37  112)(22  200  32  114)
                      (10  243  37  159)(22  175  40  73)
                      (15  250  49  137)(10  179  32  105)
                      (20  213  42  104)(14  237  51  116)
                      (12  149  30  72)
                      (14  237  51  121)(22  200  32  114)
                      (14  200  37  112)(15  250  55  125)))

(defvar *no-learn-stims* '((15  200  41 103)(10  200 29 132)))

; build-display takes the lengths of the sticks for a trial.
; It sets the global variables and draws the initial interface.

(defun build-display (a b c goal)
  
  (setf *target* goal)
  (setf *current-stick* 0)
  (setf *done* nil)
  (setf *choice* nil)
  (setf *current-line* nil)
  (setf *window* (open-exp-window "Building Sticks Task" :visible *visible* :width 600 :height 400))
  
  (add-button-to-exp-window *window* :text "A" :x 5 :y 23 :action (list "bst-ppm-button-pressed" a "under") :height 24 :width 40)
  (add-button-to-exp-window *window* :text "B" :x 5 :y 48 :action (list "bst-ppm-button-pressed" b "over") :height 24 :width 40)
  (add-button-to-exp-window *window* :text "C" :x 5 :y 73 :action (list "bst-ppm-button-pressed" c "under") :height 24 :width 40)
  (add-button-to-exp-window *window* :text "Reset" :x 5 :y 123 :action "bst-ppm-reset-button-pressed" :height 24 :width 65)
  
  (add-line-to-exp-window *window* (list 75 35)  (list (+ a 75) 35) 'black)
  (add-line-to-exp-window *window* (list 75 60)  (list (+ b 75) 60) 'black)
  (add-line-to-exp-window *window* (list 75 85)  (list (+ c 75) 85) 'black)
  (add-line-to-exp-window *window* (list 75 110) (list (+ goal 75) 110) 'green))


; button-pressed will be added as the bst-button-pressed command
; for use as the action of the stick choice buttons.  It takes
; a parameter to indicate the length of the stick and whether
; the stick is associated with under or over shoot as a first
; choice.  

(defun button-pressed (len dir)
  (unless *choice* 
    (setf *choice* dir))
  
  (unless *done*
    (if (> *current-stick* *target*)
        (decf *current-stick* len)
      (incf *current-stick* len))
    (update-current-line)))

; reset-display will be added as the bst-reseet-button-pressed 
; command for use as the action of the reset buttons.  If the
; trial is not over, then it sets the current stick length to 0
; and redraws it.

(defun reset-display ()
  (unless *done*
    (setf *current-stick* 0)
    (update-current-line)))

; Add the commands for those two functions so they can be
; used as button actions.

(add-act-r-command "bst-ppm-button-pressed" 'button-pressed "Choice button action for the Building Sticks Task.  Do not call directly")
(add-act-r-command "bst-ppm-reset-button-pressed" 'reset-display "Reset button action for the Building Sticks Task.  Do not call directly")

; update-current-line compares the length of the current
; stick to the target stick length.  If they match the
; the trial is over, it redraws the current line, and 
; displays the done prompt.  If it is zero it removes the
; line from the display.  If there is a current line then
; it is updated to match the current length, and if there
; is not a current line then one is drawn and saved for
; future modification.

(defun update-current-line ()
  (cond ((= *current-stick* *target*)
         (setf *done* t) 
         (modify-line-for-exp-window *current-line* (list 75 135) (list (+ *target* 75) 135))
         (add-text-to-exp-window *window* "Done" :x 180 :y 200))
        ((zerop *current-stick*)
         (when *current-line*
           (remove-items-from-exp-window *window* *current-line*)
           (setf *current-line* nil)))
        (*current-line*
          (modify-line-for-exp-window *current-line* (list 75 135) (list (+ *current-stick* 75) 135)))
        (t
         (setf *current-line* (add-line-to-exp-window *window* (list 75 135) (list (+ *current-stick* 75) 135) 'blue)))))

; do-experiment takes a required parameter which is
; a list of stick lengths and an optional parameter
; which indicates whether a person is doing the task.
; It draws the initial sticks and then waits for
; a person to complete the task or runs the model
; for up to a minute to do the task.

(defun do-experiment (sticks &optional human)
  (apply 'build-display sticks)
  (if human
      (when (visible-virtuals-available?)
        (wait-for-human))
    (progn
      (install-device *window*)
      (start-hand-at-mouse)
      (run 60 *visible*))))

; wait-for-human takes no parameters. It waits for
; a person to finish the task, and then waits one 
; more second after the done prompt is displayed to
; give the person a chance to read it.

(defun wait-for-human ()
  (while (not *done*)
    (process-events))
  ;; wait for 1 second to pass after done
  (let ((start-time (get-time nil)))
    (while (< (- (get-time nil) start-time) 1000)
      (process-events))))

; bst-set takes three required parameters and one optional
; parameter.  The first parameter indicates whether it 
; is a person or the model performing the task, and the
; second indicates whether it should use a visible or
; virtual window.  The third parameter is a list of 
; stick lengths for the trials to present.  The optional
; parameter indicates whether the model should learn from
; trial to trial or be reset before each new trial.
; It returns a list of strings indicating whether each
; trial presented was started with the over-shoot or
; under-shoot approach.

(defun bst-set (human visible stims &optional (learn t))
  (setf *visible* visible)
  (let ((result nil))
    (dolist (stim stims)
      (when (and (not human) (not learn))
        (reset))
      (do-experiment stim human)
      (push *choice* result))
    (reverse result)))

; bst-test is used to run multiple instances of the 2 demo
; problems.  It takes one required parameter which indicates
; how many times to run that set of two items, and an optional
; parameter to indicate if it should be a person or model
; doing the task.  It returns a list with the counts of the
; times over-shoot was tried on each of the problems.
; When the model runs the task it is not learning, and starts
; each trial as if it were the first time doing the task.
; If the model is running once through the set then it will
; use a visible window to show the interaction, otherwise it
; will use a virtual window.

(defun bst-test (n &optional human)
  (let ((stims *no-learn-stims*))
    
    (let ((result (make-list (length stims) :initial-element 0)))
      (dotimes (i n result)
        (setf result (mapcar '+ 
                       result 
                       (mapcar (lambda (x) 
                                 (if (string-equal x "over") 1 0))
                         (bst-set human (or human (= n 1)) stims nil))))))))

; bst-ppm-experiment is used to run the full experiment multiple
; times and report the results and fit to the experiment data.
; It has a required parameter which indicates how many times
; to run the task, and an optional parameter indicating whether
; it should be a person performing the task.
; It collects the over- or under- shoot choices for each problem
; and computes the proportion of time it's chosen for comparison
; to the original data.  It displays the data and its fit to the
; data from the original experiment along with the average utility
; value over the trials for the productions in the model which make
; the choice of strategy.

(defun bst-ppm-experiment (n &optional human)
  (let ((stims *exp-stims*))
    
    (let ((result (make-list (length stims) :initial-element 0))
          (p-values (list '(decide-over 0) '(decide-under 0))))
      (dotimes (i n result)
        (reset)
        (setf result (mapcar '+ result 
                       (mapcar (lambda (x) 
                                 (if (string-equal x "over") 1 0)) 
                         (bst-set human human stims))))
        (no-output
         (setf p-values (mapcar (lambda (x) 
                                  (list (car x) (+ (second x) (production-u-value (car x)))))
                          p-values))))
      
      (setf result (mapcar (lambda (x) (* 100.0 (/ x n))) result))
      
      (when (= (length result) (length *bst-exp-data*))
        (correlation result *bst-exp-data*)
        (mean-deviation result *bst-exp-data*))
      
      (format t "~%Trial ")
      
      (dotimes (i (length result))
        (format t "~8s" (1+ i)))
      
      (format t "~%  ~{~8,2f~}~%~%" result)
      
      (dolist (x p-values)
        (format t "~12s: ~6,4f~%" (car x) (/ (second x) n))))))

; production-u-value returns the current :u parameter
; value from the indicated production. 

(defun production-u-value (prod)
  (caar (spp-fct (list prod :u))))

; A similarity hook function to return
; similarity values between numbers which
; are expected to be line lengths.

(defun number-sims (a b)
  (when (and (numberp a) (numberp b))
    (/ (abs (- a b)) -300)))

(add-act-r-command "bst-number-sims" 'number-sims "Similarity hook function for building sticks task.")

; The compute-difference function is used as the
; function called by an imaginal-action buffer
; request.  It creates a chunk which is a copy of
; the chunk in the imaginal buffer and adds a
; slot called difference which holds the difference
; between the length of the current line and the
; target line.  That chunk's name is returned
; so that the imaginal module will place it into
; the imaginal buffer.

(defun compute-difference ()
  (let* ((chunk (buffer-read 'imaginal))
         (new-chunk (copy-chunk-fct chunk)))
    (mod-chunk-fct new-chunk (list 'difference (abs (- (chunk-slot-value-fct chunk 'length)
                                                       (chunk-slot-value-fct chunk 'goal-length)))))))

(add-act-r-command "bst-compute-difference" 'compute-difference "Imaginal action function to compute the difference between sticks.")

; Load the corresponding ACT-R starting model.
; Done after adding the bst-compute-difference and bst-number-sims
; commands because they are used in the model and should exist first.

(load-act-r-model "ACT-R:tutorial;unit8;bst-ppm-model.lisp")
