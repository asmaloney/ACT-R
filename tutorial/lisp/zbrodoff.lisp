; ACT-R tutorial unit 4 zbrodoff task.
; This experiment presents participants with alpha-arithmetic
; problems like "A + 2 = C" which they must respond to by
; pressing k if the problem is correct or d if it is not.
; The code runs the control condition from the paper:
;
; Zbrodoff, N. J. (1995).  Why is 9 + 7 harder than 2 + 3? 
; Strength and interference as explanations of the problem-size 
; effect.  Memory & Cognition, 23 (6), 689-700.
;
; That condition presents problems with addends of 2, 3, and 4 
; with equal frequency in blocks of 192 trials where half of the
; trials in a block are correct and half are false.  The 
; data for comparison is the average response time by block
; and addend for correct answers (including both true and
; false problems).


; Load the corresponding model for the task

(load-act-r-model "ACT-R:tutorial;unit4;zbrodoff-model.lisp")

; Global variables to hold the trials to present, the results that have
; been collected, and the original data for comparison.

(defvar *trials*)
(defvar *results*)

(defvar *zbrodoff-control-data* '(1.84 2.46 2.82 1.21 1.45 1.42 1.14 1.21 1.17))

; Also create a variable to indicate whether it will be a model or person.
; This is done to keep the number of parameters needed to run the functions
; smaller since one may want a visible window for either a person or model
; and this avoids having to specify both who is running and whether the window
; should be shown.

(defparameter *run-model* t)

; Because the data collection for this task is a little more involved
; we're going to record the trials in a structure to keep everything
; together and organized instead of just a list of items as has been
; done for other experiments.  This will also allow us to store all of
; the information needed to present a trial together so that we can 
; create them all in advance which can be useful when running in an
; event-driven style.  A trial will hold the block number, the addend
; value, the text of the problem to display, the correct answer, whether
; the window should be visible or not, whether the response from the
; participant was correct, the time the trial started, and the response
; time.

(defstruct trial block addend text answer visible correct start time)

; The construct-trial function takes a block number, a list of the items
; which describe the problem to present, and an optional parameter to
; indicate whether or not the window should be visible.  It creates 
; and returns a trial structure containing the appropriate information.

(defun construct-trial (block problem &optional visible)
  (destructuring-bind (addend1 addend2 sum answer) problem
    (make-trial :block block
                :addend addend2
                :text (format nil "~a + ~a = ~a" addend1 addend2 sum)
                :answer answer
                :visible visible)))

; The present-trial function takes one parameter which is a trial
; structure and an optional parameter which indicates whether or not
; to open a new window for this trial (since this task is running 
; continuously it will run faster if it uses the same window repeatedly,
; but because the same code is used to run it for a variety of
; different situations it needs to know when to start over with a
; new display). 

(defun present-trial (trial &optional (new-window t))
  (if new-window
      ; If a new window is requested it opens one using
      ; the visible status indicated in the trial and
      ; if the model is performing the task it installs
      ; that window device for the model.
      
      (let ((w (open-exp-window "Alpha-arithmetic Experiment" :visible (trial-visible trial))))
        (when *run-model*
          (install-device w)))
    
    ; otherwise it just clears the current window
    
    (clear-exp-window))
  
  ; add the text from the trial to the window and set the
  ; start time in the trial structure.
  
  (add-text-to-exp-window nil (trial-text trial) :x 100 :y 150)
  
  (setf (trial-start trial) (get-time *run-model*)))

; The respond-to-key-press function will be set up to monitor
; the output-key actions, and thus will be called with two parameters
; when a key is pressed: the name of the model that pressed the key 
; (or nil if it is a person) and the string naming the key that was
; pressed. 
; Unlike the previous tasks, since this one is event-driven we will
; actually do more than just record the key and time in this function.
; It will also present the next trial if there is one so that the
; model can continue to run in the task until it is complete.

(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  ; Remove the current trial from the list, and then set the response time
  ; and correctness in the structure before adding it to the results list.
  
  (let ((trial (pop *trials*)))
    (setf (trial-time trial) (/ (- (get-time *run-model*) (trial-start trial)) 1000.0))
    (setf (trial-correct trial) (string-equal (trial-answer trial) key))
    (push trial *results*))
  
  ; If there are any trials left to present then present the first of them now.
  
  (when *trials*
    (present-trial (first *trials*) nil)))

; The collect-responses function takes no parameters and runs all of
; the trials available.

(defun collect-responses ()
  
  ; record how many trials need to be run
  
  (let ((total (length *trials*)))
    
    ; Create a command for respond-to-key-press and monitor output-key.
    
    (add-act-r-command "zbrodoff-response" 'respond-to-key-press 
                       "Zbrodoff task key press response monitor")
    (monitor-act-r-command "output-key" "zbrodoff-response")
    
    ; present the first trial
    
    (present-trial (first *trials*))
    
    ; If it's a model doing the task run for 10s per trial,
    ; and if it's a person loop until there are as many results
    ; as there were trials to run.
    
    (if *run-model* 
        (run (* 10 total))        
      (if (visible-virtuals-available?)
          (while (< (length *results*) total)
            (process-events))))
    
    ; stop monitoring and remove the command
    
    (remove-act-r-command-monitor "output-key" "zbrodoff-response")
    (remove-act-r-command "zbrodoff-response")))


; The zbrodoff-problem function takes 4 required parameters.
; The first three are the strings of the elements of the problem
; to present e.g. "A" "2" "C" to preset "A + 2 = C".  The fourth
; is a string with the key press that will be a correct response
; which is "k" if the problem is correct and "d" if the problem
; is not correct.  The optional parameter can be specified as true
; to have the window displayed, but if not provided defaults to
; not showing the window for a model and showing it for a person.
; It clears the current results, creates a list with the single
; trial specified and runs the task for that trial and displays
; the results.
 
(defun zbrodoff-problem (addend1 addend2 sum answer &optional (visible (not *run-model*)))
  
  (setf *results* nil)
  (setf *trials* (list (construct-trial 1 (list addend1 addend2 sum answer) visible)))
  (collect-responses)
  (analyze-results))

; Zbrodoff-set and zbrodoff-block are similar to zbrodoff-problem
; except that instead of presenting a single trial they present
; a full set (24 trials) or block (192 trials) of items.

(defun zbrodoff-set (&optional (visible (not *run-model*)))
  
  (setf *results* nil)
  (setf *trials* (create-set 1 visible))
  (collect-responses)
  (analyze-results))

(defun zbrodoff-block (&optional (visible (not *run-model*)))
  
  (setf *results* nil)
  (dotimes (i 8)
    (setf *trials* (append *trials* (create-set 1 visible))))
  (collect-responses)
  (analyze-results))

; Zbrodoff-experiment has two optioal parameters.  The first is
; whether or not to show the window which defaults to not shown
; if it's a model or shown for a person, and the second is whether
; or not to display the results after the experiment is run (the
; default is to show them).
; It resets the model, generates three blocks of trials, runs
; those trials, and reports the results.

(defun zbrodoff-experiment (&optional (visible (not *run-model*)) (show t))
  (reset)
  (setf *trials* nil)
  (dotimes (j 3)
    (dotimes (i 8)
      (setf *trials* (append *trials* (create-set (+ j 1) visible)))))
  
  (setf *results* nil)
  (collect-responses)
  (analyze-results show))

; Zbrodoff-compare takes one required parameter which is the number
; of times to run a model through the full experiment.  It runs
; the model that many times and averages the results of those
; runs which it compares to the original data for the task and
; then displays the results.

(defun zbrodoff-compare (n)
  (let ((results nil))
    (dotimes (i n)
      (push (zbrodoff-experiment nil nil) results))
    
    (let ((rts (mapcar (lambda (x) (/ x (length results)))
                 (apply 'mapcar '+ (mapcar 'first results))))
          (counts (mapcar (lambda (x) (truncate x (length results)))
                    (apply 'mapcar '+ (mapcar 'second results)))))
      
      (correlation rts *zbrodoff-control-data*)
      (mean-deviation rts *zbrodoff-control-data*)
      
      (print-analysis rts counts '(1 2 3) '("2" "3" "4") '(64 64 64)))))


; Analyze-results takes one optional parameter which 
; indicates whether or not to print the results in addition
; to averaging the times by addend and block and returning
; the averaged results and counts of correct items in a list.

(defun analyze-results (&optional (show t))
  (let* ((blocks (sort (remove-duplicates (mapcar 'trial-block *results*)) '<))
         (addends (sort (remove-duplicates (mapcar 'trial-addend *results*) 
                                           :test 'string-equal) 
                        'string<))
         (counts nil)
         (rts nil)
         (total-counts (mapcar (lambda (x) 
                                 (/ (count x *results* 
                                           :key 'trial-addend 
                                           :test 'string=)
                                    (length blocks)))
                         addends)))
    
    (dolist (x blocks)
      (dolist (y addends)
        (let ((data (mapcar 'trial-time
                      (remove-if-not (lambda (z)
                                       (and (trial-correct z)
                                            (string= y (trial-addend z))
                                            (= x (trial-block z))))
                                     *results*))))
          (push (length data) counts)
          (push (/ (apply '+ data) (max 1 (length data))) rts))))
    
    
    (when show
      (print-analysis (reverse rts) (reverse counts) blocks addends total-counts))
      
    (list (reverse rts) (reverse counts))))

; print-analysis displays a table with the data items provided.

(defun print-analysis (rts counts blocks addends totals)
  (format t "~%        ")
  (dotimes (addend (length addends))
    (format t " ~6@a (~2d)" (nth addend addends) (nth addend totals)))
  (dotimes (block (length blocks))
    (format t "~%Block ~2d" (nth block blocks))
    (dotimes (addend (length addends))
      (format t " ~6,3f (~2d)" (nth (+ addend (* block (length addends))) rts)
        (nth (+ addend (* block (length addends))) counts))))
  (terpri))
        
; This varaible holds the problems to be presented
; in one set of the task -- 4 problems with each addend
; in a correct equation and 4 problems with each addend
; in an incorrect equation.

(defvar *data-set* '(("a" "2" "c" "k")("d" "2" "f" "k")
                     ("b" "3" "e" "k")("e" "3" "h" "k")
                     ("c" "4" "g" "k")("f" "4" "j" "k")
                     ("a" "2" "d" "d")("d" "2" "g" "d")
                     ("b" "3" "f" "d")("e" "3" "i" "d")
                     ("c" "4" "h" "d")("f" "4" "k" "d")
                     ("a" "2" "c" "k")("d" "2" "f" "k")
                     ("b" "3" "e" "k")("e" "3" "h" "k")
                     ("c" "4" "g" "k")("f" "4" "j" "k")
                     ("a" "2" "d" "d")("d" "2" "g" "d")
                     ("b" "3" "f" "d")("e" "3" "i" "d")
                     ("c" "4" "h" "d")("f" "4" "k" "d")))


; Create-set takes a block number and whether the items
; should be visible and returns a randomized list of
; trial structures representing one set of data with
; those conditions.
  
(defun create-set (block visible)
  (mapcar (lambda (x) 
            (construct-trial block x visible))
    (permute-list *data-set*)))
