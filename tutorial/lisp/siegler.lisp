; ACT-R tutorial unit 5 siegler task.
; This experiment presents a model with a pair of numbers aurally
; and the model must respond vocally with the sum of those numbers.
; The task and data to which the model is fit are in the paper:
;
; Siegler, R. S., & Shrager, J. (1984). Strategy choices in addition 
; and subtraction: How do children know what to do? In C. Sophian (Ed.), 
; Origins of cognitive skills (pp. 229-293). Hillsdale, NJ: Erlbaum.
;
; The original experiment was performed with 4 year-olds who made
; many errors in their responses.

; Load the corresponding model for the task.

(load-act-r-model "ACT-R:tutorial;unit5;siegler-model.lisp")

; Create variables for the response, to record whether the 
; monitoring function is currently available, and a subset of
; the original data for comparison.

(defvar *response*)
(defvar *monitor-installed* nil)

(defvar *siegler-data* '((0   .05 .86  0  .02  0  .02  0   0  .06)
                         (0   .04 .07 .75 .04  0  .02  0   0  .09)
                         (0   .02  0  .10 .75 .05 .01 .03  0  .06)
                         (.02  0  .04 .05 .80 .04  0  .05  0   0)
                         (0    0  .07 .09 .25 .45 .08 .01 .01 .06)
                         (.04  0   0  .05 .21 .09 .48  0  .02 .11)))

; record-model-speech will be monitoring the output-speech
; command called by the microphone device so that it can
; record the model's speech output.

(defun record-model-speech (model string)
  (declare (ignore model))
  
  (setf *response* string))

; Because the task can be run as a single trial, or over
; larger blocks it's more efficient to only install and
; remove the monitor once for the run instead of on each
; trial as has been done in other tasks.  These functions
; are used to do that when necessary.

(defun add-speech-monitor ()
  (unless *monitor-installed*
    (add-act-r-command "siegler-response" 'record-model-speech "Siegler task model response")
    (monitor-act-r-command "output-speech" "siegler-response")
    (setf *monitor-installed* t)))

(defun remove-speech-monitor ()
  (remove-act-r-command-monitor "output-speech" "siegler-response")
  (remove-act-r-command "siegler-response")
  (setf *monitor-installed* nil))

; siegler-trial takes two parameters which must be numbers
; It resets the model and adds a microphone device to record
; the models speech output.  Then, the numbers are presented
; aurally to the model using new-digit-sound, and after 
; running the model it returns any vocal response that it made.

(defun siegler-trial (arg1 arg2)
  (reset)  
  (install-device (list "speech" "microphone"))
  (let ((need-to-remove (add-speech-monitor)))
    (new-digit-sound arg1)
    (new-digit-sound arg2 .75)
    (setf *response* nil)
    (run 30)
    (when need-to-remove 
      (remove-speech-monitor))
    
  *response*))

; siegler-set runs one trial for each of the addition
; problems in the data set and returns the results of
; those trials.

(defun siegler-set ()
  
  (let ((need-to-remove (add-speech-monitor))
        (data (list (siegler-trial 1 1)
                    (siegler-trial 1 2)
                    (siegler-trial 1 3)
                    (siegler-trial 2 2)
                    (siegler-trial 2 3)
                    (siegler-trial 3 3))))
    (when need-to-remove 
      (remove-speech-monitor))
    data))

; siegler-experiment requires one parameter which is how many
; sets of trials to run.  It runs that many trials collecting 
; the responses and then passes those to analyze to compute the 
; response percentages, compare the data to the experimental data,
; and display the results.

(defun siegler-experiment (n)
  (add-speech-monitor)
  (let ((data nil))
    (dotimes (i n)
      (push (siegler-set) data))
    (remove-speech-monitor)
    (analyze data)))

(defun analyze (responses)
  (display-results 
   (mapcar (lambda (x) 
             (mapcar (lambda (y) 
                       (/ y (length responses))) x))
     (apply 'mapcar 
            (lambda (&rest z) 
              (let ((res nil))
                (dolist (i '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight"))
                  (push (count i z :test 'string-equal) res)
                  (setf z (remove i z :test 'string-equal)))
                (push (length z) res)
                (reverse res)))
            responses))))

(defun display-results (results)
  (let ((questions '("1+1" "1+2" "1+3" "2+2" "2+3" "3+3")))
    (correlation results *siegler-data*)
    (mean-deviation results *siegler-data*)
    (format t "       0     1     2     3     4     5     6     7     8   Other~%")
    (dotimes (i 6)
      (format t "~a~{~6,2f~}~%" (nth i questions) (nth i results)))))
       
