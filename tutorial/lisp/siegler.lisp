
(load-act-r-model "ACT-R:tutorial;unit5;siegler-model.lisp")

(defvar *response*)
(defvar *monitor-installed* nil)

(defvar *siegler-data* '((0   .05 .86  0  .02  0  .02  0   0  .06)
                         (0   .04 .07 .75 .04  0  .02  0   0  .09)
                         (0   .02  0  .10 .75 .05 .01 .03  0  .06)
                         (.02  0  .04 .05 .80 .04  0  .05  0   0)
                         (0    0  .07 .09 .25 .45 .08 .01 .01 .06)
                         (.04  0   0  .05 .21 .09 .48  0  .02 .11)))



(defun record-model-speech (model string)
  (declare (ignore model))
  
  (setf *response* string))


(defun add-speech-monitor ()
  (unless *monitor-installed*
    (add-act-r-command "siegler-response" 'record-model-speech "Siegler task model response")
    (monitor-act-r-command "output-speech" "siegler-response")
    (setf *monitor-installed* t)))

(defun remove-speech-monitor ()
  (remove-act-r-command-monitor "output-speech" "siegler-response")
  (remove-act-r-command "siegler-response")
  (setf *monitor-installed* nil))


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
       
