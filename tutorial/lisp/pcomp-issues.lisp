; ACT-R tutorial unit7 task for investigating production
; compilation modeling issues.

(load-act-r-model "ACT-R:tutorial;unit7;production-compilation-issues-model.lisp")

(defvar *val1*)
(defvar *val2*)
(defvar *responses*)
(defvar *start-time*)
(defvar *times*)
(defvar *exp-length*)
(defvar *task-over*)
(defvar *task-state*)
(defvar *window*)

(defparameter *result-matrix* (make-array '(4 4 4) :initial-contents '((("win" "lose" "draw" "lose") ("win" "lose" "draw" "lose") ("win" "lose" "draw" "lose") ("lose" "draw" "win" "lose"))
                                                                       (("win" "lose" "draw" "lose") ("win" "lose" "draw" "lose") ("lose" "draw" "win" "lose") ("lose" "draw" "win" "lose"))
                                                                       (("win" "lose" "draw" "lose") ("win" "lose" "draw" "lose") ("lose" "draw" "win" "lose") ("lose" "draw" "win" "lose"))
                                                                       (("win" "lose" "draw" "lose") ("lose" "draw" "win" "lose") ("lose" "draw" "win" "lose") ("lose" "draw" "win" "lose")))))

(defun convert-key-to-index (key)
  (cond ((string-equal key "s") 0)
        ((string-equal key "d") 1)
        ((string-equal key "f") 2)
        (t 3)))

(defun respond-to-key-press (model key)
  (declare (ignore model))
  (unless *task-over*
    (if (eq *task-state* 'trial)
       (let* ((response-index (convert-key-to-index key))
             (result (aref *result-matrix* *val1* *val2* response-index)))
        (push result *responses*)
        (push (- (get-time) *start-time*) *times*)
        (present-feedback result)) 
      (if (string-equal key "space")
          (if (= *exp-length* (length *responses*))
              (setf *task-over* t)
            (present-next-trial))))))


(defun present-feedback (result)
  (setf *task-state* 'feedback)
  (clear-exp-window *window*)
  (add-text-to-exp-window *window* result :x 50 :y 100))

(defun present-next-trial ()
  (setf *task-state* 'trial)
  (clear-exp-window *window*)
  (setf *val1* (act-r-random 4))
  (setf *val2* (act-r-random 4))
  (add-text-to-exp-window *window* (princ-to-string *val1*) :x 10 :y 50)
  (add-text-to-exp-window *window* (princ-to-string *val2*) :x 110 :y 50)
  (setf *start-time* (get-time)))

(defun game-over ()
  *task-over*)

(defun pcomp-issues-trials (&optional (n 150) (reset t) (output t))
  (when reset (reset))
  (setf *window* (open-exp-window "Compilation task" :visible nil))
  (setf *times* nil)
  (setf *responses* nil)
  (setf *task-over* nil)
  (setf *exp-length* n)
  (present-next-trial)
  (install-device *window*)
  
  (add-act-r-command "compilation-issues-response" 'respond-to-key-press "Compilation issues key press response monitor")
  (monitor-act-r-command "output-key" "compilation-issues-response")
  
  ; just run the model a long time to let it finish the trial
  (run 20000)
  
  (remove-act-r-command-monitor "output-key" "compilation-issues-response")
  (remove-act-r-command "compilation-issues-response")

  (analyze-results output))

(defun pcomp-issues-game (n &optional show-games)
  (let ((scores (make-list 15 :initial-element 0))
        (times (make-list 15 :initial-element 0)))
    (dotimes (i n)
      (let ((result (pcomp-issues-trials 150 t show-games)))
        (setf scores (mapcar '+ scores (first result)))
        (setf times (mapcar '+ times (second result)))))
    (format t "~%Average Score of ~d trials~%" n)
    (format t "~{~4,2f ~}" (mapcar (lambda (x) (/ x n)) scores))
    (format t "~%Average Response times~%")
    (format t "~{~4,2f ~}" (mapcar (lambda (x) (/ x n 1000.0)) times))))
  
(defun analyze-results (output)
  (let ((data (list nil nil)))
  (format output "Score~%")
  
  (setf *responses* (reverse *responses*))
  (while *responses*
    (let ((sum 0)
          (count 0))
      (dotimes (i (min (length *responses*) 10))
        (let ((result (pop *responses*)))
          (when (string-equal result "win") (incf sum))
          (when (string-equal result "lose") (decf sum)))
        (incf count))
      (format output "~4d" sum)
      (push-last sum (first data))))
  (format output "~%Average response times~%")
  (setf *times* (reverse *times*))
  (while *times*
    (let ((sum 0)
          (count 0))
      (dotimes (i (min (length *times*) 10))
        (incf sum (pop *times*))
        (incf count))
      (format output "~4,2f " (/ sum count 1000.0))
      (push-last (/ sum count) (second data))))
    (format output "~%")
    data))
