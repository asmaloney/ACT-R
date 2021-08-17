
(load-act-r-model "ACT-R:tutorial;unit3;sperling-model.lisp")

(defvar *responses* nil)
(defparameter *show-responses* t)

(defvar *sperling-exp-data* '(3.03 2.40 2.03 1.50))

(defun sperling-trial (onset-time)
  
  (reset)
  
  (let* ((letters (permute-list '("B" "C" "D" "F" "G" "H" "J" 
                                  "K" "L" "M" "N" "P" "Q" "R" 
                                  "S" "T" "V" "W" "X" "Y" "Z")))
         (answers nil)   
         (row (act-r-random 3))
         (window (open-exp-window "Sperling Experiment" :visible t))
         freq)
    
    (dotimes (i 3)
      (dotimes (j 4)        
        (let ((txt (nth (+ j (* i 4)) letters)))
          (when (= i row)
            (push txt answers))
          (add-text-to-exp-window window txt :x (+ 75 (* j 50)) :y (+ 100 (* i 50))))))
 
    (install-device window)
    
    (case row 
      (0 
       (setf freq 2000))
      (1 
       (setf freq 1000))
      (2 
       (setf freq 500)))
    
    (new-tone-sound freq .5 onset-time)
    (schedule-event-relative (+ 900 (act-r-random 200)) "clear-exp-window" :params (list window) :time-in-ms t)
    
    (setf *responses* nil)
    
    (add-act-r-command "sperling-response" 'respond-to-key-press 
                       "Sperling task key press response monitor")
    (monitor-act-r-command "output-key" "sperling-response")
   
    (run 30 t)
    
    (remove-act-r-command-monitor "output-key" "sperling-response")
    (remove-act-r-command "sperling-response")
    
    (when *show-responses*
      (format t "~%~%answers: ~S~%responses: ~S~%" answers *responses*))
    
    (compute-score answers)))


(defun compute-score (answers)
  (let ((score 0))
    (dolist (x answers score)
      (when (member x *responses* :test 'string-equal)
        (incf score)))))
    

(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (unless (string-equal key "space")
    (push key *responses*)))


(defun report-data (data)
  
  (correlation data *sperling-exp-data*)
  (mean-deviation data *sperling-exp-data*)
  (print-results data))

(defun print-results (data)
  
  (format t "~%Condition    Current Participant   Original Experiment~%")
  (do ((condition '(0.00 0.15 0.30 1.00) (cdr condition))
       (temp1 data (cdr temp1))
       (temp2 *sperling-exp-data* (cdr temp2)))
      ((null temp1))
    (format t " ~4,2F sec.          ~6,2F                ~6,2F~%" 
              (car condition) (car temp1) (car temp2))))


(defun one-block ()
  (let ((result nil))
    
    (dolist (x (permute-list '(0.0 .15 .30 1.0)))
      (push (cons x (sperling-trial x)) result))
    (mapcar 'cdr (sort result '< :key 'car))))


(defun sperling-experiment (n)
  (let ((results (list 0 0 0 0)))
    (dotimes (i n)
      (setf results (mapcar '+ results (one-block))))
    (report-data (mapcar (lambda (x) (/ x n)) results))))

