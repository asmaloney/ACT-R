
(load-act-r-model "ACT-R:tutorial;unit3;subitize-model.lisp")

(defvar *response* nil)
(defvar *response-time* nil)

(defvar *subitize-exp-data* '(.6 .65 .7 .86 1.12 1.5 1.79 2.13 2.15 2.58))

(defun number-to-word (n)
  (format nil "~r" n))

(defun number-to-string (n)
  (if (= n 10)
      "0"
    (princ-to-string n)))

(defun subitize-trial (n &optional human)
  
  (reset)
  
  (let ((points (generate-points n))
        (window (open-exp-window "Subitizing Experiment"))
        (start (get-time (if human nil t)))
        answer)
    
    (dolist (point points) 
      (add-text-to-exp-window window "x" :x (first point) :y (second point)))
    
    (setf *response* nil)
    (setf *response-time* nil)
    
    (if human
        (when (visible-virtuals-available?)
          (add-act-r-command "subitize-response" 'respond-to-key-press 
                             "Subitize task human response")
          (monitor-act-r-command "output-key" "subitize-response")
          
          (setf answer (number-to-string n))
          (while (null *response*)
            (process-events))
          
          (remove-act-r-command-monitor "output-key" "subitize-response")
          (remove-act-r-command "subitize-response"))
      (progn
        (add-act-r-command "subitize-response" 'record-model-speech 
                           "Subitize task model response")
        (monitor-act-r-command "output-speech" "subitize-response")
        
        (setf answer (number-to-word n))
        (install-device window)
        
        (run 30 t)
        
        (remove-act-r-command-monitor "output-speech" "subitize-response")
        (remove-act-r-command "subitize-response")))
    
    (if (and *response* (string-equal answer *response*))
        (list (/ (- *response-time* start) 1000.0) t)
      (list 30 nil))))

(defun subitize-experiment (&optional human)
  (let (results)
    (dolist (items (permute-list '(10 9 8 7 6 5 4 3 2 1)))
      (push (list items (subitize-trial items human)) results))
    
    (setf results (sort results '< :key 'car))
    (report-data (mapcar 'second results))))

(defun report-data (data)
  (let ((rts (mapcar 'first data)))
    (correlation rts *subitize-exp-data*)
    (mean-deviation rts *subitize-exp-data*)
    (print-results data)))

(defun print-results (data)
  (format t "Items    Current Participant   Original Experiment~%")
  (dotimes (i (length data))
    (format t "~3d        ~5,2f  (~3s)              ~5,2f~%" 
      (1+ i) (car (nth i data)) (second (nth i data))
      (nth i *subitize-exp-data*))))
  
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

(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (setf *response-time* (get-time nil))
  (setf *response* key))

(defun record-model-speech (model string)
  (declare (ignore model))
  (setf *response-time* (get-time t))
  (setf *response* string))

