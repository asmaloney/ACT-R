; ACT-R tutorial unit 3 code for a simple task to 
; investigate potential perceptual and motor issues
; with models.

(load-act-r-model "ACT-R:tutorial;unit3;perceptual-motor-issues-model.lisp")
 
(defvar *response* nil)
(defvar *response-time* nil)


(defun respond-to-key-press (model key)
  (declare (ignore model))
  (unless *response-time* (setf *response-time* (get-time)))
  (push (string key) *response*)) 

(defun pm-issues-task (&optional which)
  
  (reset)
  
  (let* ((alphabet (list "a" "b" "c" "d" "e" "f" "g" "h" 
                         "i" "j" "k" "l" "m" "n" "o" "p" "q"
                         "r" "s" "t" "u" "v" "w" "x" "y" "z"))
         (letter (first (permute-list alphabet)))
         (task (if (or (string-equal which "next") (string-equal which "previous"))
                   which
                 (if (zerop (act-r-random 2)) "next" "previous")))
         (time (+ 1500 (act-r-random 1000)))
         (window (open-exp-window "Simple task")))
    
    (push "z" alphabet)
    
    (install-device window)
    
    (add-act-r-command "pm-issues-response" 'respond-to-key-press 
                       "Perceptual-motor issues task response")
    (monitor-act-r-command "output-key" "pm-issues-response")
    
    (add-text-to-exp-window window letter :x 130 :y 150)
    
    (add-act-r-command "pm-issue-display" 'display-prompt 
                       "Perceptual-motor issues task prompt display")
        
    (schedule-event-relative time "pm-issue-display" :params (list window task) :time-in-ms t :output 'medium)
    
    (setf *response* nil) 
    (setf *response-time* nil)
        
    (run 10 t)
    
    (remove-act-r-command "pm-issue-display")
    (remove-act-r-command-monitor "output-key" "pm-issues-response")
    (remove-act-r-command "pm-issues-response")

    (list task  (and (= (length *response*) 2)
                     (< time *response-time*)
                     (string-equal (car (last *response*)) letter)
                     (or (and (string-equal task "next") (search (reverse *response*) alphabet :test 'string-equal))
                         (and (string-equal task "previous") (search *response* alphabet :test 'string-equal)))
                     t))))
          
    
(defun display-prompt (window prompt)
  (clear-exp-window)
  (add-text-to-exp-window window prompt :x 125 :y 150))
