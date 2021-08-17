
(load-act-r-model "ACT-R:tutorial;unit6;bst-model.lisp")

(defvar *target*)
(defvar *current-stick*)
(defvar *current-line*)
(defvar *done*)
(defvar *choice*)
(defvar *window* nil)
(defvar *visible* nil)


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

(defun build-display (a b c goal)
  
  (setf *target* goal)
  (setf *current-stick* 0)
  (setf *done* nil)
  (setf *choice* nil)
  (setf *current-line* nil)
  (setf *window* (open-exp-window "Building Sticks Task" :visible *visible* :width 600 :height 400))
  
  (add-button-to-exp-window *window* :text "A" :x 5 :y 23 :action (list "bst-button-pressed" a "under") :height 24 :width 40)
  (add-button-to-exp-window *window* :text "B" :x 5 :y 48 :action (list "bst-button-pressed" b "over") :height 24 :width 40)
  (add-button-to-exp-window *window* :text "C" :x 5 :y 73 :action (list "bst-button-pressed" c "under") :height 24 :width 40)
  (add-button-to-exp-window *window* :text "Reset" :x 5 :y 123 :action "bst-reset-button-pressed" :height 24 :width 65)
  
  (add-line-to-exp-window *window* (list 75 35)  (list (+ a 75) 35) 'black)
  (add-line-to-exp-window *window* (list 75 60)  (list (+ b 75) 60) 'black)
  (add-line-to-exp-window *window* (list 75 85)  (list (+ c 75) 85) 'black)
  (add-line-to-exp-window *window* (list 75 110) (list (+ goal 75) 110) 'green))

(defun button-pressed (len dir)
  (unless *choice* 
    (setf *choice* dir))
  
  (unless *done*
    (if (> *current-stick* *target*)
        (decf *current-stick* len)
      (incf *current-stick* len))
    (update-current-line)))


(defun reset-display ()
  (unless *done*
    (setf *current-stick* 0)
    (update-current-line)))


(add-act-r-command "bst-button-pressed" 'button-pressed "Choice button action for the Building Sticks Task.  Do not call directly")
(add-act-r-command "bst-reset-button-pressed" 'reset-display "Reset button action for the Building Sticks Task.  Do not call directly")


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

(defun do-experiment (sticks &optional human)
  (apply 'build-display sticks)
  (if human
      (when (visible-virtuals-available?)
        (wait-for-human))
    (progn
      (install-device *window*)
      (start-hand-at-mouse)
      (run 60 *visible*))))


(defun wait-for-human ()
  (while (not *done*)
    (process-events))
  ;; wait for 1 second to pass after done
  (let ((start-time (get-time nil)))
    (while (< (- (get-time nil) start-time) 1000)
      (process-events))))

(defun bst-set (human visible stims &optional (learn t))
  (setf *visible* visible)
  (let ((result nil))
    (dolist (stim stims)
      (when (and (not human) (not learn))
        (reset))
      (do-experiment stim human)
      (push *choice* result))
    (reverse result)))

(defun bst-test (n &optional human)
  (let ((stims *no-learn-stims*))
    
    (let ((result (make-list (length stims) :initial-element 0)))
      (dotimes (i n result)
        (setf result (mapcar '+ 
                       result 
                       (mapcar (lambda (x) 
                                 (if (string-equal x "over") 1 0))
                         (bst-set human (or human (= n 1)) stims nil))))))))

(defun bst-experiment (n &optional human)
  (let ((stims *exp-stims*))
    
    (let ((result (make-list (length stims) :initial-element 0))
          (p-values (list '(decide-over 0) '(decide-under 0) '(force-over 0) '(force-under 0))))
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

(defun production-u-value (prod)
   (caar (spp-fct (list prod :u))))
