
(load-act-r-model "ACT-R:tutorial;unit7;past-tense-model.lisp")

(defvar *report*)
(defvar *total-count*)
(defvar *word-list* nil)

(defparameter *verbs* '((have    I    12458 had)
                        (do      I     4367 did)
                        (make    I     2312 made)
                        (get     I     1486 got)
                        (use     R     1016 use)
                        (look    R      910 look)
                        (seem    R      831 seem)
                        (tell    I      759 told)
                        (show    R      640 show)
                        (want    R      631 want)
                        (call    R      627 call)
                        (ask     R      612 ask)
                        (turn    R      566 turn)
                        (follow  R      540 follow)
                        (work    R      496 work)
                        (live    R      472 live)
                        (try     R      472 try)
                        (stand   I      468 stood)
                        (move    R      447 move)
                        (need    R      413 need)
                        (start   R      386 start)
                        (lose    I      274 lost)))


(defun make-word-freq-list (l)
  (let ((data nil)
        (count 0))
    (dolist (verb l)
      (incf count (third verb))
      (push-last (list count (first verb) (fourth verb) 
                       (if (eq (second verb) 'i) 'blank 'ed))
            data))
    (setf *total-count* count)
    data))

(defun random-word ()
  (let ((num (act-r-random *total-count*)))
    (cdr (find-if (lambda (x) (< num (first x))) *word-list*))))

(defun make-one-goal ()
  (let ((word (random-word)))
    (set-buffer-chunk 'imaginal 
                      (car (define-chunks-fct 
                               (list (list 'verb (first word))))))
    (goal-focus starting-goal)
    word))

(defun add-past-tense-to-memory ()
  (let ((word (random-word)))
    (set-buffer-chunk 'imaginal 
                      (car (define-chunks-fct 
                               (list (mapcan (lambda (x y) (list x y))
                                       '(verb stem suffix) word)))))
    (clear-buffer 'imaginal)))

(defun print-header ()
  (format t "~%trials      Irregular       Regular    No inflection  Inflected correctly~%"))

(defun past-tense-results (&optional (graph t))
  (print-header)
  (let ((data (rep-f-i 0 (length *report*) 1000)))
    (when (and graph (> (length data) 1))
      (graph-it data))
  data))

(defun graph-it (data)
  (let* ((win (open-exp-window "Irregular Verbs correct" :visible t :width 500 :height 475))
         (low (apply 'min data))
         (zoom (min .9 (/ (floor low .1) 10))))
    
    (clear-exp-window win)
    (add-text-to-exp-window win "1.0" :x 5 :y 5 :width 22)
    (add-text-to-exp-window win (format nil "~0,2f" zoom) :x 5 :y 400 :width 22)
    (add-text-to-exp-window win (format nil "~0,2f" (+ zoom (/ (- 1.0 zoom) 2))) :x 5 :y 200 :width 22)
    
    (add-text-to-exp-window win "Trials" :x 200 :y 420 :width 100)
    (add-line-to-exp-window win '(30 10) '(30 410) 'black)
    (add-line-to-exp-window win '(450 410) '(25 410) 'black)
    
    (dotimes (i 10)
      (add-line-to-exp-window win (list 25 (+ (* i 40) 10)) (list 35 (+ (* i 40) 10)) 'black))
    
    (do* ((increment (max 1.0 (floor (/ 450.0 (length data)))))
          (range (floor 400 (- 1.0 zoom)))
          (intercept (+ range 10))
          (p1 (butlast data) (cdr p1))
          (p2 (cdr data) (cdr p2))
          (last-x 30 this-x)
          (last-y (- intercept (floor (* range (car p1))))
                  (- intercept (floor (* range (car p1)))))
          (this-x (+ last-x increment)
                  (+ last-x increment))
          (this-y (- intercept (floor (* range (car p2))))
                  (- intercept (floor (* range (car p2))))))
         ((null (cdr p1)) (add-line-to-exp-window win
                           (list last-x last-y) (list this-x this-y) 'red))
      (add-line-to-exp-window win (list last-x last-y) 
                              (list this-x this-y) 
                              'red))))


(defun safe-div (n d)
  (if (zerop d)
      0
    (/ n d)))

(defun rep-f-i (start end count)
  (let ((data nil))
    (dotimes (i (ceiling (- end start) count))
      (let ((irreg 0)
            (reg 0)
            (none 0)
            (e (if (> end (+ start count (* i count)))
                   (+ start count (* i count))
                 end)))
        
        (dolist (x (subseq *report* (+ start (* i count)) e))
          (when (first x)
            (case (second x)
              (reg
               (incf reg))
              (irreg 
               (incf irreg))
              (none
               (incf none)))))
         
        (let* ((total (+ irreg reg none))
               (correct (safe-div irreg (+ irreg reg))))
          (format t "~6d ~{~13,3F~^ ~}~%" e 
            (list
             (safe-div irreg total) 
             (safe-div reg total) 
             (safe-div none total) 
             correct))
          (push-last correct data))))
    data))

(defun add-to-report (target chunk)
  (let ((stem (chunk-slot-value-fct chunk 'stem))
        (word (chunk-slot-value-fct chunk 'verb))
        (suffix (chunk-slot-value-fct chunk 'suffix))
        (irreg (eq (third target) 'blank)))
    
    (if (eq (first target) word)
        (cond ((and (eq stem word) (eq suffix 'ed))
               (push-last (list irreg 'reg) *report*))
              ((and (null suffix) (null stem))
               (push-last (list irreg 'none) *report*))
              ((and (eq stem (second target)) (eq suffix 'blank))
               (push-last (list irreg 'irreg) *report*))
              (t
               (print-warning 
                (format nil "Incorrectly formed verb.  Presented ~s and produced ~{~s~^ ~}." 
                  (first target) 
                  (mapcan (lambda (x y) (list x y))
                    '(verb stem suffix) (list word stem suffix))))
               (push-last (list irreg 'error) *report*)))
      (progn
        (print-warning 
         (format nil "Incorrectly formed verb.  Presented ~s and produced ~{~s~^ ~}." 
           (first target) 
           (mapcan (lambda (x y) (list x y))
             '(verb stem suffix) (list word stem suffix))))
        (push-last (list irreg 'error) *report*)))))


(defvar *reward-check*)

(defun verify-reward (&rest r)
  (declare (ignore r))
  (setf *reward-check* t))

(defun past-tense-trials (n &optional (cont nil)(v nil))
  
  (add-act-r-command "reward-check" 'verify-reward 
                     "Past tense code check for a reward each trial.")
  (monitor-act-r-command "trigger-reward" "reward-check")
  
  (when (or (null *word-list*) (null cont))
    (reset)
    (setf *word-list* (make-word-freq-list *verbs*))
    (let ((new nil))
      (dolist (x *word-list*)
        (mapcar (lambda (y) (pushnew y new)) (cdr x)))
      
      (dolist (x new)
        (unless (chunk-p-fct x)
          (define-chunks-fct (list (list x))))))
      
    (print-header)
    (setf *report* nil))
  
  (sgp-fct (list :v v))
  
  (let* ((start (* 100 (floor (length *report*) 100)))
         (count (mod (length *report*) 100)))
    (dotimes (i n)
      (add-past-tense-to-memory)
      (add-past-tense-to-memory)
      (setf *reward-check* nil)
      (let ((target (make-one-goal))
            (duration (run 100)))
        (add-to-report target (buffer-read 'imaginal))
        (clear-buffer 'imaginal)
        (incf count)
        (when (= count 100)
          (rep-f-i start (+ start 100) 100)
          (setf count 0)
          (incf start 100))   
        (unless *reward-check*
          (print-warning 
           "Model did not receive a reward when given ~s."
           (first target)))
        (run-full-time (- 200 duration))
        
        (when (= duration 100)
          (print-warning 
           "Model spent 100 seconds generating a past tense for ~s."
           (first target)))))
    (rep-f-i start (+ start count) 100))
  
  (remove-act-r-command-monitor "trigger-reward" "reward-check")
  (remove-act-r-command "reward-check")
  nil)

