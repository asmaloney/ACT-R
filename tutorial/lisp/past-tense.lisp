; ACT-R tutorial unit 7 past-tense task.
; This task presents the model with an English
; verb (taken from a very small sample of verbs)
; randomly chosen based on the frequency of that
; verb's usage relative to the other verbs in the
; set.  The model is then supposed to generate 
; the past-tense for that verb.  For every verb
; that the model must generate, it receives two
; randomly chosen correctly formed past-tenses
; which are merged into declarative memory.  The
; code reports the percentages of correctly and
; incorrectly formed past-tenses by the model and
; will display a graph of the correctness for the
; irregular verbs which tend to show a U-shape in
; their learning.


; Load the corresponding model for the task

(load-act-r-model "ACT-R:tutorial;unit7;past-tense-model.lisp")

; Global variables to hold the collected data and the verbs
; along with their frequencies.

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


; make-word-freq-list takes one parameter which is a list
; of verb description lists (as created above for the *verbs*
; variable) that contains the verb, whether it has a regular
; or irregular past-tense, it's relative frequency, and the
; stem for its past-tense (either the verb itself for a regular
; or the correct irregular inflection).  It returns a list
; with a list for each verb that has a cumulative frequency
; value and the three components for specifying the chunk
; of the verb's past-tense (the verb, stem, and suffix slot
; values).

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

; random-word returns a list with the slot values for
; a randomly choosen word from those in the *word-list* list
; based on frequency.

(defun random-word ()
  (let ((num (act-r-random *total-count*)))
    (cdr (find-if (lambda (x) (< num (first x))) *word-list*))))

; make-one-goal randomly chooses a verb to present to the
; model in the imaginal buffer and sets the goal to be a
; copy of the starting-goal chunk.  It returns the verb list
; for the chosen verb.

(defun make-one-goal ()
  (let ((word (random-word)))
    (set-buffer-chunk 'imaginal (list 'verb (first word)))
    (goal-focus starting-goal)
    word))

; add-past-tense-to-memory randomly chooses a verb to
; merge into the model's declartive memory.  It does
; the merging by setting the imaginal buffer to a
; chunk which has the appropriate slots set and then
; clearing the buffer.

(defun add-past-tense-to-memory ()
  (set-buffer-chunk 'imaginal  (mapcan (lambda (x y) (list x y))
                                 '(verb stem suffix) (random-word)))
  (clear-buffer 'imaginal))

; print-header prints the column labels for the data displayed.

(defun print-header ()
  (format t "~%trials      Irregular       Regular    No inflection  Inflected correctly~%"))

; past-tense-results prints out the performance of the model
; averaged over blocks of 1000 verbs, and optionally draws a
; graph of the correctness of inflected irregular verbs (by 
; default it draws the graph but providing a value of nil 
; will suppress the graph)

(defun past-tense-results (&optional (graph t))
  (print-header)
  (let ((data (rep-f-i 0 (length *report*) 1000)))
    (when (and graph (> (length data) 1))
      (graph-it data))
  data))

; graph-it requires one parameter which is a list of
; data which it draws in an experiment window scaling
; increments on the x and y axis to fit the data to
; the size of the graph.

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

; safe-div takes two parameters, n and d and returns
; n/d unless d is 0 in which case it returns 0.

(defun safe-div (n d)
  (if (zerop d)
      0
    (/ n d)))

; rep-f-i computes the average performance of the model
; given a range of elements in the data in blocks of size
; count and prints those values.

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

; add-to-report takes two parameters which are the verb
; that was presented to the model and the resulting chunk
; that it had in the imaginal buffer.  It records the
; result based on the type of verb and correctness of
; the response.  It also reports warnings for verbs
; that are formed incorrectly by the model.

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


; define a global variable and a function that will be
; used to monitor the trigger-reward command so that 
; the code can verify whether or not the model receives
; a reward on each trial.

(defvar *reward-check*)

(defun verify-reward (&rest r)
  (declare (ignore r))
  (setf *reward-check* t))

; past-tense-trials is used to run the model through
; the task.  It takes one required parameter which
; is the number of trials to present.  It has two
; optional parameters.  The firt of those indicates
; whether the model should continue doing the task
; or be reset and start over.  The default is to
; start over, but providing a non-nil value will
; cause it to continue.  The second optional parameter
; controls whether the trace is shown or not.  The
; default is to not show the trace, but providing 
; a value of t will show the trace.

(defun past-tense-trials (n &optional (cont nil)(v nil))
  
  ; add a command to monitor trigger-reward 
  
  (add-act-r-command "reward-check" 'verify-reward 
                     "Past tense code check for a reward each trial.")
  (monitor-act-r-command "trigger-reward" "reward-check")
  
  ; if there isn't a word list created yet or the
  ; model is supposed to start over then
  ; reset and create chunks for the verbs on the
  ; list.
  
  (when (or (null *word-list*) (null cont))
    (reset)
    (unless *word-list* 
      (setf *word-list* (make-word-freq-list *verbs*)))
    (let ((new nil))
      (dolist (x *word-list*)
        (mapcar (lambda (y) (pushnew y new)) (cdr x)))
      
      (dolist (x new)
        (unless (chunk-p-fct x)
          (define-chunks-fct (list (list x))))))
      
    (print-header)
    (setf *report* nil))
  
  ; set the :v value as provided
  
  (sgp-fct (list :v v))
  
  ; present the n trials and report the data in
  ; blocks of 100 as it goes.
  
  (let* ((start (* 100 (floor (length *report*) 100)))
         (count (mod (length *report*) 100)))
    (dotimes (i n)
      
      ; Add the two random past-tenses to memory
      (add-past-tense-to-memory)
      (add-past-tense-to-memory)
      
      ; run the model up to 100 seconds to
      ; process a randomly chosen past-tense
      ; and record the data outputting it
      ; if there're 100 items to output
      
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
        
        ; If the model didn't get a reward or 
        ; spent all 100s running warn about that.
        
        (unless *reward-check*
          (print-warning 
           "Model did not receive a reward when given ~s."
           (first target)))
        (when (= duration 100)
          (print-warning 
           "Model spent 100 seconds generating a past tense for ~s."
           (first target)))
        
        ; run the model until 200 seconds have
        ; passed since the start of the trial
        
        (run-full-time (- 200 duration))))
    
    ; if there are any remaining data items 
    ; report them (< 100 after the last block).
    
    (rep-f-i start (+ start count) 100))
  
  ; remove the monitor for trigger-reward 
  
  (remove-act-r-command-monitor "trigger-reward" "reward-check")
  (remove-act-r-command "reward-check")
  nil)

