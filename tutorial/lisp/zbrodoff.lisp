
(load-act-r-model "ACT-R:tutorial;unit4;zbrodoff-model.lisp")

(defvar *trials*)
(defvar *results*)

(defvar *zbrodoff-control-data* '(1.84 2.46 2.82 1.21 1.45 1.42 1.14 1.21 1.17))

(defparameter *run-model* t)

(defstruct trial block addend text answer visible correct start time)

(defun construct-trial (block problem &optional visible)
  (destructuring-bind (addend1 addend2 sum answer) problem
    (make-trial :block block
                :addend addend2
                :text (format nil "~a + ~a = ~a" addend1 addend2 sum)
                :answer answer
                :visible visible)))


(defun present-trial (trial &optional (new-window t))
  (if new-window
      (let ((w (open-exp-window "Alpha-arithmetic Experiment" :visible (trial-visible trial))))
        (when *run-model*
          (install-device w)))
    (clear-exp-window))
  
  (add-text-to-exp-window nil (trial-text trial) :x 100 :y 150)
  
  (setf (trial-start trial) (get-time *run-model*)))


(defun respond-to-key-press (model key)
  (declare (ignore model))

  (let ((trial (pop *trials*)))
    (setf (trial-time trial) (/ (- (get-time *run-model*) (trial-start trial)) 1000.0))
    (setf (trial-correct trial) (string-equal (trial-answer trial) key))
    (push trial *results*))
  
  (when *trials*
    (present-trial (first *trials*) nil)))


(defun collect-responses (count)
  (setf *results* nil)
  
  (add-act-r-command "zbrodoff-response" 'respond-to-key-press 
                     "Zbrodoff task key press response monitor")
  (monitor-act-r-command "output-key" "zbrodoff-response")
  
  (present-trial (first *trials*))

  (if *run-model* 
      (run (* 10 count))        
    (if (visible-virtuals-available?)
        (while (< (length *results*) count)
          (process-events))))
  
  (remove-act-r-command-monitor "output-key" "zbrodoff-response")
  (remove-act-r-command "zbrodoff-response"))



(defun zbrodoff-problem (addend1 addend2 sum answer &optional (visible (not *run-model*)))
  
  (setf *trials* (list (construct-trial 1 (list addend1 addend2 sum answer) visible)))
  (collect-responses 1)
  (analyze-results))


(defun zbrodoff-set (&optional (visible (not *run-model*)))
  
  (setf *trials* (create-set 1 visible))
  (collect-responses 24)
  (analyze-results))


(defun zbrodoff-block (&optional (visible (not *run-model*)))
  (setf *trials* nil)
  (dotimes (i 8)
    (setf *trials* (append *trials* (create-set 1 visible))))
  (collect-responses 192)
  (analyze-results))

(defun zbrodoff-experiment (&optional (visible (not *run-model*)) (show t))
  (reset)
  (setf *trials* nil)
  (dotimes (j 3)
    (dotimes (i 8)
      (setf *trials* (append *trials* (create-set (+ j 1) visible)))))
  (collect-responses 576)
  (analyze-results show))

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


  


(defun create-set (block visible)
  (mapcar (lambda (x) 
            (construct-trial block x visible))
    (permute-list *data-set*)))
