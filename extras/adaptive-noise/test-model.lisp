#|
  This file contains a simple model which is just used to set some 
  initial parameters and have some chunks in DM.  The code then performs
  repeated retrieval requests at specified intervals with additional 
  practices interleaved and records the choice between the two possible
  chunks, the activation of the chunks, and the retrieved chunk's latency.
  Examples of that data are included in the example-data along with a
  spreadsheet showing comparisons between the results with and without 
  this modification.

  The trace in the comment at the bottom of the file indicate 
  specifically how the data for the spreadsheets was generated.

  Partial matching is used to set a difference between the activations
  of the two chunks to achieve a particular initial probability of
  retrieval based on the Boltzmann equation for the normal activation
  calculations.
|#



(clear-all)

(written-for-act-r-version "7.15" "test model for adaptive noise")

(require-extra "adaptive-noise")


(define-model test-retrieval
    (sgp :v nil :esc t :bll .5 :ans .25 :mp 1 :lf 0 :rt -1000)
  )


(defvar *results*)

(defun run-tests (request-period rehearse-period n &key (uan nil) (ol nil) (initial-p .6) (reinforce nil))
  (setf *results* nil)
  (reset)
  
  (sgp-fct (list :uan uan :ol ol))
  
  ;; added after setting :ol to avoid a warning about changing parameters
  
  (add-dm
   (a)
   (b)
   (c1 value a)
   (c2 value b))
  
  ;; set the probability based on the standard activation equation
  ;; using the Boltzmann equation and similarity to set the offset
  ;; value.
  
  (if (and (>= initial-p .5) (< initial-p 1.0))
      (let ((d (* (sqrt 2) .25 (log (/ (- 1 initial-p) initial-p)))))
        (set-similarities-fct (list (list 'a 'b d))))
    (progn
      (print-warning "Initial-p must be .5 <= p < 1.0")
      (return-from run-tests)))
    
  (schedule-periodic-event rehearse-period 'rehearse :maintenance t :initial-delay rehearse-period :priority :min)

  (schedule-periodic-event request-period 'make-request :maintenance t :initial-delay request-period :params (list reinforce))
  
  (run (* n request-period))
  
  *results*)

(defun compute-latency (activation)
  ;; assume F = 1 for simplicity
  (exp (- activation)))

(defun record-result (reinforce)
  (let ((c (awhen (buffer-read 'retrieval) 
                  (chunk-copied-from-fct it))))
    (when c
      (if reinforce
          (clear-buffer 'retrieval)
        (erase-buffer 'retrieval)))
    
    (push-last (list c
                     (chunk-retrieval-activation 'c1)
                     (chunk-retrieval-activation 'c2)
                     (when c
                       (compute-latency (chunk-retrieval-activation c))))
               *results*)))

(defun make-request (reinforce)
  (module-request 'retrieval (define-chunk-spec value a)) ;; The target chunk is c1
  (schedule-event-now 'record-result :priority :min :maintenance t :params (list reinforce)))

(defun rehearse ()
  ;; rehearse both everytime
  (merge-dm (value a) (value b)))

(defun generate-data (n retrieval-count &key (uan nil) (ol nil) (initial-p .6) (reinforce nil))
  (let ((r nil))
    (dotimes (i n)
      ;; assume two rehearsals to each retrieval
      (run-tests 1000 500 retrieval-count :ol ol :uan uan :initial-p initial-p :reinforce reinforce)
      (push (copy-list *results*) r))
    r))



(defun output-choice-data (data pathname)
  (with-open-file (f pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format f "~{~f~^~%~}"
      (mapcar (lambda (x) 
                (/ x (length data)))
        (reduce (lambda (x &optional y) 
                  (mapcar '+ x y)) 
                (mapcar (lambda (x) 
                          (mapcar (lambda (y) 
                                    (if (eq (car y) 'c1) 1 0))
                            x))
                  data))))))
    
    
(defun output-chosen-activation-data (data pathname)
  (with-open-file (f pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format f "~{~f~^~%~}"
      (mapcar (lambda (x) 
                (/ x (length data)))
        (reduce (lambda (x &optional y) 
                  (mapcar '+ x y)) 
                (mapcar (lambda (x) 
                          (mapcar (lambda (y) 
                                    (if (eq (car y) 'c1) (second y) (third y)))
                            x))
                  data))))))

(defun latency-profile (data)
  (let* ((max-time (reduce 'max data))
         (min-time (reduce 'min data))
         (step (/ (- max-time min-time) 100.0))
         (profile (make-list 101 :initial-element 0)))
    (dolist (x data)
      (incf (nth (round (- x min-time) step) profile)))
    (let ((count -1))
      (mapcar (lambda (x) (cons (* (incf count) step) x)) profile))))

(defun output-latency-profile-data (data pathname)
  (with-open-file (f pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
    (let* ((first-results (mapcar (lambda (x) (fourth (first x))) data))
           (last-results (mapcar (lambda (x) (fourth (car (last x)))) data))
           (first-profile (latency-profile first-results))
           (last-profile (latency-profile last-results)))
      (dotimes (i 101)
        (format f "~f,~d,,~f,~d~%" 
          (car (nth i first-profile))
          (cdr (nth i first-profile))
          (car (nth i last-profile))
          (cdr (nth i last-profile)))))))



#| To generate the data files the following runs were performed:

This however will take several minutes to complete, and shorter 
runs can be done for testing purposes.

CG-USER(1): (generate-data 5000 300)
(...)
CG-USER(2): (defvar *data-with-defaults* *)
*DATA-WITH-DEFAULTS*
CG-USER(3): (output-choice-data *data-with-defaults* "ACT-R:extras;adaptive-noise;default-choice.txt")
NIL
CG-USER(4): (output-chosen-activation-data *data-with-defaults* "ACT-R:extras;adaptive-noise;default-activation.txt")
NIL
CG-USER(5): (output-latency-profile-data *data-with-defaults* "ACT-R:extras;adaptive-noise;default-latency.txt")
NIL
CG-USER(6): (generate-data 5000 300 :uan t)
(...)
CG-USER(7): (defvar *data-with-uan* *)
*DATA-WITH-UAN*
CG-USER(8): (output-choice-data *data-with-uan* "ACT-R:extras;adaptive-noise;uan-choice.txt")
NIL
CG-USER(9): (output-chosen-activation-data *data-with-uan* "ACT-R:extras;adaptive-noise;uan-activation.txt")
NIL
CG-USER(10): (output-latency-profile-data *data-with-uan* "ACT-R:extras;adaptive-noise;uan-latency.txt")
NIL
CG-USER(11): (GENERATE-DATA 5000 300 :reinforce t)
(...)
CG-USER(12): (defvar *reinforce-data* *)
*REINFORCE-DATA*
CG-USER(13): (output-choice-data *reinforce-data* "ACT-R:extras;adaptive-noise;reinforce-choice.txt")
NIL
CG-USER(14): (output-chosen-activation-data *reinforce-data* "ACT-R:extras;adaptive-noise;reinforce-activation.txt")
NIL
CG-USER(15): (output-latency-profile-data *reinforce-data* "ACT-R:extras;adaptive-noise;reinforce-latency.txt")
NIL
CG-USER(16): (generate-data 5000 300 :reinforce t :uan t)
(...)
CG-USER(17): (defvar *reinforce-uan-data* *)
*REINFORCE-UAN-DATA*
CG-USER(18): (output-choice-data *reinforce-uan-data* "ACT-R:extras;adaptive-noise;reinforce-uan-choice.txt")
NIL
CG-USER(19): (output-chosen-activation-data *reinforce-uan-data* "ACT-R:extras;adaptive-noise;reinforce-uan-activation.txt")
NIL
CG-USER(20): (output-latency-profile-data *reinforce-uan-data* "ACT-R:extras;adaptive-noise;reinforce-uan-latency.txt")
NIL

|#
