;;; Example model showing the associative learning mechanims
;;; fitting the data from:
;;;
;;; Anderson, J. R. (1974). Retrieval of propositional information from long-term memory. Cognitive Psychology, 5, 451 - 474.
;;; 
;;; which is avaliable from:
;;;
;;; http://act-r.psy.cmu.edu/?post_type=publications&p=13993
;;;
;;; It works similar to the model in the tutorial which uses 
;;; the non-learning Sji mechanism.
;;;
;;; Here is the data from the experiment (as output by the
;;; tutorial code which includes whether the model responded
;;; correctly:
;;; 
;;; TARGETS:
;;;                          Person fan
;;;   Location      1             2             3
;;;     fan
;;;      1       1.110 (T  )   1.170 (T  )   1.220 (T  )
;;;      2       1.170 (T  )   1.200 (T  )   1.220 (T  )
;;;      3       1.150 (T  )   1.230 (T  )   1.360 (T  )
;;; 
;;; FOILS:
;;;      1       1.200 (T  )   1.220 (T  )   1.260 (T  )
;;;      2       1.250 (T  )   1.360 (T  )   1.290 (T  )
;;;      3       1.260 (T  )   1.470 (T  )   1.470 (T  )
;;; 
;;; Here are the results from the tutorial model and the comparison
;;; to the experiment:
;;;
;;; CORRELATION:  0.864
;;; MEAN DEVIATION:  0.053
;;;
;;; TARGETS:
;;;                          Person fan
;;;   Location      1             2             3
;;;     fan
;;;      1       1.099 (T  )   1.157 (T  )   1.205 (T  )
;;;      2       1.157 (T  )   1.227 (T  )   1.286 (T  )
;;;      3       1.205 (T  )   1.286 (T  )   1.354 (T  )
;;; 
;;; FOILS:
;;;      1       1.245 (T  )   1.290 (T  )   1.328 (T  )
;;;      2       1.290 (T  )   1.335 (T  )   1.373 (T  )
;;;      3       1.328 (T  )   1.373 (T  )   1.411 (T  )
;;; 
;;;
;;; That model does not run the whole experiment. It starts with
;;; all of the test sentences encoded in memory and is then run 
;;; through only one trial to generate a time.  It generates
;;; a result for each cell to report the data (for the foil cells
;;; it averages the result from a request based on a person and
;;; a request based on the location).
;;;
;;; To use the associative learning mechanism this model will
;;; use an approach similar to the one used for the tutorial
;;; model, and for simplicity of testing the mechanims there 
;;; won't be any productions for the model -- all of its actions
;;; will be explicitly generated in code.
;;;
;;;
;;; This model will set a history for all of the relevant chunks
;;; in the task from which the Sji values will be learned.
;;; The model will start with an initial set of declarative chunks
;;; that represent the people and locations.  Those items will
;;; have a long history of having been in the context many times
;;; plus their time in the study phase (varies by fan).  The 
;;; target item chunks will be created at the start of the task 
;;; with no explicit history.  The total model history (for the F 
;;; value in the associative learning equation) will be set to a
;;; very large count over a longer time.
;;;
;;; The optimized learning parameter will be left at its default 
;;; value of t in the model (assumes equally spaced presentation 
;;; times across the history), but can be specified as an
;;; integer value when running a test.
;;; 
;;;
;;; If the history parameter are set like this it will correspond to
;;; the situation modeled in the al-fan-study.lisp file (without
;;; the randomness of the study period):
;;;
;;; (defparameter *probe-creation-time* -2131.8)
;;; (defparameter *probe-prior-context-count* 0) 
;;; (defparameter *in-multiplier* 1) 
;;; (defparameter *study-length* 2131.8) 
;;; (defparameter *tries-per-drop-out* 1) 
;;; (defparameter *global-history-count* 130)
;;; (defparameter *global-history-creation* -2131.8) 
;;; 
;;; and if the :lf, :al-prior-p and :al-prior-w parameters are 
;;; then set to match that model the results are essentially the same:
;;;
;;; > (approach-2-results  0.75 :PRIOR-P 0.08 :PRIOR-W 2.5)
;;; CORRELATION:  0.898
;;; MEAN DEVIATION:  0.044
;;; TARGETS:
;;;                          Person fan
;;;   Location      1             2             3
;;;     fan
;;;      1       1.108         1.156         1.193      
;;;      2       1.156         1.213         1.256      
;;;      3       1.193         1.256         1.304      
;;; 
;;; FOILS:
;;;      1       1.213         1.268         1.310      
;;;      2       1.268         1.330         1.379      
;;;      3       1.310         1.379         1.432      
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Here is the starting model.  
;;; It enables the activation calculations, the associative
;;; learning mechanism with a decay parameter of .5, and sets
;;; the prior history parameter to very small value since a
;;; history is being provided.
;;; It turns off the model output and display of the activation
;;; calculations (they could be turned on to see the details
;;; if desired).  For exploratory purposes, the :ol parameter
;;; is included, and set to t initially (though other values 
;;; will be tested). Then it specifies the slots that will be used 
;;; to represent the chunks for the words and the sentences.

(clear-all)

(require-extra "associative-learning")

(define-model fan
  (sgp :esc t :eal .5)
  (sgp :v nil :act nil)
  (sgp :nsji nil) ;; turn off warning for treating negative Sji values as 0
  (sgp :ol t)
  
  (chunk-type comprehend-sentence relation arg1 arg2)
  (chunk-type meaning word)
    
  (add-dm (hippie isa meaning word "hippie")(guard isa meaning word "guard")
          (earl isa meaning word "earl")(giant isa meaning word "giant")
          (captain isa meaning word "captain")(debutante isa meaning word "debutante")
          (fireman isa meaning word "fireman")(lawyer isa meaning word "lawyer")
          (wizard isa meaning word "wizard")(clerk isa meaning word "clerk")
          (writer isa meaning word "writer")(doctor isa meaning word "doctor")
          (teacher isa meaning word "teacher")(boxer isa meaning word "boxer")
          (queen isa meaning word "queen")(singer isa meaning word "singer")
          (beach isa meaning word "beach")(castle isa meaning word "castle")
          (dungeon isa meaning word "dungeon")(forest isa meaning word "forest")
          (park isa meaning word "park")(church isa meaning word "church")
          (cave isa meaning word "cave")(bank isa meaning word "bank")
          (store isa meaning word "store")(tower isa meaning word "tower")
          (lake isa meaning word "lake")(stadium isa meaning word "stadium")
          (casino isa meaning word "casino")(school isa meaning word "school")
          (diner isa meaning word "diner")(office isa meaning word "office")
          (in isa meaning word "in")))


;;; This is the experimental data for comparison.

(defvar *person-location-data* '(1.11 1.17 1.22
                                 1.17 1.20 1.22
                                 1.15 1.23 1.36
                                 1.20 1.22 1.26
                                 1.25 1.36 1.29
                                 1.26 1.47 1.47))

;;; These are the people and locations used to generate the stimuli
;;; for the experiment.

(defvar *people* '(hippie guard earl giant captain debutante fireman lawyer
                   wizard clerk writer doctor teacher boxer queen singer))

(defvar *places* '(beach castle dungeon forest park church cave bank store
                         tower lake stadium casino school diner office))

;;; Variables to hold the experiment stimuli

(defvar *targets* nil)
(defvar *foils* nil)
(defun *fillers* nil)
(defun *sentences* nil)

;;; Create a randomized set of stimuli for the
;;; studied sentences and the testing items as
;;; described in the paper: 26 study sentences
;;; with 3 in each block of the 3x3 fan matrix
;;; except the 2x2 cell which has only two items,
;;; nine target sentences from that set (one in
;;; each fan pairing) for testing, 18 foils 
;;; (two different ones for each fan pairing), 
;;; and 14 filler sentences which are random
;;; pairings of the previously unused people
;;; and places (thus could be true or false).

(defun create-stims ()
  (let ((people (permute-list *people*))
        (places (permute-list *places*)))
    (setf *sentences*
      (list 
       (list (nth 0 people) (nth 0 places) 1 1)
       (list (nth 1 people) (nth 1 places) 1 1)
       (list (nth 2 people) (nth 2 places) 1 1)
       (list (nth 3 people) (nth 3 places) 2 1)
       (list (nth 4 people) (nth 4 places) 2 1)
       (list (nth 5 people) (nth 5 places) 2 1)
       (list (nth 6 people) (nth 6 places) 3 1)
       (list (nth 7 people) (nth 7 places) 3 1)
       (list (nth 8 people) (nth 8 places) 3 1)
       (list (nth 9 people) (nth 9 places) 1 2)
       (list (nth 10 people) (nth 10 places) 1 2)
       (list (nth 11 people) (nth 11 places) 1 2)
       (list (nth 12 people) (nth 12 places) 1 3)
       (list (nth 13 people) (nth 13 places) 1 3)
       (list (nth 14 people) (nth 14 places) 1 3)
       (list (nth 4 people) (nth 10 places) 2 2)
       (list (nth 15 people) (nth 15 places) 2 2)
       (list (nth 6 people) (nth 9 places) 3 2)
       (list (nth 7 people) (nth 15 places) 3 2)
       (list (nth 8 people) (nth 11 places) 3 2)
       (list (nth 3 people) (nth 12 places) 2 3)
       (list (nth 15 people) (nth 13 places) 2 3)
       (list (nth 5 people) (nth 14 places) 2 3)
       (list (nth 6 people) (nth 12 places) 3 3)
       (list (nth 7 people) (nth 13 places) 3 3)
       (list (nth 8 people) (nth 14 places) 3 3)))
    (setf *targets* 
      (list
       (list (nth 0 people) (nth 0 places) 1 1 t)
       (list (nth 9 people) (nth 9 places) 1 2 t)
       (list (nth 12 people) (nth 12 places) 1 3 t)                         
       (list (nth 3 people) (nth 3 places) 2 1 t)
       (list (nth 4 people) (nth 10 places) 2 2 t)
       (list (nth 15 people) (nth 13 places) 2 3 t)
       (list (nth 6 people) (nth 6 places) 3 1 t)
       (list (nth 7 people) (nth 15 places) 3 2 t)
       (list (nth 8 people) (nth 14 places) 3 3 t)))
    (setf *foils*
      (list
       (list (nth 0 people) (nth 3 places) 1 1 nil)
       (list (nth 9 people) (nth 10 places) 1 2 nil)
       (list (nth 12 people) (nth 13 places) 1 3 nil)
       (list (nth 3 people) (nth 0 places) 2 1 nil)
       (list (nth 4 people) (nth 9 places) 2 2 nil)
       (list (nth 15 people) (nth 12 places) 2 3 nil)
       (list (nth 6 people) (nth 0 places) 3 1 nil)
       (list (nth 7 people) (nth 9 places) 3 2 nil)
       (list (nth 8 people) (nth 12 places) 3 3 nil)
       
       (list (nth 0 people) (nth 6 places) 1 1 nil)
       (list (nth 9 people) (nth 15 places) 1 2 nil)
       (list (nth 12 people) (nth 14 places) 1 3 nil)
       (list (nth 3 people) (nth 6 places) 2 1 nil)
       (list (nth 4 people) (nth 15 places) 2 2 nil)
       (list (nth 15 people) (nth 14 places) 2 3 nil)
       (list (nth 6 people) (nth 3 places) 3 1 nil)
       (list (nth 7 people) (nth 10 places) 3 2 nil)
       (list (nth 8 people) (nth 13 places) 3 3 nil)))
    
       
    (setf *fillers* nil)
    
    (dotimes (i 6)
      (let ((other-people (permute-list (list 1 2 5 10 11 13 14)))
            (other-places (permute-list (list 1 2 4 5 7 8 11))))
        
        (setf *fillers*
          (append *fillers*
                  (mapcar (lambda (x y) (list (nth x people)
                                              (nth y places)
                                              -1 -1 :filler))
                    other-people
                    other-places)))))))

;;; Create a set of stims -- since the histories are being set
;;; explicitly don't need to randomize and run multiple times.

(create-stims)

;;; Add the chunks for the sentences to DM.

(defun create-sentence-chunks ()
  (dolist (x *sentences*)
    (add-dm-fct (list (list 'isa 'comprehend-sentence 'relation 'in 'arg1 (first x) 'arg2 (second x))))))

;;; Compute the response times for each of the target items and
;;; the foils by determining the activation of the chunks that
;;; could be retrieved based on each of the person or location
;;; being used as the probe recording the maximum activation
;;; from each case (the activation of the chunk that would be 
;;; retrieved).  Convert those to times using the current :lf
;;; value and the intercept from the tutorial model (.845s).
;;; Average all the times in a cell and return a list of those
;;; values in the same ordering as the experimental data for 
;;; comparison.

(defvar *task-intercept* .845)

(defun time-to-retrieve-at-start ()

  (let ((data (make-array (list 3 3 2) :initial-element nil))
        (lf (get-parameter-value :lf))
        ; put an empty context chunk into the imaginal buffer
        (ic (set-buffer-chunk 'imaginal (car (define-chunks (isa comprehend-sentence relation in))))))
    
    (dolist (x (append (copy-list *targets*) (copy-list *foils*)))
      
      ;set the person and location in the imaginal chunk
      (set-chunk-slot-value-fct ic 'arg1 (first x))
      (set-chunk-slot-value-fct ic 'arg2 (second x))
      
      ;; get the activations for chunks based on retrieval requests based on person and location
      (let* ((activations-person (no-output (mapcar (lambda (x) (sdp-fct (list x :name :activation)))
                                              (sdm-fct (list 'arg1 (first x) '- 'arg2 nil)))))
             (activations-location (no-output (mapcar (lambda (x) (sdp-fct (list x :name :activation)))
                                                (sdm-fct (list 'arg2 (second x) '- 'arg1 nil)))))
             
             (activation-person (apply 'max (mapcar 'cadar activations-person)))
             (activation-location (apply 'max (mapcar 'cadar activations-location))))
        
        ;; Record the response times using the ACT-R equation for
        ;; for retrieval time:  F * e ^ -A 
        ;; where F is the :lf value and A is the chunk's activation
        
        (push (+ *task-intercept* (* lf (exp (- activation-person)))) (aref data (1- (third x)) (1- (fourth x)) (if (fifth x) 0 1)))
        (push (+ *task-intercept* (* lf (exp (- activation-location)))) (aref data (1- (third x)) (1- (fourth x)) (if (fifth x) 0 1)))
        
        ;; as a safety check, make sure the correct chunk had the best activation
        ;; for target items.
        
        (when (fifth x)
          (let* ((m-person (mapcar 'cadar activations-person))
                 (m-location (mapcar 'cadar activations-location))
                 (retrieved-by-person (caar (find activation-person activations-person :key 'cadar)))
                 (retrieved-by-location (caar (find activation-location activations-location :key 'cadar))))
            (unless (and (eq (chunk-slot-value-fct retrieved-by-person 'arg1) (first x))
                         (eq (chunk-slot-value-fct retrieved-by-person 'arg2) (second x)))
              (format t "Retrieved incorrect chunk ~s for ~s ~s when probed with ~s~%"
                retrieved-by-person (first x) (second x) (first x)))
            (unless (and (eq (chunk-slot-value-fct retrieved-by-location 'arg1) (first x))
                         (eq (chunk-slot-value-fct retrieved-by-location 'arg2) (second x)))
              (format t "Retrieved incorrect chunk ~s for ~s ~s when probed with ~s~%"
                retrieved-by-location (first x) (second x) (second x)))))))
    
    ;; average the results recording into a list like *person-location-data*
    
    (let ((d (make-list 18 :initial-element nil)))
      (dotimes (pf 3)
        (dotimes (lf 3)
          (dotimes (t-f 2)
            (setf (nth (+ pf (* 3 lf) (* 9 t-f)) d)
              (/ (reduce '+ (aref data pf lf t-f))
                 (length (aref data pf lf t-f)))))))
      d)))


;;; These parameters indicate the values for the
;;; indicated components of the histories needed
;;; to compute Sji values.
;;;
;;; Times are in seconds, and the current time
;;; (when the retrievals will be performed) is 0.
;;;
;;; When optimizied learning with a history is enabled 
;;; (an integer value) it will set the histories for 
;;; the indicated number of items to be in steps of
;;; the *recent-history* value.
;;; 
;;; The assumptions used for setting these parameters are:
;;;  - the probe words were learned long ago and used occasionally
;;;  - the word in was learned long ago and used more often than
;;;    the probes
;;;  - the target sentences are newly learned with a history
;;;    based on the study period only and having equal study
;;;  - the total history is significantly larger than just
;;;    the probe words
;;;
;;; Only values that are needed will be set:
;;; - Creation time for probes
;;; - Length of the study period
;;; - the Cj values for when a probe was in the context
;;; - the Ni values for the target sentences 
;;; - the Ni&Cj values for the target sentences
;;; - the global history F
;;;

(defparameter *probe-creation-time* -2131.8) ;; in seconds
(defparameter *probe-prior-context-count* 0) 
(defparameter *in-multiplier* 1) ;; how many times more than probe prior
(defparameter *study-length* 2131.8) ;; in seconds
(defparameter *tries-per-drop-out* 1) 
(defparameter *global-history-count* 130)
(defparameter *global-history-creation* -2131.8) ;; in seconds

;;; Search the study sentences to find the fan of
;;; a probe item

(defun find-item-fan (probe)
  (aif (find probe *sentences* :key 'first)
       (third it)
       (fourth (find probe *sentences* :key 'second))))

;;; Internally times need to be ms but values given are
;;; in seconds 

(defun sec->ms (x) (round x 1/1000))

(defun set-probe-histories ()
  (let ((probes (append (copy-list *people*) (copy-list *places*)))
        (ol (get-parameter-value :ol)))
    
    ;; Set creation-time as specified (gets set in seconds)
    
    (dolist (x probes)
      (sdp-fct (list x :creation-time *probe-creation-time*)))
    
    (sdp-fct (list 'in :creation-time *probe-creation-time*))
  
    ;; The Cj value for probes is *probe-prior-context-count* plus the
    ;; fan of the item for initial presentation plus its fan for each
    ;; drop out attempt (there are two drop out passes with *tries-per-drop-out*
    ;; in each)
        
    (dolist (x probes)
      
      (setf (chunk-al-cj-count x) (+ *probe-prior-context-count* (* (find-item-fan x) (+ 1 (* 2 *tries-per-drop-out*)))))
        
      ;; Create any needed times for ol as equally spaced
      ;; during the study period.
      
      (when (numberp ol)
        (do ((delay (/ *study-length* ol))
              (time (- *study-length*) (+ time delay))
              (count ol (1- count))
              (times nil (cons time times)))
            ((zerop count)
             (setf (chunk-al-cj-refs x) (mapcar 'sec->ms times))))))
    
    
    ;; for in it's *in-multiplier* * *probe-prior-context-count*
    ;; plus 26 (every target's initial presentation) plus
    ;; 26 times 2 * 2 * *tries-per-drop-out* 
    ;; since each target is retrieved twice per drop try (once by person 
    ;; and once by location) in each drop out session
    
    (setf (chunk-al-cj-count 'in) (+ (* *in-multiplier* *probe-prior-context-count*)
                                     (* 26 (+ 1 (* 2 2 *tries-per-drop-out*)))))
    
    ;; for simplicity, just use the same time values for in
    ;; as with the probes
    
    (when (numberp ol)
      (do ((delay (/ *study-length* ol))
           (time (- *study-length*) (+ time delay))
           (count ol (1- count))
           (times nil (cons time times)))
          ((zerop count)
           (setf (chunk-al-cj-refs 'in) (mapcar 'sec->ms times))))))) 
    

(defun set-target-histories ()
  (let ((ol (get-parameter-value :ol)))
    (dolist (x *sentences*)
      ;; find the chunk
      (let ((chunk (car (no-output (sdm-fct (list 'arg1 (first x) 'arg2 (second x)))))))
        
        ;; creation time of targets is the start of study period
        (sdp-fct (list chunk :creation-time (- *study-length*)))
        
        ;; history is 1 + 2 * 2 * *tries-per-drop-out*
        ;; initial presentation plus two retrievals per try per drop-out
        
        (setf (chunk-al-ni-count chunk) (+ 1 (* 2 2 *tries-per-drop-out*)))
        
        ;; use the same time steps as with the probes
        
        (when (numberp ol)
          (do ((delay (/ *study-length* ol))
               (time (- *study-length*) (+ time delay))
               (count ol (1- count))
               (times nil (cons time times)))
              ((zerop count)
               (setf (chunk-al-ni-refs chunk) (mapcar 'sec->ms times)))))))))
  
  
(defun set-relational-histories ()
  (let ((ol (get-parameter-value :ol)))
    (dolist (x *sentences*)
      ;; find the chunk
      (let ((chunk (car (no-output (sdm-fct (list 'arg1 (first x) 'arg2 (second x)))))))
        
        ;; history is 1 + 2 * *tries-per-drop-out* for each probe
        ;; initial presentation (both) one try per retrieval (each) per drop-out
        ;; and 1 + 2 * 2 * tries-per-drop-out* for in
        
        (push (cons chunk (+ 1 (* 2 *tries-per-drop-out*))) (chunk-al-ni+cj-count (first x)))
        (push (cons chunk (+ 1 (* 2 *tries-per-drop-out*))) (chunk-al-ni+cj-count (second x)))
        (push (cons chunk (+ 1 (* 2 2 *tries-per-drop-out*))) (chunk-al-ni+cj-count 'in))
        
        ;; use the same time steps as with the probes
        
        (when (numberp ol)
          (do ((delay (/ *study-length* ol))
               (time (- *study-length*) (+ time delay))
               (count ol (1- count))
               (times nil (cons time times)))
              ((zerop count)
               (progn
                 (push (cons chunk (mapcar 'sec->ms times)) (chunk-al-ni+cj-refs (first x)))
                 (push (cons chunk (mapcar 'sec->ms times)) (chunk-al-ni+cj-refs (second x)))
                 (push (cons chunk (mapcar 'sec->ms times)) (chunk-al-ni+cj-refs 'in))))))))))
  
;; Need to set these directly in the module since 
;; they aren't associated with a chunk and don't 
;; have global parameters to hold them.

(defun set-total-history ()
  (let ((al (get-module associative-learning)))
    
    (setf (al-module-total-count al) *global-history-count*)
    
    (setf (al-module-total-creation-time al) (sec->ms *global-history-creation*))
    
    ;; use same time steps as everything else
    (when (numberp (al-module-ol al))
      (do ((delay (/ *study-length* (al-module-ol al)))
           (time (- *study-length*) (+ time delay))
           (count (al-module-ol al) (1- count))
           (times nil (cons time times))) 
          ((zerop count)
           (setf (al-module-total-refs al) (mapcar 'sec->ms times)))))))


;;; Set the underlying associative learning parameters based on the 
;;; values specified in the variables above, set the ACT-R parameters
;;; based on the provided values, and then compute the time for the retrieval
;;; of the sentences.
;;;
;;; Make sure a valid :ol value is provided.  Then reset
;;; the model, set the :lf and :ol values, add the chunks
;;; for the study items to DM, set the initial histories,
;;; and report the results.

(defun approach-2-results (lf &key (ol t) (prior-p .01) (prior-w 10))
  
  ;; safety checks
  (when (null ol) 
    (print-warning "Can't run if :ol is nil because of the history lengths. Using t instead.")
    (setf ol t))
  (unless (or (eq ol t) (and (integerp ol) (plusp ol)))
    (print-warning ":ol parameter must be t or a positive integer but given ~s. Using t instead." ol)
    (setf ol t))
  
  (reset)
  (suppress-warnings ;; changing declarative parameters
                     ;; with chunks in DM triggers a warning
                     ;; but since we're setting all the history
                     ;; this isn't really a problem.
   
   (set-parameter-value :lf lf)
   (set-parameter-value :ol ol))
   (set-parameter-value :al-prior-p prior-p)
   (set-parameter-value :al-prior-w prior-w)
  
  (create-sentence-chunks)
  (set-probe-histories)
  (set-target-histories)
  (set-relational-histories)
  (set-total-history)
  
  (output-person-location
   (time-to-retrieve-at-start)))


;;; Print the table of data and the comparison stats
       

(defun output-person-location (data)
  (correlation data *person-location-data*)
  (mean-deviation data *person-location-data*)
    
    (format t "TARGETS:~%                         Person fan~%")
    (format t "  Location      1             2             3~%")
    (format t "    fan")
    
    (dotimes (i 3)
      (format t "~%     ~d    " (1+ i))
      (dotimes (j 3)
        (format t "~8,3F      " (nth (+ j (* i 3)) data))))
    
    (format t "~%~%FOILS:")
    (dotimes (i 3)
      (format t "~%     ~d    " (1+ i))
      (dotimes (j 3)
        (format t "~8,3F      " (nth (+ j (* (+ i 3) 3)) data)))))

