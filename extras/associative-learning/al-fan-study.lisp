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
;;; model, and for simplicity of testing the mechanism there 
;;; won't be any productions for the model -- all of its actions
;;; will be explicitly generated in code.
;;;
;;;
;;; To create the history for learning, this model will use a 
;;; random set of testing items as described in experiment 1 
;;; of the paper. The model will start with an initial set of
;;; declarative chunks that represent the people and locations
;;; with no explicit histories provided (the prior history will
;;; be represented by the :al-prior-p and :al-prior-w parameter
;;; settings).  
;;;
;;; It will be run through an initial presentation of a
;;; randomized ordering of the target sentences, spending
;;; 10 seconds on each.  Then it will be run through two
;;; simulated drop-out passes where it sees each probe once
;;; (in a random ordering) and correclty retrieves all of 
;;; the items for that probe spending 20 seconds per retrieval
;;; (for a total time of about 35 minutes in the study phase).
;;; Then, instead of actually performing the testing the
;;; activation of retrieving the appropriate chunk for each
;;; target and foil test item will be computed based on the
;;; associations which were learned during training.  That
;;; will be done twice, once computing the activation for
;;; retrieving the item using the person and once using the
;;; location in the retrieval request (both items will be
;;; sources of activation in the imaginal buffer).  Those
;;; activations will be converted to a time (adjusting the
;;; :lf parameter to scale appropriately) and added to
;;; an intercept time of .845 seconds which is the time the
;;; tutorial model takes to perform all of the actions that 
;;; are not the critical retrieval.  Because of the randomized
;;; items, that process will be performed multiple times and
;;; all the times averaged to create the results.
;;;
;;; Below are the results of some rough parameter fitting 
;;; to get the model to perform similar to the one from the
;;; tutorial which doesn't use the learning mechanism (there 
;;; may be better sets of parameters, but these are good 
;;; enough for a demo).
;;;
;;; Here are the results after fitting the :lf, :prior-p, and
;;; :prior-w parameters with the optimized learning parameter
;;; set to it's default of t (assumes equally spaced 
;;; presentation times across the history instead of recording
;;; explicit times).

;;;
;;; > (APPROACH-1-RESULTS 50 0.75 :PRIOR-P 0.08 :PRIOR-W 2.5)
;;; CORRELATION:  0.897
;;; MEAN DEVIATION:  0.044
;;; TARGETS:
;;;                          Person fan
;;;   Location      1             2             3
;;;     fan
;;;      1       1.108         1.156         1.192      
;;;      2       1.156         1.212         1.255      
;;;      3       1.192         1.255         1.303      
;;; 
;;; FOILS:
;;;      1       1.213         1.267         1.310      
;;;      2       1.267         1.330         1.378      
;;;      3       1.310         1.378         1.431      
;;;     
;;; 
;;; Here are the results fit over 50 runs when :ol is 
;;; is set to 1 (one prior reference recorded) which
;;; required changing the :prior-p slightly to get
;;; the "same" fit:
;;; 
;;; > (APPROACH-1-RESULTS 50 0.75 :OL 1 :PRIOR-P 0.06 :PRIOR-W 2.5)
;;; CORRELATION:  0.898
;;; MEAN DEVIATION:  0.045
;;; TARGETS:
;;;                          Person fan
;;;   Location      1             2             3
;;;     fan
;;;      1       1.103         1.159         1.190      
;;;      2       1.161         1.236         1.273      
;;;      3       1.188         1.270         1.349      
;;; 
;;; FOILS:
;;;      1       1.220         1.275         1.310      
;;;      2       1.275         1.337         1.375      
;;;      3       1.311         1.377         1.420      
;;; 
;;; Here are the results fit over 50 runs when :ol is 
;;; set to nil (all references recorded) which required 
;;; changing the :lf slightly to get the "same" fit
;;; (because of the storage and computation costs :ol nil
;;; is not recommended in general when using this module
;;; but is ok for this task given the small number of
;;; chunks involved and the limited number of item
;;; presentations):
;;;
;;; > (APPROACH-1-RESULTS 50 0.7 :OL nil :PRIOR-P 0.06 :PRIOR-W 2.5)
;;; CORRELATION:  0.894
;;; MEAN DEVIATION:  0.045
;;; TARGETS:
;;;                          Person fan
;;;   Location      1             2             3
;;;     fan
;;;      1       1.105         1.155         1.188      
;;;      2       1.154         1.208         1.251      
;;;      3       1.191         1.253         1.296      
;;; 
;;; FOILS:
;;;      1       1.211         1.263         1.297      
;;;      2       1.264         1.326         1.360      
;;;      3       1.298         1.361         1.404      
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remove any existing models and make sure the associative
;;; learning extension is included:

(clear-all)

(require-extra "associative-learning")

;;; Here is the starting model.  
;;; It enables the activation calculations, the associative
;;; learning mechanism with a decay parameter of .5, and sets
;;; the retrieval threshold low so that none of the simulated
;;; retrievals in the study/training sections will fail.
;;; It turns off the model output and display of the activation
;;; calculations (they could be turned on to see the details
;;; if desired).  It also disables the use of negative Sji
;;; values (the default is to not use them and also print a
;;; warning so this just removes the warning). Then it just 
;;; specifies the slots that will be used to represent the chunks
;;; for the words and the sentences.

(define-model fan
  (sgp :esc t :eal .5 :rt -4)
  (sgp :v nil :act nil)
  (sgp :nsji nil)
  
  (chunk-type comprehend-sentence relation arg1 arg2)
  (chunk-type meaning word))


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

;;; Add chunks for the people, places, and word "in" to the model's 
;;; declarative memory.

(defun create-initial-chunks ()
  (dolist (x *people*)
    (add-dm-fct (list (list x 'isa 'meaning 'word (symbol-name x)))))
  (dolist (x *places*)
    (add-dm-fct (list (list x 'isa 'meaning 'word (symbol-name x)))))
  (add-dm (in isa meaning word "in")))


;;; Randomly present the model with each of the study sentences
;;; giving it 10 seconds between presentations.

(defun initial-presentation ()
  (dolist (x (permute-list *sentences*))
    (module-request 'imaginal (define-chunk-spec-fct (list 'relation 'in 'arg1 (first x) 'arg2 (second x))))
    (schedule-clear-buffer 'imaginal 1.5)
    (run 10)))

;;; Simulate the drop-out task by randomly presenting each of
;;; the possible probes and then having the model retrieve all
;;; of the correct responses for that probe with 20s between
;;; the retrievals.

(defun drop-out () 
  (let ((lf (get-parameter-value :lf)))
    (set-parameter-value :lf 0) ;; ignore actual retrieval time for simplicity
    
    (dolist (x (permute-list (append (copy-list *people*) (copy-list *places*))))
      
      ;; don't want to create associations to the prompts or deposit
      ;; the prompts into DM for this example
      (erase-buffer 'imaginal)
      
      (module-request 'imaginal (define-chunk-spec-fct (list 'relation 'in 'arg1 x)))
      (run .2)
      (dolist (y (remove-if-not (lambda (z)
                                  (or (eq (first z) x) (eq (second z) x)))
                                *sentences*))
        (module-request 'retrieval (define-chunk-spec-fct (list 'relation 'in 'arg1 (first y) 'arg2 (second y))))
        (run-full-time 20)))
    (set-parameter-value :lf lf))) ;; restore the latency factor parameter value
    

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


;;; Run the model n times through the task setting the 
;;; parameters :lf, :al-show, :ol, :al-prior-p, and :al-prior-w
;;; with the provided values, and report the average results
;;; and their comparison to the experiment data.

(defun approach-1-results (n lf &key (show nil) (ol t) (prior-p .01) (prior-w 10))
  (let ((results nil))
    (dotimes (i n)
      (create-stims)
      (reset)
      (set-parameter-value :lf lf)
      (set-parameter-value :al-show show)
      (set-parameter-value :ol ol) 
      (set-parameter-value :al-prior-p prior-p)
      (set-parameter-value :al-prior-w prior-w)
      (create-initial-chunks)
      (initial-presentation)
      (drop-out)
      (drop-out)
      (push (time-to-retrieve-at-start) results))
    (output-person-location
     (mapcar (lambda (x) (/ x n))
       (reduce (lambda (y z)
                 (mapcar '+ y z))
               results)))))

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

