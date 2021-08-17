;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2020 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : associative-learning.lisp
;;; Version     : 0.6
;;; 
;;; Description : Reintroduce learning of Sji values with a new approach.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Should it use Sji values that are set explicitly with
;;;                   add-sji to override the learned value?
;;;             : [ ] Should sdp report ALL "non-default" Sji values for a
;;;                   chunk instead of just those that were sources -- any
;;;                   chunk with some history is going to have an Sji that's
;;;                   different from the default but is that useful?
;;; 
;;; ----- History -----
;;; 2020.09.02 Dan [0.1]
;;;             : * First pass at implementation.
;;; 2020.09.22 Dan [0.2]
;;;             : * Fixed a bug in the activation trace output for the Sji
;;;             :   calculation -- was showing Nj data instead of the Ni values.
;;;             : * Fixed an issue with merging time references when :ol is a
;;;             :   number.
;;;             : * Added the :opa parameter to provide an option in how the F
;;;             :   value is computed.
;;;             : * Only updated for a cleared buffer if it's enabled.
;;;             : * Added a slot to store a creation time for F (which could
;;;             :   be computed as the min of all chunks' creation times, but
;;;             :   for now just storing one).
;;; 2020.09.28 Dan [0.3]
;;;             : * Monitor :nsji and warn or force to 0 as indicated.
;;; 2020.10.30 Dan [0.4]
;;;             : * Add a replacement for get-all-chunk-sjis so that sdp will
;;;             :   show all the associations that aren't the default value for
;;;             :   the items with their current value.
;;;             : * Replace all instances of true-chunk-name as a key with the
;;;             :   version since it's being called as a function -- not sure
;;;             :   why those didn't error out...
;;;             : [0.5]
;;;             : * Now sdp also reports the Sji for the chunk to itself as well
;;;             :   even if it hasn't been a source.
;;;             : [0.6]
;;;             : * This is a significant change.  Replacing the parameters for
;;;             :   setting the priors.  Now there's :al-prior-p and 
;;;             :   :al-prior-w.  In addition to that instead of using the
;;;             :   approximation for P(Cj|Ni)/P(Cj|~Ni) use that directly and
;;;             :   compute the needed quantites by removing the relevant items
;;;             :   from the total history that's recorded (or partial in the
;;;             :   case of :ol being set).
;;;             : * Also added the parameter :al-show which shows the details
;;;             :   of computing the Sji values even if :act is off.
;;; 2020.11.04 Dan
;;;             : * Updated the documentation to appropriately describe the new
;;;             :   changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Implement the mechanism described in the Anderson_ACT-R-2020-workshop slides
;;; with these changes/clarifications:
;;;
;;; - Using P(Cj|Ni)/P(Cj|~Ni) instead of the approximation P(Ni|Cj)/P(Ni).
;;; That makes the Sji = log [(F(Ni&Cj)/F(Ni))/(F(~Ni&Cj)/F(~Ni))] not
;;; counting the new parameters for priors described below.
;;;
;;; - The decay will use the optimized learning formulas when they are enabled.
;;;
;;; - Instead of only the imaginal buffer, it applies to all buffers that have
;;; a non-zero activation spread at model definition time.  Any changes to the
;;; buffer spread paramters after the model is defined will not affect how this
;;; module operates with respect to those buffers i.e. if it was on but set to
;;; zero it's still going to be considered a source and if it was zero but set
;;; to non-zero it isn't going to be considered a source.
;;;
;;; - With respect to handling multiple source buffers, if multiple source 
;;; buffers are simultaneously cleared it will result in them not being 
;;; associated with each others' contents.  This is done for ease of 
;;; implementation at this point because without significant additional code 
;;; there would be ordering issues in such a situation (and that order isn't
;;; completely in the modeler's control or guaranteed to be consistent).  If 
;;; associating the contents of multiple simultaneously cleared source buffers
;;; is found to be needed, the module can be updated to handle that.
;;;
;;; - Instead of a single parameter for a prior value that gets added to all 
;;; of the F values, use a prior probability and a weight, which if they are
;;; called p and w respectively results in this equation for Sji:
;;; 
;;; Sji = log [((F(Ni&Cj) + pw)/(F(Ni) + w)) /((F(~Ni&Cj) + pw)/(F(~Ni) + w))]
;;;
;;;
;;; If the :act parameter is set then this module will display the details of 
;;; all the Sji calculations.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Parameters:
;;;  :eal - enable associative learning
;;;         Set to the decay exponent to enable (.5 recomended)
;;;         Will automatically set :mas to a number to enable the
;;;         spreading activation calculation.
;;;  :al-prior-p - associate learning prior probability
;;;         the initial probability given to an association
;;;  :al-prior-w - associative learning prior weight
;;;         how much weight given to that prior value 
;;;  :al-show 
;;;         whether to display the details of the Sji calculations regardless
;;;         of the settings of :act (:v must also be true for the output to
;;;         actually be displayed)
;;;         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Create a separate module for it instead of trying to modify the normal
;;; module.  Tie it in through the hook functions when enabled.  Use chunk
;;; parameters to store the needed values where possible.
;;;
;;; The contents of the source buffers that are upated when a retrieval request
;;; happens are the same ones that are used to compute the spreading activation.
;;; That means any changes the production with the request makes occur before
;;; the sources are determined i.e. modifications and buffer clearing.
;;; 
;;; Setting :mas to pi when :eal is set so that there's an easy check to see if
;;; it was automatically enabled so it can be turned off if :eal is set to nil
;;; (although turning things like :mas and :eal on/off after a model has been
;;; running isn't recommended).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "CENTRAL-PARAMETERS")

(defstruct al-module eal prior-p prior-w total-count total-refs (total-creation-time 0) ol buffers mas act
  (lock (bt:make-recursive-lock)) scale last-retrieval-contexts dm-entry-method nsji show)


;; chunk i was needed -- either enters DM from a spreading buffer or gets retrieved
;;       module does all of the updating (happens after the merge if there was one)
;;       don't copy the value to new chunk
;;       merge retains the c1 value

(suppress-extension-warnings)

(extend-chunks al-ni-count :default-value 0)
(extend-chunks al-ni-refs :default-value nil)

;; chunk j (this chunk) was in the context when a chunk either enters DM from a spreading buffer or gets retrieved
;;       module updates new occurrences (happens after the merge if there was one)
;;       don't copy the value to new chunk
;;       merge needs to:
;;           add the counts 
;;           interleave the times

(defun merge-time-lists (chunk1 chunk2)
  (let* ((al (get-module associative-learning))
         (ol (and (al-module-p al) (bt:with-recursive-lock-held ((al-module-lock al)) (al-module-ol al))))
         (times (unless (eq ol t)
                  (do ((l1 chunk1)
                       (l2 chunk2)
                       (r nil))
                      ((or (null l1) (null l2))
                       (if l1
                           (append r l1)
                         (append r l2)))
                    (cond ((< (car l1) (car l2))
                           (push-last (car l2) r)
                           (setf l2 (cdr l2)))
                          (t ;; same times or l2 smaller
                           (push-last (car l1) r)
                           (setf l1 (cdr l1))))))))
    (if (and (numberp ol) times)
        (subseq times
                0 (min ol (1+ (length times))))
      ;; either it's nil or everything
      times)))

(extend-chunks al-cj-count :default-value 0 :merge-value-function +)
(extend-chunks al-cj-refs :default-value nil :merge-value-function merge-time-lists)


;; chunk j (this chunk) was in the context when a chunk i either enters DM from a spreading buffer or gets retrieved
;;       module updates new occurrences (happens after the merge if there was one so i should be the true-chunk name)
;;       don't copy the value to new chunk
;;       merge needs to:
;;           add the counts for all matching i's
;;           interleave the times for all matching i's
;;
;; the values are alists with i being the car

;; Use true-chunk-name for safety, even though that should already be the
;; case based on how things are updated.

(defun merge-ni+cj-counts (c1 c2)
  (mapcar (lambda (i)
            (cons i (+ (aif (assoc i c1 :key 'true-chunk-name-fct)
                            (cdr it)
                            0)
                       (aif (assoc i c2 :key 'true-chunk-name-fct)
                            (cdr it)
                            0))))
    (remove-duplicates (append (mapcar 'car c1) (mapcar 'car c2)) :key 'true-chunk-name-fct)))

(defun merge-ni+cj-times (c1 c2)
  (mapcar (lambda (i)
            (cons i (merge-time-lists (aif (assoc i c1 :key 'true-chunk-name-fct)
                                           (cdr it)
                                           nil)
                                      (aif (assoc i c2 :key 'true-chunk-name-fct)
                                           (cdr it)
                                           nil))))
    (remove-duplicates (append (mapcar 'car c1) (mapcar 'car c2)) :key 'true-chunk-name-fct)))


(extend-chunks al-ni+cj-count :default-value nil :merge-value-function merge-ni+cj-counts)
(extend-chunks al-ni+cj-refs :default-value nil :merge-value-function merge-ni+cj-times)


;; Record the associated items with a chunk so that sdp can show
;; the sjis for those that have been learned and itself.

(defun default-associated-js (c)
  (list c))

;; They haven't merged yet so need to remove c2's name from it's list, and
;; that will always be the last one since it pushes new js on the list.

(defun merge-associated-js (c1 c2)
  (remove-duplicates (append c1 (butlast c2)) :key 'true-chunk-name-fct))

(extend-chunks associated-js :default-function default-associated-js :merge-value-function merge-associated-js)


(unsuppress-extension-warnings)


;;; Need to replace get-all-chunk-sjis to report all the values which
;;; have been learned for a chunk instead of the default set which are
;;; just itself, its chunk-filled-slots' values, and any set with add-sji.
;;; Since this module doesn't acknowledge the add-sji values in reporting
;;; an Sji we'll ignore those for now too.

(let ((original-sji-function #'get-all-chunk-sjis))
  
  (defun get-all-chunk-sjis (dm chunk)
    (let ((al (get-module associative-learning)))
      (bt:with-recursive-lock-held ((al-module-lock al))
        (if (null (al-module-eal al))
            (funcall original-sji-function dm chunk)
          (mapcar (lambda (x)
                    (list x (compute-learned-sji x chunk)))
            (chunk-associated-js chunk)))))))


;;; Given chunks j and i return the learned Sji value
;;; using the declarative module's compute-references function
;;; that's used to compute the base-level learning from 
;;; the references since it's the same equation and already
;;; handles all the :ol options.

(defun compute-learned-sji (j i)
  (let ((al (get-module associative-learning)))
    (when (and (chunk-p-fct i) (chunk-p-fct j))
      (bt:with-recursive-lock-held ((al-module-lock al))
      (let* ((p (al-module-prior-p al))
             (w (al-module-prior-w al))
             (ol (al-module-ol al))
             (decay (- (al-module-eal al)))
             (scale (al-module-scale al))
             (nicj-count (aif (cdr (assoc i (chunk-al-ni+cj-count j))) it 0))
             (nicj-refs (cdr (assoc i (chunk-al-ni+cj-refs j))))
             (ni-count (aif (chunk-al-ni-count i) it 0))
             (ni-refs (chunk-al-ni-refs i))
             (~ni-refs (set-difference (al-module-total-refs al) ni-refs :test '=))
             (~nicj-refs (set-difference (chunk-al-cj-refs j) nicj-refs :test '=))
             
             (f~ni ;; Those items in the total not in ni
              (if (zerop (- (al-module-total-count al) ni-count))
                  0
                (exp (compute-references nil (- (al-module-total-count al) ni-count)
                                         ~ni-refs
                                         (al-module-total-creation-time al)
                                         decay nil (if (null ~ni-refs) t ol) (al-module-scale al) nil))))
             
             (fni (if (zerop ni-count)
                      0
                    (exp (compute-references nil ni-count ni-refs (chunk-creation-time i)
                                             decay nil ol scale nil))))
             (f~nicj ;; remove those items from cj which are in nicj
              (if (zerop (- (chunk-al-cj-count j) nicj-count))
                  0
                (exp (compute-references nil (- (chunk-al-cj-count j) nicj-count) 
                                         ~nicj-refs
                                         (chunk-creation-time j)
                                         decay nil (if (null ~nicj-refs) t ol) scale nil))))
             (fnicj (if (zerop nicj-count) ;; shouldn't happen but just to be safe
                        0
                      (exp (compute-references nil nicj-count nicj-refs (chunk-creation-time i)
                                               decay nil ol scale nil))))
             (pcjni (/ (+ (* p w) fnicj) (+ w fni)))
             (pcj~ni (/ (+ (* p w) f~nicj) (+ w f~ni)))
             (sji (log (/ pcjni pcj~ni))))
        (when (or (al-module-show al) (al-module-act al))
          (model-output "     Computing Sji from ~s to ~s with the decay set to ~f prior probability ~f and prior weight ~f" j i (- decay) p w)
          (model-output "      F(Ni&Cj) = ~f" fnicj)
          (if nicj-count
              (model-output "      ~s was needed while ~s was in the context ~d times with history ~s" i j nicj-count nicj-refs)
            (model-output "      ~s was needed while ~s was in the context 0 times with history nil" i j))
          (model-output "      F(Ni) = ~f" fni)
          (model-output "        ~s was needed ~d times with history ~s" i ni-count ni-refs)
          (model-output "      F(~~Ni&Cj) = ~f" f~nicj)
          (model-output "        Retrieval of something other than ~s while ~s was in the context ~d times with history ~s" 
                        i j (- (chunk-al-cj-count j) nicj-count) ~nicj-refs)
          (model-output "      F(~~Ni) = ~f" f~ni)
          (model-output "        Retrieval of something other than ~s happened ~d times with history ~s" 
                        i (- (al-module-total-count al) ni-count) ~ni-refs)
          (model-output "      P(Cj|Ni) = ~f" pcjni)
          (model-output "      P(Cj|~~Ni) = ~f" pcj~ni)
          (model-output "      Sji = ~f" sji))
        
        (when (minusp sji)
          (cond ((eq (al-module-nsji al) 'warn)
                 (print-warning "Learned Sji value between ~s and ~s is negative, but using a value of 0." j i)
                 (setf sji 0))
                ((null (al-module-nsji al))
                 (setf sji 0))))
        
        sji)))))


;;; Module interface functions

(defun create-al-module (name)
  (declare (ignore name))
  (make-al-module))

(defun primary-al-reset (al)
  (bt:with-recursive-lock-held ((al-module-lock al))
    (setf (al-module-eal al) nil) ;; normally just let sgp set it
                                  ;; but because it's used in other
                                  ;; parameter tests need to clear
                                  ;; it because order isn't guaranteed
    (setf (al-module-total-count al) 0)
    (setf (al-module-total-creation-time al) 0)
    (setf (al-module-total-refs al) nil)
    (setf (al-module-last-retrieval-contexts al) nil)
    (setf (al-module-dm-entry-method al) nil)))

(defun finish-al-reset (al)
  
  ;; Record the buffers with non-zero source spread
  ;; at model definition time.  Only those buffers
  ;; will be considered for associative learning
  ;; purposes.
  
  (bt:with-recursive-lock-held ((al-module-lock al))
    
    (setf (al-module-buffers al) nil)
    
    (dolist (buffer (buffers))
      (unless (zerop (buffer-spread buffer))
        (if (eq buffer 'retrieval)
            (print-warning "Associative learning will not consider the retrieval buffer as a source of activation.")
          (push buffer (al-module-buffers al)))))))


(defun al-params (al param)
  (bt:with-recursive-lock-held ((al-module-lock al))
    (cond ((consp param)
           (case (car param)
             (:ol (setf (al-module-ol al) (cdr param)))
             (:al-show (setf (al-module-show al) (cdr param)))
             (:eal 
              ;; Warn about decay values that create complex numbers
              (when (and (al-module-ol al) (cdr param) (>= (cdr param) 1.0))
                (print-warning "Setting :eal to a value >= 1 when optimized learning is enabled can result in complex numbers for Sjis and probably errors."))
              
              ;; Set the scale used by compute-references to handle ms->sec conversion
              
              (when (cdr param) (setf (al-module-scale al) (log (expt 1/1000 (- (cdr param))))))
              
              ;; Set the value given so monitored hooks have the value
              
              (setf (al-module-eal al) (cdr param))
              
              (if (cdr param)
                  (set-parameter-value :mas pi)
                (if (and (al-module-mas al)
                         (= (al-module-mas al) pi))
                    (set-parameter-value :mas nil)))
              
              ;; Set the Sji hook if being enabled or remove it if being
              ;; disabled and it's set
              
              (if (cdr param)
                  (sgp :sji-hook compute-learned-sji)
                (when (eq (get-parameter-value :sji-hook) 'compute-learned-sji)
                  (sgp :sji-hook nil)))
              
              ;; For the hook functions, add them if enabled and not there or
              ;; remove them if they are there and it's being disabled
              
              #| Don't need these two currently but may change that

              (if (cdr param)
                  (unless (find 'al-chunk-added (get-parameter-value :chunk-add-hook))
                    (sgp :chunk-add-hook al-chunk-added))
                (when (find 'al-chunk-added (get-parameter-value :chunk-add-hook))
                  (sgp :chunk-add-hook (:remove al-chunk-added))))
              
              (if (cdr param)
                  (unless (find 'al-chunk-merged (get-parameter-value :chunk-merge-hook))
                    (sgp :chunk-merge-hook al-chunk-merged))
                (when (find 'al-chunk-merged (get-parameter-value :chunk-merge-hook))
                  (sgp :chunk-merge-hook (:remove al-chunk-merged))))
              |#
              
              (if (cdr param)
                  (unless (find 'al-chunk-retrieval-request (get-parameter-value :retrieval-request-hook))
                    (sgp :retrieval-request-hook al-chunk-retrieval-request))
                (when (find 'al-chunk-retrieval-request (get-parameter-value :retrieval-request-hook))
                  (sgp :retrieval-request-hook (:remove al-chunk-retrieval-request))))
              
              (if (cdr param)
                  (unless (find 'al-chunk-retrieved (get-parameter-value :retrieved-chunk-hook))
                    (sgp :retrieved-chunk-hook al-chunk-retrieved))
                (when (find 'al-chunk-retrieved (get-parameter-value :retrieved-chunk-hook))
                  (sgp :retrieved-chunk-hook (:remove al-chunk-retrieved))))
              
              ;; return the value
              
              (cdr param))
             
             (:al-prior-p 
              (setf (al-module-prior-p al) (cdr param)))
             
             (:al-prior-w
              (setf (al-module-prior-w al) (cdr param)))

             (:act 
              (setf (al-module-act al) (cdr param))) 
             
             (:nsji 
              (setf (al-module-nsji al) (cdr param)))
             
             (:mas 
              (when (and (null (cdr param)) (al-module-eal al))
                (print-warning "Turning off :mas will disable associative learning."))
              (setf (al-module-mas al) (cdr param)))
             
             ;; For the hook function parameters check if the functions
             ;; are still there if enabled and they're changed.  
             ;; If not, just print a warning -- don't try to "fix" it.
             
             (:sji-hook
              (when (al-module-eal al)
                (unless (eq (cdr param) 'compute-learned-sji)
                  (print-warning "Setting the sji-hook while associative learning is enabled will prevent the learned Sji values from being used."))))
             
             #| Not currently needed
             (:chunk-add-hook
              (when (al-module-eal al)
                (cond ((null (cdr param))
                       (print-warning "Chunk-add-hook parameter cleared. This will affect the learning of Sji values."))
                      ((and (listp (cdr param))
                            (eq (first param) :remove)
                            (eq (second param) 'al-chunk-added))
                       (print-warning "Associative learning's function was removed from the chunk-add-hook. This will affect the learning of Sji values.")))))
             
             (:chunk-merge-hook
              (when (al-module-eal al)
                (cond ((null (cdr param))
                       (print-warning "Chunk-merge-hook parameter cleared. This will affect the learning of Sji values."))
                      ((and (listp (cdr param))
                            (eq (first param) :remove)
                            (eq (second param) 'al-chunk-merged))
                       (print-warning "Associative learning's function was removed from the chunk-merge-hook. This will affect the learning of Sji values.")))))
             |#
             
             (:retrieval-request-hook
              (when (al-module-eal al)
                (cond ((null (cdr param))
                       (print-warning "Retrieval-request-hook parameter cleared. This will affect the learning of Sji values."))
                      ((and (listp (cdr param))
                            (eq (first param) :remove)
                            (eq (second param) 'al-chunk-retrieval-request))
                       (print-warning "Associative learning's function was removed from the retrieval-request-hook This will affect the learning of Sji values.")))))
                
             (:retrieved-chunk-hook
              (when (al-module-eal al)
                (cond ((null (cdr param))
                       (print-warning "Retrieved-chunk-hook parameter cleared. This will affect the learning of Sji values."))
                      ((and (listp (cdr param))
                            (eq (first param) :remove)
                            (eq (second param) 'al-chunk-retrieved))
                       (print-warning "Associative learning's function was removed from the retrieved-chunk-hook This will affect the learning of Sji values.")))))))
                
          (t 
           (case param
             (:al-show 
              (al-module-show al))
             (:eal 
              (al-module-eal al))
             (:al-prior-p 
              (al-module-prior-p al))
             (:al-prior-w 
              (al-module-prior-w al)))))))



;;; When a chunk clears from a buffer schedule an event to update the
;;; parameters if it was a source buffer.
;;;
;;; The event is scheduled so that it happens after all the buffers
;;; that are clearing have been cleared but before the retrieval
;;; request starts (since DM schedules that with a negative priority
;;; even though the request happens prior to buffers being cleared).
;;;
;;; Will consider its own slots as being in the context, and any
;;; other source buffers with chunks in them when the update occurs.
;;;
;;; NOTE: clearing multiple source buffers simultaneously will result
;;; in them not being associated with each others' contents.
;;; This is done for ease of implementation at this point, but if
;;; those associations are needed it could be updated to handle that.

(defun al-check-cleared-chunks (al buffer chunk)
  (bt:with-recursive-lock-held ((al-module-lock al))
    (when (and (al-module-eal al) (find buffer (al-module-buffers al)))
      (schedule-event-now 'al-buffer-cleared :module 'associative-learning :params (list al chunk) :output nil :priority 5))))



(define-module-fct 'associative-learning nil
  (list (define-parameter :eal :valid-test 'posnumornil :default-value nil
          :warning "a positive number or nil" 
          :documentation "enable associative learning by setting a decay parameter")
        
        (define-parameter :al-prior-p :valid-test 'posnum :default-value .01
          :warning "a positive number" 
          :documentation "associative learning prior probability")
        
        (define-parameter :al-prior-w :valid-test 'posnum :default-value 10
          :warning "a positive number" 
          :documentation "associative learning prior weight")
        
        (define-parameter :al-show :valid-test 'tornil :default-value t
          :warning "T or nil" 
          :documentation "whether to show the Sji calculations even when :act is off")
        
        (define-parameter :ol :owner nil)
        (define-parameter :mas :owner nil)
        (define-parameter :act :owner nil)
        (define-parameter :nsji :owner nil)
        (define-parameter :retrieval-request-hook :owner nil)
        (define-parameter :retrieved-chunk-hook :owner nil)
        (define-parameter :chunk-add-hook :owner nil)
        (define-parameter :chunk-merge-hook :owner nil)
        (define-parameter :sji-hook :owner nil))
  :version "0.6"
  :documentation "When enabled, compute Sji values from past experiences."
  :creation 'create-al-module
  :reset (list 'primary-al-reset nil 'finish-al-reset)
  :notify-on-clear 'al-check-cleared-chunks
  :params 'al-params)
        

;;; Functions to detect the necessary situations and record or
;;; update the information as necessary.


;; Just note which situation occurred for the buffer being cleared
;; Don't need this, but if it is needed in the future it's a little
;; tricky because we don't know which buffer so end up recording
;; ones that aren't important for learning and need to be removed
;; later some how...

(defun al-chunk-added (c)
  (declare (ignore c))
  ;(let ((al (get-module associative-learning)))
  ;  (bt:with-recursive-lock-held ((al-module-lock al))
  ;    (push (cons c :added) (al-module-dm-entry-method al))))
  )

;; the original name is passed and it hasn't actually merged yet...
;; same as above

(defun al-chunk-merged (c)
  (declare (ignore c))
  ;(let ((al (get-module associative-learning)))
  ;  (bt:with-recursive-lock-held ((al-module-lock al))
  ;    (push (cons c :merged) (al-module-dm-entry-method al))))
  )


;;; Return a list of the chunks that are in slots of the source buffers.

(defun source-buffer-chunks (al)
  (let ((r nil))
    (dolist (buffer (al-module-buffers al) r)
      (awhen (buffer-read buffer)
             (dolist (slot (chunk-filled-slots-list-fct it))
               (let ((val (fast-chunk-slot-value-fct it slot)))
                 (when (chunk-p-fct val)
                   (push val r))))))))
  
;;; Just record the chunks that are in slots of source buffers.

(defun al-chunk-retrieval-request (spec)
  (declare (ignore spec))
  (let ((al (get-module associative-learning)))
    (bt:with-recursive-lock-held ((al-module-lock al))
      (setf (al-module-last-retrieval-contexts al) (source-buffer-chunks al)))))
   

;;; When the retrieval happens update the parameters where the
;;; DM chunk is i, and the js are the chunks that were in the
;;; context at the retrieval time.

(defun al-chunk-retrieved (chunk)
  (when chunk ;; failure passes nil
    (let ((al (get-module associative-learning)))
      (bt:with-recursive-lock-held ((al-module-lock al))
        (update-al-parameters al chunk (al-module-last-retrieval-contexts al))))))


;;; This is called when a buffer clears.  Update the parameters
;;; where i is the cleared chunk if it was added into DM or the
;;; DM chunk if the chunk was merged into DM and the js are
;;; the current sources plus the chunk filled slots of chunk i.
                            
(defun al-buffer-cleared (al chunk)
  (bt:with-recursive-lock-held ((al-module-lock al))
    (when (al-module-eal al)
      (let (;not needed because added and merged are treated the same
            ;(check (assoc chunk (al-module-dm-entry-method al)))
            
            (self-sources (mapcan (lambda (slot)
                                    (let ((val (fast-chunk-slot-value-fct chunk slot)))
                                      (when (chunk-p-fct val)
                                        (list val))))
                            (chunk-filled-slots-list-fct chunk))))
        ; not needed
        ;(setf (al-module-dm-entry-method al) (remove check (al-module-dm-entry-method al)))
        
        
        
        ;; Don't need this distinction, but putting this here for now incase a need to treat
        ;; those two separately is found.  That will require restoring the code for the 
        ;; hook functions and determining how to deal with chunks from other buffers in those
        ;; functions.
        
        ;(case (cdr check)
        ;  (:added
        ;   (update-al-parameters al chunk (append self-sources (source-buffer-chunks al))))
        ;  (:merged
        ;   (update-al-parameters al (true-chunk-name-fct chunk) (append self-sources (source-buffer-chunks al)))))
        
        
        (update-al-parameters al (true-chunk-name-fct chunk) (append self-sources (source-buffer-chunks al)))))))


;;; Given a needed chunk i and the context chunk js update
;;; all the chunk parameters.
;;;
;;; Only called while the lock is held, so don't need to 
;;; grab that again here.

(defun needed-times (ol ct times)
  (unless (eq ol t) ;; don't need the times when t
    (push ct times)
    (if (numberp ol)
        (subseq times
                0 (min ol (1+ (length times))))
      times)))

(defun update-al-parameters (al i js)
  (let ((ct (mp-time-ms))
        (ol (al-module-ol al)))
    
    ;; Update the global values in the module
    (incf (al-module-total-count al))
    (setf (al-module-total-refs al)
      (needed-times ol ct (al-module-total-refs al)))
    
    ;; Update the needs i values
    
    (incf (chunk-al-ni-count i))
    (setf (chunk-al-ni-refs i)
      (needed-times ol ct (chunk-al-ni-refs i)))
    
    ;; For all chunks j in the context
    (dolist (j js)
      
      ;; record that there's now an association between j and i 
      
      (pushnew j (chunk-associated-js i) :key 'true-chunk-name-fct)
      
      ;; Update the j in context values
      
      (incf (chunk-al-cj-count j))
      (setf (chunk-al-cj-refs j)
        (needed-times ol ct (chunk-al-cj-refs j)))
      
      ;; Update the needs i with j in context values
      
      (aif (assoc i (chunk-al-ni+cj-count j))
           (incf (cdr it))
           (push (cons i 1) (chunk-al-ni+cj-count j)))
      
      (aif (assoc i (chunk-al-ni+cj-refs j))
           (setf (cdr it) (needed-times ol ct (cdr it)))
           (push (cons i (needed-times ol ct nil)) (chunk-al-ni+cj-refs j))))))


;; So it gets recorded as available when required

(provide "associative-learning")

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
