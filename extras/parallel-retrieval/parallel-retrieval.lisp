;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2013 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : parallel-chunks.lisp
;;; Version     : 2.0a1
;;; 
;;; Description : Use the parallel-computation functions to perform the find-
;;;             : matching-chunk operations, declarative retrieval actions
;;;             : (finding matches and computing activation), and blending
;;;             : computations (if the blending module is available) in parallel.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] More testing.
;;; 
;;; ----- History -----
;;; 2013.10.30 Dan
;;;             : * Initial creation.
;;; 2013.11.14 Dan
;;;             : * Added (provide "PARALLEL-CHUNKS") so this can be put into
;;;             :   support for use as needed.
;;; 2014.06.16 Dan [1.0a2]
;;;             : * Updated with the ACT-R 6.1 functions for find and retrieval.
;;; 2014.07.07 Dan 
;;;             : * Updated the blending code for 6.1.
;;; 2015.06.05 Dan
;;;             : * Schedule events in ms and compute-activation-latency is now
;;;             :   returned in ms.
;;; 2015.06.08 Dan
;;;             : * Record the saved trace at the time in ms not seconds.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2016.02.12 Dan
;;;             : * Upated with some changes that happened in dm and blending
;;;             :   code.
;;; 2016.02.26 Dan
;;;             : * Fixed a potential bug with the blending code.
;;; 2018.08.31 Dan [2.0a1]
;;;             : * Updated for use with 7.x, and split it into its own extra
;;;             :   which requires the parallel-computation extra.
;;; 2018.09.06 Dan
;;;             : * Unfortunately, thread safety on chunk parameters really 
;;;             :   causes problems for trying to parallelize activations since
;;;             :   partial matching and spreading activation both have issues 
;;;             :   with trying to access the same chunks (the source and the
;;;             :   requested values) and base-level needs the current time.
;;;             :   Those end up making it less efficient than the serial 
;;;             :   mechanism without a complete rewrite of the activation code
;;;             :   (and likely the chunk parameter code as well).
;;;             : * Find matching chunks also seems to have some sort of collision
;;;             :   in ACL even with some significant rewriting although it 
;;;             :   doesn't in CCL which means there's something implicitly 
;;;             :   locking/blocking in ACL and I'm guessing hashtables...
;;;             : * For those reasons this is not going to be updated any further
;;;             :   at this time and is not recommended for use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; DO NOT USE!
;;;
;;;
;;; Very speculative replacement functions for performing some potentially costly 
;;; operations in parallel.  
;;;
;;; To load this the best option is to put it into the ACT-R support directory
;;; and then either call this or put it into a file which will be using the
;;; provided capabilities:
;;;
;;; (require-extra "parallel-retrieval")
;;;
;;; 
;;; When enabled, this will use multiple processes when searching for matching
;;; chunks with find-matching-chunks (used by the vision module for isa visual-
;;; location requests, the blending module for blending requests, and for parallel
;;; retrieval requests which bypasses the activation trace info), when 
;;; performing the activation computation for chunks which match a retrieval 
;;; request, and when performing the activation and blending computations in a 
;;; blending request.
;;;
;;; This is not likely to improve performance for most users.
;;; 
;;; Typically the model needs to have large sets of chunks to be searched over,
;;; have a large number of chunks for which the activation needs to be computed,
;;; or have complex activation computations for the chunks to see any benefit
;;; from this to overcome the overhead of running in parallel.  For the performance 
;;; testing models it looks like somewhere around 50 chunks in the set to be 
;;; tested was the point where performance improvements started to show up, but
;;; if the model uses blending the benefits may show up for small sets of chunks
;;; being blended if there are multiple slots receiving a blended value in the 
;;; majority of the blending requests.
;;;
;;; None of the tutorial models seem to show a benefit.  
;;;
;;; Parallel operation should not be used if the activation or blending traces are
;;; enabled or being saved.
;;; 
;;; Because the parallel operations will not always be executed in the same order as
;;; a sequentially run model the determinism provided by setting the seed parameter
;;; doesn't exist if it's run in parallel.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(require-extra "parallel-computation")

(defvar *actr-team* nil)

(defun enable-retrieval-team (n)
  (if *actr-team*
      (print-warning "Retrieval team is already enabled with a size of ~d." (p_team-size *actr-team*))
    (progn
      (setf *actr-team* (create-parallel-team "Retrieval-team" n))
      t)))


(add-act-r-command "enable-retrieval-team" 'enable-retrieval-team "Enable the parallel retrieval capabilities with a team of the indicated size. Params: <size>" t)

(defun release-retrieval-team ()
  (when *actr-team*
    (release-parallel-team *actr-team*)
    (setf *actr-team* nil)))

(add-act-r-command "release-retrieval-team" 'release-retrieval-team "Enable the parallel retrieval capabilities with a team of the indicated size. Params: <size>" t)


(defun unlocked-chunk-slot-equal (val1 val2)
  (if (eq val1 val2)
      t
    (let (c1 c2)
      (cond (;; The real definition of this test has to lock the chunk table to avoid
             ;; a potential merge or change between the get-chunk calls, but that lock
             ;; is held during the entire find for the parallel case otherwise each
             ;; worker blocks the others and it's worse than sequential...
               (and (setf c1 (unlocked-get-chunk val1))   
                    (setf c2 (unlocked-get-chunk val2)))
             (eq c1 c2))
            ((stringp val1) 
             (and (stringp val2) (string-equal val1 val2)))
            (t (equalp val1 val2))))))


(defun unlocked-chunk-slot-not-equal (arg1 arg2)
  (not (unlocked-chunk-slot-equal arg1 arg2)))


(defun unlocked-get-chunk (name)
  (let ((model (current-model-struct)))
    (gethash name (act-r-model-chunks-table model))))


(defun unlocked-match-chunk-spec-p (chunk-name chunk-spec
                                      &key (=test 'chunk-slot-equal) 
                                      (-test 'chunk-slot-not-equal)
                                      (>test 'safe>) (>=test 'safe>=) 
                                      (<test 'safe<) (<=test 'safe<=)
                                      (variable-char #\=))
  (let ((chunk (unlocked-get-chunk chunk-name)))
    (cond ((null chunk)
           (print-warning "~s does not name a chunk in call to match-chunk-spec-p." chunk-name))
          (t 
           (let ((s (cond ((act-r-chunk-spec-p chunk-spec) chunk-spec)
                          ((numberp chunk-spec) (id-to-chunk-spec chunk-spec))
                          (t nil))))
             (if (null s)
                 (print-warning "~s is not a valid chunk-spec or chunk-spec id in call to match-chunk-spec-p." chunk-spec)
               (let ((var (cond ((characterp variable-char) variable-char)
                                ((and (stringp variable-char) (= (length variable-char) 1)) (char variable-char 0))
                                (t nil))))
                 (if (null var)
                     (print-warning "~s is not a valid variable character in call to match-chunk-spec-p." variable-char)
                   (progn
                     
                     ;; check the variable character and reprocess the spec 
                     ;; if it's not = since that may mean different slotnames are "real"
                     
                     (unless (char-equal var #\=)
                       (setf s (reprocess-chunk-spec s var)))
                     
                     (bt:with-recursive-lock-held ((act-r-chunk-lock chunk))
                       (cond
                        ;; if the filled slots are filled and the empty slots aren't then test further
                        ((slots-vector-match-signature (act-r-chunk-filled-slots chunk) 
                                                       (act-r-chunk-spec-filled-slots s) 
                                                       (act-r-chunk-spec-empty-slots s))
                         
                         (handler-case (test-chunk-slots (act-r-chunk-slot-value-lists chunk)
                                                         s =test -test >test >=test <test <=test)
                           (error (condition) 
                             (print-warning "Error ~S encountered in matching chunk ~s." condition chunk-name))))
                        (t
                         nil))))))))))))


(defun find-matching-chunks (chunk-spec 
                             &key 
                             (chunks :all) =test -test
                             (>test 'safe>) (>=test 'safe>=) 
                             (<test 'safe<) (<=test 'safe<=)
                             (variable-char #\=))
  
  (verify-current-model
   "Find-matching-chunks called with no current model."
   
   (let ((model (current-model-struct))
         (s (cond ((act-r-chunk-spec-p chunk-spec) chunk-spec)
                  ((numberp chunk-spec) (id-to-chunk-spec chunk-spec))
                  (t nil))))
     
     (if (null s)
         (print-warning "~s is not a valid chunk-spec or chunk-spec id in call to find-matching-chunks." chunk-spec)
       (let ((var (cond ((characterp variable-char) variable-char)
                        ((and (stringp variable-char) (= (length variable-char) 1)) (char variable-char 0))
                        (t nil))))
         (if (null var)
             (print-warning "~s is not a valid variable character in call to find-matching-chunks." variable-char)
           
           (cond ((eq :all chunks)
                  (if *actr-team*
                      (bt:with-recursive-lock-held ((act-r-model-chunk-lock (current-model-struct)))
                        (parallel-mapcan *actr-team*
                                       (lambda (z)
                                         (when (unlocked-match-chunk-spec-p z s 
                                                                            :=test (aif =test it 'unlocked-chunk-slot-equal) 
                                                                            :-test (aif -test it 'unlocked-chunk-slot-not-equal)
                                                                            :>test >test :>=test >=test 
                                                                            :<test <test :<=test <=test
                                                                            :variable-char var)
                                           (list z)))
                                         (hash-table-keys (act-r-model-chunks-table model))))
                    (bt:with-recursive-lock-held ((act-r-model-chunk-lock (current-model-struct)))
                      (mapcan (lambda (z)
                                (when (unlocked-match-chunk-spec-p z s 
                                                                   :=test (aif =test it 'unlocked-chunk-slot-equal) 
                                                                   :-test (aif -test it 'unlocked-chunk-slot-not-equal)
                                                                   :>test >test :>=test >=test 
                                                                   :<test <test :<=test <=test
                                                                   :variable-char var)
                                  (list z)))
                        (hash-table-keys (act-r-model-chunks-table model))))))
                 ((listp chunks)
                  (if *actr-team*
                      (bt:with-recursive-lock-held ((act-r-model-chunk-lock (current-model-struct)))
                        (parallel-mapcan *actr-team*
                                         (lambda (z)
                                           (when (unlocked-match-chunk-spec-p z s 
                                                                              :=test (aif =test it 'unlocked-chunk-slot-equal) 
                                                                              :-test (aif -test it 'unlocked-chunk-slot-not-equal)
                                                                              :>test >test :>=test >=test 
                                                                              :<test <test :<=test <=test
                                                                              :variable-char var)
                                             (list z)))
                                         chunks))
                    (bt:with-recursive-lock-held ((act-r-model-chunk-lock (current-model-struct)))
                      (mapcan (lambda (z)
                                (when (unlocked-match-chunk-spec-p z s 
                                                                   :=test (aif =test it 'unlocked-chunk-slot-equal) 
                                                                   :-test (aif -test it 'unlocked-chunk-slot-not-equal)
                                                                   :>test >test :>=test >=test 
                                                                   :<test <test :<=test <=test
                                                                   :variable-char var)
                                  (list z)))
                        chunks))))
                 (t (print-warning "~S is not a valid value for the :chunks keyword parameter to find-matching-chunks." chunks)))))))))


#|

(defun start-retrieval (dm request)
  (let (esc rt rrh sact act er set-hook
            mp offsets blc bll ol act-scale
            sa spreading-hook w-hook sji-hook mas nsji
            pm-hook ms md sim-hook cache-sim-hook
            noise-hook ans lf le bl-hook)
  
    
    (bt:with-lock-held ((dm-state-lock dm))
      (when (dm-stuff-event dm)
        (delete-event (dm-stuff-event dm))
        (setf (dm-stuff-event dm) nil)))
    
    
    (bt:with-lock-held ((dm-param-lock dm))
      (setf
       act (dm-act dm)
       rt (dm-rt dm)
       sact (dm-sact dm)
       mp (dm-mp dm)
       esc (dm-esc dm)
       offsets (dm-offsets dm)
       blc (dm-blc dm)
       bll (dm-bll dm)
       ol (dm-ol dm)
       act-scale (dm-act-scale dm)  
       sa (dm-sa dm)
       spreading-hook (dm-spreading-hook dm)
       w-hook (dm-w-hook dm)
       sji-hook (dm-sji-hook dm)
       mas (dm-mas dm)
       nsji (dm-nsji dm)
       pm-hook (dm-partial-matching-hook dm)
       ms (dm-ms dm)
       md (dm-md dm)
       sim-hook (dm-sim-hook dm)
       cache-sim-hook (dm-cache-sim-hook-results dm)
       noise-hook (dm-noise-hook dm)
       ans (dm-ans dm)
       rrh (dm-retrieval-request-hook dm)
       er (dm-er dm)
       set-hook (dm-retrieval-set-hook dm)
       lf (dm-lf dm)
       le (dm-le dm)
       bl-hook (dm-bl-hook dm)))
    
    
    (when rrh
      (let ((id (chunk-spec-to-id request)))
        (dolist (x rrh)
          (dispatch-apply x id))
        (release-chunk-spec-id id)))
    
    (when sact
      (bt:with-lock-held ((dm-state-lock dm))
        (setf (dm-current-trace dm) (make-sact-trace :esc esc))
        (setf (gethash (mp-time-ms) (dm-trace-table dm)) (dm-current-trace dm))))
  
    (let* ((last-request (make-last-request :time (mp-time-ms) :spec request :rt rt))
           (filled (chunk-spec-filled-slots request))
           (empty (chunk-spec-empty-slots request))
           (chunk-list 
            ;; Parallel unlikely to be useful at this point -- few types in mem...
            (if *actr-team*
                           (parallel-mapcan *actr-team* 
                                            (lambda (x) 
                                              (when (slots-vector-match-signature (car x) filled empty)
                                                  (copy-list (cdr x))))
                                            (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunks dm)))
                         (mapcan (lambda (x) 
                                   (when (slots-vector-match-signature (car x) filled empty)
                                       (copy-list (cdr x))))
                           (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunks dm))))))
    
    (flet ((invalid (reason warnings)
                      (bt:with-lock-held ((dm-state-lock dm))
                        (setf (dm-busy dm) nil)
                        (setf (last-request-invalid last-request) reason)
                        (setf (dm-last-request dm) last-request)
                        )
                      (dolist (x warnings)
                        (print-warning x))
                      (return-from start-retrieval)))
      
      (when (member :recently-retrieved (chunk-spec-slots request))
        (let ((recent (chunk-spec-slot-spec request :recently-retrieved)))
          (cond ((> (length recent) 1)
                 (invalid :too-many '("Invalid retrieval request." ":recently-retrieved parameter used more than once.")))
                ((not (or (eq '- (caar recent)) (eq '= (caar recent))))
                 (invalid :bad-modifier '("Invalid retrieval request." ":recently-retrieved parameter's modifier can only be = or -.")))
              ((not (or (eq t (third (car recent)))
                        (eq nil (third (car recent)))
                        (and (eq 'reset (third (car recent)))
                             (eq '= (caar recent)))))
               (invalid :bad-value '("Invalid retrieval request." ":recently-retrieved parameter's value can only be t, nil, or reset.")))
              (t ;; it's a valid value for recently-retrieved
               
               (if (eq 'reset (third (car recent)))
                   (bt:with-lock-held ((dm-state-lock dm))
                     (setf (dm-finsts dm) nil))
                 
                 (let ((finsts (remove-old-dm-finsts dm)))
                   
                     (cond ((or (and (eq t (third (car recent)))   ;; = request t
                                     (eq (caar recent) '=)) 
                                (and (null (third (car recent)))   ;; - request nil
                                     (eq (caar recent) '-)))
                          
                            ;; only those chunks marked are available
                            
                            (setf chunk-list (intersection (mapcar 'car finsts) chunk-list))
                            
                            ;; save that info for whynot
                            (setf (last-request-finst last-request) :marked)
                            (setf (last-request-finst-chunks last-request) chunk-list)
                        
                            (when sact
                              (bt:with-lock-held ((dm-state-lock dm))
                                (setf (sact-trace-only-recent (dm-current-trace dm)) t)
                                (setf (sact-trace-recents (dm-current-trace dm)) chunk-list)))
                        
                            (when (dm-act-level act 'high)
                              (model-output "Only recently retrieved chunks: ~s" chunk-list)))
                           (t
                            ;; simply remove the marked items
                            ;; may be "faster" to do this later
                            ;; once the set is trimed elsewise, but
                            ;; for now keep things simple
                            (unwind-protect
                                (progn
                                  (when sact
                                    (bt:acquire-lock (dm-state-lock dm))
                                    (setf (sact-trace-remove-recent (dm-current-trace dm)) t))
                                  
                                  (when (dm-act-level act 'high)
                                    (model-output "Removing recently retrieved chunks:"))
                                  
                                  (setf (last-request-finst last-request) :unmarked)
                                  
                                  (setf chunk-list 
                                    (remove-if (lambda (x)
                                                 (when (member x finsts :key 'car :test 'eq-chunks-fct)
                                                   
                                                   (when sact
                                                     (push-last x (sact-trace-recents (dm-current-trace dm))))
                                                   
                                                   (when (dm-act-level act 'high)
                                                     (model-output "~s" x))
                                                   
                                                   (push x (last-request-finst-chunks last-request))
                                                   t))
                                               chunk-list)))
                              (when sact
                                (bt:release-lock (dm-state-lock dm))))))))))))
    

      (when (member :mp-value (chunk-spec-slots request))
        (let ((mp-value (chunk-spec-slot-spec request :mp-value)))
          (cond ((> (length mp-value) 1)
                 (invalid :mp-multi '("Invalid retrieval request." ":mp-value parameter used more than once.")))
                ((not (eq '= (caar mp-value)))
                 (invalid :mp-modifier '("Invalid retrieval request." ":mp-value parameter's modifier can only be =.")))
                ((not (numornil (third (car mp-value))))
                 (invalid :mp-not-num '("Invalid retrieval request." ":mp-value parameter's value can only be nil or a number.")))
                
                (t ;; it's a valid request
                 (setf mp (third (car mp-value)))))))
          
      (let ((best-val nil)
            (best nil)
            (return-val nil)
            (chunk-set 
             (cond ((or (null esc) (null mp)) ;; exact matches only
                    ;; normall would do them individually for tracing purposes
                    ;; but when parallel just get them since tracing not possible
                    ;; anyways -- should do that in both cases...
                    
                    (if *actr-team*
                        (find-matching-chunks request :chunks chunk-list)
                      (unwind-protect
                        (let ((found nil))
                          (when sact
                            (bt:acquire-lock (dm-state-lock dm)))
                        
                          (dolist (name chunk-list found)
                            (cond ((match-chunk-spec-p name request)
                                   (when sact
                                     (push-last name (sact-trace-matches (dm-current-trace dm))))
                                   (when (dm-act-level act 'medium)
                                     (model-output "Chunk ~s matches" name)) 
                                   (push-last name found))
                                  (t
                                   (when sact
                                     (push-last name (sact-trace-no-matches (dm-current-trace dm))))
                                   (when (dm-act-level act 'high)
                                     (model-output "Chunk ~s does not match" name))))))
                      (when sact
                        (bt:release-lock (dm-state-lock dm))))))
                   (t ;; partial matching
                      ;; everything that fits the general pattern:
                      ;; filled and empty slots (already handled)
                      ;; also test the inequalities >, <, >=, and <= 
                        
                    (let* ((extra-spec (mapcan (lambda (x)
                                                 (unless (or (eq (car x) '=) (eq (car x) '-) (keywordp (second x)))
                                                   x))
                                         (chunk-spec-slot-spec request)))
                           (matches (if extra-spec
                                        (find-matching-chunks (define-chunk-spec-fct extra-spec) :chunks chunk-list)
                                      ;; reverse it to keep the ordering the same
                                      ;; relative to the older version and so that
                                      ;; things are consistent with different requests
                                      (nreverse chunk-list)))
                           (non-matches (when (or act sact)
                                          (set-difference chunk-list matches))))
                          
                          (when (dm-act-level act 'high)
                            (dolist (c non-matches)
                              (model-output "Chunk ~s does not match" c)))
                          
                          (when sact
                            (bt:with-lock-held ((dm-state-lock dm))
                              (setf (sact-trace-matches (dm-current-trace dm)) matches)
                              (setf (sact-trace-no-matches (dm-current-trace dm)) non-matches)))
                          matches)))))
        
        (setf (last-request-matches last-request) chunk-set)
            
        (if esc
            (if *actr-team*
                (let* ((current-time (mp-time-ms)) ;; probably also needed in the base-level calculation...
                       (bests (parallel-funcall *actr-team*
                                                   (lambda (set) 
                                                     (let ((b nil)
                                                           (bv nil))
                                                       
                                                       (dolist (x set)
                                                         (compute-activation dm x request :params-provided t
                                                                             :act act :sact sact :mp mp :esc esc :offsets offsets 
                                                                             :blc blc :bll bll :ol ol :act-scale act-scale
                                                                             :sa sa :spreading-hook spreading-hook :w-hook w-hook :sji-hook sji-hook 
                                                                             :mas mas :nsji nsji :pm-hook pm-hook :ms ms :md md :sim-hook sim-hook
                                                                             :cache-sim-hook cache-sim-hook :noise-hook noise-hook :ans ans :bl-hook bl-hook)
      
                                                         (let ((a (setf (chunk-retrieval-activation x) (chunk-activation x))))
                                                           (setf (chunk-retrieval-time x) current-time)
        
                                                           (cond ((null bv)
                                                                  (setf bv a)
                                                                  (push x b))
                                                                 ((= a bv)
                                                                  (push x b))
                                                                 ((> a bv)
                                                                  (setf bv a)
                                                                  (setf b (list x))))))
                                                       (list (cons bv b))))
                                                   chunk-set)))
                      (setf best-val (apply 'max (mapcar 'car bests)))
                      (setf best (mapcan (lambda (x) (if (= (car x) best-val) (cdr x) nil)) bests))
                      )
              (dolist (x chunk-set)
                (compute-activation dm x request :params-provided t
                                    :act act :sact sact :mp mp :esc esc :offsets offsets 
                                    :blc blc :bll bll :ol ol :act-scale act-scale
                                    :sa sa :spreading-hook spreading-hook :w-hook w-hook :sji-hook sji-hook 
                                    :mas mas :nsji nsji :pm-hook pm-hook :ms ms :md md :sim-hook sim-hook
                                    :cache-sim-hook cache-sim-hook :noise-hook noise-hook :ans ans :bl-hook bl-hook)
                
                (setf (chunk-retrieval-activation x) (chunk-activation x))
                (setf (chunk-retrieval-time x) (mp-time-ms))
                
                (cond ((null best-val)
                       (setf best-val (chunk-activation x))
                       (push x best)
                       (when (dm-act-level act 'medium)
                         (model-output "Chunk ~s has the current best activation ~f" x best-val)))
                      ((= (chunk-activation x) best-val)
                       (push x best)
                       (when (dm-act-level act 'medium)
                         (model-output "Chunk ~s matches the current best activation ~f" x best-val)))
                      ((> (chunk-activation x) best-val)
                       (setf best-val (chunk-activation x))
                       (setf best (list x))
                       (when (dm-act-level act 'medium)
                         (model-output "Chunk ~s is now the current best with activation ~f" x best-val))))))
            
          (setf best chunk-set))
            
        (when (> (length best) 1)
          (if er
              (let ((b (random-item best)))
                (setf best (cons b (remove b best))))
            (setf best (sort best 'string<)))) ;; deterministic but unspecified...
        
        (setf (last-request-best last-request) best)
                        
        (when (car set-hook)
          (let ((chunk-set-with-best (when best (cons (car best) (remove (car best) chunk-set)))))
            (dolist (x set-hook)
              (let ((val (dispatch-apply x chunk-set-with-best)))
                (when val
                  (if return-val
                      (progn 
                        (print-warning "multiple set-hook functions returned a value - none used")
                        (setf return-val :error))
                    (setf return-val val)))))))
            
        (bt:with-lock-held ((dm-state-lock dm))
          (setf (dm-last-request dm) last-request)
          
          (cond ((and (listp return-val) (numberp (second return-val))
                      (chunk-p-fct (decode-string (first return-val))))
                 (let ((c (decode-string (first return-val))))
                   (setf (dm-busy dm) (schedule-event-relative (second return-val) 'retrieved-chunk
                                                               :module 'declarative 
                                                               :destination 'declarative 
                                                               :params (list c)
                                                               :details (concatenate 'string
                                                                          (symbol-name 'retrieved-chunk)
                                                                          " "
                                                                          (symbol-name (car return-val)))
                                                               :output 'medium))
                 
                   (when sact
                     (setf (sact-trace-result-type (dm-current-trace dm)) :force)
                     (setf (sact-trace-result (dm-current-trace dm)) c))
                   
                   (when (dm-act-level (dm-act dm) 'low)
                     (model-output "Retrieval-set-hook function forced retrieval of chunk ~s" c))))
                
                ((numberp return-val)
                 (setf (dm-busy dm) (schedule-event-relative return-val 'retrieval-failure
                                                             :module 'declarative 
                                                             :destination 'declarative 
                                                             :output 'low))
                 
                 (when sact
                   (setf (sact-trace-result-type (dm-current-trace dm)) :force-fail))
                 
                 (when (dm-act-level (dm-act dm) 'low)
                   (model-output "Retrieval-set-hook function forced retrieval failure")))
                
                ((or (null best) 
                     (and esc
                          (< best-val rt)))
                 
                 (setf (dm-busy dm) (schedule-event-relative (if esc
                                                                 (compute-activation-latency rt lf le)
                                                               0)
                                                             'retrieval-failure
                                                             :time-in-ms t 
                                                             :module 'declarative 
                                                             :destination 'declarative
                                                             :output 'low))
                 
                 (when sact
                   (setf (sact-trace-result-type (dm-current-trace dm)) :fail)
                   (setf (sact-trace-result (dm-current-trace dm)) (when best rt)))
                 
                 (when (dm-act-level act 'low)
                   (if best 
                       (model-output "No chunk above the retrieval threshold: ~f" rt)
                     (model-output "No matching chunk found retrieval failure"))))
                
                ((= (length best) 1)
                 (setf (dm-busy dm) (schedule-event-relative (if esc
                                                                 (compute-activation-latency (chunk-activation (car best)) lf le)
                                                               0)
                                                             'retrieved-chunk
                                                             :time-in-ms t 
                                                             :module 'declarative 
                                                             :destination 'declarative 
                                                             :params best
                                                             :details 
                                                             (concatenate 'string
                                                               (symbol-name 'retrieved-chunk)
                                                               " "
                                                               (symbol-name (car best)))
                                                             :output 'medium))
                 
                 (when sact
                   (setf (sact-trace-result-type (dm-current-trace dm)) :single)
                   (setf (sact-trace-result (dm-current-trace dm)) (cons (car best) (chunk-activation (car best)))))
                 
                 (when (dm-act-level act 'low)
                   (model-output "Chunk ~s with activation ~f is the best" (car best) (chunk-activation (car best)))))
                (t
                 (let ((best1 (car best)))
                   
                   (setf (dm-busy dm) (schedule-event-relative (if esc
                                                                   (compute-activation-latency (chunk-activation best1) lf le)
                                                                 0)
                                                               'retrieved-chunk
                                                               :time-in-ms t 
                                                               :module 'declarative 
                                                               :destination 'declarative 
                                                               :params (list best1)
                                                               :details 
                                                               (concatenate 'string
                                                                 (symbol-name 'retrieved-chunk)
                                                                 " "
                                                                 (symbol-name best1))
                                                               :output 'medium))
                   (when sact
                     (setf (sact-trace-result-type (dm-current-trace dm)) :multi)
                     (setf (sact-trace-result (dm-current-trace dm)) (cons best1 (chunk-activation best1))))
                   
                   (when (dm-act-level act 'low)
                     (model-output "Chunk ~s chosen among the chunks with activation ~f" best1 (chunk-activation best1))))))
          
          (when sact
            (setf (dm-current-trace dm) nil))))))))



(defun blend-slot (dm instance activation-list dont-generalize slot request)
  
  (let* ((possible-values (mapcan (lambda (x) 
                                    (awhen (fast-chunk-slot-value-fct (third x) slot)
                                           (list (make-blending-item :name (third x) :value it :prob (second x)
                                                                     :mag (funcall (blending-module-v->m instance) it)))))
                            activation-list))
         (slot-vals (mapcar 'blending-item-value possible-values))
         (mags (mapcar 'blending-item-mag possible-values))
         (true-mags (remove nil mags)))
    
    (cond ((every 'null mags) ;; they're all nil
           (cons slot nil))
          
          ((every 'numberp true-mags)
           
           (let ((sum 0))
             (dolist (mag possible-values)
               (awhen (blending-item-mag mag)
                      (let ((increment (* it (blending-item-prob mag))))
                        (incf sum increment))))
             
             (cond ((and (blending-module-m->v instance)
                         (not (equalp slot-vals mags)))
                    (let ((result (funcall (blending-module-m->v instance) sum request)))
                      (cons slot result)))
                   (t 
                    (cons slot sum)))))
                  
          (t
           (let ((which (if (every 'chunk-p-fct true-mags)
                            (if (not (find slot dont-generalize))
                                :chunks :not-generalized)
                          :other)))
             (let* ((type (when (eq which :chunks) (common-chunk-type true-mags)))
                    (chunks (if (eq which :chunks)
                                (if (zerop type) 
                                    (all-dm-chunks dm)
                                  (mapcan (lambda (x) 
                                            (if (slots-vector-match-signature (car x) type)
                                                (copy-list (cdr x))
                                              nil))
                                    (dm-chunks dm)))
                              (remove-duplicates true-mags :test 'equalp)))
                    (best-val nil)
                    (best-mag nil))
                       
               (if (= 1 (length chunks))
                   (setf best-val (car chunks))
                 
                 (dolist (val chunks)
                   (let ((sum 0.0))
                     (dolist (possible possible-values)
                       (let ((sim (chunks-similarity dm val (blending-item-mag possible))))
                         (incf sum (* (blending-item-prob possible) (expt sim 2)))))
                             
                     (when (or (null best-mag)
                               (< sum best-mag))
                       (setf best-mag sum)
                       (setf best-val val)))))
                       
               (cond ((and (blending-module-m->v instance)
                           (not (equalp slot-vals mags)))
                      (let ((result (funcall (blending-module-m->v instance) best-val request)))
                        (cons slot result)))
                     (t 
                      (cons slot best-val)))))))))
         
        
(defun start-blending (instance request)
  
    (flet ((blending-terminated (&rest warnings)
                              (complete-request request)
                              (dolist (x warnings)
                                (print-warning x))
                              (return-from start-blending)))
      (let ((ignore nil) 
            (dont-generalize nil)
            (sblt nil))
        
        ;; set the currently pending request
  
        (setf (blending-module-pending-request instance) request)

        (when (blending-module-sblt instance)
          (setf sblt (make-sblt-trace))
          (setf (gethash (mp-time) (blending-module-trace-table instance)) sblt))
    
    (when (member :ignore-slots (chunk-spec-slots request))
      
      (let ((ignore-slots (chunk-spec-slot-spec request :ignore-slots)))
        (cond ((> (length ignore-slots) 1)
               (blending-terminated "Invalid blending request." ":ignore-slots parameter used more than once."))
              ((not (listp (spec-slot-value (first ignore-slots))))
               (blending-terminated "Invalid blending request." ":ignore-slots parameter's value must be a list."))
              (t 
               (setf ignore (spec-slot-value (first ignore-slots)))))))
    
    (when (member :do-not-generalize (chunk-spec-slots request))
      
      (let ((ignore-slots (chunk-spec-slot-spec request :do-not-generalize)))
        (cond ((> (length ignore-slots) 1)
               (blending-terminated "Invalid blending request." ":do-not-generalize parameter used more than once."))
              ((not (listp (spec-slot-value (first ignore-slots))))
               (blending-terminated "Invalid blending request." ":do-not-generalize parameter's value must be a list."))
              (t 
               (setf dont-generalize (spec-slot-value (first ignore-slots)))))))
    
    (let* ((dm (get-module declarative))                   ;; get that module since we're using some of its functions
           (request-details (chunk-spec-slot-spec request))
           (fixed-values (remove-if (lambda (x) (or (keywordp (spec-slot-name x)) (not (eq (spec-slot-op x) '=)))) request-details))
           (fixed-slots (mapcar 'spec-slot-name fixed-values))
           
           ;; perform the chunk matching just like the declarative module does
           (filled (chunk-spec-filled-slots request))
           (empty (chunk-spec-empty-slots request))
           (chunk-list (remove-if 'chunk-blending-suppressed
                                   (mapcan (lambda (x) 
                                             (if (slots-vector-match-signature (car x) filled empty)
                                                 (copy-list (cdr x))
                                               nil))
                                     (dm-chunks dm))))
           
           (matching-chunks (cond ((or (null (blending-module-esc instance)) 
                                       (null (blending-module-mp instance)))
                                   ;; Perfect matching 
                                   (find-matching-chunks request :chunks chunk-list))
                                  (t
                                   ;; everything that fits the general pattern:
                                   ;; filled and empty already done
                                   ;; so just test the inequalities
                                   
                                   (let ((extra-spec (flatten (mapcan (lambda (x)
                                                                        (unless (or (eq (spec-slot-op x) '=) 
                                                                                    (eq (spec-slot-op x) '-) 
                                                                                    (keywordp (spec-slot-name x)))
                                                                          (list x)))
                                                                request-details))))
                                     (if extra-spec
                                         (find-matching-chunks (define-chunk-spec-fct extra-spec) :chunks chunk-list)
                                       chunk-list)))))
           
           (all-slots (slot-mask->names (reduce 'logior matching-chunks :key 'chunk-slots-vector)))
           
           (blended-slots (remove-if (lambda (x) 
                                       (find x ignore))
                                     (if (blending-module-blend-all instance)
                                         all-slots
                                       (set-difference all-slots fixed-slots))))
           
           (temperature (aif (blending-module-tmp instance) 
                             it
                             (if (null (blending-module-ans instance))
                                 (progn
                                   (print-warning "Blending requires :tmp or :ans to be set - assuming default of 1.")
                                   1.0)
                               (* (sqrt 2) (blending-module-ans instance)))))
           
           ;; Have the declarative module compute the activations and record them here
            (activation-list (if (and *use-actr-team* (<= *use-actr-team* (length matching-chunks)))
                                
                                (parallel-mapcan *actr-team* 
                                                 (lambda (chunk) 
                                                   (compute-activation dm chunk request)   ;; compute the activation
                                                   (setf (chunk-blended-activation chunk) (chunk-activation chunk))
                                                   (setf (chunk-blended-time chunk) (mp-time))
                                                   (if (and (blending-module-min-bl instance) (< (chunk-last-base-level chunk) (blending-module-min-bl instance)))
                                                       (progn (setf (chunk-blending-suppressed chunk) t)
                                                         nil)
                                                     (list (list (chunk-activation chunk) 
                                                                 nil
                                                                 chunk))))
                                                 matching-chunks)
                              (mapcan (lambda (chunk) 
                                        (compute-activation dm chunk request)   ;; compute the activation
                                        (setf (chunk-blended-activation chunk) (chunk-activation chunk))
                                        (setf (chunk-blended-time chunk) (mp-time))
                                        (if (and (blending-module-min-bl instance) (< (chunk-last-base-level chunk) (blending-module-min-bl instance)))
                                            (progn (setf (chunk-blending-suppressed chunk) t)
                                              nil)
                                          (list (list (chunk-activation chunk) 
                                                      nil
                                                      chunk))))
                                matching-chunks)))
           (max-ai (when activation-list (reduce 'max activation-list :key 'first))))
      
      
      (if (and *use-actr-team* (<= *use-actr-team* (length matching-chunks)))
          (parallel-run *actr-team* (lambda (x)
                                      (setf (second x)
                                        (handler-case (exp (/ (- (first x) max-ai) temperature))
                                          (floating-point-underflow () 0)
                                          (floating-point-overflow () ;; shouldn't actually happen...
                                                                   (print-warning "Math overflow during blending.  Chunk activation for ~s is ~s with temperature ~s" (third x) (first x) temperature)
                                                                   (print-warning "Results of blending are not likely to be meaningful.")
                                                                   ;; just use something big and assume it's available for now...
                                                                   (exp 50)))))
                        activation-list)
          (dolist (x activation-list)
            (setf (second x)
              (handler-case (exp (/ (- (first x) max-ai) temperature))
                (floating-point-underflow () 0)
                (floating-point-overflow () ;; shouldn't actually happen...
                                         (print-warning "Math overflow during blending.  Chunk activation for ~s is ~s with temperature ~s" (third x) (first x) temperature)
                                         (print-warning "Results of blending are not likely to be meaningful.")
                                         ;; just use something big and assume it's available for now...
                                         (exp 50))))))
      
      (when (and (blending-module-blend-all instance) (null (blending-module-mp instance)))
        (print-warning "The :blend-all-slots parameter is set to t, but the :mp parameter is nil which means only perfect matches can occur."))
      
      (when (blending-module-sblt instance)
        (setf (sblt-trace-chunk-type sblt) (cons filled empty)))
      
      (when (blending-module-trace instance)
        (model-output "Blending request for chunks ~@[with slots ~a~] ~@[without slots ~a~]" 
                      (slot-mask->names filled)
                      (slot-mask->names empty)))
      
      
      (awhen (blending-module-request-hooks instance)
             (dolist (x it)
               (funcall x request)))
      
      (awhen (blending-module-set-hooks instance)
             (dolist (x it)
               (funcall x matching-chunks)))

      
      (when (null activation-list) ;; a complete failure
        
        (when (blending-module-sblt instance)
          (setf (sblt-trace-no-matches sblt) t))
        
        (when (blending-module-trace instance)
          (model-output "No matching chunks found.")
          (model-output "Blending request fails."))
        
        ;; schedule the failure event to happen and record that as the busy flag
        ;; failure time same as for declarative - based on the retrieval threshold
        
        (setf (blending-module-busy instance) 
          (schedule-event-relative (compute-activation-latency dm (blending-module-rt instance))
                                   'blending-failure :time-in-ms t :module 'blending
                                   :destination 'blending :output 'low))
        
        (awhen (blending-module-result-hooks instance)
               (dolist (x it)
                 (funcall x nil)))
        
        (return-from start-blending nil))
      
      (when (blending-module-sblt instance)
        (setf (sblt-trace-tmp sblt) (blending-module-tmp instance))
        (setf (sblt-trace-temperature sblt) temperature))
      
      (when (blending-module-trace instance)
        (if (blending-module-tmp instance)
            (model-output "Blending temperature is: ~f" temperature)
          (if (blending-module-ans instance)
              (model-output "Blending temperature defaults to (* (sqrt 2) :ans): ~f" temperature)
            (model-output "Blending temperature defaults to 1 when :ans and :tmp are both nil"))))
      
      (let ((sum (reduce '+ activation-list :key 'second)) ;; parallelizing this probably isn't useful because tests seem to show those additions take 
                                                           ;; negligible time for lists of 10k elements...
            (new-chunk nil)
            (blended-results (mapcar (lambda (x) (cons x nil)) blended-slots))
            (sblt-slot nil))
        
        ;; also probably not worth parallelizing unless lists are expected to be
        ;; really large...
        ;;(if (and *use-actr-team* (<= *use-actr-team* (length activation-list)))
        ;;    (parallel-run *actr-team* (lambda (x) (setf (second x) (/ (second x) sum))) activation-list)
        ;;
        
        (mapc (lambda (x) (setf (second x) (/ (second x) sum))) activation-list)
        
        ;)
       
        (when (blending-module-sblt instance)
          (setf (sblt-trace-activation-list sblt) activation-list)
          (setf (sblt-trace-blended-slots sblt) blended-slots)
          (setf (sblt-trace-ignore sblt) ignore))
        
        (when (blending-module-trace instance)
          (dolist (x activation-list)
            (model-output "Chunk ~S matches blending request~%  Activation ~f~%  Probability of recall ~f~%"
                          (third x) (first x) (second x)))
          (model-output "~%Slots to be blended: ~S" blended-slots)
          (when ignore 
            (model-output "Slots being explicitly ignored: ~S~%" ignore)))
        
        (if (and *use-actr-team* (second blended-slots)) ;; if there's more than one always do in parallel
            (setf blended-results
              (parallel-run *actr-team* (lambda (x) (blend-slot dm instance activation-list dont-generalize x request)) blended-slots))
          
          (dolist (slot blended-slots)
            
            (when (blending-module-sblt instance)
              (setf sblt-slot (make-sblt-slot :name slot))
              (push-last sblt-slot (sblt-trace-slot-list sblt)))
            
            (when (blending-module-trace instance)
              (model-output "Finding blended value for slot: ~s" slot))
            
            (let* ((possible-values (mapcan (lambda (x) 
                                              (awhen (fast-chunk-slot-value-fct (third x) slot)
                                                     (list (make-blending-item :name (third x) :value it :prob (second x)
                                                                               :mag (funcall (blending-module-v->m instance) it)))))
                                      activation-list))
                   (slot-vals (mapcar 'blending-item-value possible-values))
                   (mags (mapcar 'blending-item-mag possible-values))
                   (true-mags (remove nil mags)))
              
              (when (blending-module-sblt instance)
                (setf (sblt-slot-slot-vals sblt-slot) slot-vals)
                (setf (sblt-slot-magnitudes sblt-slot) possible-values))
              
              (when (blending-module-trace instance)
                (model-output "Matched chunks' slots contain: ~S" slot-vals)
                (model-output "Magnitude values for those items: ~S" mags))
              
              (cond ((every 'null mags) ;; they're all nil
                     
                     (when (blending-module-sblt instance)
                       (setf (sblt-slot-condition sblt-slot) :null))
                     
                     (when (blending-module-trace instance)
                       (model-output "When all magnitudes are nil there's nothing to blend and the slot is ignored"))
                     
                     (setf blended-results (remove slot blended-results :key 'car)))
                    
                    ((every 'numberp true-mags)
                     
                     (when (blending-module-sblt instance)
                       (setf (sblt-slot-condition sblt-slot) :numbers))
                     
                     (let ((sum 0))
                       (when (blending-module-trace instance)
                         (model-output "With numeric magnitudes blending by weighted average"))
                       
                       (dolist (mag possible-values)
                         (awhen (blending-item-mag mag)
                                (let ((increment (* it (blending-item-prob mag))))
                                  (incf sum increment)
                                  (when (blending-module-trace instance)
                                    (model-output " Chunk ~s with probability ~f times magnitude ~f = ~f cumulative result: ~f" 
                                                  (blending-item-name mag) (blending-item-prob mag) it increment sum)))))
                       
                       (cond ((and (blending-module-m->v instance)
                                   (not (equalp slot-vals mags)))
                              (let ((result (funcall (blending-module-m->v instance) sum request)))
                                (setf (cdr (assoc slot blended-results)) result)
                                
                                (when (blending-module-sblt instance)
                                  (setf (sblt-slot-mag-adjusted sblt-slot) t)
                                  (setf (sblt-slot-adjusted-val sblt-slot) result))
                                
                                (when (blending-module-trace instance)
                                  (model-output " Final result: ~f  Converted to value: ~s" sum result))))
                             (t 
                              (setf (cdr (assoc slot blended-results)) sum)
                              (when (blending-module-trace instance)
                                (model-output " Final result: ~f" sum))))))
                    
                    (t
                     
                     (let ((which (if (every 'chunk-p-fct true-mags)
                                      (if (not (find slot dont-generalize))
                                          :chunks :not-generalized)
                                    :other)))
                       
                       (when (blending-module-sblt instance)
                         (setf (sblt-slot-condition sblt-slot) which))
                       
                       
                       (let* ((type (when (eq which :chunks) (common-chunk-type true-mags)))
                              (chunks (if (eq which :chunks)
                                          (if (zerop type) 
                                              (all-dm-chunks dm)
                                            (mapcan (lambda (x) 
                                                      (if (slots-vector-match-signature (car x) type)
                                                          (copy-list (cdr x))
                                                        nil))
                                              (dm-chunks dm)))
                                        (remove-duplicates true-mags :test 'equalp))))
                         
                         (when (blending-module-trace instance)
                           (case which
                             (:chunks
                              (model-output "When all magnitudes are chunks blending based on similarities to all related chunks"))
                             (:not-generalized
                              (model-output "When all magnitudes are chunks and the slot is not generalized blending based on similarities to only those chunks"))
                             (:other
                              (model-output "When not all magnitudes are numbers or chunks blending based on similarities using those unique values: ~s" chunks))))
                       
                       
                         
                         (when (blending-module-sblt instance)
                           (setf (sblt-slot-ctype sblt-slot) type)
                           (setf (sblt-slot-chunks sblt-slot) chunks))
                         
                         (when (and (eq which :chunks) (blending-module-trace instance))
                           (if (not (zerop type))
                               (model-output "Intersection of slots for values is: ~s" (slot-mask->names type))
                             (model-output "No intersecting slots found all chunks will be tested")))
                         
                         (let ((best-val nil)
                               (best-mag nil))
                           
                           (if (= 1 (length chunks))
                               (progn
                                 (setf best-val (car chunks))
                                 (when (blending-module-trace instance)
                                   (model-output " Only one possible value ~S" (car chunks))))
                             
                             
                             (dolist (val chunks)
                               
                               (when (blending-module-trace instance)
                                 (model-output " Comparing value ~S" val))
                               
                               (let ((sum 0.0))
                                 
                                 (dolist (possible possible-values)
                                   
                                   (let ((sim (chunks-similarity dm val (blending-item-mag possible))))
                                     (when (blending-module-sblt instance)
                                       (push-last (cons val (list possible sim))
                                                  (sblt-slot-possible-values sblt-slot)))
                                     
                                     (incf sum (* (blending-item-prob possible) (expt sim 2)))
                                     
                                     (when (blending-module-trace instance)
                                       (model-output "  Chunk ~s with probability ~f slot value ~s~@[ converted to magnitude ~s~] similarity: ~f cumulative result: ~f" 
                                                     (blending-item-name possible) (blending-item-prob possible)
                                                     (blending-item-value possible) 
                                                     (if (equalp (blending-item-value possible) (blending-item-mag possible))
                                                         nil
                                                       (blending-item-mag possible))
                                                     sim sum))))
                                 
                                 (when (or (null best-mag)
                                           (< sum best-mag))
                                   (setf best-mag sum)
                                   (setf best-val val)))))
                           
                           (cond ((and (blending-module-m->v instance)
                                       (not (equalp slot-vals mags)))
                                  (let ((result (funcall (blending-module-m->v instance) best-val request)))
                                    (setf (cdr (assoc slot blended-results)) result)
                                    
                                    (when (blending-module-sblt instance)
                                      (setf (sblt-slot-mag-adjusted sblt-slot) t)
                                      (setf (sblt-slot-adjusted-val sblt-slot) result))
                                    
                                    (when (blending-module-trace instance)
                                      (model-output " Final result: ~s  Converted to value: ~s" best-val result))))
                                 (t 
                                  (setf (cdr (assoc slot blended-results)) best-val)
                                  (when (blending-module-trace instance)
                                    (model-output " Final result: ~s" best-val))))))))))))
        
        ;; put the fixed values into the chunk def.
        
        (unless (blending-module-blend-all instance)
          (dolist (slot fixed-values)
            (push (spec-slot-name slot) new-chunk)
            (push (spec-slot-value slot) new-chunk)))
        
        ;; put the blended values into the chunk def.
        
        (dolist (slot blended-results)
          (push (car slot) new-chunk)
          (push (cdr slot) new-chunk))
        
        (setf new-chunk (nreverse new-chunk))
        
        (when (blending-module-sblt instance)
          (setf (sblt-trace-new-chunk sblt) new-chunk))
        
        (when (blending-module-trace instance)
          (model-output "This is the definition of the blended chunk:~%~s" new-chunk)
          (model-output "~%Computing activation and latency for the blended chunk"))
        
        (let ((act 0))
          
          (dolist (c activation-list)
            (incf act (exp (first c)))
            
            (when (blending-module-trace instance)
              (model-output " Activation of chunk ~S is ~f" (third c) (first c))))
          
          (setf act (log act))
          
          (when (blending-module-trace instance)
            (model-output "Activation for blended chunk is: ~f" act))
          
          (when (blending-module-sblt instance)
            (setf (sblt-trace-act sblt) act))
          
          (cond ((>= act (blending-module-rt instance))
                 (setf (blending-module-busy instance) 
                   (schedule-event-relative 
                    (compute-activation-latency dm act)
                    'blending-complete
                    :time-in-ms t
                    :module 'blending
                    :destination 'blending
                    :params (list new-chunk)
                    :details (symbol-name 'blending-complete)
                    :output 'medium)))
                (t 
                 (when (blending-module-trace instance)
                   (model-output "Not above threshold so blending failed"))
                 
                 (when (blending-module-sblt instance)
                   (setf (sblt-trace-fail sblt) t))
                 
                 (setf (blending-module-busy instance) 
                   (schedule-event-relative 
                    (compute-activation-latency dm (blending-module-rt instance))
                    'blending-failure
                    :time-in-ms t
                    :module 'blending
                    :destination 'blending
                    :details (symbol-name 'blending-failure)
                    :output 'medium))))))))))

|#
(provide "parallel-retrieval")

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
