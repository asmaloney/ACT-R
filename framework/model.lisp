;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : model.lisp
;;; Version     : 4.1
;;; 
;;; Description : Functions that support the abstraction of a model
;;; 
;;; Bugs        : 
;;;
;;; To do       : Finish the documentation.
;;; 
;;; ----- History -----
;;;
;;; 2004.20.08 Dan
;;;             : Creation
;;; 2005.01.12 Dan
;;;             : Don't need to special case the device because it's now an
;;;             : actual module.
;;; 2005.02.11 Dan
;;;             : * Changed some reset-model to use clrhash instead of 
;;;             :   creating new tables.
;;; 2005.02.28 Dan
;;;             : * Made the with-model macro hygienic.
;;; 2005.03.23 Dan
;;;             : * Update the model reset and creation to use the two reset 
;;;             :   functions that are now part of the module definition - one 
;;;             :   before the parameter are reset and one after.
;;; 2006.07.05 Dan
;;;             : * Fixed a bug in the delete-model-fct function in the format 
;;;             :   command for printing that there was no model.
;;; 2006.11.07 Dan
;;;             : * Fixed a bug in delete-model which could result in the deleted
;;;             :   model being left as the current model after deletion.
;;; 2007.04.19 Dan
;;;             : * Fixed another bug in delete-model which left the current-model
;;;             :   set even when the last model currently defined was deleted.
;;; 2008.08.21 Dan
;;;             : * Added a new with-model-eval macro that sits half way between
;;;             :   the current macro and the -fct.  This macro does evaluate the
;;;             :   first parameter to get the name, but splices in the body
;;;             :   to evaluate.  Should be more convenient in many circumstances.
;;; 2008.10.22 Dan [1.0]
;;;             : * Updated the reset function to handle the new chunk-ref table
;;;             :   and setting its flags to default values.
;;;             : * Finally took the a1 off the version.
;;; 2008.12.01 Dan
;;;             : * Added the code to call a third module reset function after
;;;             :   the user code has been evaled.
;;; 2008.12.08 Dan
;;;             : * Added new-chunk-type-size and largest-chunk-type-size to
;;;             :   record and get the largest possible chunk size.
;;; 2008.12.15 Dan
;;;             : * Added the code to call the third reset function when the
;;;             :   model is initially created too.
;;; 2009.09.09 Dan
;;;             : * Updated reset-model to also clear the chunk-set from all of
;;;             :   the multi-buffers.
;;; 2009.10.08 Dan
;;;             : * Updated define-model to clear the chunk-set as well since
;;;             :   reset doesn't happen at initial definition time.
;;; 2009.12.03 Dan
;;;             : * Delete-model needs to take events out of the meta-process's 
;;;             :   dynamics list as well.
;;; 2010.08.17 Dan
;;;             : * Better cleanup in define-model when there's an error during
;;;             :   the definition code.
;;; 2010.11.16 Dan
;;;             : * Added the :mcts and :mctrt system parameters for configuring
;;;             :   the model's chunk table because those can speed things up 
;;;             :   significantly in some circumstances (for example ACL works
;;;             :   better with :mctrt of .6 and :mcts set large if there are
;;;             :   going to be a lot of chunks created).
;;; 2011.11.11 Dan
;;;             : * Added a warning about model names that could cause problems
;;;             :   for the environment.
;;; 2012.02.06 Dan
;;;             : * Added unsafe-define-model for my debugging purposes because
;;;             :   sometimes it's easier to fix things if the errors are unhandled.
;;; 2012.08.08 Dan
;;;             : * Record the models as they are defined in meta-p-model-order
;;;             :   so things like reset can occur in the same order as when
;;;             :   initially created for consistency.
;;; 2013.01.04 Dan
;;;             : * Added unwind-protect to reset- and delete- model so that the
;;;             :   current model always gets restored (define model doesn't need
;;;             :   it since it has other protection and doesn't just restore the
;;;             :   "last" model anyway).
;;;             : * Added a check to define-model to prevent it from creating a
;;;             :   new model when it's not valid to do so based on meta-p-cannot-define-model
;;;             :   and added the cannot-define-model calls to places that need
;;;             :   to be protected.
;;; 2013.01.07 Dan
;;;             : * Changed the test on cannot-define-model to be >0 instead of
;;;             :   just true.
;;; 2014.02.24 Dan [2.0]
;;;             : * Don't bother creating the chunk-type chunk since it's 
;;;             :   automatic now.
;;;             : * Largest chunk-type size is total slot count (not the most
;;;             :   efficient method, but go with it for now).
;;; 2014.05.16 Dan
;;;             : * Added more default chunk types: constant-chunk and clear.
;;;             :   constant-chunk specifies a name slot which can then be used
;;;             :   to prevent chunks from merging to avoid oddities like free
;;;             :   and busy merging!  Clear has one slot named clear with a
;;;             :   default value of t and it can be used by modules that need
;;;             :   a "clear" request without having to define it in the module.
;;;             : * All the default chunks now use the name slot to prevent 
;;;             :   possible merging.  The chunk-name is set in the slot to
;;;             :   avoid that.  It does change the fan of those items relative
;;;             :   to the older ACT-R versions if they're used in slots of DM
;;;             :   chunks and spreading activation is on, but the fan of those
;;;             :   things shouldn't have been important anyway.
;;; 2014.06.25 Dan
;;;             : * Change the definition for the type chunk since subtype info
;;;             :   isn't kept anymore.
;;;             : * Set the act-r-chunk-type-info-types list to (chunks) on a
;;;             :   reset.
;;; 2014.09.26 Dan
;;;             : * Instead of maphashing over global-modules-table dolist over
;;;             :   all-module-names because that guarantees ordering.
;;; 2014.11.07 Dan
;;;             : * Adding another default chunk-type to specify the default
;;;             :   query slots as valid: (chunk-type query-slots state buffer error).
;;; 2015.03.19 Dan
;;;             : * Resetting a model now needs to clear the buffer flags as well. 
;;;             : * Failure needs to be the name of a default chunk.
;;; 2015.09.09 Dan [3.0]
;;;             : * Model now keeps track of requests made to modules when asked
;;;             :   and can report on whether or not a request has "completed".
;;; 2015.12.16 Dan
;;;             : * All of the "completing" functions now return t as long as
;;;             :   there is a model and meta-process regardless of whether or 
;;;             :   not the params matched something previously uncompleted.
;;; 2017.03.30 Dan
;;;             : * Replaced meta-p-current-model usage with *current-act-r-model*.
;;; 2017.06.02 Dan
;;;             : * Cleaned up some issues with the "current model" setting and
;;;             :   use.
;;;             : * Delete-model needs to call the component delete hooks since
;;;             :   they may need access to the model's modules before they're 
;;;             :   gone.
;;; 2017.06.15 Dan
;;;             : * Lock access to the modules table.
;;;             : * Only evaluate current-model-struct once in current-model.
;;; 2017.06.20 Dan
;;;             : * Protect the chunk-type-info access with its lock.
;;;             : * Protect the chunk tables with a lock.
;;; 2017.06.21 Dan
;;;             : * Protect the 'chunk updateing" slots with a lock.
;;; 2017.06.22 Dan
;;;             : * Protecting meta-p-models and meta-p-events with the lock.
;;; 2017.06.29 Dan
;;;             : * Lock access to the global buffer-table and the model's buffers.
;;;             : * Protect the access to the global parameters table.
;;;             : * Changed valid-model-name to return the struct on success and
;;;             :   adjusted the with-model-* commands accordingly to avoid the
;;;             :   double lock and gethash.
;;; 2017.07.13 Dan
;;;             : * Lock the buffer itself when modifying things during reset.
;;; 2017.07.14 Dan
;;;             : * Protect access to the meta-p-component-list.
;;; 2017.08.24 Dan
;;;             : * Add signals for the three model reset points.
;;; 2017.08.29 Dan
;;;             : * Fixed copy and paste error with last update that only called
;;;             :   reset-step1 3 times...
;;; 2017.11.01 Dan
;;;             : * Adding a switch for :standalone that doesn't invoke the 
;;;             :   debugger when running the standalone version for an error
;;;             :   in the model definition because if that reload happens in
;;;             :   another thread (called from Tcl/Tk or Python) then CCL can
;;;             :   get stuck with the "type (:y #)" to switch to that process
;;;             :   message but not let you actually type it...
;;; 2017.11.03 Dan
;;;             : * Still want to print the error message from the standalone 
;;;             :   with the previous change.
;;; 2017.12.07 Dan
;;;             : * Starting work needed to allow for remote modules.
;;; 2017.12.08 Dan
;;;             : * Added remote versions of the complete request functions.
;;;             : * Added a remote current-model, but not sure that it'll really
;;;             :   do the 'right' thing.
;;; 2018.02.05 Dan
;;;             : * Don't directly access the meta-process events.
;;; 2018.04.13 Dan
;;;             : * The buffer-lock is now recursive.
;;; 2018.04.24 Dan
;;;             : * Use top-level-load? to check before invoking the debugger
;;;             :   since can't invoke it from a dispatched thread.
;;; 2018.06.08 Dan
;;;             : * Tracking a request now returns an id instead of the chunk-spec
;;;             :   since that's safe to use through the dispatcher.
;;; 2018.06.13 Dan
;;;             : * Updated the doc strings for remote commands and removed
;;;             :   a few verify-current-mp calls.
;;; 2018.07.26 Dan
;;;             : * Added remote request-completed-p.
;;; 2019.02.05 Dan
;;;             : * Adjustments because meta-p-models is now an alist.
;;; 2019.02.13 Dan
;;;             : * The model now holds a bitvector indicating which buffers
;;;             :   currently have chunks in them.
;;; 2019.03.13 Dan
;;;             : * The act-r-chunk-type-info-slot->mask table isn't used anymore.
;;; 2019.04.03 Dan
;;;             : * Store the instance of the module in the buffer struct when
;;;             :   creating a model and also in a list to use upon reset (so
;;;             :   it doesn't need to pull them from the hash-table).
;;; 2019.11.08 Dan
;;;             : * Moved the *define-model-lock* to the meta-process file and
;;;             :   added a global variable to go with it that indicates it's
;;;             :   currently inside a model manipulation function to allow the
;;;             :   running cmds to check and not run in the body of a model.
;;; 2019.11.20 Dan
;;;             : * Need to clear the act-r-chunk-type-info-query-slots list
;;;             :   when resetting to maintain consistency since they're also
;;;             :   being added to the slot table too.
;;; 2020.06.01 Dan [4.0]
;;;             : * Allow chunk-types and chunks to be defined during module
;;;             :   creation functions and then cache all the chunk-type and
;;;             :   chunk information after creation for reapplication during
;;;             :   reset (much faster than redefining every time).
;;;             : * Any chunk defined at creation time is automatically marked
;;;             :   as immutable.
;;; 2021.06.03 Dan [4.1]
;;;             : * Set the reuse? flag for the buffers at init and reset and
;;;             :   create the initial chunk for the buffer.
;;; 2021.06.09 Dan
;;;             : * Don't actually create the initial chunk for the buffers
;;;             :   because that's a big performance hit for tasks that run a
;;;             :   non-learning short trial & reset approach (fan model in the
;;;             :   tutorial for example).
;;; 2021.07.16 Dan
;;;             : * Only set the reuse? flag if it's not a multi buffer.
;;;             : * Delete-model wasn't returning t on success.
;;; 2021.10.14 Dan [5.0]
;;;             : * Only record the modules and buffers that are actually
;;;             :   instantiated.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Model structure is not for use outside of the framework.
;;;
;;; Note possible unsafe condition with respect to threading because a run could
;;; be started after the define-model call checks.  Ignoring that for now because
;;; it's likely a low probability situation.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;; define-model
;;; 
;;; (defmacro define-model (name &body model-code))
;;; (defun define-model-fct (name model-code-list))
;;; 
;;; name a symbol that will be the name for the new model
;;; model-code any number of forms that will be evaluated for the model
;;; model-code-list a list of forms to evaluate for the model
;;; 
;;; define-model is used to create a new model in the current meta-process.
;;; 
;;; The name must not already be used for a model in the current meta-process. If the name is not a symbol or is already used to name a model in the current meta-process a warning will be displayed and the model will not be defined (the old model with that name will remain unchanged if one existed).
;;; 
;;; When a model is first defined the following sequence of events will occur:
;;; 
;;; - Create a new model with that name
;;; - with that new model as the current model
;;;    - create the default chunk-types 
;;;    - create the default chunks
;;;    - create a new instance of each module 
;;;        - call its create function if it exists
;;;        - call its reset function if it exists
;;;    - evaluate the forms of the model in the order provided
;;;   
;;; If a model is successfully created then its name is returned otherwise define-model returns nil.
;;; 
;;; Every model will need to have a call to define-model before issuing any of the model commands because there is no default model in a meta-process.  However, if one is working with only a single model then all that is necessary is to provide a name - it is not necessary to enclose all of the model code.
;;; 
;;; current-model
;;; 
;;; (defun current-model ())
;;; 
;;; current-model returns the name of the current model in the current meta-process or nil if there is no current model or no current meta-process.
;;; 
;;; delete-model
;;; 
;;; (defmacro delete-model (&optional model-name))
;;; (defun delete-model-fct (&optional model-name))
;;; 
;;; model-name a symbol that names a model
;;; 
;;; If model-name is not provided the name of the current-model is used.  
;;; 
;;; If model-name is the name of a model in the current meta-process then the following sequence of events will occur:
;;; 
;;;  - the model with that name is set to the current model
;;;  - all events generated by that model are removed from the event queue
;;;  - each module of the model is deleted
;;;  - the model is removed from the set of models in the current meta-process
;;; 
;;; If model-name is valid then t is returned.
;;; 
;;; If model-name is not valid or there is no current meta-process then a warning is printed, nothing is done and nil is returned.
;;; 
;;; with-model
;;; 
;;; (defmacro with-model (model-name &body body))
;;; (defun with-model-fct (model-name forms-list))
;;; 
;;; model-name a symbol that names a model in the current meta-process
;;; body any number of forms to execute
;;; forms-list a list of forms to execute
;;; 
;;; If model-name is the name of a model in the current meta-process then the forms are evaluated in order with the current model set to the one named by model-name.  The value of the last form evaluated is returned.
;;; 
;;; If model-name does not name a model in the current meta-process, or there is no current meta-process then none of the forms are evaluated, a warning is printed and nil is returned.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(declaim (ftype (function (t) t) id-to-chunk-spec))

(defun current-model ()
  (awhen (current-model-struct)
    (act-r-model-name it)))

(add-act-r-command "current-model" 'current-model "Get the name of the current model if there is one. No params.")

(defun largest-chunk-type-size ()
  (let ((info (act-r-model-chunk-types-info (current-model-struct))))
    (bt:with-recursive-lock-held ((act-r-chunk-type-info-lock info))
      (act-r-chunk-type-info-size info))))


(defvar *model-chunk-table-size* nil)
(defvar *model-chunk-table-rehash-threshold* nil)

(create-system-parameter :mcts :valid-test 'posnumornil :default-value nil :warning "positive number or nil"
                         :documentation "initial size of a model's chunk table" 
                         :handler (simple-system-param-handler *model-chunk-table-size*))

(create-system-parameter :mctrt :valid-test (lambda (x) (typep x '(or null (real 0 1)))) :default-value nil 
                                             :warning "a real number [0-1] or nil"
                         :documentation "rehash-threshold of a model's chunk table" 
                         :handler (simple-system-param-handler *model-chunk-table-rehash-threshold*))



(defmacro define-model (name &body model-code)
  `(define-model-fct ',name ',model-code))

(defun define-model-fct (name model-code-list)
  (verify-current-mp  
   "define-model called with no current meta-process."
   (bt:with-lock-held (*define-model-lock*)
     (unwind-protect
         (progn 
           (setf *defining-model* t)
           (cond ((not (symbolp name))
                  (print-warning "Model name must be a symbol, ~S is not valid.  No model defined." name))
                 ((null name)
                  (print-warning "Nil is not a valid name for a model.  No model defined."))
                 ((valid-model-name name)
                  (print-warning "~S is already the name of a model in the current meta-process.  Cannot be redefined." name))
                 
                 ((mp-running?)
                  (print-warning "Cannot define a model while the system is running."))
                 
                 (t
                  (when (some (lambda (x) (find x (symbol-name name))) (list #\$ #\{ #\} #\[ #\]))
                    (print-warning "Model names that contain any of the characters $, {, }, [, or ] may not work correctly with the ACT-R Environment."))
                  
                  (let ((new-model (make-act-r-model :name name))
                        (mp (current-mp)))
                    (let ((*current-act-r-model* new-model))
                      
                      (when (or *model-chunk-table-size* *model-chunk-table-rehash-threshold*)
                        (bt:with-recursive-lock-held ((act-r-model-chunk-lock new-model))
                          (if *model-chunk-table-size*
                              (if *model-chunk-table-rehash-threshold*
                                  (progn
                                    (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*))
                                    (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*)))
                                (progn
                                  (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size*))
                                  (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size*))))
                            (progn
                              (setf (act-r-model-chunks-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*))
                              (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*))))))
                      
                      
                      (add-new-model name new-model)
                      
                      (create-model-default-chunk-types new-model)
                      
                      (create-model-default-chunks)
                      
                      (bt:with-lock-held ((act-r-model-modules-lock new-model))
                        (dolist (module-name (all-module-names))
                          
                          (let ((i (instantiate-module module-name name)))
                            
                            (when i
                              (push-last i (act-r-model-module-instances new-model))
                              (setf (gethash module-name (act-r-model-modules-table new-model)) i))))
                        
                        ;; cache the starting chunk-type information
                        
                        (let ((info (act-r-model-chunk-types-info new-model)))
                          (bt:with-recursive-lock-held ((act-r-chunk-type-info-lock info))
                            (setf (act-r-model-starting-slot-vector new-model)
                              (copy-seq (act-r-chunk-type-info-index->slot info)))
                            
                            (setf (act-r-model-starting-ct-size new-model) (act-r-chunk-type-info-size info))
                            
                            (setf (act-r-model-starting-slot-structs new-model)
                              (let ((r nil))
                                (maphash (lambda (k v)
                                           (push-last (cons k v) r))
                                         (act-r-chunk-type-info-slot->index info))
                                r))
                            
                            (setf (act-r-model-starting-chunk-types new-model)
                              (mapcar (lambda (x)
                                        (cons x (deep-copy-act-r-chunk-type (gethash x (act-r-chunk-type-info-table info)))))
                                (act-r-chunk-type-info-types info)))

                            (setf (act-r-model-starting-distinct-types new-model) 
                              (maphash (lambda (k v)
                                         (cons k (copy-list v)))
                                       (act-r-chunk-type-info-distinct-types info)))))
                        
                        ;; cache the starting chunk information and make them all immutable
                        
                        (setf (act-r-model-initial-chunks new-model)
                          (let ((chunks nil))
                            (maphash (lambda (c s)
                                       (bt:with-recursive-lock-held ((act-r-chunk-lock s))
                                         (push (list c (act-r-chunk-documentation s) (act-r-chunk-filled-slots s) (act-r-chunk-slot-value-lists s))
                                               chunks)
                                         (setf (act-r-chunk-immutable s) t)))
                                     (act-r-model-chunks-table new-model))
                            chunks))
                        
                        ;; instantiate the buffers
                        
                        (bt:with-lock-held (*buffers-table-lock*) 
                          (bt:with-lock-held ((act-r-model-buffers-lock new-model))
                            
                            (maphash (lambda (buffer-name buffer-struct)
                                       (let ((module (gethash (act-r-buffer-module buffer-struct) (act-r-model-modules-table new-model))))
                                         (when module
                                           (let ((buffer (copy-act-r-buffer buffer-struct)))
                                             
                                             (setf (act-r-buffer-module-instance buffer)
                                               (gethash (act-r-buffer-module buffer) (act-r-model-modules-table new-model)))
                                             
                                             (when (act-r-buffer-multi buffer)
                                               (setf (act-r-buffer-chunk-set buffer) (make-hash-table :test 'eq :size 5)))
                                             
                                             (dolist (x (act-r-buffer-requests buffer))
                                               (add-request-parameter x)
                                               (setf (act-r-buffer-requests-mask buffer) (logior (slot-name->mask x) (act-r-buffer-requests-mask buffer))))
                                             
                                             (dolist (x (act-r-buffer-queries buffer))
                                               (add-buffer-query x))
                                             
                                             (unless (act-r-buffer-multi buffer)
                                               (setf (act-r-buffer-reuse? buffer) t))
                                             (setf (act-r-buffer-reuse-chunk buffer) nil) 
                                             
                                             (setf (gethash buffer-name (act-r-model-buffers new-model)) buffer)))))
                                     
                                     *buffers-table*))))
                      
                      (dolist (m (act-r-model-module-instances new-model))
                        (reset-module m))
                      
                      #|;; Just set all the reusable slots to nil above instead of setting
                           them here (here because naming module needs to be reset) since
                           it's an added cost to do this for a buffer that doesn't get a chunk in the
                           model

                      (bt:with-lock-held ((act-r-model-buffers-lock new-model))
                        
                        (maphash (lambda (buffer-name buffer-struct)
                                   (let ((name (new-name-fct (format nil "~s-buffer-chunk" buffer-name))))
                                     (define-chunks-fct (list name))
                                     (make-chunk-reusable name)
                                     (setf (act-r-buffer-reuse-chunk buffer-struct) name)))
                                 
                                 (act-r-model-buffers new-model)))
                      |#
                      
                      (maphash (lambda (parameter-name parameter)
                                 (when (gethash (act-r-parameter-owner parameter) (act-r-model-modules-table new-model))
                                   (push (list parameter-name (act-r-parameter-default parameter)) (act-r-model-params new-model))
                                   (sgp-fct (list parameter-name (act-r-parameter-default parameter)))))
                               (bt:with-lock-held (*parameters-table-lock*) *act-r-parameters-table*))
                      
                      
                      (dolist (m (act-r-model-module-instances new-model))
                        (secondary-reset-module m))
                      
                      
                      (let ((errored nil))
                        (dolist (form model-code-list)
                          (unwind-protect 
                              (handler-case (eval form)
                                (error (condition) 
                                  (setf errored t)
                                  (print-warning "Error encountered in model form:~%~S~%" form)
                                  
                                  #-:standalone (let ((sub-load (not (top-level-load?))))
                                                  (if sub-load
                                                      (print-warning "~/print-error-message/" condition)
                                                    (progn
                                                      (print-warning "Invoking the debugger.")
                                                      (print-warning "You must exit the error state to continue.")
                                                      (invoke-debugger condition))))
                                  
                                  #+:standalone (print-warning "~/print-error-message/" condition)
                                  ))
                            (when errored
                              (remove-model name)
                              (print-warning "Model ~s not defined." name)
                              
                              ;; delete any events that may have been scheduled by modules
                              ;; or code prior to the error
                              
                              (delete-all-model-events mp name)
                              
                              
                              ;; remove the modules which were created
                              
                              (dolist (module-name (hash-table-keys (act-r-model-modules-table new-model)))
                                (delete-module module-name))
                              
                              (return-from define-model-fct nil)))))
                      
                      (setf (act-r-model-code new-model) model-code-list)
                      
                      (dolist (m (act-r-model-module-instances new-model))
                        (tertiary-reset-module m)))
                    
                    
                    name))))
       (setf *defining-model* nil)))))
  
(defmacro unsafe-define-model (name &body model-code)
  `(unsafe-define-model-fct ',name ',model-code))

(defun unsafe-define-model-fct (name model-code-list)
  (verify-current-mp  
   "define-model called with no current meta-process."
   (bt:with-lock-held (*define-model-lock*)
     (unwind-protect
         (progn 
           (setf *defining-model* t)
           
           (cond ((not (symbolp name))
                  (print-warning "Model name must be a symbol, ~S is not valid.  No model defined." name))
                 ((null name)
                  (print-warning "Nil is not a valid name for a model.  No model defined."))
                 ((valid-model-name name)
                  (print-warning "~S is already the name of a model in the current meta-process.  Cannot be redefined." name))
                 ((mp-running?)
                  (print-warning "Cannot define a model while the system is running."))
                 
                 (t
                  (when (some (lambda (x) (find x (symbol-name name))) (list #\$ #\{ #\} #\[ #\]))
                    (print-warning "Model names that contain any of the characters $, {, }, [, or ] may not work correctly with the ACT-R Environment."))
                  
                  (let ((new-model (make-act-r-model :name name)))
                    (let ((*current-act-r-model* new-model))
                      (when (or *model-chunk-table-size* *model-chunk-table-rehash-threshold*)
                        (bt:with-recursive-lock-held ((act-r-model-chunk-lock new-model))
                          (if *model-chunk-table-size*
                              (if *model-chunk-table-rehash-threshold*
                                  (progn
                                    (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*))
                                    (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size* :rehash-threshold *model-chunk-table-rehash-threshold*)))
                                (progn
                                  (setf (act-r-model-chunks-table new-model) (make-hash-table :size *model-chunk-table-size*))
                                  (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :size *model-chunk-table-size*))))
                            (progn
                              (setf (act-r-model-chunks-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*))
                              (setf (act-r-model-chunk-ref-table new-model) (make-hash-table :rehash-threshold *model-chunk-table-rehash-threshold*))))))
                      
                      
                      (add-new-model name new-model)
                      
                      (create-model-default-chunk-types new-model)
                      
                      (create-model-default-chunks)
                      
                      (bt:with-lock-held ((act-r-model-modules-lock new-model))
                        (dolist (module-name (all-module-names))
                          (let ((i (instantiate-module module-name name)))
                            
                            (when i
                              (push-last i (act-r-model-module-instances new-model))
                              (setf (gethash module-name (act-r-model-modules-table new-model)) i))))
                        
                        ;; cache the starting chunk-type information
                        
                        (let ((info (act-r-model-chunk-types-info new-model)))
                          (bt:with-recursive-lock-held ((act-r-chunk-type-info-lock info))
                            (setf (act-r-model-starting-slot-vector new-model)
                              (copy-seq (act-r-chunk-type-info-index->slot info)))
                            
                            (setf (act-r-model-starting-ct-size new-model) (act-r-chunk-type-info-size info))
                            
                            (setf (act-r-model-starting-slot-structs new-model)
                              (let ((r nil))
                                (maphash (lambda (k v)
                                           (push-last (cons k v) r))
                                         (act-r-chunk-type-info-slot->index info))
                                r))
                            
                            (setf (act-r-model-starting-chunk-types new-model)
                              (mapcar (lambda (x)
                                        (cons x (deep-copy-act-r-chunk-type (gethash x (act-r-chunk-type-info-table info)))))
                                (act-r-chunk-type-info-types info)))

                            (setf (act-r-model-starting-distinct-types new-model) 
                              (maphash (lambda (k v)
                                         (cons k (copy-list v)))
                                       (act-r-chunk-type-info-distinct-types info)))))
                        
                        ;; cache the starting chunk information and make them all immutable
                        
                        (setf (act-r-model-initial-chunks new-model)
                          (let ((chunks nil))
                            (maphash (lambda (c s)
                                       (bt:with-recursive-lock-held ((act-r-chunk-lock s))
                                         (push (list c (act-r-chunk-documentation s) (act-r-chunk-filled-slots s) (act-r-chunk-slot-value-lists s))
                                               chunks)
                                         (setf (act-r-chunk-immutable s) t)))
                                     (act-r-model-chunks-table new-model))
                            chunks))
                        
                        ;; instantiate the buffers
                        
                        (bt:with-lock-held (*buffers-table-lock*) 
                          (bt:with-lock-held ((act-r-model-buffers-lock new-model))
                            
                            (maphash (lambda (buffer-name buffer-struct)
                                       (let ((module (gethash (act-r-buffer-module buffer-struct) (act-r-model-modules-table new-model))))
                                         (when module
                                           (let ((buffer (copy-act-r-buffer buffer-struct)))
                                             
                                             (setf (act-r-buffer-module-instance buffer)
                                               module)
                                             
                                             (when (act-r-buffer-multi buffer)
                                               (setf (act-r-buffer-chunk-set buffer) (make-hash-table :test 'eq :size 5)))
                                             
                                             (dolist (x (act-r-buffer-requests buffer))
                                               (add-request-parameter x)
                                               (setf (act-r-buffer-requests-mask buffer) (logior (slot-name->mask x) (act-r-buffer-requests-mask buffer))))
                                             
                                             (dolist (x (act-r-buffer-queries buffer))
                                               (add-buffer-query x))
                                             
                                             (unless (act-r-buffer-multi buffer)
                                               (setf (act-r-buffer-reuse? buffer) t))
                                             (setf (act-r-buffer-reuse-chunk buffer) nil)
                                             
                                             (setf (gethash buffer-name (act-r-model-buffers new-model)) buffer)))))
                                     
                                     *buffers-table*))))
                      
                      (dolist (m (act-r-model-module-instances new-model))
                        (reset-module m))
                      
                      #| ;; Create the reusable chunks for the buffers (needs to happen after naming module reset)
                      
                      (bt:with-lock-held ((act-r-model-buffers-lock new-model))
                        
                        (maphash (lambda (buffer-name buffer-struct)
                                   (let ((name (new-name-fct (format nil "~s-buffer-chunk" buffer-name))))
                                     (define-chunks-fct (list name))
                                     (make-chunk-reusable name)
                                     (setf (act-r-buffer-reuse-chunk buffer-struct) name)))
                                 
                                 (act-r-model-buffers new-model)))
                      |#
                      
                      (maphash (lambda (parameter-name parameter)
                                 (when (gethash (act-r-parameter-owner parameter) (act-r-model-modules-table new-model))
                                   (push (list parameter-name (act-r-parameter-default parameter)) (act-r-model-params new-model))
                                   (sgp-fct (list parameter-name (act-r-parameter-default parameter)))))
                               (bt:with-lock-held (*parameters-table-lock*) *act-r-parameters-table*))
                      
                      
                      (dolist (m (act-r-model-module-instances new-model))
                        (secondary-reset-module m))
                      
                      (dolist (form model-code-list)
                        (eval form))
                      
                      (setf (act-r-model-code new-model) model-code-list)
                      
                      (dolist (m (act-r-model-module-instances new-model))
                        (tertiary-reset-module m)))
                    
                    
                    
                    name))))
       (setf *defining-model* nil)))))
  

(defun create-model-default-chunk-types (model)
  (let ((info (act-r-model-chunk-types-info model))
        (c (make-act-r-chunk-type :name 'chunk :super-types (list 'chunk))))
    
    (bt:with-recursive-lock-held ((act-r-chunk-type-info-lock info))
      (setf (gethash 'chunk (act-r-chunk-type-info-table info)) c)
      (setf (gethash 0 (act-r-chunk-type-info-distinct-types info)) (list (cons nil (list 'chunk))))
      (setf (act-r-chunk-type-info-types info) (list 'chunk))))
  
  (chunk-type constant-chunks name)
  (chunk-type clear (clear t))
  (chunk-type query-slots state buffer error))


(defun create-model-default-chunks ()
  
  (dolist (x '(free busy error empty full failure requested unrequested))
    (define-chunks-fct `((,x name ,x))))
  
  (define-chunks (clear isa clear)))



(defmacro delete-model (&optional (model-name nil provided))
  `(if ,provided
       (delete-model-fct ',model-name)
     (delete-model-fct (current-model))))
  
(defun delete-model-fct (model-name)
  (verify-current-mp  
   "delete-model called with no current meta-process.~%No model deleted."
   (let ((mp (current-mp)))
     (if model-name
         (aif (cdr (assoc model-name (bt:with-lock-held ((meta-p-models-lock mp)) (meta-p-models mp))))
              (progn
                (bt:with-lock-held (*define-model-lock*)
                  (unwind-protect
                      (let ((*current-act-r-model* it))
                        (setf *defining-model* t)
                        
                        (delete-all-model-events mp model-name) 
                        
                        (dolist (c (bt:with-lock-held ((meta-p-component-lock (current-mp))) (meta-p-component-list (current-mp))))
                          (when (act-r-component-model-destroy (cdr c))
                            (funcall (act-r-component-model-destroy (cdr c)) (act-r-component-instance (cdr c)) model-name)))
                        
                        (unwind-protect 
                            (dolist (module-name (hash-table-keys (act-r-model-modules-table it)))
                              (delete-module module-name))
                          
                          (remove-model model-name)))
                    t)
                  (setf *defining-model* nil))
                t)
           (print-warning "No model named ~S in current meta-process." model-name))
       (print-warning "No current model to delete.")))))

(defmacro with-model (model-name &body body)
  (let ((mp (gensym))
        (model (gensym)))
    `(let ((,mp (current-mp)))
       (if ,mp
           (let ((,model (valid-model-name ',model-name)))
             (if ,model 
                 (let ((*current-act-r-model* ,model))
                   ,@body)
               (print-warning "~S does not name a model in the current meta-process" ',model-name)))
         (print-warning "No actions taken in with-model because there is no current meta-process")))))

(defmacro with-model-eval (model-name &body body)
  (let ((mp (gensym))
        (model (gensym)))
    `(let ((,mp (current-mp)))
       (if ,mp
           (let ((,model (valid-model-name ,model-name))) 
             (if ,model
                 (let ((*current-act-r-model* ,model))
                   ,@body)
               (print-warning "~S does not name a model in the current meta-process" ,model)))
         (print-warning "No actions taken in with-model because there is no current meta-process")))))

(defun with-model-fct (model-name forms-list)
  (let ((mp (current-mp)))
     (if mp
         (let ((model (valid-model-name model-name)))
           (if model
               (let ((*current-act-r-model* model)
                     (val nil))
                 (dolist (x forms-list val)
                   (setf val (eval x))))
             (print-warning "~S does not name a model in the current meta-process" model-name)))
       (print-warning "No actions taken in with-model because there is no current meta-process"))))


(defun valid-model-name (name)
    "Returns struct if name is the name of a model in the current meta-process - there must be a current mp"
  (cdr (assoc name (bt:with-lock-held ((meta-p-models-lock (current-mp))) (meta-p-models (current-mp))))))


(add-act-r-command "reset-step1" nil "Signal for the primary model reset point (before default parameters are set, modules primary resets, and code evaluated) for monitoring purposes.  No params." nil)
(add-act-r-command "reset-step2" nil "Signal for the secondary model reset point (after default parameters are set but before modules secondary resets and code evaluated) for monitoring purposes.  No params." nil)
(add-act-r-command "reset-step3" nil "Signal for the tertiary model reset point (after code evaluated but before model tertiary resets) for monitoring purposes.  No params." nil)

(defun reset-model (mp model)
  (declare (ignore mp))
  (let ((*current-act-r-model* model))
    
    (bt:with-lock-held (*define-model-lock*)
      (unwind-protect
          (let ((info (act-r-model-chunk-types-info model)))
            (setf *defining-model* t)
            
            ;; erase chunk-type info 
            (bt:with-recursive-lock-held ((act-r-chunk-type-info-lock info))
              (clrhash (act-r-chunk-type-info-slot->index info))
              
              (mapcar (lambda (x)
                        (setf (gethash (car x) (act-r-chunk-type-info-slot->index info))
                          (cdr x)))
                (act-r-model-starting-slot-structs model))
              
              
              (setf (act-r-chunk-type-info-index->slot info) (make-array (list 0) :adjustable t :fill-pointer t))
              
              (map nil (lambda (x) (vector-push-extend x (act-r-chunk-type-info-index->slot info)))
                (act-r-model-starting-slot-vector model))
              
              (setf (act-r-chunk-type-info-size info) (act-r-model-starting-ct-size model))
              (clrhash (act-r-chunk-type-info-distinct-types info))
              
              (dolist (x (act-r-model-starting-distinct-types model))
                (setf (gethash (car x) (act-r-chunk-type-info-distinct-types info))
                  (copy-list (cdr x))))
              
              (setf (act-r-chunk-type-info-extended-slots info) nil)
              (setf (act-r-chunk-type-info-query-slots info) nil)
              
              (clrhash (act-r-chunk-type-info-table info))
              (setf (act-r-chunk-type-info-types info) nil)
              
              (dolist (x (act-r-model-starting-chunk-types model))
                (push-last (car x) (act-r-chunk-type-info-types info))
                (setf (gethash (car x) (act-r-chunk-type-info-table info))
                  (deep-copy-act-r-chunk-type (cdr x)))))
            
            
            
            (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
              (setf (act-r-model-chunk-update model) t)
              (setf (act-r-model-dynamic-update model) t)
              (setf (act-r-model-delete-chunks model) nil))
            
            
            ;; initialize the tables and recreate the starting chunk structs 
            (bt:with-recursive-lock-held ((act-r-model-chunk-lock model))
                      
              (clrhash (act-r-model-chunks-table model))
              (clrhash (act-r-model-chunk-ref-table model))
              
              (bt:with-lock-held (*chunk-parameters-lock*)
                (dolist (c (act-r-model-initial-chunks model))
                  (setf (gethash (first c) (act-r-model-chunks-table model))
                    (make-act-r-chunk
                     :name (first c)
                     :merged-chunks (list (first c))
                     :documentation (second c)
                     :filled-slots (third c)
                     :slot-value-lists (fourth c)
                     :immutable t
                     :parameter-values (make-array *chunk-parameters-count*
                                                   :initial-element *chunk-parameter-undefined*)))))
              
              ;; update the back-links data 
             
              (dolist (c (act-r-model-initial-chunks model))
                (dolist (s (fourth c))
                  (when (gethash (cdr s) (act-r-model-chunks-table model))
                    (let ((bl (gethash (cdr s) (act-r-model-chunk-ref-table model))))
                      (if (hash-table-p bl)
                          (push (act-r-slot-name (car s)) (gethash (first c) bl))
                        (let ((ht (make-hash-table)))
                          (setf (gethash (first c) ht) (list (act-r-slot-name (car s))))
                          (setf (gethash (cdr s) (act-r-model-chunk-ref-table model)) ht))))))))
            
                        
            (bt:with-lock-held ((act-r-model-buffers-lock model))
              (setf (act-r-model-buffer-state model) 0)
              
              (maphash (lambda (buffer-name buffer)
                         (declare (ignore buffer-name))
                         (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
                           (setf (act-r-buffer-chunk buffer) nil)
                           (when (act-r-buffer-multi buffer)
                             (setf (act-r-buffer-chunk-set buffer) (make-hash-table :test 'eq :size 5)))
                           (dolist (x (act-r-buffer-requests buffer))
                             (add-request-parameter x)
                             (setf (act-r-buffer-requests-mask buffer) (logior (slot-name->mask x) (act-r-buffer-requests-mask buffer))))
                           (dolist (x (act-r-buffer-queries buffer))
                             (add-buffer-query x))
                           ;; clear any flags
                           (setf (act-r-buffer-flags buffer) nil)
                           
                           (unless (act-r-buffer-multi buffer)
                             (setf (act-r-buffer-reuse? buffer) t))
                           ;; Clear any previous name (will be created as needed)
                           (setf (act-r-buffer-reuse-chunk buffer) nil)
                           ))
                       (act-r-model-buffers model)))
            
            (dispatch-apply "reset-step1")
            
            (dolist (module-name (act-r-model-module-instances model))
              (reset-module module-name))
            
            #| ;; Create the reusable chunks for the buffers (needs to happen after naming module reset)
                      
            (bt:with-lock-held ((act-r-model-buffers-lock model))
              
              (maphash (lambda (buffer-name buffer-struct)
                         (let ((name (new-name-fct (format nil "~s-buffer-chunk" buffer-name))))
                           (define-chunks-fct (list name))
                           (make-chunk-reusable name)
                           (setf (act-r-buffer-reuse-chunk buffer-struct) name)))
                       
                       (act-r-model-buffers model)))
            |#
            
            ;; These are the default values
            
            (mapcar 'sgp-fct (act-r-model-params model))
            
            #|(maphash (lambda (parameter-name parameter)
                       (sgp-fct (list parameter-name (act-r-parameter-default parameter))))
                     (bt:with-lock-held (*parameters-table-lock*) *act-r-parameters-table*))
            |#
            (dispatch-apply "reset-step2")
            
            (dolist (module-name (act-r-model-module-instances model))
              (secondary-reset-module module-name))
            
            (dolist (form (act-r-model-code model))
              (eval form))
            
            (dispatch-apply "reset-step3")
            
            (dolist (module-name (act-r-model-module-instances model))
              (tertiary-reset-module module-name)))
        (setf *defining-model* nil)))))



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
