;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2016 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : history-recorder.lisp
;;; Version     : 2.2
;;; 
;;; Description : General history data recording mechanisms based on those which
;;;             : were used for the Environment tools previously.
;;;
;;; Bugs        : 
;;;
;;; To do       : [x] Need some way to break up large histories because the 
;;;             :     Environment currently dies when handling things like a
;;;             :     production grid that's a 3M+ json string...
;;; 
;;; ----- History -----
;;; 2016.04.15 Dan [1.0a1] 
;;;             : * Initial creation.
;;; 2016.04.21 Dan
;;;             : * Updates to make this actually usable...
;;; 2016.04.22 Dan
;;;             : * Adding the support for saving/loading data for the history
;;;             :   tools.  The approach is that a tool needs to specify a function
;;;             :   to "get" the data and should always call the get-history-information
;;;             :   function to access the data for display because that will
;;;             :   return the current or saved data as needed.
;;;             : * Adding code to support the text trace here since it doesn't
;;;             :   have its own file.
;;; 2016.05.03 Dan [1.0a2]
;;;             : * Adding support for removing the saved traces from the global
;;;             :   hash table to conserve memory.  The table now holds a count
;;;             :   of windows using it and it will decrement that when the window
;;;             :   closes (needs the signals from the environment) and can 
;;;             :   remhash it at 0.
;;; 2016.05.05 Dan
;;;             : * Allow get-history-information to work without a current model
;;;             :   when it's tabled data being accessed.
;;; 2016.05.12 Dan
;;;             : * Updated the save-history-information to better deal with
;;;             :   errors and report them back to the Environment.
;;; 2016.05.13 Dan
;;;             : * Similar update to load-history-information.
;;;             : * The text history output function can't use command-output
;;;             :   since there may not be a model when looking at saved data.
;;; 2016.05.17 Dan
;;;             : * Fixed a bug with load-history-information that didn't catch
;;;             :   bad data at the start of the loaded file.
;;; 2016.05.18 Dan
;;;             : * Fixed some issues with unused variables.
;;; 2016.05.19 Dan
;;;             : * Fixed an issue with loading a model while the previous model
;;;             :   had a select data window open already -- the new model ended
;;;             :   up with invalid settings sometimes because of the asynchrony
;;;             :   in closing the old window and defining the new model.  Now
;;;             :   the module's handler code checks to make sure it's the right  
;;;             :   model before initializing the data from the window's handler.
;;;             : * Better fix for that now since reload also had an issue if it
;;;             :   was the same model...
;;; 2016.06.01 Dan
;;;             : * When saving the history information also save the current
;;;             :   ACT-R version and then check that when loading.
;;; 2016.06.09 Dan
;;;             : * Added the clear-history-cache function which the environment
;;;             :   code can use to make sure everything is released when the
;;;             :   environment isn't connected anymore.
;;; 2017.08.11 Dan
;;;             : * Protect access to procedural-buffer-indices.
;;; 2017.09.15 Dan [2.0a1]
;;;             : * Moved this into the framework, and it doesn't need to
;;;             :   be a module now because it's just a bunch of functions.
;;;             :   The objective is to generalize this mechanism so that it
;;;             :   isn't just something tied to the Tcl/Tk Environment.
;;;             : * Don't worry about cacheing the data anymore, and basically 
;;;             :   just act as an intermediary for dealing with the data streams
;;;             :   and processors recorded.
;;; 2017.09.19 Dan
;;;             : * Removed the trace history stuff, added the standard macro/fct
;;;             :   split, and changed the top-level names to include history
;;;             :   instead of stream.
;;;             : * Added remote commands to access it.
;;; 2017.09.20 Dan
;;;             : * Make a component for the main recorder instead of a global
;;;             :   because it needs to be cleaned-up when a clear-all happens.
;;; 2017.09.21 Dan
;;;             : * Have the get and available check to see if there's a current
;;;             :   model when it's a model based data stream.
;;;             : * Status needs to return three things enabled, data, and alive.
;;;             :   The alive check is for when a model gets deleted because a
;;;             :   module may need to clear things itself upon deletion for
;;;             :   consistency (instead of assuming all starts will be stopped)
;;;             :   but doing so can result in warnings for 'external' changes
;;;             :   if the module cleans itself up and then the stops come in
;;;             :   (perhaps also driven by the delete model call).  If the
;;;             :   stream source is no longer available then it needs to say
;;;             :   it's no longer alive so the history code can skip the warnings
;;;             :   for disabling after that point.
;;; 2017.09.22 Dan
;;;             : * Fixed a bug with process-history-data because it needed to
;;;             :   pass the name of the stream to get-history-data.
;;; 2017.09.26 Dan
;;;             : * Make sure process-history-data returns the comment as a 
;;;             :   second value when data is loaded from a file.
;;; 2017.10.04 Dan
;;;             : * Allow names to be symbols or strings and convert to symbol
;;;             :   using string->symbol for storage.
;;;             : * Split all into a macro and function.
;;;             : * Allow record-history and stop-recording-history to take an 
;;;             :   arbitrary number of items.
;;;             : * Allow get, available, record, and stop to take a processor name.
;;;             :   Because of that, just switch to one table and always check
;;;             :   the type of the retrieved thing.
;;;             : * Change process-history-data to take two lists of params: one
;;;             :   for the history stream and one for the processor.  If get-
;;;             :   history is used for a processor then the parameters are only
;;;             :   passed to the stream and the processor is assumed to only take
;;;             :   the data as a parameter.
;;; 2017.11.14 Dan
;;;             : * Fixed a bug in stop-recording-history when an invalid name
;;;             :   is given.
;;; 2017.11.21 Dan
;;;             : * Save-history-data was defined as a function instead of a 
;;;             :   macro.
;;; 2017.11.27 Dan 
;;;             : * Added a first pass at an incremental data system since the
;;;             :   Environment chokes on "big" packets, like those for the 
;;;             :   production graphs and graphic traces which can be several
;;;             :   MB for a run of something like past-tense or zbrodoff
;;;             :   respectively.
;;; 2018.02.28 Dan [2.0]
;;;             : * Changed the ordering of the parameters for -processor so that
;;;             :   the history is first and the processor's name second to be
;;;             :   the same as -component.
;;;             : * Make remote versions of all the commands.
;;;             : * Require that the names be strings.
;;;             : * Pass the rest parameters from get-history-data to the status
;;;             :   command now.
;;; 2018.03.01 Dan
;;;             : * Fix a bug with passing the get-history-data params to the
;;;             :   status command.
;;;             : * Force everthing to require names be strings.
;;;             : * Return multiple values from record and stop-recording with
;;;             :   each value indicating the result of the corresponding param.
;;; 2018.03.02 Dan
;;;             : * Renamed history-component to history-constituent because 
;;;             :   component is already used to describe something...
;;;             : * NOPE! Don't require names to be strings because stuff that
;;;             :   comes in remotely could get converted from string to symbol
;;;             :   automatically and don't want to 'fix' all the Environment
;;;             :   code.
;;; 2018.06.13 Dan
;;;             : * Updated the command doc strings to match current spec.
;;;             : * Histories must be named with strings now.
;;; 2018.08.08 Dan
;;;             : * Fixed a bug with save-history-data in how it creates the
;;;             :   call to get data when parameters are given.
;;; 2018.08.22 Dan
;;;             : * Process-history-data should also return the comment from a
;;;             :   file of history data as the second result.
;;; 2019.05.29 Dan
;;;             : * Turn off file access in single threaded mode because there
;;;             :   isn't a JSON parser loaded so data will be in the original
;;;             :   Lisp format and don't want to generate incompatible files.
;;; 2020.01.13 Dan [2.1]
;;;             : * Removed the #' and lambdas from the module interface. 
;;; 2022.03.10 Dan [2.2]
;;;             : * Make this a required module.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; Provide a common mechanism for recording and saving history information.
;;; Originally designed around the needs of the Environment tools, but then
;;; generalized into something that's useful for all of the underlying data
;;; recording capabilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; See reference manual.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Use JSON as the underlying data format because that's what the RPC system
;;; uses, so the processing of that is already available.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(declaim (ftype (function (t &optional t) t) written-for-act-r-version))

(defstruct history-module 
  (state-table (make-hash-table)) ; nil, :enabled, :disabled
  (requests-table (make-hash-table))) ; nil or #

(defstruct history-recorder 
  (lock (bt:make-recursive-lock "history recorder")) 
  (module (make-history-module))
  (streams-table (make-hash-table))
  (current-id 0)
  (incremental-table (make-hash-table)))


(defun create-history-recorder () (make-history-recorder))

(defun clear-all-history-state (component)
  (bt:with-recursive-lock-held ((history-recorder-lock component))
    (setf (history-recorder-module component) (make-history-module))
    (clrhash (history-recorder-incremental-table component))))

(define-component history-recorder :version "2.0" :documentation "Maintain the tables of defined history recorders and state of non-model based history streams."
  :creation create-history-recorder :clear-all clear-all-history-state)


(defstruct data-stream
  parent
  sub-streams
  internal-name
  external-name
  enable
  disable
  status
  get
  model-based)

(defstruct data-processor
  internal-name
  external-name
  stream
  get)


(defmacro define-history (name enable disable status get &optional (model-based t))
  `(define-history-fct ',name ',enable ',disable ',status ',get ',model-based))

(defun define-history-fct (name enable disable status get &optional (model-based t))
  (cond ((not (stringp name))
         (print-warning "Name for a history source must be a string, but given ~s." name))
        ((not (local-or-remote-function-p enable))
         (print-warning "Enable function for a history source must be a valid command, but given ~s." enable))
        ((not (local-or-remote-function-p disable))
         (print-warning "Disable function for a history source must be a valid command, but given ~s." disable))
        ((not (local-or-remote-function-p status))
         (print-warning "Status function for a history source must be a valid command, but given ~s." status))
        ((not (local-or-remote-function-p get))
         (print-warning "Get data function for a history source must be a valid command, but given ~s." get))
        (t
         (let ((i-name (string->name name)) ;; use a symbol internally
               (history-recorder (get-component history-recorder)))
           (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
             (if (gethash i-name (history-recorder-streams-table history-recorder))
                 (print-warning "Cannot create a history source with name ~s because it is already used." name)
               (progn
                 (setf (gethash i-name (history-recorder-streams-table history-recorder))
                   (make-data-stream
                    :internal-name i-name
                    :external-name name
                    :enable enable
                    :disable disable
                    :status status
                    :get get
                    :model-based model-based))
                 t)))))))


(add-act-r-command "define-history" 'define-history-fct "Create a new history source recorder. Params: name enable-cmd disable-cmd status-cmd get-cmd {model-based}")

(defmacro define-history-constituent (history name enable disable status)
  `(define-history-constituent-fct ',history ',name ',enable ',disable ',status))

(defun define-history-constituent-fct (history name enable disable status)
  (cond ((not (stringp name))
         (print-warning "Name for a history stream constituent must be a string, but given ~s." name))
        ((not (stringp history))
         (print-warning "Name for a history stream must be a string, but given ~s." history))
        ((not (local-or-remote-function-p enable))
         (print-warning "Enable function for a history stream constituent must be a valid command, but given ~s." enable))
        ((not (local-or-remote-function-p disable))
         (print-warning "Disable function for a history stream constituent must be a valid command, but given ~s." disable))
        ((not (local-or-remote-function-p status))
         (print-warning "Status function for a history stream constituent must be a valid command, but given ~s." status))
        (t
         (let ((i-name (string->name name))
               (i-history (string->name history))
               (history-recorder (get-component history-recorder)))
           (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
             (if (gethash i-name (history-recorder-streams-table history-recorder))
                 (print-warning "Cannot create a history stream constituent with name ~s because it is already used." name)
               (let ((s (gethash i-history (history-recorder-streams-table history-recorder))))
                 (if (and s (null (data-stream-parent s)))
                     (let ((sub (make-data-stream
                                 :internal-name i-name
                                 :external-name name
                                 :enable enable
                                 :disable disable
                                 :status status
                                 :parent s
                                 :model-based (data-stream-model-based s))))
                       (setf (gethash i-name (history-recorder-streams-table history-recorder)) sub)
                       (push sub (data-stream-sub-streams s))
                       t)
                   (print-warning "Cannot create a history source constituent with name ~s for history source ~s because that source does not exist." name history)))))))))

(add-act-r-command "define-history-constituent" 'define-history-fct "Create a new constituent part for a history source. Params: history name enable-cmd disable-cmd status-cmd")

(defmacro define-history-processor (history name get)
  `(define-history-processor-fct ',history ',name ',get))
  
(defun define-history-processor-fct (history name get)
  (cond ((not (stringp name))
         (print-warning "Name for a history source processor must be a string, but given ~s." name))
        ((not (stringp history))
         (print-warning "Name for a history source must be a string, but given ~s." history))
        ((not (local-or-remote-function-p get))
         (print-warning "Processor function for a history source processor must be a valid command, but given ~s." get))
        (t
         (let ((i-name (string->name name))
               (i-history (string->name history))
               (history-recorder (get-component history-recorder)))
           (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
             (let ((s (gethash i-history (history-recorder-streams-table history-recorder))))
               (if (and s (data-stream-p s) (null (data-stream-parent s)))
                   (if (gethash i-name (history-recorder-streams-table history-recorder))
                       (print-warning "Cannot create a history source processor with name ~s because that is already the name of a history source item." name)
                     (progn
                         (setf (gethash i-name (history-recorder-streams-table history-recorder))
                           (make-data-processor
                            :internal-name i-name
                            :external-name name
                            :stream s
                            :get get))
                         t))
                 (print-warning "Cannot create a history source processor named ~s for source ~s because that source is ~:[not defined~;a component of another stream or a processor~]."
                                name history s))))))))

(add-act-r-command "define-history-processor" 'define-history-processor-fct "Create a new data processor for a history source. Params: history processor-name process-cmd")

(defmacro history-data-available (name &rest rest)
  `(apply 'history-data-available-fct ',name ',rest))

(defun history-data-available-fct (name &rest rest)
  (if (not (stringp name))
      (print-warning "History-data-available given name ~s but names must be strings." name)
    (let ((history-recorder (get-component history-recorder)))
      (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
        (setf name (string->name name))
        (let* ((start (gethash name (history-recorder-streams-table history-recorder)))
               (s (cond ((data-stream-p start)
                         (aif (data-stream-parent start) it start))
                        ((data-processor-p start)
                         (data-processor-stream start)))))
          (if s
              (if (and (data-stream-model-based s) (null (current-model)))
                  (print-warning "Model based history ~a requires a current model to check for available data." name)
                (multiple-value-bind (enabled data)
                    (dispatch-apply-list (data-stream-status s) rest)
                  (declare (ignore enabled))
                  (when data t)))
            (print-warning "~s does not name a history data item in call to history-data-available." name)))))))

(add-act-r-command "history-data-available" 'history-data-available-fct "Return whether there is any data available for the indicated history stream. Params: history-name {param*}")

(defmacro get-history-data (name &optional file &rest rest)
  `(apply 'get-history-data-fct ',name ',file ',rest))

(defun get-history-data-fct (name &optional file &rest rest)
  #+:single-threaded-act-r (when file 
                             (print-warning "History file access unavailable in single-threaded model.")
                             (return-from get-history-data-fct))
  (if (not (stringp name))
      (print-warning "Get-history-data given name ~s but names must be strings." name)
    (let ((history-recorder (get-component history-recorder)))
      (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
        (setf name (string->name name))
        (let ((s (gethash name (history-recorder-streams-table history-recorder))))
          
          (cond ((and (data-stream-p s) (data-stream-parent s))
                 (apply 'get-history-data-fct (data-stream-external-name (data-stream-parent s)) file rest))
                ((data-processor-p s)
                 (process-history-data-fct (data-processor-external-name s) file rest nil))
                ((data-stream-p s)
                 (if file
                     (multiple-value-bind (result err) (ignore-errors (probe-file file))
                       (if (subtypep (type-of err) 'condition)   
                           (print-warning "Error ~/print-error-message/ encountered while trying to open file ~s for get-history-data." err file)
                         (if result
                             (multiple-value-bind (d c) (parse-saved-history-data (data-stream-internal-name s) result)
                               (if d
                                   (if c
                                       (values d c)
                                     d)
                                 (print-warning "File ~s does not contain valid data for history data source ~s in attempt get-history-data." file name)))
                           (print-warning "File ~s could not be found in attempt to get-history-data for ~s." file name))))
                   
                   (if (and (data-stream-model-based s) (null (current-model)))
                       (print-warning "Model based history ~a requires a current model to get data." name)
                     
                     (multiple-value-bind (enabled data alive)
                         (dispatch-apply-list (data-stream-status s) rest)
                       (if data
                           (let ((d (dispatch-apply-list (data-stream-get s) rest)))
                             (if (and d (listp d)
                                      (every (lambda (x)
                                               (and (listp x)
                                                    (numberp (first x))))
                                             d))
                                 (multiple-value-bind (result err)
                                     (ignore-errors (json:encode-json-to-string d))
                                   (if (subtypep (type-of err) 'condition)   
                                       (print-warning "Error ~/print-error-message/ encountered converting data from ~s to JSON." err d)
                                     result))
                               (print-warning "Get-history-data for history data source ~s with parameters ~s returned invalid data ~s." name rest d)))
                         (let ((m (if (data-stream-model-based s)
                                      (suppress-warnings (get-module :history-recorder))
                                    (history-recorder-module history-recorder))))
                           (if m
                               (let ((state (gethash (data-stream-internal-name s) (history-module-state-table m))))
                                 (cond ((null alive)
                                        (print-warning "There is no data available for history data source ~s because the source is no longer available." name))
                                       ((and (null enabled) (null state))
                                        (print-warning "There is no data available for history data source ~s because recording was never started." name))
                                       ((and (null enabled) (eq state :enabled))
                                        (print-warning "There is no data available for history data source ~s and although recording was started it has since been disabled through other means." name))
                                       ((and (null enabled) (eq state :was-enabled))
                                        (print-warning "There is no data available for history data source ~s and it appears that recording was not started because it had been enabled through other means and has since been disabled through other means." name))
                                       ((and (null enabled) (eq state :disabled))
                                        (print-warning "There is no data available for history data source ~s and recording was already stopped." name))
                                       (enabled
                                        (print-warning "There is no data available for history data source ~s but the recording is currently enabled." name))
                                       (t 
                                        (print-warning "There is no data available for history data source ~s and no indication as to why." name))))
                             (print-warning "There is no data available for history data source ~s and there is no current model to determine the reason why." name))))))))
                (t
                 (print-warning "~s does not name a history data source in call to get-history-data." name))))))))

(add-act-r-command "get-history-data" 'get-history-data-fct "Return the indicated history stream data from the specfied file or current data using indicated parameters if no file given. Params: history-name {file-name-or-nil} {additional-parameters}*")

(defmacro process-history-data (name &optional file data-params processor-params)
  `(process-history-data-fct ',name ',file ',data-params ',processor-params))

(defun process-history-data-fct (name &optional file data-params processor-params)
  #+:single-threaded-act-r (when file 
                             (print-warning "History file access unavailable in single-threaded model.")
                             (return-from process-history-data-fct))
  (if (not (stringp name))
      (print-warning "Process-history-data given name ~s but names must be strings." name)
    (let ((history-recorder (get-component history-recorder)))
      (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
        (setf name (string->name name))
        (let ((p (gethash name (history-recorder-streams-table history-recorder))))
          (if (data-processor-p p)
              (multiple-value-bind (data comment) (apply 'get-history-data-fct (data-stream-external-name (data-processor-stream p)) file data-params)
                (if data
                    (let ((d (dispatch-apply-list (data-processor-get p) (append (list data) processor-params))))
                      (multiple-value-bind (result err)
                          (ignore-errors (json:encode-json-to-string d))
                        (if (subtypep (type-of err) 'condition)   
                            (print-warning "Error ~/print-error-message/ encountered converting data from ~s to JSON." err d)
                          (values result comment))))
                  (print-warning "No history data available from the source for processor ~a" name)))
            (print-warning "~a does not name a history data processor in call to process-history-data" name)))))))

(add-act-r-command "process-history-data" 'process-history-data-fct "Return the result of applying the named processor to the history data from the specfied file or current data using indicated parameters if no file given. Params: processor-name {file-name-or-nil} {additional-parameters}*")


(defun start-incremental-history-data-fct (name size &optional file &rest rest)
  #+:single-threaded-act-r (when file 
                             (print-warning "History file access unavailable in single-threaded model.")
                             (return-from start-incremental-history-data-fct))
  (if (not (stringp name))
      (print-warning "Start-incremental-history-data given name ~s but names must be strings." name)
    (let ((history-recorder (get-component history-recorder)))
      (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
        
        (if file
            (multiple-value-bind (results comment) (apply 'get-history-data-fct (append (list name) (list (if file file nil)) rest))
              (if results
                  (let* ((id (incf (history-recorder-current-id history-recorder)))
                         (data (json:decode-json-from-string results))
                         (segments (ceiling (length results) size))
                         (step-size (max 1 (floor (length data) segments))))
                    (setf (gethash id (history-recorder-incremental-table history-recorder))
                      (list :file step-size (if comment comment "") data))
                    id)
                nil))
          
          (let ((results (apply 'get-history-data-fct name file rest)))
            (if results
                (let* ((id (incf (history-recorder-current-id history-recorder)))
                       (data (json:decode-json-from-string results))
                       (segments (ceiling (length results) size))
                       (step-size (max 1 (floor (length data) segments))))
                  (setf (gethash id (history-recorder-incremental-table history-recorder))
                    (list :data step-size data))
                  id)
              nil)))))))

(add-act-r-command "start-incremental-history-data" 'start-incremental-history-data-fct "Returns an id for incrementally acquiring the history data from the specfied file or current data using the indicated parameters if no file given in segements roughly the length specified. Params: processor-name byte-size-suggestion {file-name-or-nil} {additional-parameters}*")

(defun get-incremental-history-data-fct (id)
  (let ((history-recorder (get-component history-recorder)))
    (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
      (multiple-value-bind (item exists) (gethash id (history-recorder-incremental-table history-recorder))
        (if exists
            (if (eq (car item) :file)
                (let* ((step (second item))
                       (comment (third item))
                       (data (fourth item))
                       (send (json:encode-json-to-string comment)))
                  (setf (gethash id (history-recorder-incremental-table history-recorder)) (list :data step data))
                  send)
              (let* ((step (second item))
                     (data (third item))
                     (end (min step (length data)))
                     (sub (subseq data 0 end))
                     (send (json:encode-json-to-string sub))
                     (remain (when (<= end (length data)) (subseq data end))))
                (if data
                    (setf (gethash id (history-recorder-incremental-table history-recorder)) (list :data step remain))
                  (remhash id (history-recorder-incremental-table history-recorder))
                  )
                send))
          (print-warning "Invalid id ~s given for get-incremental-history-data." id))))))

(add-act-r-command "get-incremental-history-data" 'get-incremental-history-data-fct "Return the next segment from the history data associated with the given id or an empty list if there is no more data. Params: id")

(defmacro save-history-data (name file &optional comment &rest rest)
  `(save-history-data-fct ',name ',file ',comment ',@rest))

(defun save-history-data-fct (name file &optional comment &rest rest)
  #+:single-threaded-act-r (when file 
                             (print-warning "History file access unavailable in single-threaded model.")
                             (return-from save-history-data-fct))
  (if (not (stringp name))
      (print-warning "Save-history-data given name ~s but names must be strings." name)
    (let ((history-recorder (get-component history-recorder)))
      (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
        (setf name (string->name name))
        (let* ((start (gethash name (history-recorder-streams-table history-recorder)))
               (s (cond ((data-stream-p start)
                         (aif (data-stream-parent start) it start))
                        ((data-processor-p start)
                         (data-processor-stream start)))))
          (if s
              (let ((data (apply 'get-history-data-fct (append (list (data-stream-external-name s) nil) rest))))
                (if data
                    (handler-case 
                        (progn
                          (with-open-file (f file :direction :output :if-does-not-exist :create :if-exists :supersede)
                            (format f "{\"name\": \"~a\",~%" (data-stream-internal-name s))
                            (multiple-value-bind (second minute hour date month year) (get-decoded-time)
                              (format f "\"date\": \"~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d\",~%"  year month date hour minute second))
                            (format f "\"version\": ~s,~%" (act-r-version-string))
                            (if (stringp comment)
                                (format f "\"comment\": ~s,~%" comment)
                              (print-warning "A comment for save-history-data must be a string but given ~s which will be ignored." comment))
                            (format f "\"data\": ~s~%}" data))
                          t)
                      (error (x) 
                        (print-warning "Error ~/print-error-message/ occurred while trying to save history ~s to file ~s." x name file)))
                  (print-warning "No history data available for ~a in call to save-history-data." name)))
            (print-warning "~a does not name a history data source item in call to save-history-data." name)))))))

(add-act-r-command "save-history-data" 'save-history-data-fct "Save the current history data from the indicated history stream and parameters to the specifed file. Params: history-name file-name {additional-parameters}*")


(defun parse-saved-history-data (requested file)
  #+:single-threaded-act-r (when file 
                             (print-warning "History file access unavailable in single-threaded model.")
                             (return-from parse-saved-history-data))
  (handler-case
      (let* ((d (json:decode-json-from-source file))
             (name (cdr (assoc :name d)))
             (version (cdr (assoc :version d)))
             (data (cdr (assoc :data d)))
             (comment (cdr (assoc :comment d))))
        (if (eq (string->name name) requested)
            (progn
              (written-for-act-r-version version (format nil "Saved history data stream ~s in file ~s" requested file))
              (if comment 
                  (values data comment) 
                data))
          (print-warning "File ~s contains data for history ~s instead of desired history ~s." file name requested)))
    (error (x) 
      (print-warning "Error ~/print-error-message/ occurred while trying to read history ~s from file ~s." x requested file))))


(defmacro record-history (&rest names)
  `(apply 'record-history-fct ',names))
  
(defun record-history-fct (&rest names)
  (let ((history-recorder (get-component history-recorder))
        (results (make-list (length names) :initial-element nil)))
    (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
      (dotimes (i (length names) (values-list results))
        (let ((n (nth i names)))
          (if (not (stringp n))
              (print-warning "Record-history given name ~s but names must be strings." n)
            
            (let* ((o-name (string->name n))
                   (start (gethash o-name (history-recorder-streams-table history-recorder)))
                   (s (cond ((data-stream-p start)
                             start)
                            ((data-processor-p start)
                             (data-processor-stream start))))
                   (name (when s (data-stream-internal-name s))))
              (if s
                (let ((m (if (data-stream-model-based s)
                             (suppress-warnings (get-module :history-recorder))
                           (history-recorder-module history-recorder))))
                  (if m
                      (multiple-value-bind (enabled data alive)
                          (dispatch-apply (data-stream-status s))
                        (declare (ignore data))
                        (if alive
                            (let ((state (gethash name (history-module-state-table m))))
                              (cond ((and enabled (or (eq state :disabled) (null state)))
                                     (setf (gethash name (history-module-state-table m))
                                       :was-enabled))
                                    (enabled 
                                     ;; do nothing - either was-enabled or enabled so what expected
                                     )
                                    ((and (null enabled) (eq state :enabled))
                                     (print-warning "History data source ~s should have already been recording, but was off and is being re-enabled." name)
                                     (dispatch-apply (data-stream-enable s)))
                                    ((and (null enabled) (eq state :was-enabled))
                                     (print-warning "History data source ~s had been enabled for recording by other means and is now off, but is being re-enabled." name)
                                     (dispatch-apply (data-stream-enable s)))
                                    ((null enabled)
                                     (dispatch-apply (data-stream-enable s))
                                     (setf (gethash name (history-module-state-table m)) :enabled)))
                              
                              (if (gethash name (history-module-requests-table m))
                                  (incf (gethash name (history-module-requests-table m)))
                                (setf (gethash name (history-module-requests-table m)) 1))
                              
                              (when (data-stream-parent s)
                                (unless (dispatch-apply (data-stream-status (data-stream-parent s)))
                                  (print-warning "Data source constituent ~s enabled but parent source ~s is not." name (data-stream-external-name (data-stream-parent s)))))
                              (setf (nth i results) t))
                          (print-warning "Cannot enable history data source ~s because the current source is no longer available." name)))
                    (print-warning "Cannot enable model-based history data source ~s when no model is available." name)))
              (print-warning "~s does not name a history data source in call to record-history." n)))))))))
  
(add-act-r-command "record-history" 'record-history-fct "Start recording the named history information if not already being recorded. Params: history-name*")

(defmacro stop-recording-history (&rest names)
  `(apply 'stop-recording-history-fct ',names))
  
(defun stop-recording-history-fct (&rest names)
  (let ((history-recorder (get-component history-recorder))
        (results (make-list (length names) :initial-element nil)))
    (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
      (dotimes (i (length names) (values-list results))
        (let ((n (nth i names)))
          (if (not (stringp n))
              (print-warning "Stop-recording-history given name ~s but names must be strings." n)
            (let* ((o-name (string->name n))
                   (start (gethash o-name (history-recorder-streams-table history-recorder)))
                   (s (cond ((data-stream-p start)
                             start)
                            ((data-processor-p start)
                             (data-processor-stream start))))
                   (name (when s (data-stream-internal-name s))))
              (if s
                  (let ((m (if (data-stream-model-based s)
                               (suppress-warnings (get-module :history-recorder))
                             (history-recorder-module history-recorder))))
                    (if m
                        (multiple-value-bind (enabled data alive)
                            (dispatch-apply (data-stream-status s))
                          (declare (ignore data))
                          
                          (when alive ;; if it's not alive don't care about disabling and no need to warn
                            (let ((state (gethash name (history-module-state-table m)))
                                  (val (gethash name (history-module-requests-table m))))
                              
                              (cond ((and val (numberp val) (plusp val))
                                     (setf val (decf (gethash name (history-module-requests-table m)))))
                                    ((and val (not (and (numberp val) (zerop val))))
                                     (print-warning "Clearing unexpected value in recording table when attempting to stop recording of history source ~s." name)
                                     (setf (gethash name (history-module-requests-table m)) 0
                                       val 0))
                                    (t
                                     (print-warning "Attempting to stop recording history source ~s but it hasn't explicitly been started." name)))
                              
                              (setf (nth i results)
                                (cond ((and enabled (eq state :enabled) (zerop val))
                                       (dispatch-apply (data-stream-disable s))
                                       (setf (gethash name (history-module-state-table m))
                                         :disabled)
                                       t)
                                      ((and enabled (eq state :enabled))
                                       ; do nothing already decremented
                                       nil)
                                      ((and enabled (null state))
                                       (setf (gethash name (history-module-state-table m))
                                         :was-enabled)
                                       (print-warning "Cannot disable recording for history source ~s because it was enabled by other means." name))
                                      (enabled
                                       (print-warning "Cannot disable recording for history source ~s because it was enabled by other means." name))
                                      ((and (eq state :enabled) (zerop val))
                                       (setf (gethash name (history-module-state-table m)) :disabled) 
                                       (print-warning "History recording for history source ~s was disabled by other means." name)
                                       t)
                                      ((eq state :enabled)
                                       (print-warning "History recording for history source ~s was disabled externally but there are still pending requests to start it." name))
                                      ((null state)
                                       t)
                                      (t 
                                       (print-warning "History recording for history source ~s encounterd unexpected state because it toggled from enabled to disabled through other means." name)))))))
                      (print-warning "Cannot disable model-based history data source ~s when no model is available." name)))
                (print-warning "~s does not name a history data source in call to stop-recording-history." n)))))))))

(add-act-r-command "stop-recording-history" 'stop-recording-history-fct "Stop recording the named history information if all requested starts have stopped. Params: history-name*")


(defun reset-history-recorder (module) 
  ;; If there are any unstopped starts (>0 value in requests)
  ;; make sure it's turned on
  (let ((history-recorder (get-component history-recorder)))
    
    (bt:with-recursive-lock-held ((history-recorder-lock history-recorder))
      
      (maphash (lambda (name val)
                 (when (and (numberp val) (> val 0))
                   (let ((s (gethash name (history-recorder-streams-table history-recorder))))
                     (when s
                       (unless (dispatch-apply (data-stream-status s))
                         (dispatch-apply (data-stream-enable s)))))))
               (history-module-requests-table module)))))

(defun create-history-recorder-module (x)
  (declare (ignore x))
  (make-history-module))

(define-module-fct :history-recorder nil nil
  :creation 'create-history-recorder-module
  :reset '(nil nil reset-history-recorder)
  :required t
  :version "2.2"
  :documentation "Module to support the saving and accessing of information as a model runs.")


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

