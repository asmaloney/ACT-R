;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2006 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : buffer-trace.lisp
;;; Version     : 3.1
;;; 
;;; Description : Provide a tool that shows what activities are occuring in
;;;             : the buffers instead of the current "event" based trace and
;;;             : make that information available to the modeler as well if
;;;             : desired.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [] Watch the :trace-filter parameter and warn if it gets set
;;;             :    to a function other than disable-event-trace when the buffer
;;;             :    trace is enabled.
;;;             : [] Better monitor the setting/removing of the pre-event hook.
;;; 
;;; ----- History -----
;;; 2006.01.26 Dan
;;;             : * Initial creation.
;;; 2006.02.07 Dan
;;;             : * Fixed an issue with subseq going past the end of the event-details.
;;; 2006.07.18 Dan
;;;             : * Changed schedule-maintenance-event-relative to just
;;;             :   schedule-event-relative because maintenance is now just a 
;;;             :   keyword in all of the scheduling functions.
;;; 2006.09.11 Dan
;;;             : * Changed the parameter test for the trace step to just be posnumornil.
;;; 2007.05.18 Dan
;;;             : * Added the busy->free flag to the buffer summary records because
;;;             :   it's possible for a module to be marked as busy during some 
;;;             :   early events and then become free later at the same time
;;;             :   and that can be useful to know.
;;; 2007.05.22 Dan
;;;             : * Added two parameters for use with the graphic trace tools
;;;             :   in the environment: :buffer-trace-colors and :graphic-column-widths.
;;;             : * Took the a1 off the version number.
;;;             : * Added the additional slot of notes to the buffer summary 
;;;             :   records and added the command add-buffer-trace-notes
;;;             :   to allow one to add them to the record.  
;;; 2007.06.06 Dan
;;;             : * Fixed a bug in the value returned when setting the :traced-buffers
;;;             :   parameter.
;;;             : * Added the error->clear flag to the buffer summary records
;;;             :   because like busy->free that can be an important transition.
;;; 2011.04.25 Dan
;;;             : * Converting to using the ms time internally.  The buffer-
;;;             :   record-time-stamp slot will no longer be a member of the 
;;;             :   structure, but a function with the same name will still
;;;             :   return the time in seconds.  The slot will be buffer-record-
;;;             :   ms-time and hold an integer count of milliseconds instead.
;;; 2011.06.17 Dan
;;;             : * Added an update function and use that to call the trace-hook
;;;             :   functions instead of doing it in the post hook because that
;;;             :   is problematic since the first event of the new time has already
;;;             :   occurred before the hooks get called.
;;; 2011.11.11 Dan 
;;;             : * Added an optional parameter to get-current-buffer-trace to allow
;;;             :   the user to clear the history if desired for performance
;;;             :   purposes.  
;;;             : * Changed add-buffer-trace-notes to return the notes passed
;;;             :   instead of the event that gets scheduled and check to make
;;;             :   sure the buffer is a valid name.
;;; 2013.09.18 Dan
;;;             : * Fixing it so that setting the buffer-trace param returns the
;;;             :   value that was set.
;;; 2014.03.17 Dan [2.0]
;;;            : * Changed the query-buffer call to be consistent with the new
;;;            :   internal code.
;;; 2014.05.28 Dan
;;;            : * Changed how the module request summary is recorded since it
;;;            :   can't be the chunk-type of the spec.  Now it's the whole 
;;;            :   spec printed to a single line.
;;; 2014.10.22 Dan
;;;            : * When the buffer-trace-step is set only record steps when there's
;;;            :   a "real" action that's going to occur i.e. if the run would
;;;            :   end without the step event let it.
;;; 2015.03.18 Dan
;;;            : * Fixed the return value from setting :save-buffer-trace so it
;;;            :   returns t when set to t.
;;;            : * Changed the parameter warning strings to remove the final '.'.
;;; 2015.06.04 Dan
;;;            : * The :buffer-trace-step parameter was returning an incorrectly
;;;            :   converted result since it's set in seconds.
;;; 2015.06.05 Dan
;;;            : * Use schedule-event-now instead of a realtive 0.
;;; 2016.04.20 Dan
;;;            : * Fixed a bug that prevented one from setting :traced-buffers
;;;            :   to nil.
;;; 2016.06.18 Dan
;;;            : * Fixed a bug with the return value of :buffer-trace-hook when it
;;;            :   gets set.
;;; 2016.06.21 Dan
;;;            : * Fixed a bug with the previous fix.
;;; 2017.08.09 Dan
;;;            : * Replaced capture-model-output with a call to printed-chunk-spec.
;;; 2017.09.28 Dan [3.0]
;;;            : * Transition to the history data stream mechanism and eliminating
;;;            :   the buffer trace option (for now at least).  Also removing all
;;;            :   of the parameters for the module:
;;;            :      - GUI based parameters (size and color) because those
;;;            :        should be options in the GUI
;;;            :      - buffer-trace and -step since the trace is going away
;;;            :      - save- and traced- because those now go through the 
;;;            :        general history recording tool
;;;            :      - trace-hook because nothing else needs it and I doubt anyone
;;;            :        actually uses it
;;;            : * The default is for all buffers to be off now instead of on.
;;;            : * Added a lock to protect access.
;;;            : * Catch erase-buffer and treat like clear-buffer.
;;; 2017.10.02 Dan
;;;            : * Instead of just the + or * at the start of the request details
;;;            :   for default actions include the buffer and > too e.g. +goal>.
;;; 2017.10.03 Dan
;;;            : * Moved the translator/accessor functions for the data lists 
;;;            :   to here from the environment trace processing code.
;;; 2018.02.05 Dan
;;;            : * Updated this to use the internal event slot accessors and get
;;;            :   the real struct since it's going to be passed the id.
;;; 2018.02.22 Dan
;;;            : * Protect access to the event time and priority.
;;; 2018.02.28 Dan
;;;            : * Fixed a bug in with the last update.
;;; 2018.03.02 Dan
;;;            : * Updated define-history-component to -constituent.
;;; 2018.03.16 Dan
;;;            : * Meta-p-schedule-lock is recursive.
;;; 2018.03.20 Dan
;;;            : * Add a remote version of add-buffer-trace-notes and convert the
;;;            :   buffer name provided from a string if needed.
;;; 2018.06.22 Dan
;;;            : * Only the remote add-buffer-trace-notes allows a string name.
;;; 2018.07.31 Dan
;;;            : * Add optional parameters to the buffer-trace history stream
;;;            :   to allow specifying start and stop times.
;;; 2020.01.14 Dan [3.1]
;;;            : * Buffer-trace history constituents can't be lambdas.
;;; 2021.06.07 Dan 
;;;             : * Deal with set-buffer-chunk and overwrite-... possibly having
;;;             :   a chunk-spec as the second parameter.
;;; 2021.10.18 Dan
;;;             : * Changed call to buffers to model-buffers instead when adding
;;;             :   notes (still check all at module creation time).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The following information is recorded at each event of the model and aggregated
;;; over all events at a given time:
;;; 
;;; Whether the module is busy
;;; Whether the module transitioned from busy to free at this time
;;; Whether the module is in an error
;;; Whether the buffer is full
;;; Whether the buffer is cleared
;;; Whether the chunk in the buffer is modified
;;;
;;; Whether a request is sent to the module
;;; Whether a new chunk is set in the buffer
;;; Any notes which are recorded using add-buffer-trace-notes.
;;;
;;; For the first 6, if the stated condition is true during any event at the
;;; current time the buffer record will indicate t.
;;;
;;; For the requests, each request overwrites any prior request recorded
;;; at that time.  The value recorded is the details string provided for the event
;;; if there was one or a printing of the request's chunk-spec if not.
;;; If a chunk is set into the buffer, then the name of that chunk is recorded,
;;; and only the last setting at a specific time is recorded.
;;; The notes are set to the string specified by add-buffer-trace-notes, but
;;; only the last note at a given time will be saved (they overwrite).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;  (defstruct buffer-record ms-time buffers)
;;;  (defstruct buffer-summary name cleared busy busy->free error full modified request chunk-name notes)
;;;
;;;
;;; add-buffer-trace-notes
;;;
;;;    Takes two parameters which are the name of a buffer and any notes
;;;    to set for the buffer summary of that buffer at the current time.  
;;;
;;;    This can be used to augment the summaries on the fly, and would most likely
;;;    be called from an event hook that was watching for something that needed to
;;;    be noted.
;;;
;;;    There are no restrictions on what can be passed as notes.
;;;    
;;;    Calling this will create a maintenance event which records the notes at
;;;    the current time.
;;;
;;;    The graphic trace tools will display the notes when the mouse is placed over
;;;    the corresponding box in the trace (the notes will be printed using the Lisp
;;;    format string "~a").
;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Grown out of Scott's graphic module tracer and the old environment's PERT
;;; style trace.  The idea being that instead of looking at the specific actions
;;; of a module one can "watch" the buffers since they're the interface to
;;; the module.  As long as a module takes requests through the buffers and 
;;; responds to the state queries appropriately it can be monitored.
;;;
;;; The addition of the production buffer was necessary so that the procedural
;;; module could be queried and report "requests" (production firings) like
;;; any other module.  It is a bit strange, and not really a buffer of the
;;; theory (note it doesn't end in 'al') but may end up being so as work on
;;; meta-cognitive processing continues - being able to monitor the state 
;;; of the prodceural system may be an important thing to do.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defclass buffer-trace-module ()
  ((history-lock :accessor history-lock :initform (bt:make-lock "buffer-trace") :allocation :class)
   (btm-enabled :accessor btm-enabled :initform nil)
   (alive :accessor alive :initform t)
   (btm-traced-buffers :accessor btm-traced-buffers :initform nil)
   (btm-current-summary :accessor btm-current-summary :initform nil)
   (btm-saved-records :accessor btm-saved-records :initform nil)
   
   (available-buffers :accessor available-buffers :initform nil :allocation :class)
   (post-recorder :accessor post-recorder :initform nil :allocation :class)
   (monitor-count :accessor monitor-count :initform 0 :allocation :class)))

(defstruct buffer-record
  ms-time
  buffers)

(defstruct buffer-summary
  name
  cleared
  busy
  busy->free
  error
  error->clear
  full
  modified
  request
  chunk-name
  notes)


(defun buffer-trace-event-recorder (event)
  (let ((btm (get-module buffer-trace)))
    (when btm
      (bt:with-lock-held ((history-lock btm))
        (when (btm-enabled btm)
          (let* ((evt (get-event-by-id event))
                 (e-time (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime evt))))
            (when (null (btm-current-summary btm))
              
              ;; create a new one and set as current
              (setf (btm-current-summary btm)
                (make-buffer-record :ms-time e-time))
              (setf (buffer-record-buffers (btm-current-summary btm))
                (mapcar (lambda (x) (make-buffer-summary :name x))
                  (btm-traced-buffers btm))))
            
            (unless (= (buffer-record-ms-time (btm-current-summary btm)) e-time)
              (push-last (btm-current-summary btm) (btm-saved-records btm))
              
              (setf (btm-current-summary btm)
                (make-buffer-record :ms-time e-time))
              
              (setf (buffer-record-buffers (btm-current-summary btm))
                (mapcar (lambda (x) (make-buffer-summary :name x))
                  (btm-traced-buffers btm))))
            
            ;; Update the records
            ;; First pull any meaningful info out of the evt itself
            
            (case (act-r-event-action evt)
              ((set-buffer-chunk overwrite-buffer-chunk)
               (let ((bn (car (act-r-event-params evt))))
                 (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key 'buffer-summary-name)
                        (setf (buffer-summary-chunk-name it) 
                          (if (symbolp (second (act-r-event-params evt)))
                              (string (second (act-r-event-params evt)))
                            (format nil "~S" (chunk-spec-to-chunk-def (second (act-r-event-params evt)))))))))
              
              (mod-buffer-chunk
               (let ((bn (car (act-r-event-params evt))))
                 (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key 'buffer-summary-name)
                        (setf (buffer-summary-modified it) t))))
              
              ((clear-buffer erase-buffer)
               (let ((bn (car (act-r-event-params evt))))
                 (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key 'buffer-summary-name)
                        (setf (buffer-summary-cleared it) t))))
              
              (module-request
               (let ((bn (car (act-r-event-params evt))))
                 (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key 'buffer-summary-name)
                        (setf (buffer-summary-request it)
                          (if (and (>= (length (act-r-event-details evt)) 15)
                                   (string-equal "module-request " (subseq (act-r-event-details evt) 0 15)))
                              (concatenate 'string "+" (symbol-name bn) "> " (printed-chunk-spec (second (act-r-event-params evt)) t))
                            (act-r-event-details evt))))))
              
              (record-buffer-trace-notes
               (let ((bn (car (act-r-event-params evt))))
                 (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key 'buffer-summary-name)
                        (setf (buffer-summary-notes it)
                          (second (act-r-event-params evt))))))
              
              (module-mod-request
               (let ((bn (car (act-r-event-params evt))))
                 (awhen (find bn (buffer-record-buffers (btm-current-summary btm)) :key 'buffer-summary-name)
                        (setf (buffer-summary-request it)
                          (if (and (>= (length (act-r-event-details evt)) 19)
                                   (string-equal "module-mod-request " (subseq (act-r-event-details evt) 0 19)))
                              (concatenate 'string "*" (symbol-name bn) "> " (printed-chunk-spec (second (act-r-event-params evt)) t))
                            (act-r-event-details evt)))))))
            
            ;; Now for each one set busy, error, and full
            
            (dolist (x (buffer-record-buffers (btm-current-summary btm)))
              
              (if (query-buffer (buffer-summary-name x) '(state busy))
                  (setf (buffer-summary-busy x) t)
                (when (buffer-summary-busy x)
                  (setf (buffer-summary-busy->free x) t)))
              
              (if (query-buffer (buffer-summary-name x) '(state error))
                  (setf (buffer-summary-error x) t)
                (when (buffer-summary-error x)
                  (setf (buffer-summary-error->clear x) t)))
              
              (when (query-buffer (buffer-summary-name x) '(buffer full))
                (setf (buffer-summary-full x) t)))))))))


(defun get-current-buffer-trace (&optional (clear nil))
  (let ((btm (get-module buffer-trace)))
    (when btm
      (let ((return (btm-saved-records btm)))
        (when clear
          (setf (btm-saved-records btm) nil))
        return))))


(defun record-buffer-trace-notes (buffer notes)
  "Dummy function for recording event"
  (declare (ignore buffer notes)))

(defun add-buffer-trace-notes (buffer notes)
  (if (and (current-model) (find buffer (model-buffers)))
      (progn 
        (schedule-event-now 'record-buffer-trace-notes
                            :maintenance t :priority :max
                            :output nil :params (list buffer notes))
        notes)
    (if (current-model)
        (print-warning "~s does not name a buffer in the current model no notes added." buffer)
      (print-warning "No current model so cannot add notes."))))

(defun remote-add-buffer-trace-notes (buffer notes)
  (add-buffer-trace-notes (string->name buffer) notes))

(add-act-r-command "add-buffer-trace-notes" 'add-buffer-trace-notes "Command to add notes to the buffer-trace history data. Params: buffer-name note.")

(defun reset-buffer-trace-module (module)
  (bt:with-lock-held ((history-lock module))
    
    (setf (btm-current-summary module) nil)
    (setf (btm-saved-records module) nil)
    (setf (btm-traced-buffers module) nil)
    
    (when (btm-enabled module)
      (setf (btm-enabled module) nil)
      (decf (monitor-count module))
      (when (zerop (monitor-count module))
        (delete-event-hook (post-recorder module))))))


(defun delete-buffer-trace-module (module)
  (bt:with-lock-held ((history-lock module))
    (setf (alive module) nil)
    
    (when (btm-enabled module)
      (setf (btm-enabled module) nil)
      (decf (monitor-count module))
      (when (zerop (monitor-count module))
        (delete-event-hook (post-recorder module))))
    t))


(defun create-buffer-trace-module (name)
  (declare (ignore name))
  (let ((m (make-instance 'buffer-trace-module)))
    (bt:with-lock-held ((history-lock m))
      (dolist (x (buffers))
        (let ((buffer x))
          (unless (find x (available-buffers m))
            (push x (available-buffers m))
            (define-history-constituent-fct "buffer-trace" (string buffer)
              (eval `(defun ,(read-from-string (concatenate 'string "buffer-trace-enable-" (string buffer))) ()
                       (enable-buffer-trace-buffer ',buffer)))
              (eval `(defun ,(read-from-string (concatenate 'string "buffer-trace-disable-" (string buffer))) ()
                       (disable-buffer-trace-buffer ',buffer)))
              (eval `(defun ,(read-from-string (concatenate 'string "buffer-trace-status-" (string buffer))) ()
                       (buffer-trace-buffer-status ',buffer)))))))
      m)))


(defun enable-buffer-trace-buffer (name)
  (let ((module (get-module buffer-trace)))
    (bt:with-lock-held ((history-lock module))
      (push name (btm-traced-buffers module))
      t)))


(defun disable-buffer-trace-buffer (name)
  (let ((module (get-module buffer-trace)))
    (bt:with-lock-held ((history-lock module))
      (setf (btm-traced-buffers module) (remove name (btm-traced-buffers module)))
      t)))

(defun buffer-trace-buffer-status (name)
  (let ((module (get-module buffer-trace)))
    (bt:with-lock-held ((history-lock module))
      (values 
       (when (find name (btm-traced-buffers module)) t) 
       (when (or (btm-current-summary module) (btm-saved-records module)) t)
       (alive module)))))


(define-module-fct 'buffer-trace nil nil
  :version "3.1"
  :documentation "A module that provides a buffer based history mechanism."
  :creation 'create-buffer-trace-module
  :reset 'reset-buffer-trace-module
  :delete 'delete-buffer-trace-module)


(defun enable-buffer-trace-history ()
  (let ((module (get-module buffer-trace)))
    (bt:with-lock-held ((history-lock module))
      (unless (btm-enabled module)
        (setf (btm-enabled module) t)
        (when (zerop (monitor-count module))
          (setf (post-recorder module) (add-post-event-hook 'buffer-trace-event-recorder)))
        (incf (monitor-count module)))
      t)))

(defun disable-buffer-trace-history ()
  (let ((module (get-module buffer-trace)))
    (bt:with-lock-held ((history-lock module))
      (when (btm-enabled module)
        (setf (btm-enabled module) nil)
        (decf (monitor-count module))
        (when (zerop (monitor-count module))
          (delete-event-hook (post-recorder module))))
      t)))
            

(defun buffer-trace-history-status (&optional start stop)
  (let ((module (get-module buffer-trace))
        (start-time (if (numberp start) (safe-seconds->ms start) 0))
        (stop-time (if (numberp stop) (safe-seconds->ms stop) (mp-time-ms))))
    (bt:with-lock-held ((history-lock module))
      (values (btm-enabled module) 
              (when (and (or (btm-current-summary module) (btm-saved-records module))
                         (some (lambda (x)
                                 (<= start-time (buffer-record-ms-time x) stop-time))
                               (if (null (btm-current-summary module))
                                   (btm-saved-records module)
                                 (append (btm-saved-records module) (list (btm-current-summary module))))))
                t)
              (alive module)))))

(defun get-buffer-trace-history (&optional start stop)
  (let ((module (get-module buffer-trace))
        (start-time (if (numberp start) (safe-seconds->ms start) 0))
        (stop-time (if (numberp stop) (safe-seconds->ms stop) (mp-time-ms))))
    
    (mapcan (lambda (x) 
              (when (<= start-time (buffer-record-ms-time x) stop-time)
                (list (list (buffer-record-ms-time x)
                            (mapcar (lambda (y)
                                      (list (symbol-name (buffer-summary-name y))
                                            (when (buffer-summary-busy y) t)
                                            (when (buffer-summary-cleared y) t)
                                            (when (buffer-summary-busy->free y) t)
                                            (when (buffer-summary-error y) t)
                                            (when (buffer-summary-error->clear y) t)
                                            (when (buffer-summary-full y) t)
                                            (when (buffer-summary-modified y) t)
                                            (aif (buffer-summary-request y) it "")
                                            (aif (buffer-summary-chunk-name y) it "")
                                            (aif (buffer-summary-notes y) it "")))
                              (buffer-record-buffers x))))))
      (if (null (btm-current-summary module))
          (btm-saved-records module)
        (append (btm-saved-records module) (list (btm-current-summary module)))))))
              

;; Define accessors to make the transition easier for the code that
;; was accessing the structures before.

(defun br-time (x) (first x))
(defun br-buffers (x) (second x))

(defun bs-name (x) (first x))
(defun bs-busy (x) (second x))
(defun bs-cleared (x) (third x))
(defun bs-busy->free (x) (fourth x))
(defun bs-error (x) (fifth x))
(defun bs-error->clear (x) (sixth x))
(defun bs-full (x) (seventh x))
(defun bs-modified (x) (eighth x))

(defun check-bs-request (x) (let ((y (ninth x))) (and (not (zerop (length y))) y)))
(defun value-bs-request (x) (let ((y (ninth x))) (or y "")))

(defun check-bs-chunk-name (x) (let ((y (tenth x))) (and (not (zerop (length y))) y)))
(defun value-bs-chunk-name (x) (let ((y (tenth x))) (or y "")))

(defun check-bs-notes (x) (let ((y (nth 10 x))) (and (not (zerop (length y))) y)))
(defun value-bs-notes (x) (let ((y (nth 10 x))) (or y "")))



(define-history "buffer-trace" enable-buffer-trace-history disable-buffer-trace-history buffer-trace-history-status get-buffer-trace-history)


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
