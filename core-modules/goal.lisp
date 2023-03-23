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
;;; Filename    : goal.lisp
;;; Version     : 3.0
;;; 
;;; Description : Implementation of the goal module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.10.13 Dan
;;;             : Initial creation.
;;;
;;; 2005.01.19 Dan
;;;             : Updated goal-focus to better match what ACT-R 4/5 did.
;;; 2005.02.22 Dan
;;;             : * Added a reset function to the goal to clear the stuffed flag
;;;             : * Changed the stuffed flag to holding the chunk that is set
;;;             :   by the user so that goal-focus can print a "pending" message
;;;             :   when queried between the goal-focus and the model actually
;;;             :   running to set the buffer.
;;; 2005.02.25 Dan
;;;             : * Clean up the "pending" info a little more so that if there
;;;                 is a chunk in the buffer things still "look" right.
;;; 2005.03.18 Dan
;;;             : * The goal buffer will not be strict harvested by default,
;;;             :   so the reset function is going to set that parameter.
;;; 2005.03.21 Dan 
;;;             : * Changing the version to 1.0b1.
;;; 2005.03.23 Dan
;;;             : * In order to set the do-not-harvest parameter correctly it 
;;;             :   do so after the defaults are set so goal uses the second
;;;             :   reset parameter to do its reseting (doesn't need the first).
;;; 2005.03.24 Dan
;;;             : * Undoing the setting of goal on do-not-havrest list for now
;;;             :   under the idea that parsimony is better overall.
;;; 2005.03.27 Dan
;;;             : * Back it goes - goal is once again set to not be strict
;;;             :   harvested by default.
;;; 2005.04.23 Dan
;;;             : * Stuffed is no longer a query for the module, but the 
;;;             :   internal slot has other uses.
;;; 2005.05.20 Dan
;;;             : * Renamed the stuffed slot to delayed and cleaned up how
;;;             :   it gets used becasue there were some situations where 
;;;             :   calling goal-focus resulted in incorrect information.
;;; 2005.07.06 Dan
;;;             : * Updated the docs and changed the version to 1.0.
;;; 2005.07.21 Dan
;;;             : * Changed mod-focus so that it schedules a notice of the
;;;             :   modification.  May want to actually schedule the mod, but
;;;             :   that could break some older/existing stuff so I want to be
;;;             :   safe for now.
;;; 2005.08.10 Dan
;;;             : * Updated goal-query to specify that the instance is ignored.
;;; 2006.07.20 Dan
;;;             : * Changed the goal-focus command so that the clean-up event
;;;             :   is flagged as a maintenance event.
;;; 2007.06.05 Dan
;;;             : * Added buffer modification requests as an option in the goal 
;;;             :   buffer.  Acts just like a production's buffer modification
;;;             :   except that with the + it's attributed to the goal module
;;;             :   instead of procedural i.e. there is no difference in performance
;;;             :   between the RHS actions:
;;;             :   =goal> slot value  
;;;             :   and 
;;;             :   +goal> slot value 
;;;             :   
;;;             :   The only difference will be in the output of the trace and
;;;             :   the buffer summary records.
;;; 2008.09.19 Dan 
;;;             : * Moved the mod-request function to goal-style support
;;;             :   and changed the goal module's definition to use it.
;;; 2013.03.20 Dan
;;;             : * Minor edits while verifying that it'll work with the typeless
;;;             :   chunk mechanism.
;;; 2015.02.11 Dan [2.0]
;;;             : * Change the default value for :ga from 1 to 0.  The imaginal
;;;             :   buffer is now the only one which spreads activation by default.
;;;             :   Should have made this change with the first version of 6.1.
;;; 2015.06.04 Dan
;;;             : * Use :time-in-ms t for all the scheduled events.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2016.09.27 Dan [2.1]
;;;             : * Mod-focus now does the modification in the scheduled event
;;;             :   instead of directly which makes a goal-focus then mod-focus
;;;             :   work which seems more natural for some simple situations.
;;; 2016.11.23 Dan
;;;             : * Modified goal-focus to work through the central dispatcher.
;;; 2016.12.01 Dan
;;;             : * Modified goal-focus-internal so that it will handle a string
;;;             :   containing the name of a chunk as well as the symbol.
;;; 2017.01.18 Dan
;;;             : * Removed the echo-act-r-output from goal-focus.  The assumption
;;;             :   now is that echoing is handled by the user.
;;;             : * Use handle-evaluate-results.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.08 Dan
;;;             : * Reworked the local versions of the remote commands to not
;;;             :   go out through the dispatcher.
;;;             : * Added an external mod-focus command.
;;; 2017.02.15 Dan
;;;             : * For mod-focus, assume if the first item is a string then it
;;;             :   needs to be decoded.
;;; 2017.06.13 Dan
;;;             : * Add a lock to protect goal-module-delayed.
;;; 2017.12.13 Dan
;;;             : * Why didn't goal-focus check whether there was a goal module?
;;; 2018.06.12 Dan
;;;             : * Only the external commands should convert strings to names.
;;; 2020.01.10 Dan [2.2]
;;;             : * Removed the #' from the module interface functions since 
;;;             :   that's not allowed in the general system now.
;;; 2020.08.26 Dan
;;;             : * Removed the path for require-compiled since it's not needed
;;;             :   and results in warnings in SBCL.
;;; 2021.03.10 Dan [2.3]
;;;             : * Set the :do-not-query parameter for goal buffer now.
;;; 2021.10.20 Dan 
;;;             : * Check that there is a procedural module before setting the
;;;             :   :do-not-query and :do-not-harvest parameters.
;;; 2021.10.28 Dan [3.0]
;;;             : * Allow goal-focus to take a chunk description, slots and
;;;             :   values, instead of just a chunk name.  Function requires
;;;             :   the values passed in a list.
;;;             : * The remote version requires doing the string conversion
;;;             :   now since slot values could be strings when a list given.
;;; 2021.11.30 Dan
;;;             : * Fixed a problem with the goal-focus macro that happened
;;;             :   with the change to allow a description.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The goal module has one buffer called goal.
;;; The source spread parameter of the goal is called :ga and defaults to 0.
;;;
;;; The goal module responds to requests by creating a new chunk and placing it
;;; into the buffer.  The requests must be a unique specification of a chunk.
;;; Thus, no variables are allowed and each slot may be specified at most once.
;;; The new chunk is placed into the buffer at the same time as the request.
;;;
;;; It only responds to the required queries - state {busy, free, error} and 
;;; buffer {empty, full, requested, unrequested}.
;;;
;;; State free will always return t.
;;; State busy will always return nil.
;;; State error will always return nil.
;;; Buffer requested will respond nil if the goal chunk is created with the
;;;   goal-focus command otherwise it will respond t.
;;;
;;; The goal module now DOES respond to buffer modification requests.
;;; A buffer modification request acts just like the corresponding modification
;;; action in the production.  The only difference is that it is now attributed
;;; to the goal module - it still takes 0 time.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; User Functions:
;;;
;;;   (defmacro goal-focus (&optional (chunk-name-or-desc nil)))
;;;   (defun goal-focus-fct (&optional (chunk-name-or-desc nil)))
;;; 
;;;   If the chunk-name-or-desc is a symbol that names a chunk then an event is
;;;   scheduled to place a copy of that chunk into the goal buffer at the
;;;   current time and chunk name is returned.
;;;
;;;   If the chunk-name-or-desc is a list of slot-value pairs then an event is
;;;   scheduled to create a chunk with that description into the goal buffer at
;;;   the current time and the list is returned.

;;;   If chunk-name-or-desc is not given then the chunk currently in the goal
;;;   buffer is printed and that chunk's name is returned.
;;;
;;;   (defmacro mod-focus (&rest modifications)
;;;   (defun mod-focus-fct (modifications)
;;;
;;;   Modifies the chunk currently in the goal buffer using mod-chunk and the
;;;   modifications provided.
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

;;; Rely on the general functions in the goal-style-module 

(require-compiled "GOAL-STYLE-MODULE")

;;; Only need to record the chunk that will be stuffed into the buffer

(defstruct goal-module (lock (bt:make-lock "goal-module")) delayed)

(defun create-goal-module (model-name)
  (declare (ignore model-name))
  (make-goal-module))


(defun goal-reset (instance)
  (bt:with-lock-held ((goal-module-lock instance))
    (setf (goal-module-delayed instance) nil))
  ; Do NOT strict harvest the goal buffer by default
  ; and don't add explicit queries when requests are made
  (when (get-module procedural t)
    (sgp :do-not-harvest goal :do-not-query goal))
  )

(defun goal-query (instance buffer-name slot value)
  (declare (ignore buffer-name) (ignore instance))
  ;; only valid slot is state
  (case slot
    (state
     (case value
       (busy nil)
       (free t)
       (error nil)
       (t (print-warning "Unknown state query ~S to goal module" value)
          nil)))
    (t (print-warning "Unknown query ~S ~S to the goal module" slot value))))




;;; Actually define the module now

(define-module-fct 'goal '((goal (:ga 0.0)))
  nil
  :version "3.0"
  :documentation "The goal module creates new goals for the goal buffer"
  :creation 'create-goal-module
  :query 'goal-query
  :request 'goal-style-request
  :buffer-mod 'goal-style-mod-request
  :reset (list nil 'goal-reset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User functions for the Goal module - since there aren't too many
;;; just putting them right here
;;;

(defmacro goal-focus (&optional (chunk-name-or-spec nil))
  "Place a chunk into the goal buffer or return either the chunk that is there
   now or the one that will be placed there by a pendng goal-focus"
  `(goal-focus-fct ',chunk-name-or-spec))

(defun goal-focus-fct (&optional (chunk-name-or-spec nil))
  "Place a chunk into the goal buffer or return either the chunk that is there
   now or the one that will be placed there by a pending goal-focus"
    (let ((g-module (get-module goal)))
      (when g-module
        (if chunk-name-or-spec
            (if (or (chunk-p-fct chunk-name-or-spec)
                    (and (listp chunk-name-or-spec)
                         (define-chunk-spec-fct chunk-name-or-spec nil)))
                  (progn
                    (schedule-set-buffer-chunk 'goal chunk-name-or-spec 0 :time-in-ms t :module 'goal :priority :max :requested nil)
                    (schedule-event-after-module 'goal 'clear-delayed-goal :module 'goal :output nil  
                                                 :destination 'goal :maintenance t)
                    
                    (bt:with-lock-held ((goal-module-lock g-module))
                      (setf (goal-module-delayed g-module) chunk-name-or-spec))
                    chunk-name-or-spec)
              
              (if (listp chunk-name-or-spec)
                  (print-warning "~S is not a valid chunk description - goal-focus failed." chunk-name-or-spec)
                (print-warning "~S is not the name of a chunk in the current model - goal-focus failed" chunk-name-or-spec)))
          
          (let ((chunk (buffer-read 'goal))
                (delayed (bt:with-lock-held ((goal-module-lock g-module)) (goal-module-delayed g-module))))
            (cond ((and (null chunk) (null delayed))
                   (command-output "Goal buffer is empty")
                   nil)
                  ((null chunk)
                   (if (listp delayed)
                       (command-output "Will be set to a chunk with this description when model runs:~%~{   ~a ~s~^~%~}" delayed)
                     (progn
                       (command-output "Will be a copy of ~a when the model runs" delayed)
                       (pprint-chunks-fct (list delayed))))
                     delayed)
                  ((null delayed)
                   (pprint-chunks-fct (list chunk))
                   chunk)
                  (t
                   (if (listp delayed)
                       ; don't try to compare to current because 
                       ; maybe it's supposed to match...
                       (progn
                         (command-output "Will be set to a chunk with this description when model runs:~%~{   ~a ~s~^~%~}" delayed)
                         delayed)
                     (if (eq delayed (chunk-copied-from-fct chunk))
                         ;; caught it before the delayed chunk was cleared
                         (progn
                           (pprint-chunks-fct (list chunk))
                           chunk)
                       (progn
                         (command-output "Will be a copy of ~a when the model runs" delayed)
                         (command-output "Currently holds:")
                         (pprint-chunks-fct (list chunk))
                         delayed))))))))))

(defun external-goal-focus (&optional (chunk-name nil))
  (if (listp chunk-name)
      (encode-string-names (goal-focus-fct (decode-string-names chunk-name))) 
    (goal-focus-fct (string->name chunk-name))))

(add-act-r-command "goal-focus" 'external-goal-focus "Schedule a chunk to enter the goal buffer at the current time or print the current goal buffer chunk. Params: {chunk-name}.")


(defun clear-delayed-goal (instance)
  (bt:with-lock-held ((goal-module-lock instance))
    (setf (goal-module-delayed instance) nil)))

(defmacro mod-focus (&rest modifications)
  "Modify the chunk in the goal buffer as if by mod-chunk"
  `(mod-focus-fct ',modifications))

(defun mod-focus-fct (modifications)
  "Modify the chunk in the goal buffer as if by mod-chunk-fct"
  (let ((g-module (get-module goal)))
    (when g-module
      (let ((chunk (buffer-read 'goal))
            (delayed (bt:with-lock-held ((goal-module-lock g-module)) (goal-module-delayed g-module))))
        (if (or chunk delayed)
            (progn
              (if (oddp (length modifications))
                  (print-warning "Odd length modifications list in call to mod-focus.")
                (let ((slots nil))
                  (do ((s modifications (cddr s)))
                      ((null s))
                    (if (find (car s) slots)
                        (progn 
                          (print-warning "Slot ~a used more than once in modifications list." (car s))
                          (return-from mod-focus-fct))
                      (push (car s) slots)))
                  (cond ((not (every 'valid-slot-name slots))
                         (print-warning "Invalid slot name ~s specified for mod-focus." 
                                        (find-if (lambda (x) (not (valid-slot-name x))) slots)))
                        (t
                         (schedule-event-now 'goal-modification :module 'goal :params (list modifications) 
                                             :priority :max :output 'medium :details (format nil "~a" 'goal-modification))
                         (or delayed chunk))))))
          
          (print-warning "No chunk currently in the goal buffer and no pending goal-focus chunk to be modified."))))))


(defun mod-focus-external (&rest modifications)
  (encode-string-names (mod-focus-fct (decode-string-names modifications))))

(add-act-r-command "mod-focus" 'mod-focus-external "Modify the chunk in the goal buffer using the slots and values provided. Params: '{<slot-name> <new-slot-value>}'*")



(defun goal-modification (modifications)
  (mod-buffer-chunk 'goal modifications))

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
