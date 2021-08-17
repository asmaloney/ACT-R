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
;;; Filename    : meta-process.lisp
;;; Version     : 3.0
;;; 
;;; Description : The meta-process handling functions as defined in the
;;;               ACT-R 6 software framework API.
;;; 
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Improve on the max-time-delta situation for multiple models.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.11 Dan
;;;             : Creation.
;;; 2005.02.28 Dan
;;;             : * Made the with-meta-process macro hygienic.
;;; 2006.02.27 Dan
;;;             : * Added the mp-real-time-management function to allow one to
;;;             :   configure external time sources.
;;; 2006.03.03 Dan
;;;             : * Updated mp-real-time-management to add the max-time-delta
;;;             :   parameter.  This provides a solution for a problem that can
;;;             :   occur when hooking a model up to an asynchronous system.
;;;             :   The problem is that if there aren't any model events to 
;;;             :   process at some point the model just jumps right to its end
;;;             :   time and waits for real time to catch up and asynchronous
;;;             :   events that come in effectively get pushed off until then.
;;;             :   This effectively provides the maximum amount of time that
;;;             :   the model will "skip ahead" without some event occuring.
;;;             :   This still isn't perfect for a multi-model situation because
;;;             :   it only works at the meta-process level and thus one model
;;;             :   could still end up skipping way ahead if other models were
;;;             :   still doing things, but it's better than nothing right now.
;;; 2007.08.03 Dan
;;;             : * Moved the *meta-processes* definition before the macros
;;;             :   that use it to avoid a warning at compile time.
;;; 2008.05.05 Dan
;;;             : * Fixed a bug with a missing parameter to format in 
;;;             :   delete-meta-process-fct.
;;; 2009.11.30 Dan
;;;             : * Make sure to set meta-p-running back to nil on reset because
;;;             :   some abnormal situations could leave that set.
;;; 2009.12.03 Dan
;;;             : * Clear the dynamics and in-slack slots of the meta-process on
;;;             :   reset and adding an allow-dynamics keyword to mp-real-time-management
;;;             :   to enable dynamic event testing.
;;; 2010.03.03 Dan
;;;             : * Added a with-meta-process-eval like with-model has.
;;; 2010.03.05 Dan
;;;             : * Fixed a bug in define-meta-process-fct with the printing of
;;;             :   the warning when the name is invalid.
;;; 2010.09.02 Dan
;;;             : * Added code to allow the coercion of the mp-time variable to
;;;             :   a different float type.
;;;             : * Added commands for checking the accuracy of the mp-time float
;;;             :   type and allow changing it on the fly.
;;; 2010.11.03 Dan
;;;             : * Changing the internal time to be ms and converting it for
;;;             :   what's sent out (mp-time) which means the accuracy checks
;;;             :   and conversion code can go away once this is settled.
;;;             : * Removed the coercion code, but left the size tests in for
;;;             :   now just in case that may be useful in the future since
;;;             :   specifying large event in seconds would still be represented as
;;;             :   floats that would lose precision.
;;; 2010.12.22 Dan
;;;             : * Added mp-modules-events to return a list of all events that
;;;             :   are scheduled for a module - both active and waiting.
;;; 2011.03.25 Dan
;;;             : * Added mp-time-ms as a first step to transitioning everything
;;;             :   internal to milliseconds to fix the last few lingering issues.
;;; 2011.04.27 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions.
;;; 2011.06.08 Dan
;;;             : * Changed reset-mp and mp-configure-real-time since the latter
;;;             :   is now documented as a user function so it's got more safety
;;;             :   checks in it.
;;; 2013.01.03 Dan
;;;             : * Added the :delta-maintenance parameter to mp-real-time-management
;;;             :   to allow setting meta-p-max-time-maintenance.
;;; 2013.01.07 Dan
;;;             : * Added the clearing of the cannot-define-model slot to reset-mp.
;;;             : * Added a safety check to define-meta-process to prevent the
;;;             :   creation of new meta-processes while there's some meta-process
;;;             :   running (basically same issue as defining new models on the fly).
;;; 2014.02.18 Dan
;;;             : * Mp-models now returns the models in a deterministic, but 
;;;             :   unspecified order.
;;; 2014.07.17 Dan
;;;             : * Mp-print-versions now includes the *actr-major-version-string*
;;;             :   in the header.
;;; 2014.09.29 Dan
;;;             : * Mp-print-versions now uses all-module-names so that they are
;;;             :   always printed in order.
;;; 2014.09.22 Dan
;;;             : * Added mp-queue-count to be able to check if there are any
;;;             :   active events without having to print them or do some trick
;;;             :   with aborting a scheduled "after" event.
;;; 2014.11.13 Dan
;;;             : * Don't allow mp-real-time-management to adjust things if the 
;;;             :   model is currently running.
;;; 2014.12.17 Dan
;;;             : * Changed the declaim for some get-abstract-module because it
;;;             :   didn't indicate multiple return values and some Lisps care
;;;             :   about that.
;;; 2015.05.19 Dan
;;;             : * Added an optional parameter to mp-show-queue which if true
;;;             :   indicates events that are in the trace with a *.
;;; 2015.05.20 Dan
;;;             : * Added the declaim for event-displayed-p.
;;; 2015.06.03 Dan
;;;             : * Commented out the time "accuracy" code since it's not used
;;;             :   and the variables it is using are being set and used differently
;;;             :   for other reasons now.
;;; 2015.06.04 Dan
;;;             : * Use safe-seconds->ms in mp-real-time-management.
;;; 2015.07.29 Dan
;;;             : * Changed how mp-print-versions outputs info for the software
;;;             :   itself (previously called the framework).
;;; 2015.08.25 Dan
;;;             : * Adding a separate scale keyword to mp-real-time-management 
;;;             :   because just adjusting the units-per-second doesn't quite
;;;             :   work for going faster than real time with the default slack
;;;             :   hook.
;;; 2016.03.04 Dan 
;;;             : * mp-print-versions now uses *actr-architecture-version* to 
;;;             :   get the main version info.
;;; 2016.09.27 Dan [2.0]
;;;             : * There can be only one! Or maybe, one meta-process to rule 
;;;             :   them all!
;;;             :   The objective here is to have one central system through
;;;             :   which everything must communicate and given that the meta-
;;;             :   process is a discrete-event simulation system it makes sense
;;;             :   to use that as the central system since much of the existing
;;;             :   activity progresses through it already.  This does remove
;;;             :   the possibility of asynchronously running models, but given
;;;             :   how infrequent I suspect that is I think it's an acceptable
;;;             :   loss given the end objective of opening this up with an 
;;;             :   interface to other systems/languages.
;;;             : * Lots of code removed -- basically everything that dealt with
;;;             :   additional meta-processes.
;;; 2017.02.23 Dan
;;;             : * Added a dispatch function for mp-show-queue and now use
;;;             :   command-output to display the info so remote connections 
;;;             :   can see the output.
;;; 2017.03.31 Dan
;;;             : * Added the accessors for testing/setting the running slot
;;;             :   because it's protected by a lock now.
;;; 2017.05.30 Dan
;;;             : * Adding the code to support the use of components.
;;; 2017.06.02 Dan
;;;             : * Cleaned up some issues with the "current model" setting and
;;;             :   don't reset the default in reset-mp.
;;;             : * Component model-delete functions can't be called in remove-
;;;             :   model because it needs to happen before the modules are gone.
;;; 2017.06.05 Dan
;;;             : * Adding a reset functions to components.  A before and after
;;;             :   are available based on when the models get reset.
;;; 2017.06.07 Dan
;;;             : * Changed mp-show-queue to use meta-p-output since command-
;;;             :   output depends on current model which prevents it from working
;;;             :   in a multiple model situation.
;;; 2017.06.15 Dan
;;;             : * Making things thread safe: mp-models.
;;; 2017.06.22 Dan
;;;             : * Protected meta-p-time with the schedule-lock.
;;; 2017.06.23 Dan
;;;             : * Protecting meta-p-delayed.
;;; 2017.06.28 Dan
;;;             : * Removed the in-slack slot of the meta-process because it
;;;             :   isn't used anymore.
;;;             : * Protect all the scheduling slots with the schedule-lock.
;;; 2017.07.14 Dan
;;;             : * Protect all the meta-p-component-list access.
;;; 2017.08.24 Dan
;;;             : * Added the creating-model and deleting-model signals.
;;;             : * Added the mp-models command.
;;; 2017.08.28 Dan
;;;             : * Added the act-r-running-p command as a remote version of
;;;             :   mp-running?.
;;; 2017.08.31 Dan
;;;             : * Adding a remote version of mp-show-queue which returns the
;;;             :   text instead of outputing it.
;;; 2017.10.04 Dan
;;;             : * Fixed a bug in get-component when the name is invalid.
;;; 2018.01.24 Dan
;;;             : * Mp-show-queue now uses act-r-output instead of meta-p-output.
;;;             : * Added a remote mp-queue-count.
;;;             : * Mp-show-waiting now uses act-r-output.
;;;             : * Added a remote mp-show-waiting.
;;; 2018.01.25 Dan
;;;             : * Added remote mp-print-versions and adjusted it to use the
;;;             :   act-r-output command instead of just format t.
;;; 2018.02.05 Dan [3.0]
;;;             : * Abstracting the access to event lists and access the event
;;;             :   internals with act-r-event-... and maintain the count for
;;;             :   when events are created.
;;;             : * Adjusting how real-time management works since it can't just
;;;             :   spin now and may also want to have a 'real' external clock 
;;;             :   instead of an interval one.
;;; 2018.02.07 Dan
;;;             : * Have mp-show-waiting include the wait condition since the
;;;             :   general event formatter doesn't now.
;;;             : * Add a 'user' version of mp-scheduled-events for use in some
;;;             :   module code (like procedural) which doesn't require actual
;;;             :   event access.
;;; 2018.02.23 Dan
;;;             : * Add a remote delete-event.
;;; 2018.02.26 Dan
;;;             : * Fixed a bug in the output of mp-show-waiting.
;;; 2018.06.13 Dan
;;;             : * Updated command doc strings to the current spec.
;;; 2018.07.30 Dan
;;;             : * Updated some of the comments and changed mp-real-time-management
;;;             :   back to keyword parameters.  When the external version is
;;;             :   eventually added it can use the options list for them.
;;; 2019.02.05 Dan
;;;             : * Adjustments because meta-p-models is now an alist.
;;; 2019.03.22 Dan
;;;             : * Next-scheduled-event is only called when the lock is held
;;;             :   so make that assumption and skip the lock.
;;; 2019.04.09 Dan
;;;             : * Adjust compare-events and the scheduling of waiting and
;;;             :   after events to use the new min and max slots for priority.
;;; 2019.04.15 Dan
;;;             : * Replace splice-into-list with splice-into-position-des and
;;;             :   don't setf with the returned value since it's destructive.
;;; 2019.11.08 Dan
;;;             : * Moved the *define-model-lock* definition here from model
;;;             :   and updated set-mp-running to check that lock and the 
;;;             :   corresponding variable so that it can prevent a run that's
;;;             :   in the body of a model definition.  The lock alone isn't
;;;             :   sufficient because some Lisps create recursive locks even
;;;             :   for make-lock.
;;; 2020.05.20 Dan
;;;             : * Added a :count 1 to most places that remove an event
;;;             :   (tried using delete instead of remove but didn't seem to
;;;             :   make a difference and actually looked like it made things
;;;             :   worse in some cases).
;;; 2020.08.25 Dan
;;;             : * Changed the declaim for format-event to avoid SBCL warning.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;; mp-time
;;; mp-time returns the current time of the current meta-process in seconds. 
;;;
;;; mp-models
;;; mp-models returns a list of the names of all the models defined in the current meta-process. 
;;; 
;;; mp-show-queue 
;;; mp-show-queue prints the events that are on the event queue of the current meta-process 
;;; to *standard-output* in the order that they would be executed.
;;; 
;;; mp-show-waiting
;;; mp-show-waiting prints the events that are in the waiting queue of the current meta-process 
;;; along with a description of the condition for which each needs to be added to the event queue to *standard-output*.
;;; 
;;; mp-print-versions
;;; mp-print-versions prints the version number of the framework and the name, 
;;; version number, and documentation of each module which is currently defined to *standard-output*.
;;; 
;;; (defun mp-real-time-management (&key (mode 'interval)
;;;                                           (time-function 'get-internal-real-time)
;;;                                           (units-per-second internal-time-units-per-second)
;;;                                           (slack-function 'real-time-slack))
;;;
;;; mp-real-time-management sets the function and divisor used to determine the
;;; current time in seconds when then real-time flag is specified to run the
;;; meta-process.  The slack function is called while the model is waiting for
;;; the time to advance when there is a discrepancy.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Using structs for now because I don't need the flexibility of CLOS classes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(declaim (ftype function format-event))
(declaim (ftype (function () t) global-modules-table))
(declaim (ftype (function () t) max-module-name-length))
(declaim (ftype (function (t) t) delete-model-fct))
(declaim (ftype (function (t) (values t t)) get-abstract-module))
(declaim (ftype (function () t) all-module-names))
(declaim (ftype (function (t) t) event-displayed-p))




(defun reset-mp (meta-process)
  "Set a meta-process to time 0 and clear the events"
  
  (clear-mp-running)
  
  (bt:with-recursive-lock-held ((meta-p-schedule-lock meta-process))
    (setf (meta-p-time meta-process) 0)
    (setf (meta-p-start-time meta-process) nil)
    (setf (meta-p-start-real-time meta-process) nil)
    (setf (meta-p-events meta-process) nil)
    (setf (meta-p-delayed meta-process) nil)
    (setf (meta-p-dynamics meta-process) nil)
    (setf (meta-p-temp-event meta-process) nil)
    
    (setf (meta-p-time-mode meta-process) 'interval)
    (setf (meta-p-time-function meta-process) 'get-internal-real-time)
    (setf (meta-p-units-per-second meta-process) internal-time-units-per-second)
    (setf (meta-p-slack-function meta-process) 'real-time-slack)
    
    
    
    
    (setf (meta-p-event-id meta-process) 0)))


(defun mp-running? ()
  (awhen (current-mp)
         (bt:with-lock-held ((meta-p-run-lock it))
           (meta-p-running it))))

(add-act-r-command "act-r-running-p" 'mp-running? "Predicate to check whether or not the system is currently running. No params.")

(defvar *define-model-lock* (bt:make-lock "define-model"))
(defvar *defining-model* nil)

(defun set-mp-running (warning)
  (awhen (current-mp)
         (let ((l (bt:acquire-lock *define-model-lock* nil)))
           (if (null l)
               (print-warning "Cannot run the system while a model is being defined or deleted.")
             (unwind-protect
                 (if *defining-model*
                     (print-warning "Cannot run the system while a model is being defined or deleted.")
                   (bt:with-lock-held ((meta-p-run-lock it))
                     (if (meta-p-running it)
                         (print-warning warning)
                       (setf (meta-p-running it) t))))
               (bt:release-lock *define-model-lock*))))))

(defun clear-mp-running ()
  (awhen (current-mp)
         (bt:with-lock-held ((meta-p-run-lock it))
           (setf (meta-p-running it) nil))))

(add-act-r-command "creating-model" nil "Signal for a model being created for monitoring purposes. Params: model-name." nil)

(defun add-new-model (name model-struct)
  (awhen (current-mp)
         
         (dispatch-apply "creating-model" name)
         
         (bt:with-lock-held ((meta-p-models-lock it))
           
           (push (cons name model-struct) (meta-p-models it))
           (push-last name (meta-p-model-order it))
           (incf (meta-p-model-count it))
           (when (> (length (symbol-name name)) (meta-p-model-name-len it))
             (setf (meta-p-model-name-len it) (length (symbol-name name))))
           
           (dolist (c (bt:with-lock-held ((meta-p-component-lock it)) (meta-p-component-list it)))
             (when (act-r-component-model-create (cdr c))
               (funcall (act-r-component-model-create (cdr c)) (act-r-component-instance (cdr c)) name)))
           
           (if (= 1 (meta-p-model-count it))
               (setf (meta-p-default-model it) model-struct)
             (setf (meta-p-default-model it) nil)))))


(add-act-r-command "deleting-model" nil "Signal for a model being deleted for monitoring purposes. Params: model-name." nil)

(defun remove-model (name)
  (awhen (current-mp)
         
         (dispatch-apply "deleting-model" name)
         
         (bt:with-lock-held ((meta-p-models-lock it))
           (setf (meta-p-models it) (remove name (meta-p-models it) :key 'car))
           (setf (meta-p-model-order it) (remove name (meta-p-model-order it)))
           (decf (meta-p-model-count it))
           
           (when (= (length (symbol-name name)) (meta-p-model-name-len it))
             (setf (meta-p-model-name-len it) 
               (apply 'max (cons 0 (mapcar (lambda (x) (length (symbol-name x))) (meta-p-model-order it))))))
           
           (if (= 1 (meta-p-model-count it))
               (setf (meta-p-default-model it) (cdr (assoc (first (meta-p-model-order it)) (meta-p-models it))))
             (setf (meta-p-default-model it) nil)))))

           
           
(defun mp-time ()
  "returns the current time of the current meta-process in seconds"
  (let ((mp (current-mp)))
    (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
      (ms->seconds (meta-p-time mp)))))

(add-act-r-command "mp-time" 'mp-time "Return the current simulation time in seconds. No params.")

(defun mp-time-ms ()
   (let ((mp (current-mp)))
    (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
      (meta-p-time mp))))

(add-act-r-command "mp-time-ms" 'mp-time-ms "Return the current simulation time in milliseconds. No params.")



(defun real-time-slack (current-time next-time)
  (declare (ignore current-time next-time))
  (bt:thread-yield))

(defun mp-real-time-management (&key (mode 'interval)
                                     (time-function 'get-internal-real-time)
                                     (units-per-second internal-time-units-per-second)
                                     (slack-function 'real-time-slack))
  (verify-current-mp  
   "mp-real-time-management called with no current meta-process."
   
   (cond ((not (or (eq mode 'interval) (eq mode 'absolute)))
          (print-warning "Mode ~s not a valid value for mp-real-time-management. Must be absolute or interval." mode))
         ((not (and time-function (local-or-remote-function-or-nil time-function)))
          (print-warning "Time-function ~s not a valid function for mp-real-time-management" time-function))
         ((not (posnum units-per-second))
          (print-warning "Units-per-second ~s must be a positive number" units-per-second))
         ((not (and slack-function (local-or-remote-function-or-nil slack-function)))
          (print-warning "Slack-function ~s not a valid function for mp-real-time-management" slack-function))
         ((mp-running?)
          (print-warning "Mp-real-time-management cannot adjust real-time operation while the model is running."))
         (t
          (let ((mp (current-mp)))
            (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
              (setf (meta-p-time-mode mp) mode)
              (setf (meta-p-time-function mp) time-function)
              (setf (meta-p-units-per-second mp) units-per-second)
              (setf (meta-p-slack-function mp) slack-function)
              ))
          t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Abstracting the access to the meta-process events because 
;;; I may want to change internal rep. at some point, and since 
;;; I need to update all the event accessors for remote access 
;;; might as well handle that now too.


(defun mp-scheduled-events (mp) ;; Returns a list of events
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    (copy-list (meta-p-events mp))))


;; A "user" version where user is really me but in code that's
;; outside of the framework, like procedural.

(defun scheduled-events ()
  (let ((mp (current-mp)))
    (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
      (mapcar 'act-r-event-num (meta-p-events mp)))))

(defun mp-delayed-events (mp) ;; Returns a list of events
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    (copy-list (meta-p-delayed mp))))


(defun mp-all-events (mp)
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    (append (meta-p-events mp) (meta-p-delayed mp))))
  

(defun get-event-by-id (id)
  (let ((mp (current-mp))) 
    (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
      (or (find id (meta-p-events mp) :key 'act-r-event-num :test '=)
          (find id (meta-p-delayed mp) :key 'act-r-event-num :test '=)
          (and (meta-p-temp-event mp) (= id (act-r-event-num (meta-p-temp-event mp)))
               (meta-p-temp-event mp))))))
  


(defun delete-all-model-events (mp name)
  (let ((model (get-model name)))
    (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
      (setf (meta-p-events mp)
        (remove model (meta-p-events mp) :key #'act-r-event-model))
      
      (setf (meta-p-delayed mp)
        (remove model (meta-p-delayed mp) :key #'act-r-event-model))
      
      (setf (meta-p-dynamics mp)
        (remove model (meta-p-dynamics mp) :key #'(lambda (x) (act-r-event-model (car x))))))))


(defun next-scheduled-event (mp)
  ;; assume this is true! (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
  
  (first (meta-p-events mp)))


(defun remove-scheduled-event (mp event)
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    (setf (meta-p-events mp) (remove event (meta-p-events mp) :test 'eq :count 1))
    (when (act-r-event-dynamic event)
      (setf (meta-p-dynamics mp) (remove event (meta-p-dynamics mp) :key 'car :test 'eq :count 1)))
    (setf (meta-p-temp-event mp) event)))

(defun remove-tagged-events (mp tag &key (key 'act-r-event-num) (test '=) (count 1))
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    (setf (meta-p-events mp)
      (remove tag (meta-p-events mp) :key key :test test :count count))))




;;; insert-queue-event
;;;
;;; This function takes two parameter which are a meta-process and an
;;; event.  The event is added to the list of scheduled events for the
;;; meta-process in order based on its time and priority.
;;;
;;; Items are ordered by time first, lower times occuring before higher
;;; times.  If two events have the same time then:
;;;
;;; An event with priority :max occurs before any event in the queue with
;;;                             the same time except existing events of :max
;;;                             priority
;;; An event with a numeric priority occurs before an event with a lesser
;;;                             numbered priority or a priority of :min
;;; An event with priority :min occurs after any events at that time
;;;

(defun insert-queue-event (mp event &key now delta)
  "Place an event into the scheduled-events list maintaning the ordering"
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    
    (unless (act-r-event-num event)
      (setf (act-r-event-num event) (incf (meta-p-event-id mp))))
    
    (when (or now delta)
      (setf (act-r-event-mstime event)
        (if now
            (meta-p-time mp)
          (+ (meta-p-time mp) delta))))
    
    (when (< (act-r-event-mstime event) (meta-p-time mp))
      (print-warning "Event with action ~s and time ~s specifies a time that's already past.  Scheduling for current time ~s."
                     (act-r-event-action event) (act-r-event-mstime event) (mp-time))
      (setf (act-r-event-mstime event) (meta-p-time mp)))
    
    (if (null (meta-p-events mp))
        (push event (meta-p-events mp))
      (do* ((pos 0 (1+ pos))
            (queue (meta-p-events mp) (cdr queue)))
           ((or (null queue)
                (compare-events event (car queue)))
            (splice-into-position-des (meta-p-events mp) pos event))))
    
    (when (or (meta-p-delayed mp) (meta-p-dynamics mp))
      (update-waiting-events mp event))))



(defun insert-final-queue-event (mp event)
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    
    (unless (act-r-event-num event)
      (setf (act-r-event-num event) (incf (meta-p-event-id mp))))
    
    (setf (act-r-event-mstime event) 
      (if (meta-p-events mp)
          (act-r-event-mstime (car (last (meta-p-events mp))))
        (meta-p-time mp)))
    
    (push-last event (meta-p-events mp))))


;;; This comparison rule is now the specified operation!
;;; If this is changed for some reason the same ordering needs
;;; to be maintained -- always schedule the new event after 
;;; existing events with the same priority.

(defun compare-events (new-event old-event)
  (or (< (act-r-event-mstime new-event) (act-r-event-mstime old-event))
      (and (= (act-r-event-mstime new-event) (act-r-event-mstime old-event))
           (null (act-r-event-max old-event))
           (null (act-r-event-min new-event))
           (or (act-r-event-max new-event)
               (act-r-event-min old-event)
               (and (> (act-r-event-priority new-event) 
                       (act-r-event-priority old-event)))))))




;;; update-waiting-events
;;;
;;; This function takes two parameters, a meta-process and an event.  The 
;;; list of waiting events for the meta-process is checked to see if the
;;; event specified allows any of them to be added to the scheduled events.
;;; If an event can be moved to the scheduled events list it is removed
;;; from the waiting events list and it is added to the scheduled events
;;; list (which will call update-waiting-events to test whether that event
;;; frees others from the waiting list).
;;; The list of dynamic events is also tested in the same way as the
;;; waiting events.

(defun update-waiting-events (mp new-event)
  "Check the list of waiting events to see if a new event allows any to run"
  (let ((moved-events nil))
    ; assumed since it's only called from insert-queue-event (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    
    (dolist (event (meta-p-delayed mp))
      (when (conditions-met event new-event)
        (setf (meta-p-delayed mp) (remove event (meta-p-delayed mp) :count 1))
        (unless (act-r-event-dynamic event)
          (setf (act-r-event-wait-condition event) nil))
        (push event moved-events)))
    
    (dolist (event-check (meta-p-dynamics mp))
      (let ((event (car event-check))
            (cur-time (cdr event-check)))
        (when (and (< (act-r-event-mstime new-event) cur-time)
                   (conditions-met event new-event))
          
          (setf (meta-p-events mp) (remove event (meta-p-events mp) :count 1))
               
          (setf (meta-p-dynamics mp) (remove event-check (meta-p-dynamics mp) :count 1))
          
          (push event moved-events))))
    
    (dolist (event moved-events)
      (setf (act-r-event-mstime event) (act-r-event-mstime new-event))
      (setf (act-r-event-min event) :min)
      (insert-queue-event mp event))
    
    (dolist (event moved-events)
      (when (act-r-event-dynamic event)
        (push (cons event (act-r-event-mstime new-event)) (meta-p-dynamics mp))))))

;;; conditions-met
;;;
;;; This function takes two parameters which are a waiting-event and a
;;; new-event.  

(defun conditions-met (w-event new-event)
  "Test whether a waiting event's wait-for reason is met by a new event"
  (case (car (act-r-event-wait-condition w-event))
    (:any (and (eq (act-r-event-model w-event) (act-r-event-model new-event))
               (or (second (act-r-event-wait-condition w-event))
                   (not (act-r-maintenance-event-p new-event)))))
    
    (:module (and (or (eq (act-r-event-model w-event) (act-r-event-model new-event))
                      (null (act-r-event-model w-event)))
                  (eq (second (act-r-event-wait-condition w-event)) 
                      (act-r-event-module new-event))
                  (or (third (act-r-event-wait-condition w-event))
                   (not (act-r-maintenance-event-p new-event)))))
    (t nil)))




(defun insert-waiting-event (mp new-event delay)
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    (let ((matching-event (find-if (lambda (x)
                                     (conditions-met new-event x))
                                   (meta-p-events mp))))
      
      (cond (matching-event 
             (setf (act-r-event-mstime new-event) 
               (act-r-event-mstime matching-event))
             
             (unless (act-r-event-dynamic new-event)
               (setf (act-r-event-wait-condition new-event) nil))
                             
             (insert-queue-event mp new-event)
             
             (when (act-r-event-dynamic new-event)
               (push (cons new-event (act-r-event-mstime new-event)) (meta-p-dynamics mp)))
             
             (values (act-r-event-num new-event) (if (act-r-event-wait-condition new-event) t nil)))
            ((null delay)
             (setf (act-r-event-mstime new-event) (meta-p-time mp))
             (setf (act-r-event-max new-event) :max)
             (setf (act-r-event-wait-condition new-event) nil)
             (setf (act-r-event-dynamic new-event) nil)
             
             (insert-queue-event mp new-event)
             
             (values (act-r-event-num new-event) nil))
            ((eq delay :abort)
             (values nil :abort))
            (t
             (setf (act-r-event-num new-event) (incf (meta-p-event-id mp)))
             (push new-event (meta-p-delayed mp))
             (values (act-r-event-num new-event) t))))))


(defun delete-event (event-num)
  (cond ((not (numberp event-num)) (print-warning "~S is not a valid event identifier." event-num))
        (t
         (let ((mp (current-mp)))
           (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
             (let ((l (+ (length (meta-p-events mp)) (length (meta-p-dynamics mp)) (length (meta-p-delayed mp)))))
               (setf (meta-p-events mp) (remove event-num (meta-p-events mp) :key 'act-r-event-num :test '= :count 1))
               
               (setf (meta-p-dynamics mp) (remove event-num (meta-p-dynamics mp) :key (lambda (x) (act-r-event-num (car x))) :test '= :count 1))
               
               (setf (meta-p-delayed mp) (remove event-num (meta-p-delayed mp) :key 'act-r-event-num :test '= :count 1))
               
               (not (= l (+ (length (meta-p-events mp)) (length (meta-p-dynamics mp)) (length (meta-p-delayed mp)))))))))))


(add-act-r-command "delete-event" 'delete-event "Remove an event which has been scheduled. Params: event-id")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun mp-models ()  
  "returns a list of the names of all the models in the current meta-process"
  (let ((mp (current-mp)))
    (bt:with-lock-held ((meta-p-models-lock mp))
      (meta-p-model-order mp))))

(add-act-r-command "mp-models" 'mp-models "Return a list of all existing model names. No params." nil)

(defun mp-show-queue (&optional indicate-traced)
  (let ((events (mp-scheduled-events (current-mp))))
    (act-r-output "Events in the queue:")
    (dolist (evt events (length events))
      (act-r-output "~:[~*~;~:[ ~;*~]~]~A" indicate-traced (event-displayed-p evt) (format-event evt)))))

(add-act-r-command "mp-show-queue" 'mp-show-queue "Print out a description of the events currently scheduled to occur optionally indicating which will show in the trace. Params: {mark-traced?}")


(defun mp-queue-text (&optional indicate-traced)
  (let ((s (make-string-output-stream))
        (events (mp-scheduled-events (current-mp))))
    (dolist (evt events)
      (format s "~:[~*~;~:[ ~;*~]~]~A~%" indicate-traced (event-displayed-p evt) (format-event evt)))
    (prog1
        (get-output-stream-string s)
      (close s))))

(add-act-r-command "mp-queue-text" 'mp-queue-text "Return the text contianing a description of the events currently scheduled to occur optionally indicating which will show in the trace. Params: {mark-traced?}")



(defun mp-queue-count ()
  (let ((events (mp-scheduled-events (current-mp))))
    (length events)))


(add-act-r-command "mp-queue-count" 'mp-queue-count "Return the number of the events currently scheduled to occur. No params.")


(defun mp-show-waiting ()
  (let ((events (mp-delayed-events (current-mp))))
    (act-r-output "Events waiting to be scheduled:~%")
    (dolist (evt events (length events))
      (act-r-output "~A Waiting for: ~s~%" (format-event evt) (act-r-event-wait-condition evt)))))

(add-act-r-command "mp-show-waiting" 'mp-show-waiting "Print out a description of the events which are currently waiting to be scheduled. No params.")

(defun mp-modules-events (module)
  (let ((events (mp-all-events (current-mp)))
        (results nil))
    (dolist (e events results)
      (when (eq module (act-r-event-module e))
        (push (act-r-event-num e) results)))))
     
(defun mp-print-versions ()
  (let ((mp (current-mp)))
    (act-r-output "ACT-R ~a Version Information:~%~va: ~10a ~a" *actr-architecture-version*
      (max (max-module-name-length) 10)
      "Software"
      (meta-p-version mp)
      (meta-p-documentation mp))
    
    (act-r-output "=================================================")
    
    (bt:with-lock-held ((meta-p-component-lock mp))
      (when (meta-p-component-list mp)
        (act-r-output "Components")
        (act-r-output "-------------------------------------------------")
        (dolist (c (meta-p-component-list mp))
          (act-r-output "~va: ~10a ~a"
            (max (meta-p-component-length mp) 10)
            (car c)
            (act-r-component-version (cdr c))
            (act-r-component-documentation (cdr c))))
        (act-r-output "=================================================")))
    
    
    (act-r-output "Modules")
    (act-r-output "-------------------------------------------------")
    (dolist (name (all-module-names))
      (let ((module (get-abstract-module name)))
        (act-r-output "~va: ~10a ~a"
          (max (max-module-name-length) 10)
          name
          (act-r-module-version module)
          (act-r-module-documentation module))))))

(add-act-r-command "mp-print-versions" 'mp-print-versions "Print out the version information for the overall software and all the components and modules. No params.")

;;;;;; Support for components
;;;;;; Since they're defined at the meta-process level putting
;;;;;; them in this file instead of a separate one.

(defmacro define-component (name &key (version nil) (documentation nil)
                                 (creation nil) (delete nil) 
                                 (clear-all nil) (create-model nil) (delete-model nil)
                                 (before-reset nil) (after-reset nil))
  `(define-component-fct ',name ,@(when version `(:version ,version))
     ,@(when documentation `(:documentation ,documentation))
     :creation ',creation :delete ',delete :clear-all ',clear-all 
     :create-model ',create-model :delete-model ',delete-model
     :before-reset ',before-reset :after-reset ',after-reset))


(defun define-component-fct (name &key (version "" version?) (documentation "" docs?)
                                  creation delete clear-all create-model delete-model
                                  before-reset after-reset)
  (let ((mp (current-mp)))
    (unless (and version? docs?)
      (print-warning "Components should always provide a version and documentation string."))
    
    (cond ((mp-models)
           (print-warning "Cannot create a new component when there are models defined."))
          ((null name)
           (print-warning "Nil is not a valid component name. No component defined."))
          ((not (symbolp name))
           (print-warning "~s is not a symbol and thus not a valid component name.~%No component defined." name))
          ((assoc name (bt:with-lock-held ((meta-p-component-lock mp))
                         (meta-p-component-list mp)))
           (print-warning "Componenet named ~S already exists.  Delete it with undefine-component if you want to redefine it." name))
          
          ((not (and (fctornil creation) (fctornil delete) (fctornil clear-all)
                     (fctornil create-model) (fctornil delete-model)
                     (fctornil before-reset) (fctornil after-reset)))
           (print-warning "Invalid parameter for a component call-back function")
           (do ((items (list creation delete clear-all create-model delete-model before-reset after-reset)
                       (cdr items))
                (names '(creation delete clear-all create-model delete-model before-reset after-reset)
                       (cdr names)))
               ((null items))
             (unless (fctornil (car items))
               (print-warning "Parameter: ~s is not a function, function name, or nil" (car names))))
           (print-warning "Component ~s not defined" name))
          (t
           (let* ((instance (when creation
                              (funcall creation)))
                  (component (make-act-r-component :name name
                                                   :instance instance
                                                   :destroy delete
                                                   :clear-all clear-all
                                                   :model-create create-model
                                                   :model-destroy delete-model
                                                   :before-reset before-reset
                                                   :after-reset after-reset
                                                   :version version
                                                   :documentation documentation)))
             (bt:with-lock-held ((meta-p-component-lock mp))
               (setf (meta-p-component-list mp) (sort (push (cons name component) (meta-p-component-list mp)) 'string< :key (lambda (x) (symbol-name (car x)))))
               
               
               (when (> (length (format nil "~S" name)) (meta-p-component-length mp))
                 (setf (meta-p-component-length mp) (length (format nil "~S" name)))))
             
             name)))))


(defmacro undefine-component (name)
  `(undefine-component-fct ',name))

(defun undefine-component-fct (name)
  (let ((mp (current-mp)))
    (cond ((mp-models)
           (print-warning "Cannot delete a component when there are models defined."))
          ((not (assoc name (bt:with-lock-held ((meta-p-component-lock mp)) (meta-p-component-list mp))))
           (print-warning "~S is not the name of a currently defined component." name))
          (t
           (bt:with-lock-held ((meta-p-component-lock mp))
             (let ((c (cdr (assoc name (meta-p-component-list mp)))))
               
               (when (act-r-component-destroy c)
                 (funcall (act-r-component-destroy c) (act-r-component-instance c)))
               
               (setf (meta-p-component-list mp) (remove name (meta-p-component-list mp) :key 'car))
               
               (when (= (length (format nil "~S" name)) (meta-p-component-length mp))
                 (setf (meta-p-component-length mp)
                   (if (meta-p-component-list mp)
                       (apply #'max (mapcar #'(lambda (x)
                                                (length (format nil "~S" (car x))))
                                      (meta-p-component-list mp)))
                     0)))))
             t))))

(defmacro get-component (name)
  `(get-component-fct ',name))

(defun get-component-fct (name)
  (let ((mp (current-mp)))
    (aif (assoc name (bt:with-lock-held ((meta-p-component-lock mp))
                       (meta-p-component-list mp)))
       (values (act-r-component-instance (cdr it)) t)
         (values (print-warning "~s does not name a current component." name) nil))))





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
