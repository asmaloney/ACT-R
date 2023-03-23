;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : stepper-control.lisp
;;; Version     : 7.0
;;; 
;;; Description : No system dependent code.
;;;             : This file contains the Lisp to support the stepper window.
;;;             : 
;;; Bugs        : [ ] Display more info in the cases when data wasn't recorded
;;;             :     2021.01.27 fix at least stopped the warning
;;; 
;;; Todo        : [ ] Extend the possible information which can be recorded
;;;             :     with some sort of action triggers and data hooks.
;;; 
;;; ----- History -----
;;;
;;; 05/24/2002  Dan
;;;             : File creation
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Modified the stepper-control-function to use the
;;;             : new uni-wait-for function.
;;; 10/14/2002  Dan
;;;             : Made the changes to implement the tutor mode
;;;             : in the instantiation window instead of the bindings
;;;             : window of the stepper.
;;; 11/11/2002  Dan
;;;             : Modified stepper-instan-info and stepper-control-function
;;;             : so that instantiation picking was possible.  It works
;;;             : off of the step buttons value and the *last-stepper-instantiation*
;;;             : variable.  Requires that the option be set in the
;;;             : environment before it's enabled.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;;             : Updated run-master-process with the RPM 2.2 version.
;;; -----------------------------------------------------------------------
;;; 2005.04.13  Dan
;;;             : * Moved to ACT-R 6.
;;;             : * LOTS of things to change - not included yet.
;;; 2005.04.20  Dan
;;;             : * Updated and added - should work with the tutorial mode.
;;; 2005.05.14 Dan
;;;             : * Fixed a typo in the stepper window - it was printing
;;;             :   +retreval> for retrieval requests...
;;; 2005.08.10 Dan
;;;             : * Minor clean-up to declare event unused in stepper-test.
;;; 2006.03.10 Dan
;;;             : * Calls to get-production no longer need the procedural
;;;             :   module so took that out of stepper-instan-binding.
;;; 2007.07.13 Dan
;;;             : * Added the stepper-stop-button function because the
;;;             :   stop button is being put back on the stepper.
;;; 2007.08.03 Dan
;;;             : * Moved the *stepper-open* defvar to environment-cmds
;;;             :   because it's used there and that's loaded first...
;;; 2007.08.07 Dan
;;;             : * When :esc is set to t the stepper now shows the 
;;;             :   declarative or procedural parameters for the 
;;;             :   item in a new window in the lower left.
;;; 2007.08.08 Dan
;;;             : * Put the "run until" button back into the stepper and
;;;             :   added module as an option now too.
;;; 2007.08.15 Dan
;;;             : * The chunk list shown in the stepper is now sorted
;;;             :   by activation with the chunk being retrieved at the top.
;;; 2011.04.20 Dan
;;;             : * Changed the evt-time call to an evt-mstime one and
;;;             :   converted the user time to ms.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.05.25 Dan
;;;             : * Fixed a bug in the tutor-completed function.
;;; 2011.05.26 Dan 
;;;             : * Protected some code that assumed there was a model for
;;;             :   eventual use with multiple models.
;;; 2011.05.31 Dan
;;;             : * Removed a declaration from tutor-completed.
;;;             : * Changed stepper-test so that when it's "until" a production
;;;             :   that name can be from any of the defined models.
;;; 2012.03.01 Dan
;;;             : * Added the macro suppress-declarative-noise which will
;;;             :   temporarily set :ans to nil so that when sdp gets called
;;;             :   from the stepper it doesn't affect the current random
;;;             :   stream because otherwise it changes the results for a
;;;             :   fixed seed model when stepping through it and that makes
;;;             :   debugging more difficult.
;;; 2012.03.21 Dan
;;;             : * Instead of suppress-declarative-noise I can just use 
;;;             :   with-parameters to temporarily set :ans to nil.
;;; 2012.10.25 Dan
;;;             : * Fixed the stepper so it doesn't step when :v is nil, which
;;;             :   is what it's documented as doing i.e. only stepping on
;;;             :   events that show in the trace.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2017.08.11 Dan
;;;             : * Protect access to production-bindings.
;;; 2017.08.28 Dan [4.0]
;;;             : * Complete reworking to not use the hander mechanism and
;;;             :   instead provide functions for the stepper to call and
;;;             :   call stepper side functions as needed.
;;; 2017.08.29 Dan
;;;             : * Continuing the rewrite to complete the stepper tool.
;;;             : * Using a module for it now to more easily handle the
;;;             :   information necessary when multiple models are defined.
;;; 2017.08.31 Dan
;;;             : * Simplified this from all the cross-talk that the old
;;;             :   handler interface had and basically just need the hook
;;;             :   functions, stuff for parsing the events, and things to
;;;             :   deal with multiple models on the Lisp side now.
;;; 2017.10.06 Dan
;;;             : * Need to know if tutor mode is off or on because when it's
;;;             :   on it needs to stop on production-selection events even if
;;;             :   they aren't displayed in the trace.
;;; 2018.08.08 Dan
;;;             : * Have to print the retrieval chunk-spec during the hook
;;;             :   because declaritive frees it when its done.
;;; 2018.12.03 Dan
;;;             : * Need to add a check for whether a varaible is the name of
;;;             :   a buffer for the update when in tutor mode and add extra 
;;;             :   quotes around a binding which is a string.
;;; 2019.05.31 Dan [5.0]
;;;             : * Add the ability to step on all events regardless of :v
;;;             :   setting.  Goes along with the ability to open the stepper
;;;             :   while ACT-R is running now.
;;; 2019.08.02 Dan [6.0]
;;;             : * Reworking this so that it's a general interface instead
;;;             :   of tied to the current Stepper tool.
;;;             :  - use a component so clear-all handled nicely
;;;             :  - can be started from anywhere
;;;             :  - only one ever allowed
;;;             :  - only the starter can manipulate it (given an id)
;;;             :  - there is always a waiting loop on the ACT-R side, but
;;;             :    one could wait on the other side too by not responding
;;;             :  - removed the tutor info slots since tutoring is all on
;;;             :    the other side
;;; 2019.08.12 Dan
;;;             : * Check if there is an event for the update-stepper action.
;;; 2019.08.14 Dan
;;;             : * Need to clear the skip type and value when a new stepper
;;;             :   is started.
;;; 2019.08.15 Dan
;;;             : * Also need to clear the current-event and the stepped-event-
;;;             :   details when reset occurs.
;;; 2019.08.19 Dan
;;;             : * Added a declare to update-stepper-info to avoid warnings.
;;; 2020.01.14 Dan [6.1]
;;;             : * Use the new (:remove <>) option for hook functions.
;;; 2020.08.19 Dan
;;;             : * Added a handler-case to start-stepper so that if there is
;;;             :   an error it doesn't leave the stepper open but unusable, and
;;;             :   then a bunch of ignore-errors in the remove-stepper-controls
;;;             :   so it can clean things up safely despite whatever caused the
;;;             :   error.
;;; 2020.10.29 Dan [6.2]
;;;             : * Use suppress-undelay-cr and unsuppress-undelay-cr so that
;;;             :   opening and closing the stepper doesn't potentially lead to
;;;             :   scheduling a new conflict-resolution.
;;; 2021.01.27 Dan
;;;             : * Added a safety check to displaying the production-fired
;;;             :   details since it's possible that the stepper wasn't open
;;;             :   when the production was selected and thus there's no info
;;;             :   recorded.  In that case it will just print the name in the
;;;             :   list, but none of the details.
;;; 2021.11.01 Dan [7.0]
;;;             : * Allow for other tutoring possibilities.  The update function
;;;             :   is passed the name of the tutored event if it is one instead
;;;             :   of just t as was the case before when there was only one 
;;;             :   possible tutoring event.
;;;             : * If "update-stepper" is passed nil then it returns one item  
;;;             :   which is any additional information recorded for tutoring
;;;             :   purposes at the time of the event.
;;;             : * Now saves the production conditions text and whynot info 
;;;             :   for conflict-resolution tutoring and the printed DM chunk 
;;;             :   info for start-retrieval (the retrieval request is already
;;;             :   saved for display purposes).  When asked for the tutor data
;;;             :   c-r sends a list of two element lists where the first item
;;;             :   in a list is a production's condition text and the second
;;;             :   is the whynot reason for that production or an empty string
;;;             :   if it does match, and s-r sends a list of two items where 
;;;             :   the first item is the text of the retrieval request and the
;;;             :   second is a list of two element lists for each chunk in DM
;;;             :   with the first item being the printed chunk string and the
;;;             :   second being t if the chunk matches and nil if it does not.
;;; 2021.11.05 Dan
;;;             : * Always step on start-retrieval and conflict-resolution when
;;;             :   tutoring is enabled, just like production-selected.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Provide a set of commands that can be used to provide an interactive
;;; stepper through the remote interface that will provide the retrieval and
;;; procedural details.  Only one stepper can be active at a time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;; 
;;; "start-stepper"
;;;
;;;  Ask to start an interactive stepper session.  Requires 4 parameters:
;;;   step-all? - a boolean indicating whether it should stop on all events
;;;               or only those displayed in the trace (can be changed later)
;;;   tutor-mode? - a boolean indicating whether it should stop on production-
;;;                 selected events even if they aren't shown in the trace (can
;;;                 be changed later)
;;;   next-event-cmd - a command which will be called to indicate that there is
;;;                    an event step.  It will be passed one parameter which is
;;;                    the format-string of the event.  This is called before the
;;;                    event has occurred.
;;;                    
;;;   update-cmd - a command which will be called after a stepped event has been
;;;                executed.  It will be passed seven parameters: 
;;;                  event - format-event string of the event 
;;;                  items - a list of items for which there may be additional
;;;                          details available (currently retrieved chunks and
;;;                          selected or fired productions)
;;;                  tutorable - a generalized boolean indicating whether or not
;;;                              this is a tutorable item with a true value 
;;;                              indicating the event type
;;;                  item-label - a string naming the type of items provided
;;;                  features-1 - a string indicating the information provided
;;;                               in the first value of an update
;;;                  features-2 - a string indicating the information provided
;;;                               in the second value of an update
;;;                  features-3 - a string indicating the information provided
;;;                               in the third value of an update
;;;               
;;;  It returns two values.  The first is a boolean indicating whether a new
;;;  stepper could be started (only one stepper allowed at a time) and the second
;;;  is a string.  If the stepper was started, the string is an id which must be
;;;  provided to interact with that stepper, and if the stepper was not started
;;;  it indicates the reason why not.  
;;;
;;;  Once started, the stepper will remain active until stopped or a clear-all
;;;  occurs (it will persist across a reset).
;;;
;;; "set-stepper-ready"
;;;
;;;  Indicate that an open stepper should start pausing the run.  Requires one
;;;  parameter which is the id of a started stepper.  The stepper interface does
;;;  not pause on any events until this command is called.  If the id matches the
;;;  currently open stepper, then it will now pause on the events which occur.
;;;  If the stepper was successfully readied then it returns t otherwise it
;;;  returns nil.  The reason for the extra step is because one may need to
;;;  perform some additional setup after opening a stepper before being able to
;;;  actually process the information e.g. opening a window to display the 
;;;  information and get user input which wouldn't happen if opening the stepper
;;;  failed.  
;;;  
;;;  
;;; "stop-stepper"
;;;
;;;  Stop using a stepper that was started.  Requires one parameter which is the
;;;  id string returned by start-stepper.  If that id matches the currently open
;;;  stepper interface then it is removed and the run is allowed to continue as
;;;  normal.  Returns a boolean indicating whether the stepper was stopped or not.
;;;
;;; "step-stepper"
;;;
;;;  Advance the stepper control interface when it is waiting.  It requires one
;;;  parameter, and accepts three optional parameters.  The required parameter is
;;;  the id string returned by start-stepper.  If the id string matches the
;;;  currently open stepper it may stop waiting on the current event, depending
;;;  upon the optional parameter values.  If none of the optional parameters are
;;;  given, then the current event will be allowed to occur and the run will
;;;  continue until the next stepped event or it completes.  The first optional 
;;;  parameter is a boolean indicating whether or not the current run should be
;;;  termianted instead of allowing the event to occur.  If it is provided as
;;;  a true value, then the run will be terminated before executing the current
;;;  event.  If the value is false, then a normal step will occur.  The other
;;;  two optional parameters can be used to specify a condition for the next
;;;  event at which to step instead of the next valid event.  The third must
;;;  be one of the strings: "production", "time", or "module", to indicate the
;;;  constraint used to test for the next step and the fourth must indicate a 
;;;  valid value for the specified constraint step.  If the constraint is 
;;;  production, then the value must be a string which names a production in
;;;  a current model and the stepper will allow the system to run until a
;;;  production with that name is selected or fired.  If the constraint is
;;;  time, then the value must be a number indicating a time in seconds and
;;;  the stepper will allow the run to occur until an event with the given
;;;  time (or greater) occurs.  If the constraint is module, then the value
;;;  must be a string naming a module in the system and the stepper will allow
;;;  the run to occur until an event which has that module designation.  
;;;
;;;  The command returns two values.  The first is a boolean indicating whether
;;;  or not the provided parameters were valid (the id and constraint and value
;;;  if specified).  If they were valid the second will be an empty string, 
;;;  but if an invalid value is given the second will be a string indicating
;;;  why something was invalid.
;;;
;;; "update-stepper"
;;; 
;;;  This command provides additional details for the items given to the update
;;;  command provided when the stepper was started.  It requires one parameter
;;;  which should be one of the items in the list passed to the update command.
;;;  
;;;  If it is passed an item from the list (which is either a chunk name or
;;;  production name since those are the only lists provided at this time),
;;;  then it returns four values.  The first two are strings with information 
;;;  for that item corresponding to the first two labels provided to the update.  
;;;  Currently, for production items those are the parameters and variable 
;;;  bindings, and for chunk items they are the chunk parameters and the 
;;;  retrieval request.  The third item provides a string for the third label.
;;;  For a production item it is the printed production if it is a production-
;;;  selected event or the tutor mode is enabled otherwise it is the printed
;;;  instantiatiation of that production, and for a chunk item it is a string
;;;  of the printed chunk.  The fourth value is additional information about 
;;;  the item.  For chunk items it is nil, and for production items it is a
;;;  list of lists related to the variable bindings.  There will be one list
;;;  for each variable with three items: the first is a string of the variable
;;;  itself, the second is the value that variable has in the production 
;;;  instantiation (which will be an encoded string i.e. if the value itself
;;;  is a string it will have a set of single quote characters around it) and
;;;  the third is a boolean indicating whether or not the variable is the name
;;;  of a buffer in the production.
;;;  
;;;  If it is passed a value of nil then it will return one value which is 
;;;  any additional tutoring information for the current situation.
;;;
;;; "set-stepper-tutoring"
;;;  
;;;  This command can be used to update whether the stepper is set to tutoring
;;;  mode or not (in tutoring model it stops on production-selected actions
;;;  even if they are not shown in the trace).  It requires two parameters.
;;;  The first is the id of the stepper and the second is a boolean indicating
;;;  whether tutoring mode should be enabled or not.  If the id is valid then
;;;  the tutoring mode is set as indicated and t is returned, otherwise it
;;;  returns nil.
;;;
;;; "set-stepper-step-all"
;;;  
;;;  This command can be used to update whether the stepper is set to step on
;;;  all events (regardless of their output mode) or not.  It requires two 
;;;  parameters.  The first is the id of the stepper and the second is a boolean
;;;  indicating whether the stepper should step all events or not.  If the id is
;;;  valid then the stepping mode is set as indicated and t is returned, 
;;;  otherwise it returns nil.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;;
;;; Since there are a lot of hooks needed to record the information and interact
;;; with the event queue that should probably all be on the "inside".  The early
;;; versions of this were tied directly to the Tcl/Tk Environment and had a lot
;;; of odd interdependencies and representation choices.
;;;
;;; Now things are cleaned up to use the normal remote interface.
;;; It also depends upon the remote interface to keep track of whether a stepper
;;; is still needed.  If the commands provided when starting the stepper are no
;;; longer available then it will terminate the stepper.  That's handled by a
;;; thread that just checks every 1/4 second and stops stepping if they aren't
;;; there (since run time performance isn't an issue when stepping that doesn't
;;; seem like a problem).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct stepper-control
  (lock (bt:make-recursive-lock "stepper-lock"))
  open pre-hook post-hook skip-type skip-val tutoring current-event stepped-event-details step-all next-cmd update-cmd waiting step-val
  (step-lock (bt:make-lock)) (step-cv (bt:make-condition-variable))
  safety-thread
  ready)

(defvar *stepper* (make-stepper-control))

(defun stepper-cleared (stepper)
  (bt:with-recursive-lock-held ((stepper-control-lock stepper))
    (setf (stepper-control-open stepper) nil)))

(define-component-fct :stepper 
  :version "7.0"   
  :documentation "Stepper interface global information"
  :creation (lambda () *stepper*)
  :clear-all 'stepper-cleared)

    
(defstruct stepper-module rr rs cs)

(defun reset-stepper-module (instance)
  (setf (stepper-module-rr instance) nil)
  (setf (stepper-module-rs instance) nil)
  (setf (stepper-module-cs instance) nil)
  
  (let (open)
    
    (bt:with-recursive-lock-held ((stepper-control-lock *stepper*)) 
      (setf open (stepper-control-open *stepper*))
      (setf (stepper-control-stepped-event-details *stepper*) nil)
      (setf (stepper-control-current-event *stepper*) nil))
    
  (when open
    
    (no-output
     (unless (find 'stepper-rr-hook (car (sgp :retrieval-request-hook)))
       (sgp :retrieval-request-hook stepper-rr-hook))
     (unless (find 'stepper-rs-hook (car (sgp :retrieval-set-hook)))
       (sgp :retrieval-set-hook stepper-rs-hook))
     
     (unwind-protect
         (progn
           (suppress-undelay-cr) 
           (unless (find 'stepper-cs-hook (car (sgp :conflict-set-hook)))
             (sgp :conflict-set-hook stepper-cs-hook)))
       (unsuppress-undelay-cr))))))

(defun create-stepper-module (name)
  (declare (ignore name))
  (make-stepper-module))

(define-module stepper nil nil 
  :creation create-stepper-module 
  :reset (nil reset-stepper-module) 
  :version 7.0 
  :documentation "Store the model specific information for the stepper interface.")



(defun start-stepper (step-all tutor-mode next-event-fn update-fn)
  (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
    (if (stepper-control-open *stepper*)
        (values nil "There is already a stepper open")
      (cond ((not (check-act-r-command next-event-fn))
             (values nil (format nil "The next-event-fn given ~s did not name a valid command." next-event-fn)))
            ((not (check-act-r-command update-fn))
             (values nil (format nil "The update-fn given ~s did not name a valid command." update-fn)))
            ((and (null step-all) (every (lambda (x) (with-model-eval x (null (car (no-output (sgp :v)))))) (mp-models)))
             (values nil "Step-all not enabled and all models have :v set to nil."))
            (t
             (handler-case
                 (progn
                   
                   
                   (setf (stepper-control-step-all *stepper*) step-all)
                   (setf (stepper-control-tutoring *stepper*) tutor-mode)
                   (setf (stepper-control-next-cmd *stepper*) next-event-fn)
                   (setf (stepper-control-update-cmd *stepper*) update-fn)
                   (setf (stepper-control-current-event *stepper*) nil)
                   (setf (stepper-control-stepped-event-details *stepper*) nil)
                   (setf (stepper-control-ready *stepper*) nil)
                   (setf (stepper-control-skip-type *stepper*) nil)
                   (setf (stepper-control-skip-val *stepper*) nil)
                   
                   (setf (stepper-control-open *stepper*) (symbol-name (new-symbol "stepper")))
                   
                   (setf (stepper-control-safety-thread *stepper*)
                     (let ((id (stepper-control-open *stepper*))
                           (next next-event-fn)
                           (update update-fn))
                       (bt:make-thread (lambda ()
                                         (loop
                                           (sleep .25)
                                           (unless (check-act-r-command next)
                                             (return))
                                           (unless (check-act-r-command update)
                                             (return)))
                                         (stop-stepper id nil)))))
                   
                   (setf (stepper-control-pre-hook *stepper*)
                     (add-pre-event-hook 'stepper-pre-hook))
                   (setf (stepper-control-post-hook *stepper*)
                     (add-post-event-hook 'stepper-post-hook))
                   (dolist (m (mp-models))
                     (with-model-eval m
                       (let ((s (get-module stepper)))
                         (reset-stepper-module s))))
                   (values t (stepper-control-open *stepper*)))
               (error (x)
                 (remove-stepper-controls *stepper* t)
                 (return-from start-stepper
                   (values nil
                           (let ((*print-circle* t))
                             (format nil "Error ~/print-error-message/ occurred while trying to start a stepper" x)))
               
               ))))))))

(add-act-r-command "start-stepper" 'start-stepper "Start the stepper control interface if possible. Returns t and an id or nil and a failure string.  Params: step-all? tutor-mode? next-event-cmd update-cmd")

(defun stop-stepper (id &optional (kill t))
  (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
      (when (and (stringp id) (stepper-control-open *stepper*)
                 (string-equal id (stepper-control-open *stepper*)))
        
        (remove-stepper-controls *stepper* kill)
        
        (terminate-waiting *stepper*)
        t)))


(defun remove-stepper-controls (stepper kill)
  
  (ignore-errors (when kill (bt:destroy-thread (stepper-control-safety-thread stepper))))
  
  (ignore-errors
   (setf (stepper-control-open stepper) nil)
   (setf (stepper-control-current-event stepper) nil)
   (setf (stepper-control-stepped-event-details stepper) nil)
  
   (when (stepper-control-pre-hook stepper)
     (delete-event-hook (stepper-control-pre-hook stepper))
     (setf (stepper-control-pre-hook stepper) nil))
   (when (stepper-control-post-hook stepper)
     (delete-event-hook (stepper-control-post-hook stepper))
     (setf (stepper-control-post-hook stepper) nil)))
  
  (dolist (m (mp-models)) 
    (with-model-eval m
      (no-output
       (ignore-errors
        (when (find 'stepper-rr-hook (car (sgp :retrieval-request-hook)))
          (sgp :retrieval-request-hook (:remove stepper-rr-hook))))
       (ignore-errors
        (when (find 'stepper-rs-hook (car (sgp :retrieval-set-hook)))
          (sgp :retrieval-set-hook (:remove stepper-rs-hook))))
       (ignore-errors 
        (when (find 'stepper-cs-hook (car (sgp :conflict-set-hook)))
          (unwind-protect
              (progn
                (suppress-undelay-cr) 
                (sgp :conflict-set-hook (:remove stepper-cs-hook)))
            (unsuppress-undelay-cr))))))))
  
  
(add-act-r-command "stop-stepper" 'stop-stepper "Stop the stepper control interface.  Params: stepper-id.")


(defun terminate-waiting (stepper)
  (bt:acquire-lock (stepper-control-step-lock stepper) t)
  (when (stepper-control-waiting stepper)
    (setf (stepper-control-step-val stepper) t)
    (bt:condition-notify (stepper-control-step-cv stepper)))
  (bt:release-lock (stepper-control-step-lock stepper)))


(defun check-step-type (type val)
  (if (stringp type)
    (let* ((*read-eval* nil)
           (real-val (if (stringp val) (read-from-string val nil :stepper-string-read-error) val)))
      
      (if (eq real-val :stepper-string-read-error)
          (if (zerop (length val))
              (values nil "No step value provided.")
            (values nil "Could not parse step value provided."))

        (cond ((string-equal type "production")
           (cond ((or (not (symbolp real-val))
                      (notany (lambda (model) 
                                (with-model-eval model 
                                  (find real-val (all-productions))))
                              (mp-models)))
                  (values nil "Step type Production requires a valid production name."))
                 (t 
                  (values 'production real-val))))
              ((string-equal type "time") 
               (cond ((not (numberp real-val))
                      (values nil "Step type Time requires a number in seconds."))
                     ((>= (mp-time) real-val)
                      (values nil "Steo type Time given a time that has already occurred."))
                     (t 
                      (values 'time (seconds->ms real-val)))))
              ((string-equal type "module") 
               (if (not (find val (mapcar 'symbol-name (all-module-names)) :test #'string-equal))
                   (values nil "Step type Module requires a valid module name.")
                 (values 'module val)))
              (t 
               (values nil (format nil "Step type ~s not a valid type." type))))))
  (values nil "Step type not a string.")))



(defun model-trace-enabled (event)
  (with-model-eval (if (evt-model event) (evt-model event) (first (mp-models))) ;; just use the first if there isn't one (a break event)
    (car (no-output (sgp :v)))))

(defun stepper-pre-hook (event)
  
  (bt:acquire-lock (stepper-control-step-lock *stepper*) t)
  
  (let (type val all next tutor ready)
    (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
      (setf type (stepper-control-skip-type *stepper*)
        val (stepper-control-skip-val *stepper*)
        all (stepper-control-step-all *stepper*)
        next (stepper-control-next-cmd *stepper*)
        tutor (stepper-control-tutoring *stepper*)
        ready (stepper-control-ready *stepper*)))
    (if (and ready
             (or all
              (and type ;; running until a specific action
                   (event-displayed-p event)
                   (model-trace-enabled event)
                   (case type
                     (production
                      (and (or (eq (evt-action event) 'production-selected)
                               (eq (evt-action event) 'production-fired))
                           (string-equal val (symbol-name (production-name (car (evt-params event)))))))
                     (time
                      (>= (evt-mstime event) val))
                     (module
                      (string-equal val (symbol-name (evt-module event))))))
              (and (null type)
                   (or 
                    (and (event-displayed-p event)
                         (model-trace-enabled event))
                    (and tutor
                         (or 
                          (eq (evt-action event) 'production-selected)
                          (eq (evt-action event) 'conflict-resolution)
                          (eq (evt-action event) 'start-retrieval))
                         )))))
      
        (progn
          (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
            (setf (stepper-control-current-event *stepper*) event))
          
          
          ;; wait for step or termination
          
          (let (result)
            
            (setf (stepper-control-step-val *stepper*) nil)
            (setf (stepper-control-waiting *stepper*) t)
            
            (dispatch-apply next (format-event event))
            
            (loop
              (when (stepper-control-step-val *stepper*)
                (setf result (stepper-control-step-val *stepper*))
                (setf (stepper-control-waiting *stepper*) nil)
                (return))
              (bt:condition-wait (stepper-control-step-cv *stepper*) (stepper-control-step-lock *stepper*)))
            (setf (stepper-control-waiting *stepper*) nil)
            (bt:release-lock (stepper-control-step-lock *stepper*))
            result))
      
      (bt:release-lock (stepper-control-step-lock *stepper*)))))


(defun stepper-step (id &optional stop type value)
  (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
    
    (let (val result msg
              (valid-id (and (stringp id) (stepper-control-open *stepper*)
                             (string-equal id (stepper-control-open *stepper*))))
              )
      (if valid-id
          (progn
            (setf (stepper-control-skip-type *stepper*) nil)
            (setf (stepper-control-skip-val *stepper*) nil)

          (if stop
              (setf val "break" result t msg "")
            (if type
                (multiple-value-bind (valid-type valid-val) (check-step-type type value)
                  (if valid-type
                      (progn
                        (setf (stepper-control-skip-type *stepper*) valid-type)
                        (setf (stepper-control-skip-val *stepper*) valid-val)
                        (setf val t result t msg ""))
                    (setf result nil msg valid-val)))
            (setf val t result t msg ""))))
        (setf result nil msg "Invalid id provided to stepper-step"))
      
      (when result
        
        (bt:acquire-lock (stepper-control-step-lock *stepper*) t)
        (when (stepper-control-waiting *stepper*)
          (setf (stepper-control-step-val *stepper*) val)
          (bt:condition-notify (stepper-control-step-cv *stepper*)))
        (bt:release-lock (stepper-control-step-lock *stepper*)))
    
    (values result msg))))


(add-act-r-command "step-stepper" 'stepper-step "Advance the stepper control interface when it is waiting. Returns: success error-msg. Params: stepper-id {stop? {step-type step-value}}")
  
(defun stepper-post-hook (event)
  (let (step-event cmd ready)
    (bt:with-recursive-lock-held ((stepper-control-lock *stepper*)) 
      (setf step-event (stepper-control-current-event *stepper*)
        cmd (stepper-control-update-cmd *stepper*)
        ready (stepper-control-ready *stepper*))
      )
  
  (when (and ready (eq event step-event))
    (let ((current-details (cond ((or (eq (evt-action event) 'production-selected)
                                      (eq (evt-action event) 'production-fired))
                                  (list (with-model-eval (evt-model event)
                                          (if (eq (evt-action event) 'production-fired) 
                                              ;; catch when no saved data
                                              (if (car (stepper-module-cs (get-module stepper)))
                                                  (list (car (stepper-module-cs (get-module stepper))))
                                                ;; get production named in the event
                                                (list (production-name (first (evt-params event)))))
                                            (stepper-module-cs (get-module stepper))))
                                        (if (eq (evt-action event) 'production-selected) 'production-selected nil)
                                        "Possible Productions" "Production Parameters" "Bindings" "Production"))
                                 ((or (eq (evt-action event) 'retrieved-chunk)
                                      (eq (evt-action event) 'retrieval-failure))
                                  (list (with-model-eval (evt-model event)
                                          (stepper-module-rs (get-module stepper)))
                                        nil
                                        "Possible Chunks" "Chunk Parameters" "Retrieval Request" "Chunk"))
                                 ((eq (evt-action event) 'conflict-resolution)
                                  (list nil 'conflict-resolution "" "" "" ""))
                                 ((eq (evt-action event) 'start-retrieval)
                                  (list nil 'start-retrieval "" "" "" ""))
                                 (t
                                  (list nil nil "" "" "" ""))))
          (saved-details (cond ((or (eq (evt-action event) 'production-selected)
                                    (eq (evt-action event) 'production-fired))
                                (with-model-eval (evt-model event)
                                  (mapcar (lambda (p)
                                            (when p
                                              (let* ((prod (get-production p))
                                                   (bindings (when prod
                                                               (bt:with-recursive-lock-held ((production-lock prod)) 
                                                                 (mapcar (lambda (x)
                                                                           (list (car x) (cdr x) 
                                                                                 (and (find (car x) (production-lhs-buffers prod) :key 'buffer-name->variable) t)))
                                                                   (production-bindings prod))))))
                                              (list p
                                                    (capture-command-output (spp-fct (list p)))
                                                    (let ((s (make-string-output-stream)))
                                                      (mapcar (lambda (x) 
                                                                (format s "~a: ~s~%" 
                                                                  (first x) (second x)))
                                                        bindings)
                                                      (get-output-stream-string s))
                                                    (list (printed-production-text p) (printed-production-text p t))
                    
                                                    (mapcar (lambda (x) (list (first x) 
                                                                              (if (stringp (second x))
                                                                                  (encode-string (second x))
                                                                                (second x))
                                                                              (third x)))
                                                      bindings)
                                                    ))))
                                    
                                    (if (eq (evt-action event) 'production-fired) 
                                        (list (car (stepper-module-cs (get-module stepper))))
                                      (stepper-module-cs (get-module stepper))))))
                               
                               ((or (eq (evt-action event) 'retrieved-chunk)
                                    (eq (evt-action event) 'retrieval-failure))
                                
                                (with-model-eval (evt-model event)
                                (let ((s (get-module stepper)))
                                  (mapcar (lambda (c)
                                            (list 
                                             c
                                             (with-parameters (:ans nil) (capture-command-output (sdp-fct (list c))))
                                             (if (stepper-module-rr s) (format nil "+retrieval>~%~a" (stepper-module-rr s)) "")
                                             (printed-chunk c)
                                             nil))
                                    (stepper-module-rs (get-module stepper))))))
                               ((eq (evt-action event) 'conflict-resolution)
                                (with-model-eval (evt-model event)
                                  (mapcar (lambda (x)
                                            (list (printed-production-side x t)
                                                  (let ((w (whynot-text x)))
                                                    (aif (search "It fails because:" w)
                                                         (let ((ws (list #\return #\space #\newline)))
                                                           (string-right-trim ws (string-left-trim ws (subseq w (+ it 17)))))
                                                         ""))))
                                    (let ((seed (get-parameter-value :seed)))
                                      (prog1
                                          (permute-list (all-productions))
                                        (set-parameter-value :seed seed))))))
                               ((eq (evt-action event) 'start-retrieval)
                                (with-model-eval (evt-model event)
                                  
                                  (let* ((s (get-module stepper))
                                         (request-text
                                          (if (stepper-module-rr s) 
                                              (format nil "+retrieval>~%~a" (stepper-module-rr s)) 
                                            ""))
                                         (request-spec (read-from-string (format nil "(~a)" (stepper-module-rr s))))
                                         (matching (no-output (sdm-fct request-spec)))
                                         )
                                    
                                    (list request-text
                                          (mapcar (lambda (c)
                                                    (list 
                                                     (let ((ws (list #\return #\newline))
                                                           (name (symbol-name c)))
                                                       (string-right-trim 
                                                        ws (string-left-trim 
                                                            ws (subseq (printed-chunk c) (length name)))))
                                                     (when (find c matching) t)))
                                            (no-output (dm)))))))
                               (t (list "" "" "" nil)))))
                                  
      
      (bt:with-recursive-lock-held ((stepper-control-lock *stepper*)) 
        
        (setf (stepper-control-stepped-event-details *stepper*)
          (list (evt-time step-event) (evt-action step-event) (evt-params step-event) saved-details)))
      
      (dispatch-apply-list cmd (cons (format-event event) current-details))))))

(defun update-stepper-info (item)
  (if item
      (let (event tutor
            (name (string->name item)))
        (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
          (setf event  (stepper-control-stepped-event-details *stepper*))
          (setf tutor (stepper-control-tutoring *stepper*)))
        (if event
            (destructuring-bind (time action params details) event
              (declare (ignore time params))
              (cond ((or (eq action 'production-selected)
                         (eq action 'production-fired))
                     (let ((d (assoc name details)))
                       (if d
                           (values
                            (second d)
                            (third d)
                            (if (or (eq action 'production-selected)
                                    tutor)
                                (first (fourth d))
                              (second (fourth d)))
                            (fifth d))
                         (values "" "" "" nil))))
                    
                    ((or (eq action 'retrieved-chunk)
                         (eq action 'retrieval-failure))
                     (let ((d (assoc name details)))
                       (if d
                           (values-list (cdr d))
                         (values "" "" "" nil))))
                    (t
                     (values "" "" "" nil))))
          (values "" "" "" nil)))
    
    (fourth (stepper-control-stepped-event-details *stepper*))))

(add-act-r-command "update-stepper" 'update-stepper-info "Stepper interface command for getting the detailed information. Returns: strings for the three information panels for the given item. Params: seleted-item")




(defun stepper-rr-hook (request)
  (setf (stepper-module-rr (get-module stepper)) (printed-chunk-spec request))
  nil)

(defun stepper-rs-hook (set)
  (setf (stepper-module-rs (get-module stepper)) (copy-list set))
  nil)
  
(defun stepper-cs-hook (set)
  (setf (stepper-module-cs (get-module stepper)) (copy-list set))
  nil)
                        


(defun set-stepper-tutoring (id value)
  (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
    (when (and (stringp id) (stepper-control-open *stepper*)
               (string-equal id (stepper-control-open *stepper*)))
      (setf (stepper-control-tutoring *stepper*) value)
      t)))


(add-act-r-command "set-stepper-tutoring" 'set-stepper-tutoring "Stepper interface command for changing the tutor mode. Params: stepper-id tutoring-value.")


(defun set-stepper-step-all (id value)
  (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
    (when (and (stringp id) (stepper-control-open *stepper*)
               (string-equal id (stepper-control-open *stepper*)))
      (setf (stepper-control-step-all *stepper*) value)
      t)))


(add-act-r-command "set-stepper-step-all" 'set-stepper-step-all "Stepper interface command for changing the step-all mode. Params: stepper-id step-all-value.")


(defun set-stepper-ready (id)
  (bt:with-recursive-lock-held ((stepper-control-lock *stepper*))
    (when (and (stringp id) (stepper-control-open *stepper*)
               (string-equal id (stepper-control-open *stepper*)))
      (setf (stepper-control-ready *stepper*) t))))


(add-act-r-command "set-stepper-ready" 'set-stepper-ready "Stepper interface command for signaling that code which opened a stepper is ready to start stepping. Params: stepper-id.")


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
