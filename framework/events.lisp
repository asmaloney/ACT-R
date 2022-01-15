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
;;; Filename    : events.lisp
;;; Version     : 1.1
;;; 
;;; Description : The definition of events for the scheduler as described in
;;;               the ACT-R 6 software framework API.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.08.11 Dan
;;;             : Creation.
;;; 2005.01.09 Dan
;;;             : Changing the event trace a little. I don't think it's
;;;             : necessary to print the destination in the trace since
;;;             : it's either the same as the module (and already there)
;;;             : or the module that was the destination will show an
;;;             : event later in response...
;;; 2006.07.16 Dan
;;;             : Changed the API section to note the correct accessor is
;;;             : evt-output.
;;; 2007.10.31 Dan
;;;             : * Fixed the format-event for break events so that if the longest
;;;             :   model or module names are shorter than the string of "-" 
;;;             :   characters printed the string is truncated to fit the trace
;;;             :   column size.
;;; 2011.03.25 Dan
;;;             : * Changed format-event and the format string so that a nil time
;;;             :   doesn't cause a problem (waiting events have nil times).
;;;             : * Adjusted format-event so that a dynamic will indicate that
;;;             :   it may still be moved (not quite the same as waiting).
;;;             : * One step farther in format-event to remove the evt-time call
;;;             :   entirely, and add the dynamic reporting to break events.
;;; 2014.09.04 Dan
;;;             : * Finally removed that trailing space from the event output!
;;; 2016.04.14 Dan
;;;             : * Since :output nil events are actually being output in some
;;;             :   cases now format-event doesn't show the params for such an
;;;             :   event since they're probably non-readable.
;;; 2016.09.28 Dan
;;;             : * Removed the check for multiple meta-processes from the 
;;;             :   format-event methods.
;;; 2018.02.05 Dan [1.1]
;;;             : * Actually define user accessors and access the structs internals
;;;             :   with the act-r-event-... accessors.
;;; 2018.02.07 Dan
;;;             : * Don't print the dynamically adjusted or waiting for parts of
;;;             :   the events anymore.
;;;             : * The underlying event now holds the actual model structure
;;;             :   and this needs to get the name for returning and printing.
;;; 2018.02.22 Dan
;;;             : * Event time and priority test the meta-p-schedule-lock since
;;;             :   it can change those values.
;;; 2018.02.23 Dan
;;;             : * Fixed a bug in format-event introduced with the last update.
;;;             : * Added a default format-event method which just returns nil.
;;;             : * Added a remote format-event.
;;; 2018.03.16 Dan
;;;             : * Meta-p-schedule-lock is a recursive lock which is important.
;;; 2018.06.13 Dan
;;;             : * Updated the command doc strings to match current spec.
;;; 2019.04.09 Dan
;;;             : * Adjust evt-priority to check the new min and max slots.
;;; 2020.03.11 Dan
;;;             : * Changed the class for the format-event method from fixnum
;;;             :   to integer since some Lisps don't have a fixnum class.
;;; 2020.08.25 Dan
;;;             : * Switch from defconstant to the define-constant macro to 
;;;             :   avoid issues with SBCL (instead of redefining defconstant
;;;             :   for SBCL as was done previously).
;;; 2021.07.06 Dan
;;;             : * Changed the external evt-params to encode strings in the
;;;             :   list since that may be important.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Despite the fact that events are simply structs one should not create
;;; them explicitly and only the scheduling fuctions provided in schedule.lisp
;;; should be used to generate them.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; 
;;; evt-time (event)
;;; returns the time in seconds at which the event is scheduled to occur.
;;;
;;; evt-priority (event)
;;; returns the priority of the event.  
;;;
;;; evt-action (event)
;;; returns the action function that will be executed for the event.  
;;;
;;; evt-model (event)
;;; returns the name of the model in which this event was created.
;;;
;;; evt-module (event)
;;; returns the name of the module which created this event.
;;;
;;; evt-destination (event)
;;; returns the destination which was specified for the event.
;;;
;;; evt-params (event)
;;; returns the list of parameters which will be passed to the event's 
;;; action function when the event is executed.
;;;
;;; evt-details (event)
;;; returns the details string that will be displayed in the trace for 
;;; this event if it has one, or nil if it does not.
;;;
;;; evt-output (event)
;;; returns the output value of this event, which indicates whether to 
;;; print this event in the trace.
;;;
;;; format-event (event)
;;; returns a string that contains the text that will be displayed in the 
;;; trace for event if it is executed and has its output set to t.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Because a function with the name 'event-'something is likely to conflict 
;;; with existing functions in some Lisp and the default operation of the 
;;; system is to not separately package the components, the names of the 
;;; accessors begin with evt- instead of event-.
;;;
;;; Using a struct because I don't need anything fancy at this point.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; Actually define the accessors since can't use the structs anymore

(defun evt-time (event)
  (aif (get-event-by-id event)
       (values 
        (let ((time (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime it))))
          (if (numberp time)
              (* time .001)
            nil))
        t)
       (values nil nil)))

(defun evt-mstime (event)
  (aif (get-event-by-id event)
       (values (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime it)) t)
       (values nil nil)))

(defun evt-priority (event)
  (aif (get-event-by-id event)
       (values (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (or (act-r-event-max it)
                                                                                      (act-r-event-min it)
                                                                                      (act-r-event-priority it))) t)
       (values nil nil)))

(defun evt-action (event)
  (aif (get-event-by-id event)
       (values (act-r-event-action it) t)
       (values nil nil)))

(defun evt-model (event)
  (aif (get-event-by-id event)
       (values (if (act-r-model-p (act-r-event-model it))
                   (act-r-model-name (act-r-event-model it))
                 nil)
               t)
       (values nil nil)))

(defun evt-module (event)
  (aif (get-event-by-id event)
       (values (act-r-event-module it) t)
       (values nil nil)))

(defun evt-destination (event)
  (aif (get-event-by-id event)
       (values (act-r-event-destination it) t)
       (values nil nil)))

(defun evt-params (event)
  (aif (get-event-by-id event)
       (values (act-r-event-params it) t)
       (values nil nil)))

(defun evt-details (event)
  (aif (get-event-by-id event)
       (values (act-r-event-details it) t)
       (values nil nil)))

(defun evt-output (event)
  (aif (get-event-by-id event)
       (values (act-r-event-output it) t)
       (values nil nil)))


(defun external-evt-action (event)
  (aif (get-event-by-id event)
       (values (let ((action (act-r-event-action it)))
                 (if (stringp action) (concatenate 'string "'" action "'") action))
               t)
       (values nil nil)))

(defun external-evt-params (event)
  (aif (get-event-by-id event)
       (values (encode-string-names (act-r-event-params it))
               t)
       (values nil nil)))

(add-act-r-command "evt-time" 'evt-time "Return the time of an event in seconds. Params: event-id")
(add-act-r-command "evt-mstime" 'evt-mstime "Return the time of an event in milliseconds. Params: event-id")
(add-act-r-command "evt-priority" 'evt-priority "Return the priority of an event. Params: event-id")
(add-act-r-command "evt-action" 'external-evt-action "Return the action of an event. Params: event-id")
(add-act-r-command "evt-model" 'evt-model "Return the model of an event. Params: event-id")
(add-act-r-command "evt-module" 'evt-module "Return the module of an event. Params: event-id")
(add-act-r-command "evt-destination" 'evt-destination "Return the destination of an event. Params: event-id")
(add-act-r-command "evt-params" 'external-evt-params "Return the parameters of an event. Params: event-id")
(add-act-r-command "evt-details" 'evt-details "Return the details of an event. Params: event-id")
(add-act-r-command "evt-output" 'evt-output "Return the output setting of an event. Params: event-id")



(defun act-r-event-break-action (mp)
  (bt:with-recursive-lock-held ((meta-p-schedule-lock mp))
    (setf (meta-p-break mp) t)))

(defmethod format-event ((event t))
               nil)

(defmethod format-event ((event integer))
  (aif (get-event-by-id event)
       (format-event it)
       nil))

(add-act-r-command "format-event" 'format-event "Returns a string with the output of the event as it would show in the trace.  Params: event-id")

(define-constant +format-event-event-string+ 
    (formatter "~:[~*~*~;~6d.~3,'0d~] ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va ~:[~*~a~{ ~a~}~;~a~*~*~]"))


(defmethod format-event ((event act-r-event))
  (let ((*print-pretty* nil)
        (mp (current-mp)))
    (let (mcount name-len)
      (bt:with-lock-held ((meta-p-models-lock mp))
        (setf mcount (meta-p-model-count mp)
          name-len (meta-p-model-name-len mp)))
      
      (let ((time (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime event))))
        (multiple-value-bind (sec ms) (when (numberp time) (truncate time 1000))
          (format nil +format-event-event-string+
            
            time
            sec ms
            
            nil
            (act-r-event-mp event)
            
            (< 1 mcount)
            name-len
            
            (if (act-r-model-p (act-r-event-model event))
                (act-r-model-name (act-r-event-model event))
              (act-r-event-model event))
            
            (max-module-name-length)
            (act-r-event-module event)
            
            (act-r-event-details event)
            (act-r-event-details event)
            (act-r-event-action event)
            (when (act-r-event-output event) (act-r-event-params event))))))))

(define-constant +format-event-break-event-string+ (formatter "~:[~*~*~;~6d.~3,'0d~] ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va BREAK-EVENT ~@[~a~^ ~]"))


(defmethod format-event ((event act-r-break-event))
  (let ((*print-pretty* nil)
        (mp (current-mp)))
    (let (mcount name-len)
      (bt:with-lock-held ((meta-p-models-lock mp))
        (setf mcount (meta-p-model-count mp)
          name-len (meta-p-model-name-len mp)))
      
      (let ((time (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime event))))
        (multiple-value-bind (sec ms) (when (numberp time) (truncate time 1000))
          (format nil +format-event-break-event-string+
            time
            sec ms
            
            nil
            (act-r-event-mp event)
            (< 1 mcount)
            name-len
            (subseq "------" 0 (min 6 name-len))
            (max-module-name-length)
            (subseq "------" 0 (min 6 (max-module-name-length)))
            (act-r-event-details event)))))))
  

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
