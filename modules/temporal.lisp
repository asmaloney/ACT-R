;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Niels Taatgen
;;; Copyright   : (c) 2005 Niels Taatgen
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : taatgen@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : temporal.lisp
;;; Version     : 3.1
;;; 
;;; Description : Implementation of the temporal module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2005.04.26 Niels
;;;             : Initial creation.
;;; 2010.02.05 Dan [1.0b2]
;;;             : * Moved the noise added to t0 into the reset function and
;;;             :   made that a 3rd pass reset so that if the seed is set in
;;;             :   the model code the temporal module will perform the same
;;;             :   every time.  However, if the model time isn't 0 (other than
;;;             :   during a reset) then it will still add the noise when the 
;;;             :   :time-master-start-increment parameter is changed.
;;;             : * Along with that I changed it from a fixed .075 multiplier
;;;             :   (which is 5 * the default b) to actually 5 times the current
;;;             :   b value (:time-noise).
;;;             : * Also changed the return value for setting :time-master-start-increment
;;;             :   to always be the value set instead of the randomized value
;;;             :   since the non-setting sgp returns the non-randomized value.
;;;             : * Needs a primary reset to create the chunk type.
;;; 2010.06.17 Dan
;;;             : * Added a mod-request function which uses the goal-style-mod-request
;;;             :   and then use that to make the updates to the time tick.  That
;;;             :   will then be recorded in the buffer trace history which will then
;;;             :   show in the tracing tools.
;;;             : [1.0b3]
;;;             : * Changed that to put it on a switch so that it can be turned
;;;             :   off for something like computing bold response where only
;;;             :   the "reseting" is what's important.
;;;             :   The :record-ticks parameter controls whether or not the
;;;             :   individual updates create an event to record.  The default
;;;             :   value is t.
;;; 2010.08.09 Dan [1.0b4]
;;;             : * Moved the setting of the t0 noise from reset/param. init.
;;;             :   time to the first request.  That way it doesn't affect how
;;;             :   the existing test models operate.  Also makes it easy if
;;;             :   we want to switch it to have a new random offset each time.
;;;             : * Took out the temporal-mod-request function which isn't used
;;;             :   now since it uses the goal-style-mod-request.
;;; 2010.08.10 Dan
;;;             : * Added code to make sure that there is a chunk-type called clear
;;;             :   defined and if not to define it.
;;;             : * Actually generate an event for the clear request in the trace.
;;;             : * Changed the update event's details from format to concatenate
;;;             :   which is typically significantly faster.
;;;             : * Changed the test for :time-noise to be posnum since act-r-noise
;;;             :   only works for values >0.
;;;             : * Changed all the parameters' warnings to indicate a positive
;;;             :   number is required.
;;; 2010.09.17  Dan
;;;             : * Changed the 3rd reset function to a second reset function
;;;             :   because otherwise the :do-not-harvest setting doesn't apply
;;;             :   during production definition...
;;; 2011.05.17  Dan [1.0b5]
;;;             : * Added some saftey checks so that there's always at least
;;;             :   a 1ms increment in time before the next tick to avoid bad
;;;             :   situations which could arrive with some parameter adjustments.
;;; 2011.07.04  Dan
;;;             : * Set a 1ms minimum time for temporal-module-time-start-increment
;;;             :   to avoid negative calls to act-r-noise.
;;; 2013.01.17  Dan [1.0]
;;;             : * Changed it so the timer ticks don't show in the low detail
;;;             :   trace.
;;; 2014.05.16 Dan [2.0]
;;;             : * Updated to work with the type-less chunk mechanism.
;;; 2014.11.03 Dan
;;;             : * Allow clear requests like PM modules by using the 
;;;             :   test-for-clear-request function.
;;; 2015.06.05 Dan [2.1]
;;;             : * Compute the tick lengths in ms by converting :time-master-start-increment
;;;             :   when provided and schedule everything in ms.
;;; 2018.06.04 Dan
;;;             : * Don't require that the tick value be zero in the request to
;;;             :   start the module going.
;;; 2020.01.10 Dan [2.2]
;;;             : * Removed the #' from the module interface functions
;;;             :   since that's not allowed in the general system now.
;;; 2020.06.01 Dan [3.0]
;;;             : * Moved the chunk-type definition to the creation 
;;;             :   function instead of the reset function.
;;; 2020.07.06 Dan [3.1]
;;;             : * Got rid of the temporal-clear dummy function and just 
;;;             :   schedule nil since the details are all that matters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; User Functions:
;;;
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

(defstruct temporal-module 
  time-noise 
  time-mult 
  time-start-increment 
  time-master-start-increment 
  tick 
  ticks
  next-increment
  record-ticks)

(defun create-temporal-module (model-name)
  (declare (ignore model-name))
  (chunk-type time (ticks 0))
  (make-temporal-module))


(defun temporal-reset-1 (instance)
  ;; Create the chunk-type and clear the internal
  ;; marker for the next update.
  
  (setf (temporal-module-next-increment instance) nil))

(defun temporal-reset-2 (instance)
  (declare (ignore instance))
  
  ;; Do NOT strict harvest the temporal buffer by default
  (sgp :do-not-harvest temporal)
  )


(defun temporal-query (instance buffer-name slot value)
  (declare (ignore instance buffer-name))
  (case slot
    (state
     (case value
       (busy nil)
       (free t)
       (error nil)
       (t (print-warning "Unknown state query ~S to temporal module" value)
          nil)))
    (t (print-warning "Unknown query of the temporal module"))))


(defun temporal-request (instance buffer-name chunk-spec)
  (declare (ignore buffer-name))
  ;; Don't want to perform the next increment if a new
  ;; request comes in or a bad value could get written.
  
  (when (temporal-module-next-increment instance)
    (delete-event (temporal-module-next-increment instance)))
  
  ;; Add the noise to the start tick (for now only happens
  ;; at the time of the first request).
  
  (when (null (temporal-module-time-start-increment instance))
    (setf (temporal-module-time-start-increment instance)
      (max 1
           (+ (temporal-module-time-master-start-increment instance)
              (act-r-noise (* (temporal-module-time-master-start-increment instance)
                              5 (temporal-module-time-noise instance)))))))
  
  (let ((ticks (chunk-spec-slot-spec chunk-spec 'ticks))
        (clear (test-for-clear-request chunk-spec)))
    (if (or ticks clear)
        (if clear
            (schedule-event-now nil :module 'temporal :details "Clear")
          (if (not (verify-single-explicit-value chunk-spec 'ticks 'temporal 'time))
              (print-warning "Invalid time request made to the temporal module.")
            (progn
              (schedule-event-now 'create-new-buffer-chunk :module 'temporal
                                  :priority -100 :params (list 'temporal '(isa time))
                                  :details "create-new-buffer-chunk isa time")
         
              (setf (temporal-module-tick instance) 
                (max 1 (+ (temporal-module-time-start-increment instance) 
                          (act-r-noise 
                           (* (temporal-module-time-noise instance) 
                              (temporal-module-time-start-increment instance))))))
              
              (setf (temporal-module-ticks instance) 0)
              
              (setf (temporal-module-next-increment instance)
                (schedule-event-relative (round (temporal-module-tick instance)) 'next-time-tick
                                         :time-in-ms t
                                         :module 'temporal :priority :min
                                         :details "Incrementing time ticks to 1"
                                         :destination 'temporal)))))
      (print-warning "Invalid request made to the temporal module."))))

(defun next-time-tick (instance)
  ;; if the chunk in the temporal buffer has a slot named ticks which holds a number
  (when (let ((c (buffer-read 'temporal)))
          (and c (numberp (chunk-slot-value-fct c 'ticks))))
    
    (incf (temporal-module-ticks instance))
     
    (setf (temporal-module-tick instance) 
      (* (temporal-module-tick instance) (temporal-module-time-mult instance)))
    
    (setf (temporal-module-tick instance) 
      (max 1 (+ (temporal-module-tick instance) 
                    (act-r-noise (* (temporal-module-time-noise instance)
                                    (temporal-module-tick instance))))))
    
    ;; using a mod-request to do it so that it shows up in the
    ;; tracing tools when desired otherwise just do the modification
    ;; directly on the buffer chunk
    
    (if (temporal-module-record-ticks instance)
        (schedule-module-mod-request 'temporal (list 'ticks (temporal-module-ticks instance)) 0 :time-in-ms t :module 'temporal)
      (schedule-mod-buffer-chunk 'temporal (list 'ticks (temporal-module-ticks instance)) 0 :time-in-ms t :module 'temporal :output 'medium))
    
    (setf (temporal-module-next-increment instance)
      (schedule-event-relative (round (temporal-module-tick instance)) 'next-time-tick
                               :time-in-ms t :module 'temporal :priority :min :destination 'temporal
                               :details (concatenate 'string "Incrementing time ticks to "
                                          (princ-to-string (1+ (temporal-module-ticks instance))))))))

(defun temporal-mod-request (instance buffer mods)
  (declare (ignore instance))
  (schedule-mod-buffer-chunk buffer mods 0 :time-in-ms t :module buffer :output 'medium))

(defun temporal-params (tmp param)
  (cond ((consp param)
         (case (car param)
           (:time-noise (setf (temporal-module-time-noise tmp) (cdr param)))
           (:time-mult (setf (temporal-module-time-mult tmp) (cdr param)))
           (:record-ticks (setf (temporal-module-record-ticks tmp) (cdr param)))
           (:time-master-start-increment (setf (temporal-module-time-start-increment tmp) nil)
                                         (setf (temporal-module-time-master-start-increment tmp) (seconds->ms (cdr param)))
                                         (cdr param))))
        (t
         (case param
           (:time-noise (temporal-module-time-noise tmp))
           (:time-mult (temporal-module-time-mult tmp))
           (:record-ticks (temporal-module-record-ticks tmp))
           (:time-master-start-increment (ms->seconds (temporal-module-time-master-start-increment tmp)))))))


;;; Actually define the module now

(define-module-fct 'temporal '(temporal)
  (list (define-parameter :time-noise :valid-test 'posnum :default-value .015
          :warning "a positive number" :documentation "Temporal noise")
        (define-parameter :time-master-start-increment :valid-test 'posnum :default-value .011
          :warning "a positive number" :documentation "Temporal start interval")
        (define-parameter :time-mult :valid-test 'posnum :default-value 1.1
          :warning "a positive number" :documentation "Temporal multiplier")
        (define-parameter :record-ticks :valid-test 'tornil :default-value t
          :warning "t or nil" :documentation "Record each time increment as a buffer event")
        )
  :version "3.1"
  :documentation "The temporal module is used to estimate short time intervals"
  :creation 'create-temporal-module
  :query 'temporal-query
  :request 'temporal-request
  :buffer-mod 'temporal-mod-request
  :params 'temporal-params
  :reset (list 'temporal-reset-1 'temporal-reset-2))


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
