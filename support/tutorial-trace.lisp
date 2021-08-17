;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2014 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : tutorial-trace.lisp
;;; Version     : 1.0
;;; 
;;; Description : Hack to modify the model trace so it fits the page width for
;;;             : the tutorial traces.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2014.08.20 Dan 
;;;             : * Why didn't I do something like this sooner?
;;; 2017.06.22 Dan
;;;             : * Protecting the meta-p-model-count and -model-name-len access.
;;; 2018.02.05 Dan
;;;             : * Use the underlying structure accessors instead of evt-.
;;; 2018.02.07 Dan
;;;             : * Don't print the dynamically adjusted and waiting for messages.
;;;             : * Get the model name since the slot now holds the struct.
;;; 2018.03.16 Dan
;;;             : * Meta-p-schedule-lock is recursive.
;;; 2018.05.07 Dan
;;;             : * Updated with current format-event as the base.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Call format-trace-for-tutorial providing the keyword parameters for the
;;; number of columns to use for the seconds (time-cols) and columns for the
;;; module names (module-cols).  
;;;
;;; To set things back to normal use restore-normal-trace.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Nothing public -- this is internal and use at your own risk stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Just smash the fdefinition of format-event instead of trying to redefine the
;;; methods because that makes it easier to set back.  
;;;
;;; No real safety checks in this since it's not for general use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defvar *time-cols* 1)
(defvar *module-cols* 11)
(defvar *tutorial-trace-on* nil)
(defvar *saved-format-event* nil)

(defun format-trace-for-tutorial (&key (time-cols 1) (module-cols 11))
  (setf *time-cols* time-cols)
  (setf *module-cols* module-cols)
  (unless *saved-format-event*
    (setf *saved-format-event* (fdefinition 'format-event)))
  (setf (fdefinition 'format-event) (fdefinition 'tutorial-format-event)))

(defun restore-normal-trace ()
  (when *saved-format-event*
    (setf (fdefinition 'format-event) *saved-format-event*)
    (setf *saved-format-event* nil)))

(defmethod tutorial-format-event ((event fixnum))
  (aif (get-event-by-id event)
       (tutorial-format-event it)
       nil))

(defmethod tutorial-format-event ((event act-r-event))
  (let ((*print-pretty* nil)
        (mp (current-mp)))
    (let (mcount name-len)
      (bt:with-lock-held ((meta-p-models-lock mp))
        (setf mcount (meta-p-model-count mp)
          name-len (meta-p-model-name-len mp)))
      
      (let ((time (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime event))))
        (multiple-value-bind (sec ms) (when (numberp time) (truncate time 1000))
          (format nil "~:[~*~*~*~;~vd.~3,'0d~] ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va ~:[~*~a~{ ~a~}~;~a~*~*~]"
            
            time
            *time-cols*
            sec ms
            nil
            (act-r-event-mp event)
            
            (< 1 mcount)
            name-len
            
            
            (if (act-r-model-p (act-r-event-model event))
                (act-r-model-name (act-r-event-model event))
              (act-r-event-model event))
            
            *module-cols*
            (act-r-event-module event)
            
            (act-r-event-details event)
            (act-r-event-details event)
            (act-r-event-action event)
            (when (act-r-event-output event) (act-r-event-params event))))))))


(defmethod tutorial-format-event ((event act-r-break-event))
  (let ((*print-pretty* nil)
        (mp (current-mp)))
    (let (mcount name-len)
      (bt:with-lock-held ((meta-p-models-lock mp))
        (setf mcount (meta-p-model-count mp)
          name-len (meta-p-model-name-len mp)))
      
      (let ((time (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime event))))
        (multiple-value-bind (sec ms) (when (numberp time) (truncate time 1000))
          (format nil "~:[~*~*~*~;~vd.~3,'0d~] ~:[~*~;~a ~] ~:[~2*~;~va ~] ~va BREAK-EVENT ~@[~a ~]"
            
            time
            *time-cols*
            sec ms
            
            nil
            (act-r-event-mp event)
            (< 1 mcount)
            name-len
            (subseq "------" 0 (min 6 name-len))
            *module-cols*
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