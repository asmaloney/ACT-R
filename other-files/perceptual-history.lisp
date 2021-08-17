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
;;; Filename    : perceptual-history.lisp
;;; Version     : 3.1
;;; 
;;; Description : Record visicon and audicon data when requested.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2016.04.06 Dan [1.0a1] 
;;;             : * Initial creation.
;;; 2016.04.11 Dan [1.0a2]
;;;             : * Doesn't work right in a multi-model situation since each
;;;             :   model wants to remove the event hook as soon as it no longer
;;;             :   needs it.  Instead keep a counter with the hook so that all
;;;             :   models in a meta-process know if anyone else is using it.
;;; 2016.04.12 Dan
;;;             : * Fix the text box response functions for the environment so
;;;             :   that they don't display nil when there's nothing to show.
;;; 2016.04.15 Dan
;;;             : * Fixed a bug in setting :save-audicon-history -- turning it
;;;             :   off turned off the visicon history.
;;; 2016.04.22 Dan
;;;             : * Added the "get" functions for the data to support the saving
;;;             :   of data from the environment. 
;;; 2016.04.27 Dan
;;;             : * Changed the environment interface functions to also accept
;;;             :   a key for the data and then call get-history-information 
;;;             :   instead of using the module info directly.
;;; 2016.05.18 Dan
;;;             : * Fixed a bug with tracking the data for wheteher or not to
;;;             :   keep the event hook in place.
;;; 2016.06.24 Dan [1.0a3]
;;;             : * The after methods for process-display and new-sound-event
;;;             :   didn't check whether the module actually enabled the recording
;;;             :   of the data and thus were always capturing that component of
;;;             :   things.
;;; 2016.11.17 Dan
;;;             : * Replace current-meta-process with current-mp.
;;; 2016.12.19 Dan [2.0]
;;;             : * Start the transition to the new central dispatcher and don't
;;;             :   rely on the methods of the modules themselves.  For vision 
;;;             :   now record visicon updates using a monitor on the update-visicon
;;;             :   command instead of a method on process-display.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.08.03 Dan
;;;             : * Monitor new-sound instead of adding a method on new-sound-event.
;;;             : * Make the visicon monitor a simple monitor command.
;;; 2017.08.09 Dan
;;;             : * Use printed-visicon and printed-audicon instead of capturing
;;;             :   the output of the commands.
;;;             : * Don't need the event hooks anymore.
;;;             : * Adding a state-lock to protect the internals and params.
;;;             : * Only monitor when needed instead of always.
;;; 2017.09.20 Dan [3.0]
;;;             : * Use the history recorder mechanism and eliminate the parameters
;;;             :   for saving the info.  Still need to track whether or not the
;;;             :   monitor is enabled, but that's easier since the bulk of the
;;;             :   work is handled by the history recorder interface and there's
;;;             :   only one meta-process.
;;;             : * Makes sense to just have two modules now as well using a 
;;;             :   single simpler class.
;;; 2017.09.21 Dan
;;;             : * Cleaned up the interface code and defined all the history
;;;             :   stream functions necessary.
;;;             : * Added the alive slot so that it can provide that in the 
;;;             :   status which is necessary when deleting the module because
;;;             :   don't want to assume the stop recordings will come in, and
;;;             :   even if they do, there's a potential race with respect to 
;;;             :   deletion order (assuming the stops are also driven by deletion).
;;; 2018.06.22 Dan
;;;             * Added "Do not call directly" to command the doc strings.
;;; 2020.01.13 Dan [3.1]
;;;             : * Removed the lambdas from the module interfaces.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; To support visicon history and audicon history tools in the Environment there
;;; needs to be something which saves the information.
;;; Since it's being saved might as well also provide commands for a text output
;;; as well.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;; show-visicon-history 
;;; 
;;; Takes no parameters.  If there is any saved history for the visicon then for
;;; each time that there was a visicon change recorded this command will print
;;; the time of the change followed by the output from print-visicon at that
;;; time.  It's possible that there could be more than one change at a given
;;; time, in which case the different print-visicon outputs will be shown in 
;;; the order that they occurred (the older ones before the new ones).  It will
;;; always return nil.
;;;
;;; show-audicon-history 
;;; 
;;; Takes no parameters.  If there is any saved history for the audicon then for
;;; each time that there was an audicon change recorded this command will print
;;; the time of the change followed by the output from print-audicon at that
;;; time.  It's possible that there could be more than one change at a given
;;; time, in which case the different print-audicon outputs will be shown in 
;;; the order that they occurred (the older ones before the new ones).  It will
;;; always return nil.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Use the monitoring functionality of the dispatcher to get called when needed
;;; for visicon-update and new-sound actions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defclass perceptual-history ()
  ((perceptual-history :accessor perceptual-history :initform nil)
   (history-lock :accessor history-lock :initform (bt:make-lock "perceptual-history"))
   (last-item :accessor last-item :initform "")
   (save-history :accessor save-history :initform nil)
   (alive :accessor alive :initform t)
   (cmd-to-monitor :accessor cmd-to-monitor :initform nil :initarg :cmd)
   (recorder :accessor recorder :initform nil :initarg :recorder)
   (data-cmd :accessor data-cmd :initform nil :initarg :data-cmd)))

(defclass audicon-history (perceptual-history)
  ((monitor-count :accessor monitor-count :initform 0 :allocation :class))
  (:default-initargs 
      :cmd "new-sound" 
    :recorder "monitor-audicon-updates" 
    :data-cmd 'printed-audicon))
      
(defclass visicon-history (perceptual-history)
  ((monitor-count :accessor monitor-count :initform 0 :allocation :class))
  (:default-initargs 
      :cmd "visicon-update" 
    :recorder "monitor-visicon-updates" 
    :data-cmd 'printed-visicon))      
    
 
      
(defun monitor-visicon-updates ()
  (let ((instance (get-module :visicon-history)))
    (perceptual-history-update instance "visicon")))

(defun monitor-audicon-updates ()
  (let ((instance (get-module :audicon-history)))
    (perceptual-history-update instance "audicon")))

(add-act-r-command "monitor-visicon-updates" 'monitor-visicon-updates "Perceptual history tool for recording visicon updates via a monitor of visicon-update. Do not call directly.")
(add-act-r-command "monitor-audicon-updates" 'monitor-audicon-updates "Perceptual history tool for recording audicon updates via a monitor of new-sound. Do not call directly.")

(defun perceptual-history-update (instance name)
  (if instance
      (bt:with-lock-held ((history-lock instance))
        (when (save-history instance)
          (let ((current (funcall (data-cmd instance))))
            (unless (string-equal (last-item instance) current)
              (setf (last-item instance) current)
              (let ((last-time (caar (last (perceptual-history instance)))))
                (if (and last-time (= last-time (mp-time-ms)))
                    (push-last current (car (last (perceptual-history instance))))
                  (push-last (list (mp-time-ms) current) (perceptual-history instance))))))))
    (print-warning "No ~a history module available cannot record ~a information" name name)))


(defun reset-perceptual-history (module)
  (bt:with-lock-held ((history-lock module))
    (setf (perceptual-history module) nil)
    (setf (last-item module) "")
    (when (save-history module)
      (setf (save-history module) nil)
      (decf (monitor-count module))
      (when (zerop (monitor-count module))
        (remove-act-r-command-monitor (cmd-to-monitor module) (recorder module))))))

(defun disable-perceptual-history (module)
  (bt:with-lock-held ((history-lock module))
    (when (save-history module)
      (setf (save-history module) nil)
      (decf (monitor-count module))
      (when (zerop (monitor-count module))
        (remove-act-r-command-monitor (cmd-to-monitor module) (recorder module))))
    t))


(defun delete-perceptual-history (module)
  (bt:with-lock-held ((history-lock module))
    (setf (alive module) nil)
    (when (save-history module)
      (setf (save-history module) nil)
      (decf (monitor-count module))
      (when (zerop (monitor-count module))
        (remove-act-r-command-monitor (cmd-to-monitor module) (recorder module))))
    t))

(defun create-audicon-history-module (x) 
  (declare (ignore x))
  (make-instance 'audicon-history))

(defun create-visicon-history-module (x) 
  (declare (ignore x))
  (make-instance 'visicon-history))

(define-module-fct :audicon-history nil nil
  :creation 'create-audicon-history-module
  :reset 'reset-perceptual-history
  :delete 'delete-perceptual-history
  :version "3.1"
  :documentation "Module to record audicon changes.")

(define-module-fct :visicon-history nil nil
  :creation 'create-visicon-history-module
  :reset 'reset-perceptual-history
  :delete 'disable-perceptual-history
  :version "3.1"
  :documentation "Module to record visicon changes.")



(defun show-visicon-history (&optional start end)
  (let ((instance (get-module :visicon-history)))
    (show-perceptual-history instance "visicon" start end)))

(defun show-audicon-history (&optional start end)
  (let ((instance (get-module :audicon-history)))
    (show-perceptual-history instance "audicon" start end)))

(defun show-perceptual-history (instance name start end)
  (if instance
      (cond ((not (numornil start))
             (print-warning "Start value for show-~a-history must be a number or nil, but given ~s" name start))
            ((not (numornil end))
             (print-warning "End value for show-~a-history must be a number or nil, but given ~s" name end))
            ((and (numberp start) (numberp end) (> start end))
             (print-warning "End value for show-~a-history must be greater-than or equal to the start value, but given start=~f and end=~f" name start end))
            (t
             (bt:with-lock-held ((history-lock instance))
               
               (let* ((history (perceptual-history instance))
                      (si (if (numberp start) 
                              (aif (position-if (lambda (x) (>= (first x) (seconds->ms start))) history)
                                   it
                                   (length history))
                            0))
                      (ei (if (numberp end)
                              (position-if (lambda (x) (> (first x) (seconds->ms end))) history)
                            (length history))))
                 (dolist (x (subseq history si ei) (copy-seq (subseq history si ei)))
                   (command-output "Time: ~f~%~{~a~}" (ms->seconds (first x)) (rest x)))))))
        (print-warning "No ~a history module available cannot show ~a history" name name)))

(defun enable-audicon-history ()
  (enable-perceptual-history (get-module :audicon-history)))

(defun enable-visicon-history ()
  (enable-perceptual-history (get-module :visicon-history)))

(defun enable-perceptual-history (module)
  (bt:with-lock-held ((history-lock module))
    (unless (save-history module)
      (setf (save-history module) t)
      (when (zerop (monitor-count module))
        (monitor-act-r-command (cmd-to-monitor module) (recorder module)))
      (incf (monitor-count module)))
    t))

(defun disable-audicon-history ()
  (disable-perceptual-history (get-module :audicon-history)))

(defun disable-visicon-history ()
  (disable-perceptual-history (get-module :visicon-history)))


(defun audicon-history-status ()
  (perceptual-history-status (get-module :audicon-history)))

(defun visicon-history-status ()
  (perceptual-history-status (get-module :visicon-history)))

(defun perceptual-history-status (module)
  (bt:with-lock-held ((history-lock module))
    (values (save-history module) (when (perceptual-history module) t) (alive module))))


(defun get-audicon-history ()
  (get-perceptual-history (get-module :audicon-history)))

(defun get-visicon-history ()
  (get-perceptual-history (get-module :visicon-history)))

(defun get-perceptual-history (module)
  (bt:with-lock-held ((history-lock module))
    (copy-list (perceptual-history module))))

(define-history "audicon-history" enable-audicon-history disable-audicon-history audicon-history-status get-audicon-history)
(define-history "visicon-history" enable-visicon-history disable-visicon-history visicon-history-status get-visicon-history)

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
