;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008-2011 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : buffer-history.lisp
;;; Version     : 2.1
;;; 
;;; Description : Code to support the buffer history tool in the environment.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [x] Consider converting the internal time over to milliseconds
;;;             :     by using buffer-record-ms-time instead of -time-stamp.
;;;             :     The down side of that is the display for the environment
;;;             :     should probably still be in seconds.
;;; 
;;; ----- History -----
;;; 2008.08.20 Dan
;;;             : * Initial creation.
;;; 2011.04.25 Dan
;;;             : * Added the "to do" to consider.
;;; 2011.06.20 Dan [1.1]
;;;             : * Changed the tool to record all the details everytime, not 
;;;             :   just when there is a "change" to the buffer because some
;;;             :   changes are below what the buffer trace detects i.e. custom
;;;             :   queries for modules.
;;; 2011.06.24 Dan
;;;             : * Changed the output so that the query info is first so that
;;;             :   it's easier to watch that for a change in a buffer since it
;;;             :   will be formatted consistently.
;;; 2013.11.15 Dan
;;;             : * Chunk contents are now displayed at time stamps when there
;;;             :   isn't an "action" on that buffer which was a bug because of
;;;             :   how the events were recorded and compared.
;;;             : * Removed the special :cleared marker for the chunk details
;;;             :   since it wasn't really being used -- now it's either the
;;;             :   string of the chunk details or nil if the buffer is empty or
;;;             :   being cleared at that time.
;;; 2015.06.09 Dan
;;;             : * Record time in ms internally, but still show seconds to the
;;;             :   user in the list.
;;; 2016.04.22 Dan
;;;             : * Start of the upgrade to allow saving history info so that it
;;;             :   can be reloaded for the environment tools.
;;; 2016.04.27 Dan
;;;             : * Modify the environment interface functions so that they use
;;;             :   the indicated underlying data.
;;; 2016.05.04 Dan
;;;             : * Just some minor code clean-up in the param function.
;;; 2016.05.06 Dan
;;;             : * Fixed an issue with buffers that were cleared and then set
;;;             :   at the same time -- before the end state showed as empty.
;;;             : * Also marked the to do as complete since that happened with
;;;             :   the 2015.06.09 update.
;;; 2016.05.16 Dan
;;;             : * Previous fix actually broke the chunk recording so this now
;;;             :   gets it right.
;;; 2017.09.28 Dan [2.0]
;;;             : * Reworking the buffer history to use the history data stream
;;;             :   mechanism and to not rely upon the buffer trace recorder.
;;;             :   Now it will record all the buffer information itself using
;;;             :   an event hook for all buffers regardless of the :traced-
;;;             :   buffers setting and it will record the information at the
;;;             :   start of a time stamp and after each action so that it can
;;;             :   report start of time stamp, after action, and end of time
;;;             :   stamp results (where end is start of next if available).
;;; 2018.02.05 Dan
;;;             : * Rework this to actually use the internal event structs 
;;;             :   directly since it'll get an id now.
;;; 2018.02.22 Dan
;;;             : * Need to protect access to time using meta-p-schedule-lock.
;;; 2020.01.13 Dan [2.1]
;;;             : * Removed the lambda from the module interface. 
;;; 2021.06.07 Dan 
;;;             : * Deal with set-buffer-chunk and overwrite-... possibly having
;;;             :   a chunk-spec as the second parameter.
;;; 2021.10.18 Dan
;;;             : * Changed call to buffers to model-buffers instead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
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


(defclass buffer-history-module ()
  ((history-lock :accessor history-lock :initform (bt:make-lock "buffer-history") :allocation :class)
   (save-history :accessor save-history :initform nil)
   (alive :accessor alive :initform t)
   (start-time-data :accessor start-time-data :initform nil)
   (recorded-data :accessor recorded-data :initform nil)
   (current-data :accessor current-data :initform nil)
   (last-time-stamp :accessor last-time-stamp :initform -1 :allocation :class)
   (pre-recorder :accessor pre-recorder :initform nil :allocation :class)
   (post-recorder :accessor post-recorder :initform nil :allocation :class)
   (monitor-count :accessor monitor-count :initform 0 :allocation :class)))


(defvar *top-level-buffer-history-module* (make-instance 'buffer-history-module))

(defun buffer-start-time-recorder (evt)
  (bt:with-lock-held ((history-lock *top-level-buffer-history-module*))
    (let ((event (get-event-by-id evt)))
      (unless (= (last-time-stamp *top-level-buffer-history-module*) (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime event)))
        (dolist (m (mp-models))
          (with-model-eval m
            (let ((module (get-module buffer-history)))
              (when (save-history module)
                (let ((last (start-time-data module)))
                  (setf (start-time-data module)
                    (mapcar (lambda (bn)
                              (list bn
                                    (aif (buffer-read bn) (printed-chunk it) "buffer empty")
                                    (printed-buffer-status bn)))
                      (model-buffers)))
                  (unless (= (last-time-stamp module) -1)
                    (let ((results nil))
                      (dolist (x (current-data module))
                        (push-last (concatenate 'list x (cdr (find (first x) last :key 'first)) (cdr (find (first x) (start-time-data module) :key 'first)))
                                   results))
                      (push-last (list (last-time-stamp module) results) (recorded-data module))))
                  (setf (current-data module) nil)))))))
      (setf (last-time-stamp *top-level-buffer-history-module*) (bt:with-recursive-lock-held ((meta-p-schedule-lock (current-mp))) (act-r-event-mstime event))))))

(defun buffer-event-recorder (evt)
  (when (current-model)
    (let ((module (get-module buffer-history)))
      (bt:with-lock-held ((history-lock module))
        (let ((event (get-event-by-id evt)))
          (when (save-history module)
            (case (act-r-event-action event)
              (set-buffer-chunk
               (push-last (list (first (act-r-event-params event)) 
                                "set-buffer-chunk" 
                                (if (symbolp (second (act-r-event-params event)))
                                    (printed-chunk (second (act-r-event-params event)))
                                  (printed-chunk-spec (second (act-r-event-params event))))
                                (printed-buffer-status (first (act-r-event-params event))))
                          (current-data module)))
              (clear-buffer
               (push-last (list (first (act-r-event-params event)) "clear-buffer" "" (printed-buffer-status (first (act-r-event-params event))))
                          (current-data module)))
              (mod-buffer-chunk
               (push-last (list (first (act-r-event-params event)) "mod-buffer-chunk" (printed-chunk-spec (second (act-r-event-params event))) (printed-buffer-status (first (act-r-event-params event))))
                          (current-data module)))
              (overwrite-buffer-chunk
               (push-last (list (first (act-r-event-params event)) 
                                "overwrite-buffer-chunk" 
                                (if (symbolp (second (act-r-event-params event)))
                                    (printed-chunk (second (act-r-event-params event)))
                                  (printed-chunk-spec (second (act-r-event-params event))))
                                (printed-buffer-status (first (act-r-event-params event))))
                          (current-data module)))
              (module-request
               (push-last (list (first (act-r-event-params event)) "module-request" (printed-chunk-spec (second (act-r-event-params event))) (printed-buffer-status (first (act-r-event-params event))))
                          (current-data module)))
              (module-mod-request
               (push-last (list (first (act-r-event-params event)) "module-mod-request" (printed-chunk-spec (second (act-r-event-params event))) (printed-buffer-status (first (act-r-event-params event))))
                          (current-data module)))
              (erase-buffer
               (push-last (list (first (act-r-event-params event)) "erase-buffer" "" (printed-buffer-status (first (act-r-event-params event))))
                          (current-data module))))))))))
  
  
(defun reset-buffer-history-module (module)
  (bt:with-lock-held ((history-lock module))
    (setf (recorded-data module) nil)
    (setf (current-data module) nil)
    (setf (last-time-stamp module) -1)
    (setf (start-time-data module) nil)
  
    (when (save-history module)
      (setf (save-history module) nil)
      (decf (monitor-count module))
      (when (zerop (monitor-count module))
        (delete-event-hook (pre-recorder module))
        (delete-event-hook (post-recorder module))))))


(defun disable-buffer-history ()
  (let ((module (get-module buffer-history)))
    (bt:with-lock-held ((history-lock module))
      (when (save-history module)
        (setf (save-history module) nil)
        (decf (monitor-count module))
        (when (zerop (monitor-count module))
          (delete-event-hook (pre-recorder module))
          (delete-event-hook (post-recorder module))))
      t)))


(defun delete-buffer-history-module (module)
  (bt:with-lock-held ((history-lock module))
    (setf (alive module) nil)
    (when (save-history module)
      (setf (save-history module) nil)
      (decf (monitor-count module))
      (when (zerop (monitor-count module))
        (delete-event-hook (pre-recorder module))
        (delete-event-hook (post-recorder module))))
    t))


(defun create-buffer-history-module (x)
  (declare (ignore x))
  (make-instance 'buffer-history-module))

(define-module-fct 'buffer-history nil nil
  :creation 'create-buffer-history-module
  :reset 'reset-buffer-history-module
  :delete 'delete-buffer-history-module
  :version "2.1"
  :documentation "Module to record buffer change history.")
  

(defun enable-buffer-history ()
  (let ((module (get-module buffer-history)))
    (bt:with-lock-held ((history-lock module))
      (unless (save-history module)
        (setf (save-history module) t)
        (when (zerop (monitor-count module))
          (setf (pre-recorder module) (add-pre-event-hook 'buffer-start-time-recorder))
          (setf (post-recorder module) (add-post-event-hook 'buffer-event-recorder)))
        (incf (monitor-count module)))
      t)))


(defun buffer-history-status ()
  (let ((module (get-module buffer-history)))
    (bt:with-lock-held ((history-lock module))
      (values (save-history module) (when (or (recorded-data module) (current-data module)) t) (alive module)))))

(defun get-buffer-history ()
  (let ((module (get-module buffer-history)))
    (if (null (current-data module))
        (recorded-data module)
      (let ((results nil))
        (dolist (x (current-data module))
          (push-last (concatenate 'list x (cdr (find (first x) (start-time-data module) :key 'first)) (list "" ""))
                     results))
        (append (recorded-data module)  (list (list (last-time-stamp module) results)))))))

(define-history "buffer-history" enable-buffer-history disable-buffer-history buffer-history-status get-buffer-history)

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
