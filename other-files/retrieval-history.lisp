;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : retrieval-history.lisp
;;; Version     : 2.1
;;; 
;;; Description : Code to record retrieval history data.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2008.08.14 Dan
;;;             : * Initial creation.
;;; 2011.06.22 Dan [1.1]
;;;             : * Added another pane to the display which shows the activation
;;;             :   trace information as saved with the new :sact parameter.
;;; 2012.02.08 Dan
;;;             : * Added checks so that trying to get information from an old
;;;             :   display no longer throws an error.
;;; 2012.02.09 Dan
;;;             : * Explicitly close streams made with make-string-output-stream 
;;;             :   to be safe.
;;; 2012.03.21 Dan
;;;             : * Use with-parameters instead of saving and setting :cmdt 
;;;             :   explicitly.
;;; 2012.03.27 Dan
;;;             : * Fixed dm-history-chunk-display so that it doesn't show 
;;;             :   "History no longer available" when there was a failure.
;;; 2015.05.05 Dan
;;;             : * Added a suppress-warnings to the recording of the sdp info
;;;             :   in dm-retrieval-set-recorder since there might be issues 
;;;             :   with negative Sjis that don't matter in the actual model
;;;             :   so don't want to see those warnings from the recorder.
;;; 2015.06.09 Dan
;;;             : * Record time in ms internally, but still show seconds to the
;;;             :   user in the list.
;;; 2016.04.22 Dan 
;;;             : * Start of the work to support the new history tools that 
;;;             :   allow saving data and then displaying that saved data.
;;;             : * Now also turn on :sact automatically with :save-dm-history.
;;; 2016.04.28 Dan
;;;             : * Use the key from the environment to access the appropriate
;;;             :   underlying data source.
;;; 2017.08.09 Dan
;;;             : * Replacing calls to capture-model-output with the corresponding
;;;             :   direct output capture functions: printed-chunk-spec, printed-chunk,
;;;             :   and printed-chunk-activation-trace.
;;; 2017.09.22 Dan [2.0]
;;;             : * Making this a history stream and eliminating the parameter
;;;             :   for enabling it.
;;; 2018.04.18 Dan
;;;             : * Adding an optional parameter for the history stream to allow
;;;             :   getting a single request's data.
;;; 2018.04.27 Dan
;;;             : * Moved the retrieval-times processor for the history here.
;;; 2020.01.13 Dan [2.1]
;;;             : * Removed the lambdas from the module interface.
;;;             : * Use the (:remove <>) option for the hook functions.
;;; 2020.08.27 Dan
;;;             : * Switch from (no-output (sgp ...)) to get-parameter and cast
;;;             :   the result as a real to avoid a warning in SBCL.
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

(defstruct dm-history-module
  history
  (lock (bt:make-lock "dm-history-module"))
  (alive t))
  

(defstruct dm-history
  time
  request
  chunks
  chunk-texts
  params
  activation-trace)

(defun dm-request-recorder (request)
  (let ((history (get-module retrieval-history))
        (block (make-dm-history :time (mp-time-ms) :request (printed-chunk-spec request))))
    (bt:with-lock-held ((dm-history-module-lock history))
      (push block (dm-history-module-history history))
      nil)))

(defun dm-retrieval-set-recorder (set)
  (let ((history (get-module retrieval-history))
        (best (car set)))
    (bt:with-lock-held ((dm-history-module-lock history))
      (let ((record (car (dm-history-module-history history))))
    
        (cond ((null set)
               (setf (dm-history-chunks record) (list :retrieval-failure)))
              ((and best (get-parameter-value :esc)
                    (let ((a (no-output (caar (sdp-fct (list best :last-retrieval-activation))))))
                      (when (numberp a) ;; will always be true, but testing to avoid an SBCL compiler warning
                        (< a (the real (get-parameter-value :rt))))))
               (setf (dm-history-chunks record) (cons :retrieval-failure set)))
              (t 
               (setf (dm-history-chunks record) set)))
        
        (dolist (x (dm-history-chunks record))
          (if (eq x :retrieval-failure)
              (progn
                (push-last "" (dm-history-chunk-texts record))
                (push-last "" (dm-history-activation-trace record))
                (push-last "" (dm-history-params record)))
            (progn
              (push-last (printed-chunk x) (dm-history-chunk-texts record))
              (push-last (printed-chunk-activation-trace x (mp-time-ms) t)
                         (dm-history-activation-trace record))
              (let ((s (suppress-warnings (capture-command-output (sdp-fct (list x))))))
                (push-last s (dm-history-params record)))))))))
  nil)



(defun reset-dm-history-module (module)
  (bt:with-lock-held ((dm-history-module-lock module))
   (setf (dm-history-module-history module) nil)))

(defun delete-dm-history-module (module)
  (bt:with-lock-held ((dm-history-module-lock module))
    (setf (dm-history-module-alive module) nil)))

(defun enable-retrieval-history ()
  (no-output
   (unless (find 'dm-request-recorder (car (sgp :retrieval-request-hook)))
     (sgp :retrieval-request-hook dm-request-recorder))
   (unless (find 'dm-retrieval-set-recorder (car (sgp :retrieval-set-hook)))
     (sgp :retrieval-set-hook dm-retrieval-set-recorder))
   (unless (car (sgp :sact))
     (sgp :sact t))))

  
(defun disable-retrieval-history ()  
  (no-output
   (let ((current-hooks (car (sgp :retrieval-request-hook))))
     (when (find 'dm-request-recorder current-hooks)
       (sgp :retrieval-request-hook (:remove dm-request-recorder)))
     (setf current-hooks (car (sgp :retrieval-set-hook)))
     (when (find 'dm-retrieval-set-recorder current-hooks)
       (sgp :retrieval-set-hook (:remove dm-retrieval-set-recorder))))))

(defun retrieval-history-status (&optional time)
  (let ((m (get-module retrieval-history)))
    (if m
        (bt:with-lock-held ((dm-history-module-lock m))
          (no-output 
           (values 
            (and (car (sgp :sact))
                 (find 'dm-request-recorder (car (sgp :retrieval-request-hook)))
                 (find 'dm-retrieval-set-recorder (car (sgp :retrieval-set-hook)))
                 t)
            (if (numberp time)
                (when (find time (dm-history-module-history m) :key 'dm-history-time) t)
              (when (dm-history-module-history m) t))
            (dm-history-module-alive m))))
      (values nil nil nil))))

(defun create-retrieval-history-module (x)
  (declare (ignore x))
  (make-dm-history-module))


(define-module-fct 'retrieval-history nil nil
  :creation 'create-retrieval-history-module
  :reset 'reset-dm-history-module
  :delete 'delete-dm-history-module
  :version "2.1"
  :documentation "Module to record retrieval history data.")

(defun get-retrieval-history (&optional time)
  (let ((m (get-module retrieval-history)))
    (when m
      (bt:with-lock-held ((dm-history-module-lock m))
        (if (numberp time)
            (awhen (find time (dm-history-module-history m) :key 'dm-history-time)
                   (list (list (dm-history-time it)
                               (dm-history-request it)
                               (mapcar 'list 
                                 (dm-history-chunks it)
                                 (dm-history-chunk-texts it)
                                 (dm-history-params it)
                                 (dm-history-activation-trace it)))))
          (mapcar (lambda (x)
                    (list (dm-history-time x)
                          (dm-history-request x)
                          (mapcar 'list 
                            (dm-history-chunks x)
                            (dm-history-chunk-texts x)
                            (dm-history-params x)
                            (dm-history-activation-trace x))))
            (reverse (dm-history-module-history m))))))))

(define-history "retrieval-history" enable-retrieval-history disable-retrieval-history retrieval-history-status get-retrieval-history t)


(defun retrieval-times (history)
  (let ((data (json::decode-json-from-string history)))
    (mapcar (lambda (x)
              (list (first x)
                    (mapcar 'car (third x))))
      data)))
(define-history-processor "retrieval-history" "retrieval-times" retrieval-times)


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
