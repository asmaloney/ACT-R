;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2017 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : trace-history.lisp
;;; Version     : 1.0
;;; 
;;; Description : Component for recording the event trace -- replaces the 
;;;             : printing modules :save-trace functionality.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2017.09.19 Dan [1.0]
;;;             : * Initial creation as a split from printing module and the
;;;             :   environment-specific history mechanism.
;;; 2017.09.20 Dan
;;;             : * Mark it as non-model based since it uses a component and 
;;;             :   doesn't depend on a current model for tracking things.
;;;             : * Accept the optional parameters for the status function as
;;;             :   well and use those to determine if there's any data which
;;;             :   matches available.
;;; 2017.09.21 Dan
;;;             : * Return a third value of t for the status checks to indicate
;;;             :   that it's always available.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct trace-history (lock (bt:make-lock "Trace history")) trace event)

(defun remove-trace-history (th)
  (bt:with-lock-held ((trace-history-lock th))
    (setf (trace-history-trace th) nil)
    (when (trace-history-event th)
      (delete-event-hook (trace-history-event th))
      (setf (trace-history-event th) nil))))

(defun reset-trace-history (th)
  (bt:with-lock-held ((trace-history-lock th))
    (setf (trace-history-trace th) nil)))


(defun trace-history-recorder (event)
  (let ((th (get-component trace-history)))
      (bt:with-lock-held ((trace-history-lock th))
        (push-last (list (evt-mstime event) (evt-output event) (evt-model event) (format-event event))
                   (trace-history-trace th)))))

(defun enable-trace-history ()
  (let ((th (get-component trace-history)))
    (bt:with-lock-held ((trace-history-lock th))
      (unless (trace-history-event th)
        (setf (trace-history-event th)
          (add-post-event-hook 'trace-history-recorder))))))

(defun disable-trace-history ()
  (let ((th (get-component trace-history)))
    (bt:with-lock-held ((trace-history-lock th))
      (when (trace-history-event th)
        (delete-event-hook (trace-history-event th))
        (setf (trace-history-event th) nil)))))

(defun check-trace-history (&optional start end (detail 'medium) model)
  (let ((th (get-component trace-history)))
    (bt:with-lock-held ((trace-history-lock th))
      (values (when (trace-history-event th) t)
              (not (null (check-saved-trace th start end detail model)))
              t))))
  

(define-component trace-history :version "1.0" :documentation "Component for recording trace information"
  :creation make-trace-history :before-reset reset-trace-history :clear-all remove-trace-history)

(defun get-saved-trace (&optional s e (d 'medium) m)
  
  (let ((detail (string->name d))
        (model (string->name m))
        (start (string->name s))
        (end (string->name e)))
    
  (cond ((and start (not (numberp start)))
         (print-warning "Invalid start for get-saved-trace ~s.  Must be a number indicating a time in seconds." start))
        ((and end (not (numberp end)))
         (print-warning "Invalid end for get-saved-trace ~s.  Must be a number indicating a time in seconds." end))
        ((and (numberp start) (numberp end) (< end start))
         (print-warning "End time for get-saved-trace must be greater than the start time, but given start=~f and end=~f" start end))
        ((not (position detail '(t low medium high)))
         (print-warning "Invalid detail level for get-saved-trace ~s.  Possible values are: t, low, medium, and high." detail))
        (t
         (let ((th (get-component trace-history)))
           (bt:with-lock-held ((trace-history-lock th))
             (let* ((s (when (numberp start) (seconds->ms start)))
                    (e (when (numberp end) (seconds->ms end)))
                    
                    (full-trace (trace-history-trace th))
                    (start-index (if s
                                     (aif (position-if (lambda (x) (>= (first x) s)) full-trace)
                                          it 
                                          (length full-trace))
                                   0))
                    (end-index (when e (position-if (lambda (x) (> (first x) e)) full-trace)))
                    (trace-segment (subseq full-trace start-index end-index)))
               (mapcan (lambda (x)
                         (when (and (or (eq detail t)
                                        (case detail
                                          (low (eq (second x) 'low))
                                          (medium (or (eq (second x) 'low)
                                                      (eq (second x) 'medium)))
                                          (high (second x))))
                                    (or (null model)
                                        (or (null (third x)) (eq (third x) model))))
                           (list x)))
                 trace-segment))))))))

(defun check-saved-trace (th &optional s e (d 'medium) m)
  
  (let ((detail (string->name d))
        (model (string->name m))
        (start (string->name s))
        (end (string->name e)))
    
  (cond ((and start (not (numberp start)))
         (print-warning "Invalid start for check-saved-trace ~s.  Must be a number indicating a time in seconds." start))
        ((and end (not (numberp end)))
         (print-warning "Invalid end for check-saved-trace ~s.  Must be a number indicating a time in seconds." end))
        ((and (numberp start) (numberp end) (< end start))
         (print-warning "End time for check-saved-trace must be greater than the start time, but given start=~f and end=~f" start end))
        ((not (position detail '(t low medium high)))
         (print-warning "Invalid detail level for check-saved-trace ~s.  Possible values are: t, low, medium, and high." detail))
        (t
         (let* ((s (when (numberp start) (seconds->ms start)))
                (e (when (numberp end) (seconds->ms end)))
                
                (full-trace (trace-history-trace th))
                (start-index (if s
                                 (aif (position-if (lambda (x) (>= (first x) s)) full-trace)
                                      it 
                                      (length full-trace))
                               0))
                (end-index (when e (position-if (lambda (x) (> (first x) e)) full-trace)))
                (trace-segment (subseq full-trace start-index end-index)))
           (mapcan (lambda (x)
                     (when (and (or (eq detail t)
                                    (case detail
                                      (low (eq (second x) 'low))
                                      (medium (or (eq (second x) 'low)
                                                  (eq (second x) 'medium)))
                                      (high (second x))))
                                (or (null model)
                                    (or (null (third x)) (eq (third x) model))))
                       (list x)))
             trace-segment))))))

(defun show-saved-trace (&optional start end (detail 'medium) model)
  (let ((trace (get-saved-trace start end detail model)))
    (dolist (x trace)
      (command-output (fourth x)))))

(define-history "trace-history" enable-trace-history disable-trace-history check-trace-history get-saved-trace nil)

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
