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
;;; Filename    : top-level.lisp
;;; Version     : 3.0
;;; 
;;; Description : The framework's top level user commands that aren't
;;;               part of another section.
;;; 
;;; Bugs        : 
;;;
;;; To do       : * Documentation.
;;;             : * Make reload/clear-all smart about compiled files so that 
;;;             :   it knows to check the .lisp if the current file is a
;;;             :   compiled one.
;;;             : [X] Why doesn't clear-all use the reset-mp command?
;;;             : [ ] Deal with clear-all not checking the top-level-lock.
;;; 
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation
;;; 2005.01.29 Dan
;;;             : * Added an optional parameter to reload that if specified as
;;;             :   non-nil will use compile-and-load to load the file.
;;; 2005.02.11 Dan
;;;             : * Changed clear-all to use clrhash instead of building new
;;;             :   tables.
;;; 2005.02.22 Dan
;;;             : * Modified reset so it better reports what's happening in
;;;             :   other than normal circumstances.
;;; 2009.10.27 Dan
;;;             : * Fixed clear-all so that it also restores the meta-process
;;;             :   timing code to the default if it has been changed by mp-
;;;             :   real-time-management.
;;; 2009.12.03 Dan
;;;             : * Clear the meta-process dynamics and in-slack on a clear-all too.
;;; 2009.12.04 Dan
;;;             : * Use reset-mp in clear-all to avoid duplicate code.
;;; 2012.01.11 Dan
;;;             : * Added a safety check to clear-all which throws an error if
;;;             :   it's called while there's a meta-process running.  This 
;;;             :   should avoid situations which can be very problematic when
;;;             :   model files get loaded while things are running.
;;; 2012.08.08 Dan
;;;             : * Change reset so that it uses meta-p-model-order so that 
;;;             :   things work the same at reset and reload times and between
;;;             :   Lisps.
;;; 2016.09.28 Dan
;;;             : * Removed code relating to multiple meta-processes.
;;;             : * Reset now returns t instead of a meta-process name.
;;; 2016.11.16 Dan [2.0]
;;;             : * Making the commands available through the central dispatcher
;;;             :   and having the user commands execute them that way.  Only
;;;             :   doing reset and reload through the dispatcher right now 
;;;             :   because I'm not sure about top-level model file commands
;;;             :   at this point.
;;; 2016.11.17 Dan
;;;             : * Changed execute-act-r-command to evaluate-act-r-command.
;;; 2016.12.07 Dan
;;;             : * Updated the reload internal function so that it captures
;;;             :   the non-ACT-R output during the load the same way load does.
;;; 2017.01.18 Dan
;;;             : * Removed the echo-act-r-output from commands.  The assumption
;;;             :   now is that echoing is handled by the user.
;;;             : * Only return the actual result of evaluating reset and reload
;;;             :   printing errors using handle-evaluate-results.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.03.30 Dan
;;;             : * Don't need meta-p-current-model in clear-all.
;;; 2017.05.30 Dan
;;;             : * Adding the call to component functions into clear-all.
;;; 2017.06.22 Dan
;;;             : * Protect meta-p-model-order and meta-p-models with lock.
;;; 2017.06.30 Dan
;;;             : * Protect the meta-p event-hook related slots.
;;; 2017.07.14 Dan
;;;             : * Reset and reload check the top-level-lock.  Clear-all DOES NOT.
;;;             :   That's because it can't be a recursive lock and clear-all
;;;             :   gets called during reload, but since clear-all isn't remotely
;;;             :   available that isn't a serious issue at this point.
;;;             : * Protect access to meta-p-component-list.
;;; 2017.08.25 Dan
;;;             : * If reload is provided the optional compile flag and the 
;;;             :   last loaded file was compiled now it will recompile the source
;;;             :   file if it exists and should be.
;;;             : * Reset and reload return t upon success.
;;; 2017.08.28 Dan
;;;             : * Adding a signal for clear-all (clear-all-start). 
;;;             : * Added a reset-start signal with the last update but didn't
;;;             :   note that above.
;;; 2017.12.14 Dan [3.0]
;;;             : * Eliminated the special case of not resetting a single empty
;;;             :   model because that usage should be discouraged and this seems
;;;             :   like a good time to eliminate a 5.0 backwards compatibility
;;;             :   feature!
;;;             : * Why does reload generate errors?  Just print the warnings
;;;             :   now instead.  Should probably do the same for clear-all,
;;;             :   but holding off on that for now.
;;; 2017.12.18 Dan
;;;             : * Fixed a bug with changing the errors to print-warnings in
;;;             :   reload.
;;; 2019.02.05 Dan
;;;             : * Adjustments because meta-p-models is now an alist.
;;; 2019.05.24 Dan
;;;             : * At clear-all make sure all remote modules are still connected.
;;;             :   Would be nice to check at other times, but can't undefine
;;;             :   when there are models so this is really the only safe spot
;;;             :   right now.
;;; 2019.09.09 Dan
;;;             : * Adjusted calls to with-top-level lock to pass the string
;;;             :   that indicates why the lock is being held.
;;; 2021.10.15 Dan [4.0]
;;;             : * Added the call to clear-used-modules during clear-all.
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


(defvar *recorded-load-file* nil)


(defun clear-all ()
  (when (mp-running?)
    (error "Clear-all cannot be used while ACT-R is running.~%If you are loading a model file you must stop the current run first.~%If the Stepper is open you must close it or hit the stop button.~%"))
  
  ;; There is only one mp in this system
  
  (let ((mp (current-mp)))
    
    (dispatch-apply "clear-all-start")
    
    
    (mapcar (lambda (x)
              (delete-model-fct (car x)))
             (bt:with-lock-held ((meta-p-models-lock mp)) (meta-p-models mp)))
    
    
    ;; Make sure all the remote modules are still available
    (verify-remote-modules)

    ;; update the componenets
    
    (dolist (c (bt:with-lock-held ((meta-p-component-lock mp)) (meta-p-component-list mp)))
      (when (act-r-component-clear-all (cdr c))
        (funcall (act-r-component-clear-all (cdr c)) (act-r-component-instance (cdr c)))))
    
    
    ;; This resets the scheduler and real-time management
    
    (reset-mp mp)
    
    (bt:with-lock-held ((meta-p-event-hook-lock mp))
      (setf (meta-p-pre-events mp) nil)
      (setf (meta-p-post-events mp) nil)
      (setf (meta-p-next-hook-id mp) 0)
      
      (clrhash (meta-p-hook-table mp)))
    
    
    (bt:with-lock-held ((meta-p-models-lock mp))
      (setf (meta-p-models mp) nil)
      (setf (meta-p-model-order mp) nil)
      (setf (meta-p-model-count mp) 0)
      (setf (meta-p-model-name-len mp) 0))
    )
  
  (clear-used-modules)
  (setf *recorded-load-file* *load-truename*)
  nil) 

(add-act-r-command "clear-all-start" nil "Signal that a clear-all has begun for monitoring. No params." nil)
(add-act-r-command "reset-start" nil "Signal that a reset has begun for monitoring. No params." nil)

(defun internal-reset ()
  (with-top-level-lock "Reset cannot be used." "reset"
    (let ((mp (current-mp)))
      (dispatch-apply "reset-start")
      
      (reset-mp mp)
      
      (dolist (c (bt:with-lock-held ((meta-p-component-lock mp)) (meta-p-component-list mp)))
        (awhen (act-r-component-before-reset (cdr c))
               (funcall it (act-r-component-instance (cdr c)))))
      
      (dolist (model-name (bt:with-lock-held ((meta-p-models-lock mp)) (meta-p-model-order mp)))
        (awhen (bt:with-lock-held ((meta-p-models-lock mp)) (cdr (assoc model-name (meta-p-models mp))))
               (reset-model mp it)))
      
      (dolist (c (bt:with-lock-held ((meta-p-component-lock mp)) (meta-p-component-list mp)))
        (awhen (act-r-component-after-reset (cdr c))
               (funcall it (act-r-component-instance (cdr c)))))
      t)))


(add-act-r-command "reset" 'internal-reset "Reset the ACT-R scheduler and all models. No params." "Reset already in progress")

(defun reset ()
  (handle-evaluate-results (evaluate-act-r-command "reset")))


(defun internal-reload (&optional (compile nil))
  (if *recorded-load-file*
     (if (probe-file *recorded-load-file*)
         (with-top-level-lock "Reload cannot be used." "reload"
           (let ((no-errors t))
             (cond ((and compile 
                         (string= (pathname-type *recorded-load-file*)
                                  (pathname-type *.lisp-pathname*)))
                    
                    (let* ((save-stream (make-string-output-stream))
                           (display-stream (make-broadcast-stream *standard-output* save-stream))
                           (error-stream (make-broadcast-stream *error-output* save-stream))
                           (*standard-output* display-stream)
                           (*error-output* error-stream))              
                      (handler-case
                          (compile-and-load *recorded-load-file*)
                        (error (x)
                          (print-warning "Error ~/print-error-message/ while trying to reload file ~s" x *recorded-load-file*)
                          (setf no-errors nil)))
                      (let ((s (get-output-stream-string save-stream)))
                        (unless (zerop (length s))
                          (print-warning "Non-ACT-R messages during load of ~s:~%~a~%" *recorded-load-file* s)))))
                   ((and compile 
                         (string= (pathname-type *recorded-load-file*)
                                  (pathname-type *.fasl-pathname*))
                         (probe-file (merge-pathnames *.lisp-pathname* *recorded-load-file*)))
                    
                    (print-warning "Since the last loaded file was compiled and the source file exists, recompiling the source file if necessary.")
                    
                    (let* ((save-stream (make-string-output-stream))
                           (display-stream (make-broadcast-stream *standard-output* save-stream))
                           (error-stream (make-broadcast-stream *error-output* save-stream))
                           (*standard-output* display-stream)
                           (*error-output* error-stream))              
                      (handler-case
                          (compile-and-load (merge-pathnames *.lisp-pathname* *recorded-load-file*))
                        (error (x)
                          (print-warning "Error ~/print-error-message/ while trying to reload file ~s" x *recorded-load-file*)
                          (setf no-errors nil)))
                      (let ((s (get-output-stream-string save-stream)))
                        (unless (zerop (length s))
                          (print-warning "Non-ACT-R messages during load of ~s:~%~a~%" *recorded-load-file* s)))))
                   (t
                    (let* ((save-stream (make-string-output-stream))
                           (display-stream (make-broadcast-stream *standard-output* save-stream))
                           (error-stream (make-broadcast-stream *error-output* save-stream))
                           (*standard-output* display-stream)
                           (*error-output* error-stream))              
                      (handler-case
                          (load *recorded-load-file*)
                        (error (x)
                          (print-warning "Error ~/print-error-message/ while trying to reload file ~s" x *recorded-load-file*)
                          (setf no-errors nil)))
                      (let ((s (get-output-stream-string save-stream)))
                        (unless (zerop (length s))
                          (print-warning "Non-ACT-R messages during load of ~s:~%~a~%" *recorded-load-file* s))))))
             no-errors))
       (print-warning "File ~s does not exist." *recorded-load-file*))
    (progn
      (print-warning "No model file recorded to reload.")
      :none)))



(add-act-r-command "reload" 'internal-reload "Reload the last ACT-R model file which was loaded. Params: {compile}." "Reload already in progress")

(defun reload (&optional (compile nil))
  (handle-evaluate-results (evaluate-act-r-command "reload" compile)))


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
