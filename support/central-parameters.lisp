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
;;; Filename    : central-parameters.lisp
;;; Version     : 2.0
;;; 
;;; Description : A module to hold parameters that could be used by more than
;;;               one module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;;
;;; 2004.10.17 Dan
;;;             : Creation.
;;; 2005.01.09 Dan
;;;             : Moved the provide to the end.
;;; 2006.01.17 Dan
;;;             : * Updated the version to 1.0 and modified the description.
;;; 2008.07.23 Dan
;;;             : * Adding the register-subsymbolic-parameters command and the 
;;;             :   necessary changes to the module to record and verify that.
;;; 2008.07.24 [1.1]
;;;             : * Updated the version number because of the change.
;;; 2013.07.16 Dan [1.2]
;;;             : * Adding a system parameter :custom-defaults and changing the
;;;             :   reset to secondary.  Setting :custom-defaults to a list of 
;;;             :   parameter-value pairs as acceptable for sgp causes those 
;;;             :   to be set at the start of every model.
;;; 2013.08.06 Dan
;;;             : * Renaming custom-defaults to starting-parameters since it 
;;;             :   doesn't actually change the default values and this name
;;;             :   feels like a better description.
;;; 2014.09.03 Dan
;;;             : * When testing the :starting-parameters param names need to
;;;             :   make sure there is a current meta-process.
;;; 2015.06.05 Dan
;;;             : * Use schedule-event-now for initial check.
;;; 2016.09.28 Dan
;;;             : * Removed a with-meta-process from test-user-defaults-parameter.
;;; 2017.08.07 Dan
;;;             : * Add a lock for the *subsymbolic-parameter-values* variable.
;;; 2017.08.15 Dan
;;;             : * Add a lock for the central-parameters parameters.
;;; 2018.04.10 Dan
;;;             : * Add a remote version of register-subsymbolic-parameters.
;;; 2018.06.22 Dan
;;;             : * Fixed a bug with the remote register-subsymbolic-parameters.
;;; 2019.02.15 Dan
;;;             : * If :starting-parameters is set to nil don't try to create
;;;             :   a model to test the values since there aren't any to check.
;;; 2020.01.10 Dan [1.3]
;;;             : * Removed the #' and lambdas from the module interface functions
;;;             :   since that's not allowed in the general system now.
;;; 2021.10.19 Dan [2.0]
;;;             : * Set the :required flag on the module.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Creates a module to hold some of the critical parameters that may
;;; be used by multiple modules:
;;;
;;; :esc Enable Subsymbolic Computation
;;; :ol  Optimized Learning
;;; :er  Enable Randomness
;;;
;;; Also sets the model parameters to the values provided in the :starting-parameters
;;; system parameter for every model during the secondary reset.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; The three parameters :esc, :ol, and :er and also the system parameter :starting-parameters.
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

(defvar *subsymbolic-parameter-values* nil)
(defvar *spv-lock* (bt:make-lock "subsymbolic-parameter-values"))


(defvar *user-default-parameter-list* nil)


(defun test-user-defaults-parameter (params)
  (and (listp params)
       (evenp (length params))
       (or (null params)
           (let ((names (let ((i -1)) (mapcan (lambda (x) (incf i) (when (evenp i) (list x))) params))))
             (and
              (every 'keywordp names)
              (if (current-model)
                  (notany (lambda (y)
                            (eq :bad-parameter-name (car (no-output (sgp-fct (list y))))))
                          names)
                (let ((name (define-model-fct (gensym) nil))
                      (res nil))
                  (with-model-eval name
                    (setf res (notany (lambda (y)
                                        (eq :bad-parameter-name (car (no-output (sgp-fct (list y))))))
                                      names)))
                  (delete-model-fct name)
                  res)))))))

(create-system-parameter :starting-parameters :handler (simple-system-param-handler *user-default-parameter-list*)
                         :documentation "Parameter settings to apply at the start of every model."
                         :valid-test 'test-user-defaults-parameter
                         :warning "A list that is valid for passing to sgp-fct"
                         :default-value nil)

(defstruct central-parameters
  esc ol er (lock (bt:make-lock "central-parameters")))

(defun central-parameters-reset (instance)
  (schedule-event-now 'check-for-esc-nil :maintenance t :output nil 
                           :priority :max :params (list instance))
  (when *user-default-parameter-list*
    (sgp-fct *user-default-parameter-list*)))

(defun check-for-esc-nil (instance)
  (when (and (null (bt:with-lock-held ((central-parameters-lock instance)) (central-parameters-esc instance)))
             (some (lambda (param)
                     (let ((current (car (no-output (sgp-fct (list param))))))
                       (and (not (eq current :BAD-PARAMETER-NAME))
                            (not (equalp current (get-parameter-default-value param))))))
                   (bt:with-lock-held (*spv-lock*) *subsymbolic-parameter-values*)))
    (model-warning "Subsymbolic parameters have been set but :esc is currently nil.")))

(defun register-subsymbolic-parameters (&rest params)
  (bt:with-lock-held (*spv-lock*)
    (dolist (param params)
      (when (and (valid-parameter-name param) 
                 (not (find param *subsymbolic-parameter-values*)))
        (push param *subsymbolic-parameter-values*)))))


(defun remote-register-subsymbolic-parameters (&rest params)
  (apply 'register-subsymbolic-parameters (string->name-recursive params)))

(add-act-r-command "register-subsymbolic-parameters" 'remote-register-subsymbolic-parameters "Specify parameters which require :esc to be t so that the system can warn if it is nil when they are used. Params: param-name*")


(defun create-central-params (model-name)
  (declare (ignore model-name))
  (make-central-parameters))

(defun central-parameters-params (instance param)
  (bt:with-lock-held ((central-parameters-lock instance))
    (cond ((consp param)
           (case (car param)
             (:esc (setf (central-parameters-esc instance) (cdr param)))
             (:ol (setf (central-parameters-ol instance) (cdr param)))
             (:er (setf (central-parameters-er instance) (cdr param)))))
          (t
           (case param
             (:esc (central-parameters-esc instance))
             (:ol (central-parameters-ol instance))
             (:er (central-parameters-er instance)))))))

(define-module-fct 'central-parameters nil
  (list
   (define-parameter :esc :owner t :valid-test 'tornil :default-value nil
     :warning "either t or nil" :documentation "Enable Subsymbolic Computations")
   (define-parameter :er :owner t :valid-test 'tornil :default-value nil
     :warning "either t or nil" :documentation "Enable Randomness")
   (define-parameter :ol :owner t :valid-test 'posnumorbool 
     :default-value t :warning "either t, nil, or a positive number"
     :documentation "Optimized Learning"))
  :version "2.0"
  :documentation "a module that maintains parameters used by other modules"
  :creation 'create-central-params
  :params 'central-parameters-params
  :reset (list nil 'central-parameters-reset)
  :required t)


(provide "CENTRAL-PARAMETERS")

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
