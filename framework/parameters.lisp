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
;;; Filename    : parameters.lisp
;;; Version     : 1.0
;;; 
;;; Description : Functions for defining and accessing parameters.
;;; 
;;; Bugs        : 
;;;
;;; To do       : Finish the documentation.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.18 Dan
;;;             : Creation.
;;;
;;; 2005.01.04 Dan
;;;             : Took some of the newlines out of show-all-parameters.
;;;             : Shrunk down to max width of 80 chars.
;;; 2005.01.17 Dan
;;;             : * Changed model-output to command-output so that one can
;;;             :   see sgp printing when :v is nil.
;;;             : * Changed the order of sgp's printing so that it's easier
;;;             :   to see the parameter values.
;;; 2005.02.11 Dan
;;;             : * Replaced a reverse with push-last in set-parameters.
;;; 2005.08.10 Dan
;;;             : * Added the remove-modules-parameters and remove-parameter
;;;             :   to support the undefine-module function.
;;;             : * Updated the version to 1.0.
;;; 2006.07.12 Dan
;;;             : * Modified TEST-AND-SET-PARAMETER-VALUE so that when a value
;;;             :   failed the validity test :invalid-value is returned for the
;;;             :   list of parameter values (could return the current setting
;;;             :   instead, but I think an error marker works better).
;;;             : * Also did some reformatting because I find it difficult to
;;;             :   read some of the code if I keep it to only 80 columns.
;;; 2008.07.23 Dan
;;;             : * Added a get-parameter-default-value function so that that
;;;             :   value can be retrieved through a function in the API.
;;; 2008.08.29 Dan
;;;             : * Updated define-parameter to check the valid-test parameter
;;;             :   for validity like the rest.
;;; 2012.03.20 Dan
;;;             : * Added the with-parameters and with-parameters-fct commands
;;;             :   which can be used to temporarily set parameter values while
;;;             :   evaluating some code.
;;; 2012.08.06 Dan
;;;             : * Sgp now prints the paramter table sorted by module name
;;;             :   and then by parameter name within a module when it prints
;;;             :   all parameters.
;;; 2017.03.08 Dan
;;;             : * Because (no-output (car (sgp :xxx))) is a frequently needed
;;;             :   construct adding a function to do just that and making it
;;;             :   available remotely: get-parameter-value.
;;; 2017.06.29 Dan
;;;             : * Protecting the parameters table with a lock.
;;; 2017.07.14 Dan
;;;             : * Make sure sgp is thread safe.
;;; 2017.11.07 Dan
;;;             : * Changed the warning in test-and-set-parameter-value to use
;;;             :   ~s instead of ~a when printing an invalid value.
;;; 2017.11.14 Dan
;;;             : * Added the external command set-parameter-value.
;;; 2018.01.26 Dan
;;;             : * Renamed the things so that get-paraemter-value and set-
;;;             :   parameter-value are valid user commands associated with the
;;;             :   corresponding remote commands and don't error out for get.
;;; 2018.06.14 Dan
;;;             : * Set- and get- parameter-value need to encode/decode strings
;;;             :   for remote calls only.
;;; 2020.09.04 Dan
;;;             : * Get-parameters simplified to avoid the need to get the 
;;;             :   struct from the table twice (once to check valid and once to
;;;             :   actually get the struct).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Internal structures not for external use.
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


;;; Holds all the parameters that have been "registered" with a module.

(defvar *act-r-parameters-table* (make-hash-table :test #'eq)
  "The table of all used parameters")

(defvar *parameters-table-lock* (bt:make-lock "parameters-table"))


(defun define-parameter (param-name 
                         &key (owner t) (valid-test nil)
                         (default-value nil) (warning "")
                         (documentation ""))
  (cond ((not (keywordp param-name))
         (print-warning "Parameter name must be a keyword."))
        ((keywordp default-value)
         (print-warning "default-value cannot be a keyword."))
        ((not (fctornil valid-test))
         (print-warning "valid-test must be a function, the name of a function, or nil."))
        ((not (stringp documentation))
         (print-warning "documentation must be a string."))
        ((not (stringp warning))
         (print-warning "warning must be a string."))
        (t
         (make-act-r-parameter :owner owner
                               :param-name param-name
                               :default default-value
                               :test valid-test
                               :warning warning
                               :details documentation))))



(defun parse-parameters (parameters-list)
  "Make sure that they are parameters and not already owned if ownership 
   requested or that it exists if not owned"
  (if (every (lambda (x)
               (and (act-r-parameter-p x)
                    (or (and (act-r-parameter-owner x) 
                             (not (valid-parameter-name 
                                   (act-r-parameter-param-name x))))
                        (and (not (act-r-parameter-owner x)) 
                             (valid-parameter-name 
                              (act-r-parameter-param-name x))))))
             parameters-list)
      parameters-list
    :error))
                                 

(defun install-parameters (module-name parameters)
  (bt:with-lock-held (*parameters-table-lock*)
    (dolist (x parameters)
      (if (act-r-parameter-owner x)
          (let ((param-copy (copy-act-r-parameter x)))
            (setf (act-r-parameter-owner param-copy) module-name)
            (setf (gethash (act-r-parameter-param-name param-copy) 
                           *act-r-parameters-table*)
              param-copy))
        (push module-name (act-r-parameter-users 
                           (gethash (act-r-parameter-param-name x) *act-r-parameters-table*)))))))  


(defun remove-modules-parameters (module-name)
  "Remove all parameters of the module both owned and watched"
  
  (bt:with-lock-held (*parameters-table-lock*)
    (maphash (lambda (name param)
               (when (eq module-name
                         (act-r-parameter-owner param))
                 (remhash name *act-r-parameters-table*))
               (setf (act-r-parameter-users param)
                 (remove module-name (act-r-parameter-users param))))
             *act-r-parameters-table*)))


(defun remove-parameter (param-name)
  "Remove a specific parameter from the table"
  (bt:with-lock-held (*parameters-table-lock*)
    (remhash param-name *act-r-parameters-table*)))
 
(defmacro sgp (&rest parameters)
  `(sgp-fct ',parameters))

(defun sgp-fct (&optional (parameters-list nil))
  (verify-current-mp  
   "sgp called with no current meta-process."
   (verify-current-model
    "sgp called with no current model."
    (set-or-get-parameters parameters-list))))
    

(defun set-or-get-parameters (params)
  (if (null params)
      (show-all-parameters)
    (if (every 'keywordp params)
        (get-parameters params)
      (set-parameters params))))


(defun internal-get-parameter-value (parameter)
  (first (get-parameters (list parameter) nil)))

(defun get-parameter-value (parameter)
  (let ((m (current-model)))
    (cond ((null m)
           (print-warning "get-parameter-value called with no current model.")
           :no-model)
          (t
           (let ((p (internal-get-parameter-value parameter)))
             (when (eq p :bad-parameter-name)
               (print-warning "Invalid parameter name ~s in call to get-parameter-value." parameter))
             p)))))

(defun remote-get-parameter-value (parameter)
  (encode-string-names (get-parameter-value (string->name parameter))))

(add-act-r-command "get-parameter-value" 'remote-get-parameter-value "Return the current value of a parameter in the current model. Params: parameter-name." nil)


(defun get-parameters (params &optional (output t))
  (let ((res nil))
    (dolist (p-name params (reverse res))
      (aif (get-parameter-struct p-name)
          (let* ((owner (act-r-parameter-owner it))
                 (val (process-parameters owner p-name)))
            (push val res)
            (when output
              (command-output "~S ~S (default ~S) : ~A"
                              p-name
                              val
                              (act-r-parameter-default it)
                              (act-r-parameter-details it))))
        (push :bad-parameter-name res)))))


(defun get-parameter-struct (p-name)
  (bt:with-lock-held (*parameters-table-lock*)
    (gethash p-name *act-r-parameters-table*)))

(defun valid-parameter-name (p-name)
  (bt:with-lock-held (*parameters-table-lock*)
    (gethash p-name *act-r-parameters-table*)))


(defun set-parameters (params)
  (if (evenp (length params))
      (let ((res nil))
        (while params
          (let ((p-name (pop params))
                (p-val (pop params)))
            (push-last (test-and-set-parameter-value p-name p-val) res)))
        res)
    (print-warning "Odd number of parameters and values passed to sgp.")))


(defun test-and-set-parameter-value (p-name value)
  (let ((param (bt:with-lock-held (*parameters-table-lock*) (gethash p-name *act-r-parameters-table*))))
    (if param
        (if (or (null (act-r-parameter-test param))
                (funcall (act-r-parameter-test param) value))
            (internal-set-parameter-value param value)
          (progn
            (print-warning "Parameter ~S cannot take value ~s because it must be ~A."
                           p-name value (act-r-parameter-warning param))
            :invalid-value))
      (progn
        (print-warning "Parameter ~s is not the name of an available parameter" p-name)
        :bad-parameter-name))))

(defun internal-set-parameter-value (param value)
  (let* ((current-value (process-parameters (act-r-parameter-owner param)
                                            (cons (act-r-parameter-param-name param) value))))
    
    (dolist (s (bt:with-lock-held (*parameters-table-lock*) (act-r-parameter-users param)) current-value)
      
      (process-parameters s (cons (act-r-parameter-param-name param) current-value)))))

(defun set-parameter-value (param value)
  (let ((m (current-model)))
    (cond ((null m)
           (print-warning "set-parameter-value called with no current model.")
           :no-model)
          (t
           (test-and-set-parameter-value param value)))))

(defun remote-set-parameter-value (param val)
  (encode-string-names (set-parameter-value (string->name param) (decode-string-names val))))

(add-act-r-command "set-parameter-value" 'remote-set-parameter-value "Sets the current value of a parameter in the current model. Params: parameter-name 'new-value'." nil)

(defmacro with-parameters (temp-params &body body)
  (let ((params (gensym))
        (current-vals (gensym))
        (new-vals (gensym))
        (p-name (gensym))
        (p-val (gensym))
        (val (gensym)))
    
    `(let ((,params ',temp-params))
       (block with-parameters
         (verify-current-mp  
          "with-parameters called with no current meta-process."
          (verify-current-model
           "with-parameters called with no current model."
           (if (oddp (length ,params))
               (print-warning "Odd length parameters list in call to with-parameters. The body is ignored.")
             
             (let ((,current-vals nil)
                   (,new-vals nil))
               
               (while ,params
                 (let ((,p-name (pop ,params))
                       (,p-val (pop ,params)))
                   (unless (valid-parameter-name ,p-name)
                     (print-warning "~s is not the name of a parameter. with-parameters body ignored." ,p-name)
                     (return-from with-parameters nil))
                   (push (cons ,p-name (car (get-parameters (list ,p-name) nil))) ,current-vals)
                   (push-last (cons ,p-name ,p-val) ,new-vals)))
               
               (unwind-protect 
                   (progn 
                     (dolist (,val ,new-vals)
                       (test-and-set-parameter-value (car ,val) (cdr ,val)))
                     ,@body)
                 (dolist (,val ,current-vals)
                       (test-and-set-parameter-value (car ,val) (cdr ,val))))))))))))

(defmacro with-parameters-fct (temp-params &body body)
  (let ((params (gensym))
        (current-vals (gensym))
        (new-vals (gensym))
        (p-name (gensym))
        (p-val (gensym))
        (val (gensym)))
    
    `(let ((,params (list ,@temp-params)))
       (block with-parameters
         (verify-current-mp  
          "with-parameters called with no current meta-process."
          (verify-current-model
           "with-parameters called with no current model."
           (if (oddp (length ,params))
               (print-warning "Odd length parameters list in call to with-parameters. The body is ignored.")
             
             (let ((,current-vals nil)
                   (,new-vals nil))
               
               (while ,params
                 (let ((,p-name (pop ,params))
                       (,p-val (pop ,params)))
                   (unless (valid-parameter-name ,p-name)
                     (print-warning "~s is not the name of a parameter. with-parameters body ignored." ,p-name)
                     (return-from with-parameters nil))
                   (push (cons ,p-name (car (get-parameters (list ,p-name) nil))) ,current-vals)
                   (push-last (cons ,p-name ,p-val) ,new-vals)))
               
               (unwind-protect 
                   (progn 
                     (dolist (,val ,new-vals)
                       (test-and-set-parameter-value (car ,val) (cdr ,val)))
                     ,@body)
                 (dolist (,val ,current-vals)
                       (test-and-set-parameter-value (car ,val) (cdr ,val))))))))))))

(defun show-all-parameters ()
  (let ((current-val-table (make-hash-table)))
    (maphash (lambda (p-name param)
               (push 
                (cons param (process-parameters (act-r-parameter-owner param) p-name)) 
                (gethash (act-r-parameter-owner param) current-val-table)))
             (bt:with-lock-held (*parameters-table-lock*) *act-r-parameters-table*))
    (let ((name-len (1+ (apply #'max 
                               (mapcar (lambda (x) 
                                         (length (string x)))
                                 (hash-table-keys (bt:with-lock-held (*parameters-table-lock*) *act-r-parameters-table*))))))
          (default-len (apply #'max 
                              (with-hash-table-iterator (generator-fn (bt:with-lock-held (*parameters-table-lock*) *act-r-parameters-table*))
                                (let ((items nil))
                                  (loop     
                                    (multiple-value-bind (more? key value) (generator-fn)
                                      (declare (ignore key))
                                      (unless more? (return items))
                                      (push (length (format nil "~s" (act-r-parameter-default value)))
                                            items)))))))
                                          
          (value-len (apply #'max 
                            (with-hash-table-iterator (generator-fn current-val-table)
                              (let ((items nil))
                                (loop     
                                  (multiple-value-bind (more? key value) (generator-fn)
                                    (declare (ignore key))
                                    (unless more? (return items))
                                    (dolist (param value)
                                      (push (length (format nil "~S" (cdr param))) 
                                            items)))))))))
      
      
      (dolist (module-name (sort (hash-table-keys current-val-table) #'string< :key 'symbol-name))
        
        (command-output "--------------------------------~%~S module" module-name)
        (command-output "--------------------------------")
        (dolist (param (sort (gethash module-name current-val-table)  #'string< :key (lambda (x) (symbol-name (act-r-parameter-param-name (car x))))))
                     
          (command-output "~vS ~vS default: ~vS : ~A"
                          name-len
                          (act-r-parameter-param-name (car param))
                          value-len
                          (cdr param)
                          default-len
                          (act-r-parameter-default (car param))
                          (act-r-parameter-details (car param))))))))
      
  
(defun get-parameter-default-value (param)
  (aif (get-parameter-struct param)
       (act-r-parameter-default it)
       (progn
         (print-warning "Invalid parameter name ~S in call to get-parameter-default-value." param)
         :bad-parameter-name)))

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
