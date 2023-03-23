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
;;; Filename    : naming-module.lisp
;;; Version     : 3.0
;;; 
;;; Description : Module that supports model-specific and "safe" gentemping for
;;;               name creation.
;;; 
;;; Bugs        : 
;;;
;;; To do       : Documentation.
;;; 
;;; ----- History -----
;;;
;;; 2004.09.17 Dan
;;;             : Creation
;;; 2005.02.22 Dan
;;;             : * Changed reset-naming-module to work around an issue in 
;;;             :   Lispworks 4.4.
;;;             : * Fixed a bug in new-name-fct - it wasn't catching the
;;;             :   existing symbol info correctly.
;;;             : * Updated the version number.
;;; 2006.01.17 Dan
;;;             : * Updated the version to 1.0 since there haven't been any
;;;             :   problems it's time to drop the "a".
;;; 2006.08.10 Dan
;;;             : * Modified new-symbol so that it only returns one value 
;;;             :   instead of two from intern.
;;; 2006.10.10 Dan
;;;             : * Changed the representation of what's stored in the *global-
;;;             :   names-table* so that I can add the release-name command.
;;;             :   That way it's possible for a model to clear out all of the
;;;             :   temp chunk names that were used for the buffer chunks which
;;;             :   got merged away if the symbol table size becomes an issue.
;;;             :   (There's another command called normalize-chunk-names in
;;;             :   the chunks file that uses release-name because it should
;;;             :   be rare for a user to call release-name directly.)
;;; 2006.10.23 Dan
;;;             : * Complete overhaul of how the symbol creation/deletion is
;;;             :   managed.  Should be better overall, but performance testing
;;;             :   is still underway - this might need some more tweaks...
;;; 2007.04.16 Dan
;;;             : * Changed the version number to 1.1.
;;;             : * Added the :ncnar parameter to the module.  If it is set to
;;;             :   t then normalize-chunk-names will be called at the end of
;;;             :   every run.
;;; 2007.07.03 Dan
;;;             : * Changed the :ncnar default value to t.
;;;             :   That means that like :v one should always disable :ncnar
;;;             :   when performing long runs or when debugging is not necessary.
;;; 2007.07.13 Dan
;;;             : * Added a new value for :ncnar - delete.  When it's set to
;;;             :   delete it not only normalizes the names but uninterns the
;;;             :   and deletes the other chunks (it passes t to normalize-chunk
;;;             :   names).  NOT a recommended setting, but does provide a 
;;;             :   direct way to handle the issue raised by Bill Kennedy and
;;;             :   Greg Trafton about long term memory usage of the system.
;;;             :   Setting it to delete for a model that's run incrementally
;;;             :   (multiple calls to run) will almost certainly cause errors
;;;             :   with the vision module if there's a device installed.
;;; 2007.07.24 Dan
;;;             : * The quote in front of the lambda in the valid-test of the
;;;             :   :ncnar parameter causes problems for some Lisps (it shouldn't
;;;             :   really be there and it's just that some Lisps are more
;;;             :   forgiving).
;;; 2007.11.21 Dan
;;;             : * Fixed a bug with new-symbol-fct that was introduced when
;;;             : the :ncnar parameter got added...
;;; 2008.04.17 Dan
;;;             : * Performance improvement for the new-name and new-symbol 
;;;             :   functions (at least in ACL, LispWorks, and MCL others 
;;;             :   untested at this point).
;;;             :   Instead of using princ-to-string created a custom function
;;;             :   for converting numbers into strings which takes about half
;;;             :   the time in the tested systems which adds up because of how
;;;             :   often new-name gets used (creating copies of chunks being
;;;             :   a big part of model running).
;;; 2008.10.23 Dan [1.3]
;;;             : * Added the new parameter :dcnn and modified how :ncnar sets
;;;             :   the flags for the model since they're in the model struct
;;;             :   now.
;;;             : * Added the functions to check the normalizing conditions
;;;             :   which bypass getting at the specific parameter values:
;;;             :   update-chunks-at-all, update-chunks-on-the-fly and
;;;             :   delete-chunks-after-run.
;;; 2008.11.13 Dan
;;;             : * Added the :dcsc-hook parameter to provide a hook for functions
;;;             :   to be called when chunk name normalizing modifies chunks.
;;; 2009.02.13 Dan
;;;             : * Added the :short-copy-names parameter to determine how the
;;;             :   chunk-name for a copied chunk gets created.
;;; 2009.03.24 Dan
;;;             : * Changed update-chunks-on-the-fly to actually check update-
;;;             :   chunks-at-all as well since :dcnn is supposed to be turned
;;;             :   off with :ncnar but wasn't completely off unless it was also
;;;             :   explicitly disabled.
;;; 2012.06.27 Dan
;;;             : * Fixed a bug in new-symbol because it would throw an error
;;;             :   if there isn't a current model instead of printing the warning.
;;; 2017.05.31 Dan [2.0]
;;;             : * Also adding a naming component which can generate symbols
;;;             :   outside of any model and move new-symbol to use the component
;;;             :   instead of the module.
;;; 2017.06.16 Dan
;;;             : * Add locks for the global tables and actually protect the
;;;             :   symbols table (the name table is not protected yet).
;;; 2017.06.21 Dan
;;;             : * Protect everything else with locks (parameters, global 
;;;             :   symbol table, local table, and model level 'updating' slots).
;;; 2017.06.23 Dan
;;;             : * Added names for all the locks.
;;; 2018.02.27 Dan
;;;             : * Allow the :dcsc-hook function to accept command strings.
;;;             : * Provide remote versions of new-name, release-name, and new-symbol.
;;; 2020.01.10 Dan [2.1]
;;;             : * Removed the #' and lambdas from the module interface functions
;;;             :   since that's not allowed in the general system now.
;;;             : * Also allow hook function parameters to remove items with 
;;;             :   a setting of (:remove <item>).
;;; 2020.08.25 Dan
;;;             : * Replaced constants that had *...* with +...+ to avoid the
;;;             :   warnings in SBCL.
;;;             : * Switch from defconstant to the define-constant macro to 
;;;             :   avoid issues with SBCL (instead of redefining defconstant
;;;             :   for SBCL as was done previously).
;;; 2021.10.19 Dan [3.0]
;;;             : * Set the :required flag on the module.
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

(defvar *global-names-table* (make-hash-table :test #'equal))

(defvar *global-symbols-table* (make-hash-table :test #'equal))

(defvar *symbols-table-lock* (bt:make-lock "symbols-table"))
(defvar *names-table-lock* (bt:make-lock "names-table"))

(define-constant +naming-module-num-vector+ (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun fast-num->string (n)
  (if (< n 4) ;; catch the common/easy cases fast
      (if (< n 2)
          (if (zerop n) "0" "1")
        (if (= n 2) "2" "3"))
    (let (rem 
          (s nil))
      (while (> n 0)
        (multiple-value-setq (n rem) (floor n 10))
        (push rem s))
      (let ((s2 (make-string (length s))))
        (dotimes (i (length s))
          (setf (aref s2 i)
            (aref +naming-module-num-vector+ (pop s))))
        s2))))


(defstruct act-r-name (next-name 0) used-symbols)

(defstruct naming-module model ncnar dcnn hook (table (make-hash-table :test #'equal)) (table-lock (bt:make-lock "naming-table")) (param-lock (bt:make-lock "naming-param")))

(defun create-naming-module (model-name)
  (make-naming-module :model model-name))


(defun reset-naming-module (name-module)
  (bt:with-lock-held ((naming-module-table-lock name-module))
    (bt:with-lock-held (*names-table-lock*)
      (let ((m-name (naming-module-model name-module))
            (table (naming-module-table name-module)))
        (maphash (lambda (base val) 
                     (dotimes (i (act-r-name-next-name val))
                       
                       (let* ((symbol-name (concatenate 'string base (fast-num->string i)))
                              (val (gethash symbol-name *global-names-table*)))
                         
                         (when (and val
                                    (not (eq val t))
                                    (find m-name val))
                           (setf val (remove m-name val))
                           (if (null val)
                               (let ((symbol (intern symbol-name)))
                                 (unintern symbol)
                                 (remhash symbol-name *global-names-table*))
                             (setf (gethash symbol-name *global-names-table*) val)))))
                     
                     (remhash base table))
                 table)))))

(defun reset-naming-component (name-module)
  (bt:with-lock-held ((naming-module-table-lock name-module))
    (let ((table (naming-module-table name-module)))
      (maphash (lambda (base val) 
                   (dolist (x (act-r-name-used-symbols val))
                     (let ((symbol (intern (concatenate 'string base "-" (fast-num->string x)))))
                       (unintern symbol)))
                   (remhash base table))
               table))))


(defun params-naming-module (module param)
  (bt:with-lock-held ((naming-module-param-lock module))
    (if (consp param)
        (case (car param)
          (:ncnar
           (let ((model (current-model-struct)))
             (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
               (cond ((null (cdr param))
                      (setf (act-r-model-chunk-update model) nil)
                      (setf (act-r-model-delete-chunks model) nil))
                     ((eq (cdr param) 'delete)
                      (setf (act-r-model-chunk-update model) t)
                      (setf (act-r-model-delete-chunks model) t))
                     (t  
                      (setf (act-r-model-chunk-update model) t)
                      (setf (act-r-model-delete-chunks model) nil)))))
           
           (setf (naming-module-ncnar module) (cdr param)))
          
          (:dcnn (let ((model (current-model-struct)))
                   (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
                     
                     (setf (act-r-model-dynamic-update model) (cdr param))))
                 (setf (naming-module-dcnn module) (cdr param)))
          
          (:dcsc-hook 
           (if (cdr param)
               (let ((l (length (naming-module-hook module)))
                     (v (setf (naming-module-hook module) 
                          (set-or-remove-hook-parameter :dcsc-hook (naming-module-hook module) (cdr param)))))
                 (cond ((< l (length v)) ;; added one
                        (let ((model (current-model-struct)))
                          (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
                            (push (cdr param) (act-r-model-dynamic-update-hooks model)))))
                       
                       ((> l (length v)) ;; removed one
                        (let ((model (current-model-struct)))
                          (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
                            (setf (act-r-model-dynamic-update-hooks model) 
                              (remove (cddr param) (act-r-model-dynamic-update-hooks model)))))))
                 v)
             (progn
               (setf (naming-module-hook module) nil)
               (let ((model (current-model-struct)))
                 (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
                   (setf (act-r-model-dynamic-update-hooks model) nil))))))
          
          (:short-copy-names 
           (let ((model (current-model-struct)))
             (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
               (setf (act-r-model-short-copy-names model) (cdr param))))))
      (case param
        (:ncnar (naming-module-ncnar module))
        (:dcnn (naming-module-dcnn module))
        (:dcsc-hook (naming-module-hook module))
        (:short-copy-names 
         (let ((model (current-model-struct)))
           (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
             (act-r-model-short-copy-names model))))))))

(defun ncnar-value-test (x)
  (or (tornil x) (eq x 'delete)))

(define-module-fct 'naming-module nil 
  (list 
   (define-parameter :ncnar
       :documentation "Normalize chunk names after run"
     :default-value t
     :warning "must be t, nil, or delete"
     :valid-test 'ncnar-value-test)
   (define-parameter :dcnn
       :documentation "Dynamic chunk name normalizing"
     :default-value t
     :warning "must be t or nil"
     :valid-test 'tornil)
   (define-parameter :dcsc-hook :valid-test 'local-or-remote-function-or-remove 
     :default-value nil
     :warning "a function, string naming a command, nil, or (:remove <item>)" 
     :documentation "Hook called when a chunk is changed due to normalizing")
   (define-parameter :short-copy-names :valid-test 'tornil
     :default-value nil
     :warning "T or nil" 
     :documentation "Flag to signal how copied chunks are named"))
  
  :version "3.0"
  :documentation "Provides safe and repeatable new name generation for models."
  :params 'params-naming-module
  :creation 'create-naming-module
  :reset 'reset-naming-module
  :delete 'reset-naming-module
  :required t)



(define-component naming :version "2.0" :documentation "Provides new symbol generation for the system."
  :creation make-naming-module
  :delete reset-naming-component
  :clear-all reset-naming-component)


(defmacro new-name (&optional (prefix "CHUNK"))
  `(new-name-fct ',prefix))

(defun new-name-fct (&optional (prefix "CHUNK"))
  (if (or (stringp prefix) (symbolp prefix))
      (let ((name-module (get-module naming-module)))
        (if name-module
            (bt:with-lock-held ((naming-module-table-lock name-module))
              (let* ((name-table (naming-module-table name-module))
                     (m-name (naming-module-model name-module))
                     (base (string-upcase prefix))
                     (element (gethash base name-table)))
                (unless element
                  (setf element
                    (setf (gethash base name-table) (make-act-r-name))))
                
                (multiple-value-bind (symbol-name symbol previous)
                    (do* ((count (act-r-name-next-name element) (1+ count))
                          (symbol-name (concatenate 'string base (fast-num->string count))
                                       (concatenate 'string base (fast-num->string count)))
                          (existed (find-symbol symbol-name)
                                   (find-symbol symbol-name))
                          (symbol (intern symbol-name)
                                  (intern symbol-name)))
                         ((not (get-chunk symbol))
                          (progn
                            (setf (act-r-name-next-name element) (1+ count)))
                          (values symbol-name symbol existed)))
                  
                  (bt:with-lock-held (*names-table-lock*)
                    (multiple-value-bind (val exists)
                        (gethash symbol-name *global-names-table*)
                      (if exists
                          (unless (or (eq val t)
                                      (find m-name val))
                            (setf (gethash symbol-name *global-names-table*)
                              (push m-name val)))
                        (if previous
                            (setf (gethash symbol-name *global-names-table*) t)
                          (setf (gethash symbol-name *global-names-table*) (list m-name))))))
                  
                  symbol)))
          (print-warning "No naming module available cannot create new name.")))
    (print-warning "Invalid parameter passed to new-name.  Must be a string or symbol.")))

(add-act-r-command "new-name" 'new-name-fct "Create new item names within a model. Params: {name-prefix}")


(defmacro release-name (symbol)
  `(release-name-fct ',symbol))

(defun release-name-fct (symbol)
  (bt:with-lock-held (*names-table-lock*)
    (multiple-value-bind (val exists) (gethash (symbol-name symbol) *global-names-table*)
      (when (and exists 
                 (not (eq t val)))
        (let ((name-module (get-module naming-module)))
          (if name-module
              (let ((m-name (naming-module-model name-module))) 
                
                (when (find m-name val)
                  (setf val (remove m-name val))
                  
                  (if (null val)
                      (progn
                        (remhash (symbol-name symbol) *global-names-table*)
                        (unintern symbol)
                        t)
                    (progn
                      (setf (gethash (symbol-name symbol) *global-names-table*) val)
                      nil))))
            (print-warning "No naming module available cannot release name ~s." symbol)))))))


(defun release-name-external (name-string)
  (release-name-fct (string->name name-string)))

(add-act-r-command "release-name" 'release-name-external "Unintern a name generated by new-name. Params: {name-string}")

(defmacro new-symbol (&optional (prefix "CHUNK"))
  `(new-symbol-fct ',prefix))

(defun new-symbol-fct (&optional (prefix "CHUNK"))
  (if (or (stringp prefix) (symbolp prefix))
      (let* ((component (get-component naming))
             (name-table (when component (naming-module-table component))))
        (if name-table
            (bt:with-lock-held ((naming-module-table-lock component))
              (let* ((base (string-upcase prefix))
                     (element (gethash base name-table)))
                (unless element
                  (setf element
                    (setf (gethash base name-table) (make-act-r-name))))
                (bt:with-lock-held (*symbols-table-lock*)
                  (unless (gethash base *global-symbols-table*)
                    (setf (gethash base *global-symbols-table*) 0))
                  
                  (do* ((count (gethash base *global-symbols-table*)
                               (incf (gethash base *global-symbols-table*)))
                        (symbol-name (concatenate 'string base "-" (fast-num->string count))
                                     (concatenate 'string base "-" (fast-num->string count)))
                        (existed (find-symbol symbol-name)
                                 (find-symbol symbol-name)))
                       ((not existed)
                        (progn
                          (push count (act-r-name-used-symbols element))
                          (incf (gethash base *global-symbols-table*))
                          (values (intern symbol-name))))))))
          (print-warning "No naming component available cannot create new symbol.")))
    (print-warning "Invalid parameter passed to new-symbol.  Must be a string or symbol.")))

(add-act-r-command "new-symbol" 'new-symbol-fct "Create new names outside of a model. Params: {name-prefix}")

(defun update-chunks-at-all ()
  (let ((model (current-model-struct)))
    (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
      (act-r-model-chunk-update model))))
    
(defun update-chunks-on-the-fly ()
  (let ((model (current-model-struct)))
    (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
      (and (act-r-model-chunk-update model)
           (act-r-model-dynamic-update model)))))

(defun notify-on-the-fly-hooks ()
  (let ((model (current-model-struct)))
    (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
      (act-r-model-dynamic-update-hooks model))))

(defun delete-chunks-after-run ()
  (let ((model (current-model-struct)))
    (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
      (act-r-model-delete-chunks model))))

(defun use-short-copy-names ()
  (let ((model (current-model-struct)))
    (bt:with-lock-held ((act-r-model-chunk-updating-lock model))
      (act-r-model-short-copy-names model))))

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
    
