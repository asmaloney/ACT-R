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
;;; Filename    : system-locks.lisp
;;; Version     : 1.0
;;; 
;;; Description : Code for creating and using locks at the meta-process and model
;;;             : levels.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2017.03.30 Dan [1.0]
;;;             : * Initial creation.
;;; 2020.07.29 Dan
;;;             : * Fixed a bug with make-act-r-model-lock even though it's not
;;;             :   currently being used.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; In order for things to be thread safe need locks around lots of things...
;;; So, this code provides the mechanism for creating and using locks that exist
;;; at the meta-process and the model level.  A lock gets named with a string and
;;; the with-... and acquire-.../release-... macros are used to protect the code 
;;; as needed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; The meta-process table is not cleared upon clear-all and the model table is
;;; not cleared upon reset.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defun make-act-r-lock (name &optional recursive)
  (if (gethash name (meta-p-lock-table (current-mp)))
      (print-warning "A lock named ~s already exists." name)
    (progn
      (setf (gethash name (meta-p-lock-table (current-mp)))
        (if recursive
            (cons (bt:make-recursive-lock name) t)
          (cons (bt:make-lock name) nil)))
      name)))


(defun make-act-r-model-lock (name &optional recursive)
  (with-current-model 
   "No current model available to create a model lock"
   (if (gethash name (act-r-model-lock-table current-model))
       (print-warning "A lock named ~s already exists in the current model." name)
     (progn
       (setf (gethash name (act-r-model-lock-table current-model))
         (if recursive
             (cons (bt:make-recursive-lock name) t)
           (cons (bt:make-lock name) nil)))
       name))))

(defmacro with-act-r-lock (name &body body)
  (let ((lock (gensym)))
    `(let* ((,lock (gethash ,name (meta-p-lock-table (current-mp)))))
       (if ,lock
           (if (cdr ,lock)
               (bt:with-recursive-lock-held ((car ,lock))
                 (progn ,@body))
             (bt:with-lock-held ((car ,lock))
               (progn ,@body)))
         (print-warning "No actions taken in with-act-r-lock because there is no lock named ~s" ,name)))))

(defun acquire-act-r-lock (name &optional (wait t))
  (let ((lock (gethash name (meta-p-lock-table (current-mp)))))
    (if lock
        (if (cdr lock)
            (bt:acquire-recursive-lock (car lock))
          (bt:acquire-lock (car lock) wait))
      (print-warning "Cannot acquire act-r-lock because there is no lock named ~s" name))))

(defun release-act-r-lock (name)
  (let ((lock (gethash name (meta-p-lock-table (current-mp)))))
    (if lock
        (if (cdr lock)
            (bt:release-recursive-lock (car lock))
          (bt:release-lock (car lock)))
      (print-warning "Cannot release act-r-lock because there is no lock named ~s" name))))


(defmacro with-act-r-model-lock (name &body body)
  (let ((lock (gensym)))
    `(with-current-model
         (let* ((,lock (gethash ,name (act-r-model-lock-table current-model))))
           (if ,lock
               (if (cdr ,lock)
                   (bt:with-recursive-lock-held ((car ,lock))
                     (progn ,@body))
                 (bt:with-lock-held ((car ,lock))
                   (progn ,@body)))
             (print-warning "No actions taken in with-act-r-model-lock because there is no lock named ~s in the current model: ~s" ,name current-model))))))

(defun acquire-act-r-model-lock (name &optional (wait t))
  (with-current-model 
      "Cannot acquire a lock because there is no current model."
      (let ((lock (gethash name (act-r-model-lock-table current-model))))
        (if lock
            (if (cdr lock)
                (bt:acquire-recursive-lock (car lock))
              (bt:acquire-lock (car lock) wait))
          (print-warning "Cannot acquire act-r-lock because there is no lock named ~s" name)))))

(defun release-act-r-model-lock (name)
  (with-current-model
      "Cannot acquire a lock because there is no current model."
    (let ((lock (gethash name (act-r-model-lock-table current-model))))
        (if lock
            (if (cdr lock)
                (bt:release-recursive-lock (car lock))
              (bt:release-lock (car lock)))
          (print-warning "Cannot release act-r-lock because there is no lock named ~s" name)))))
  


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
