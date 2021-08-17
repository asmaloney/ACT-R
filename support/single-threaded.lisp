;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2019 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : single-threaded.lisp
;;; Version     : 2.0
;;; 
;;; Description : No-op all the lock actions.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2019.04.09 Dan
;;;             : * Created this to provide a fast single threaded version.
;;; 2019.04.17 Dan
;;;             : * Didn't add the acquire/release-recursive-lock functions.
;;; 2019.05.29 Dan [2.0]
;;;             : * Actually define the packages and all the Quicklisp library
;;;             :   functions used in the code so I can skip Quicklisp for the
;;;             :   single-threaded version.
;;; 2019.05.30 Dan
;;;             : * Remove the jsown package and stub.
;;; 2019.11.12 Dan
;;;             : * The stubs for acquire-lock and acquire-recursive-lock need
;;;             :   to return t otherwise it looks like a failure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Allows the ACT-R code to compile and run properly while ignoring all of the
;;; locking actions and other Quicklisp library code.  This of course means that
;;; the code is no longer thread safe.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; None.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Just make the lock based actions no-ops by redefining the functions and
;;; macros from the :bordeaux-threads package with dummies.
;;;
;;; Create dummy stubs for all the other functions referenced in the code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure not to start the dispatcher

(pushnew :standalone *features*)

;; define the packages which would come from Quicklisp loads

(defpackage :bordeaux-threads
  (:nicknames :bt)
  (:use :cl)
  (:export
   :make-lock
   :acquire-lock
   :release-lock
   :with-lock-held
   :make-recursive-lock
   :acquire-recursive-lock
   :release-recursive-lock
   :with-recursive-lock-held
   :make-condition-variable
   :condition-notify
   :condition-wait
   :make-thread
   :threadp
   :thread-alive-p
   :destroy-thread
   :thread-yield))


(defpackage :usocket
  (:use :cl)
  (:export 
   :socket-stream
   :socket-close
   :socket-listen
   :socket-accept
   :ip=))


(defpackage :json
  (:nicknames :cl-json)
  (:use :cl)
  (:export
   
   :*lisp-identifier-name-to-json*
   :encode-json-to-string
   :decode-json-from-string
   :decode-json-from-source))

;; In each one define dummy versions of the things used.

(in-package :json)

(defparameter +json-lisp-symbol-tokens+ nil)
(defvar *json-output* nil)
(defvar *lisp-identifier-name-to-json* nil)

(defun write-json-string (&rest x) (declare (ignore x)))
(defun encode-json-to-string (s &rest x) (declare (ignore x)) s)
(defun decode-json-from-string (s &rest x) (declare (ignore x)) s)
(defun decode-json-from-source (s &rest x) (declare (ignore s x)))

(defgeneric encode-json (object &optional stream))

(defmethod encode-json (x &optional s)
  (declare (ignorable x s)))


(in-package :usocket)

(defun socket-stream (&rest x) (declare (ignore x)))
(defun socket-close (&rest x) (declare (ignore x)))
(defun socket-listen (&rest x) (declare (ignore x)))
(defun socket-accept (&rest x) (declare (ignore x)))
(defun ip= (&rest x) (declare (ignore x)))

(defun get-peer-port (&rest x) (declare (ignore x)))
(defun get-peer-address (&rest x) (declare (ignore x)))
(defun split-sequence (&rest x) (declare (ignore x)))
(defun get-hosts-by-name (&rest x) (declare (ignore x)))
(defun get-host-name (&rest x) (declare (ignore x)))
(defun vector-quad-to-dotted-quad (&rest x) (declare (ignore x)))
(defun usocket-p (&rest x) (declare (ignore x)))

(define-condition address-in-use-error () ())


(in-package :bordeaux-threads)

(defmacro with-lock-held ((place) &body body)
  (declare (ignore place))
  `(progn
     ,@body))

(defmacro with-recursive-lock-held ((place &key timeout) &body body)
  (declare (ignore place timeout))
  `(progn
     ,@body))

(defun make-lock (&optional x) (declare (ignore x)))
(defun make-recursive-lock (&optional x) (declare (ignore x)))
(defun acquire-lock (l &optional w) (declare (ignore l w)) t)
(defun release-lock (l) (declare (ignore l)))
(defun acquire-recursive-lock (l) (declare (ignore l)) t)
(defun release-recursive-lock (l) (declare (ignore l)))
(defun make-condition-variable (&rest x) (declare (ignore x)))
(defun condition-notify (&rest x) (declare (ignore x)))
(defun condition-wait (&rest x) (declare (ignore x)))

(defun make-thread (&rest x) (declare (ignore x)))
(defun threadp (&rest x) (declare (ignore x)))
(defun thread-alive-p (&rest x) (declare (ignore x)))
(defun destroy-thread (&rest x) (declare (ignore x)))
(defun thread-yield (&rest x) (declare (ignore x)))
   

;; Switch back to the original package for loading ACT-R

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


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
