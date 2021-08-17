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
;;; Filename    : high-performance.lisp
;;; Version     : 1.0
;;; 
;;; Description : Add a system parameter that kills the trace (bypasses the 
;;;               filter tests) for all models and sets the default for some
;;;               other parameters that affect speed.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2019.02.15 Dan [1.0]
;;;             : * Initial creation.
;;; 2018.03.04 Dan
;;;             : * Added visual-movement-tolerance set to 0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; New system parameter named :high-performance.  Can be set to t or nil, and
;;; the default is nil.  Can only be changed when there are no models defined.
;;; When set to nil it does not affect the system.  Setting it to t should 
;;; provide a significant improvement in the time required to run a model, but
;;; at the cost of all trace and diagnostic outputs.  When set to t it prevents
;;; all model traces from being displayed without having to check the trace
;;; filters.  It also sets the :starting-parameters system parameter like this:
;;;
;;; (:ncnar nil :style-warnings nil :model-warnings nil :cmdt nil :lhst nil 
;;;  :rhst nil :stable-loc-names nil :visual-movement-tolerance 0)
;;;
;;; because those parameters default to t and when set can slow down the system.
;;; 
;;; The setting of :starting-parameters will overwrite any current settings and
;;; it will print a warning if it does so.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; system parameter :high-performance can be set to t or nil.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Uses a simple global variable to store the value which is also the flag for
;;; the scheduler to check, and by not allowing it to be changed when there are
;;; models defined avoids the need for a lock which would basically defeat the
;;; purpose.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;; Make sure :starting-parameters exists 

(require-compiled "CENTRAL-PARAMETERS")

(defun high-performance-param (set-or-get value)
  (when set-or-get
    (setf *allow-event-output* (not value))
    (if value
        (progn
          (awhen (car (ssp :starting-parameters))
                 (print-warning "Setting :high-performance will overwrite the current :starting-parameters setting of ~s" it))
          (ssp :starting-parameters (:ncnar nil :style-warnings nil :model-warnings nil :cmdt nil :lhst nil :rhst nil :stable-loc-names nil :visual-movement-tolerance 0))
          )
      (ssp :starting-parameters nil)))
  (not *allow-event-output*))

(defun high-performance-test (val)
  (and (tornil val)
       (zerop (length (mp-models)))))

(create-system-parameter :high-performance
                         :handler 'high-performance-param
                         :documentation "Change default value for performance based parameters and disable the trace output."
                         :valid-test 'high-performance-test
                         :warning "T or nil and only when no models are defined"
                         :default-value nil)



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
