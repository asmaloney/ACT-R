;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2010 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : motor-compilation.lisp
;;; Version     : 2.0
;;; 
;;; Description : Production compilation MOTOR style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; ----- History -----
;;;
;;; 2010.12.06 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;;             : * Added the details of the functions.
;;; 2010.12.10 Dan
;;;             : * Changed m-b-c1 since buffer empty is not a sufficient case
;;;             :   for safety since the buffer is always empty.
;;; 2012.04.04 Dan [1.1]
;;;             : * Added the whynot reason function.
;;; 2012.05.01 Dan
;;;             : * Removed a duplicate number from the cases in the whynot function.
;;; 2020.06.22 Dan [2.0]
;;;             : * The functions are now all passed the same set of parameters:
;;;             :   buffer module p1 p1-s p1-index p2 p2-s p2-index
;;;             :   the p1-s and p2-s are the new "standard" representation that
;;;             :   can be decomposed with composition-rep-* where * is:
;;;             :    op -- the character of the operation =,?,!,+,-,*,@
;;;             :    name -- the symbol naming the buffer or bang action
;;;             :    token -- the symbol of the production item for op and name
;;;             :    slots -- list of slot-spec lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun COMPOSE-MOTOR-BUFFER (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (declare (ignore module p1 p2 p2-index))
  ;; This is based on the limited set of conditions that can
  ;; be composed.
  ;;
  ;; There are no buffer tests or modification actions allowed.
  ;;
  ;; The constraints are:
  ;;
  ;;   If the first production doesn't make a query 
  ;;      or make a request (0)
  ;;      any query and action are used from the second
  ;;   If the first production makes a request without a query (4)
  ;;      there is no query and the action from the first is used
  ;;   If the first has a query and no request (16)
  ;;      the query from the first and the action
  ;;      of the second are used 
  ;;   If the first has both a query and a request (20)
  ;;      the query and action from the first are used
  
  
  (let ((q1 (find-if (lambda (x) (and (char= #\? (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s)))
        (q2 (find-if (lambda (x) (and (char= #\? (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s)))
        
        (a1+ (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s)))
        (a2+ (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p2-s))))
    
    (case p1-index
      (0 
       (list (when q2 (list q2))  
             (when a2+ (list a2+))))
      (4
       (list nil
             (when a1+ (list a1+))))
      (16 
       (list (when q1 (list q1))  
             (when a2+ (list a2+))))
      
      (20
       (list (when q1 (list q1))  
             (when a1+ (list a1+)))))))

(defun M-B-C1 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Compilation check for queries such that p2 only uses 'state busy' 
  since buffer empty is meaningless for motor style buffers"
  (declare (ignore module p1 p1-s p1-index p2 p2-index))
  ;; there can be only one
  (let ((query (find-if (lambda (x)
                          (and (char= (compose-rep-op x) #\?)
                               (eq (compose-rep-name x) buffer)))
                        (first p2-s))))
    (every (lambda (x)      
             (equalp x '(= state busy)))
           (compose-rep-slots query))))


(defun M-B-C2 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "queries in p1 and p2 must be the same
   NOTE: this doesn't take into account any variables at this time"
  (declare (ignore module p1 p1-index p2 p2-index))
  (let ((query1 (awhen (find-if (lambda (x)
                          (and (char= (compose-rep-op x) #\?)
                               (eq (compose-rep-name x) buffer)))
                        (first p1-s))
                       (compose-rep-slots it)))
        (query2 (awhen (find-if (lambda (x)
                          (and (char= (compose-rep-op x) #\?)
                               (eq (compose-rep-name x) buffer)))
                        (first p2-s))
                       (compose-rep-slots it))))
    
    (= (length query1) (length query2) 
       (length (remove-duplicates (append query1 query2) :test 'equal)))))

(defun motor-reason (p1-index p2-index failed-function)
  (cond ((eql failed-function 'm-b-c1)
         "when the first production makes a request the second can only query for state busy.")
        ((eql failed-function 'm-b-c2)
         "the queries in both productions must be the same.")
        (t 
         (case p1-index
           ((8 9 10 11 12 13 14 15 24 25 26 27 28 29 30 31 40 41 42 44 43 45 46 47 56 57 58 60 59 61 62 63)
            "buffer tests in first production are not allowed.")
           ((2 6 18 22)
            "the buffer is explicitly cleared in the first production.")
           (t
            (case p2-index
              ((8 9 10 11 12 13 14 15 24 25 26 27 28 29 30 31 40 41 42 44 43 45 46 47 56 57 58 60 59 61 62 63)
               "buffer tests in second production are not allowed.")
              ((2 6 18 22)
               "the buffer is explicitly cleared in the second production.")
              (t
               "both productions make requests.")))))))

(define-compilation-type MOTOR ((20 16 M-B-C1)
                                (20 0 T)
                                (16 20 M-B-C2)
                                (16 16 M-B-C2)
                                (16 4 T)
                                (16 0 T)
                                (4 16 M-B-C1)
                                (4 0 T)
                                (0 20 T)
                                (0 16 T)
                                (0 4 T))
  :DEFAULT NIL COMPOSE-MOTOR-BUFFER NIL NIL NIL motor-reason)

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
