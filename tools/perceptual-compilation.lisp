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
;;; Filename    : perceptual-compilation.lisp
;;; Version     : 4.1
;;; 
;;; Description : Production compilation PERCEPTUAL style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; ----- History -----
;;;
;;; 2010.12.06 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;;             : * Added the details of the functions.
;;; 2012.04.04 Dan [1.1]
;;;             : * Added the whynot reason function.
;;; 2014.05.07 Dan [2.0]
;;;             : * Start of conversion to typeless chunks.
;;; 2019.04.26 Dan [3.0]
;;;             : * Allow compilation of perceptual buffers when strict harvesting
;;;             :   is turned off over cases which would be stuffed buffers when
;;;             :   it is on.
;;; 2020.06.22 Dan [4.0]
;;;             : * The functions are now all passed the same set of parameters:
;;;             :   buffer module p1 p1-s p1-index p2 p2-s p2-index
;;;             :   the p1-s and p2-s are the new "standard" representation that
;;;             :   can be decomposed with composition-rep-* where * is:
;;;             :    op -- the character of the operation =,?,!,+,-,*,@
;;;             :    name -- the symbol naming the buffer or bang action
;;;             :    token -- the symbol of the production item for op and name
;;;             :    slots -- list of slot-spec lists
;;; 2020.11.05 Dan [4.1]
;;;             : * Drop buffer empty queries that occur in p2 if the buffer
;;;             :   was strict harvested from p1 otherwise you end up with a
;;;             :   production that can't fire since it tests for both there
;;;             :   being a chunk in the buffer and a query that it's empty.
;;; 2020.11.10 Dan
;;;             : * Fixed the typo of modfication in the failure strings.
;;; 2021.02.17 Dan
;;;             : * Updated the calls to buffer-condition-union since that needs
;;;             :   a 4th parameter of bindings that may be needed deal with
;;;             :   dynamic issues (not needed for perceptual).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun COMPOSE-PERCEPTUAL-BUFFER (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (declare (ignore p1 p2 p2-index))
  ;; This is based on the limited set of conditions that can
  ;; be composed.
  ;;
  ;; There are no buffer modification actions allowed.
  ;;
  ;; The constraints are:
  ;;
  ;;
  ;;   If the first production doesn't mention the buffer 
  ;;      or make a request (0)
  ;;      any = condition, query and action are used from the second
  ;;   If the first production makes a request without a query (4, 12)
  ;;      any = condition in the first production is used, there
  ;;      are no queries and the action from the first is used
  ;;   If the first production tests the buffer but doesn't make
  ;;      any queries or requests (8)
  ;;      any = condition in the first is used along with any 
  ;;      query from the second and the action of the second
  ;;   If the first a query and no request (16, 24)
  ;;      any = condition in either (there can be at most 1) is used 
  ;;      along with the query from the first and the action
  ;;      of the second
  ;;   If the first has both a query and a request (20, 28)
  ;;      the = condition query and action from the first are used
  
  
  (let ((c1 (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s)))
        (c2 (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s)))
        (q1 (find-if (lambda (x) (and (char= #\? (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s)))
        (q2 (find-if (lambda (x) (and (char= #\? (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s)))
        
        (a1+ (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s)))
        (a2+ (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p2-s))))
    
    (case p1-index
      (0 
       (list (append 
              (when c2 
                (list c2)) 
              (when q2 
                (list q2)))  
             (when a2+ 
               (list a2+))))
      ((4 12)
       (list (when c1 
               (list c1))
             (when a1+ 
               (list a1+))))
      (8
       (list (append 
              (awhen (buffer-condition-union c1 c2 nil nil) 
                     (list it)) 
              (if q2 
                    (if (find buffer (compilation-module-no-harvest module))
                        (list q2)
                      ;; strict harvested so need to ignore a buffer empty query from p2
                      (let ((slots (compose-rep-slots q2)))
                        (setf slots (remove '(= buffer empty) slots :test 'equalp))
                        (if slots
                            (list (list (first q2) slots)) ;; building a compose-rep explicitly because don't want to destructively modify
                          nil)))
                  nil))
             (when a2+ 
               (list a2+))))
      ((16 24)
       (list (append 
              (awhen (buffer-condition-union c1 c2 nil nil) 
                     (list it)) 
              (when q1 
                (list q1)))
             (when a2+ 
               (list a2+))))
      ((20 28)
       (list (append 
              (when c1 
                (list c1)) 
              (when q1 
                (list q1)))
             (when a1+ 
               (list a1+)))))))

(defun P-B-C1 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Compilation check for queries such that p2 only uses 'buffer empty' or
   'state busy'"
  (declare (ignore module p1 p1-s p1-index p2 p2-index))
  (let ((query (find-if (lambda (x)
                          (and (char= (compose-rep-op x) #\?)
                               (eq (compose-rep-name x) buffer)))
                        (first p2-s))))
    (every (lambda (x)      
             (or 
              (equalp x '(= state busy))
              (equalp x '(= buffer empty))))
           (compose-rep-slots query))))


(defun P-B-C2 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
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


(defun P-B-C3 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "only if the buffer is not strict harvested"
  (declare (ignore p1 p1-s p1-index p2 p2-s p2-index))
  (find buffer (compilation-module-no-harvest module)))


(defun P-B-C4 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Not strict harvested and same queries"
  (and (p-b-c2 buffer module p1 p1-s p1-index p2 p2-s p2-index) (p-b-c3 buffer module p1 p1-s p1-index p2 p2-s p2-index)))

(defun perceptual-reason (p1-index p2-index failed-function)
  (cond ((eql failed-function 'p-b-c1)
         "when the first production makes a request and the second does not harvest it the second can only query for state busy or buffer empty.")
        ((eql failed-function 'p-b-c2)
         "the queries in both productions must be the same.")
        ((> p1-index 30)
         "buffer modification actions in first production are not allowed.")
        ((> p2-index 30)
         "buffer modification actions in second production are not allowed.")
        (t 
         (case p1-index
           ((9 11 13 15 25 27 29)
            "buffer modification actions in first production are not allowed.")
           ((2 6 10 14 18 22 26 30)
            "the buffer is explicitly cleared in the first production.")
           (t
            (case p2-index
              ((2 6 10 14 18 22 26 30)
               "the buffer is explicitly cleared in the second production.")
              ((9 11 13 15 25 27 29)
               "buffer modification actions in second production are not allowed.")
              ((4 20)
               "both productions make a request.")
              (t
               (case p1-index
                 ((4 12 20 28)
                  "the first production makes a request and the second production harvests the chunk.")
                 (t
                  "the second production harvests a chunk which is there because of buffer stuffing or because strict harvesting has been disabled.")))))))))

(define-compilation-type PERCEPTUAL ((28 16 P-B-C1)
                                     (28 0 T)
                                     (24 28 P-B-C4)
                                     (24 24 P-B-C4)
                                     (24 20 P-B-C2)
                                     (24 16 P-B-C2)
                                     (24 12 P-B-C3)
                                     (24 8 P-B-C3)
                                     (24 4 T)
                                     (24 0 T)
                                     (20 16 P-B-C1)
                                     (20 0 T)
                                     (16 28 P-B-C2)
                                     (16 24 P-B-C2)
                                     (16 20 P-B-C2)
                                     (16 16 P-B-C2)
                                     (16 12 T)
                                     (16 8 T)
                                     (16 4 T)
                                     (16 0 T)
                                     (12 16 P-B-C1)
                                     (12 0 T)
                                     (8 28 P-B-C3)
                                     (8 24 P-B-C3)
                                     (8 20 T)
                                     (8 16 T)
                                     (8 12 P-B-C3)
                                     (8 8 P-B-C3)
                                     (8 4 T)
                                     (8 0 T)
                                     (4 16 P-B-C1)
                                     (4 0 T)
                                     (0 28 T)
                                     (0 24 T)
                                     (0 20 T)
                                     (0 16 T)
                                     (0 12 T)
                                     (0 8 T)
                                     (0 4 T)) 
  (VISUAL-LOCATION VISUAL AURAL-LOCATION AURAL TEMPORAL) NIL COMPOSE-PERCEPTUAL-BUFFER NIL NIL NIL perceptual-reason)

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
