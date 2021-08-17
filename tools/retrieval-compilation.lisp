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
;;; Filename    : retrieval-compilation.lisp
;;; Version     : 3.0
;;; 
;;; Description : Production compilation RETRIEVAL style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; ----- History -----
;;;
;;; 2010.12.06 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;;             : * Added the details of the functions.
;;; 2010.12.15 Dan
;;;             : * Added module to the mapping functions args list.
;;; 2011.04.28 Dan
;;;             : * Removed an unneeded let variable from map-retrieval-buffer.
;;; 2012.04.04 Dan [1.1]
;;;             : * Added the whynot reason function.
;;; 2014.05.07 Dan [2.0]
;;;             : * Start of conversion to typeless chunks.
;;;             : * References to compilation-module-previous are now using a
;;;             :   structure instead of list.
;;; 2016.11.18 Dan
;;;             : * When the buffer is strict harvested in p1 (8 or 24) and p2 has
;;;             :   a query for buffer empty (16 or 20) then drop that buffer
;;;             :   empty query from the composed production.
;;; 2018.02.19 Dan
;;;             : * P1 = 24 is not the same as p1 = 20 and 28 so it needs its
;;;             :   own case.
;;; 2018.02.20 Dan
;;;             : * When retrieval requests in both functions are composed it
;;;             :   should always be p2's request which stays, but for some
;;;             :   reason case p1 = 4 || 12 was keeping p1's.  Of course I don't
;;;             :   know why we allow that anyway, but I guess someone wanted it
;;;             :   at some point.
;;; 2020.06.22 Dan [3.0]
;;;             : * The functions are now all passed the same set of parameters:
;;;             :   buffer module p1 p1-s p1-index p2 p2-s p2-index
;;;             :   the p1-s and p2-s are the new "standard" representation that
;;;             :   can be decomposed with composition-rep-* where * is:
;;;             :    op -- the character of the operation =,?,!,+,-,*,@
;;;             :    name -- the symbol naming the buffer or bang action
;;;             :    token -- the symbol of the production item for op and name
;;;             :    slots -- list of slot-spec lists
;;; 2020.11.05 Dan 
;;;             : * Fixed a bug with the dropping of the buffer empty query.
;;; 2020.11.10 Dan
;;;             : * Fixed the typo of modfication in the failure strings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun MAP-RETRIEVAL-BUFFER (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "map references from p1 to p2 for retrieval style buffer"
  (declare (ignore p1))
  ;; current specification will only allow this
  ;; in the case that it's a RHS + or nothing
  ;; combined with a LHS = or "busy" query.
  ;;
  ;; Then, the only time there are mappings are
  ;; when it's a + followed by an =.
 
  (cond ((and (find p1-index '(4 12 20 28))
              (find p2-index '(8 12 24 28)))
         
         ;; map variables onto constants of retrieved chunk
         
         (let* ((buffer-variable (intern (concatenate 'string "=" (symbol-name buffer))))
                (the-chunk (cdr (assoc buffer-variable (production-bindings p2)))))
           
           (when the-chunk
             
             (let ((mappings (aif (assoc buffer-variable (production-drop-out-buffers-map (production-name p2)))
                                 (list (cons (cdr it) the-chunk))
                               (list (cons buffer-variable the-chunk)))))
               
               (dolist (request-slot-spec (compose-rep-slots (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s))))
                 
                 (when (chunk-spec-variable-p (spec-slot-name request-slot-spec))
                   
                   ;; Variablized slot needs to be instantiated...
                   
                   (push (assoc (spec-slot-name request-slot-spec) (previous-production-bindings (compilation-module-previous module))) mappings))
                 
                 (when (and (eq (spec-slot-op request-slot-spec) '=)
                            (chunk-spec-variable-p (spec-slot-value request-slot-spec)))
                   
                   ;; Update to handle partial matching
                   ;; get the binding for the action from the first production's 
                   ;; instantiation instead of what's retrieved in the buffer
                   ;(push (cons (third condition) (chunk-slot-value-fct the-chunk (second condition))) mappings)
                   
                   (push (assoc (spec-slot-value request-slot-spec) (previous-production-bindings (compilation-module-previous module))) mappings)))
               
               (dolist (condition-slot-spec (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                 
                 ;; Bind a variablized slot to its instantiation
                 
                 (when (chunk-spec-variable-p (spec-slot-name condition-slot-spec))
                   (push (assoc (spec-slot-name condition-slot-spec) (production-compilation-instan (production-name p2))) mappings))
                 
                 (when (and (eq (spec-slot-op condition-slot-spec) '=)
                            (chunk-spec-variable-p (spec-slot-value condition-slot-spec)))
                   (if (chunk-spec-variable-p (spec-slot-name condition-slot-spec))
                       ;; if the slot name is a variable we need to use the instantiation of
                       ;; that to know what the real slot name was which was pushed onto mappings above
                       (push (cons (spec-slot-value condition-slot-spec) (chunk-slot-value-fct the-chunk (cdr (assoc (spec-slot-name condition-slot-spec) mappings)))) mappings)
                     
                     (push (cons (spec-slot-value condition-slot-spec) (chunk-slot-value-fct the-chunk (spec-slot-name condition-slot-spec))) mappings))))
               
               mappings))))
        (t
         nil)))

(defun COMPOSE-RETRIEVAL-BUFFER (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  ;; This is based on the limited set of conditions that can
  ;; be composed.
  ;;
  ;; The constraints are:
  ;;
  ;;   The only action that will remain is a
  ;;      request from the second production if such exists 
  ;;    or
  ;;      a request from the first if the second doesn't 
  ;;      harvest the buffer
  ;;
  ;;   If the first production doesn't mention the buffer (0)
  ;;      any = condition and any query are used from the second
  ;;   If the first production makes a request without a query (4, 12)
  ;;      any = condition in the first production is used and there
  ;;      are no queries
  ;;   If the first production tests the buffer but doesn't make
  ;;      any queries or requests (8)
  ;;      any = condition in the first is used along with any 
  ;;      query from the second except that if the buffer is strict
  ;;      harvested a buffer empty query from p2 is dropped 
  ;;   If the first has no = condition but does have queries 
  ;;      and is without a request (16)
  ;;      the = condition from the second is used along with
  ;;      the query from the first
  ;;   If the first has both an = condition and a query or a
  ;;      query and a request (20, 24, 28)
  ;;      Both the = condition and query from the first are used
  
  (declare (ignore p1 p2))
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
       (if (find p2-index '(0 16))
           (list (when c1 (list c1))
                 (when a1+ (list a1+)))
         (list (when c1 (list c1))
               (when a2+ (list a2+)))))
      (8
       (list (append 
              (when c1 
                (list c1)) 
              
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
      (16
       (list (append 
              (when c2 
                (list c2)) 
              (when q1 
                (list q1)))
             (when a2+ 
               (list a2+))))
      (24
       (list (append (when c1 (list c1)) (when q1 (list q1)))
             (when a2+ (list a2+))))
      ((20 28)
       (if (find p2-index '(0 16))
           (list (append (when c1 (list c1)) (when q1 (list q1)))
                 (when a1+ (list a1+)))
         (list (append (when c1 (list c1)) (when q1 (list q1)))
               (when a2+ (list a2+))))))))

(defun R-B-C1 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Compilation check for queries such that p2 only uses 'buffer empty' or 'state busy'"
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


(defun R-B-C2 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
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

(defun retrieval-reason (p1-index p2-index failed-function)
  (cond  ((eql failed-function 'r-b-c1)
         "when the first production makes a request and the second does not harvest it the second can only query for state busy or buffer empty.")
        ((eql failed-function 'r-b-c2)
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
              (t
               "strict harvesting should have prevented the buffer condition from matching in the second production.")))))))

(define-compilation-type RETRIEVAL ((28 28 T)
                                    (28 24 T)
                                    (28 20 R-B-C1)
                                    (28 16 R-B-C1)
                                    (28 12 T)
                                    (28 8 T)
                                    (28 4 T)
                                    (28 0 T)
                                    (24 20 R-B-C2)
                                    (24 16 R-B-C2)
                                    (24 4 T)
                                    (24 0 T)
                                    (20 28 T)
                                    (20 24 T)
                                    (20 20 R-B-C1)
                                    (20 16 R-B-C1)
                                    (20 12 T)
                                    (20 8 T)
                                    (20 4 T)
                                    (20 0 T)
                                    (16 28 R-B-C2)
                                    (16 24 R-B-C2)
                                    (16 20 R-B-C2)
                                    (16 16 R-B-C2)
                                    (16 12 T)
                                    (16 8 T)
                                    (16 4 T)
                                    (16 0 T)
                                    (12 28 T)
                                    (12 24 T)
                                    (12 20 R-B-C1)
                                    (12 16 R-B-C1)
                                    (12 12 T)
                                    (12 8 T)
                                    (12 4 T)
                                    (12 0 T)
                                    (8 20 T)
                                    (8 16 T)
                                    (8 4 T)
                                    (8 0 T)
                                    (4 28 T)
                                    (4 24 T)
                                    (4 20 R-B-C1)
                                    (4 16 R-B-C1)
                                    (4 12 T)
                                    (4 8 T)
                                    (4 4 T)
                                    (4 0 T)
                                    (0 28 T)
                                    (0 24 T)
                                    (0 20 T)
                                    (0 16 T)
                                    (0 12 T)
                                    (0 8 T)
                                    (0 4 T)) 
  (RETRIEVAL) MAP-RETRIEVAL-BUFFER COMPOSE-RETRIEVAL-BUFFER NIL NIL T retrieval-reason)

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
