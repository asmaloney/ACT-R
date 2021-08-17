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
;;; Filename    : goal-compilation.lisp
;;; Version     : 4.0
;;; 
;;; Description : Production compilation GOAL style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       :  [X] Update the functions to properly handle the modification requests.
;;;             :  [ ] Consider allowing queries: any P1 --> P3 and P2 state free --> null.
;;;
;;; ----- History -----
;;;
;;; 2010.12.06 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;;             : * Added the details of the functions.
;;; 2010.12.07 Dan
;;;             : * Added the appropriate support for modification requests to the
;;;             :   functions.
;;; 2010.12.15 Dan
;;;             : * Added module to the mapping functions args list.
;;; 2010.12.20 Dan
;;;             : * Added some additional code to handle things better when 
;;;             :   :ppm is enabled -- creates productions that match more closely
;;;             :   to the same conditions as the original pair did and does 
;;;             :   the "same" thing. 
;;;             : * Allow subtypes to be composed in the consistency check.
;;; 2011.04.28 Dan
;;;             : * Added some declares to avoid compiler warnings.
;;; 2012.04.04 Dan [1.1]
;;;             : * Added the whynot reason function.
;;; 2014.05.07 Dan [2.0]
;;;             : * Start of conversion to typeless chunks.
;;;             : * Pass the module to constant-value-p.
;;;             : * References to compilation-module-previous are now using a
;;;             :   structure instead of list.
;;; 2014.05.27 Dan
;;;             : * Fixed a bug in compose-goal-buffer with the (4 12 13 44) 
;;;             :   case not wrapping c1 in a list.
;;; 2015.08.17 Dan
;;;             : * The last update actually broke things for the case where
;;;             :   there is no condition in c1 and this fixes that.
;;; 2016.08.05 Dan [3.0]
;;;             : * Updated the compilation type definition with one that was
;;;             :   created automatically by build-compilation-type-file from the
;;;             :   new spreadsheet that better handles "safe" compilation based
;;;             :   on whether the buffer is strict harvested or not.  Also added
;;;             :   some cases which should have been allowed but weren't where
;;;             :   an = action with a preceeding * can be put together into a *.
;;;             : * Added the code necessary to deal with the new cases and the 
;;;             :   new test functions.
;;; 2020.06.22 Dan [4.0]
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


(defun MAP-GOAL-BUFFER (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "map references from p1 to p2 for goal style buffer"
  ;; With the current restricted set of productions
  ;; that could be composed it's the case that
  ;; we only need to go from: a RHS + to a
  ;; LHS = (includes when the RHS has both + and =),
  ;; from a RHS = to a LHS = or from a LHS = with
  ;; null RHS to a LHS =.  
  ;;
  ;; Additionally now we need to consider the 
  ;; RHS * to LHS = (with both + and * on the RHS)
  ;; and additional instances of the above cases
  ;; which may include the RHS * options.
  
  (declare (ignore p1))
  
  (let* ((ppm (compilation-module-ppm module))
         (bindings (when ppm 
                     (append (previous-production-bindings (compilation-module-previous module))
                             (production-compilation-instan (production-name p2))))))
    
    (cond (;; The RHS + to LHS = case -- also overrides when there's a RHS * in p1
           (and (find p1-index '(4 12 13 44))
                (find p2-index '(8 9 12 13 40 44)))
           
           ;; Map the RHS +'s with the LHS ='s
           ;; here the slots of interest are just the intersection of the two sets
           
           (let* ((mappings nil)
                  (p1-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s))))
                  (p2-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                  (interesting-slots (intersection (mapcan (lambda (x)
                                                             (when (eq (spec-slot-op x) '=)
                                                               (list (spec-slot-name x))))
                                                     p1-slots)
                                                   (mapcan (lambda (x)
                                                               (when (eq (spec-slot-op x) '=)
                                                                 (list (spec-slot-name x))))
                                                     p2-slots))))
             
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not (lambda (x) (and (eq (spec-slot-op x) '=) (eq (spec-slot-name x) slot))) p1-slots))
                 (dolist (p2slots (remove-if-not (lambda (x) (and (eq (spec-slot-op x) '=) (eq (spec-slot-name x) slot))) p2-slots))
                   (if (constant-value-p (spec-slot-value p2slots) module)
                       (if ppm
                           (if (constant-value-p (spec-slot-value p1slots) module)
                               (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings)
                             (push (find (spec-slot-value p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings))
                     (push (cons (spec-slot-value p2slots) (spec-slot-value p1slots)) mappings)))))
             mappings))
          
          (;; The RHS = to a LHS = case
           (and (find p1-index '(9))
                (find p2-index '(8 9 12 13)))
           
           ;; Map the RHS ='s and LHS ='s not in the RHS with
           ;; the LHS ='s
           
           ;; Here the slots of interest are the union of the
           ;; p1 bits with the RHS superseding the LHS intersected
           ;; with the LHS of the second one
           
           
           (let* ((mappings nil)
                  (p1-slotsa (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s))))
                  (p1-slotsb (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s))))
                  (p2-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                  
                  (p1-slots (append (remove-if (lambda (x)
                                                   (or (not (eq (spec-slot-op x) '=))
                                                       (find (spec-slot-name x) p1-slotsb :key 'spec-slot-name)))
                                               p1-slotsa)
                                    p1-slotsb))
                  (interesting-slots  (intersection (mapcan (lambda (x) 
                                                              (list (spec-slot-name x)))
                                                      p1-slots)
                                                    (mapcan (lambda (x)
                                                                (when (eq (spec-slot-op x) '=)
                                                                  (list (spec-slot-name x))))
                                                      p2-slots))))
             
                                     
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not (lambda (x) (eq (spec-slot-name x) slot)) p1-slots))
                 (dolist (p2slots (remove-if-not (lambda (x) (eq (spec-slot-name x) slot)) p2-slots))
                   
                   (if (constant-value-p (spec-slot-value p2slots) module)
                       (if ppm
                           (if (constant-value-p (spec-slot-value p1slots) module)
                               (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings)
                             (push (find (spec-slot-value p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings))
                     (push (cons (spec-slot-value p2slots) (spec-slot-value p1slots)) mappings)))))
             
             mappings))
          
          (;; The RHS * to a LHS = case
           (and (find p1-index '(40))
                (find p2-index '(8 12 40 44)))
           
           ;; Map the RHS *'s and LHS ='s not in the RHS with
           ;; the LHS ='s
           
           ;; Here the slots of interest are the union of the
           ;; p1 bits with the RHS superseding the LHS intersected
           ;; with the LHS of the second one
           
           
           (let* ((mappings nil)
                  (p1-slotsa (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s))))
                  (p1-slotsb (compose-rep-slots (find-if (lambda (x) (and (char= #\* (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s))))
                  (p2-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                  
                  (p1-slots (append (remove-if (lambda (x)
                                                   (or (not (eq (spec-slot-op x) '=))
                                                       (find (spec-slot-name x) p1-slotsb :key 'spec-slot-name)))
                                               p1-slotsa)
                                    p1-slotsb))
                  (interesting-slots  (intersection (mapcan (lambda (x) 
                                                              (list (spec-slot-name x)))
                                                      p1-slots)
                                                    (mapcan (lambda (x)
                                                                (when (eq (spec-slot-op x) '=)
                                                                  (list (spec-slot-name x))))
                                                      p2-slots))))
             
                                     
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not (lambda (x) (eq (spec-slot-name x) slot)) p1-slots))
                 (dolist (p2slots (remove-if-not (lambda (x) (eq (spec-slot-name x) slot)) p2-slots))
                   
                   (if (constant-value-p (spec-slot-value p2slots) module)
                       (if ppm
                           (if (constant-value-p (spec-slot-value p1slots) module)
                               (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings)
                             (push (find (spec-slot-value p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings))
                     (push (cons (spec-slot-value p2slots) (spec-slot-value p1slots)) mappings)))))
             
             mappings))
                    
          (;; The LHS = RHS null to a LHS = case
           (and (find p1-index '(8))
                (find p2-index '(8 9 12 13 40 44)))
           
           ;; Map the LHS ='s with the LHS ='s
           
           
           ;; The slots of interest are the ones at the intersection of the
           ;; two sets - the mappings are then done for those
           ;; such that 
           ;;   - if it's a variable in both then p2 vars go to p1 vars 
           ;;   - if it's a constant in one then it goes from the var to the constant
           ;;     (note that buffer variables are considered constants and not variables)
           ;;   - if it's a constant in both we're in trouble if they aren't equal
           ;;     because how did they fire...
           ;;
           ;; When there is more than one option we have to add both but they need to
           ;; be evaluated in the order of variables before constants (that's handled
           ;; elsewhere though)
           
           
           (let* ((mappings nil)
                  (p1-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s))))
                  (p2-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                  (interesting-slots (intersection (mapcan (lambda (x)
                                                               (when (eq (spec-slot-op x) '=)
                                                                  (list (spec-slot-name x))))
                                                     p1-slots)
                                                   (mapcan (lambda (x)
                                                               (when (eq (spec-slot-op x) '=)
                                                                  (list (spec-slot-name x))))
                                                     p2-slots))))
             
             (dolist (slot (remove-duplicates interesting-slots))
               (dolist (p1slots (remove-if-not (lambda (x) (and (eq (spec-slot-op x) '=) (eq (spec-slot-name x) slot))) p1-slots))
                 (dolist (p2slots (remove-if-not (lambda (x) (and (eq (spec-slot-op x) '=) (eq (spec-slot-name x) slot))) p2-slots))
                   (if (constant-value-p (spec-slot-value p2slots) module)
                       (if ppm
                           (if (constant-value-p (spec-slot-value p1slots) module)
                               (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings)
                             (push (find (spec-slot-value p1slots) bindings :key 'car) mappings))
                         
                         (push (cons (spec-slot-value p1slots) (spec-slot-value p2slots)) mappings))
                     (push (cons (spec-slot-value p2slots) (spec-slot-value p1slots)) mappings)))))
             mappings))
          
          (t
           nil))))

(defun COMPOSE-GOAL-BUFFER (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (declare (ignore module p2 p2-index))
  ;; This is based on the limited set of conditions that can
  ;; be composed.
  ;;
  ;; Generally:
  ;;   If the first has a + (4, 12, 13, 44) then
  ;;      the conditions are those of the first
  ;;      the actions are the = or * of the first if there is one and
  ;;      the + will be the + of the first (can't be a + in the second)
  ;;      with the = or * of the second unioned in and overriding
  ;;   If the first has no actions (0, 8)
  ;;      the conditions are the union of those in the first
  ;;      and those of the second
  ;;      the actions are those of the second
  ;;   Otherwise (9 & 40)
  ;;      the conditions are the union of those in the first
  ;;      and those from the second that are not set by the 
  ;;      actions of the first
  ;;      the actions are the = or * from the first with the = or * from
  ;;      the second unioned in and overriding and
  ;;      the + of the second if there is one
  ;;
  (declare (ignore p1))
  
  (let* ((c1 (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s)))
         (c2 (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s)))
         (a1= (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s)))
         (a2= (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p2-s)))
         
         (a1+ (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s)))
         (a2+ (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p2-s)))
         (a1* (find-if (lambda (x) (and (char= #\* (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s)))
         (a2* (find-if (lambda (x) (and (char= #\* (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p2-s))))
    
    (case p1-index
      ((4 12 13 44)
       (list (if c1 (list c1) nil)
             (append (when (or a1= a1*) ;; can't have both with current description
                       (if a1= 
                           (list a1=) 
                         (list a1*))) 
                     (cond ((and a1+ a2=)
                            (awhen (buffer+-union a1+ a2=) 
                                   (list it)))
                           ((and a1+ a2*)
                            (awhen (buffer+-union a1+ a2*) 
                                   (list it)))
                           (a1+
                            (list a1+))
                           (t 
                            nil)))))
      ((0 8)
       (list (awhen (buffer-condition-union c1 c2 a1=) ;; a1= is always nil, though  so why use it?
                    (list it))  
             (append (when a2= 
                       (list a2=)) 
                     (when a2* 
                       (list a2*)) 
                     (when a2+ 
                       (list a2+)))))
      ((9 40)
       (list (awhen (buffer-condition-union c1 c2 (if a1= a1= a1*)) 
                    (list it))
             (append 
              (cond ((and a1* a2=)
                     (awhen (buffer=-union a1* a2=)
                            (list it)))
                    ((or a1= a2=) ;; if there's at least one = union those                             
                     (awhen (buffer=-union a1= a2=) 
                            (list it)))
                    ((or a1* a2*) ;; if there's at least one * union those
                     (awhen (buffer=-union a1* a2*) 
                            (list it)))
                    
                    (t nil))
              (when a2+ 
                (list a2+))))))))


(defun CHECK-GOAL-CONSISTENCY (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (declare (ignore p1 p2-index))
  (case p1-index
    ((4 12 13) ;; a RHS +
     (check-consistency module (find-if (lambda (x)
                                          (and (char= (compose-rep-op x) #\+)
                                               (eq (compose-rep-name x) buffer)))
                                        (second p1-s))
                        (previous-production-bindings (compilation-module-previous module))
                        (find-if (lambda (x)
                                   (and (char= (compose-rep-op x) #\=)
                                        (eq (compose-rep-name x) buffer)))
                                 (first p2-s))
                        (production-bindings p2)))
    ((44) ;; a RHS + with a * that's not considered
     (check-consistency module (find-if (lambda (x)
                                          (and (char= (compose-rep-op x) #\+)
                                               (eq (compose-rep-name x) buffer)))
                                        (second p1-s))
                        (previous-production-bindings (compilation-module-previous module))
                        (find-if (lambda (x)
                                   (and (char= (compose-rep-op x) #\=)
                                        (eq (compose-rep-name x) buffer)))
                                 (first p2-s))
                        (production-bindings p2)))
    
    ((9) ;; a RHS =
     (check-consistency module (find-if (lambda (x)
                                          (and (char= (compose-rep-op x) #\=)
                                               (eq (compose-rep-name x) buffer)))
                                        (second p1-s))
                        (previous-production-bindings (compilation-module-previous module))
                        (find-if (lambda (x)
                                   (and (char= (compose-rep-op x) #\=)
                                        (eq (compose-rep-name x) buffer)))
                                 (first p2-s))
                        (production-bindings p2)))
    ((40) ;; a RHS *
     (check-consistency module (find-if (lambda (x)
                                          (and (char= (compose-rep-op x) #\*)
                                               (eq (compose-rep-name x) buffer)))
                                        (second p1-s))
                        (previous-production-bindings (compilation-module-previous module))
                        (find-if (lambda (x)
                                   (and (char= (compose-rep-op x) #\=)
                                        (eq (compose-rep-name x) buffer)))
                                 (first p2-s))
                        (production-bindings p2)))
    (t
     t)))


(defun NO-RHS-GOAL-REF (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Can't compile if the variable naming the buffer is used in the actions of p2"
  (declare (ignore p1 module p1-s p1-index p2 p2-index))
  (not (recursive-find (intern (concatenate 'string "=" (symbol-name buffer)))
                       (second p2-s))))


(defun NO-GOAL-HARVESTING (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (declare (ignore p1 p1-s p1-index p2 p2-s p2-index))
  (find buffer (compilation-module-no-harvest module)))

(defun G-B-C1 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (and (no-rhs-goal-ref buffer module p1 p1-s p1-index p2 p2-s p2-index) 
       (no-goal-harvesting buffer module p1 p1-s p1-index p2 p2-s p2-index)))

(defun goal-reason (p1-index p2-index failed-function)
  (if failed-function
      (cond ((eql failed-function 'no-rhs-goal-ref)
             "the buffer variable cannot be used in the actions of the second production if there is a request in the first production.")
            ((eql failed-function 'no-goal-harvesting)
             "the buffer is being strict harvested.")
            ((eql failed-function 'g-b-c1)
             "either the buffer is being strict harvested or the buffer variable is used in the actions of the second production.")
            (t
             (format nil "unknown reason ~s" failed-function)))
    (case p1-index
      ((2 6 10 11 14 15 42 43 46 47)
       "the buffer is explicitly cleared in the first production")
      ((16 18 20 22 24 25 26 27 28 29 30 31 56 57 58 60 59 61 62 63)
       "there are queries of the buffer in the first production")
      ((41 45)
       "the first production makes both a modification and a modification request")
      (t
       (case p2-index
         ((2 6 10 11 14 15 42 43 46 47)
          "the buffer is explicitly cleared in the second production")
         ((16 18 20 22 24 25 26 27 28 29 30 31 56 57 58 60 59 61 62 63)
          "there are queries of the buffer in the second production")
         ((41 45)
          "the second production makes both a modification and a modification request")
         (t
          (case p1-index
            ((4 12 13 44)
             "both productions make requests")
            (t
             "one production makes a modification and the other makes a modification request"))))))))



(define-compilation-type GOAL ((44 40 NO-RHS-GOAL-REF)
                               (44 9 NO-RHS-GOAL-REF) (44 8 G-B-C1)
                               (44 0 T) (40 44 T) (40 40 T) (40 13 T)
                               (40 12 T) (40 9 T)
                               (40 8 NO-GOAL-HARVESTING) (40 4 T)
                               (40 0 T) (13 40 NO-RHS-GOAL-REF)
                               (13 9 NO-RHS-GOAL-REF) (13 8 G-B-C1)
                               (13 0 T) (12 40 NO-RHS-GOAL-REF)
                               (12 9 NO-RHS-GOAL-REF) (12 8 G-B-C1)
                               (12 0 T) (9 13 T) (9 12 T) (9 9 T)
                               (9 8 NO-GOAL-HARVESTING) (9 4 T) (9 0 T)
                               (8 44 NO-GOAL-HARVESTING)
                               (8 40 NO-GOAL-HARVESTING)
                               (8 13 NO-GOAL-HARVESTING)
                               (8 12 NO-GOAL-HARVESTING)
                               (8 9 NO-GOAL-HARVESTING)
                               (8 8 NO-GOAL-HARVESTING) (8 4 T) (8 0 T)
                               (4 40 NO-RHS-GOAL-REF)
                               (4 9 NO-RHS-GOAL-REF) (4 8 G-B-C1)
                               (4 0 T) (0 44 T) (0 40 T) (0 13 T)
                               (0 12 T) (0 9 T) (0 8 T)
                               (0 4 T)) 
  (GOAL) MAP-GOAL-BUFFER COMPOSE-GOAL-BUFFER CHECK-GOAL-CONSISTENCY T NIL GOAL-REASON)


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
