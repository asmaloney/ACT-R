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
;;; Filename    : imaginal-compilation.lisp
;;; Version     : 6.0
;;; 
;;; Description : Production compilation IMAGINAL style definition.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;;
;;; ----- History -----
;;;
;;; 2010.12.10 Dan
;;;             : * Created automatically by build-compilation-type-file.
;;; 2010.12.13 Dan
;;;             : * Put the control functions and tests in place.
;;; 2010.12.15 Dan
;;;             : * Added module to the mapping functions args list.
;;; 2010.12.20 Dan
;;;             : * Added some additional code to handle things better when 
;;;             :   :ppm is enabled -- creates productions that match more closely
;;;             :   to the same conditions as the original pair did and does 
;;;             :   the "same" thing.
;;;             : * Allow subtypes to be composed in the consistency check.
;;; 2011.01.07 Dan
;;;             : * Fixed a bug with map-imaginal-buffer since it needs to be a
;;;             :   let* for ppm to be used.
;;; 2011.04.28 Dan
;;;             : * Added some declares to avoid compiler warnings.
;;; 2012.04.04 Dan [1.1]
;;;             : * Added the whynot reason function.
;;; 2014.05.07 Dan [2.0]
;;;             : * Start of conversion to typeless chunks.
;;;             : * Pass the module to constant-value-p.
;;;             : * References to compilation-module-previous are now using a
;;;             :   structure instead of list.
;;; 2016.08.04 Dan [3.0]
;;;             : * Updated the compilation type definition with one that was
;;;             :   created automatically by build-compilation-type-file from the
;;;             :   new spreadsheet that better handles "safe" compilation based
;;;             :   on whether the buffer is strict harvested or not.  Also added
;;;             :   some cases which should have been allowed but weren't like 13,40
;;;             :   where the * in the second can be combined with the + from the
;;;             :   first and 40,9 which can combine an = with a preceeding * (there
;;;             :   are some other similar additions but not listing all of them here).
;;;             :   Those new cases make things more consistent with respect to the
;;;             :   indicated constraints.
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
;;; 2020.11.05 Dan [4.1]
;;;             : * Drop buffer empty queries that occur in p2 if the buffer
;;;             :   was strict harvested from p1 otherwise you end up with a
;;;             :   production that can't fire since it tests for both there
;;;             :   being a chunk in the buffer and a query that it's empty.
;;; 2021.02.05 Dan [5.0]
;;;             : * Allowing the 9,8 28,8 9,24 25,24 cases to happen if the mod
;;;             :   in p1 is an empty mod when the buffer is strict harvested 
;;;             :   and then just drop the empty mod.
;;; 2021.02.10 Dan [6.0]
;;;             : * There isn't a pre-instantiation step for variablized slots
;;;             :   now so that needs to be done here instead for both mapping
;;;             :   and composition.
;;;             : * Needs to deal with var->var, const->var, var->const, and
;;;             :   the possibility of multiple vars being bound to the same
;;;             :   slot name.
;;; 2021.02.23 Dan
;;;             : * Wasn't catching the possible dynamic action to action
;;;             :   mappings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun MAP-IMAGINAL-BUFFER (buffer module p1 p1-s p1-index p2 p2-s p2-index)
 "map references from p1 to p2 for imaginal style buffer"
  ;; Possible cases: 
  ;;    a RHS + to a LHS = (includes when the RHS has both + and =)
  ;;    a RHS = to a LHS = 
  ;;    a LHS = with null RHS to a LHS =
  ;;    a RHS * to a LHS =
  (declare (ignore p1))
  (let* ((ppm (compilation-module-ppm module))
         (p1-bindings (previous-production-bindings (compilation-module-previous module)))
         (p2-bindings (production-compilation-instan (production-name p2)))
         (bindings (when ppm 
                     (append p1-bindings p2-bindings))))
    
    (cond (;; The RHS + to LHS = case
           (and (find p1-index '(4 12 13 20 28 29))
                (find p2-index '(8 9 12 13 24 25 28 29 40 56)))
           
           ;; Map the RHS +'s with the LHS ='s
           
           ;; here the slots of interest are just the intersection 
           ;; of the two sets
           ;;
           
           (let* ((mappings nil)
                  (original-p1-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s))))
                  (original-p2-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                  (original-p2-rhs-slots (compose-rep-slots (find-if (lambda (x) (and (eq buffer (compose-rep-name x))
                                                                                      (or (char= #\= (compose-rep-op x))
                                                                                          (char= #\* (compose-rep-op x)))))
                                                                     (second p2-s))))
                  (p1-var-slots (mapcar (lambda (x) (assoc (spec-slot-name x) p1-bindings)) (remove-if-not 'chunk-spec-variable-p original-p1-slots :key 'spec-slot-name)))
                  (p2-var-slots (mapcar (lambda (x) (assoc (spec-slot-name x) p2-bindings)) (remove-if-not 'chunk-spec-variable-p original-p2-slots :key 'spec-slot-name)))
                  (p2-rhs-var-slots (mapcar (lambda (x) (assoc (spec-slot-name x) p2-bindings)) (remove-if-not 'chunk-spec-variable-p original-p2-rhs-slots :key 'spec-slot-name)))
                  
                  (p1-slots (instantiate-slot-names
                             original-p1-slots
                             p1-bindings))
                  (p2-slots (instantiate-slot-names
                             original-p2-slots
                             p2-bindings))
                  (p2-rhs-slots (instantiate-slot-names
                                 original-p2-rhs-slots
                                 p2-bindings))
                  (interesting-slots (remove-duplicates (append (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p1-slots)
                                                                (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p2-slots)
                                                                (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p2-rhs-slots)))))
             
                          
             (dolist (slot interesting-slots)
               
               (when (or p1-var-slots p2-var-slots p2-rhs-var-slots) ;; there are dynamic slots which may need to be mapped
                                                                     ;; if the same slot is variablized in both or a var slot maps
                                                                     ;; to a constant slot in either direction (multiple options
                                                                     ;; possible as well as multiple variables matching the same slot)
                 
                 
                 (when (and (rassoc slot p1-var-slots)  ;; it's a variable in action and condition both
                            (rassoc slot p2-var-slots)) ;; so need to add all the variable to variable mappings  
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slots)))
                     (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-var-slots)))
                       (push (cons p1-var p2-var) mappings))))
                 
                 (when (and (rassoc slot p1-var-slots)  ;; it's a variable in both actions
                            (rassoc slot p2-rhs-var-slots)) ;; so need to add all the variable to variable mappings  
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slots)))
                     (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-rhs-var-slots)))
                       (push (cons p1-var p2-var) mappings))))
                 
                 
                 (when (and (rassoc slot p1-var-slots)  ;; it's a variable in p1 and const in p2 so need to instantiate
                            (find slot original-p2-slots :key 'spec-slot-name)) ;; all p1 vars
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slots)))
                     (push (cons p1-var slot) mappings)))
                 
                 (when (and (rassoc slot p1-var-slots)  ;; it's a variable in p1 and const in p2 action so need to instantiate
                            (find slot original-p2-rhs-slots :key 'spec-slot-name)) ;; all p1 vars
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slots)))
                     (push (cons p1-var slot) mappings)))
                 
                 (when (and (rassoc slot p2-var-slots)  ;; it's a variable in p2 and const in p1 so need to instantiate
                            (find slot original-p1-slots :key 'spec-slot-name)) ;; all p2 vars
                   (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-var-slots)))
                     (push (cons p2-var slot) mappings)))
                 
                 (when (and (rassoc slot p2-rhs-var-slots)  ;; it's a variable in p2 action and const in p1 so need to instantiate
                            (find slot original-p1-slots :key 'spec-slot-name)) ;; all p2 vars
                   (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-rhs-var-slots)))
                     (push (cons p2-var slot) mappings))))
               
               
                              
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
           (and (find p1-index '(9 25))
                (find p2-index '(8 9 12 13 24 25 28 29)))
           
           ;; Map the RHS ='s and LHS ='s not in the RHS with
           ;; the LHS ='s
           
           ;; Here the slots of interest are the union of the
           ;; p1 bits with the RHS superseding the LHS intersected
           ;; with the LHS of the second one
           
           
           (let* ((mappings nil)
                  (original-p1a-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s))))
                  (original-p1b-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s))))
                  
                  (original-p2-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                  (original-p2-rhs-slots (compose-rep-slots (find-if (lambda (x) (and (eq buffer (compose-rep-name x))
                                                                                      (or (char= #\= (compose-rep-op x))
                                                                                          (char= #\* (compose-rep-op x)))))
                                                                     (second p2-s))))
                  
                  (p1-var-slotsa (mapcar (lambda (x) (assoc (spec-slot-name x) p1-bindings)) (remove-if-not 'chunk-spec-variable-p original-p1a-slots :key 'spec-slot-name)))
                  (p1-var-slotsb (mapcar (lambda (x) (assoc (spec-slot-name x) p1-bindings)) (remove-if-not 'chunk-spec-variable-p original-p1b-slots :key 'spec-slot-name)))
                  (p2-var-slots (mapcar (lambda (x) (assoc (spec-slot-name x) p2-bindings)) (remove-if-not 'chunk-spec-variable-p original-p2-slots :key 'spec-slot-name)))
                  (p2-rhs-var-slots (mapcar (lambda (x) (assoc (spec-slot-name x) p2-bindings)) (remove-if-not 'chunk-spec-variable-p original-p2-rhs-slots :key 'spec-slot-name)))
                  
                  (p1-slotsa (instantiate-slot-names
                             original-p1a-slots
                             p1-bindings))
                  (p1-slotsb (instantiate-slot-names
                             original-p1b-slots
                             p1-bindings))
                  (p2-slots (instantiate-slot-names
                             original-p2-slots
                             p2-bindings))
                  (p2-rhs-slots (instantiate-slot-names
                                 original-p2-rhs-slots
                                 p2-bindings))
                  
                  (p1-slots (append (remove-if (lambda (x)
                                                   (or (not (eq (spec-slot-op x) '=))
                                                       (find (spec-slot-name x) p1-slotsb :key 'spec-slot-name)))
                                               p1-slotsa)
                                    p1-slotsb))
                  
                  (p1-var-slots (remove-duplicates (append p1-var-slotsa p1-var-slotsb) :test 'equalp))
                  
                  (interesting-slots (remove-duplicates (append (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p1-slots)
                                                                (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p2-slots)
                                                                (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p2-rhs-slots)))))
             
             (dolist (slot interesting-slots)
               
               (when (or p1-var-slots p2-var-slots p2-rhs-var-slots) ;; there are dynamic slots which may need to be mapped
                                                                     ;; if the same slot is variablized in both or a var slot maps
                                                                     ;; to a constant slot in either direction (multiple options
                                                                     ;; possible as well as multiple variables matching the same slot)
                 
                 
                 (when (and (rassoc slot p1-var-slots)  ;; it's a variable in both
                            (rassoc slot p2-var-slots)) ;; so need to add all the variable to variable mappings  
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slots)))
                     (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-var-slots)))
                       (push (cons p1-var p2-var) mappings))))
                 
                 (when (and (rassoc slot p1-var-slotsb)  ;; it's a variable in both actions
                            (rassoc slot p2-rhs-var-slots)) ;; so need to add all the variable to variable mappings  
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slotsb)))
                     (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-rhs-var-slots)))
                       (push (cons p1-var p2-var) mappings))))
                 
                 (when (and (rassoc slot p1-var-slots)  ;; it's a variable in p1 and const in p2 so need to instantiate
                            (find slot original-p2-slots :key 'spec-slot-name)) ;; all p1 vars
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slots)))
                     (push (cons p1-var slot) mappings)))
                 
                 (when (and (rassoc slot p1-var-slotsb)  ;; it's a variable in p1 and const in p2 action so need to instantiate
                            (find slot original-p2-rhs-slots :key 'spec-slot-name)) ;; all p1 vars
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slotsb)))
                     (push (cons p1-var slot) mappings)))
                 
                 (when (and (rassoc slot p2-var-slots)  ;; it's a variable in p2 and const in p1 (somewhere) so need to instantiate
                            (or                         ;; all p2 vars
                             (find slot original-p1a-slots :key 'spec-slot-name)
                             (find slot original-p1b-slots :key 'spec-slot-name))) 
                   (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-var-slots)))
                     (push (cons p2-var slot) mappings)))
                 
                 (when (and (rassoc slot p2-rhs-var-slots)  ;; it's a variable in p2 action and const in p1 so need to instantiate
                            (find slot original-p1b-slots :key 'spec-slot-name)) ;; all p2 vars
                   (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-rhs-var-slots)))
                     (push (cons p2-var slot) mappings))))
               
               
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
           (and (find p1-index '(40 56))
                (find p2-index '(8 9 24 25 40 56)))
           
           ;; Map the RHS *'s and LHS ='s not in the RHS with
           ;; the LHS ='s
           
           ;; Here the slots of interest are the union of the
           ;; p1 bits with the RHS superseding the LHS intersected
           ;; with the LHS of the second one
           
           
           (let* ((mappings nil)
                  (original-p1a-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s))))
                  (original-p1b-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\* (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s))))
                  
                  (original-p2-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                  (original-p2-rhs-slots (compose-rep-slots (find-if (lambda (x) (and (eq buffer (compose-rep-name x))
                                                                                      (or (char= #\= (compose-rep-op x))
                                                                                          (char= #\* (compose-rep-op x)))))
                                                                     (second p2-s))))
                  
                  (p1-var-slotsa (mapcar (lambda (x) (assoc (spec-slot-name x) p1-bindings)) (remove-if-not 'chunk-spec-variable-p original-p1a-slots :key 'spec-slot-name)))
                  (p1-var-slotsb (mapcar (lambda (x) (assoc (spec-slot-name x) p1-bindings)) (remove-if-not 'chunk-spec-variable-p original-p1b-slots :key 'spec-slot-name)))
                  (p2-var-slots (mapcar (lambda (x) (assoc (spec-slot-name x) p2-bindings)) (remove-if-not 'chunk-spec-variable-p original-p2-slots :key 'spec-slot-name)))
                  (p2-rhs-var-slots (mapcar (lambda (x) (assoc (spec-slot-name x) p2-bindings)) (remove-if-not 'chunk-spec-variable-p original-p2-rhs-slots :key 'spec-slot-name)))
                  
                  (p1-slotsa (instantiate-slot-names
                             original-p1a-slots
                             p1-bindings))
                  (p1-slotsb (instantiate-slot-names
                             original-p1b-slots
                             p1-bindings))
                  (p2-slots (instantiate-slot-names
                             original-p2-slots
                             p2-bindings))
                  (p2-rhs-slots (instantiate-slot-names
                                 original-p2-rhs-slots
                                 p2-bindings))
                  
                  (p1-slots (append (remove-if (lambda (x)
                                                   (or (not (eq (spec-slot-op x) '=))
                                                       (find (spec-slot-name x) p1-slotsb :key 'spec-slot-name)))
                                               p1-slotsa)
                                    p1-slotsb))
                  
                  (p1-var-slots (remove-duplicates (append p1-var-slotsa p1-var-slotsb) :test 'equalp))
                  
                  (interesting-slots (remove-duplicates (append (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p1-slots)
                                                                (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p2-slots)
                                                                (mapcan (lambda (x)
                                                                          (when (eq (spec-slot-op x) '=)
                                                                            (list (spec-slot-name x))))
                                                                  p2-rhs-slots)))))
             
             (dolist (slot interesting-slots)
               
               (when (or p1-var-slots p2-var-slots p2-rhs-var-slots) ;; there are dynamic slots which may need to be mapped
                                                                     ;; if the same slot is variablized in both or a var slot maps
                                                                     ;; to a constant slot in either direction (multiple options
                                                                     ;; possible as well as multiple variables matching the same slot)
                 
                 
                 (when (and (rassoc slot p1-var-slots)  ;; it's a variable in both
                            (rassoc slot p2-var-slots)) ;; so need to add all the variable to variable mappings  
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slots)))
                     (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-var-slots)))
                       (push (cons p1-var p2-var) mappings))))
                 
                 (when (and (rassoc slot p1-var-slotsb)  ;; it's a variable in both actions
                            (rassoc slot p2-rhs-var-slots)) ;; so need to add all the variable to variable mappings  
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slotsb)))
                     (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-rhs-var-slots)))
                       (push (cons p1-var p2-var) mappings))))
                 
                 (when (and (rassoc slot p1-var-slots)  ;; it's a variable in p1 and const in p2 so need to instantiate
                            (find slot original-p2-slots :key 'spec-slot-name)) ;; all p1 vars
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slots)))
                     (push (cons p1-var slot) mappings)))
                 
                 (when (and (rassoc slot p1-var-slotsb)  ;; it's a variable in p1 and const in p2 action so need to instantiate
                            (find slot original-p2-rhs-slots :key 'spec-slot-name)) ;; all p1 vars
                   (dolist (p1-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p1-var-slotsb)))
                     (push (cons p1-var slot) mappings)))
                 
                 (when (and (rassoc slot p2-var-slots)  ;; it's a variable in p2 and const in p1 (somewhere) so need to instantiate
                            (or                         ;; all p2 vars
                             (find slot original-p1a-slots :key 'spec-slot-name)
                             (find slot original-p1b-slots :key 'spec-slot-name))) 
                   (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-var-slots)))
                     (push (cons p2-var slot) mappings)))
                 
                 (when (and (rassoc slot p2-rhs-var-slots)  ;; it's a variable in p2 action and const in p1 so need to instantiate
                            (find slot original-p1b-slots :key 'spec-slot-name)) ;; all p2 vars
                   (dolist (p2-var (mapcar 'car (remove-if-not (lambda (x) (eq (cdr x) slot)) p2-rhs-var-slots)))
                     (push (cons p2-var slot) mappings))))
               
               
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
           (and (find p1-index '(8 24))
                (find p2-index '(8 9 12 13 24 25 28 29 40 56)))
           
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
                  (original-p1-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s))))
                  (original-p2-slots (compose-rep-slots (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s))))
                  
                  (p1-slots (instantiate-slot-names
                             original-p1-slots
                             p1-bindings))
                  (p2-slots (instantiate-slot-names
                             original-p2-slots
                             p2-bindings))
                  (interesting-slots (intersection (mapcan (lambda (x)
                                                             (when (eq (spec-slot-op x) '=)
                                                               (list (spec-slot-name x))))
                                                     p1-slots)
                                                   (mapcan (lambda (x)
                                                               (when (eq (spec-slot-op x) '=)
                                                                 (list (spec-slot-name x))))
                                                     p2-slots))))
             
             (dolist (slot interesting-slots)
               
               ;; no dynamic slot issues because can't make assumptions based on conditions alone
               
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


(defun COMPOSE-IMAGINAL-BUFFER (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  ;; Generally:
  ;;   If the first has a + (4, 12, 13, 20, 28 29) then
  ;;      the conditions are those of the first including a query
  ;;      the actions are the = or * of the first if there is one and
  ;;      the + will be the + of the first (can't be a + in the second)
  ;;      with the = or * of the second unioned in and overriding
  
  ;;   If the first has no actions (0, 8, 16 24)
  ;;      the buffer conditions are the union of those in the first
  ;;      and those of the second with the query from the first being
  ;;      used if there is one (16 24) otherwise the query from the second
  ;;      is used if there is one
  ;;      the actions are those of the second
  
  ;;   Otherwise (9 40 25 56)
  ;;      the conditions are the union of those in the first
  ;;      and those from the second that are not set by the 
  ;;      actions of the first If there is a query in the first it is
  ;;      used (25 56) otherwise a query from the second is used
  ;;      the actions are the = or * from the first with the = or * from
  ;;      the second unioned in and overriding and
  ;;      the + of the second if there is one
  ;;
  (declare (ignore p1))
  
  (let ((c1 (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s)))
        (c2 (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s)))
        (q1 (find-if (lambda (x) (and (char= #\? (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p1-s)))
        (q2 (find-if (lambda (x) (and (char= #\? (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (first p2-s)))
        
        (a1= (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s)))
        (a2= (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p2-s)))
        
        (a1+ (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s)))
        (a2+ (find-if (lambda (x) (and (char= #\+ (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p2-s)))
        (a1* (find-if (lambda (x) (and (char= #\* (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s)))
        (a2* (find-if (lambda (x) (and (char= #\* (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p2-s)))
        (bindings (append (previous-production-bindings (compilation-module-previous module)) 
                          (production-compilation-instan (production-name p2)))))
    
    (case p1-index
      ((4 12 13 20 28 29)
       (list (append 
              (when c1 
                (list c1)) 
              (when q1 
                (list q1)))
             (append 
              (when (or a1= a1*) ;; can't have both with current description
                (if a1= 
                    (list a1=) 
                  (list a1*))) 
              (cond ((and a1+ a2=)
                     (awhen (buffer+-union a1+ a2= bindings) 
                            (list it)))
                    ((and a1+ a2*)
                     (awhen (buffer+-union a1+ a2* bindings) 
                            (list it)))
                    (a1+
                     (list a1+))
                    (t 
                     nil)))))
      ((0 8 16 24)
       (list (append 
              (awhen (buffer-condition-union c1 c2 a1= bindings) 
                     (list it))  
              (if q1 
                  (list q1) 
                (if q2 
                    (if (find buffer (compilation-module-no-harvest module))
                        (list q2)
                      ;; strict harvested so need to ignore a buffer empty query from p2
                      (let ((slots (compose-rep-slots q2)))
                        (setf slots (remove '(= buffer empty) slots :test 'equalp))
                        (if slots
                            (list (list (first q2) slots)) ;; building a compose-rep explicitly because don't want to destructively modify
                          nil)))
                  nil)
                ))
             (append 
              (when a2= 
                (list a2=))
              (when a2* 
                (list a2*)) 
              (when a2+ 
                (list a2+)))))
      ((9 40 25 56)
       (list (append 
              (awhen (buffer-condition-union c1 c2 (if a1= a1= a1*) bindings) 
                     (list it))
              (if q1 
                  (list q1) 
                (if (and q2 ;; when it's a * followed by a state free query drop the query
                         (not (and (= p1-index 40) (or (= p2-index 24) (= p2-index 25) (= p2-index 56)))))
                    (list q2) 
                  nil)))
             (append (cond ((and (or (= p2-index 8) (= p2-index 24))
                                 (not (find buffer (compilation-module-no-harvest module))) ;; it is strict harvested
                                 (null (second a1=))) ;; the mod in a1 is a null mod
                            nil)
                           ((and a1* a2=)
                            (awhen (buffer=-union a1* a2= bindings)
                                   (list it)))
                           ((or a1= a2=) ;; if there's at least one = union those                             
                            (awhen (buffer=-union a1= a2= bindings) 
                                   (list it)))
                           ((or a1* a2*) ;; if there's at least one * union those
                            (awhen (buffer=-union a1* a2* bindings) 
                                   (list it)))
                           
                           (t nil)) ;; can't have other mix of = and * so just ignore 
                     (when a2+ 
                       (list a2+))))))))


(defun CHECK-IMAGINAL-CONSISTENCY (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (declare (ignore p1 p2-index))
  (case p1-index
    ((4 12 13 20 28 29) ;; a RHS +
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
       
    ((9 25) ;; a RHS =
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
    
    
    ((40 56) ;; a RHS *
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


(defun I-B-C3 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Compilation check queries in p2 for 'state free'"
  (declare (ignore module p1 p1-s p1-index p2 p2-index))
  ;; there can be only one
  (let ((query (find-if (lambda (x)
                          (and (char= (compose-rep-op x) #\?)
                               (eq (compose-rep-name x) buffer)))
                        (first p2-s))))
    (every (lambda (x)      
             (equalp x '(= state free)))
           (compose-rep-slots query))))


(defun I-B-C1 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
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

(defun NO-RHS-IMAGINAL-REF (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Can't compile if the variable naming the buffer is used in the actions of p2"
  (declare (ignore module p1 p1-s p1-index p2 p2-index))
  (not (recursive-find (intern (concatenate 'string "=" (symbol-name buffer)))
                       (second p2-s)))) 

(defun I-B-C4 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (and (i-b-c3 buffer module p1 p1-s p1-index p2 p2-s p2-index)
       (no-rhs-imaginal-ref buffer module p1 p1-s p1-index p2 p2-s p2-index)))

(defun I-B-C2 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
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

(defun I-B-C5 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "only if the buffer is not strict harvested"
  (declare (ignore p1 p1-s p1-index p2 p2-s p2-index))
  (find buffer (compilation-module-no-harvest module)))

(defun I-B-C6 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Not strict harvested and same queries"
  (and (i-b-c5 buffer module p1 p1-s p1-index p2 p2-s p2-index) 
       (i-b-c2 buffer module p1 p1-s p1-index p2 p2-s p2-index)))

(defun I-B-C7 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Not strict harvested and no RHS imaginal references"
  (and (i-b-c5 buffer module p1 p1-s p1-index p2 p2-s p2-index) 
       (no-rhs-imaginal-ref buffer module p1 p1-s p1-index p2 p2-s p2-index)))

(defun I-B-C8 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Not strict harvested and p2 query must be state free"
  (and (i-b-c5 buffer module p1 p1-s p1-index p2 p2-s p2-index) 
       (i-b-c3 buffer module p1 p1-s p1-index p2 p2-s p2-index)))

(defun I-B-C9 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  (declare (ignore p1 p1-index p2 p2-s p2-index))
  (or (find buffer (compilation-module-no-harvest module))
      (let ((mod (find-if (lambda (x) (and (char= #\= (compose-rep-op x)) (eq buffer (compose-rep-name x)))) (second p1-s))))
        (and mod (null (second mod))))))

(defun I-B-C10 (buffer module p1 p1-s p1-index p2 p2-s p2-index)
  "Not strict harvested and p2 query must be state free"
  (and (i-b-c2 buffer module p1 p1-s p1-index p2 p2-s p2-index) 
       (i-b-c9 buffer module p1 p1-s p1-index p2 p2-s p2-index)))


(defun imaginal-reason (p1-index p2-index failed-function)
  (cond ((eql failed-function 'no-rhs-imaginal-ref)
         "the buffer variable cannot be used in the actions of the second production if there is a request in the first production.")
        ((eql failed-function 'i-b-c1)
         "when the first production makes a request and the second does not harvest it the second can only query for state busy or buffer empty.")
        ((eql failed-function 'i-b-c2)
         "the queries in both productions must be the same.")
        ((eql failed-function 'i-b-c3)
         "when the first production makes a request and the second harvests it the second can only query for state free.")
        ((eql failed-function 'i-b-c4)
         "when the first production makes a request and the second harvests it the second can only query for state free and the buffer variable cannot be used in the actions of the second.")
        ((eql failed-function 'i-b-c5)
         "strict harvesting is enabled for the buffer and either the first production cleared the buffer or the combined production would leave the buffer with a chunk while the original pair would have left it empty.")
        ((eql failed-function 'i-b-c6)
         "either strict harvesting is enabled for the buffer and the combined production would leave the buffer with a chunk while the original pair would have left it empty or the queries in the productions are not the same.")
        ((eql failed-function 'i-b-c7)
         "either strict harvesting is enabled for the buffer and the combined production would leave the buffer with a chunk while the original pair would have left it empty or the buffer variable cannot be used in the actions of the second production if there is a request in the first production.")
        ((eql failed-function 'i-b-c8)
         "either strict harvesting is enabled for the buffer and the combined production would leave the buffer with a chunk while the original pair would have left it empty or the first production makes a request and the second harvests it so it can only query for state free.")
        
        (t 
         (case p1-index
           ((2 6 10 11 14 15 42 43 46 47 18 22 26 27 30 31 58 59 63 62)
            "the buffer is explicitly cleared in the first production.")
           ((44 45 60 61)
            "the first production makes both a request and a modification request.")
           ((41 57)
            "the first production makes both a modification and a modification request.")
           (t
            (case p2-index
              ((2 6 10 11 14 15 42 43 46 47 18 22 26 27 30 31 58 59 63 62)
               "the buffer is explicitly cleared in the second production.")
              ((44 45 60 61)
               "the second production makes both a request and a modification request.")
              ((41 57)
               "the second production makes both a modification and a modification request.")
              ((40 56)
               "the first production makes a modification and the second makes a modification request.")
              (t
               (case p1-index
                 ((4 12 13 20 28 29)
                  "both productions make requests.")
                 (t
                  (case p2-index
                    ((4 12 13 20 28 29)
                     "the first production makes a modification request and the second makes a request.")
                    (t
                     "the first production makes a modification request and the other makes a modification.")))))))))))

(define-compilation-type IMAGINAL ((56 56 I-B-C3)
                                   (56 40 T)
                                   (56 25 I-B-C3)
                                   (56 24 I-B-C8)
                                   (56 16 I-B-C1)
                                   (56 9 T)
                                   (56 8 I-B-C5)
                                   (56 0 T)
                                   (40 56 I-B-C3)
                                   (40 40 T)
                                   (40 25 I-B-C3)
                                   (40 24 I-B-C8)
                                   (40 16 I-B-C1)
                                   (40 9 T)
                                   (40 8 I-B-C5)
                                   (40 0 T)
                                   (29 56 I-B-C4)
                                   (29 40 NO-RHS-IMAGINAL-REF)
                                   (29 25 I-B-C3)
                                   (29 24 I-B-C8)
                                   (29 16 I-B-C1)
                                   (29 9 NO-RHS-IMAGINAL-REF)
                                   (29 8 I-B-C7)
                                   (29 0 T)
                                   (28 56 I-B-C4)
                                   (28 40 NO-RHS-IMAGINAL-REF)
                                   (28 25 I-B-C3)
                                   (28 24 I-B-C8)
                                   (28 16 I-B-C1)
                                   (28 9 NO-RHS-IMAGINAL-REF)
                                   (28 8 I-B-C7)
                                   (28 0 T)
                                   (25 29 I-B-C2)
                                   (25 28 I-B-C2)
                                   (25 25 I-B-C2)
                                   (25 24 I-B-C10)
                                   (25 20 I-B-C2)
                                   (25 16 I-B-C2)
                                   (25 13 T)
                                   (25 12 T)
                                   (25 9 T)
                                   (25 8 I-B-C9)
                                   (25 4 T)
                                   (25 0 T)
                                   (24 56 I-B-C6)
                                   (24 40 I-B-C6)
                                   (24 29 I-B-C6)
                                   (24 28 I-B-C6)
                                   (24 25 I-B-C6)
                                   (24 24 I-B-C6)
                                   (24 20 I-B-C2)
                                   (24 16 I-B-C2)
                                   (24 13 I-B-C5)
                                   (24 12 I-B-C5)
                                   (24 9 I-B-C5)
                                   (24 8 I-B-C5)
                                   (24 4 T)
                                   (24 0 T)
                                   (20 56 I-B-C4)
                                   (20 40 NO-RHS-IMAGINAL-REF)
                                   (20 25 I-B-C3)
                                   (20 24 I-B-C8)
                                   (20 16 I-B-C1)
                                   (20 9 NO-RHS-IMAGINAL-REF)
                                   (20 8 I-B-C7)
                                   (20 0 T)
                                   (16 56 I-B-C2)
                                   (16 40 T)
                                   (16 29 I-B-C2)
                                   (16 28 I-B-C2)
                                   (16 25 I-B-C2)
                                   (16 24 I-B-C2)
                                   (16 20 I-B-C2)
                                   (16 16 I-B-C2)
                                   (16 13 T)
                                   (16 12 T)
                                   (16 9 T)
                                   (16 8 T)
                                   (16 4 T)
                                   (16 0 T)
                                   (13 56 I-B-C4)
                                   (13 40 NO-RHS-IMAGINAL-REF)
                                   (13 25 I-B-C3)
                                   (13 24 I-B-C8)
                                   (13 16 I-B-C1)
                                   (13 9 NO-RHS-IMAGINAL-REF)
                                   (13 8 I-B-C7)
                                   (13 0 T)
                                   (12 56 I-B-C4)
                                   (12 40 NO-RHS-IMAGINAL-REF)
                                   (12 25 I-B-C3)
                                   (12 24 I-B-C8)
                                   (12 16 I-B-C1)
                                   (12 9 NO-RHS-IMAGINAL-REF)
                                   (12 8 I-B-C7)
                                   (12 0 T)
                                   (9 29 T)
                                   (9 28 T)
                                   (9 25 T)
                                   (9 24 I-B-C9)
                                   (9 20 T)
                                   (9 16 T)
                                   (9 13 T)
                                   (9 12 T)
                                   (9 9 T)
                                   (9 8 I-B-C9)
                                   (9 4 T)
                                   (9 0 T)
                                   (8 56 I-B-C5)
                                   (8 40 I-B-C5)
                                   (8 29 I-B-C5)
                                   (8 28 I-B-C5)
                                   (8 25 I-B-C5)
                                   (8 24 I-B-C5)
                                   (8 20 T)
                                   (8 16 T)
                                   (8 13 I-B-C5)
                                   (8 12 I-B-C5)
                                   (8 9 I-B-C5)
                                   (8 8 I-B-C5)
                                   (8 4 T)
                                   (8 0 T)
                                   (4 56 I-B-C4)
                                   (4 40 NO-RHS-IMAGINAL-REF)
                                   (4 25 I-B-C3)
                                   (4 24 I-B-C8)
                                   (4 16 I-B-C1)
                                   (4 9 NO-RHS-IMAGINAL-REF)
                                   (4 8 I-B-C7)
                                   (4 0 T)
                                   (0 56 T)
                                   (0 40 T)
                                   (0 29 T)
                                   (0 28 T)
                                   (0 25 T)
                                   (0 24 T)
                                   (0 20 T)
                                   (0 16 T)
                                   (0 13 T)
                                   (0 12 T)
                                   (0 9 T)
                                   (0 8 T)
                                   (0 4 T)) (IMAGINAL) MAP-IMAGINAL-BUFFER COMPOSE-IMAGINAL-BUFFER CHECK-IMAGINAL-CONSISTENCY T NIL IMAGINAL-REASON)

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
