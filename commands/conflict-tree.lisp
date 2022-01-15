;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2008 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : conflict-tree.lisp
;;; Version     : 3.0
;;; 
;;; Description : Code for creating and searching a simple decision tree of
;;;               production conditions.
;;; 
;;; Bugs        :
;;;
;;; To do       : [2.0] Consider handling conditions other than constants.
;;;             : [2.0] See if it can infer other things about non-equal or
;;;             :       numeric values in creating the tree.
;;;             : [-] Consider extending the tree when adding new productions
;;;             :     on the fly - track the used tests and call build-...
;;;             :     Seems like a bad idea for performance which is the 
;;;             :     objective of using the tree.
;;;             : [3.0] Make the negative limit a parameter somewhere.
;;; 
;;; ----- History -----
;;; 2008.12.23 Dan [1.0]
;;;            : * First complete version of the tree building/searching code
;;;            :   for use in production selection.
;;; 2008.01.07 Dan
;;;            : * Fixed print-conflict-tree.
;;; 2009.06.03 Dan
;;;            : * Added a hard limit to how many negative splits can occur
;;;            :   sequentially when building the tree (3).  If it gets that
;;;            :   far it just gives up on the branch and creates a leaf.
;;; 2010.04.26 Dan
;;;            : * Tree-condition-equal now uses eql for the value tests to
;;;            :   make sure numerical slot conditions can be considered
;;;            :   the same (doesn't affect the matching itself).
;;; 2011.04.28 Dan
;;;            : * Added some declares to quite compliation warnings.
;;; 2013.10.18 Dan
;;;            : * Added a few more stats to conflict-tree-stats. 
;;; 2014.03.17 Dan [2.0]
;;;            : * Changed the query-buffer call to be consistent with the new
;;;            :   internal code.
;;; 2014.04.03 Dan
;;;            : * Ignore the isa tests for now and instead use all the implicit
;;;            :   tests which were being ignored previously...
;;; 2014.05.19 Dan
;;;            : * To avoid warnings at load time commenting out the code from
;;;            :   the isa-node processing methods.
;;; 2014.05.30 Dan
;;;            : * Also commented out the isa condition in split-productions-with-condition.
;;; 2016.07.05 Dan [2.1]
;;;            : * Add-production-to-tree removes the isa conditions before 
;;;            :   passing things to add-to-tree.
;;;            : * When adding a new node for a new production being added check
;;;            :   the condition added at the leaf with the current productions 
;;;            :   at that leaf since previously it just pushed them to all 
;;;            :   branches.
;;; 2020.08.25 Dan
;;;            : * Moved the defstructs to the procedural module file.
;;; 2021.03.31 Dan [3.0]
;;;            : * Something broke this at some point.  Instead of just fixing
;;;            :   it, doing some significant updating.  Just remove all the isa 
;;;            :   conditions beforehand instead of no-oping them everywhere.
;;;            :   Also switching to the info gain for deciding which to use
;;;            :   which eliminates the need for the repeated 0 tests, but 
;;;            :   that requires the real calculations and to improve that it
;;;            :   groups the productions by the conditions they have so that
;;;            :   the calcualtions better measure how well a condition does at
;;;            :   splitting things (treating each production as its own class
;;;            :   as was done previously doesn't really produce a good metric
;;;            :   resulting in bigger trees than necessary and all the extra
;;;            :   checks to decide when to stop).  The downside is the added
;;;            :   computation for this calcuation...
;;;            : * Switched from the 'doesn't test' branch being marked with
;;;            :   :other to '=other since it's possible for someone to put the
;;;            :   keyword value into the productions but something which starts
;;;            :   with an = would never be a specific value since it's a
;;;            :   variable.
;;;            : * Don't try to extend the tree when new productions are added
;;;            :   for now (previously it just picked some remaining condition
;;;            :   and used that to split things).  May want to try using the
;;;            :   gain to decide to split a leaf, but would require keeping
;;;            :   some of the class grouping info to be efficient.
;;; 2021.04.15 Dan
;;;            : * Changed print-conflict-tree to output to the command trace.
;;; 2021.04.21 Dan
;;;            : * Fixed a place where :other hadn't been changed to '=other.
;;; 2021.05.10 Dan
;;;            : * Allow use with ppm by just dropping the slot tests - it's
;;;            :   just the query and test-slot cases.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Builds a tree from production conditions.  Initially uses all the productions
;;; in the model definition using the info. gain calculation to try and create a
;;; small tree.  Originally used the ID3 algorithm ofr that but switched to info.
;;; gain to alleviate some issues with detecting terminating conditions.  Not
;;; using the normalized info. gain from the C4.5 algorithm (the updated version
;;; of ID3) since that biases away from creating many specific classes which is
;;; something we want here -- if a single test uniquely splits things that's what
;;; we want instead of something that generalizes to more cases.
;;; If productions get added after the model is defined (through compilation or 
;;; otherwise) currently they just end up in an existing leaf.
;;;
;;; One assumption in the matching is that chunk names used in constant slot 
;;; tests (those hard coded into the productions) will always have thier true
;;; name.  It's not likely people have models where that's not the case, but
;;; it is technically possible to build such a model and it would not work
;;; correctly with the tree based matching.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; print-conflict-tree
;;; 
;;;     Has no parameters.  Can be called to print out the tree in the current
;;;     model's procedural module.  The tree is printed one node per line with
;;;     indenting and numbering indicating depth within a branch.
;;;
;;; conflict-tree-stats
;;;
;;;     Takes no parameters.  Will return a list of cons cells with some info
;;;     about the tree in the current procedural module.  The cons cells have
;;;     a description of the stat as the car and the value as the cdr.  The
;;;     current stats computed are:
;;;     :depth - deepest node in the tree
;;;     :min-depth - shortest branch in the tree
;;;     :total-nodes - count of all nodes (internal and terminal) in the tree
;;;     :terminal - count of only terminal nodes (leaves)
;;;     :non-empty - number of leaf nodes with 1 or more productions that
;;;                  are valid
;;;     :sets - the number of different sets of productions found in the
;;;             non-empty leaf nodes
;;;     :average-set - the mean size of non-empty sets
;;;     :largest-set - the size of the largest set
;;;
;;; > (conflict-tree-stats )
;;; ((:DEPTH . 24) (:MIN-DEPTH . 3) (:TOTAL-NODES . 4670) (:TERMINAL . 3422) (:NON-EMPTY . 3373) (:SETS . 1073) 
;;;  (:AVERAGE-SET . 4.746098) (:LARGEST-SET . 187))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Using the info gain from C4.5 now because the simple entropy from ID3 and
;;; assuming each production was its own classification had some problems with
;;; building a "good" tree.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)



;;; Find the valid productions given the tree and the procedural module
;;; returns the list of valid productions and a table with the conditions tested

(defmethod get-valid ((node conflict-node) prod )
  (get-valid (select-child node prod) prod ))

(defmethod get-valid ((node test-node) prod )
  (get-valid (select-child node prod) prod ))

(defmethod get-valid ((node leaf-node) prod )
  (declare (ignore prod))
  (conflict-node-valid node))

(defmethod get-valid ((node null) prod)
  (declare (ignore prod))
  node)


;;; pick the next node to check

(defmethod select-child ((node root-node) prod)
  (declare (ignore prod))
  (root-node-child node))


(defmethod select-child ((node test-slot-node) prod)
  (let ((buffer-val (cr-buffer-slot-read prod (test-slot-node-buffer node) (test-slot-node-buffer-index node) (test-slot-node-slot-index node) (test-slot-node-slot node))))
    (if (funcall (test-slot-node-test node) buffer-val (test-slot-node-value node))
        (test-slot-node-true node)
      (test-slot-node-false node))))


(defmethod select-child ((node query-node) prod)
  (declare (ignore prod))
  (if (query-buffer (query-node-buffer node) (list (query-node-query node) (query-node-value node)))
      (binary-test-node-true node)
    (binary-test-node-false node)))

(defmethod select-child ((node slot-node) prod)
  (let ((buffer-val (cr-buffer-slot-read prod (slot-node-buffer node) (slot-node-buffer-index node) (slot-node-slot-index node) (slot-node-slot node))))
    (aif (gethash buffer-val (slot-node-children node))
         it
         (gethash '=other (slot-node-children node)))))




;;; Print out the tree for debugging purposes

(defun print-conflict-tree ()
  (walk-tree (procedural-conflict-tree (get-module procedural))))

(defun walk-tree (node)
  (print-tree node nil 0 ))

(defun print-tree (node branch depth)
    
  (cond ((root-node-p node)
         (command-output "---->[root ~s]" (conflict-node-valid node))
         (awhen (root-node-child node)
                (print-tree it t (+ depth 2))))
        ((leaf-node-p node)
         (command-output "~vt-~d- ~s ->[leaf ~s]" depth (/ depth 2) branch (conflict-node-valid node))
         )
        ((binary-test-node-p node)
         (command-output "~vt-~d- ~s ->[~s ~a ~s ~@[~s ~]~s]" depth (/ depth 2) branch (cr-condition-type (test-node-condition node))
                         (test-node-buffer node)
                         (if (eq (cr-condition-type (test-node-condition node)) 'query)
                             (query-node-query node)
                           (test-slot-node-slot node))
                         (if (eq (cr-condition-type (test-node-condition node)) 'query)
                             nil
                           (test-slot-node-test node))
                         (test-node-value node))
         (awhen (binary-test-node-true node)
                (print-tree it t (+ depth 2)))
         (awhen (binary-test-node-false node)
                (print-tree it nil (+ depth 2))))
        ((wide-test-node-p node)
         (command-output "~vt-~d- ~s ->[~s ~a ~s]" depth (/ depth 2) branch (cr-condition-type (test-node-condition node)) (test-node-buffer node) (slot-node-slot node))
         (maphash (lambda (branch node)
                    (print-tree node branch (+ depth 2)))
                  (wide-test-node-children node)))))
   

;;; Compute tree statistics for comparison and analysis

(defvar *tree-data* nil)

(defun conflict-tree-stats ()
  (setf *tree-data* nil)
  (mapcar #'cons '(:depth :min-depth :total-nodes :terminal :non-empty :sets :average-set :largest-set) 
    (append (multiple-value-list (get-tree-stats (procedural-conflict-tree (get-module procedural))))
            (list (length *tree-data*)) (list (length (remove nil *tree-data*)))
            (list (length (remove-duplicates *tree-data* :test 'equalp)))
            (list (let ((nodes (remove nil *tree-data*)))
                    (unless (null nodes)
                      (* 1.0 (/ (reduce '+ (mapcar 'length nodes)) (length nodes))))))
            (list (reduce 'max (mapcar 'length *tree-data*))))))

(defmethod get-tree-stats ((node binary-test-node))
  (let ((max-depth 0)
        (min-depths nil)
        (nodes 1))
    (awhen (binary-test-node-true node)
           (multiple-value-bind (d md n)
               (get-tree-stats it)
             (incf nodes n)
             (setf min-depths md)
             (when (> d max-depth)
               (setf max-depth d))
             ))
    (awhen (binary-test-node-false node)
           (multiple-value-bind (d md n)
               (get-tree-stats it)
             (incf nodes n)
             (when (or (null min-depths) (< md min-depths))
               (setf min-depths md))
             (when (> d max-depth)
               (setf max-depth d))
             ))
    (values (1+ max-depth) (if min-depths (1+ min-depths) 1) nodes)))

(defmethod get-tree-stats ((node wide-test-node))
  (let ((max-depth 0)
        (min-depths nil)
        (nodes 1))
    (maphash (lambda (branch node)
               (declare (ignore branch))
               (multiple-value-bind (d md n)
                   (get-tree-stats node)
                 (incf nodes n)
                 (setf min-depths md)
                 (when (> d max-depth)
                   (setf max-depth d))))
             (wide-test-node-children node))
    (values (1+ max-depth) (if min-depths (1+ min-depths) 1) nodes)))

(defmethod get-tree-stats ((node root-node))
  (let ((max-depth 0)
        (min-depths nil)
        (nodes 1))
    (awhen (root-node-child node)
           (multiple-value-bind (d md n)
               (get-tree-stats it)
             (incf nodes n)
             (setf min-depths md)
             (when (> d max-depth)
               (setf max-depth d))
             ))
    (values (1+ max-depth) (if min-depths (1+ min-depths) 1) nodes)))


(defmethod get-tree-stats ((node leaf-node))
  (push (leaf-node-valid node) *tree-data*)
  (values 1 1 1))

#| don't generally build incrementally now so not updating this for the current representations 
   unless something changes to make it useful

(defun get-production-stats ()
  (let ((c nil))
    (dolist (p (productions-list (get-module procedural)))
      (setf c (append c (production-constants p))))
    (list (cons :total-constants (length c)) 
          (cons :unique-constants (length (remove-duplicates c :test 'equalp))) 
          (cons :unique-tests (length (remove-duplicates c :test (lambda (x y) (equal (butlast x) (butlast y)))))))))

(defun detail-adding-ps (ps)
  (dolist (x ps)
    (eval x)
  (format t "~s: ~s ~s~%" (second x) (get-production-stats) (conflict-tree-stats))))

|#


;;; Build the tree incrementally when a production is added.
;;;
;;; only happens after the initial tree creation - which should only be production compilation
;;; for most models.
;;; 
;;; Don't add any more nodes than necessary i.e. if it gets to a leaf add the first condition and stop
;;;
;;; Input: - current-tree 
;;;        - list of cr-condition structs valid for constants
;;;               so the type is one of: query, slot, or test-slot
;;;        - production name



         

(defmethod add-to-tree ((node leaf-node) conditions production)
  ; just storing in leaves for now
  ;(if conditions
  ;(create-branch node (list (car conditions)) production)
  (declare (ignore conditions))
  (push-last production (leaf-node-valid node)))


(defmethod add-to-tree ((node root-node) conditions production)
  
  (push-last production (conflict-node-valid node))
  
  (aif (root-node-child node)
       (add-to-tree it conditions production)
       nil
       ; Don't make a tree if there isn't one
       ;(let ((new-node (make-leaf-node :parent node :branch t :valid nil)))
       ;  (setf (root-node-child node) new-node)
       ;  (add-to-tree new-node conditions production))))
       ))

(defmethod add-to-tree ((node binary-test-node) conditions production) ;; query and test-slot
  
  (aif (find (binary-test-node-condition node) conditions :test 'cr-condition-equal)
       ;; this test matches so progress down the proper branch
       (add-to-tree (if (cr-condition-result it) 
                        (binary-test-node-true node) 
                      (binary-test-node-false node))
                    (remove it conditions) production)

        ;; otherwise add it to both branches
         (progn
           (add-to-tree (binary-test-node-true node) conditions production)
           (add-to-tree (binary-test-node-false node) conditions production))))


(defmethod add-to-tree ((node slot-node) conditions production) 

 (aif (find (slot-node-condition node) conditions :test 'cr-condition-equal)
      (if (gethash (cr-condition-value it) (slot-node-children node))
           ;; push it down the existing branch
          (add-to-tree (gethash (cr-condition-value it) (slot-node-children node)) (remove it conditions) production)
        
        ;; doesn't have a branch so create one then add it
        ;; copy the default node if there is once since those may have
        ;; a subtree already otherwise just create a leaf
        
        (let* ((default (gethash '=other (slot-node-children node)))
               (new-node (if default
                             (copy-conflict-tree default node)
                           (make-leaf-node :parent node))))
          
          (setf (gethash (cr-condition-value it) (slot-node-children node)) new-node)
          (setf (conflict-node-branch new-node) (cr-condition-value it))
          (add-to-tree new-node (remove it conditions) production)))
  
      ;; need to add it to every branch and if there's not a default
      ;; create one
      
      (let ((default nil))
        (maphash (lambda (key value)
                   (when (eq key '=other) (setf default t))
                   (add-to-tree value conditions production))
                 (slot-node-children node))
        (unless default
          (setf (gethash '=other (slot-node-children node))
            (make-leaf-node :parent node :branch '=other :valid (list production)))))))


#| Not going to extend the tree for now -- but may come back to this, and if so
   should use info gain to determine whether to branch or not but that may
   require keeping some more info in the leaves (the production class info with
   remaining conditions) to make it easy to do the calculations

(defmethod create-branch ((node leaf-node) conditions production)
  ;(format t "create-branch ~20s from ~20s~%" branch (type-of node))
  (let* ((condition (car conditions))
         (new-node (case (cr-condition-type condition)
                     
                     (query
                      (make-query-node :parent (conflict-node-parent node)
                                       :branch (conflict-node-branch node)
                                       :buffer (cr-condition-buffer condition)
                                       :query  (cr-condition-slot condition)
                                       :value (cr-condition-value condition)
                                       :condition condition))
                     (test-slot
                      (make-test-slot-node :parent (conflict-node-parent node)
                                           :branch (conflict-node-branch node)
                                           :buffer (cr-condition-buffer condition)
                                           :buffer-index (cr-condition-bi condition)
                                           :slot (cr-condition-slot condition)
                                           :slot-index (cr-condition-si condition)
                                           :value (cr-condition-value condition)
                                           :test (cr-condition-test condition)
                                           :condition condition))
                     (slot
                      (make-slot-node :parent (conflict-node-parent node)
                                      :branch (conflict-node-branch node)
                                      :buffer (cr-condition-buffer condition)
                                      :buffer-index (cr-condition-bi condition)
                                      :slot (cr-condition-slot condition)
                                      :slot-index (cr-condition-si condition)
                                      :condition condition)))))
    
    (if (binary-test-node-p new-node)
        (progn
          (setf (binary-test-node-true new-node) (make-leaf-node :parent new-node :branch t ))
          (setf (binary-test-node-false new-node) (make-leaf-node :parent new-node :branch nil ))
          )
      (progn
        (let ((other-tree (make-leaf-node :parent new-node :branch '=other)))
          (setf (gethash '=other (wide-test-node-children new-node)) other-tree))))
    
    
    ;; splice this in as the child of the leaf's parent

    (cond ((binary-test-node-p (conflict-node-parent node))
           (if (conflict-node-branch node)
               (setf (binary-test-node-true (conflict-node-parent node)) new-node)
             (setf (binary-test-node-false (conflict-node-parent node)) new-node)))
          ((root-node-p (conflict-node-parent node))
           (setf (root-node-child (conflict-node-parent node)) new-node))
          ((wide-test-node-p (conflict-node-parent node))
           (setf (gethash (conflict-node-branch new-node) (wide-test-node-children (conflict-node-parent node))) new-node)))
                               
    ;; now call the add code for this new-node
    
    (add-to-tree new-node conditions production)
    
    ;; then add the current node's valid productions based on whether they have the
    ;; same condition or not
    
    (dolist (p-name (conflict-node-valid node))
      
      (let* ((p (get-production p-name)) 
             (c (or (find (car conditions) (production-constants p) :test 'tree-condition-equal)
                    (find (car conditions) (production-implicit p) :test 'tree-condition-equal))))
        (add-to-tree new-node (when c (list c)) p-name)))))

|#

;;; Code to copy a tree making sure not to save things that shouldn't be saved

(defmethod copy-conflict-tree ((node leaf-node) parent)
  (let ((new-node (copy-conflict-node node)))
    (setf (conflict-node-parent new-node) parent)
    (setf (conflict-node-valid new-node) (copy-list (conflict-node-valid node)))
    new-node))


(defmethod copy-conflict-tree ((node root-node) parent)
  (let ((new-node (copy-conflict-node node)))
    (setf (conflict-node-parent new-node) parent)
    (setf (conflict-node-valid new-node) (copy-list (conflict-node-valid node)))
    (awhen (root-node-child node)
           (setf (root-node-child new-node) (copy-conflict-tree it new-node)))
    new-node))


(defmethod copy-conflict-tree ((node binary-test-node) parent)
  (let ((new-node (copy-conflict-node node)))
    (setf (conflict-node-parent new-node) parent)
    (setf (conflict-node-valid new-node) (copy-list (conflict-node-valid node)))
    (awhen (binary-test-node-true node)
           (setf (binary-test-node-true new-node) (copy-conflict-tree it new-node)))
    (awhen (binary-test-node-false node)
           (setf (binary-test-node-false new-node) (copy-conflict-tree it new-node)))
    new-node))

(defmethod copy-conflict-tree ((node wide-test-node) parent)
  (let ((new-node (copy-conflict-node node)))
    (setf (conflict-node-parent new-node) parent)
    (setf (conflict-node-valid new-node) (copy-list (conflict-node-valid node)))
    (setf (wide-test-node-children new-node) (make-hash-table :test 'equalp :size (hash-table-size (wide-test-node-children node))))
    (maphash (lambda (key value)
               (setf (gethash key (wide-test-node-children new-node)) (copy-conflict-tree value new-node)))
             (wide-test-node-children node))
    new-node))

;;;

(defun tree-condition-equal (a b)
  (and (eq (cr-condition-type a) (cr-condition-type b))
       (eq (cr-condition-buffer a) (cr-condition-buffer b))
       (case (cr-condition-type a)
         (slot (= (cr-condition-si a) (cr-condition-si b)))
         (query (and (eq (cr-condition-slot a) (cr-condition-slot b))
                     (eql (cr-condition-value a) (cr-condition-value b))))
         (test-slot (and (eq (cr-condition-test a) (cr-condition-test b))
                         (= (cr-condition-si a) (cr-condition-si b))
                         (eql (cr-condition-value a) (cr-condition-value b))))
         (t nil))))


;;; Sort the root nodes based on the cannonical production order so that
;;; using the tree should result in identical choices for 'tie' situations
;;; and can't just build them that way now with the class based grouping 

(defun production-ordering (productions procedural)
  (sort (copy-list productions) #'< :key (lambda (x) (position x (productions-list procedural) :key 'production-name))))

;;; Updated the scoring to use the info gain.  Also, now group the productions
;;; by their sets of conditions instead of treating each as a separate class 
;;; which should allow for a more efficient tree (at the cost of more initial
;;; calculations).

(defun tree-entropy (classes) 
  ;; s is a list of lists where each list is a 'class' of items and the car of the
  ;; list holds the items in that class.

  (let ((N (reduce '+ (mapcar (lambda (x) (length (car x))) classes) :initial-value 0)))
    (if (zerop N)
        0
      (reduce '+ (mapcar (lambda (x)  
                           (if (zerop (length (car x)))
                               0
                             (- (* (/ (length (car x)) N) (log (/ (length (car x)) N) 2)))))
                   classes)))))


(defun tree-gain (base-entropy sub-groupings)
  ;; the sub-groupings are based on 
  ;; the repartitioning of the sets based on a condition.
  ;; It's a list of lists of lists where the toplevel lists
  ;; are the possible values for the condition, the lists 
  ;; within that are the classes, and the first list in those
  ;; are the items.
  (let ((N (reduce '+ (mapcar (lambda (x) (reduce '+ (mapcar (lambda (y) (length (car y))) x) :initial-value 0)) sub-groupings) :initial-value 0)))
    (if (zerop N)
        0
      (- base-entropy 
         (reduce '+ (mapcar (lambda (x) 
                              (* (/ (reduce '+ (mapcar (lambda (y) (length (car y))) x)) N)
                                 (tree-entropy x)))
                      sub-groupings))))))

(defun split-productions-with-condition (condition n classes)
  
  ;; Need to compute the entropy for the condition
  ;; which means ignore the items without the condition
  
  (let (haves have-nots entropy)
    
    (dolist (x classes)
      (if (assoc n (second x))
          (push x haves)
        (push x have-nots)))
    
    (setf entropy (tree-entropy haves))
    
    ;(pprint classes)
    
    
    (if (eq 'slot (cr-condition-type condition))
        
        (let* ((vals (remove-duplicates (mapcar (lambda (x) (cdr (assoc n (second x)))) haves)))
               (results (mapcar (lambda (x)
                                  (cons x nil))
                          vals)))
          
          ;; split the items that have it to determine the gain
          
          (dolist (x haves)
            (let ((val (cdr (assoc n (second x))))
                  (without (list (first x) (remove n (second x) :key 'car))))
              (push without
                    (cdr (assoc val results)))))
          
          ;; update their classes
          
          (dolist (x results)
            (setf (cdr x)
              (group-by-condition-class (cdr x))))
          
          (let* ((new-classes (mapcar 'cdr results))
                 (gain (tree-gain entropy new-classes)))
            
            (when have-nots
              
              (dolist (y results)
                (setf (cdr y)
                  (append have-nots (cdr y))))
              
              (push (cons '=other have-nots) results)
            
              ;; update the classes again with the have-nots included
            
              (setf results (mapcar (lambda (x)
                                      (cons (car x)
                                            (group-by-condition-class (cdr x))))
                              results)))
            
            ;(pprint results)
            (values gain results)))
               
      ; test-slot and queries only have two options t or nil
      
      (let ((results (list (cons t nil) (cons nil nil))))
        
        ;; split the items that have it to determine the gain
          
          (dolist (x haves)
            (let ((val (cdr (assoc n (second x))))
                  (without (list (first x) (remove n (second x) :key 'car))))
              (push without
                    (cdr (if val
                             (first results)
                           (second results))))))
        
        ;; update their classes
        
        (dolist (x results)
          (setf (cdr x)
            (group-by-condition-class (cdr x))))
          
        (let* ((new-classes (mapcar 'cdr results))
               (gain (tree-gain entropy new-classes)))
            
          (when have-nots
            (dolist (y results)
              (setf (cdr y)
                (append have-nots (cdr y))))
            
              ;; update the classes again with the have-nots included
              
              (setf results (mapcar (lambda (x)
                                      (cons (car x)
                                            (group-by-condition-class (cdr x))))
                              results)))
            
            ;(pprint results)
            (values gain results))))))
    



(defun build-tree-from-productions (branch parent conditions classes procedural)
  
  (if (= (length classes) 1) ;; nothing to decide
      (make-leaf-node :parent parent :branch branch :valid (production-ordering (first (first classes)) procedural))

    ;; only consider the conditions that are actually used.
    ;; I thought limiting that to only those where every class has it
    ;; or there are classes with different values for the condition
    ;; would be useful, but it can find shorter trees if all of the
    ;; used conditions are tested though that's potentially a lot of
    ;; extra computation...
    
  
    (let ((used-conditions (remove-duplicates (mapcar 'car (reduce 'append (mapcar 'second classes))))))
          ;(used-conditions (reduce 'append (mapcar 'second classes)))
          ;(all-contain nil)
          ;(diff-vals nil)
          ;(n (length conditions))
          ;(class-count (length classes)))
    
    ;; find the interesting cases
    #|(dotimes (i n)
      (let ((match (remove-if-not (lambda (x) (= x i)) used-conditions :key 'car)))
        (when (= (length match) class-count)
          (push i all-contain))
        (when (> (length (remove-duplicates match :test 'equalp)) 1)
          (push i diff-vals))))
    
    (setf used-conditions (remove-duplicates (append diff-vals all-contain)))
    |#
   
    (if (null used-conditions) ;; nothing useful to split with
        (make-leaf-node :parent parent :branch branch :valid (production-ordering (apply 'append (mapcar 'first classes)) procedural))
      
      ;; Find the condition with the best info gain ration
      (let ((best nil)
            (val nil)
            (sub-nodes nil))
      
        (dolist (x used-conditions)
          (let ((condition (svref conditions x)))
          
            (multiple-value-bind (gain subs) (split-productions-with-condition condition x classes)
           
           ;(format t "splitting ~S~% with ~s~% scores ~S~%" classes condition gain)
           
           (when (or (null val)
                     (> gain val))
             (setf val gain)
             (setf sub-nodes subs)
             (setf best condition)))))
       
        ;(format t "Best: ~S ~S ~%~%" val sub-nodes)
      
      ;(break)
      ;; No relevant conditions left to split them
      
        (if (or (null val) (<= val 0.0)) ;;;  should it always stop if zero or negative now?
            
           (make-leaf-node :parent parent :branch branch :valid (production-ordering (flatten (mapcar 'first classes)) procedural))
       
      (let* (
             (new-node (case (cr-condition-type best)
                         (query
                          (make-query-node :parent parent
                                           :branch branch
                                           :buffer (cr-condition-buffer best)
                                           :query  (cr-condition-slot best)
                                           :value (cr-condition-value best)
                                           :condition best))
                         (test-slot
                          (make-test-slot-node :parent parent
                                               :branch branch
                                               :buffer (cr-condition-buffer best)
                                               :buffer-index (cr-condition-bi best)
                                               :slot (cr-condition-slot best)
                                               :slot-index (cr-condition-si best)
                                               :value (cr-condition-value best)
                                               :test (cr-condition-test best)
                                               :condition best))
                         (slot
                          (make-slot-node :parent parent
                                          :branch branch
                                          :buffer (cr-condition-buffer best)
                                          :buffer-index (cr-condition-bi best)
                                          :slot (cr-condition-slot best)
                                          :slot-index (cr-condition-si best)
                                          :condition best))))
             )
    
         (if (binary-test-node-p new-node)
             (progn
               (setf (binary-test-node-true new-node) 
                 (build-tree-from-productions t new-node conditions (cdr (assoc t sub-nodes)) procedural))
               (setf (binary-test-node-false new-node) 
                 (build-tree-from-productions nil new-node conditions (cdr (assoc nil sub-nodes)) procedural)))
           (progn
             (dolist (x sub-nodes)
               (setf (gethash (car x) (wide-test-node-children new-node))
                 (build-tree-from-productions (car x) new-node conditions (cdr x) procedural)))))
        new-node)))))))
    


(defun group-by-condition-class (productions)
  (let ((classes nil))
    (dolist (p productions classes)
      (aif (find (second p) classes :test 'equalp :key 'second)
           (setf (first it) (append (first p) (first it)))
           (push (list (first p) (second p)) classes)))))

;;; Interface to the procedural module

(defun build-conflict-tree (procedural)
  (let* ((productions-with-conditions (mapcar (lambda (x) 
                                                (list (list (production-name x))
                                                      (append (remove-if (lambda (x)
                                                                           (or (eq (cr-condition-type x) 'isa)
                                                                               (and
                                                                                (procedural-ppm procedural) ;; param lock held by calling fn
                                                                                (eq (cr-condition-type x) 'slot))))
                                                                         (copy-list (production-constants x)))
                                                              (copy-list (production-implicit x)))))
                                        (productions-list procedural)))
         (conditions (remove-duplicates (mapcan (lambda (x) (copy-list (second x))) productions-with-conditions) :test 'tree-condition-equal)))
    
    (setf conditions (coerce conditions 'vector))
    
    ;(pprint productions-with-conditions)
    
    (dolist (p productions-with-conditions)
      (setf (second p)
        (sort (mapcar (lambda (x)
                        (let ((n (position x conditions :test 'tree-condition-equal))
                              (val (if (eq 'slot (cr-condition-type x))
                                       (cr-condition-value x)
                                     (cr-condition-result x))))
                          (cons n val)))
                (second p))
              #'< :key 'car)))
    
    ;(pprint conditions))
    ;(pprint productions-with-conditions))
    
    
    (setf (root-node-conditions (procedural-conflict-tree procedural)) conditions)
    (setf (root-node-valid (procedural-conflict-tree procedural)) (mapcar 'production-name (productions-list procedural)))
    
    
    (setf (root-node-child (procedural-conflict-tree procedural))
      (build-tree-from-productions t 
                                   (procedural-conflict-tree procedural) 
                                   conditions
                                   (group-by-condition-class productions-with-conditions)
                                   procedural))))



(defun add-production-to-tree (p procedural)
  (add-to-tree (procedural-conflict-tree procedural) 
               (remove-if (lambda (x)
                            (or (eq (cr-condition-type x) 'isa)
                                (and
                                 (procedural-ppm procedural) ;; param lock held by calling fn
                                 (eq (cr-condition-type x) 'slot))))
                          (remove-duplicates (append (production-constants p) (production-implicit p)) :test 'cr-condition-equal))
               (production-name p)))



(defun remove-production-from-tree (p procedural)
  (declare (ignore p procedural))
  (model-warning "Removing productions not supported when :use-tree is set to t - tree removal not implemented."))


(defun get-valid-productions (procedural)
  (get-valid (procedural-conflict-tree procedural) procedural))


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
