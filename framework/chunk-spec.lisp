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
;;; Filename    : chunk-spec.lisp
;;; Version     : 3.0
;;; 
;;; Description : Definition of chunk specifications and corresponding functions
;;; 
;;; Bugs        : 
;;;
;;; To do       : * Finish the documentation.
;;;             : * Investigate optimizations after there's some use.
;;;             : * Add a function to check chunk-specs to make module writing
;;;             :   easier.
;;; ----- History -----
;;;
;;; 2004.09.02 Dan
;;;             : Creation.
;;; 2004.12.29 Dan
;;;             : Realized that the comparitors are backwards with respect
;;;             : to productions in test-chunk-slots.
;;; 2005.02.03 Dan
;;;             : * Changing the internal slot-value-lists of a chunk to be a
;;;             :   hash-table instead of an alist...
;;; 2005.02.09 Dan
;;;             : * Some minor cleanup - changing member to find where possible.
;;; 2005.04.19 Dan
;;;             : * Added pprint-chunk-spec.
;;; 2005.05.16 Dan
;;;             : * Modified chunk-spec-variable-p to test that the name has
;;;             :   a length > 1 to reject the symbol itself as a variable.
;;;             :   That fixes a minor problem in production parsing and I
;;;             :   don't think it breaks anything else.
;;; 2005.09.09 Dan
;;;             : * Renamed chunk-to-chunk-spec chunk-name-to-chunk-spec to
;;;             :   clarify its use because I introduced a bug somewhere
;;;             :   along the line with its usage that didn't actually affect
;;;             :   any existing modules, but may cause problems for other
;;;             :   module writers.
;;;             : * Also fixed chunk-name-to-chunk-spec because it didn't 
;;;             :   include nil slots in the spec, but it probably should (it
;;;             :   did prior to my "fixing" it when I changed over to hash
;;;             :   tables).
;;; 2005.11.17 Dan
;;;             : * Fixed chunk-name-to-chunk-spec because it incorrectly
;;;             :   referenced the internal chunk-type slot list instead of
;;;             :   using ct-slot-names.
;;; 2006.09.11 Dan
;;;             : * Changed chunk-slot-equal so that it uses equalp instead of
;;;             :   equal when the string and chunk checks fall through because
;;;             :   numbers (which is the typical value that'd fall through)
;;;             :   don't test well with equal...
;;; 2007.06.18 Dan
;;;             : * Added slot-specs-to-chunk-spec-list and chunk-spec-to-chunk-def
;;;             :   as "official" commands now and also added slot-specs-to-chunk-spec
;;;             :   which removes the need to call define-chunk-spec after slot-specs-
;;;             :   to-chunk-spec-list.
;;; 2007.07.02 Dan
;;;             : * Changed PROCESS-SLOTS-SPECS so that it keeps the slot-spec
;;;             :   in the order provided during definition.
;;; 2007.08.07 Dan
;;;             : * Fixed pprint-chunk-spec so that it actually has the : on the
;;;             :   front of the request parameters.
;;; 2008.06.16 Dan
;;;             : * Changed test-chunk-slots so that it always fails if there
;;;             :   are unbound variables even when they're used in other 
;;;             :   tests.  Doesn't change the normal operation, just shuts
;;;             :   down the possibility of "odd" chunk-specs doing unusual
;;;             :   matches that result in a true result.
;;; 2008.06.17 Dan
;;;             : * Changed test-chunk-slots so that it ignores request parameters
;;;             :   in the chunk-spec for matching purposes.
;;;             : * Added tests for current model and mp to match-chunk-spec-p.
;;; 2008.06.18 Dan
;;;             : * Added a test to find-matching-chunks to verify that the 
;;;             :   chunks parameter is a list when it's not :all.
;;;             : * Fixed the warning in chunk-spec-slot-spec to name the right
;;;             :   function.
;;; 2008.06.20 Dan
;;;             : * Changed chunk-spec-to-chunk-def so that tests to make sure
;;;             :   the parameter is a chunk-spec.
;;;             : * Changed chunk-spec-to-chunk-def so that it also rejects a
;;;             :   spec with request parameters.
;;;             : * Removed the macros of slot-specs-to-chunk-spec-list and
;;;             :   slot-specs-to-chunk-spec and renamed the functions to not
;;;             :   have the -fct since none of the other chunk-spec manipulation
;;;             :   commands use that convention and they aren't the types of
;;;             :   things that would be entered 'directly' and need a macro
;;;             :   form.
;;; 2008.07.31 Dan
;;;             : * Took chunk-slot-equal out of here and put it into the chunks
;;;             :   file.
;;; 2008.10.30 Dan
;;;             : * Performance improvement for test-chunk-slots - fewer loops
;;;             :   over the spec/slots.
;;; 2013.01.28 Dan
;;;             : * Changed the internal storing of the chunk-type to the name
;;;             :   since that's what got used anyway and changed process-slots-specs
;;;             :   to call valid-chunk-type-slot instead (which is the command
;;;             :   it really should have been using anyway) and similarly don't
;;;             :   use the internal chunk/chunk-type accessors in chunk-name-to-chunk-spec.
;;; 2013.02.14 Dan
;;;             : * Changed strip-request-parameters-from-chunk-spec so that it
;;;             :   maintains the ordering of the slots and removed an unneeded
;;;             :   cond from chunk-spec-slot-spec.
;;; 2013.03.13 Dan [1.1]
;;;             : * Changed process-slots-specs so that it allows a parent type
;;;             :   to specify slots of child types in chunk-specs.
;;;             : * Changed test-chunk-slots so that it requires that a chunk
;;;             :   have the specified slot as well as matching its value --
;;;             :   not having the slot is not the same as having the slot with
;;;             :   a value of nil.
;;; 2013.03.18 Dan 
;;;             : * Fixed some order dependence issues in test-chunk-slots.
;;; 2013.06.07 Dan
;;;             : * Changed test-chunk-slots so that it now requires a parameter
;;;             :   indicating whether the chunk is static or not because now a
;;;             :   static chunk which doesn't have a particular slot will match
;;;             :   to a spec which says "= <that slot> nil" because for statics
;;;             :   not having a slot is the same as having a slot with an explict
;;;             :   value of nil.
;;; 2014.02.14 Dan
;;;             : * Fixed a bug in test-chunk-slots with how it tests variables
;;;             :   between slots that was broken with the 13/3/13 change.
;;; 2014.03.03 Dan [2.0]
;;;             : * Conversion to the no chunk-types at run time mechanism.  A
;;;             :   chunk-spec now holds two vectors of slot info (filled and empty)
;;;             :   instead of a type symbol.
;;;             : * Chunk-spec-chunk-type returns nil and prints a warning.
;;;             : * Chunk-spec-filled-slots and chunk-spec-empty-slots return 
;;;             :   the corresponding vectors for comparison purposes.
;;;             : * The isa is optional when defining a chunk-spec but if provided 
;;;             :   starts with the default spec for the type, but overwrites 
;;;             :   any default values with those provided in the spec.
;;; 2014.03.14 Dan
;;;             : * Treat request parameters like slots in a chunk-spec, but not
;;;             :   in chunks.
;;; 2014.03.19 Dan
;;;             : * Allow a chunk-spec to have variablized slot names, add an
;;;             :   instantiate-chunk-spec command, and require that matching a
;;;             :   chunk-spec only uses the "instantiated" slot specs.
;;;             : * Remove the warning when a chunk-spec-slot-spec gets a slot
;;;             :   that's not specified to avoid the need for pre-checking the
;;;             :   slots everywhere that's used, and do the same for the 
;;;             :   slot-in-chunk-spec-p function as well.
;;; 2014.03.20 Dan
;;;             : * Let define-chunk-spec-fct extend chunks automatically with
;;;             :   optional parameters that indicate if it can and if it should
;;;             :   print a warning when it does.
;;;             : * Now returns 2 values: the spec and a list of slots that were
;;;             :   added by extension.
;;; 2014.03.26 Dan
;;;             : * Don't allow modifiers for request parameters i.e. they must
;;;             :   have an = modifier.
;;; 2014.03.27 Dan
;;;             : * Added merge-chunk-specs command.
;;; 2014.03.28 Dan
;;;             : * Instantiate-chunk-spec now returns two values like define-
;;;             :   chunk-spec.
;;; 2014.04.01 Dan
;;;             : * Added the functions for comparing a chunk/slot-vector to
;;;             :   the filled and empty specs so I don't need to do the bit
;;;             :   tests directly everywhere.
;;; 2014.04.07 Dan
;;;             : * Moved chunk-spec-variable-p to misc-utils.
;;; 2014.05.08 Dan
;;;             : * Adding the structure spec-slot to allow for more cleanly
;;;             :   accessing the components of a slot specification.  However,
;;;             :   they are still represented as lists this just provides 
;;;             :   accessors to treat the list as a struct.
;;; 2014.05.19 Dan
;;;             : * Putting slot-specs-to-chunk-spec back in because vision and
;;;             :   audio can use such a function.
;;; 2014.06.09 Dan
;;;             : * Changed a warning so that it matches the older version.
;;; 2014.06.11 Dan
;;;             : * Fixed some logic in add-slot-spec-to-chunk-spec so that the
;;;             :   backward compatibility flag properly indicates how to deal
;;;             :   with a - slot <val> condition in a chunk-spec.
;;; 2014.06.12 Dan
;;;             : * Reevaluated the logic in how matching nil slots should work
;;;             :   under various conditions and decided that it should be the
;;;             :   same with or without the backward compat. flag because we
;;;             :   don't want to capture the "odd" behavior of 6.0 rejecting
;;;             :   a non-existant slot.
;;; 2014.06.24 Dan
;;;             : * Allow the definition of a chunk-spec which uses an isa and
;;;             :   slots which don't belong to that type, but print a hard
;;;             :   warning.
;;; 2014.09.05 Dan
;;;             : * Add extra tests to process-slots-specs when trying to extend
;;;             :   with a new slot because with allowing 'bad' slots as above
;;;             :   it has to be sure bad values are at least valid as a slot 
;;;             :   name first.
;;; 2014.11.04 Dan
;;;             : * Added model and meta-process checks to pprint-chunk-spec.
;;;             : * Starting to fix issues with the match and find code to deal
;;;             :   with the fact that variables are now allowed in the slot name
;;;             :   positions of chunk-specs.  Isn't a problem for the current
;;;             :   code that uses chunk-specs since conflict resolution doesn't 
;;;             :   rely on it and things like visicon searches which use vars
;;;             :   only do so in the value spot, but it should do the "right"
;;;             :   thing for all usage.
;;; 2014.11.05 Dan
;;;             : * Match-chunk-spec-p now works for arbitrary depth slot name
;;;             :   bindings when using the default variable character.
;;; 2014.11.06 Dan
;;;             : * Match-chunk-spec-p works fully with other variable characters
;;;             :   though it can be tough to take advantage of that since slot
;;;             :   names need to be valid during the definition of the chunk-spec.
;;;             : * Changed slot-in-chunk-spec-p to return t when it's a varaible
;;;             :   now too.
;;;             : * Added the slot-spec-* accessors to go along with spec-slot
;;;             :   since they better match the description in the manual and are
;;;             :   safer since they don't throw an error if non-list provided.
;;;             : * Fixed chunk-spec-to-chunk-def to fail if there are variablized
;;;             :   slots in the chunk-spec.
;;;             : * Safety check added to verify-single-explicit-value.
;;; 2014.12.16 Dan
;;;             : * Don't use the list structure trick to define the spec-slot-*
;;;             :   accessors since not all Lisps like that when passed nil.
;;;             :   Instead just make it functions calling first, second, and
;;;             :   third with no error checking (also was true for the struct
;;;             :   accessors) along with corresponding setf forms.
;;; 2015.08.19 Dan
;;;             : * Added the chunk-difference-to-chunk-spec function which 
;;;             :   returns a chunk-spec with the modifications necessary to
;;;             :   turn the second chunk into a copy of the first.  Needed by
;;;             :   visual tracking because with nil slots not being part of a
;;;             :   chunk now chunk-name-to-chunk-spec isn't sufficient if the
;;;             :   first chunk doesn't have a slot the second.  May be useful
;;;             :   in other places as well.
;;; 2017.06.20 Dan
;;;             : * Locking access to the model's chunk table for find-matching-
;;;             :   chunks.
;;; 2017.06.21 Dan
;;;             : * Lock a chunk when accessing its internal slots.
;;; 2017.08.09 Dan
;;;             : * Added the printed-chunk-spec command to provide an alternative
;;;             :   for capture-model-output which doesn't work in an RPC world.
;;; 2017.09.28 Dan
;;;             : * Added an optional parameter to printed-chunk-spec to have it
;;;             :   printed to a 'flat' string instead of embedding the new lines
;;;             :   and production spacing.
;;; 2017.12.06 Dan [3.0]
;;;             : * Adding a component to hold a table of chunk-specs since a
;;;             :   remote call can't use the internal struct.  Instead cache
;;;             :   the struct in the table and return a # as the chunk-spec-id
;;;             :   instead.  Need to provide a 'release' mechanism too since
;;;             :   otherwise the table could get large before a reset/clearall
;;;             :   can flush it.
;;; 2018.06.06 Dan
;;;             : * Added a remote define-chunk-spec-fct.
;;;             : * Chunk-spec-to-id tests that it's actually given a chunk-spec.
;;;             : * Don't throw an error for bad remote define-chunk-spec calls.
;;;             :   Just return nil since there'll already be a warning.
;;;             : * Added a remote chunk-name-to-chunk-spec and pprint-chunk-spec.
;;; 2018.06.07 Dan
;;;             : * Added a remote match-chunk-spec-p, allow commands for the
;;;             :   test functions, and do a little more parameter checking.
;;;             : * Added remote find-matching-chunks, chunk-spec-slots, slot-in-
;;;             :   chunk-spec-p, chunk-spec-slot-spec, slot-spec-modifier,
;;;             :   slot-spec-slot, slot-spec-value.
;;; 2018.06.08 Dan
;;;             : * Added a remote chunk-spec-variable-p here since the
;;;             :   original is in misc-utils which gets loaded before the
;;;             :   dispatcher.
;;;             : * Verify-single-explicit-value now always returns 2 values 
;;;             :   since that's safer for use remotely, and added a remote
;;;             :   command for it.
;;;             : * Added remote chunk-difference-to-chunk-spec and printed-
;;;             :   chunk-spec.
;;; 2018.06.11 Dan
;;;             : * Fixed a bug with chunk-spec-slots introduced with that last
;;;             :   update.
;;; 2018.06.12 Dan
;;;             : * Work to make sure things are consistent with respect to when
;;;             :   special string handling is used.
;;;             : * Use the new process-options-list function from the dispatcher
;;;             :   to handle remote 'keyword-like' parameters and update the doc
;;;             :   strings accordingly.
;;; 2018.06.14 Dan
;;;             : * Fixed a bug when there's an alternate variable char to match-
;;;             :   chunk-spec-p.
;;; 2019.01.30 Dan
;;;             : * Fixed some copy and paste issues with warnings and removed
;;;             :   all the verify-current-meta-process checks.
;;;             : * Use a separate list of testable chunk-specs instead of doing
;;;             :   a check on a testable flag.
;;; 2019.02.13 Dan
;;;             : * Add a comment with an alternate test for slots-vector-match-
;;;             :   signature.
;;; 2019.03.06 Dan
;;;             : * Adjust how act-r-chunk-slot-value-lists is used since its
;;;             :   first elements are slot structs.
;;; 2019.04.04 Dan
;;;             : * Take advantage of slot-struct being returned by tests to
;;;             :   simplify some code.
;;;             : * Instantiating a chunk-spec always calls replace-variables
;;;             :   on the value because it could be a list which wouldn't have
;;;             :   been flagged as a variable.
;;;             : * Put slot names into a slot of the struct so for chunk-spec-
;;;             :   slots to use instead of recomputing.
;;; 2019.04.18 Dan
;;;             : * Fix a bug with creating a chunk-spec where nil is used in a
;;;             :   slot name position.
;;; 2020.08.25 Dan
;;;             : * Fixed a bug with chunk-spec-slots when it was given an id
;;;             :   instead of an actual chunk-spec.
;;; 2021.07.15 Dan
;;;             : * Fixed a typo in a warning in verify-single-explicit-value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The structures are not for external use.
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
;;; Find-matching-chunks automatically ignores request parameters now so they
;;; don't have to be stripped out first.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; Create a component to provide a way to map a number (id) to a chunk-spec
;;; since the structure can't be passed out through the remote interface.

(defstruct chunk-spec-component (table (make-hash-table)) (number 0) (lock (bt:make-lock "chunk-spec-component")))

(defun clear-chunk-spec-table (cs-component)
  (bt:with-lock-held ((chunk-spec-component-lock cs-component))
    (clrhash (chunk-spec-component-table cs-component))))

(define-component :chunk-spec :version "3.0" :documentation "Maintains a table of chunk-specs for remote use"
  :creation make-chunk-spec-component :clear-all clear-chunk-spec-table :after-reset clear-chunk-spec-table)

;;; Commands for working with the chunk-spec ids

(defun chunk-spec-to-id (chunk-spec)
  (when (act-r-chunk-spec-p chunk-spec)
    (let ((c (get-component :chunk-spec)))
      (bt:with-lock-held ((chunk-spec-component-lock c))
        (let ((n (incf (chunk-spec-component-number c))))
          (setf (gethash n (chunk-spec-component-table c)) chunk-spec)
          n)))))

(defun id-to-chunk-spec (chunk-spec-id)
  (let ((c (get-component :chunk-spec)))
    (bt:with-lock-held ((chunk-spec-component-lock c))
      (gethash chunk-spec-id (chunk-spec-component-table c)))))


(defun release-chunk-spec-id (chunk-spec-id)
  (let ((c (get-component :chunk-spec)))
    (bt:with-lock-held ((chunk-spec-component-lock c))
      (remhash chunk-spec-id (chunk-spec-component-table c)))))


(add-act-r-command "release-chunk-spec-id" 'release-chunk-spec-id "Release the chunk-spec associated with the provided id. Params: chunk-spec-id." nil)



;;; Internal accessors for the parts of a chunk-spec slot specification.
;;;
;;; Can't do this (defstruct (spec-slot (:type list)) op name value)
;;; safely in all Lisps. So just make them simple macros instead.

(defun spec-slot-op (spec) (first spec))
(defun set-spec-slot-op (spec value) (setf (first spec) value))
(defsetf spec-slot-op set-spec-slot-op)

(defun spec-slot-name (spec) (second spec))
(defun set-spec-slot-name (spec value) (setf (second spec) value))
(defsetf spec-slot-name set-spec-slot-name)

(defun spec-slot-value (spec) (third spec))
(defun set-spec-slot-value (spec value) (setf (third spec) value))
(defsetf spec-slot-value set-spec-slot-value)


;;; User level accessors.

(defun slot-spec-modifier (slot-spec-list)
  (if (and (listp slot-spec-list) (= 3 (length slot-spec-list)))
      (first slot-spec-list)
    (print-warning "Invalid slot-spec list ~s passed to slot-spec-modifier." slot-spec-list)))
  
(defun slot-spec-slot (slot-spec-list)
  (if (and (listp slot-spec-list) (= 3 (length slot-spec-list)))
      (second slot-spec-list)
    (print-warning "Invalid slot-spec list ~s passed to slot-spec-slot." slot-spec-list)))

(defun slot-spec-value (slot-spec-list)
  (if (and (listp slot-spec-list) (= 3 (length slot-spec-list)))
      (third slot-spec-list)
    (print-warning "Invalid slot-spec list ~s passed to slot-spec-value." slot-spec-list)))

(add-act-r-command "slot-spec-modifier" 'slot-spec-modifier "Return the modifier from a slot-spec list (the first item). Params: slot-spec-list.")
(add-act-r-command "slot-spec-slot" 'slot-spec-slot "Return the slot name from a slot-spec list (the second item). Params: slot-spec-list.")
(add-act-r-command "slot-spec-value" 'slot-spec-value "Return the value from a slot-spec list (the third item). Params: slot-spec-list.")

;;; The rest of the real commands

(defmacro define-chunk-spec (&rest specifications)
  `(define-chunk-spec-fct ',specifications))

(defun define-chunk-spec-fct (specifications-list &optional (extend t) (warn t))
   (verify-current-model
    "define-chunk-spec-fct called with no current model."
    (cond ((= (length specifications-list) 1)
           (if (get-chunk (car specifications-list))
               (chunk-name-to-chunk-spec (car specifications-list))
             (print-warning "define-chunk-spec's 1 parameter doesn't name a chunk: ~S" specifications-list)))
          ((eq (car specifications-list) 'isa)
           (let ((chunk-type (get-chunk-type (second specifications-list))))
             (if chunk-type
                 (process-slots-specs chunk-type (cddr specifications-list) extend warn)
               (print-warning "Element after isa in define-chunk-spec isn't a chunk-type. ~S" specifications-list))))
          (t
           (process-slots-specs nil specifications-list extend warn)))))



(defun remote-define-chunk-spec (&rest spec)
  (let ((cs (define-chunk-spec-fct (decode-string-names spec))))
    (when cs
      (chunk-spec-to-id cs))))

(defun remote-define-chunk-spec-fct (spec &optional (extend t) (warn t))
  (let ((cs (define-chunk-spec-fct (decode-string-names spec) extend warn)))
    (when cs
      (chunk-spec-to-id cs))))

(add-act-r-command "define-chunk-spec" 'remote-define-chunk-spec "Create a chunk-spec and return its id. Params: '{{mod} slot value}'*." nil)
(add-act-r-command "define-chunk-spec-fct" 'remote-define-chunk-spec-fct "Create a chunk-spec and return its id. Params: '({{mod} slot value}*)' {extend {warn}}." nil)


(defmacro define-query-spec (&rest specifications)
  `(define-query-spec-fct ',specifications))

(defun define-query-spec-fct (specifications-list)
  (verify-current-model
   "define-query-spec-fct called with no current model."
   (process-query-specs specifications-list)))

(defun chunk-name-to-chunk-spec (chunk-name)
  (let ((spec (make-act-r-chunk-spec))
        (chunk (get-chunk chunk-name)))
    (if chunk
        (bt:with-recursive-lock-held ((act-r-chunk-lock chunk))
          (let ((slot-vector (act-r-chunk-filled-slots chunk)))
            (setf (act-r-chunk-spec-filled-slots spec) slot-vector)
            (setf (act-r-chunk-spec-equal-slots spec) slot-vector)
            (dolist (slot (act-r-chunk-slot-value-lists chunk) spec)
              (let ((s (make-act-r-slot-spec :name (act-r-slot-name (car slot)) :value (cdr slot))))
                (pushnew (act-r-slot-name (car slot)) (act-r-chunk-spec-slot-names spec))
                (push s (act-r-chunk-spec-slots spec))
                (push s (act-r-chunk-spec-testable-slots spec))))))
      (print-warning "Chunk-name-to-chunk-spec called with a non-chunk ~s." chunk-name))))


(defun remote-chunk-name-to-chunk-spec (chunk-name)
  (let ((cs (chunk-name-to-chunk-spec (string->name chunk-name))))
    (when cs
      (chunk-spec-to-id cs))))

(add-act-r-command "chunk-name-to-chunk-spec" 'remote-chunk-name-to-chunk-spec "Given the name of a chunk return a chunk-spec id of a chunk-spec that contains a specification of all the slots and values of that chunk. Params: chunk-name.")


(defun chunk-difference-to-chunk-spec (chunk-name1 chunk-name2)
  (let ((spec (make-act-r-chunk-spec))
        (chunk1 (get-chunk chunk-name1))
        (chunk2 (get-chunk chunk-name2)))
    (if (and chunk1 chunk2)
        (bt:with-recursive-lock-held ((act-r-chunk-lock chunk1))
          (bt:with-recursive-lock-held ((act-r-chunk-lock chunk2))
            (let* ((slot-vector (act-r-chunk-filled-slots chunk1))
                   (slots-in-c2 (act-r-chunk-filled-slots chunk2))
                   (slots-to-remove-from-c2 (logandc2 slots-in-c2 slot-vector)))
              (setf (act-r-chunk-spec-filled-slots spec) slot-vector)
              (setf (act-r-chunk-spec-empty-slots spec) slots-to-remove-from-c2)
              (setf (act-r-chunk-spec-equal-slots spec) (logior slot-vector slots-to-remove-from-c2))
              (dolist (slot (act-r-chunk-slot-value-lists chunk1))
                
                (let ((s (make-act-r-slot-spec :name (act-r-slot-name (car slot)) :value (cdr slot))))
                  (pushnew (act-r-slot-name (car slot)) (act-r-chunk-spec-slot-names spec))
                  (push s (act-r-chunk-spec-slots spec))
                  (push s (act-r-chunk-spec-testable-slots spec))))
              (dolist (slot (slot-mask->names slots-to-remove-from-c2))
                
                (let ((s (make-act-r-slot-spec :name slot :value nil)))
                  (pushnew slot (act-r-chunk-spec-slot-names spec))
                  (push s (act-r-chunk-spec-slots spec))
                  (push s (act-r-chunk-spec-testable-slots spec))))
              spec)))
      (print-warning "Chunk-difference-to-chunk-spec called with ~s and ~s which are not both chunks." chunk-name1 chunk-name2))))

(defun remote-chunk-difference-to-chunk-spec (c1 c2)
  (aif (chunk-difference-to-chunk-spec (string->name c1) (string->name c2))
       (chunk-spec-to-id it)))

(add-act-r-command "chunk-difference-to-chunk-spec" 'remote-chunk-difference-to-chunk-spec "Create a chunk-spec that could be used to modify chunk-2 to be identical to chunk-1. Params: chunk-1 chunk-2")


(defun chunk-spec-filled-slots (chunk-spec)
  (if (act-r-chunk-spec-p chunk-spec)
      (act-r-chunk-spec-filled-slots chunk-spec)
    (print-warning "Chunk-spec-filled-slots called with a non-chunk-spec")))

(defun chunk-spec-empty-slots (chunk-spec)
  (if (act-r-chunk-spec-p chunk-spec)
      (act-r-chunk-spec-empty-slots chunk-spec)
    (print-warning "Chunk-spec-empty-slots called with a non-chunk-spec")))



(defun chunk-spec-slots (chunk-spec)
  (if (act-r-chunk-spec-p chunk-spec)
      (act-r-chunk-spec-slot-names chunk-spec) ;(remove-duplicates (mapcar 'act-r-slot-spec-name (act-r-chunk-spec-slots chunk-spec)))
    (aif (id-to-chunk-spec chunk-spec)
         (act-r-chunk-spec-slot-names it) ;(remove-duplicates (mapcar 'act-r-slot-spec-name (act-r-chunk-spec-slots it)))
         (print-warning "Chunk-spec-slots called with something other than a chunk-spec or chunk-spec-id"))))


(add-act-r-command "chunk-spec-slots" 'chunk-spec-slots "Return a list of slots and request-parameters that are found in the given chunk-spec. Params: chunk-spec.")
  
(defun chunk-spec-slot-spec (chunk-spec &optional slot)
  (if (act-r-chunk-spec-p chunk-spec)
      (let ((result nil))
        (dolist (x (act-r-chunk-spec-slots chunk-spec) result)
          (when (or (null slot) (eq slot (act-r-slot-spec-name x)))
            (push-last (slot-spec-to-list x) result))))
    (aif (id-to-chunk-spec chunk-spec)
         (chunk-spec-slot-spec it slot)
         (print-warning "Chunk-spec-slot-spec was not passed a valid chunk-spec or chunk-spec id"))))

(defun remote-chunk-spec-slot-spec (chunk-spec &optional slot)
  (encode-string-names (chunk-spec-slot-spec chunk-spec (string->name slot))))

(add-act-r-command "chunk-spec-slot-spec" 'remote-chunk-spec-slot-spec "Return a list of the slot spec lists from a chunk-spec for the indicated slot or all slots if none provided. Params: chunk-spec {slot}.")

(defun slot-in-chunk-spec-p (chunk-spec slot)
  (if (act-r-chunk-spec-p chunk-spec)
      (aif (valid-slot-name slot)
          (let ((index (act-r-slot-index it)))
            (and (or (logbitp index (act-r-chunk-spec-filled-slots chunk-spec))
                     (logbitp index (act-r-chunk-spec-empty-slots chunk-spec)))
                 t))
           (when (find slot (act-r-chunk-spec-slots chunk-spec) :key 'act-r-slot-spec-name)
             t))
    (aif (id-to-chunk-spec chunk-spec)
         (slot-in-chunk-spec-p it slot)
         (print-warning "Slot-in-chunk-spec-p called with something other than a chunk-spec or chunk-spec-id"))))

(defun external-slot-in-chunk-spec-p (chunk-spec slot)
  (slot-in-chunk-spec-p chunk-spec (string->name slot)))

(add-act-r-command "slot-in-chunk-spec-p" 'external-slot-in-chunk-spec-p "Test whether a given slot is used in the indicated chunk-spec. Params: chunk-spec slot.")
  
(defun slot-spec-to-list (slot-spec)
  (list (act-r-slot-spec-modifier slot-spec)
        (act-r-slot-spec-name slot-spec)
        (act-r-slot-spec-value slot-spec)))
         
(defun process-slots-specs (chunk-type specs extend warn)
  (let ((spec (make-act-r-chunk-spec))
        (defaults nil)
        (extended? nil))
    
    (when chunk-type
      (let ((default-spec (act-r-chunk-type-initial-spec chunk-type)))
        (setf defaults (act-r-chunk-spec-slots default-spec))))
    
    (loop 
      (when (null specs)
        (dolist (x defaults)
          (let ((index (slot-name->index (act-r-slot-spec-name x))))
            ;; If the default slot is already tested for full or empty
            ;; skip the default value
            (unless (or (logbitp index (act-r-chunk-spec-filled-slots spec))
                        (logbitp index (act-r-chunk-spec-empty-slots spec)))
              (add-slot-spec-to-chunk-spec x spec))))
        (return (values spec extended?)))
      
      (let ((slot-spec (make-act-r-slot-spec))
            (slot-struct nil))
        
        (when (find (car specs) '(= - > < >= <=))
          (setf (act-r-slot-spec-modifier slot-spec) (pop specs)))
        
        (when (null specs)
          (print-warning "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return nil))
        
        (let ((slot? (car specs)))
          (when (null slot?)
            (print-warning "Cannot use nil as a slot name.")
            (return nil))
          (when (and (keywordp slot?) (not (eq (act-r-slot-spec-modifier slot-spec) '=)))
            (print-warning "Request parameters may not use a modifier other than =.")
            (return nil))
          (setf slot-struct (if chunk-type (valid-ct-slot chunk-type slot?) (valid-slot-name slot?)))
          (unless (or slot-struct (chunk-spec-variable-p slot?))
            (if (and extend (not (keywordp slot?)) ;; unnecessary since the alphanumericp catches this too  (not (chunk-spec-variable-p slot?))
                     (symbolp slot?) (alphanumericp (char (symbol-name slot?) 0)))
                (let ((extended (extend-possible-slots slot? nil)))
                  (if extended
                      (progn
                        (push extended extended?)
                        (setf slot-struct (valid-slot-name slot?))
                        (when warn
                          (model-warning "Chunks extended with slot ~s during a chunk-spec definition." slot?)))
                    (if chunk-type
                        (progn
                          (print-warning "Slot ~s invalid for type ~s but chunk-spec definition still created." slot? (act-r-chunk-type-name chunk-type))
                          (push (list :mismatch (act-r-chunk-type-name chunk-type) slot?) extended?))
                      (progn
                        (print-warning "Invalid slot-name ~s in call to define-chunk-spec." slot?)
                        (return nil)))))
              (progn
                (print-warning "Invalid slot-name ~S in call to define-chunk-spec." slot?)
                (return nil)))))
        
        (let ((slot (pop specs)))
          
          (setf (act-r-slot-spec-name slot-spec) slot)
          
          (when (chunk-spec-variable-p slot)
            (setf (act-r-slot-spec-variable slot-spec) :slot)))
        
        (when (null specs)
          (print-warning "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return nil))
        
        (let ((value (pop specs)))
          (setf (act-r-slot-spec-value slot-spec) value)
          
          (when (chunk-spec-variable-p value)
            (if (act-r-slot-spec-variable slot-spec)
                (setf (act-r-slot-spec-variable slot-spec) :both)
              (setf (act-r-slot-spec-variable slot-spec) :value))))
        
        (unless (add-slot-spec-to-chunk-spec slot-spec spec slot-struct)
          (return nil))))))


(defun process-query-specs (specs)
  (let ((spec (make-act-r-chunk-spec)))
    (loop 
      (when (null specs)
        (return (values spec nil)))
      
      (let ((slot-spec (make-act-r-slot-spec)))
        
        (if (find (car specs) '(= -))
            (setf (act-r-slot-spec-modifier slot-spec) (pop specs))
          (if (find (car specs) '(> < >= <=))
              (progn
                (print-warning "Query specs only allow = or - modifiers.")
                (return nil))))
        
        (when (null specs)
          (print-warning "Invalid specs in call to define-query-spec - not enough arguments")
          (return nil))
        
        (unless (valid-query-name (car specs)) 
          (print-warning "Invalid query name ~S in call to define-query-spec." (car specs))
          (return nil))
        
        (setf (act-r-slot-spec-name slot-spec) (pop specs))
        (pushnew (act-r-slot-spec-name slot-spec) (act-r-chunk-spec-slot-names spec))
        
        (when (null specs)
          (print-warning "Invalid specs in call to define-query-spec - not enough arguments")
          (return nil))
        
        (setf (act-r-slot-spec-value slot-spec) (pop specs))
        
        (when (chunk-spec-variable-p (act-r-slot-spec-value slot-spec))
          (setf (act-r-slot-spec-variable slot-spec) :value)
          (pushnew (act-r-slot-spec-value slot-spec) (act-r-chunk-spec-variables spec)))
        
        (push-last slot-spec (act-r-chunk-spec-slots spec))))))


(defun add-slot-spec-to-chunk-spec (slot-spec spec &optional slot-struct)
  
  (push-last slot-spec (act-r-chunk-spec-slots spec))
  
  (pushnew (act-r-slot-spec-name slot-spec) (act-r-chunk-spec-slot-names spec))
  
  (case (act-r-slot-spec-variable slot-spec)
    (:slot
     (pushnew (act-r-slot-spec-name slot-spec) (act-r-chunk-spec-variables spec))
     (pushnew (act-r-slot-spec-name slot-spec) (act-r-chunk-spec-slot-vars spec))
     (push slot-spec (act-r-chunk-spec-testable-slots spec))
     (return-from add-slot-spec-to-chunk-spec t))
    (:both
     (pushnew (act-r-slot-spec-value slot-spec) (act-r-chunk-spec-variables spec))
     (pushnew (act-r-slot-spec-name slot-spec) (act-r-chunk-spec-variables spec))
     (pushnew (act-r-slot-spec-name slot-spec) (act-r-chunk-spec-slot-vars spec))
     (push slot-spec (act-r-chunk-spec-testable-slots spec))
     
     (aif (assoc (act-r-slot-spec-value slot-spec) (act-r-chunk-spec-dependencies spec))
          (pushnew (act-r-slot-spec-name slot-spec) (cdr it))
          (push (list (act-r-slot-spec-value slot-spec) (act-r-slot-spec-name slot-spec)) (act-r-chunk-spec-dependencies spec)))
     (return-from add-slot-spec-to-chunk-spec t))
    
    (:value
     (pushnew (act-r-slot-spec-value slot-spec) (act-r-chunk-spec-variables spec))))
  
  (let* ((mod (act-r-slot-spec-modifier slot-spec))
         (slot-name (act-r-slot-spec-name slot-spec))
         (val (act-r-slot-spec-value slot-spec))
         (struct (aif slot-struct it (valid-slot-name slot-name)))
         (mask (act-r-slot-mask struct))
         (index (act-r-slot-index struct)))
    
    (when (keywordp slot-name)
      (setf (act-r-chunk-spec-request-param-slots spec) (logior mask (act-r-chunk-spec-request-param-slots spec))))
    
    (when (or (logbitp index (act-r-chunk-spec-filled-slots spec))
              (logbitp index (act-r-chunk-spec-empty-slots spec)))
      (setf (act-r-chunk-spec-duplicate-slots spec) (logior mask (act-r-chunk-spec-duplicate-slots spec))))
    
    (case mod
      (= (setf (act-r-chunk-spec-equal-slots spec) (logior mask (act-r-chunk-spec-equal-slots spec))))
      (- (setf (act-r-chunk-spec-negated-slots spec) (logior mask (act-r-chunk-spec-negated-slots spec))))
      (t (setf (act-r-chunk-spec-relative-slots spec) (logior mask (act-r-chunk-spec-relative-slots spec)))))
    
    (unless (keywordp slot-name)
      (if val
          ;; anything that's not nil means the slot filled bit gets set
          ;; and we need the test
          (progn
            (push slot-spec (act-r-chunk-spec-testable-slots spec))
            (unless (eq mod '-)
              (setf (act-r-chunk-spec-filled-slots spec) (logior mask (act-r-chunk-spec-filled-slots spec)))))
        ;; for nil things are a little more interesting and we don't need the tests
        (cond ((eq mod '=)
               (setf (act-r-chunk-spec-empty-slots spec) (logior mask (act-r-chunk-spec-empty-slots spec))))
              ((eq mod '-)
               (setf (act-r-chunk-spec-filled-slots spec) (logior mask (act-r-chunk-spec-filled-slots spec))))
              (t
               (return-from add-slot-spec-to-chunk-spec
                 (print-warning "A ~s test for slot ~s with a value of nil not allowed in chunk-spec." mod slot-name))))))
    t))
  
  
(defun instantiate-chunk-spec (chunk-spec bindings &optional (extend t))
  (let ((extended-slots nil))
    (cond ((not (act-r-chunk-spec-p chunk-spec))
           (print-warning "instantiate-chunk-spec called with something other than a chunk-spec: ~s" chunk-spec))
          ((not (and (listp bindings) (every (lambda (x)
                                               (and (consp x) (chunk-spec-variable-p (car x)) (cdr x)))
                                             bindings)))
           (print-warning "Instantiate-chunk-spec called with an invalid bindings alist: ~s" bindings))
          (t
           (let ((new-spec (make-act-r-chunk-spec)))
             (dolist (x (act-r-chunk-spec-slots chunk-spec))
               (let ((it (act-r-slot-spec-variable x)))
                 (let* ((slot (if (eq it :value) 
                                  (act-r-slot-spec-name x)
                                (aif (assoc (act-r-slot-spec-name x) bindings)
                                     (cdr it)
                                     (act-r-slot-spec-name x))))
                        (value (replace-variables (act-r-slot-spec-value x) bindings)))
                   
                   (unless (or (chunk-spec-variable-p slot)
                               (valid-slot-name slot))
                     (if extend
                         (let ((extended (if (keywordp slot) nil (extend-possible-slots slot))))
                           (if extended
                               (push slot extended-slots)
                             (progn
                               (print-warning "Invalid slot-name ~s in instantiation of chunk spec for variable ~s" slot (act-r-slot-spec-name x))
                               (return-from instantiate-chunk-spec nil))))
                       (progn
                         (print-warning "Invalid slot-name ~s in instantiation of chunk spec for variable ~s" slot (act-r-slot-spec-name x))
                         (return-from instantiate-chunk-spec nil))))
                   
                   (add-slot-spec-to-chunk-spec (make-act-r-slot-spec :modifier (act-r-slot-spec-modifier x)
                                                                      :name slot
                                                                      :value value
                                                                      :variable (cond ((and (chunk-spec-variable-p slot)
                                                                                            (chunk-spec-variable-p value))
                                                                                       :both)
                                                                                      ((chunk-spec-variable-p slot)
                                                                                       :slot)
                                                                                      ((chunk-spec-variable-p value)
                                                                                       :value)
                                                                                      (t nil)))
                                                new-spec))))
           (values new-spec extended-slots))))))


(defun instantiate-query-spec (chunk-spec bindings)
  (cond ((not (act-r-chunk-spec-p chunk-spec))
         (print-warning "instantiate-query-spec called with something other than a chunk-spec: ~s" chunk-spec))
        ((not (and (listp bindings) (every (lambda (x)
                                             (and (consp x) (chunk-spec-variable-p (car x)) (cdr x)))
                                           bindings)))
         (print-warning "Instantiate-query-spec called with an invalid bindings alist: ~s" bindings))
        (t
         (let ((new-spec (make-act-r-chunk-spec)))
           (dolist (x (act-r-chunk-spec-slots chunk-spec))
             (let ((value (replace-variables (act-r-slot-spec-value x) bindings)))
               (pushnew (act-r-slot-spec-name x) (act-r-chunk-spec-slot-names new-spec))
               (push-last (make-act-r-slot-spec :modifier (act-r-slot-spec-modifier x)
                                                    :name (act-r-slot-spec-name x)
                                                    :value value
                                                    :variable (if (chunk-spec-variable-p value) :value nil))
                              (act-r-chunk-spec-slots new-spec))))
           (values new-spec nil)))))

;;; A specific value of nil may be important to some things, so that's why it
;;; returns a second value of t on success.

(defun verify-single-explicit-value (chunk-spec slot module cmd &optional (var-char #\=))
  (if (act-r-chunk-spec-p chunk-spec)
      (let ((index (slot-name->index slot)))
        (cond ((null index) 
               (values nil nil))
              ((not (or (logbitp index (act-r-chunk-spec-filled-slots chunk-spec))
                        (logbitp index (act-r-chunk-spec-empty-slots chunk-spec))
                        (logbitp index (act-r-chunk-spec-request-param-slots chunk-spec))))
               (print-warning "~a command to ~s module requires a value for the ~a slot." cmd module slot)
               (values nil nil))
              ((logbitp index (act-r-chunk-spec-duplicate-slots chunk-spec))
               (print-warning "~a slot may only be specified once in a ~a command to the ~s module." slot cmd module)
               (values nil nil))
              ((not (logbitp index (act-r-chunk-spec-equal-slots chunk-spec)))
               (print-warning "~a slot may only have the = modifier in a ~a command to the ~s module." slot cmd module)
               (values nil nil))
              (t
               (let* ((slot-spec (find slot (act-r-chunk-spec-slots chunk-spec) :key 'act-r-slot-spec-name))
                      (value (when slot-spec
                               (act-r-slot-spec-value slot-spec)))
                      (var (if (and (stringp var-char) (> (length var-char) 0)) (char var-char 0) var-char)))
                 
                 (if (and value (characterp var) (symbolp value) (char-equal var (char (symbol-name value) 0)))
                     (values nil (print-warning "~a slot must be explicit - not a variable in a ~a command to the ~s module." slot cmd module))
                   (values value t))))))
    (aif (id-to-chunk-spec chunk-spec)
         (verify-single-explicit-value it slot module cmd var-char)
         (values nil (print-warning "~a is not a chunk-spec in ~a command to the ~s module." chunk-spec cmd module)))))

(defun remote-verify-single-explicit-value (chunk-spec slot module cmd &optional (var-char #\=))
  (multiple-value-bind (val exist)
      (verify-single-explicit-value chunk-spec (string->name slot) module cmd var-char)
    (values (encode-string-names val) exist))) 

(add-act-r-command "verify-single-explicit-value" 'remote-verify-single-explicit-value "Determine if a slot in a chunk-spec has exactly one value specified and if so return it.  Params: chunk-spec slot module-name command-name {variable-indicator}.")


(defun chunk-match-signature (chunk-name filled &optional empty)
  (let ((chunk (get-chunk chunk-name)))
    (cond ((not chunk)
           (print-warning "~s does not name a chunk in call to chunk-match-signature." chunk-name))
          ((not (integerp filled))
           (print-warning "~s is not an appropriate filled signature in call to chunk-match-signature." filled))
          ((not (or (null empty) (integerp empty)))
           (print-warning "~s is not an appropriate empty signature in call to chunk-match-signature." empty))
          (t
           (bt:with-recursive-lock-held ((act-r-chunk-lock chunk))
             (slots-vector-match-signature (act-r-chunk-filled-slots chunk) filled empty))))))
  
(defun slots-vector-match-signature (slots-vector filled &optional empty)
  ;; if empty defaults to 0 could do this instead but doesn't seem to matter time wise
  ;; (zerop (logior (logandc1 slots-vector filled) (logand slots-vector empty)))
  (and (= filled (logand filled slots-vector))
       (or (null empty)
           (= empty (logandc2 empty slots-vector)))))

(defun compare-chunk-to-signature (chunk-name filled &optional empty)
  (let ((chunk (get-chunk chunk-name)))
    (cond ((not chunk)
           (print-warning "~s does not name a chunk in call to compare-chunk-to-signature." chunk-name))
          ((not (integerp filled))
           (print-warning "~s is not an appropriate filled signature in call to compare-chunk-to-signature." filled))
          ((not (or (null empty) (integerp empty)))
           (print-warning "~s is not an appropriate empty signature in call to compare-chunk-to-signature." empty))
          (t
           (bt:with-recursive-lock-held ((act-r-chunk-lock chunk))
             (compare-slots-vector-to-signature (act-r-chunk-filled-slots chunk) filled empty))))))
  
(defun compare-slots-vector-to-signature (slots-vector filled &optional empty)
  "Returns match, extra slots with values, slots which were missing values, and slots which shouldn't have had values"
  (let ((filled? (= filled (logand filled slots-vector)))
        (empty? (or (null empty)
                    (= empty (logandc2 empty slots-vector)))))
    (values (and filled? empty?)
            (slot-mask->names (logandc2 slots-vector filled))
            (slot-mask->names (logandc1 slots-vector filled))
            (and empty (slot-mask->names (logand slots-vector empty))))))

(defun reprocess-chunk-spec (spec var-char)
  (let ((new-spec (make-act-r-chunk-spec)))
    (dolist (x (act-r-chunk-spec-slots spec))
      (let ((slot-spec (copy-act-r-slot-spec x)))
        (setf (act-r-slot-spec-variable slot-spec)
          (cond ((and (chunk-spec-variable-p (act-r-slot-spec-name slot-spec) var-char)
                      (chunk-spec-variable-p (act-r-slot-spec-value slot-spec) var-char))
                 :both)
                ((chunk-spec-variable-p (act-r-slot-spec-name slot-spec) var-char)
                 :slot)
                ((chunk-spec-variable-p (act-r-slot-spec-value slot-spec) var-char)
                 :value)
                (t nil)))
        (add-slot-spec-to-chunk-spec slot-spec new-spec)))
    new-spec))

(defun match-chunk-spec-p (chunk-name chunk-spec 
                                      &key (=test 'chunk-slot-equal) 
                                      (-test 'chunk-slot-not-equal)
                                      (>test 'safe>) (>=test 'safe>=) 
                                      (<test 'safe<) (<=test 'safe<=)
                                      (variable-char #\=))
   (verify-current-model
    "Match-chunk-spec-p called with no current model."
    (let ((chunk (get-chunk chunk-name)))
      (cond ((null chunk)
             (print-warning "~s does not name a chunk in call to match-chunk-spec-p." chunk-name))
            (t 
             (let ((s (cond ((act-r-chunk-spec-p chunk-spec) chunk-spec)
                            ((numberp chunk-spec) (id-to-chunk-spec chunk-spec))
                            (t nil))))
               (if (null s)
                   (print-warning "~s is not a valid chunk-spec or chunk-spec id in call to match-chunk-spec-p." chunk-spec)
                 (let ((var (cond ((characterp variable-char) variable-char)
                                  ((and (stringp variable-char) (= (length variable-char) 1)) (char variable-char 0))
                                  (t nil))))
                   (if (null var)
                       (print-warning "~s is not a valid variable character in call to match-chunk-spec-p." variable-char)
                     (progn
             
                       ;; check the variable character and reprocess the spec 
                       ;; if it's not = since that may mean different slotnames are "real"
             
                       (unless (char-equal var #\=)
                         (setf s (reprocess-chunk-spec s var)))
             
                       (bt:with-recursive-lock-held ((act-r-chunk-lock chunk))
                         (cond
                          ;; if the filled slots are filled and the empty slots aren't then test further
                          ((slots-vector-match-signature (act-r-chunk-filled-slots chunk) 
                                                         (act-r-chunk-spec-filled-slots s) 
                                                         (act-r-chunk-spec-empty-slots s))
                 
                           (handler-case (test-chunk-slots (act-r-chunk-slot-value-lists chunk)
                                                           s =test -test >test >=test <test <=test)
                             (error (condition) 
                               (print-warning "Error ~S encountered in matching chunk ~s." condition chunk-name))))
                          (t
                           nil)))))))))))))

(defun remote-match-chunk-spec-p (chunk spec &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'remote-match-chunk-spec-p '(:=test :-test :<test :>test :<=test :>=test :variable-char))
    (when valid 
      (apply 'match-chunk-spec-p (string->name chunk) spec ol))))

(add-act-r-command "match-chunk-spec-p" 'remote-match-chunk-spec-p "Test whether a chunk matches the pattern in a chunk-spec. Params: chunk-name chunk-spec {< =test, -test, <test, <=test, >test, >=test, variable-char >}.")


(defun chunk-slot-not-equal (arg1 arg2)
 (not (chunk-slot-equal arg1 arg2)))

(defun test-chunk-slots (slots spec =test -test >test >=test <test <=test)
  (let ((bindings nil)
        (get-bindings nil)
        (others nil))
    
    (flet ((test-slot (modifier chunks-slot-value spec-value)
                      (dispatch-apply (case modifier
                                        (= =test)
                                        (- -test)
                                        (> >test)
                                        (>= >=test)
                                        (< <test)
                                        (<= <=test))
                                      chunks-slot-value spec-value))
           (chunks-slot-value (slot)
                              (cdr (assoc slot slots :key 'act-r-slot-name))))
      
      ;; First pass to get all the constant bindings and test the constants
      
      (dolist (x (act-r-chunk-spec-testable-slots spec))
        (let ((value (act-r-slot-spec-value x))
              (modifier (act-r-slot-spec-modifier x))
              (slot (act-r-slot-spec-name x))
              (variable (act-r-slot-spec-variable x)))
          
          (cond (;; it's a constant test
                 (null variable)
                 (unless (test-slot modifier (chunks-slot-value slot) value)
                   (return-from test-chunk-slots nil)))
                
                (;; variables in constant slots
                 (eq (act-r-slot-spec-variable x) :value)
                 (cond (;; it has been bound and can be tested
                        (assoc value bindings)
                        (unless (test-slot modifier (chunks-slot-value slot) (cdr (assoc value bindings)))
                          (return-from test-chunk-slots nil)))
                       
                       (;; it's an equal test so create the binding
                        (eq modifier '=)
                        (push (cons value (chunks-slot-value slot)) bindings))
                       (;; some other test required so save it
                        t
                        (push x others))))
                (;; other potential bindings save for pass 2
                 (and (eq modifier '=)
                      (eq variable :both))
                 (push x get-bindings))
                (t ;; everything else goes at the end
                 (push x others)))))
      
      ;; Second pass if all variables bound then check things
      ;; that needed the bindings.
      
      (when get-bindings
        ;; just do this in repeated loops instead of trying to sort
        ;; things up front
        
        (do ((conds get-bindings)
             (changed t))
            ((or (null conds) (null changed)) 
             (unless (null conds)
               (return-from test-chunk-slots nil)))
          (setf changed nil)
          (dolist (x conds)
            (let ((value (act-r-slot-spec-value x))
                  (slot (act-r-slot-spec-name x)))
              
              (cond (;; slot name not bound so skip it
                       (null (assoc slot bindings)))
                    (;; value variable is bound already
                     ;; so just test it
                     (assoc value bindings)
                     (unless (test-slot '= (chunks-slot-value (cdr (assoc slot bindings))) (cdr (assoc value bindings)))
                       (return-from test-chunk-slots nil))
                     (setf conds (remove x conds))
                     (setf changed t))
                    (;; variable not bound yet so create binding if slot actually exists
                     ;; and remove this from the set
                     t
                     (let ((slot-value (chunks-slot-value (cdr (assoc slot bindings)))))
                       (unless slot-value
                         (return-from test-chunk-slots nil))
                       (push (cons value slot-value) bindings)
                       (setf conds (remove x conds))
                       (setf changed t))))))))
            
      ;; third pass - test all the others since bindings done
      
      (dolist (x others t)
        (let ((value (act-r-slot-spec-value x))
                (modifier (act-r-slot-spec-modifier x))
                (slot (act-r-slot-spec-name x))
                (variable (act-r-slot-spec-variable x)))
          
          (case variable
            (:value
             (unless (test-slot modifier (chunks-slot-value slot) (cdr (assoc value bindings)))
               (return-from test-chunk-slots nil)))
            (:slot
             (unless (test-slot modifier (chunks-slot-value (cdr (assoc slot bindings))) value)
               (return-from test-chunk-slots nil)))
            (:both
             (unless (test-slot modifier (chunks-slot-value (cdr (assoc slot bindings))) (cdr (assoc value bindings)))
               (return-from test-chunk-slots nil)))
            (t
             (unless (test-slot modifier (chunks-slot-value slot) value)
               (return-from test-chunk-slots nil)))))))))

(defun find-matching-chunks (chunk-spec 
                             &key 
                             (chunks :all) (=test 'chunk-slot-equal) 
                             (-test 'chunk-slot-not-equal)
                             (>test 'safe>) (>=test 'safe>=) 
                             (<test 'safe<) (<=test 'safe<=)
                             (variable-char #\=))
  
   (verify-current-model
    "Find-matching-chunks called with no current model."
    
    (let ((found nil)
          (model (current-model-struct))
          (s (cond ((act-r-chunk-spec-p chunk-spec) chunk-spec)
                   ((numberp chunk-spec) (id-to-chunk-spec chunk-spec))
                   (t nil))))
      
      (if (null s)
          (print-warning "~s is not a valid chunk-spec or chunk-spec id in call to find-matching-chunks." chunk-spec)
        (let ((var (cond ((characterp variable-char) variable-char)
                         ((and (stringp variable-char) (= (length variable-char) 1)) (char variable-char 0))
                         (t nil))))
          (if (null var)
              (print-warning "~s is not a valid variable character in call to find-matching-chunks." variable-char)
                  
            (cond ((eq :all chunks)
                   (dolist (name (bt:with-recursive-lock-held ((act-r-model-chunk-lock model)) (hash-table-keys (act-r-model-chunks-table model))) found)
                     (when (match-chunk-spec-p name s 
                                               :=test =test :-test -test
                                               :>test >test :>=test >=test 
                                               :<test <test :<=test <=test
                                               :variable-char var)
                       (push name found))))
                  ((listp chunks)
                   (dolist (name chunks found)
                     (when (match-chunk-spec-p name s 
                                               :=test =test :-test -test
                                               :>test >test :>=test >=test 
                                               :<test <test :<=test <=test
                                               :variable-char var)
                       (push name found))))
                  (t (print-warning "~S is not a valid value for the :chunks keyword parameter to find-matching-chunks." chunks)))))))))
                       

(defun remote-find-matching-chunks (chunk-spec &optional (chunks "all" chunksp) params)
  (if (null chunksp)
      (find-matching-chunks chunk-spec)
    (multiple-value-bind (valid params) 
        (process-options-list params 'find-matching-chunks '(:=test :-test :<test :>test :<=test :>=test :variable-char))
      (when valid 
        (if (and (stringp chunks) (string-equal chunks "all"))
            (apply 'find-matching-chunks chunk-spec params)
          (if (listp chunks)
              (apply 'find-matching-chunks chunk-spec :chunks (string->name-recursive chunks) params)
            (print-warning "Chunks parameter to find-matching-chunks must be a list of chunk names, but given ~s" chunks)))))))
        
        
(add-act-r-command "find-matching-chunks" 'remote-find-matching-chunks "Return all chunks which match the pattern in a chunk-spec. Params: chunk-spec {list-of-chunks {< =test, -test, <test, <=test, >test, >=test, variable-char >}}.")


(defun pprint-chunk-spec (chunk-spec)
  "Print a chunk specification in a 'production like' way to command output"
   (verify-current-model
    "Pprint-chunk-spec called with no current model."
    (let ((spec (if (act-r-chunk-spec-p chunk-spec) chunk-spec (id-to-chunk-spec chunk-spec))))
      (when spec
        (dolist (slot (act-r-chunk-spec-slots spec))
          (if (eql '= (act-r-slot-spec-modifier slot))
              (command-output "    ~s ~s" (act-r-slot-spec-name slot) (act-r-slot-spec-value slot))
            (command-output " ~2a ~s ~s" (act-r-slot-spec-modifier slot) (act-r-slot-spec-name slot) (act-r-slot-spec-value slot))))))))

(add-act-r-command "pprint-chunk-spec" 'pprint-chunk-spec "Print a chunk-spec representation to the command trace. Params: chunk-spec.")

(defun printed-chunk-spec (chunk-spec &optional (flat nil))
  "Return a string with the print of a chunk specification in a 'production like' way"
  (verify-current-model
   "Printed-chunk-spec called with no current model."
   (if (act-r-chunk-spec-p chunk-spec)
     (let ((s (make-string-output-stream)))
       (dolist (slot (act-r-chunk-spec-slots chunk-spec))
         (if (eql '= (act-r-slot-spec-modifier slot))
             (if flat
                 (format s " ~s ~s" (act-r-slot-spec-name slot) (act-r-slot-spec-value slot))
               (format s "    ~s ~s~%" (act-r-slot-spec-name slot) (act-r-slot-spec-value slot)))
           (if flat
               (format s " ~a ~s ~s" (act-r-slot-spec-modifier slot) (act-r-slot-spec-name slot) (act-r-slot-spec-value slot))
               (format s " ~2a ~s ~s~%" (act-r-slot-spec-modifier slot) (act-r-slot-spec-name slot) (act-r-slot-spec-value slot)))))
       (if flat
           (string-left-trim '(#\space) (get-output-stream-string s))
         (get-output-stream-string s)))
     (awhen (id-to-chunk-spec chunk-spec)
            (printed-chunk-spec it flat)))))

(add-act-r-command "printed-chunk-spec" 'printed-chunk-spec "Return a string with the representation of a chunk-spec. Params: chunk-spec {flat}.")

(defun slot-specs-to-chunk-spec-list (slot-specs)
  (apply 'append slot-specs))

(defun slot-specs-to-chunk-spec (slot-specs)
  (define-chunk-spec-fct (slot-specs-to-chunk-spec-list slot-specs)))



(defun chunk-spec-to-chunk-def (chunk-spec)
  "Convert a chunk-spec to a chunk definition list ignoring request parameters"
  (if (act-r-chunk-spec-p chunk-spec)
      (let ((requests (lognot (act-r-chunk-spec-request-param-slots chunk-spec)))) ;; don't consider request parameters
        (cond ((act-r-chunk-spec-slot-vars chunk-spec)
               (print-warning "Chunk-spec has variablized slots in a call to chunk-spec-to-chunk-def."))
              ((act-r-chunk-spec-variables chunk-spec)
               (print-warning "Chunk-spec has variables in the values in a call to chunk-spec-to-chunk-def."))
              ((not (zerop (logand requests (act-r-chunk-spec-duplicate-slots chunk-spec))))
               (print-warning "Chunk-spec may only specify a slot once in a call to chunk-spec-to-chunk-def."))
              ((or (not (zerop (logand requests (act-r-chunk-spec-negated-slots chunk-spec))))
                   (not (zerop (logand requests (act-r-chunk-spec-relative-slots chunk-spec)))))
               (print-warning "Chunk-spec may only use the = modifier in a call to chunk-spec-to-chunk-def."))
              (t
               (aif (mapcan (lambda (x) 
                              (unless (keywordp (act-r-slot-spec-name x))
                                (list (act-r-slot-spec-name x) (act-r-slot-spec-value x))))
                      (act-r-chunk-spec-slots chunk-spec))
                    it
                    (list 'isa 'chunk)))))
    (aif (id-to-chunk-spec chunk-spec)
         (chunk-spec-to-chunk-def it)
         (print-warning "Chunk-spec-to-chunk-def called with something other than a chunk-spec."))))



(defun remote-chunk-spec-to-chunk-def (chunk-spec-id)
  (let ((cs (id-to-chunk-spec chunk-spec-id)))
    (when cs
      (encode-string-names (chunk-spec-to-chunk-def cs)))))

(add-act-r-command "chunk-spec-to-chunk-def" 'remote-chunk-spec-to-chunk-def "Convert a chunk-spec id to a list of slot value pairs. Params: chunk-spec." nil)


(defun merge-chunk-specs (spec1 spec2)
  (if (and (act-r-chunk-spec-p spec1) (act-r-chunk-spec-p spec2))
      (let ((new-spec (make-act-r-chunk-spec)))
        (dolist (slot (act-r-chunk-spec-slots spec1))
          (add-slot-spec-to-chunk-spec slot new-spec))
        (dolist (slot (act-r-chunk-spec-slots spec2))
          (add-slot-spec-to-chunk-spec slot new-spec))
        new-spec)
    (print-warning "merge-chunk-specs requires two valid chunk-specs")))
        


(defun remote-chunk-spec-variable-p (chunk-spec-slot-value &optional (char "="))
  (chunk-spec-variable-p (string->name chunk-spec-slot-value) (when (stringp char) (char char 0))))

(add-act-r-command "chunk-spec-variable-p" 'remote-chunk-spec-variable-p "Test a value to determine if it would be considered a variable in a chunk-spec. Params: value {var-character}.")



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
