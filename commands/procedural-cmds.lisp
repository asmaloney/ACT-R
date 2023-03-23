;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : procedural-cmds.lisp
;;; Version     : 2.0
;;; 
;;; Description : User functions for the procedural module.
;;; 
;;; Bugs        : * [should be fixed now]
;;;             :   Doesn't properly check that a variable has a binding before
;;;             :   use on the LHS.  Doesn't break things, but it doesn't warn
;;;             :   in the case of productions that will never fire.
;;;
;;;             : * [fixed]
;;;             :   RHS direct requests don't schedule an implicit clear because
;;;             :   the test for that is that the second item be ISA (to avoid
;;;             :   clearing on a mod-request).  SO, was that by design or an
;;;             :   over-generalization?  IF it's a bug, fix in p* as well.


;;; To do       : * [done]
;;;             :   Try redoing the matching so that all bindings occur before
;;;             :   all other tests so that buffer ordering isn't important
;;;             :   other than to determine which buffer is used for the
;;;             :   binding.
;;;             : * Save the variable name in the parse-condition (probably
;;;             :   as a third element to the first or something) so that
;;;             :   it doesn't have to be created and interned in p.
;;;             : * In general that can be done in the parsing so that 
;;;             :   there's no need to call find-variables in p.
;;;             : * Signal a warning when there is a !eval! in the slot
;;;             :   value position because a list is valid but it won't
;;;             :   do the evaling which could confuse those moving things
;;;             :   from 5.
;;;
;;;
;;; ----- History -----
;;; 2005.01.15 Dan
;;;             : * Moved things to here from procedural in core-modules.
;;; 2005.01.16 Dan
;;;             : * Added the checks for delayed conflict resolution so that
;;;             :   user changes can force it back into the queue (either a
;;;             :   new/changed production or setting a parameter with spp).
;;; 2005.01.17 Dan
;;;             : * Changed model-output to command-output in spp and its
;;;             :   associated functions.
;;; 2005.01.19 Dan
;;;             : * Added most of the ACT-R 4/5 user commands.
;;; 2005.02.03 Dan
;;;             : * Moved production-firing-only here from the printing module.
;;; 2005.02.04 Dan
;;;             : * Complete reworking of how production parsing occurs to
;;;             :   attempt to cache more stuff (define-chunk-specs) at parse
;;;             :   time.
;;;             : * Undid the 80 column limit because it really makes it hard
;;;             :   for me to work with this.
;;; 2005.02.10 Dan
;;;             : * Use the new parse tables in parse-conditions and parse-
;;;             :   action to help make a reset faster.
;;; 2005.02.13 Dan 
;;;             : * Minor change to add more #' to the lambdas.
;;; 2005.02.14 Dan
;;;             : * Fixed bind-variable so that it returns nil wnen a
;;;                 variable gets bound to nil so it breaks the matching.
;;; 2005.02.25 Dan
;;;             : * Added replace-variables-for-eval because the values passed
;;;             :   to a function in an eval need to be quoted.
;;; 2005.03.18 Dan
;;;             : * Modfying the p command so that it only does strict
;;;             :   harvesting for the buffers not on the do-not-harvest
;;;             :   list.
;;; 2005.04.12 Dan
;;;             : * Fixed a small typo (LHS->RHS) in a warning in parse-actions. 
;;; 2005.04.26 Dan
;;;             : * Restored the setting of lhs and rhs of the production to
;;;             :   the parsed code for use by compilation.
;;; 2005.05.03 Dan
;;;             : * Possible bug has shown up (see above) and may need to be
;;;             :   fixed...
;;; 2005.05.04 Dan
;;;             : * Fixed the issue with direct requests not clearing the
;;;             :   buffer.
;;; 2005.05.11 Dan
;;;             : * Changed the output parameter for buffer clearing so that
;;;             :   it prints in the medium traces as well.
;;; 2005.05.17 Dan
;;;             : * Added the delete-production function.  Not intended as a
;;;             :   user command...
;;; 2005.06.01 Dan
;;;             : * Made pp/pp-fct more tolerant of bad production names.
;;; 2005.06.02 Dan
;;;             : * Made replace-variables-for-eval smarter about replacing
;;;             :   a variable with another variable because compilation can
;;;             :   result in that and it's not desireable to have it quoted.
;;; 2005.06.18 Dan
;;;             : * Modified spp so it prints a warning if a named production
;;;             :   does not exist.
;;; 2005.09.09 Dan
;;;             : * Changed reference of chunk-to-chunk-spec to chunk-name-to-
;;;             :   chunk-spec and made sure to call it appropriately.
;;; 2005.09.14 Dan
;;;             : * Change to the priority of RHS actions. = is now set to a
;;;             :   higher priority than +.  See log in procedural.lisp for
;;;             :   the detailed rational.
;;; 2005.11.09 Dan
;;;             : * Fixed a bug in pmatches which occured when more than one
;;;             :   production matched the current state - it threw an error.
;;;             :   That also caused whynot to break because it uses pmatches.
;;; 2005.12.07 Dan
;;;             : * Restricting the potential uses of !bind!.  Instead of 
;;;             :   having it participate in bindings at the same level as the
;;;             :   buffers, it now only occurs explicitly after all buffer
;;;             :   tests have been allowed to do their bindings and warns
;;;             :   (rejecting the production) if there is an attempt to rebind
;;;             :   a particular variable.  It also now tests for circualar
;;;             :   references among !binds! and orders them as needed.
;;; 2005.12.15 Dan 
;;;             : * Modified replace-variables to handle something like
;;;             :   (cons 'a 'b) properly because I'm using that in the
;;;             :   compilation code now.  If this seems to impact performance
;;;             :   then I'll need to just make a special version for that
;;;             :   case.
;;; 2005.12.22 Dan
;;;             : * Removed the spp code to put in the utility-and-reward file.
;;; 2006.03.09 Dan
;;;             : * Get-production has changed to not require the procedural
;;;             :   module so make that change as needed.
;;; 2006.03.10 Dan
;;;             : * Changed pp to not exclude disabled productions and going to
;;;             :   make print-production mark them as such instead.
;;; 2006.03.13 Dan 
;;;             : * Fixed pmatches to deal with the new utility computations.
;;; 2006.03.14 Dan
;;;             : * Added the define-p macro which does the same thing as p,
;;;             :   but the define- name provides some consistency with every-
;;;             :   thing else (define-model, define-chunks, define-module, etc)
;;;             :   and helps people who use MCL as the editor.
;;; 2006.03.14 Dan
;;;             : * Added the function all-productions because I realized that
;;;             :   using pp is a bad idea since it invokes the cost of the
;;;             :   production printer each time. 
;;; 2006.03.21 Dan
;;;             : * Fixed a bug with un-delay-conflict-resolution that slipped
;;;             :   in when I updated the utility mechanims.
;;; 2006.10.11 Dan
;;;             : * Changed the priority of the buffer-overwrite action to be
;;;             :   90 instead of 100 (the priority of the buffer mod action)
;;;             :   because the modifications must occur first, but if they
;;;             :   have the same priority then they could be reversed which
;;;             :   can lead to run-time issues and isn't right with respect
;;;             :   to how productions should work.
;;; 2006.11.07 Dan
;;;             : * Fixed a bug in all-productions which caused errors if there
;;;             :   were more than one model defined and it was called with no
;;;             :   current model (not an issue for running models).
;;;             : * Made penable, pdisable, pbreak, and punbreak report nicer
;;;             :   warnings when there is no current model.
;;;             : * Fixed the penable macro so it actually called penable-fct!
;;;             : * Changed penable-fct so that like punbreak if it's passed 
;;;             :   nil all productions are enabled.
;;; 2006.11.08 Dan
;;;             : * Changed whynot so it better reports the warning that there
;;;             :   is no current model.
;;; 2006.11.09 Dan
;;;             : * Changed print-production-output so that it can distinguish
;;;             :   between an explicit string as the first element on a list
;;;             :   and when it just happens to be a string in the current 
;;;             :   binding (to differentiate when it should use the string
;;;             :   as a format string and when it should just output it as
;;;             :   is).
;;;             : * Modified the call to print-production-output in the parsing
;;;             :   of an !output! command so that the "old style" format string
;;;             :   usage doesn't get triggered implicitly (using the change
;;;             :   described above).
;;;             : * Fixed a bug in production parsing that would lead to run time
;;;             :   errors if a buffer overwrite action didn't specify a chunk
;;;             :   or at least a variable (the variable could still bind to a
;;;             :   non-chunk at run time).
;;; 2006.11.10 Dan
;;;             : * Additional update to the parsing like the last one - now
;;;             :   direct requests must be a variable or chunk.
;;; 2006.11.20 Dan
;;;             : * Removed the special case implicit action for visual requests in
;;;             :   the production definition and replaced it with the new 
;;;             :   (generic) module-warning mechanims.
;;; 2007.06.11 Dan
;;;             : * Fixed a bug in REPLACE-VARIABLES-FOR-EVAL which would quote
;;;             :   strings that had been bound to the variable in the replacement.
;;; 2007.06.18 Dan
;;;             : * Moved slot-specs-to-chunk-spec-list to the chunk-spec file
;;;             :   and then changed the references in here to add the -fct.
;;; 2008.03.24 Dan
;;;             : * Start of the work to add the !mv-bind! to productions.
;;;             : * Added the failure reason for !bind! so whynot shows that it
;;;             :   fails for a binding to nil.
;;;             : * Sort the conditions of the productions now so that explicit
;;;             :   binds always precede buffer bindings (thought I did that
;;;             :   already).
;;; 2008.03.25 Dan
;;;             : * Added the new !mv-bind! option for productions.  It works
;;;             :   like !bind! except that it can bind multiple values in one
;;;             :   call.  Here is an example:
;;;             :   !mv-bind! (=var1 =var2) (some-function)
;;;             :   that would bind the variables =var1 and =var2 to the first
;;;             :   and second values returned by some-function.  If there are
;;;             :   not as many return values as variables to be bound the extra
;;;             :   are bound to nil which means the production will not match 
;;;             :   if it's a LHS !mv-bind!.
;;;             : * Fixed a bug that would let one rebind a variable with an
;;;             :   explicit !bind! on the RHS of a production.
;;; 2008.03.27 Dan
;;;             : * Fixed a nasty bug I introduced in production parsing with
;;;             :   the sorting of conditions for binding pruposes.
;;; 2008.03.28 Dan
;;;             : * Fixed !mv-bind! for the RHS.
;;; 2008.06.20 Dan
;;;             : * Removed the -fct from slot-specs-to-chunk-spec-list since
;;;             :   the macro was removed from the chunk-spec code.
;;; 2008.07.09 Dan 
;;;             : * Changed the p function so that invalid lhs conditions don't
;;;             :   lead to an error during the sort.
;;; 2008.07.22 Dan
;;;             : * Updated whynot so that it notes if a production was below
;;;             :   the utility threshold the last time it was in the conflict
;;;             :   set.
;;; 2008.08.01 Dan
;;;             : * Updated pmatches to work the same as conflict-resolution
;;;             :   since they use the same 'sorter'.
;;; 2008.08.05 Dan
;;;             : * Added the production-failure-reason function needed by the
;;;             :   new production history tool here to keep the code which
;;;             :   accesses the native production structure in the 'system'
;;;             :   code.
;;; 2008.10.23 Dan
;;;             : * Fixed a bug introduced with the "fix" on 2008/07/09 that
;;;             :   could cause an invalid LHS condition to allow the production
;;;             :   to still be created with a null LHS.
;;; 2008.11.03 Dan
;;;             : * Updated the parsing of productions so that the LHS code uses
;;;             :   the new cr-buffer-read option in the conflict resolution
;;;             :   code.
;;; 2008.11.25 Dan
;;;             : * Changed things to use add-production and remove-production
;;;             :   to better abstract away from the internals of the module.
;;;             : * Changed calls to get-production to use get-production-internal
;;;             :   to avoid the unnecessary hash-table look-up when possible.
;;;             : * Abstracting away from procedural-productions with productions-
;;;             :   list so less code is dependent on the procedural structure.
;;;             : * Clear-productions now iteratively removes the productions
;;;             :   instead of just erasing the list.
;;; 2008.12.08 Dan [1.1]
;;;             : * Significant overhaul of the internal representation of 
;;;             :   productions.  No longer create lambdas for the conditions.  
;;;             :   Instead, create more specific single condition tests.
;;;             : * Whynot information can now be more specific - lists which
;;;             :   slot in a buffer caused the mismatch.
;;;             : * Pmatches (and thus whynot too) no longer computes the
;;;             :   utilities and thus it doesn't sort the conflict-set based
;;;             :   on those values.
;;; 2009.05.05 Dan
;;;             : * Changed the inequality tests so that they all use the "right"
;;;             :   test instead of only testing > and negating and then add the
;;;             :   opposite test to the implicit ones for the tree.
;;; 2009.05.22 Dan
;;;             : * Fixed a bug in production parsing which would allow something
;;;             :   like this to "work" (p name =goal> free ==> ...).
;;; 2009.06.03 Dan 
;;;             : * Added explicit conditions for the inequality tests to check
;;;             :   if a value is a number and an implicit check if a slot has
;;;             :   a constant to possibly help in building the conflict tree.
;;; 2009.08.14 Dan [1.2]
;;;             : * Updated whynot and pmatches to work better with the partial 
;;;             :   matching of productions -- they report partial match info.
;;;             :   when appropriate.
;;; 2009.09.10 Dan
;;;             : * Moved code common from this and p-star-cmd into production-parsing-
;;;             :   support.lisp in the support directory and added the require-
;;;             :   compiled for that.
;;; 2009.09.18 Dan
;;;             : * P and p* creation is now done with a single set of code which
;;;             :   is in the production-parsing-support.lisp file.
;;; 2009.10.15 Dan
;;;             : * Updated whynot & pmatches to work right with the possibility
;;;             :   of buffer search.  If the check is made at the same time as
;;;             :   the last conflict-resolution call then the buffer chunks
;;;             :   searched over is the cached set and ordering used in that
;;;             :   conflict-resolution.  Otherwise, a new set and ordering is
;;;             :   requested from the owning module.
;;; 2010.01.18 Dan
;;;             : * Pmatches-internal now uses the new :report keyword for 
;;;             :   conflict-tests to suppress the :crt info if it is enabled.
;;; 2011.04.26 Dan
;;;             : * Use mp-time-ms for all time references instead of mp-time.
;;; 2011.04.28 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions and removed some unneeded let variables.
;;; 2013.08.05 Dan
;;;             : * Clear the hashtables for the style warnings in clear-productions.
;;; 2013.08.09 Dan
;;;             : * Added the command decalare-buffer-usage to avoid style
;;;             :   warnings when chunks are being set through code or otherwise
;;;             :   not in the initial model definition.
;;; 2013.08.12 Dan
;;;             : * Changed declare-buffer-usage to return t/nil.
;;; 2013.10.18 Dan
;;;             : * Finally fixed the typo in test-and-perfrom.
;;; 2013.11.14 Dan
;;;             : * Changed declare-buffer-usage to also allow suppressing the
;;;             :   "modified without use" style warnings by adding the slots to
;;;             :   the procedural-cond-style-usage-table as well.
;;; 2014.04.07 Dan
;;;             : * Changed calls to failure-reason-string to not pass procedural.
;;; 2014.05.11 Dan [2.0]
;;;             : * Updates to be consistent with the no chunk-type mechanisms.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2015.09.11 Dan
;;;             : * Add the require for productions here eventhough the procedural
;;;             :   module will have certainly loaded it already.
;;; 2016.11.23 Dan
;;;             : * Add a dispatch version of whynot.
;;; 2016.12.02 Dan
;;;             : * Allow whynot-internal to accept strings for the names of
;;;             :   the productions as well as the symbols.
;;; 2017.01.18 Dan
;;;             : * Removed the echo-act-r-output from whynot.  The assumption
;;;             :   now is that echoing is handled by the user.
;;;             : * Use handle-evaluate-results.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.03 Dan
;;;             : * Make penable and pdisable available remotely and allow them
;;;             :   to take strings with names.
;;; 2017.02.08 Dan
;;;             : * Reworked the local versions of the remote commands to not
;;;             :   go out through the dispatcher.
;;; 2017.08.11 Dan
;;;             : * Lock access to procedural-productions.
;;;             : * Allow pdisable to disable everything if nil provided.
;;; 2017.08.16 Dan
;;;             : * Fixed a bug with the return values from penable and pdisable.
;;; 2017.09.05 Dan
;;;             : * Make all-productions remotely available.
;;; 2017.10.02 Dan
;;;             : * Added the used-production-buffers command since that's useful
;;;             :   for the Environment to initialize things.
;;; 2017.11.16 Dan
;;;             : * Added a remote version of pp.
;;; 2017.12.20 Dan
;;;             : * Fix a bug in pbreak and punbreak.
;;; 2018.01.04 Dan
;;;             : * Added remote versions of pbreak and punbreak.
;;; 2018.04.18 Dan
;;;             : * Added remote clear-productions.
;;; 2018.06.12 Dan
;;;             : * Reworked external commands so that only external does the
;;;             :   string->name processing.
;;; 2018.06.13 Dan
;;;             : * Fixed a bug in whynot-fct from the previous update.
;;; 2018.08.17 Dan
;;;             : * Can assume last-cr-time is a number.
;;; 2018.08.20 Dan
;;;             : * Use the same code to initialize the buffer-lookup as is used
;;;             :   in conflict-resolution (necessary if whynot used first).
;;; 2018.09.04 Dan
;;;             : * Undo the last two updates since that change to pattern 
;;;             :   matching wasn't better in general.
;;; 2019.01.15 Dan
;;;             : * Only rebuild the lookup tables when needed -- just reset the
;;;             :   values otherwise.
;;; 2019.01.17 Dan
;;;             : * Use an alist for the saved-search-chunk in production matching.
;;; 2019.02.13 Dan
;;;             : * Add the buffer-state test to pmatches-internal so that the
;;;             :   whynot information matches the crt info.
;;; 2020.08.26 Dan
;;;             : * Removed the path for require-compiled since it's not needed
;;;             :   and results in warnings in SBCL.
;;; 2021.07.08 Dan
;;;             : * Fixed a typo in the warning for p-fct about no procedural
;;;             :   module being available.
;;; 2021.08.23 Dan 
;;;             : * Fixed a problem with production-failure-reason because it
;;;             :   didn't work when the buffer array mechanism was used since
;;;             :   productions not in the matching buffer set didn't have their
;;;             :   failure-condition cleared or set.  Now, it sets the ones
;;;             :   that aren't in the buffer set to indicate that.
;;; 2021.10.18 Dan
;;;             : * Changed call to buffers to model-buffers instead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
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
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(declaim (ftype (function () t) minimum-utility))
(declaim (ftype (function (t) t) production-utility))

(require-compiled "PRODUCTIONS")
(require-compiled "PRODUCTION-PARSING")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The user functions mostly from ACT-R 5

(defun all-productions ()
  (let ((prod (get-module procedural)))
    (when prod
      (mapcar 'production-name (productions-list prod)))))

(add-act-r-command "all-productions" 'all-productions "Returns a list of all the production names in the current model. No params.")

(defmacro pp (&rest productions)
  `(pp-fct ',productions))

(defun pp-fct (productions)
  (let ((prod (get-module procedural)))
    (if prod
        (let ((res nil)
              (p (if (null productions) 
                     (mapcar 'production-name (productions-list prod))
                   productions)))
          (dolist (p-name p)
            (let ((production (get-production-internal p-name prod)))
              (if production
                  (progn
                    (print-production production)
                    (push p-name res))
                (print-warning "No production named ~S is defined" p-name))))
          (reverse res))
      (print-warning "No procedural module found"))))

(defun pp-external (&rest productions)
  (pp-fct (string->name-recursive productions)))

(add-act-r-command "pp" 'pp-external "Prints the internal representation of the productions with the given names or all if none provided. Params: p-name*.")

(defun clear-productions ()
  (let ((prod (get-module procedural)))
    (if prod
        (progn
          (print-warning "Clearing the productions is not recommended")
          
          (clrhash (procedural-cond-style-usage-table prod))
          (clrhash (procedural-req-style-usage-table prod))
          (clrhash (procedural-mod-style-usage-table prod))
          (clrhash (procedural-retrieval-cond-style-usage-table prod))
          (clrhash (procedural-retrieval-req-style-usage-table prod))

          (dolist (p (productions-list prod))
            (remove-production p prod)))
      (print-warning "No procedural module was found."))))

(add-act-r-command "clear-productions" 'clear-productions "Remove all productions from the current model. No params.")

(defmacro pbreak (&rest productions)
  `(pbreak-fct ',productions))

(defun pbreak-fct (productions)
  (if (current-model)
      (progn
        (dolist (production productions)
            (aif (get-production production)
               (bt:with-recursive-lock-held ((production-lock it))
                 (setf (production-break it) t))
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (let ((p (get-production production)))
              (bt:with-recursive-lock-held ((production-lock p))
              (when (production-break p)
                (push production res)))))))
    (print-warning "There is no current model - pbreak cannot be used.")))

(defun pbreak-external (&rest productions)
  (pbreak-fct (string->name-recursive productions)))

(add-act-r-command "pbreak" 'pbreak-external "Pbreak causes the scheduler to stop when the specified productions are selected. Params: production-name*.")


(defmacro punbreak (&rest productions)
  `(punbreak-fct ',productions))

(defun punbreak-fct (productions)
  (if (current-model)
      (progn
        (dolist (production (if (null productions)
                                (all-productions)
                              productions))
            (aif (get-production production)
               (bt:with-recursive-lock-held ((production-lock it))
                 (setf (production-break it) nil))
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (let ((p (get-production production)))
              (bt:with-recursive-lock-held ((production-lock p))
              (when (production-break p)
                (push production res)))))))
    (print-warning "There is no current model - punbreak cannot be used.")))

(defun punbreak-external (&rest productions)
  (punbreak-fct (string->name-recursive productions)))

(add-act-r-command "punbreak" 'punbreak-external "Punbreak removes the break flag from the specified productions. Params: production-name*.")


(defmacro pdisable (&rest productions)
  `(pdisable-fct ',productions))

(defun pdisable-fct (productions)
  (if (current-model)
      (let ((all-productions (all-productions)))
        (dolist (production (if (null productions)
                                all-productions
                              productions))
            (aif (get-production production)
                 (bt:with-recursive-lock-held ((production-lock it))
                   (setf (production-disabled it) t))
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (p-name all-productions res)
            (let ((production (get-production p-name)))
              (bt:with-recursive-lock-held ((production-lock production))
                (when (production-disabled production)
                  (push p-name res)))))))
    (print-warning "There is no current model - pdisable cannot be used.")))

(defun pdisable-external (&rest productions)
  (pdisable-fct (string->name-recursive productions)))

(add-act-r-command "pdisable" 'pdisable-external "Prevent the specified productions from being selected. Params: production-name*.")


(defmacro penable (&rest productions)
  `(penable-fct ',productions))

(defun penable-fct (productions)
  (if (current-model)
      (let ((all-productions (all-productions)))
        (dolist (production (if (null productions)
                                all-productions
                              productions))
            (aif (get-production production)
                 (bt:with-recursive-lock-held ((production-lock it))
                   (setf (production-disabled it) nil))
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (p-name all-productions res)
            (let ((production (get-production p-name)))
              (bt:with-recursive-lock-held ((production-lock production))
                (when (production-disabled production)
                (push p-name res)))))))
    (print-warning "There is no current model - penable cannot be used.")))

(defun penable-external (&rest productions)
  (penable-fct (string->name-recursive productions)))

(add-act-r-command "penable" 'penable-external "Restore the specified productions which were disabled. Params: production-name*.")



(defmacro whynot (&rest productions)
  `(whynot-fct ',productions))


(defun whynot-fct (productions)
  (if (current-model) 
      (let* ((procedural (get-module procedural))
             (conflict-set (no-output (pmatches-internal procedural))))
        
        (dolist (production-name (if (null productions)
                                     (all-productions)
                                   productions))
          
          (let ((production (get-production production-name)))
            (if (null production)
                (command-output "~%~s does not name a production." production-name)
              (bt:with-recursive-lock-held ((production-lock production))
                (if (production-disabled production)
                  (command-output "~%Production ~s is disabled." production-name)
                (if (member production-name conflict-set)
                    (if (and (bt:with-lock-held ((procedural-param-lock procedural)) (procedural-ppm procedural)) (production-partial-matched-slots production))
                        (progn ;; It's only a partial match
                          (command-output "~%Production ~s partially matches the current state:" production-name)
                          (print-instantiation production)
                          (let ((ut (car (no-output (sgp :ut)))))
                            (when (and (numberp ut)
                                       (numberp (production-utility production-name))
                                       (< (production-utility production-name) ut))
                              (command-output "Utility was below the threshold the last time it was in the conflict set."))))
                      (progn ;; It's a complete match
                        
                        (command-output "~%Production ~s matches:" production-name)
                        (print-instantiation production)
                        (let ((ut (car (no-output (sgp :ut)))))
                          (when (and (numberp ut)
                                     (numberp (production-utility production-name))
                                     (< (production-utility production) ut))
                            (command-output "Utility was below the threshold the last time it was in the conflict set.")))))
                  (progn
                    (command-output "~%Production ~s does NOT match." production-name)
                    
                    (print-production production)
                    
                    (command-output "It fails because: ")
                    (command-output (failure-reason-string (production-failure-condition production) production)))))))))
        conflict-set)
    (print-warning "Whynot called with no current model.")))

(defun whynot-external (&rest productions)
  (whynot-fct (string->name-recursive productions)))


(add-act-r-command "whynot" 'whynot-external "Print the production matching details for specified productions. Params: production-name*.")

  
(defun production-failure-reason (p-name)
  ; only called with the procedural-cr-lock already held since it's
  ; called during the conflict-set-hook by the procedural history recorder
  
  (let ((production (get-production p-name))
        (procedural (get-module procedural)))
    (bt:with-recursive-lock-held ((production-lock production))
  
      (when (procedural-buffer-use-array procedural)
        (let* ((buffer-state 
                (let ((m (current-model-struct)))
                  (bt:with-lock-held ((act-r-model-buffers-lock m))
                    (act-r-model-buffer-state m))))
               
               (tested-productions (aref (procedural-buffer-use-array procedural) 
                                         (aref (procedural-master-buffer-map procedural) buffer-state))))
      
          (unless (find production tested-productions)
            (setf (production-failure-condition production) (cons :buffer-state buffer-state)))
          ))
  
      (if (and production (production-failure-condition production))
          (failure-reason-string (production-failure-condition production) production)
        ""))))

(defun pmatches ()
  (let ((procedural (get-module procedural)))
    (if procedural
        (pmatches-internal procedural)
      (print-warning "No procedural module found"))))
    
(defun pmatches-internal (procedural)
  
  (bt:with-lock-held ((procedural-cr-lock procedural))
    (if (null (procedural-buffer-lookup procedural))
        (setf (procedural-buffer-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural)) :initial-element :untested))
      (fill (procedural-buffer-lookup procedural) :untested :start 0 :end (procedural-buffer-lookup-size procedural)))
                     
    (if (or (null (procedural-slot-lookup procedural))
            (not (= (largest-chunk-type-size) (procedural-largest-chunk-type procedural))))
        (progn
          (setf (procedural-largest-chunk-type procedural) (largest-chunk-type-size))
          (setf (procedural-slot-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural) (largest-chunk-type-size)) :initial-element :untested)))
      (dolist (x (procedural-slot-used procedural) (setf (procedural-slot-used procedural) nil))
        (setf (aref (procedural-slot-lookup procedural) (car x) (cdr x)) :untested)))
    
    (unless (and (numberp (procedural-last-cr-time procedural)) (= (procedural-last-cr-time procedural) (mp-time-ms)))
      (clrhash (procedural-search-buffer-table procedural)))
    
    (bt:with-lock-held ((procedural-param-lock procedural))
    (let ((conflict-set nil)
          (saved-search-chunks nil)
          (buffer-state (let ((m (current-model-struct)))
                          (bt:with-lock-held ((act-r-model-buffers-lock m))
                            (act-r-model-buffer-state m)))))
      
      (dolist (b (procedural-used-search-buffers procedural))
        (aif (buffer-read b)
             (push (cons b it) saved-search-chunks)
             (push (cons b :clear) saved-search-chunks)))
      
      
      (dolist (production (productions-list procedural))
        (bt:with-recursive-lock-held ((production-lock production))
          (setf (production-bindings production) nil)
          (setf (production-failure-condition production) nil)
          
          (setf (procedural-current-p procedural) production)
          (setf (production-partial-matched-slots production) nil)
          
          (unless (production-disabled production)
            
            (when (and (test-buffer-states procedural production buffer-state :report nil)
                       (conflict-tests procedural (production-constants production) production 'test-constant-condition :report nil)
                       (conflict-tests procedural (production-binds production) production 'test-and-perform-bindings :report nil)
                       (conflict-tests procedural (production-others production) production 'test-other-condition :report nil)
                       (conflict-tests procedural (production-searches production) production 'test-search-buffers :report nil)
                       (conflict-tests procedural (production-search-binds production) production 'test-and-perform-bindings :report nil)
                       (conflict-tests procedural (production-search-others production) production 'test-other-condition :report nil)) 
              
              (push-last production conflict-set)))))
      
      
      (dolist (b (procedural-used-search-buffers procedural))
        (let ((val (assoc b saved-search-chunks)))
          (when val
            (if (eq (cdr val) :clear)
                (erase-buffer b)
              (overwrite-buffer-chunk b (cdr val))))))
      
      
      (dolist (production conflict-set)
        (print-instantiation production))
      
      (mapcar 'production-name conflict-set)))))
  
  
(defun used-production-buffers () 
  (let ((prod (get-module procedural)))
    (when prod
      (let* ((productions (productions-list prod))
             (used (when productions (list 'production))))
        (bt:with-lock-held ((procedural-cr-lock prod))
          (setf used (append used (mapcar 'car (procedural-buffer-indices prod)))))
        used))))

(add-act-r-command "used-production-buffers" 'used-production-buffers "Returns a list of all the buffers which are used in the productions of the current model. No params.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This section is production parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-p (&rest definition)
  "Production definition."
  `(p-fct ',definition))

(defun define-p-fct (definition)
  (p-fct definition))

(defmacro p (&rest definition)
  "Production definition."
  `(p-fct ',definition))

(defun p-fct (definition)
  (let ((prod (get-module procedural)))  
    (if (procedural-p prod)  
        (create-production prod definition) 
      (print-warning "No procedural module found cannot create production."))))


(defun delete-production (prod-name)
  (let ((procedural (get-module procedural)))
    (if procedural
        (remove-production (get-production-internal prod-name procedural) procedural)
      (print-warning "No procedural module found.  Cannot delete production ~S." prod-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A command to avoid style warnings.

(defmacro declare-buffer-usage (buffer type &rest slots)
  `(declare-buffer-usage-fct ',buffer ',type ',slots))

(defun declare-buffer-usage-fct (buffer type &optional slots)
  (let ((procedural (get-module procedural)))
    (if procedural
        (cond ((not (find buffer (model-buffers)))
               (print-warning "Cannot declare usage for ~S because it does not name a buffer in the model." buffer))
              ((not (chunk-type-p-fct type))
               (print-warning "Cannot declare usage for buffer ~s because ~s does not name a chunk-type in the model." buffer type))
              ((not (or (eq slots :all)
                        (and (listp slots) (= (length slots) 1) (eq (car slots) :all))
                        (every (lambda (x)
                                 (valid-chunk-type-slot type x))
                               slots)))
               (print-warning "Cannot declare usage for buffer ~s because the slots (~{~s~^ ~}) are not valid for chunk-type ~s." 
                              buffer (remove-if (lambda (x) 
                                                  (valid-chunk-type-slot type x))
                                                slots)
                              type))
              (t
               (when (or (eq slots :all)
                         (and (listp slots) (= (length slots) 1) (eq (car slots) :all)))
                   (setf slots (chunk-type-possible-slot-names-fct type)))
               (dolist (s slots) (push s (gethash buffer (procedural-cond-style-usage-table procedural))))
               (dolist (s slots) (push s (gethash buffer (procedural-init-chunk-slots procedural))))
               t))
      (print-warning "No procedural module found.  Cannot declare buffer usage."))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A trace filter to restrict things to production firing only.

(defun production-firing-only (event)
  "Filter to show only production firing in the trace"
  (eq (evt-action event) 'production-fired))

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
