;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell & Mike Byrne
;;; Copyright   : (c) 2004-10 Dan Bothell/Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : misc-utils.lisp
;;; Version     : 2.0
;;; 
;;; Description : Various useful functions that don't seem to belong anywhere
;;;               else (many of which come directly from the old RPM code).
;;; 
;;; Bugs        : * The "-output" commands break if passed strings with format
;;;             :   control sequences in them.
;;;
;;; To do       : * Add most if not all of these to the official API.
;;;             : * Add all the ones copied from rpm to the public API section.
;;;             : * Handle different warning levels.
;;;             : * Fix an issue with MCL double printing model warnings because
;;;             :   *error-output* isn't the same as *standard-output* even 
;;;             :   though both print to the listener - grrr.
;;;             : * Possibly split the "-output" commands into macros and functions
;;;             :   like everything else because right now they're funny in that
;;;             :   they're macros but they do result in evaluating the parameters.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.12 Dan
;;;             : Creation
;;; 2005.01.17 Dan
;;;             : * Updated model-output and added command-output. I don't think
;;;             :   it needs the no-output command, but maybe it will.
;;; 2005.02.10 Dan
;;;             : * Added the expt-coerced, log-coerced, and sqrt-coerced 
;;;             :   for performance improvements in Lisps that use doubles when 
;;;             :   the default read format is singles.  
;;; 2005.02.21 Dan
;;;             : * Added some to do stuff.
;;; 2005.02.28 Dan
;;;             : * Note that the -coerced macros don't really do anything 
;;;             :   in MCL 5 or ACL, so probalby not necessary anymore...
;;;             : * Made a bunch of the output macros hygienic.
;;;             : * Added back meta-p-output to print to all models' :v stream
;;;             :   but only once per stream.
;;; 2005.04.14 Dan
;;;             : * Made use of printing-module-suppress-cmds in no-output and
;;;             :   command-output to fix an issue with reading :cmdt in the
;;;             :   context of a no-output.
;;; 2005.05.11 Dan
;;;             : * Added *one-stream-hack* to be used in model-warning to
;;;             :   get around an issue in MCL with it doubling warnings due
;;;             :   to *error-output* not equaling *standard-output* even
;;;             :   though they're the same place.  
;;;             :   NOT a good solution, but makes things look nicer for now...
;;; 2005.07.22 mdb
;;;             : * Changed WITHIN, GREATER-THAN, and LESS-THAN to return NIL
;;;             :   if compared against non-numbers.
;;;             : * Added NOT-EQUAL function to support negations.
;;; 2005.08.10 Dan
;;;             : * Changed no-output because it didn't need return0val and it
;;;             :   generated a warning.
;;;             : * Updated version to 1.0.
;;; 2005.09.08 Dan
;;;             : * Added support for a new paramter called :model-warnings
;;;             :   in the printing module.  When it is nil all the calls to
;;;             :   model-warning result in no output.
;;; 2005.10.19 Dan
;;;             : * Changed dovector slightly to assign an initial value of
;;;             :   nil to the variable in the let explicitily to get around
;;;             :   an issue with that in CMUCL.
;;; 2005.10.21 Dan
;;;             : * Doh! Realized the problem isn't the let issue but that 
;;;             :   CMUCL already defines dovector and doesn't like overwritting
;;;             :   it.  Fixed that now.
;;; 2006.03.13 Dan
;;;             : * Fixed no-outupt because it could fail when there were
;;;             :   nested calls.
;;; 2006.05.22 Dan
;;;             : * Noticed that Mike isn't listed as an author even though
;;;             :   many of these come from his older files.
;;; 2006.06.29 Dan
;;;             : * Added components provided by Don Morrison to allow it to be 
;;;             :   loaded into CLisp v2.38 - just added clisp to the switch to
;;;             :   not define this method (defmethod random-item ((seq vector)).
;;; 2006.07.12 Dan
;;;             : * Modified meta-p-output so that it always returns nil.
;;; 2006.09.08 Dan
;;;             : * Cleaned up the definition of posnum and added a corresponding
;;;             :   nonneg because zero isn't positive and there are situations
;;;             :   where that distinction matters (and those modules are now
;;;             :   also being updated to use nonneg).
;;; 2008.02.04 Dan
;;;             : * Adding the suppress-warnings command to provide a quick way
;;;             :   to eliminate all warnings.  Works like no-output i.e. wrap
;;;             :   it around the code you don't want printing warnings.
;;; 2008.02.28 Dan
;;;             : * Had to adjust how suppress-warnings works to comply with
;;;             :   the strict interpretation of the CL spec in SBCL.
;;; 2008.05.02 Dan
;;;             : * Fixed a bug with suppress-warnings because it was restoring
;;;             :   the model-warnings parameter with the wrong parameter's value.
;;; 2008.08.01 Dan
;;;             : * Changed the append in splice-into-list-des to nconc to 
;;;             :   actually make it destructive - splice-into-list copies it
;;;             :   and works non-destructive if needed.
;;; 2008.08.06 Dan
;;;             : * Changed fctornil to only call fboundp with symbols so that
;;;             :   it doesn't throw an error in such cases.
;;; 2008.08.20 Dan
;;;             : * Added the capture-model-output command.  It works like 
;;;             :   no-output except that it saves the suppressed output
;;;             :   and returns it in a string.
;;; 2008.10.09 Dan
;;;             : * Neq and dovector are already defined in the newest OpenMcl 
;;;             :   (Clozure Common Lisp) so adding a switch for those.
;;; 2010.03.27 Dan
;;;             : * Changed the safe[> | < | >= |<=] testers so that they will
;;;             :   always return t or nil instead of a general true.
;;; 2010.05.03 Dan
;;;             : * Modified model-warning so that it prints the name of the 
;;;             :   model in the warning if there is more than one model defined.
;;; 2010.05.13 Dan
;;;             : * Rewrote push-last so that it uses a macro defined with
;;;             :   define-modify-macro to do the setting to avoid the need to
;;;             :   evaluate place twice.  It's a minor performance improvement 
;;;             :   but it does add up in longer runs.
;;; 2010.06.04 Dan
;;;             : * Changed the nconcf macro in the updated push-last since
;;;             :   that name conflicts with an internal name in LispWorks.
;;; 2010.09.01 Dan
;;;             : * Changed ms-round to use fround so that the floating point
;;;             :   type of its parameter is preserved.
;;;             : * Added variables to track the current and available floating
;;;             :   point types and their sizes.
;;; 2010.10.06 mdb
;;;             : * Added function POSNUMORBOOL for use by incremental mouse
;;;             :   moves.
;;; 2010.11.03 Dan
;;;             : * Added seconds->ms and ms->seconds functions to go along with 
;;;             :   the conversion of the internal clock to be an integer count 
;;;             :   of ms.
;;;             : * Changed *time-size-current-limit* to be in ms since that's
;;;             :   what it will be comapred to if I put that back into the
;;;             :   scheduling code.
;;; 2011.02.11 Dan
;;;             : * Added a splice-into-position-des which works like splice-into-
;;;             :   list-des except that it doesn't "unpack" a list item being
;;;             :   spliced in.
;;; 2011.04.27 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions.
;;; 2011.04.28 Dan
;;;             : * Changed a declare ignore to ignorable to suppress an ACL
;;;             :   warning.
;;; 2011.09.13 Dan
;;;             : * Have CCL also set the *one-stream-hack* fix to prevent double
;;;             :   warnings.
;;; 2012.02.09 Dan
;;;             : * Explicitly close streams made with make-string-output-stream 
;;;             :   to be safe.
;;; 2012.06.12 Dan
;;;             : * Changed suppress-warnings so that it doesn't print the warning
;;;             :   when it is used outside of a model and always evaluates the
;;;             :   forms provided.
;;; 2012.08.08 Dan
;;;             : * Changed meta-p-output so that it uses meta-p-model-order
;;;             :   for consistency (which matters since for now the arguments
;;;             :   are evaluated each time the output is performed so something
;;;             :   like (incr x) in the output will result in each different output
;;;             :   stream getting a different value).
;;; 2012.09.28 Dan
;;;             : * Changed print-warning so that if there are multiple models
;;;             :   defined and there is a current-model it displays that model in 
;;;             :   the warning the same way model-warning does.
;;; 2012.10.02 Dan
;;;             : * Added declaims so the new print-warning doesn't trigger a bunch
;;;             :   of warnings in functions that use it.
;;; 2012.12.19 Dan
;;;             : * Changed dist from a method to a function so it can handle
;;;             :   any combination of sequences of numbers, extended it so that
;;;             :   it works for an arbitrary number of dimensions, and removed
;;;             :   the coersion from the math since it doesn't seem to matter
;;;             :   anymore.
;;; 2013.01.04 Dan
;;;             : * Added an unwind-protect to meta-p-output so that it makes sure
;;;             :   to restore the current model correctly.
;;;             : * Added a cannot-define-model to meta-p-output as well.
;;; 2013.01.07 Dan
;;;             : * Added a check for a current meta-process to print-warning to
;;;             :   avoid a problem with checking for the current model.
;;; 2014.03.21 Dan
;;;             : * Moved circular-references here. 
;;; 2014.03.27 Dan
;;;             : * Moved replace-variables here.
;;; 2014.04.07 Dan
;;;             : * Moved chunk-spec-variable-p here as well.
;;; 2014.05.20 Dan
;;;             : * Moved objs-max-val here from vision.
;;; 2014.05.21 Dan
;;;             : * Added an objs-min-val as well.
;;;             : * Added a pz macro.
;;; 2014.06.18 Dan
;;;             : * Added test to dist to make sure that it ignores non-numbers
;;;             :   in the vectors.
;;; 2014.09.29 Dan
;;;             : * Changed string-to-lines from a defmethod to defun since it
;;;             :   isn't called with different object types anyway.
;;; 2014.10.14 Dan
;;;             : * Change seconds->ms to multiply by 1000 instead of using 
;;;             :   round with .001 because of issues with converting large
;;;             :   times: (round (* 24 60 60) .001) doesn't result in 86400000
;;;             :   in some Lisps for example.
;;; 2015.06.01 Dan
;;;             : * Added the one-time-model-warning command which is like model-
;;;             :   warning except that there's an a parameter before the control
;;;             :   string and args which is the "tag".  The first time a warning 
;;;             :   with that tag is called since reset the warning is printed,
;;;             :   but all subsequent calls will be ignored and result in no
;;;             :   output and will also not evaluate the args.
;;;             : * Changed the "time-size" testing to check for accuracy at the
;;;             :   1 millisecond level by default instead of 50ms as was done
;;;             :   before since it's acutally going to be used again with the
;;;             :   *seconds->ms-accuracy-limit* variable which holds the approximate
;;;             :   time in seconds beyond which the current floating point format
;;;             :   may not accurately represent 1 ms.
;;;             : * Added the safe-seconds->ms command which tests the value
;;;             :   provided against *seconds->ms-accuracy-limit* and issues a
;;;             :   one time warning if the provided seconds value is above that
;;;             :   limit.
;;; 2015.06.02 Dan
;;;             : * Added an optional parameter to safe-seconds->ms to provide a
;;;             :   name that will be included in the warning.
;;;             : * Check the absolute value of the time since something like
;;;             :   creation time for declarative items is sometimes specified
;;;             :   as a negative.
;;; 2015.06.03 Dan
;;;             : * Some more adjustments to safe-seconds->ms and the variables
;;;             :   that were holding the time accuracy info.  Now *time-size-test-list*
;;;             :   is just an alist of a type and the actual number for the limit 
;;;             :   calculated directly from the float info instead of by a
;;;             :   search and safe-seconds->ms considers the specific type of the 
;;;             :   number it was passed instead of assuming the default floating point
;;;             :   format so that one can use an appropriate float format for a
;;;             :   particular number without having to change the default and
;;;             :   recompile everything i.e. (run-until-time 10000.5d0).
;;;             : * Added a declaim for get-module-fct since that's called because
;;;             :   of the warning in safe-seconds->ms.
;;; 2015.06.08 Dan
;;;             : * New format function for printing a millisecond time in seconds
;;;             :   without loss of accuracy that converting to a float might have.
;;;             :   It is used like this: (format t "~10/print-time-in-seconds/" ms-time)
;;;             :   The only modifier it uses is the first one which specifies the min width.
;;; 2016.05.31 Dan
;;;             : * Added finish-output to print-warning and model-warning since
;;;             :   at least one Lisp seems to buffer *error-output* for some 
;;;             :   crazy reason!
;;;             : * Using the finish-format macro instead now since there are
;;;             :   other places that have the same issue.
;;; 2016.06.24 Dan
;;;             : * Create a single 'dummy' broadcast stream to use for suppress-
;;;             :   warnings instead of creating a fresh one each time.
;;; 2016.11.16 Dan [2.0]
;;;             : * Rework how the output macros operate to work with the central
;;;             :   dispatch system.
;;;             :   Now, when the underlying parameter is t (:v or :cmdt) and for
;;;             :   all the warning output instead of writing directly to that
;;;             :   stream (*standard-output* or *error-output*) it sends
;;;             :   the string to a "trace" command which could be monitored by
;;;             :   anyone that wants to record/output that information.  
;;;             :   If the parameter is a file or stream however all the output
;;;             :   still goes there instead.
;;;             :   Also adding commands so remote systems can send output to
;;;             :   model, command, or warning systems.
;;; 2016.12.01 Dan
;;;             : * Added string->name which returns (intern (string-upcase <>))
;;;             :   if it's passed a string otherwise just return the passed value.
;;; 2017.01.20 Dan
;;;             : * Updated one-time-model-warning to remove the unneeded let.
;;; 2017.01.26 Dan
;;;             : * Added string->number.
;;; 2017.02.08 Dan
;;;             : * Removed an unnecessary gensym from one-time-model-warning.
;;; 2017.02.24 Dan
;;;             : * Suppress-warnings doesn't work anymore since they aren't
;;;             :   being written to *error-output*.  Replacing that with a 
;;;             :   capture and return mechanism.  Call suppress-and-capture-warnings
;;;             :   to shunt all warnings into a list and prevent their dispatching.
;;;             :   Only one entity can suppress warnings at a time and suppress-...
;;;             :   will return a unique tag if you are the one that actually
;;;             :   suppressed them.  To reenable their output you need to call 
;;;             :   unsuppress-warnings and provide the tag you got when suppressing
;;;             :   them, and if the optional is true it will return the list of the
;;;             :   warnings which were captured.  
;;; 2017.03.08 Dan
;;;             : * All the coordinate code now accepts any sequence instead of
;;;             :   requiring vectors or coercing everything to vectors.
;;; 2017.03.30 Dan
;;;             : * Moved print-warning to the structures file.
;;;             : * Changed meta-p-current-model usage to *current-act-r-model*.
;;; 2017.03.31 Dan
;;;             : * Removed the cannot-define-model macro from meta-p-output.
;;; 2017.05.09 Dan
;;;             : * Print-time-in-seconds now rounds the time it's passed because
;;;             :   some of those times can be floats (they shouldn't but that's
;;;             :   more things to fix), and it also handles negative numbers
;;;             :   better.
;;; 2017.06.15 Dan
;;;             : * Make the output commands thread safe.
;;; 2017.07.13 Dan
;;;             : * Add the act-r-output command which sends to the general-trace.
;;;             :   It has no connection to printing module and just sends the
;;;             :   output directly to the trace.
;;;             : * Moved the print-error-message functions here.
;;; 2017.08.30 Dan
;;;             : * Added capture-command-output macro for use internally for now.
;;;             :   It could capture 'unwanted' output because anything that 
;;;             :   outputs to the command stream for that model will be captured
;;;             :   not just those commands wrapped in the macro, but until I
;;;             :   come up with a better option I'm using this...
;;; 2017.09.07 Dan
;;;             : * Adjusted string->name to deal with keywords encoded in strings
;;;             :   better.
;;; 2017.10.20 Dan
;;;             : * Adjusted string->name again to have it return numbers as a
;;;             :   number instead of interning them as symbols.
;;; 2018.02.02 Dan
;;;             : * Removed capture-model-output and capture-command-output.
;;;             : * Also removed *one-stream-hack*!
;;;             : * Fixed one-time-model-output since it held a lock while
;;;             :   making the call to the dispatched command which also needed
;;;             :   the lock.
;;;             : * Actually still need capture-command-output...
;;; 2018.06.11 Dan
;;;             : * Add a safety precaution to string->name and turn off *read-eval*
;;;             :   when checking for a number.
;;; 2018.09.17 Dan
;;;             : * Moved color->name here.
;;; 2019.01.15 Dan
;;;             : * Minor tweak to chunk-spec-variable-p to avoid converting the
;;;             :   symbol to a string twice.
;;; 2019.02.05 Dan
;;;             : * Adjustments because meta-p-models is now an alist.
;;; 2019.04.15 Dan
;;;             : * Use the truely destructive splice-into-position-des.
;;; 2020.01.09 Dan
;;;             : * Added set-or-remove-hook-parameter which can be used to
;;;             :   simplify the parameter handlers.  It takes the parameter
;;;             :   name, current list, and given value and returns the new
;;;             :   item list (either added, removed, or unchanged).  The 
;;;             :   assumption is that they should always return the current
;;;             :   setting instead of nil on 'failure'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; While
;;; (defmacro while (test &body body)
;;; 
;;; test a form to evaluate
;;; body any number of forms
;;;
;;; while the test evaluates to a non-nil value continue to evaluate the forms
;;; of the body.
;;;
;;; returns nil.
;;;
;;; Push-last
;;; (defmacro push-last (item place)
;;; 
;;; item anything
;;; place a Lisp place
;;;
;;; push-last postpends item to the list that is stored in place and stores the 
;;; resulting list in place.
;;;
;;; returns place. 
;;;
;;; Print-warning
;;; (defmacro print-warning (control-string &rest args))
;;;
;;; control-string is a control-string as would be passed to the format function
;;; args are the arguments to use in that control string
;;;
;;; control-string and args are passed to format on the stream *error-output* 
;;; with the text "#|Warning: " proceeding it and "|#" after it so that it would 
;;; appear as a comment if the stream were to be read.
;;;
;;; nil is returned.  
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


(declaim (ftype (function (t) t) act-r-random))
(declaim (ftype (function () t) current-model))
(declaim (ftype (function () t) mp-models))
(declaim (ftype (function (t) t) get-module-fct))
(declaim (ftype (function (t &rest t) t) evaluate-act-r-command))

;;; WHILE      [Macro]
;;; From _On Lisp_.
;;; Already defined in ACL with the IDE or all versions 6.0 or newer.

;;; (defmacro while (test &body body)
;;; 
;;; test a form to evaluate
;;; body any number of forms
;;;
;;; while the test evaluates to a non-nil value continue to evaluate the forms
;;; of the body.
;;;
;;; returns nil.

#-(or :allegro-ide (and :allegro-version>= (version>= 6)))
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))


;;; AIF      [Macro]
;;; Date        : 97.02.09
;;; Description : From _On Lisp_, anaphoric if.  That is, can use variable
;;;             : "it" to refer to result of the test-form.

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))


(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))



;;; push-last
;;;
;;; (defmacro push-last (item place)
;;; 
;;; item anything
;;; place a Lisp place
;;;
;;; push-last postpends item to the list that is stored in place and stores the 
;;; resulting list in place.
;;;
;;; returns place. 
;;;


(define-modify-macro  act-r_nconcf (&rest args) nconc)

(defmacro push-last (item place)
  `(act-r_nconcf ,place (list ,item)))

;(defmacro push-last (item place)
;  `(setf ,place (nconc ,place (list ,item))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; expt-coerced, exp-coerced, log-coerced, and sqrt-coerced
;;;
;;; These are for improved speed in Lisps that use doubles for math so that
;;; it can coerce them to single when that's the setting of 
;;; *read-default-float-format*.  Really makes a difference in MCL...
;;; Actually, in MCL 5 it makes no difference, and may want to just 
;;; eliminate this...

(defmacro expt-coerced (base power)
  "Computes expt and coerce to *read-default-float-format* if needed"
  (if (typep (expt 1.0 1.0) *read-default-float-format*)
    `(expt ,base ,power)
    `(coerce (expt ,base ,power) ,*read-default-float-format*)))

(defmacro exp-coerced (arg)
  "Computes expt and coerce to *read-default-float-format* if needed"
  (if (typep (expt 1.0 1.0) *read-default-float-format*)
    `(exp ,arg)
    `(coerce (exp ,arg) ,*read-default-float-format*)))

(defmacro log-coerced (arg &optional (base nil basep))
  "Computes log and coerce to *read-default-float-format* if needed
   doesn't accept a base however"
  (if (typep (log 1.0) *read-default-float-format*)
      (if basep
          `(log ,arg ,base)
        `(log ,arg))
    (if basep
        `(coerce (log ,arg ,base) ,*read-default-float-format*)
      `(coerce (log ,arg) ,*read-default-float-format*))))

(defmacro sqrt-coerced (arg)
  "Computes sqrt and coerce to *read-default-float-format* if needed"
  (if (typep (sqrt 2.0) *read-default-float-format*)
    `(sqrt ,arg)
    `(coerce (sqrt ,arg) ,*read-default-float-format*)))



(defun hash-table-keys (ht)
  "Return the list of current keys in a hash-table"
  (let ((keys nil))
    (maphash #'(lambda (key val)
                 (declare (ignore val))
                 (push key keys))
             ht)
    keys))


(defun ms-round (x)
  "Rounds a time to the nearest millisecond"
  (declare (number x))
  (/ (fround (* x 1000)) 1000.0))

(defun seconds->ms (x)
  "Rounds a time in seconds to an integer number of milliseconds"
  (round (* x 1000)))


(defun ms->seconds (x)
  "Converts an integer number of milliseconds to a floating point number of seconds"
  (/ x 1000.0))

(defun fctornil (x)
  "Checks if a symbol is a function, function name, or nil"
  (or (null x)
      (functionp x)
      (and (symbolp x) (fboundp x))))

(defun tornil (x)
  "Checks if a symbol is T or NIL"
  (or (eq x t) (eq x nil)))

(defun posnum (x)
  "Returns true only if <x> is a positive number"
  (and (numberp x) (plusp x)))

(defun nonneg (x)
  "Returns true only if <x> is a non-negative number"
  (and (numberp x) (>= x 0.)))


(defun numornil (x)
  "Returns true only if <x> is a number or nil"
  (or (null x) (numberp x)))

(defun posnumornil (x)
  "Returns true only if <x> is a positive number or nil"
  (or (null x) (posnum x)))

(defun nonnegornil (x)
  "Returns true only if <x> is a non-negative number or nil"
  (or (null x) (nonneg x)))

(defun numorbool (x)
  "Returns true only if <x> is a number, T or nil"
  (or (tornil x) (numberp x)))

(defun posnumorbool (x)
  "Returns true only if <x> is a positive number, T or nil"
  (or (tornil x) (posnum x)))

(defun safe> (val1 val2)
  "Return t if val1 and val2 are numbers and val1 > val2"
  (and (numberp val1) (numberp val2) (> val1 val2) t))

(defun safe>= (val1 val2)
  "Return t if val1 and val2 are numbers and val1 >= val2"
  (and (numberp val1) (numberp val2) (>= val1 val2) t))

(defun safe< (val1 val2)
  "Return t if val1 and val2 are numbers and val1 < val2"
  (and (numberp val1) (numberp val2) (< val1 val2) t))

(defun safe<= (val1 val2)
  "Return t if val1 and val2 are numbers and val1 <= val2"
  (and (numberp val1) (numberp val2) (<= val1 val2) t))


;;; SPLICE-INTO-LIST      [Function]
;;; Date        : 97.01.15
;;; Description : 

(defun splice-into-list (lis position item)
  (let ((temp (copy-list lis)))
    (splice-into-list-des temp position item)))
      
    
;;; SPLICE-INTO-LIST-DES      [Function]
;;; Date        : 97.01.15
;;; Description : 

(defun splice-into-list-des (lis position item)
  (if (= position 0)
      (push item lis)
    (if (listp item)
        (nconc (subseq lis 0 position) item (nthcdr position lis))   
      (nconc (subseq lis 0 position) (list item) (nthcdr position lis)))))
  
#|
;;; splice-into-position-des
;;; Doesn't unpack a list like splice-into-list-des does.

(defun splice-into-position-des (lis position item)
  (if (= position 0)
      (push item lis)
    (nconc (subseq lis 0 position) (list item) (nthcdr position lis))))

doesn't seem to be any better...

|#



(defun splice-into-position-des (l p i)
  (if (zerop p)
      (rplaca (rplacd (nthcdr 0 l) (cons (car l) (nthcdr 1 l))) i)
    (rplacd (nthcdr (1- p) l) (cons i (nthcdr p l)))))



;;; MKLIST      [Function]
;;; Description : From Graham's _On Lisp_, make sure we have a list.

(defun mklist (obj)
  "If the object is not a list, return a list containing the object"
  (if (listp obj) obj (list obj)))


;;; Theoretically, these are part of the printing module, but 
;;; since they are macros that are used by lots of the internal
;;; functions they need to be defined early in the loading.

(defmacro act-r-output (control-string &rest args)
  `(progn
     (evaluate-act-r-command "general-trace" (format nil "~@?~%" ,control-string ,@args))
     nil))

(defun act-r-output-internal (string)
  (evaluate-act-r-command "general-trace" (format nil "~a~%" string))
  nil)


(defmacro model-output (control-string &rest args)
  (let ((module (gensym))
        (present (gensym))
        (stream (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present 
         (let (,stream)
           (bt:with-recursive-lock-held ((printing-module-param-lock ,module))
             (setf ,stream (act-r-output-stream (printing-module-v ,module))))
           (when ,stream
             (evaluate-act-r-command "model-output" (format nil "~@?" ,control-string ,@args)))))
       nil)))

(defun model-output-internal (string)
  (multiple-value-bind (module present)
      (get-module-fct 'printing-module)
    (when present 
      (let (stream)
        (bt:with-recursive-lock-held ((printing-module-param-lock module))
          (setf stream (act-r-output-stream (printing-module-v module))))
        (when stream
          (if (eq t stream)
              (evaluate-act-r-command "model-trace" (format nil "~&~a~%" string))
            (format stream "~&~a~%" string))))))
  nil)


(defmacro command-output (control-string &rest args)
  (let ((module (gensym))
        (present (gensym))
        (suppress (gensym))
        (stream (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present 
         (let (,suppress ,stream)
           (bt:with-recursive-lock-held ((printing-module-param-lock ,module))
             (setf ,suppress (printing-module-suppress-cmds ,module))
             (setf ,stream (act-r-output-stream (printing-module-c ,module))))
           (when (and ,stream (not ,suppress))
             (evaluate-act-r-command "command-output" (format nil "~@?" ,control-string ,@args)))))
       nil)))

(defun command-output-internal (string)
  (multiple-value-bind (module present)
      (get-module-fct 'printing-module)
    (when present 
      (let (suppress stream)
        (bt:with-recursive-lock-held ((printing-module-param-lock module))
          (setf suppress (printing-module-suppress-cmds module))
          (setf stream (act-r-output-stream (printing-module-c module))))
        (when (and stream (not suppress))
          (if (eq t stream)
              (evaluate-act-r-command "command-trace" (format nil "~&~a~%" string))
            (format stream "~&~a~%" string))))))
  nil)


(defmacro no-output (&rest commands)
  "Suppress command output while evaluating ACT-R commands"
  (let ((module (gensym))
        (present (gensym))
        (current (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present
         (let (,current)
           (bt:with-recursive-lock-held ((printing-module-param-lock ,module))
             (setf ,current (printing-module-suppress-cmds ,module))
             (setf (printing-module-suppress-cmds ,module) t))
           (unwind-protect 
               (progn ,@commands)
             (bt:with-recursive-lock-held ((printing-module-param-lock ,module))
               (setf (printing-module-suppress-cmds ,module) ,current))))))))


(defmacro suppress-warnings (&rest commands)
  "Suppress all ACT-R warnings while evaluating ACT-R commands"
  (let ((key (gensym)))
    `(let ((,key (suppress-and-capture-warnings)))
        (unwind-protect 
            (progn ,@commands)
          (unsuppress-warnings ,key)))))


(defun suppress-and-capture-warnings ()
  (bt:with-recursive-lock-held ((printing-module-lock *act-r-warning-capture*))
    (if (printing-module-capture-warnings *act-r-warning-capture*)
        nil
      (let ((key (symbol-name (gensym))))
        (setf (printing-module-capture-warnings *act-r-warning-capture*) key)
        (setf (printing-module-captured-warnings *act-r-warning-capture*) nil)
        key))))

(defun unsuppress-warnings (key &optional return)
  (bt:with-recursive-lock-held ((printing-module-lock *act-r-warning-capture*))
    (if (and (stringp key) (string-equal key (printing-module-capture-warnings *act-r-warning-capture*)))
        (prog1
            (if return
                (printing-module-captured-warnings *act-r-warning-capture*)
              nil)
          (setf (printing-module-capture-warnings *act-r-warning-capture*) nil)
          (setf (printing-module-captured-warnings *act-r-warning-capture*) nil))
      nil)))

#|
(defmacro capture-model-output (&rest commands)
  "Return the string of all model output from commands"
  (let ((module (gensym))
        (present (gensym))
        (current-v (gensym))
        (current-c (gensym))
        (out-stream (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present
         (let ((,out-stream (make-string-output-stream))
               (,current-v (printing-module-v ,module))
               (,current-c (printing-module-c ,module)))
                         
           (setf (printing-module-v ,module) (make-act-r-output :stream ,out-stream))
           (setf (printing-module-c ,module) (make-act-r-output :stream ,out-stream))
           
           (unwind-protect 
               (progn ,@commands)
             (progn
               (setf (printing-module-v ,module) ,current-v)
               (setf (printing-module-c ,module) ,current-c)))
           (prog1 
               (get-output-stream-string ,out-stream)
             (close ,out-stream)))))))






;;; Put this in for now because while the output goes to the 
;;; same place, the streams aren't equal between *error-output*
;;; and *standard-output* so it ends up doubling the model warnings.
;;; I do NOT like this solution, but for now it's the easiest/only
;;; way I can come up with.
;;;
;;; Not writing directly to *error-output* anymore so this isn't needed,
;;; but leaving it here for now just because there are other
;;; things which check it that aren't being updated yet.

(defparameter *one-stream-hack* #+(or :digitool :ccl) t
                                #-(or :digitool :ccl) nil)

|#

(defmacro capture-command-output (&rest commands)
  "Return the string of all command output from commands"
  (let ((module (gensym))
        (present (gensym))
        (current-c (gensym))
        (out-stream (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present
         (let ((,out-stream (make-string-output-stream))
               (,current-c (printing-module-c ,module)))
                         
           (setf (printing-module-c ,module) (make-act-r-output :stream ,out-stream))
           
           (unwind-protect 
               (progn ,@commands)
             (progn
               (setf (printing-module-c ,module) ,current-c)))
           (prog1 
               (get-output-stream-string ,out-stream)
             (close ,out-stream)))))))

(defmacro model-warning (control-string &rest args)
  (let ((module (gensym))
        (present (gensym))
        (test (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present
         (let (,test)
           (bt:with-recursive-lock-held ((printing-module-param-lock ,module))
             (setf ,test (and (act-r-output-stream (printing-module-v ,module)) (printing-module-model-warnings ,module))))
           (when ,test
             (evaluate-act-r-command "model-warning" (format nil "~@?" ,control-string ,@args)))))
       nil)))

(defun model-warning-internal (string)
  (multiple-value-bind (module present)
      (get-module-fct 'printing-module)
    (when present
      (bt:with-recursive-lock-held ((printing-module-lock module))
        (bt:with-recursive-lock-held ((printing-module-param-lock module))
          (when (act-r-output-stream (printing-module-v module))
            (let ((stream (act-r-output-stream (printing-module-v module))))
              (cond ((null (printing-module-model-warnings module))
                     ;; if warnings disabled ignore
                     )
                    ((printing-module-capture-warnings module)
                     ;; warnings are being suppressed so just record them
                     (push-last string (printing-module-captured-warnings module)))
                    ((eq t stream)
                     ;; just send it to the warning-trace 
                     (evaluate-act-r-command "warning-trace" (format nil "~&#|Warning~:[~*~; (in model ~a)~]: ~a |#~%" 
                                                               (> (length (mp-models)) 1) (current-model) string)))
                    (t
                     ;; it's a custom stream so send it there and to the warning-trace
                     
                     (let ((out (format nil "~&#|Warning~:[~*~; (in model ~a)~]: ~a |#~%" 
                                  (> (length (mp-models)) 1) (current-model) string)))
                       
                       (evaluate-act-r-command "warning-trace" out)
                       
                       (format stream out))))))))))
  nil)


(defmacro one-time-model-warning (tag control-string &rest args)
  (let ((module (gensym))
        (present (gensym))
        (output (gensym)))
    `(multiple-value-bind (,module ,present)
         (get-module-fct 'printing-module)
       (when ,present
         (let ((,output nil))
           (bt:with-recursive-lock-held ((printing-module-param-lock ,module))
             (when (and (act-r-output-stream (printing-module-v ,module)) (printing-module-model-warnings ,module))
               (setf ,output (not (find ,tag (printing-module-one-time-tags ,module) :test 'equal)))))
           (when ,output (evaluate-act-r-command "one-time-model-warning" ,tag (format nil "~@?" ,control-string ,@args)))))
       nil)))

(defun one-time-model-warning-internal (tag string)
  (multiple-value-bind (module present)
      (get-module-fct 'printing-module)
    (when present
      (bt:with-recursive-lock-held ((printing-module-lock module))
        (bt:with-recursive-lock-held ((printing-module-param-lock module))
          (when (act-r-output-stream (printing-module-v module))
            (let ((stream (act-r-output-stream (printing-module-v module))))
              (unless (find tag (printing-module-one-time-tags module) :test 'equal)
                (push tag (printing-module-one-time-tags module))
                (cond ((null (printing-module-model-warnings module))
                     ;; if warnings disabled ignore
                     )
                    ((printing-module-capture-warnings module)
                     ;; warnings are being suppressed so just record them
                     (push-last string (printing-module-captured-warnings module)))
                    ((eq t stream)
                     ;; just send it to the warning-trace 
                     (evaluate-act-r-command "warning-trace" (format nil "~&#|Warning~:[~*~; (in model ~a)~]: ~a |#~%" 
                                                               (> (length (mp-models)) 1) (current-model) string)))
                    (t
                     ;; it's a custom stream so send it there and to the warning-trace
                     
                     (let ((out (format nil "~&#|Warning~:[~*~; (in model ~a)~]: ~a |#~%" 
                                  (> (length (mp-models)) 1) (current-model) string)))
                       
                       (evaluate-act-r-command "warning-trace" out)
                       
                       (format stream out)))))))))))
  nil)




(defmacro meta-p-output (control-string &rest args)
  (let ((module (gensym))
        (present (gensym))
        (stream (gensym))
        (used-streams (gensym))
        (m (gensym))
        (mp (gensym))
        (order (gensym)))
    `(let ((,used-streams nil)
           (,mp (current-mp))
           ,order)
       (bt:with-lock-held ((meta-p-models-lock ,mp))
         (setf ,order (meta-p-model-order ,mp)))
       (dolist (,m ,order)
         (let ((*current-act-r-model* (cdr (assoc ,m (bt:with-lock-held ((meta-p-models-lock ,mp)) (meta-p-models ,mp))))))
           (multiple-value-bind (,module ,present)
               (get-module-fct 'printing-module)
             (when ,present
               (let (,stream)
                 (bt:with-recursive-lock-held ((printing-module-param-lock ,module))
                   (setf ,stream (act-r-output-stream (printing-module-v ,module))))
                 
                 (when ,stream
                   (unless (member ,stream ,used-streams)
                     (push ,stream ,used-streams)
                     (if (eq t ,stream)
                         (evaluate-act-r-command "model-output" (format nil "~@?" ,control-string ,@args))
                       (format ,stream "~&~@?~%" ,control-string ,@args)))))))))
       nil)))


;; don't exponse meta-p-output remotely for now 


(defun rad->deg (r)
  "Converts radians into degrees."
  (declare (number r))
  (* r (/ 180 pi)))


(defun deg->rad (d)
  "Converts degrees into radians."
  (declare (number d))
  (* (/ pi 180) d))

(defmacro px (vpt)
  "X coordinate of an XY sequence."
  `(elt ,vpt 0))

(defmacro py (vpt)
  "Y coordinate of an XY sequence."
  `(elt ,vpt 1))

(defmacro pz (vpt)
  "Z coordinate of an XYZ sequence."
  `(elt ,vpt 2))

(defmacro vr (vrt)
  "R component of an r-theta vector."
  `(elt ,vrt 0))

(defmacro vtheta (vrt)
  "Theta component of an r-theta vector."
  `(elt ,vrt 1))


(defun vpt= (vpt1 vpt2)
  (and (= (px vpt1) (px vpt2))
       (= (py vpt1) (py vpt2))))

(defun round-xy (loc)
  (map 'vector #'round loc))


(defun polar-move-xy (loc move)
  (round-xy
   (list (+ (px loc) (* (px move) (cos (py move))))
         (+ (py loc) (* (px move) (sin (py move)))))))


;;; DIST      [Function]
;;; Description : Computes the distance between two points in N-dimensional space
;;; loc1 and loc2 which can each be any sequence of coordnates and N is the size 
;;; smaller of the sequences.

(defun dist (loc1 loc2)
  (sqrt (reduce '+ (map 'vector (lambda (x1 x2)
                                  (if (and (numberp x1) (numberp x2))
                                      (expt (- x1 x2) 2)
                                    0))
                     loc1 loc2) :initial-value 0)))


(defgeneric objs-match-slotval (lst slot-name value)
  (:documentation 
   "Takes a list of CLOS objects and returns a list containing those items which have the slot <slot-name> equal to <value>."))

(defmethod objs-match-slotval ((ls list) (slot-name symbol) value)
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (equal value (slot-value obj slot-name))
          (push obj accum))))))


(defmethod objs-match-slotval ((ls list) (slot-name symbol) 
                                  (value number))
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (= value (slot-value obj slot-name))
          (push obj accum))))))


(defmethod objs-match-slotval ((ls list) (slot-name symbol) 
                                  (value symbol))
  (when ls
    (let (accum)
      (dolist (obj ls (nreverse accum))
        (when (eq value (slot-value obj slot-name))
          (push obj accum))))))


(defgeneric objs-min-slotval (lst slot-name)
  (:documentation 
   "Given a list of CLOS objects and a slot name, return a list containing the object(s) with the lowest value for that slot."))

(defmethod objs-min-slotval ((ls list) (slot-name symbol))
  (when ls
    (let ((best (slot-value (first ls) slot-name))
          (current nil)
          (out-ls (list (first ls))))
      (dolist (obj (rest ls) (nreverse out-ls))
        (setf current (slot-value obj slot-name))
        (cond ((= current best) (push obj out-ls))
              ((< current best) 
               (setf best current)
               (setf out-ls (list obj))))))))


(defgeneric objs-max-slotval (lst slot-name)
  (:documentation 
   "Given a list of CLOS objects and a slot name, return a list containing the object(s) with the highest value for that slot."))

(defmethod objs-max-slotval ((ls list) (slot-name symbol))
  (when ls
    (let ((best (slot-value (first ls) slot-name))
          (current nil)
          (out-ls (list (first ls))))
      (dolist (obj (rest ls) (nreverse out-ls))
        (setf current (slot-value obj slot-name))
        (cond ((= current best) (push obj out-ls))
              ((> current best) 
               (setf best current)
               (setf out-ls (list obj))))))))

(defgeneric objs-nearest-slotval (lst slot-name val)
  (:documentation 
   "Given a list of CLOS objects and a slot name, return a list containing the objects with the slot value closest to the supplied value."))


(defmethod objs-nearest-slotval ((lst list) (slot-name symbol) 
                                     (val number))
  (let ((best (abs (- val (slot-value (first lst) slot-name))))
        (current nil)
        (out-lst  (list (first lst))))
    (dolist (obj (rest lst) (nreverse out-lst))
      (setf current (abs (- val (slot-value obj slot-name))))
      (cond ((= current best) (push obj out-lst))
            ((< current best)
             (setf best current)
             (setf out-lst (list obj)))))))


;; similar functions for non-objects using a general accessor


(defun objs-min-val (list accessor)
  (let ((value nil)
        (matches nil))
          
    (dolist (y list)
      (let ((cur-val (funcall accessor y)))
        (if (or (null value) 
                (< cur-val value))
            (progn
              (setf value cur-val)
              (setf matches (list y)))
          (when (= cur-val value)
            (push y matches)))))
    matches))

(defun objs-max-val (list accessor)
  (let ((value nil)
        (matches nil))
          
    (dolist (y list)
      (let ((cur-val (funcall accessor y)))
        (if (or (null value) 
                (> cur-val value))
            (progn
              (setf value cur-val)
              (setf matches (list y)))
          (when (= cur-val value)
            (push y matches)))))
    matches))

;;; MKSTR      [Function]
;;; Date        : 97.07.02
;;; Description : From Graham's _On Lisp_, makes sure we have a string.

(defun mkstr (&rest args)
  "Return a concatenated string representation of the arguments"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


(defun string->name (s)
  (if (stringp s)
      (if (eq (char s 0) #\:)
          (intern (string-upcase (subseq s 1)) :keyword)
        (let ((n (ignore-errors (let ((*read-eval* nil)) (read-from-string s)))))
          (if (numberp n)
              n
            (intern (string-upcase s)))))
    s))

(defun string->number (s)
  (if (stringp s) 
      (let ((n (ignore-errors (read-from-string s))))
        (if (numberp n)
            n
          s))
    s))

(defun make-string-name (s)
  (when (symbolp s)
    (setf s (symbol-name s)))
  (if (stringp s)
      (string-downcase s)
    (string-downcase (princ-to-string s))))

(defgeneric random-item (seq)
  (:documentation "Returns a random item from a sequence using act-r-random."))

(defmethod random-item ((seq list))
  (nth (act-r-random (length seq)) seq))

#+:mcl
(defmethod random-item ((seq simple-vector))
  (svref seq (act-r-random (length seq))))

#-(or :mcl :clisp)
(defmethod random-item ((seq vector))
  (svref seq (act-r-random (length seq))))


(defmethod random-item ((seq sequence))
  (elt seq (act-r-random (length seq))))

(defmethod random-item ((seq null))
  (declare (ignorable seq))
  nil)

(defun sym->key (symbol)
  "Given a symbol, return the corresponding keyword."
  (read-from-string (mkstr ":" symbol)))


;;; FLATTEN      [Function]
;;; Description : From Graham's _On Lisp_, takes a nested list and turns it
;;;             : into a flat one.  "Fast" version.

(defun flatten (lis)
  "Takes a nested list and makes in into a single-level list"
  (declare (list lis))
  (labels ((rec (lis acc)
             (cond ((null lis) acc)
                   ((atom lis) (cons lis acc))
                   (t (rec (car lis) (rec (cdr lis) acc))))))
    (rec lis nil)))


#-(or :mcl :cmu :clozure-common-lisp) (defmacro dovector ((varsym vec &optional ret) &body body)
  (let ((idx (gensym)))
    `(let ((,varsym nil))
       (dotimes (,idx (length ,vec) ,ret)
         (setq ,varsym (aref ,vec ,idx))
         ,@body
         ))))


(defgeneric within (min max)
  (:documentation 
   "Returns a closure that will test whether the argument is betwen <min> and <max>, inclusive."))

(defmethod within ((min number) (max number))
  (lambda (val)
    (and (numberp val) (<= val max) (>= val min))))

;;(defmethod within ((min list) (max list))
;;  (within (check-fct min) (check-fct max)))

;;(defmethod within ((min number) (max list))
;;  (within min (check-fct max)))

;;(defmethod within ((min list) (max number))
;;  (within (check-fct min) max))


(defgeneric greater-than (criterion)
  (:documentation 
   "Returns a closure that will return whether or not the argument is greater than <criterion>."))

(defmethod greater-than ((criterion number))
  (lambda (val)
    (and (numberp val) (> val criterion))))

;;(defmethod greater-than ((criterion list))
;;  (greater-than (check-fct criterion)))



(defgeneric less-than (criterion)
  (:documentation 
   "Returns a closure that will return whether or not the argument is less than <criterion>."))

(defmethod less-than ((criterion number))
  (lambda (val)
    (and (numberp val) (< val criterion))))


;;(defmethod less-than ((criterion list))
;;  (less-than (check-fct criterion)))


(defun not-equal (x)
  (declare (inline not-equal))
  (lambda (val)
    (not (equal x val))))


#-(or :mcl :clozure-common-lisp)
(defun neq (x y)
  "The NOT of EQ."
  (declare (inline neq))
  (not (eq x y)))


(defun string-to-lines (s)
  (aif (position #\Newline s)
    (append (mklist (subseq s 0 it))
            (string-to-lines (subseq s (1+ it) (length s))))
    (list s)))


(defun find-float-time-limit (size &optional (digits 3))
  (let* ((radix (float-radix (coerce 1.0 size)))
         (precision (float-precision (coerce 1.0 size)))
         (significant (log (expt radix precision) 10)))
    
    (cons size (coerce (expt 10 (/ (floor (- significant digits) .1) 10)) size))))

(defvar *time-size-test-list* (mapcar 'find-float-time-limit
                                (list 'short-float 'single-float 'double-float 'long-float)))

;(defvar *time-size-current-limit* (seconds->ms (expt 2 (find-float-time-limit *read-default-float-format*))))
(defvar *time-size-current-type* *read-default-float-format*)                                    


(defun safe-seconds->ms (seconds &optional fname)
  (when (floatp seconds)
    (let* ((type (type-of seconds))
           (size (assoc type *time-size-test-list*)))
      (when (null size)
        (setf size (find-float-time-limit type))
        (push-last size *time-size-test-list*))
      
      (when (> (abs seconds) (cdr size))
        (let* ((larger (find-if (lambda (x) (> (cdr x) (cdr size))) *time-size-test-list*))
               (marker (case (car larger) (single-float #\f) (double-float #\d) (long-float #\L)))
               (options (list "specify the time in milliseconds if the command that was used has such an option"
                              "use a rational number instead of a float e.g. 1234567/1000 instead of 1234.567")))
          (when larger
            (push-last (format nil "change the default floating point format to a higher precision type like ~s and recompile the ACT-R sources" (car larger))
                       options))
          (when marker
            (push-last (format nil "directly specify the time as a higher precision floating point type e.g. 1234.567~c0" marker)
                       options))
          (one-time-model-warning :large-seconds "A time of ~f seconds was specified~@[ in a call to the ~a command~] which is beyond the limit to accurately represent time in milliseconds for the floating point type of that number (~s).~%To avoid potential problems you could do one of the following:~%~{ - ~a~%~}"
                                seconds fname (car size) options)))))
  (round (* seconds 1000)))
                            

(defun circular-references (dependencies)
  "Modify the dependencies lists passed and return t if there are any circularities"
  (dolist (x dependencies)
    (dolist (y (cdr x))
      (awhen (find y dependencies :test (lambda (i j) (if (listp (car j))
                                                          (find i (car j))
                                                        (eq i (car j)))))
             (setf (cdr x) (append (cdr x) (cdr it))))))
  
  (some (lambda (x) 
          (if (listp (car x))
              (some (lambda (z) (find z (cdr x))) (car x))
            (find (car x) (cdr x)))) dependencies))


(defun chunk-spec-variable-p (chunk-spec-slot-value &optional (char #\=))
  (when (symbolp chunk-spec-slot-value) 
    (let ((s (symbol-name chunk-spec-slot-value)))
      (and (> (length s) 1)
           (char-equal (char s 0) char)))))

(defun replace-variables (arg bindings)
  (cond ((and (consp arg) (eq (last arg) arg))  ;; detect that it's something like (a . b)
         (cons (replace-variables (car arg) bindings)
               (replace-variables (cdr arg) bindings)))
         
         ((listp arg) 
          (mapcar (lambda (x)
                    (replace-variables x bindings))
            arg))
        ((chunk-spec-variable-p arg) 
         (aif (assoc arg bindings)
              (cdr it)
              arg))
        (t arg)))


;;; convert a color to the string or use the default
;;; if not valid or given.

(defun color->name (color &optional (default "black"))
  (cond ((or (eq color t) (null color))
         default)
        ((symbolp color)
         (string-downcase (symbol-name color)))
        ((stringp color)
         color)
        (t default)))


;;; Functions that are callable as format directives


(defun cl-user::print-time-in-seconds (stream time colon-p atsign-p &optional (width 0) padchar commachar)
  (declare (ignore colon-p atsign-p padchar commachar))
  (multiple-value-bind (sec ms) (truncate (abs (round time)) 1000)
    (let* ((digits (format nil "~:[~;-~]~d.~3,'0d" (minusp time) sec ms))
           (pad (max 0 (- width (length digits)))))
      (format stream "~va~a" pad "" digits))))





(defun set-or-remove-hook-parameter (param current-items value)
  (if value
      (cond ((and (listp value)             ;; don't really need the remove test
                  (eq (car value) :remove)) ;; but just to be safe
             (if (member (second value) current-items)
                 (remove (second value) current-items)
               (progn
                 (print-warning 
                  "Removing item ~s from parameter ~s failed because it is not on the hook."
                  (second value)
                  param)
                 current-items)))
            ((member value current-items)
             (print-warning 
              "Setting parameter ~s failed because ~s already on the hook."
              param value)
             current-items)
            (t
             (push value current-items)))
    nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACL and CCL don't seem to print the text provided in an error
;;; call normally so create a special format function so that the
;;; created error strings can be more informative.

#-(or :allegro :ccl) (defun cl-user::print-error-message (stream error colon-p atsign-p &optional (width 0) padchar commachar)
                   (declare (ignore colon-p atsign-p width padchar commachar))
                   (format stream "~s" error))

#+(or :allegro :ccl) (defun cl-user::print-error-message (stream error colon-p atsign-p &optional (width 0) padchar commachar)
         (declare (ignore colon-p atsign-p width padchar commachar))
         (format stream "#<~a " (type-of error))
         (write error :stream stream :escape nil)
         (format stream  ">"))


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
