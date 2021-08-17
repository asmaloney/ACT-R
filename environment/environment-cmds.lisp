;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : environment-cmds.lisp
;;; Version     : 3.0
;;; 
;;; Description : No system dependent code.
;;;             : Defines stuff that is needed to handle "environment"
;;;             : things.
;;; Bugs        : 
;;; 
;;; Todo        : Clean up what goes here and what's passed literally
;;;             : from the Tcl side - it's an odd mix at this point
;;;             : and I should have some consistency.
;;; 
;;; ----- History -----
;;;
;;; 05/22/2002  Dan
;;;             : File creation
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;; 01/20/2003  Dan
;;;             : Added the show-module-state-chunk function and
;;;             : hacked buffer-contents so that the module-state
;;;             : chunks shown by the environment match internal
;;;             : module states even though the "real" chunks
;;;             : don't seem to be updated anymore. WHY and WHEN?!
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; ----------------------------------------------------------------------
;;; 2005.04.13  Dan [2.0]
;;;             : * Moved to ACT-R 6.
;;; 2007.08.03 Dan
;;;             : * Moved the *stepper-open* defvar here to avoid a warning
;;;             :   at compile time.
;;;             : * Deleted the commented out in-package calls.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.05.25 Dan
;;;             : * Modified reload-model and safe-load so that warnings aren't
;;;             :   double printed in most cases.
;;;             : * Added a finishing of the output streams to those because
;;;             :   it seems to lag sometimes in ACL...
;;;             : * The open button uses a function called smart-loader which
;;;             :   isn't defined anymore, so adding that.
;;; 2012.02.09 Dan
;;;             : * Explicitly close streams made with make-string-output-stream 
;;;             :   to be safe.
;;; 2012.12.18 Dan
;;;             : * Changed reload-model and safe-load so that they can signal
;;;             :   an error without opening the interactive debugger because
;;;             :   that's an issue in the environment particularly with the
;;;             :   standalones since the error would have to be cleared before
;;;             :   the environment notice can be displayed, but with CCL that's
;;;             :   very difficult becuase the background process doesn't have
;;;             :   access to the terminal...
;;; 2013.02.19 Dan
;;;             : * Haven't been able to reconstruct why the loaders "ignore"
;;;             :   unbound-variable errors, but since it can lead to very bad
;;;             :   situations for users I'm taking that out.  There are two
;;;             :   guesses so far as to where it comes from.  Christian thinks 
;;;             :   it may go all the way back to the old environment's split 
;;;             :   edit files because code may have been loaded out of order.
;;;             :   It might also have been a "fix" for a problem with the early 
;;;             :   OpenMCL versions of the standalone environment based on an 
;;;             :   error report I found in an email from 9/12/02:
;;;             :   
;;;             :   ... whenever I try to load a model (I get that far without 
;;;             :   problem), OpenMCL complains:
;;;             :   ? (start-environment)
;;;             :   ((#<TCP-STREAM (SOCKET/4) #x54A8926> #<PROCESS Environment-Connection(2) [Enabled] #x54A8DEE> ("127.0.0.1" . 2621)))
;;;             :   <try to load a model>
;;;             :   ? Error in update of Handler: HANDLER5
;;;             :   message: #<Anonymous Function #x54C3E9E>
;;;             :   
;;;             :   Error:Unbound variable: \?
;;;             :   
;;;             :   and then hangs.
;;; 2014.07.30 Dan
;;;             : * Added the all-dm-slot-lists and filter-dm-chunks functions
;;;             :   to use for the filter in the declarative viewer since 
;;;             :   chunk-type won't work now.
;;; 2015.05.18 Dan
;;;             : * Filter-dm-chunks now verifies that all the items are valid
;;;             :   slot names first because there are potential issues across
;;;             :   reset and loading where a slot name from a previous run
;;;             :   doesn't exist anymore.
;;; 2017.08.25 Dan
;;;             : * Remove the functions that aren't needed as I go.
;;; 2017.09.01 Dan
;;;             : * Updated the declarative cmds: dm-slot-filters and filter-
;;;             :   dm-chunks.
;;;             : * To get around an issue in how the cmds come over from
;;;             :   Tcl with respect to the encoding of lists just make it
;;;             :   work 'right' here when necessary using convert-tcl-list-to-list.
;;; 2017.09.05 Dan
;;;             : * Added dm-whynot-text to capture the whynot-dm output.
;;;             : * Added production-details for capturing spp+pp output.
;;; 2017.09.06 Dan
;;;             : * Added the sorted-module-names, printed-parameter-details,
;;;             :   and modules-parameters commands.
;;; 2017.09.07 Dan
;;;             : * Fixed printed-parameter-details so that it decodes the
;;;             :   parameter from the string.
;;;             : * Added modules-with-parameters for the parameters dialog.
;;; 2018.06.12  Dan
;;;             : * Changed some external doc strings for consistency.
;;; 2019.07.30  Dan
;;;             : * Adjusted filter-dm-chunks so that it doesn't assume a tcl
;;;             :   encoded list of slots.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;; This is a hack to get around an issue in the encoding
;; of things on the Tcl side when generating the commands.
;; The right fix would be to have it better parse things 
;; and create a list in JSON instead of sending the string
;; directly, but that's more work than just dealing with the
;; occasional issues in values sent.

(defun convert-tcl-list-to-list (string)
  (handler-case (read-from-string (format nil "(~a)" string))
    (error () nil)))


(defun dm-slot-filters ()
  (cons '_none_ (all-dm-chunk-types (get-module declarative))))

(add-act-r-command "dm-slot-filters" 'dm-slot-filters "Returns a list of the possible filters (lists of slot names) for chunks in DM for the Environment to use. No params.")

(defun filter-dm-chunks (slot-list)
  (let ((dm (get-module declarative)))
    (if (and (stringp slot-list) (or (string-equal slot-list "_none_") (string-equal slot-list "null")))
        (setf slot-list nil)
      (setf slot-list (if (stringp slot-list) (convert-tcl-list-to-list slot-list) (decode-string-names slot-list))))
    (when (and dm (every 'valid-slot-name slot-list))
      (let ((mask (reduce 'logior slot-list :key 'slot-name->mask)))
        (mapcan (lambda (x) 
                  (if (slots-vector-match-signature (car x) mask)
                      (copy-list (cdr x))
                    nil))
          (bt:with-lock-held ((dm-chunk-lock dm))(dm-chunks dm)))))))

(add-act-r-command "filter-dm-chunks" 'filter-dm-chunks "Returns a list of chunks in DM which match the specified slot list filter. Params: list-of-slots.")

(defun dm-chunk-details (name)
  (let ((cname (string->name name)))
    (if (chunk-p-fct cname)
        (with-parameters (:ans nil)
          (format nil "~a~%~a" (capture-command-output (sdp-fct (list cname)))
            (printed-chunk cname)))
      "")))

(add-act-r-command "dm-chunk-details" 'dm-chunk-details "Returns the text of the declarative parameters and printed representaion of a chunk for the Environment. Params: chunk-name.")


(defun dm-whynot-text (name)
  (let ((cname (string->name name)))
    (if (chunk-p-fct cname)
        (capture-command-output (whynot-dm-fct (list cname)))
      "")))

(add-act-r-command "dm-whynot-text" 'dm-whynot-text "Returns the text of the whynot-dm information for a chunk for use in the Environment. Params: chunk-name.")

(defun production-details (name)
  (let ((pname (string->name name)))
    (if (find pname (all-productions))
        (format nil "~a~%~a" (capture-command-output (spp-fct (list pname)))
          (printed-production-text pname))
      "")))

(add-act-r-command "production-details" 'production-details "Returns the text of the procedural parameters and printed representaion of a production for the Environment. Params: production-name.")

(defun whynot-text (name)
  (let ((pname (string->name name)))
    (if (find pname (all-productions))
        (format nil "Time: ~/print-time-in-seconds/~%~a" (mp-time-ms)
          (capture-command-output (whynot-fct (list pname))))
      "")))

(add-act-r-command "whynot-text" 'whynot-text "Returns the text of the whynot information for a production for use in the Environment. Params: production-name.")


(defun sorted-module-names ()
  (sort (all-module-names) #'string< :key 'symbol-name))

(add-act-r-command "sorted-module-names" 'sorted-module-names "Returns a list of the names of all current modules sorted alphabetically. No params.")

(defun modules-with-parameters ()
  (remove-if-not 'modules-parameters (sorted-module-names)))

(add-act-r-command "modules-with-parameters" 'modules-with-parameters "Returns a list of the names of all current modules which provided parameters sorted alphabetically. No params.")

(defun printed-parameter-details (param)
  (capture-command-output (sgp-fct (list (string->name param)))))

(add-act-r-command "printed-parameter-details" 'printed-parameter-details "Returns a string of the parameter output for the given parameter's value and details. Params: parameter-name.")

(defun modules-parameters (module-name)
  (let (params
        (name (string->name module-name)))
    (bt:with-lock-held (*parameters-table-lock*)
      (maphash (lambda (key value) 
                 (when (eq (act-r-parameter-owner value) name)
                   (push key params)))
               *act-r-parameters-table*))
    (sort params 'string< :key 'symbol-name)))

(add-act-r-command "modules-parameters" 'modules-parameters "Returns a list of the names of all the parameters for the named module sorted alphabetically. Params: module-name.")

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
