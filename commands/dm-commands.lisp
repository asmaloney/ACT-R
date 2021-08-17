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
;;; Filename    : dm-commands.lisp
;;; Version     : 2.0
;;; 
;;; Description : ACT-R commands relavent to Declarative Memory
;;; 
;;; Bugs        : 
;;;
;;; To do       : [x] Fix issues with merged-copies not showing up as being in DM.
;;; 
;;; ----- History -----
;;;
;;; 2005.01.11 Dan
;;;             : Created this file to hold stuff that was in declarative-
;;;             : memory in core-modules.
;;; 2005.01.17 Dan
;;;             : * Updated dm and sdm to use pprint-chunks-fct instead of
;;;             :   the now non-existent pprint-chunk-fct.
;;;             : * Fixed dm and sdm so that they don't print anything in the
;;;             :   case that there are no chunks to print instead of all 
;;;             :   existing chunks.
;;; 2005.01.18 Dan
;;;             : * Added more of the ACT-R 5 commands.
;;; 2005.01.21 Dan
;;;             : * Changed references to dm-chunks to reflect the change to
;;;             :   the new representation as a hash table by chunk-type.
;;; 2005.01.26 Dan
;;;             : * Added sdp functionality.
;;;             : * Removed calls with "old" names and moved them to
;;;             :   backward.lips in support.
;;; 2005.01.31 Dan
;;;             : * Fixed dm so that it only prints chunks from DM even when
;;;             :   names are provided.
;;; 2005.05.06 Dan
;;;             : * Fixed a bug in add-sji-fct where it called chunk-p instead
;;;             :   of chunk-p-fct.
;;; 2005.06.10 Dan
;;;             : * Changed chunk-parameters so that the references returned
;;;             :   is sensitive to the current setting of :ol i.e. it hides
;;;             :   the reference list when :ol is t and truncates it to the
;;;             :   appropriate size when :ol is a number.
;;; 2005.06.15 Dan
;;;             : * Updated the dm command so that chunks which have merged
;;;             :   into dm still print out if asked for, but it doesn't
;;;             :   show all merged names if called with no parameters.
;;; 2005.07.06 Dan
;;;             : * Changed sdm so that the isa isn't required.
;;; 2005.07.29 Dan
;;;             : * Fixed sdp so that it only works for chunks that are actually
;;;             :   in DM.
;;; 2005.08.01 Dan
;;;             : * Fixed an issue with BLC in the main module file that
;;;             :   reflects on the usage of sdp here.  Basically, BLC is added
;;;             :   to all base-level calculations, but no values are stored
;;;             :   in the chunk-base-level other than user settings.
;;; 2005.08.03 Dan
;;;             : * Added the print-dm-finsts command.
;;; 2005.09.15 Dan
;;;             : * Cleaned up sdm so that it checks the chunk-type and prints
;;;             :   a reasonable warning when an invalid chunk-type is given.
;;; 2006.06.20 Dan
;;;             : * Fixed a bug in set-base-level that lead to an error if 
;;;             :   :ol was nil and a creation time wasn't provided for a chunk.
;;; 2006.07.10 Dan
;;;             : * Changed call to true-chunk-name to true-chunk-name-fct since
;;;             :   that's being moved to a macro/-fct for user's access.
;;; 2006.11.29 Dan
;;;             : * Removed use of dm-pm and replaced it with dm-mp because the
;;;             :   :pm parameter is being depricated and :mp is both the flag
;;;             :   and value now like :bll and :mas.
;;;             : * Changed a warning in sdm to be a little clearer.
;;; 2006.11.30 Dan
;;;             : * Fixed add-sji-fct so that it removes an older setting
;;;             :   when adding a new one.
;;;             : * Changed clear-dm so that it returns t if the chunks were
;;;             :   cleared and nil if not.
;;;             : * Fixed clear-dm so that it also clears the merge table.
;;; 2006.12.01 Dan
;;;             : * Renamed the internal function set-base-level to set-bl
;;;             :   to avoid any issues with the user function set-base-levels.
;;;             : * Updates to sdp to better handle base-level reporting.
;;; 2006.12.05 Dan
;;;             : * Modified sdp to use the chunk-last-base-level parameter
;;;             :   setting instead of calling for a recomputation of the
;;;             :   base-level.
;;;             : * Modified the base-level setting code (set-base-levels and 
;;;             :   set-all-base-levels) so that when :ol is a number the
;;;             :   appropriate number of references are created.
;;;             : * Fixed set-base-levels and set-all-base-levels so that they
;;;             :   now return the base-level activations.
;;; 2006.12.06 Dan
;;;             : * Changed set-all-base-levels to return t/nil because if a
;;;             :   creation time isn't specified there's no particular base-level 
;;;             :   that's meaningful to return - they could all be different.
;;;             : * Added tests in set-base-levels-fct and set-all-base-levels
;;;             :   to make sure that the level and creation-time are numbers.
;;;             : * Turn off the activation trace when updating activations in sdp.
;;; 2006.12.07 Dan
;;;             : * Added the new sdp parameters :reference-list, :reference-count,
;;;             :   :retrieval-activation and :retrieval-time.  The first two of
;;;             :   those effectively make :references unnecessary and it will
;;;             :   no longer be shown (and shouldn't be used now either).
;;; 2006.12.08 Dan
;;;             : * Cleaned up the return values of sjis and similarities in sdp
;;;             :   so "default" values are returned in addition to user settings.
;;; 2007.04.13 Dan
;;;             : * Fixed a bug with sdm-fct which left the return value as nil
;;;             :   if there wasn't an isa even if matching chunks were found.
;;; 2007.04.19 Dan
;;;             : * Renamed the :retrieval-activation and :retrieval-time parameters
;;;             :   to :last-... because :retrieval-activation is also a general
;;;             :   parameter so that avoids confusion there and adding the "last"
;;;             :   on the front might make their interpretation more obvious.
;;;             : * Also moved them to the end of the sdp output for the chunks.
;;;             : * Fixed sdp so that it doesn't print invalid parameters as if
;;;             :   they existed when requested (even though there was a warning
;;;             :   the output made it look like it was still ok).
;;; 2007.06.18 Dan
;;;             : * Added the reset-declarative-finsts command so people can
;;;             :   clear the finsts from code if needed.
;;; 2008.09.13 Dan
;;;             : * Updated all the commands so that they will take the merged
;;;             :   names and treat them "right" (the to do above).
;;; 2010.06.15 Dan
;;;             : * Modified sdp so that it only recomputes the activation when
;;;             :   the :activation or :base-level parameter is requested.
;;; 2011.04.25 Dan
;;;             : * Converting over to storing ms internally for times.
;;; 2011.06.22 Dan
;;;             : * Added the print-chunk-activation-trace and print-activation-trace
;;;             :   commands for displaying the information saved when the :sact
;;;             :   parameter is set.
;;; 2011.09.12 Dan
;;;             : * Added a declare to ingore data in print-chunk-activation-details
;;;             :   to avoid a warning, but should go back and figure out why it's
;;;             :   not used and remove it if there's no reason for it.
;;; 2012.02.03 Dan
;;;             : * Updated print-dm-finsts so that it prints and returns the time  
;;;             :   in seconds.
;;;             : * Updated print-dm-finsts so that it removes the old finsts 
;;;             :   before printing them.
;;; 2012.02.08 Dan
;;;             : * Changed sdp so that it does not recompute the activation for
;;;             :   a chunk if it is at the same time as a retrieval request.
;;;             :   Previously it would, but doing that makes it difficult to
;;;             :   preserve the trace and parameter information for something
;;;             :   like the retrieval history tool since turning that on would
;;;             :   then change how the model ran if it was using a fixed seed.
;;; 2012.02.10 Dan
;;;             : * Added a check to print-chunk-activation-trace-fct so that 
;;;             :   it handles things correctly when :esc is nil.
;;; 2012.02.20 Dan
;;;             : * Changed the activation trace printing commands to use the
;;;             :   command stream instead of the model stream.
;;;             : * Print-chunk-activation-trace now returns multiple values
;;;             :   which represent the total, base-level, spreading, similarity,
;;;             :   and noise components of the activation calculation.
;;;             : * Added the saved-activation-history command which returns the
;;;             :   list of times and chunks for which there is a history 
;;;             :   available.
;;; 2013.04.17 Dan
;;;             : * Added the whynot-dm command that can be used to output info
;;;             :   about how a chunk (or chunks) compared to the last retrieval
;;;             :   request which occurred.
;;; 2013.07.15 Dan
;;;             : * Sort the whynot-dm return value based on the retrieval-activation
;;;             :   of the chunks.
;;;             : * Added the simulate-retrieval-request command which takes a chunk-spec
;;;             :   description and then prints a stripped down activation trace of
;;;             :   what would happen if that request were made at this time (with
;;;             :   no side effects) and returns the list of matching chunks sorted
;;;             :   by activation.
;;; 2013.07.16 Dan
;;;             : * Realized that I forgot the -fct on the simulate-retrieval-request
;;;             :   function.
;;; 2013.07.18 Dan
;;;             : * and then I failed to update the return-from calls within the
;;;             :   simulate-retrieval-request-fct function until now.
;;; 2014.03.10 Dan [2.0]
;;;             : * Updating to work with the chunks that don't have a chunk-type
;;;             :   and the corresponding changes to DM.
;;; 2014.04.01 Dan
;;;             : * Used slots-vector-match-signature instead of doing the bit
;;;             :   tests directly.
;;; 2014.08.29 Dan
;;;             : * Fixed a typo in the output of whynot-dm.
;;; 2015.06.04 Dan
;;;             : * Use safe-seconds->ms for creation time and references.
;;; 2015.06.08 Dan
;;;             : * Saved trace now holds milliseconds for -ct and -refs so print
;;;             :   appropriately.
;;;             : * The history table is also indexed by ms now so give an
;;;             :   optional parameter to allow asking for the saved traces in
;;;             :   milliseconds instead of seconds.
;;;             : * Print-dm-finsts displays the time accurately, but still
;;;             :   returns a float of seconds.
;;; 2015.06.09 Dan
;;;             : * Sdp now prints times accurately, but still returns seconds.
;;;             : * Whynot-dm updated since last-request-time is now in ms.
;;; 2015.07.23 Dan
;;;             : * Added the simulate-retrieval-request-plus-seed-fct function
;;;             :   which does the same thing as simulate-retrieval-request 
;;;             :   except that it has a second return value which is the seed
;;;             :   value after the retrieval process completed so that one can
;;;             :   use that to implement an extra retrieval buffer which updates
;;;             :   the random state appropriately.
;;; 2015.07.24 Dan
;;;             : * Fixed a bug with simulate-retrieval-request-plus-seed-fct since
;;;             :   it was trying to return-from simulate-retrieval-request-fct.
;;; 2016.01.14 Dan
;;;             : * Fixed a bug with print-dm-finsts because it was trying to
;;;             :   convert the conses of the finst list to seconds instead of
;;;             :   the time in the cdr of the conses.
;;; 2016.09.01 Dan
;;;             : * Print-activation-trace didn't actually print the name of the
;;;             :   chunk when the retrieval-set-hook returned a result (the main
;;;             :   activation trace had the same bug).
;;; 2017.02.23 Dan
;;;             : * Added the remote whynot-dm command.
;;;             : * Added remote print-dm-finsts and changed it to return a list
;;;             :   of lists instead of conses.
;;; 2017.08.07 Dan
;;;             : * Updated the calls to compute-sji and chunks-similarity to 
;;;             :   pass in the params for thread safety.
;;; 2017.08.08 Dan
;;;             : * Update calls to the activation related function from the
;;;             :   declarative module code to pass in parameters.
;;;             : * Finished making everything thread safe.
;;; 2017.10.06 Dan
;;;             : * Fixed a bug with whynot-dm-internal not returning the right
;;;             :   value, and bugs in the calls to invalid in simulate-retrieval-
;;;             :   request-plus-seed-fct.
;;; 2017.10.20 Dan
;;;             : * Added remote versions of dm and sdm.
;;; 2017.11.14 Dan
;;;             : * Added an external sdp.
;;;             : * Changed the default for time-in-ms for the activation history
;;;             :   related commands to be t.
;;;             : * Added external versions of saved-activation-history, print-chunk-
;;;             :   activation-trace, and print-activation-trace.
;;; 2018.03.09 Dan
;;;             : * Sdm needs to use decode-string-names not string->name.
;;; 2018.04.05 Dan
;;;             : * Splitting sdm so that only the remote version converts the
;;;             :   strings to names to keep things Lisp side more consistent.
;;; 2018.04.18 Dan
;;;             : * Sjis need to be stored and returned as 2 element lists 
;;;             :   instead of conses to avoid issues with JSON.
;;;             : * Same with similarities.
;;;             : * Added remote sji/add-sji and similarity/set-similarities commands.
;;; 2018.04.20 Dan
;;;             : * Added remote versions of get-base-level, set-base-levels,
;;;             :   set-base-levels-fct, set-all-base-levels.
;;; 2018.04.25 Dan
;;;             : * Added remote clear-dm, reset-declarative-finsts, and -fct and 
;;;             :   -plus-seed-fct versions of simulate-retrieval-request.
;;;             : * Added the retrieval-times processor for the retrieval-history
;;;             :   stream to return just the times and chunk names (similar to
;;;             :   the saved-activation-history command).
;;; 2018.04.27 Dan
;;;             : * Moved the retrieval-times processor since the history isn't
;;;             :   yet defined when this is loaded.
;;; 2018.06.11 Dan
;;;             : * Making sure that the string to name conversion is done 
;;;             :   appropriately and only for external calls, and also redoing
;;;             :   the doc strings for external commands to be consistent with
;;;             :   the manual's indication of syntax.
;;; 2018.06.13 Dan
;;;             : * Fixed a couple typos from the previous update.
;;; 2018.12.17 Dan
;;;             : * The remote whynot-dm wasn't using the external version.
;;; 2019.04.04 Dan
;;;             : * Cache the result of chunk-spec-slots in the retrieval
;;;             :   request since that's costly don't want to do it twice.
;;; 2019.08.15 Dan
;;;             : * Fixed a bug with whynot-dm that happens when :esc is nil.
;;; 2019.09.10 Dan
;;;             : * To avoid some warnings with the single-threaded build, made
;;;             :   some simplifications to set-similarities-fct and set-bl.
;;; 2019.12.02 Dan
;;;             : * Clear-dm should set the in-dm parameter to nil so that the
;;;             :   chunks can be added back if needed.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The user functions from previous versions


(defmacro add-sji (&rest settings)
  "Macro to add an sji value between chunks"
  `(add-sji-fct ',settings))

(defun add-sji-fct (settings)
  "Function to add an sji value between chunks"
  (let ((returns nil))
    (dolist (x settings (reverse returns))
      (if (and (listp x)
               (= (length x) 3)
               (chunk-p-fct (first x))
               (chunk-p-fct (second x))
               (numberp (third x)))
          (let ((sji (third x))
                (chunk-i (second x))
                (chunk-j (first x)))
            (push sji returns)
            (setf (chunk-sjis chunk-i) (remove chunk-j (chunk-sjis chunk-i) :key 'car))
            (push (list chunk-j sji) (chunk-sjis chunk-i)))
        (progn
          (print-warning "Bad Sji setting in ~S" x)
          (push :error returns))))))

(defmacro sji (chunkj chunki)
  "Macro to return the current Sji value between chunkj and chunki"
  `(sji-fct ',chunkj ',chunki))

(defun sji-fct (chunkj chunki)
  "Function to return current Sji value between chunkj and chunki"
  (let ((dm (get-module declarative)))
    (if dm
        (let (sji-hook mas nsji)
          (bt:with-lock-held ((dm-param-lock dm))
            (setf sji-hook (dm-sji-hook dm)
              mas (dm-mas dm)
              nsji (dm-nsji dm)))
          (compute-sji chunkj chunki sji-hook mas nsji))
      (print-warning "No declarative memory module found"))))

(defun external-sji (chunkj chunki)
  (sji-fct (string->name chunkj) (string->name chunki)))

(defun external-add-sji (&rest settings)
  (add-sji-fct (string->name-recursive settings)))

(defun external-add-sji-fct (settings)
  (add-sji-fct (string->name-recursive settings)))

(add-act-r-command "sji" 'external-sji "Get the Sji value between chunks j and i. Params: chunkj chunki.")
(add-act-r-command "add-sji" 'external-add-sji "Set the Sji value between chunks. Params: (chunkj chunki Sji)*.")
(add-act-r-command "add-sji-fct" 'external-add-sji-fct "Set the Sji value between chunks. Params: ((chunkj chunki Sji)*).")

(defmacro similarity (chunkj chunki)
  "Macro to return the current similarity value between chunkj and chunki"
  `(similarity-fct ',chunkj ',chunki))

(defun similarity-fct (chunkj chunki)
  "Function to return current similarity value between chunkj and chunki"
  (let ((dm (get-module declarative)))
    (if dm
        (let (sim-hook cache-sim-hook act ms md)
          (bt:with-lock-held ((dm-param-lock dm))
            (setf sim-hook (dm-sim-hook dm)
              cache-sim-hook (dm-cache-sim-hook-results dm)
              act (dm-act dm)
              ms (dm-ms dm)
              md (dm-md dm)))
          (chunks-similarity dm chunkj chunki sim-hook cache-sim-hook act ms md))
      (print-warning "No declarative memory module found"))))

(defun external-similarity (chunkj chunki)
  (similarity-fct (decode-string chunkj) (decode-string chunki)))

(add-act-r-command "similarity" 'external-similarity "Get the similarity value between items j and i. Params: 'itemj itemi'.")

(defun clear-dm ()
  "User function to clear declarative memory - not recommended"
  (let ((dm (get-module declarative)))
    (if dm
        (bt:with-lock-held ((dm-chunk-lock dm))
          (mapcar (lambda (x) (setf (chunk-in-dm x) nil))
            (mapcan (lambda (x) 
                      (copy-list (cdr x))) (dm-chunks dm)))
          (setf (dm-chunks dm) nil)
          (clrhash (dm-chunk-hash-table dm))
          (print-warning "All the chunks cleared from DM.")
          t)
      (print-warning "No declarative memory module found"))))

(add-act-r-command "clear-dm" 'clear-dm "Remove all chunks from declarative memory.  No params.")

(defmacro dm (&rest chunks)
  "Macro to print and return chunks in declarative memory"
  `(dm-fct ',chunks))

(defun dm-internal (&rest chunk-list)
  "Function to print and return chunks in declarative memory"
  (let ((dm (get-module declarative)))
    (setf chunk-list (string->name-recursive chunk-list))
    (if dm
        (if chunk-list
            (let* ((all (all-dm-chunks dm))
                   (c-l (remove-if-not 
                         (lambda (x)
                           (member (true-chunk-name-fct x) all))
                         chunk-list)))
              (when c-l
                (pprint-chunks-fct c-l)))
          (when (dm-chunks dm)
            (pprint-chunks-fct (all-dm-chunks dm))))
      (print-warning "No declarative memory module found"))))

(add-act-r-command "dm" 'dm-internal "Print the representation of the chunks from declarative memory with the given names.  Params: chunk-name*")

(defun dm-fct (chunk-list)
  (apply 'dm-internal chunk-list))

(defmacro get-base-level (&rest chunks)
  "Macro to return the base-level activation of a chunk in dm"
  `(get-base-level-fct ',chunks))

(defun get-base-level-fct (chunks)
  "Function to return the base-level activation of a chunk in dm"
  (let ((dm (get-module declarative))
        (returns nil))
    (if dm
        (let ((dm-chunks (all-dm-chunks dm))
              bl-hook blc bll ol act-scale)
          (bt:with-lock-held ((dm-param-lock dm))
            (setf bl-hook (dm-bl-hook dm)
              blc (dm-blc dm)
              bll (dm-bll dm)
              ol (dm-ol dm)
              act-scale (dm-act-scale dm)))
              
          (dolist (chunk chunks returns)
            (let ((c (true-chunk-name-fct chunk)))
              (if (member c dm-chunks)
                  (push-last (base-level-activation dm c nil bl-hook nil blc bll ol act-scale) returns)
                (push-last :error returns)))))
      (print-warning "No declarative memory module found"))))
      
(defun get-base-level-external (&rest chunks)
  (get-base-level-fct (string->name-recursive chunks)))

(defun get-base-level-fct-external (chunks)
  (get-base-level-fct (string->name-recursive chunks)))


(add-act-r-command "get-base-level" 'get-base-level-external "Returns the base-level activation of chunks in DM. Params: chunk*")
(add-act-r-command "get-base-level-fct" 'get-base-level-external-fct "Returns the base-level activation of chunks in DM. Params: (chunk*)")

(defmacro sdm (&rest spec)
  "Macro to search for and print chunks in declarative memory"
  `(sdm-fct ',spec))

(defun sdm-external (&rest spec)
  (sdm-fct (decode-string-names spec)))
  
(defun sdm-fct (spec)  
  "Function to search for and print chunks in declarative memory"
  (let ((dm (get-module declarative)))
    (if dm
        (if (null spec) 
            (dm)
          (let ((chunk-spec (define-chunk-spec-fct spec nil)))
            (if chunk-spec
                (let* ((filled (chunk-spec-filled-slots chunk-spec))
                       (empty (chunk-spec-empty-slots chunk-spec))
                       (chunk-sets (mapcar 'cdr (remove-if-not (lambda (x) 
                                                                 (slots-vector-match-signature (car x) filled empty))
                                                               (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunks dm)))))
                       (chunks (mapcan (lambda (x)
                                         (find-matching-chunks chunk-spec :chunks x))
                                 chunk-sets)))
                  (when chunks (pprint-chunks-fct chunks)))
              (print-warning "Invalid chunk specification ~s passed to sdm" spec))))
      (print-warning "No declarative memory module found"))))

(add-act-r-command "sdm" 'sdm-external "Print the chunks from declarative memory which match with the given specification.  Params: 'spec*' where spec ::= {modifier} slot-name slot-value")

(defmacro set-similarities (&rest settings)
  "Macro to set the similarity value between chunks"
  `(set-similarities-fct ',settings))

(defun set-similarities-fct (settings)
  "Function to set the similarity value between chunks"
  (let ((returns nil))
    (dolist (x settings returns)
      (if (and (listp x)
               (= (length x) 3)
               (chunk-p-fct (first x))
               (chunk-p-fct (second x))
               (numberp (third x)))
          (let ((similarity (third x))
                (chunk-i (second x))
                (chunk-j (first x)))
            (push-last similarity returns)
            
            (bt:with-recursive-lock-held ((act-r-chunk-lock (get-chunk chunk-i)))
              (bt:with-recursive-lock-held ((act-r-chunk-lock (get-chunk chunk-j)))
                
                (awhen (assoc chunk-j (chunk-similarities chunk-i))
                       (setf (chunk-similarities chunk-i) (remove it (chunk-similarities chunk-i))))
                
                (awhen (assoc chunk-i (chunk-similarities chunk-j))
                       (setf (chunk-similarities chunk-j) (remove it (chunk-similarities chunk-j))))
                
                (push (list chunk-j similarity) (chunk-similarities chunk-i))
                (push (list chunk-i similarity) (chunk-similarities chunk-j)))))
        (progn
          (print-warning "Bad similarity setting in ~S" x)
          (push-last :error returns))))))


(defun external-set-similarity (&rest settings)
  (set-similarities-fct (decode-string-names settings)))

(defun external-set-similarities-fct (settings)
  (set-similarities-fct (decode-string-names settings)))

(add-act-r-command "set-similarities" 'external-set-similarity "Set the Sji value between chunks. Params: '(chunkj chunki similarity)*'.")
(add-act-r-command "set-similarities-fct" 'external-set-similarities-fct "Set the Sji value between chunks. Params: '((chunkj chunki similarity)*)'.")


(defmacro set-base-levels (&rest settings)
  "Macro to set the base-level activation of chunks"
  `(set-base-levels-fct ',settings))

(defun set-base-levels-fct (settings)
  "Function to set the base-level activation of chunks"
  (let ((base-levels nil)
        (dm (get-module declarative)))
    (if dm
        (let ((all (all-dm-chunks dm))
              bl-hook blc bll ol act-scale)
          (bt:with-lock-held ((dm-param-lock dm))
            (setf bl-hook (dm-bl-hook dm)
              blc (dm-blc dm)
              bll (dm-bll dm)
              ol (dm-ol dm)
              act-scale (dm-act-scale dm)))
          (dolist (setting settings base-levels)
            (if (and (chunk-p-fct (car setting)) (member (true-chunk-name-fct (car setting)) all)
                     (numberp (second setting)) (or (null (third setting)) (numberp (third setting))))
                (push-last (set-bl dm (true-chunk-name-fct (car setting)) (second setting) (third setting) bl-hook blc bll ol act-scale)
                           base-levels)
              (progn 
                (cond ((not (and (chunk-p-fct (car setting)) (member (car setting) all)))
                       (print-warning "~S does not name a chunk in DM." (car setting)))
                      ((not (numberp (second setting)))
                       (print-warning "Invalid level in setting ~S" setting))
                      (t    
                       (print-warning "Invalid creation-time in setting ~S" setting)))
                (push-last :error base-levels)))))
      (print-warning "No declarative memory module found"))))


(defun set-base-levels-external (&rest settings)
  (get-base-level-fct (string->name-recursive settings)))

(defun set-base-levels-fct-external (settings)
  (get-base-level-fct (string->name-recursive settings)))


(add-act-r-command "set-base-levels" 'set-base-levels-external "Set the base-level activation of chunks in DM. Params: (chunk level {creation-time})*")
(add-act-r-command "set-base-levels-fct" 'set-base-levels-external-fct "Set the base-level activation of chunks in DM. Params: ((chunk level {creation-time})*)")

(defun set-all-base-levels (base-level &optional creation-time)
  "Function to set the base-level activation of all dm chunks"
  (let ((dm (get-module declarative)))
    (if dm
        (if (and (numberp base-level) (or (null creation-time) (numberp creation-time)))
            (let (bl-hook blc bll ol act-scale)
              (bt:with-lock-held ((dm-param-lock dm))
                (setf bl-hook (dm-bl-hook dm)
                  blc (dm-blc dm)
                  bll (dm-bll dm)
                  ol (dm-ol dm)
                  act-scale (dm-act-scale dm)))
              
              (dolist (chunk (all-dm-chunks dm) t)
                (set-bl dm chunk base-level creation-time bl-hook blc bll ol act-scale)))
          (if (numberp base-level)
              (print-warning "Invalid creation-time ~S" creation-time)
            (print-warning "Invalid level ~S" base-level)))
      (print-warning "No declarative memory module found"))))


(add-act-r-command "set-all-base-levels" 'set-all-base-levels "Set the base-level activation of all chunks in DM. Params: base-level {creation-time}")

(defun set-bl (dm chunk base-level creation-time bl-hook blc bll ol act-scale)
  "Internal function to set the base level of a chunk"
  (bt:with-recursive-lock-held ((act-r-chunk-lock (get-chunk chunk)))  
    (cond (bll
           (when (numberp creation-time)
             (setf (chunk-creation-time chunk) (safe-seconds->ms creation-time 'set-base-levels)))
           (setf (chunk-reference-count chunk) base-level)
           (setf (chunk-reference-list chunk) (adapt-references dm base-level (chunk-creation-time chunk)))
           (base-level-activation dm chunk nil bl-hook nil blc bll ol act-scale))
          (t
           (when (numberp creation-time)
             (setf (chunk-creation-time chunk) (safe-seconds->ms creation-time 'set-base-levels)))
           (setf (chunk-base-level chunk) base-level)))))



(defun even-references (start end n &optional (m n))
  "Distributes m references evenly along n intervals between start and end."
  (when (plusp n)
    (let ((decrement (/ (- end start) n))
          (time end)
          (times nil))
      (dotimes (i (round m) times)
        (decf time decrement)
        (push-last time times)))))

(defun adapt-references (dm references creation-time)
  (let ((ol (bt:with-lock-held ((dm-param-lock dm)) (dm-ol dm))))
    (cond ((eq ol t) nil)
          ((null ol)
           (even-references creation-time (mp-time-ms) references))
          (t
           (even-references creation-time (mp-time-ms) references (min ol references))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDP functionality based on ACT-R 4/5 code.
;;; May need some adjustment at some point...

(defmacro sdp (&rest settings)
  "Macro to inspect and set declarative memory's chunks' parameters."
  `(sdp-fct ',settings))


(defun sdp-fct (parameters)
  "Function to inspect and set declarative memory's chunks' parameters."
  (let ((results nil)
        (dm (get-module declarative)))
    (if dm
        (if (null parameters) ; print all parameters for all chunks
            (dolist (chunk (all-dm-chunks dm) results)
              (push-last (dm-chunk-parameters-fct dm chunk) results))
          (dolist (description (if (or (keywordp (first parameters))
                                       (keywordp (second parameters))
                                       (and (listp (first parameters))
                                            (null (second parameters))
                                            (not (keywordp (second (first parameters))))))
                                   (list parameters)
                                 parameters)
                               results)
            (when (atom description) (setf description (list description)))
            (if (keywordp (first description))
                (dolist (chunk (all-dm-chunks dm) results)
                  (push-last
                   (if (and (cdr description)
                            (not (keywordp (second description))))
                       (set-dm-chunk-parameters-fct dm chunk description)
                     (dm-chunk-parameters-fct dm chunk description))
                   results))
              (dolist (chunk (if (atom (first description))
                                 (list (first description))
                               (first description)))
                (let ((c (true-chunk-name-fct chunk)))
                  (if (find c (all-dm-chunks dm))
                    (push-last
                     (if (and (cddr description)
                              (not (keywordp (third description))))
                         (set-dm-chunk-parameters-fct dm c (rest description))
                       (dm-chunk-parameters-fct dm c (rest description)))
                     results)
                  (progn
                    ; Should it print a warning?
                    (print-warning "~S does not name a chunk in DM." chunk)
                    (push-last :error results))))))))
      (print-warning "No declarative memory module found"))))


(defun external-sdp (&rest params)
  (sdp-fct (decode-string-names params)))

(add-act-r-command "sdp" 'external-sdp "Set or get declarative parameters. Params: see ACT-R manual." t)


(defun dm-chunk-parameters-fct (dm chunk &optional parameters)
  "Internal function to return the value of declarative chunk parameter(s), or  
   print them all if none specified."
  (let ((values nil)
        esc ol bll sa mp)
    (bt:with-lock-held ((dm-param-lock dm)) 
      (setf esc (dm-esc dm)
        ol (dm-ol dm)
        bll (dm-bll dm)
        sa (dm-sa dm)
        mp (dm-mp dm)))
    
    (cond (chunk
           (bt:with-recursive-lock-held ((act-r-chunk-lock (get-chunk chunk)))
             (command-output "Declarative parameters for chunk ~S:" chunk)
             
             ;;; Update activations ignoring partial matching
             ;;; by making the request spec only isa chunk.
             ;;; also turn off the activation trace 
             
             (when (and esc (or (find :activation parameters) (find :base-level parameters) (null parameters))
                        (not (and (numberp (chunk-retrieval-time chunk)) (= (chunk-retrieval-time chunk) (mp-time-ms)))))
               (compute-activation dm chunk (define-chunk-spec isa chunk)))
             
             (cond (parameters
                    (dolist (parameter parameters)
                      (multiple-value-bind (value output output-val)
                          (case parameter
                            (:name (values chunk "~a"))
                            (:activation (values (chunk-activation chunk) "~6,3f"))
                            (:base-level (values (chunk-last-base-level chunk) "~6,3f"))
                            (:creation-time (values (ms->seconds (chunk-creation-time chunk)) "~6/print-time-in-seconds/" (chunk-creation-time chunk)))
                            (:references 
                             (cond ((null ol)
                                    (values (cons (chunk-reference-count chunk)
                                                  (mapcar 'ms->seconds (chunk-reference-list chunk)))
                                            (concatenate 'string "(" (princ-to-string (chunk-reference-count chunk)) " ~{~/print-time-in-seconds/~^ ~})")
                                            (chunk-reference-list chunk)))
                                   ((numberp ol)
                                    (values (cons (chunk-reference-count chunk)
                                                  (mapcar 'ms->seconds (subseq (chunk-reference-list chunk) 0 ol)))
                                            (concatenate 'string "(" (princ-to-string (chunk-reference-count chunk)) " ~{~/print-time-in-seconds/~^ ~})")
                                            (subseq (chunk-reference-list chunk) 0 ol)))
                                   (t
                                    (values (list (chunk-reference-count chunk)) "(~d)" (chunk-reference-count chunk)))))
                            
                            (:source-spread (values (chunk-source-spread chunk) "~6,3f"))
                            (:sjis (values (get-all-chunk-sjis dm chunk) "~a"))
                            (:permanent-noise (values (chunk-permanent-noise chunk) "~6,3f"))
                            (:similarities (values (get-all-chunk-similarities dm chunk) "~a"))
                            (:retrieval-time (print-warning ":retrieval-time parameter has been renamed :last-retrieval-time")
                                             :error)
                            (:retrieval-activation (print-warning ":retrieval-activation parameter has been renamed :last-retrieval-activation")
                                                   :error)
                            (:last-retrieval-activation (values (chunk-retrieval-activation chunk) "~6,3f"))
                            (:last-retrieval-time (if (chunk-retrieval-time chunk) 
                                                      (values (ms->seconds (chunk-retrieval-time chunk)) "~6/print-time-in-seconds/" (chunk-retrieval-time chunk))
                                                    (values nil "~a")))
                            (:reference-count (values (chunk-reference-count chunk) " ~d"))
                            (:reference-list (values (mapcar 'ms->seconds (chunk-reference-list chunk)) 
                                                     "(~{~/print-time-in-seconds/~^ ~})"
                                                     (chunk-reference-list chunk)))
                            (t (print-warning "~A is not a declarative parameter for chunks." parameter)
                               :error))
                        (push-last value values)
                        (unless (eq value :error)
                          (command-output " ~S ~?" parameter output (list (if output-val output-val value))))))
                    values)
                   (t
                    (when esc
                      (command-output " :Activation ~6,3F~% :Permanent-Noise ~6,3F~% :Base-Level ~6,3F"
                                      (chunk-activation chunk) (chunk-permanent-noise chunk) (chunk-last-base-level chunk))
                      (when bll
                        (command-output " :Creation-Time ~/print-time-in-seconds/" (chunk-creation-time chunk))
                        (when ol
                          (command-output " :Reference-Count  ~d" (chunk-reference-count chunk)))
                        (unless (eq t ol)
                          (command-output " :Reference-List (~{~/print-time-in-seconds/~^ ~})" (chunk-reference-list chunk))))
                      (when sa
                        (command-output" :Source-Spread ~6,3F~% :Sjis ~a" (chunk-source-spread chunk) (get-all-chunk-sjis dm chunk)))
                      (when mp
                        (command-output " :Similarities ~a" (get-all-chunk-similarities dm chunk)))
                      (when (chunk-retrieval-time chunk)
                        (command-output " :Last-Retrieval-Activation ~6,3F~% :Last-Retrieval-Time ~6/print-time-in-seconds/"
                                        (chunk-retrieval-activation chunk) (chunk-retrieval-time chunk))))
                    chunk))))
          (t :error))))


(defun get-all-chunk-sjis (dm chunk)
  (let ((sjis nil)
        (js (mapcan (lambda (slot)
                      (let ((val (fast-chunk-slot-value-fct chunk slot)))
                        (when (chunk-p-fct val)
                          (list val))))
              (chunk-filled-slots-list-fct chunk))))
    
    (let (sji-hook mas nsji)
      (bt:with-lock-held ((dm-param-lock dm))
        (setf sji-hook (dm-sji-hook dm)
          mas (dm-mas dm)
          nsji (dm-nsji dm)))
      
      (setf sjis (mapcar (lambda (j) (list j (compute-sji j chunk sji-hook mas nsji))) (remove-duplicates (cons chunk js)))))
    
    (dolist (x (chunk-sjis chunk))
      (setf sjis (remove (car x) sjis :key 'car))
      (push x sjis))
    sjis))


(defun get-all-chunk-similarities (dm chunk)
  (let ((sims (chunk-similarities chunk)))
    (unless (find chunk sims :key 'car)
      (push (list chunk (bt:with-lock-held ((dm-param-lock dm)) (dm-ms dm))) sims))
    sims))

(defun set-dm-chunk-parameters-fct (dm chunk parameters)
  "Internal function to set the value of declarative chunk parameter(s)."
  (let ((values nil))
    (if chunk
        (let (bll ol)
          (bt:with-lock-held ((dm-param-lock dm))
            (setf bll (dm-bll dm) ol (dm-ol dm)))
          (bt:with-recursive-lock-held ((act-r-chunk-lock (get-chunk chunk)))
            (loop
              (unless parameters 
                (return values))
              (let* ((parameter (pop parameters))
                     (value (pop parameters)))
                (push-last
                 (case parameter
                   ;; Things that can't be set directly.
                   (:name (print-warning "CHUNK NAME CANNOT BE SET.")
                          :error)
                   (:activation (print-warning "CHUNK ACTIVATION CANNOT BE SET DIRECTLY.")
                                :error)
                   (:last-retrieval-activation (print-warning "CHUNK LAST-RETRIEVAL-ACTIVATION CANNOT BE SET DIRECTLY.")
                                               :error)
                   (:last-retrieval-time (print-warning "CHUNK LAST-RETRIEVAL-TIME CANNOT BE SET DIRECTLY.")
                                         :error)
                   (:source-spread (print-warning "CHUNK SOURCE-SPREAD CANNOT BE SET DIRECTLY: SET BUFFER CHUNKS AND/OR Sjis INSTEAD.")
                                   :error)
                   
                   (:base-level
                    (cond (bll
                           (print-warning "CHUNK BASE-LEVEL CANNOT BE SET DIRECTLY WHEN BASE LEVEL LEARNING IS ENABLED")
                           (print-warning "SET CREATION-TIME AND/OR REFERENCES INSTEAD.")
                           :error)
                          ((numberp value)
                           (setf (chunk-base-level chunk) value))
                          (t
                           (print-warning "CHUNK BASE-LEVEL MUST BE SET TO A NUMBER.")
                           :error)))
                   
                   (:creation-time
                    (cond ((and (numberp value) (<= value (mp-time)))
                           (setf (chunk-creation-time chunk) (safe-seconds->ms value 'sdp))
                           value)
                          (t
                           (print-warning "CHUNK CREATION-TIME MUST BE SET TO A NUMBER LESS THAN OR EQUAL TO THE CURRENT TIME.")
                           :error)))
                   
                   (:reference-count
                    (cond ((null bll)
                           (print-warning "WHEN BLL DISABLED BASE-LEVEL SHOULD BE SET DIRECTLY")
                           :error)
                          ((null ol)
                           (print-warning "WHEN OL IS DISABLED BASE-LEVEL MUST BE SET THROUGH THE REFERENCE-LIST")
                           :error)
                          ((not (numberp value))
                           (print-warning "CHUNK REFERENCE-COUNT MUST BE SET TO A NUMBER")
                           :error) 
                          ((numberp ol) 
                           (cond ((= (length (chunk-reference-list chunk)) value)
                                  ;; do nothing
                                  )
                                 ((> (length (chunk-reference-list chunk)) value)
                                  (setf (chunk-reference-list chunk) (subseq (chunk-reference-list chunk) 0 value)))
                                 ((= (length (chunk-reference-list chunk)) ol)
                                  ;; do nothing
                                  )
                                 (t 
                                  (setf (chunk-reference-list chunk) (adapt-references dm value (chunk-creation-time chunk)))))
                           (setf (chunk-reference-count chunk) value))
                          (t ;; dm-ol = t so just set the number...
                           (setf (chunk-reference-count chunk) value))))
                   
                   (:reference-list
                    (cond ((null bll)
                           (print-warning "WHEN BLL DISABLED BASE-LEVEL SHOULD BE SET DIRECTLY")
                           :error)
                          ((eq t ol)
                           (print-warning "WHEN OL IS T BASE-LEVEL MUST BE SET THROUGH THE REFERENCE-COUNT")
                           :error)
                          ((not (listp value))
                           (print-warning "CHUNK REFERENCE-LIST MUST BE SET TO A LIST OF NUMBERS")
                           :error)
                          ((not (every 'numberp value))
                           (print-warning "CHUNK REFERENCE-LIST MUST BE SET TO A LIST OF NUMBERS")
                           :error)
                          ((numberp ol)
                           (if (> (length value) ol)
                               (setf (chunk-reference-list chunk) (mapcar (lambda (x) (safe-seconds->ms x 'sdp)) (subseq value 0 ol)))
                             (setf (chunk-reference-list chunk) (mapcar (lambda (x) (safe-seconds->ms x 'sdp)) value)))
                           (when (< (chunk-reference-count chunk) (length (chunk-reference-list chunk)))
                             (setf (chunk-reference-count chunk) (length (chunk-reference-list chunk))))
                           (mapcar 'ms->seconds (chunk-reference-list chunk)))
                          (t ;; dm-ol = nil
                           (setf (chunk-reference-count chunk) (length value))
                           (setf (chunk-reference-list chunk) (mapcar (lambda (x) (safe-seconds->ms x 'sdp)) value))
                           (mapcar 'ms->seconds (chunk-reference-list chunk)))))
                   
                   (:references
                    (cond ((listp value)
                           (setf (chunk-reference-count chunk) (length value))
                           
                           (setf (chunk-reference-list chunk)
                             
                             (cond ((null ol)
                                    (mapcar (lambda (x) (safe-seconds->ms x 'sdp)) value))
                                   (ol ;; it's a number so ignore the
                                    ;; items in the list and even space them -
                                    ;; that's what 4/5 does...
                                    (adapt-references dm (length value) (chunk-creation-time chunk)))
                                   (t nil)))
                           
                           (cons (chunk-reference-count chunk)
                                 (mapcar 'ms->seconds (chunk-reference-list chunk))))
                          
                          ((numberp value)
                           (setf (chunk-reference-count chunk) value)
                           (setf (chunk-reference-list chunk)
                             (adapt-references dm value (chunk-creation-time chunk)))
                           
                           (cons (chunk-reference-count chunk)
                                 (mapcar 'ms->seconds (chunk-reference-list chunk))))
                          (t
                           (print-warning "CHUNK REFERENCES MUST BE SET TO A NUMBER OR A LIST.")
                           :error)))
             
                   (:sjis
                    (cond ((listp value)
                           (dolist (sji-pair value)
                             (let ((chunkj (car sji-pair))
                                   (sji (if (numberp (cdr sji-pair))
                                            (cdr sji-pair)
                                          (cadr sji-pair))))
                               (if (numberp sji)
                                   (if (chunk-p-fct chunkj)
                                       (add-sji-fct (list (list chunkj chunk sji)))
                                     (print-warning "~S is not the name of a chunk." chunkj))
                                 (print-warning "Sji VALUE ~S IS NOT A NUMBER." sji))))
                           (get-all-chunk-sjis dm chunk))
                          (t
                           (print-warning "CHUNK Sjis MUST BE SET USING A LIST OF CHUNK-NUMBER PAIRS.")
                           :error)))
                   
                   (:permanent-noise
                    (cond ((numberp value)
                           (setf (chunk-permanent-noise chunk) value))
                          (t
                           (print-warning "CHUNK PERMANENT-NOISE MUST BE SET TO A NUMBER.")
                           :error)))
                   
                   (:similarities
                    (cond ((listp value)
                           (dolist (similarity-pair value)
                             (let ((chunkj (car similarity-pair))
                                   (similarity (if (numberp (cdr similarity-pair))
                                                   (cdr similarity-pair)
                                                 (cadr similarity-pair))))
                               (if (numberp similarity)
                                   (if (chunk-p-fct chunkj)
                                       (progn
                                         (awhen (assoc chunkj (chunk-similarities chunk))
                                                (setf (chunk-similarities chunk)
                                                  (remove it (chunk-similarities chunk))))
                                         
                                         (push (list chunkj similarity) 
                                               (chunk-similarities chunk)))
                                     (print-warning "~S is not the name of a chunk." chunkj))
                                 (print-warning "CHUNK SIMILARITY VALUE ~S IS NOT A NUMBER." similarity))))
                           
                           (get-all-chunk-similarities dm chunk))
                          (t
                           (print-warning "CHUNK SIMILARITIES MUST BE SET USING A LIST OF CHUNK-NUMBER PAIRS.")
                           :error)))
                   
                   (t (print-warning "NO PARAMETER ~s DEFINED FOR CHUNKS." parameter)
                      :error))
                 values)))))
      :error)))

;;; 
;;; New user level functions
;;;

(defun reset-declarative-finsts ()
  (let ((dm (get-module declarative)))
    (if dm
        (bt:with-lock-held ((dm-state-lock dm)) (setf (dm-finsts dm) nil))
      (print-warning "No declarative module found - cannot reset the finsts."))))

(add-act-r-command "reset-declarative-finsts" 'reset-declarative-finsts "Clear all of the finsts marking recently retrieved chunks in declarative memory. No params.")

(defun print-dm-finsts ()
  (let ((dm (get-module declarative)))
    (when dm
      (let* ((finsts (remove-old-dm-finsts dm))
             (max-name-len (max 11
                               (if finsts
                                   (apply 'max 
                                          (mapcar (lambda (x)
                                                    (length (symbol-name (car x))))
                                            finsts))
                                 0))))
        
        (command-output  "~%~va    Time Stamp~%~v,1,0,'-a" max-name-len "Chunk name" (+ 14 max-name-len) "")
        
        (dolist (x finsts (mapcar (lambda (x) (list (car x) (ms->seconds (cdr x)))) finsts))
          (command-output  "~va    ~8/print-time-in-seconds/" max-name-len (car x) (cdr x)))))))

(add-act-r-command "print-dm-finsts" 'print-dm-finsts "Print a table showing any finsts marking recently retrieved chunks in declarative memory. No params.")


(defun print-activation-trace (time &optional (time-in-ms t))
  (let ((dm (get-module declarative)))
    (if dm
        (let* ((index (if time-in-ms time (safe-seconds->ms time 'print-activation-trace)))
               (data (gethash index (bt:with-lock-held ((dm-state-lock dm)) (dm-trace-table dm))))
               (sact (bt:with-lock-held ((dm-param-lock dm)) (dm-sact dm))))
          (if data
              (progn
                (if (sact-trace-only-recent data)
                    (when (dm-act-level sact 'high)
                      (command-output "Only recently retrieved chunks: ~s" (sact-trace-recents data)))
                  (when (sact-trace-remove-recent data)
                    (when (dm-act-level sact 'high)
                      (command-output "Removing recently retrieved chunks:")
                      (dolist (x (sact-trace-recents data))
                        (command-output "~s" x)))))

                (when (dm-act-level sact 'medium)
                  (dolist (x (sact-trace-matches data))
                    (command-output "Chunk ~s matches" x)))
                      
                (when (dm-act-level sact 'high)
                  (dolist (x (sact-trace-no-matches data))
                    (command-output "Chunk ~s does not match" x)))
      
                (when (sact-trace-esc data)
                  (dolist (chunk (sact-trace-chunks data))
                    (print-chunk-activation-details chunk sact)))

                (case (sact-trace-result-type data)
                  (:force
                   (when (dm-act-level sact 'low)
                     (command-output 
                      "Retrieval-set-hook function forced retrieval of chunk ~s" (sact-trace-result data))))
                  
                  (:force-fail
                   (when (dm-act-level sact 'low)
                     (command-output "Retrieval-set-hook function forced retrieval failure")))
                  
                  (:fail         
                   (when (dm-act-level sact 'low)
                     (if (sact-trace-result data)
                         (command-output "No chunk above the retrieval threshold: ~f" (sact-trace-result data))
                       (command-output "No matching chunk found retrieval failure"))))
                  
                  (:single
                   (when (dm-act-level sact 'low)
                     (command-output "Chunk ~s with activation ~f is the best"
                                   (car (sact-trace-result data)) (cdr (sact-trace-result data)))))
                  
                  (:multi
                   (when (dm-act-level sact 'low)
                     (command-output "Chunk ~s chosen among the chunks with activation ~f"
                                   (car (sact-trace-result data)) (cdr (sact-trace-result data)))))))
            
            (model-warning "No activation trace information available for time ~S" time)))
      (print-warning "No declarative module available for reporting activation trace."))))

(add-act-r-command "print-activation-trace" 'print-activation-trace "Prints out the activation trace from the saved data at the specified time. Params: time {ms?}")

(defun print-chunk-activation-details (chunk sact)
  (when (dm-act-level sact 'medium)
    (command-output "Computing activation for chunk ~s" (sact-chunk-name chunk)))
  
  (when (dm-act-level sact 'medium)
    (command-output "Computing base-level"))
  
  (case (sact-chunk-bl-style chunk)
    
    (:hook 
     (when (dm-act-level sact 'medium)
       (command-output "base-level hook returns: ~f" (sact-chunk-bl-result chunk))))
    
    (:learn 
     (when (dm-act-level sact 'medium)
       (command-output "Starting with blc: ~f" (sact-chunk-blc chunk)))
     
     (if (sact-chunk-zero-ref chunk)
         (when (dm-act-level sact 'low)
           (command-output "Cannot compute base-level for a chunk with no references."))
       
       (when (dm-act-level sact 'medium)
         (command-output "Computing base-level from ~d references (~{~/print-time-in-seconds/~^ ~})" 
                       (sact-chunk-bl-count chunk) (sact-chunk-bl-refs chunk))
         (command-output "  creation time: ~/print-time-in-seconds/ decay: ~f  Optimized-learning: ~s" 
                       (sact-chunk-bl-ct chunk) (sact-chunk-decay chunk) (sact-chunk-ol chunk))
         (command-output "base-level value: ~f" (sact-chunk-base-level chunk)))))
    
    (:simple 
     (if (sact-chunk-base-level chunk)
         (when (dm-act-level sact 'medium)
           (command-output "User provided chunk base-level: ~f" (sact-chunk-base-level chunk)))
       (when (dm-act-level sact 'medium)
         (command-output "Starting with blc: ~f" (sact-chunk-blc chunk))))))
  
  (when (dm-act-level sact 'medium)
    (command-output "Total base-level: ~f" (sact-chunk-bl-result chunk)))
  
  (when (sact-chunk-sa chunk)
    (when (dm-act-level sact 'medium)
      (command-output "Computing activation spreading from buffers")))
  
  (case (sact-chunk-sa chunk)
    (:hook 
     (when (dm-act-level sact 'medium)
       (command-output "spreading activation hook returns: ~f" (sact-chunk-sa-value chunk))))
    (:full
     (dolist (buffer (reverse (sact-chunk-sa-buffers chunk)))
       
       (when (dm-act-level sact 'medium)
         (command-output "  Spreading ~f from buffer ~s chunk ~s" (third buffer) (first buffer) (second buffer)))
       
       (when (dm-act-level sact 'medium)
         (command-output "    sources of activation are: ~s" (mapcar 'car (nthcdr 3 buffer))))
       
       (dolist (sji (nthcdr 3 buffer))
         (when (dm-act-level sact 'medium)
           (command-output "    Spreading activation  ~f from source ~s level  ~f times Sji ~f"
                         (second sji) (first sji) (third sji) (fourth sji)))))
     
     (when (dm-act-level sact 'medium)
       (command-output "Total spreading activation: ~f" (sact-chunk-sa-value chunk)))))
  
  (when (sact-chunk-pm chunk)
    (when (dm-act-level sact 'medium)
      (command-output "Computing partial matching component")))
  
  (case (sact-chunk-pm chunk)
    (:hook
     (when (dm-act-level sact 'medium)
       (command-output "partial matching hook returns: ~f" (sact-chunk-pm-value chunk))))
    (:full
     (dolist (slot (sact-chunk-pm-tests chunk))
       (when (dm-act-level sact 'medium)
         (command-output "  comparing slot ~S" (first slot))
         (command-output "  Requested: ~s ~s  Chunk's slot value: ~s" (second slot) (third slot) (fourth slot))
         (command-output "  similarity: ~f" (fifth slot))
         (command-output "  effective similarity value is ~f" (sixth slot))))
     
     (when (dm-act-level sact 'medium)
       (command-output "Total similarity score ~f" (sact-chunk-pm-value chunk)))))
  
  (case (sact-chunk-noise chunk)
    (:hook
     (when (dm-act-level sact 'medium)
       (command-output "noise hook returns: ~f" (sact-chunk-noise-p chunk))))
    (:full
     (when (dm-act-level sact 'medium)
       (command-output "Adding transient noise ~f" (sact-chunk-noise-t chunk))
       (command-output "Adding permanent noise ~f" (sact-chunk-noise-p chunk)))))
  
  (when (dm-act-level sact 'low)
    (command-output "Chunk ~s has an activation of: ~f" (sact-chunk-name chunk) (sact-chunk-total chunk))))


(defmacro print-chunk-activation-trace (chunk-name time &optional (time-in-ms t))
  `(print-chunk-activation-trace-fct ',chunk-name ,time ,time-in-ms))

(defun print-chunk-activation-trace-fct (chunk-name time &optional (time-in-ms t))
  (let ((dm (get-module declarative)))
    (if dm
        (let* ((index (if time-in-ms time (safe-seconds->ms time 'print-activation-trace)))
               (data (gethash index (bt:with-lock-held ((dm-state-lock dm)) (dm-trace-table dm)))))
          (if data
              (cond ((and (sact-trace-remove-recent data)                
                          (find chunk-name (sact-trace-recents data)))
                     (command-output "Chunk ~s was not considered because it was recently retrieved." chunk-name))
                    ((and (sact-trace-only-recent data)
                          (not (find chunk-name (sact-trace-recents data))))
                     (command-output "Chunk ~s was not considered because it was not recently retrieved." chunk-name))
                    ((find chunk-name (sact-trace-no-matches data))
                     (command-output "Chunk ~s did not match the request." chunk-name))
                    ((null (sact-trace-esc data))
                     (command-output "No activation calculation when :esc is nil."))
                    ((not (find chunk-name (sact-trace-chunks data) :key 'sact-chunk-name))
                     (command-output "Chunk ~s was not considered." chunk-name))
                    (t 
                     (let ((chunk (find chunk-name (sact-trace-chunks data) :key 'sact-chunk-name)))
                       (print-chunk-activation-details chunk (bt:with-lock-held ((dm-param-lock dm)) (dm-sact dm)))
                       (values (sact-chunk-total chunk) (sact-chunk-bl-result chunk) 
                               (sact-chunk-sa-value chunk) (sact-chunk-pm-value chunk)
                               (case (sact-chunk-noise chunk)
                                 (:hook
                                  (sact-chunk-noise-p chunk))
                                 (:full
                                  (+ (sact-chunk-noise-t chunk)
                                     (sact-chunk-noise-p chunk))))))))
            (model-warning "No activation trace information available for time ~S" time)))
      (print-warning "No declarative module available for reporting activation trace."))))


(defun print-chunk-activation-trace-external (chunk-name time &optional (time-in-ms t))
  (print-chunk-activation-trace-fct (string->name chunk-name) time time-in-ms))

(add-act-r-command "print-chunk-activation-trace" 'print-chunk-activation-trace-external 
                   "Prints out the activation trace from the saved data for a specific chunk at the specified time. Params: chunk time {ms?}")

(defun printed-chunk-activation-details (chunk sact)
  (let ((s (make-string-output-stream)))
    (when (dm-act-level sact 'medium)
      (format s "Computing activation for chunk ~s~%" (sact-chunk-name chunk)))
    
    (when (dm-act-level sact 'medium)
      (format s "Computing base-level~%"))
    
    (case (sact-chunk-bl-style chunk)
      
      (:hook 
       (when (dm-act-level sact 'medium)
         (format s "base-level hook returns: ~f~%" (sact-chunk-bl-result chunk))))
      
      (:learn 
       (when (dm-act-level sact 'medium)
         (format s "Starting with blc: ~f~%" (sact-chunk-blc chunk)))
       
       (if (sact-chunk-zero-ref chunk)
           (when (dm-act-level sact 'low)
             (format s "Cannot compute base-level for a chunk with no references.~%"))
         
         (when (dm-act-level sact 'medium)
           (format s "Computing base-level from ~d references (~{~/print-time-in-seconds/~^ ~})~%" 
                           (sact-chunk-bl-count chunk) (sact-chunk-bl-refs chunk))
           (format s "  creation time: ~/print-time-in-seconds/ decay: ~f  Optimized-learning: ~s~%" 
                           (sact-chunk-bl-ct chunk) (sact-chunk-decay chunk) (sact-chunk-ol chunk))
           (format s "base-level value: ~f~%" (sact-chunk-base-level chunk)))))
      
      (:simple 
       (if (sact-chunk-base-level chunk)
           (when (dm-act-level sact 'medium)
             (format s "User provided chunk base-level: ~f~%" (sact-chunk-base-level chunk)))
         (when (dm-act-level sact 'medium)
           (format s "Starting with blc: ~f~%" (sact-chunk-blc chunk))))))
    
    (when (dm-act-level sact 'medium)
      (format s "Total base-level: ~f~%" (sact-chunk-bl-result chunk)))
    
    (when (sact-chunk-sa chunk)
      (when (dm-act-level sact 'medium)
        (format s "Computing activation spreading from buffers~%")))
    
    (case (sact-chunk-sa chunk)
      (:hook 
       (when (dm-act-level sact 'medium)
         (format s "spreading activation hook returns: ~f~%" (sact-chunk-sa-value chunk))))
      (:full
       (dolist (buffer (reverse (sact-chunk-sa-buffers chunk)))
         
         (when (dm-act-level sact 'medium)
           (format s "  Spreading ~f from buffer ~s chunk ~s~%" (third buffer) (first buffer) (second buffer)))
         
         (when (dm-act-level sact 'medium)
           (format s "    sources of activation are: ~s~%" (mapcar 'car (nthcdr 3 buffer))))
         
         (dolist (sji (nthcdr 3 buffer))
           (when (dm-act-level sact 'medium)
             (format s "    Spreading activation  ~f from source ~s level  ~f times Sji ~f~%"
                             (second sji) (first sji) (third sji) (fourth sji)))))
       
       (when (dm-act-level sact 'medium)
         (format s "Total spreading activation: ~f~%" (sact-chunk-sa-value chunk)))))
    
    (when (sact-chunk-pm chunk)
      (when (dm-act-level sact 'medium)
        (format s "Computing partial matching component~%")))
    
    (case (sact-chunk-pm chunk)
      (:hook
       (when (dm-act-level sact 'medium)
         (format s "partial matching hook returns: ~f~%" (sact-chunk-pm-value chunk))))
      (:full
       (dolist (slot (sact-chunk-pm-tests chunk))
         (when (dm-act-level sact 'medium)
           (format s "  comparing slot ~S~%" (first slot))
           (format s "  Requested: ~s ~s  Chunk's slot value: ~s~%" (second slot) (third slot) (fourth slot))
           (format s "  similarity: ~f~%" (fifth slot))
           (format s "  effective similarity value is ~f~%" (sixth slot))))
       
       (when (dm-act-level sact 'medium)
         (format s "Total similarity score ~f~%" (sact-chunk-pm-value chunk)))))
    
    (case (sact-chunk-noise chunk)
      (:hook
       (when (dm-act-level sact 'medium)
         (format s "noise hook returns: ~f~%" (sact-chunk-noise-p chunk))))
      (:full
       (when (dm-act-level sact 'medium)
         (format s "Adding transient noise ~f~%" (sact-chunk-noise-t chunk))
         (format s "Adding permanent noise ~f~%" (sact-chunk-noise-p chunk)))))
    
    (when (dm-act-level sact 'low)
      (format s "Chunk ~s has an activation of: ~f~%" (sact-chunk-name chunk) (sact-chunk-total chunk)))
    (get-output-stream-string s)))
  
(defun printed-chunk-activation-trace (chunk-name time &optional (time-in-ms t))
  (let ((dm (get-module declarative)))
    (if dm
        (let* ((index (if time-in-ms time (safe-seconds->ms time 'print-activation-trace)))
               (data (gethash index (bt:with-lock-held ((dm-state-lock dm)) (dm-trace-table dm)))))
          (if data
              (cond ((and (sact-trace-remove-recent data)                
                          (find chunk-name (sact-trace-recents data)))
                     (format nil "Chunk ~s was not considered because it was recently retrieved.~%" chunk-name))
                    ((and (sact-trace-only-recent data)
                          (not (find chunk-name (sact-trace-recents data))))
                     (format nil "Chunk ~s was not considered because it was not recently retrieved.~%" chunk-name))
                    ((find chunk-name (sact-trace-no-matches data))
                     (format nil "Chunk ~s did not match the request.~%" chunk-name))
                    ((null (sact-trace-esc data))
                     (format nil "No activation calculation when :esc is nil.~%"))
                    ((not (find chunk-name (sact-trace-chunks data) :key 'sact-chunk-name))
                     (format nil "Chunk ~s was not considered.~%" chunk-name))
                    (t 
                     (let ((chunk (find chunk-name (sact-trace-chunks data) :key 'sact-chunk-name)))
                       (printed-chunk-activation-details chunk (bt:with-lock-held ((dm-param-lock dm)) (dm-sact dm))))))
            (format nil "#|Warning: No activation trace information available for time ~S |#~%" time)))
      (format nil "#|Warning: No declarative module available for reporting activation trace. |#~%"))))


(defun saved-activation-history ()
  (let ((dm (get-module declarative)))
    (if dm
        (let ((result nil)
              (data (bt:with-lock-held ((dm-state-lock dm)) (dm-trace-table dm)))
              (sact (bt:with-lock-held ((dm-param-lock dm)) (dm-sact dm))))
          
          (if (and sact (hash-table-p data))
              (progn
                (maphash (lambda (key value)
                           (push (cons key (mapcar 'sact-chunk-name (sact-trace-chunks value))) result))
                         data)
                (sort result '< :key 'car))
            (model-warning "No activation trace information available.")))
      (print-warning "No declarative module available for reporting activation trace."))))

(add-act-r-command "saved-activation-history" 'saved-activation-history "Returns a list of the lists of times and chunks for which activation data has been recorded. No params.")



(defmacro whynot-dm (&rest chunks)
  `(whynot-dm-fct ',chunks))

(defun whynot-dm-internal (&rest chunks)
  (if (current-model) 
      (let* ((dm (get-module declarative))
             (last (bt:with-lock-held ((dm-state-lock dm)) (dm-last-request dm)))
             (esc (bt:with-lock-held ((dm-param-lock dm)) (dm-esc dm))))
        (cond ((null last)
               (command-output "No retrieval request has been made."))
              ((not (last-request-p last))
               (command-output "Bad last-request stored -- contact Dan."))
              ((last-request-invalid last)
               (command-output "Last retrieval request was at time ~/print-time-in-seconds/ but was invalid because: ~a" (last-request-time last)
                               (case (last-request-invalid last)
                                 (:too-many "too many :recently-retrieved specifications")
                                 (:bad-modifier ":recently-retrieved had a modifier other than = or -.")
                                 (:bad-value ":recently-retrieved had a value other than t, nil, or reset")
                                 (:mp-not-allowed ":mp-value can only be used when partial matching is enabled.")
                                 (:mp-multi ":mp-value specified more than once.")
                                 (:mp-modifier ":mp-value had a modifier other than =.")
                                 (:mp-not-num ":mp-value was not a number or nil.")))
               (pprint-chunk-spec (last-request-spec last)))
              (t
               (command-output "Retrieval request made at time ~/print-time-in-seconds/:" (last-request-time last))
               (pprint-chunk-spec (last-request-spec last))
               
               (dolist (chunk-name (if (null chunks) (all-dm-chunks dm) chunks))
                 (let ((chunk chunk-name)) ;; they're converted before they get here now
                   (command-output "")
                   (if (chunk-p-fct chunk)
                       (if (chunk-in-dm chunk)
                           (progn
                             (pprint-chunks-fct (list chunk))
                             (when esc
                               (sdp-fct (list chunk))
                               (command-output ""))
                             (if (find chunk (last-request-matches last))
                                 (progn
                                   (command-output "~s matched the request" chunk)
                                   (if esc
                                       (cond ((eq chunk (car (last-request-best last)))
                                              (if (>= (chunk-retrieval-activation chunk) (last-request-rt last))
                                                  (command-output "~s was the chunk chosen to be retrieved" chunk)
                                                (command-output "~s was below the retrieval threshold ~f" chunk (last-request-rt last))))
                                             ((find chunk (last-request-best last))
                                              (if (>= (chunk-retrieval-activation chunk) (last-request-rt last))
                                                  (command-output "~s was not chosen among those with the highest activation" chunk)
                                                (command-output "~s was below the retrieval threshold ~f" chunk (last-request-rt last))))
                                             (t
                                              (if (>= (chunk-retrieval-activation chunk) (last-request-rt last))
                                                  (command-output "~s did not have the highest activation" chunk)
                                                (command-output "~s was below the retrieval threshold ~f" chunk (last-request-rt last)))))
                                     (if (eq chunk (car (last-request-best last)))
                                         (command-output "~s was the chunk chosen to be retrieved" chunk)
                                       (command-output "~s was not chosen as the chunk to be retrieved" chunk))))
                               
                               (cond ((and (eq (last-request-finst last) :marked)
                                           (not (find chunk (last-request-finst-chunks last))))
                                      (command-output "~s was not considered because it was not :recently-retrieved" chunk))
                                     ((and (eq (last-request-finst last) :unmarked)
                                           (find chunk (last-request-finst-chunks last)))
                                      (command-output "~s was not considered because it was :recently-retrieved" chunk))
                                     ((> (chunk-creation-time chunk) (last-request-time last))
                                      (command-output "~s was not considered because it was not in declarative memory at the time of the request" chunk))
                                     (t
                                      (command-output "~s did not match the request" chunk)))))
                         (command-output "Chunk ~s is not in the model's declarative memory." chunk))
                     (command-output "~s does not name a chunk in the current model." chunk))))
               (sort (copy-list (last-request-matches last)) (lambda (x y) 
                                                               (if (or (null (chunk-retrieval-activation x))
                                                                       (null (chunk-retrieval-activation y))
                                                                       (= (chunk-retrieval-activation x)
                                                                          (chunk-retrieval-activation y)))
                                                                   (let ((p1 (position x (last-request-best last)))
                                                                         (p2 (position y (last-request-best last))))
                                                                     (or (and p1 p2 (< p1 p2))
                                                                         (and p1 (null p2))))
                                                                 (> (chunk-retrieval-activation x)
                                                                    (chunk-retrieval-activation y))))))))
    (print-warning "Whynot-dm called with no current model.")))

(defun whynot-dm-external (&rest chunks)
  (apply 'whynot-dm-internal (string->name-recursive chunks)))

(add-act-r-command "whynot-dm" 'whynot-dm-external "Print the declarative memory matching information based on the last retrieval request. Params: chunk-name*.")

(defun whynot-dm-fct (chunks)
  (apply 'whynot-dm-internal chunks))

(defmacro simulate-retrieval-request (&rest spec)
  `(simulate-retrieval-request-fct ',spec))

(defun simulate-retrieval-request-fct (spec-list)
  (values (simulate-retrieval-request-plus-seed-fct spec-list)))

(defun simulate-retrieval-request-plus-seed-fct (spec-list)
  (let ((dm (get-module declarative)))
    (if dm
        (let ((seed (car (no-output (sgp :seed))))
              end-seed starting-finsts esc rt  er 
              mp offsets blc bll ol act-scale
              sa spreading-hook w-hook sji-hook mas nsji
              pm-hook ms md sim-hook cache-sim-hook
              noise-hook ans bl-hook)
          
          (bt:with-lock-held ((dm-state-lock dm))
            (setf starting-finsts (dm-finsts dm)))

          (bt:with-lock-held ((dm-param-lock dm))
            (setf
             rt (dm-rt dm)
             mp (dm-mp dm)
             esc (dm-esc dm)
             offsets (dm-offsets dm)
             blc (dm-blc dm)
             bll (dm-bll dm)
             ol (dm-ol dm)
             act-scale (dm-act-scale dm)  
             sa (dm-sa dm)
             spreading-hook (dm-spreading-hook dm)
             w-hook (dm-w-hook dm)
             sji-hook (dm-sji-hook dm)
             mas (dm-mas dm)
             nsji (dm-nsji dm)
             pm-hook (dm-partial-matching-hook dm)
             ms (dm-ms dm)
             md (dm-md dm)
             sim-hook (dm-sim-hook dm)
             cache-sim-hook (dm-cache-sim-hook-results dm)
             noise-hook (dm-noise-hook dm)
             ans (dm-ans dm)
             er (dm-er dm)
             bl-hook (dm-bl-hook dm)))
          
          (values 
           (unwind-protect 
            (let ((request (define-chunk-spec-fct spec-list)))
              (if (act-r-chunk-spec-p request)
                  
                  (let* ((filled (chunk-spec-filled-slots request))
                         (empty (chunk-spec-empty-slots request))
                         (chunk-list (mapcan (lambda (x) 
                                               (if (slots-vector-match-signature (car x) filled empty)
                                                   (copy-list (cdr x))
                                                 nil))
                                       (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunks dm)))))
                    
                    (flet ((invalid (warnings)
                                    (dolist (x warnings)
                                      (print-warning x))
                                    (return-from simulate-retrieval-request-plus-seed-fct)))
                    
                      (let ((requested-slots (chunk-spec-slots request)))
                        (when (member :recently-retrieved requested-slots)
                          (let ((recent (chunk-spec-slot-spec request :recently-retrieved)))
                            (cond ((> (length recent) 1)
                                   (invalid '("Invalid retrieval request." ":recently-retrieved parameter used more than once.")))
                                  ((not (or (eq '- (caar recent)) (eq '= (caar recent))))
                                   (invalid '("Invalid retrieval request." ":recently-retrieved parameter's modifier can only be = or -.")))
                                  ((not (or (eq t (third (car recent)))
                                            (eq nil (third (car recent)))
                                            (and (eq 'reset (third (car recent)))
                                                 (eq '= (caar recent)))))
                                   (invalid '("Invalid retrieval request." ":recently-retrieved parameter's value can only be t, nil, or reset.")))
                                  
                                  (t ;; it's a valid value for recently-retrieved
                                   
                                   (if (eq 'reset (third (car recent)))
                                       (bt:with-lock-held ((dm-state-lock dm))
                                         (setf (dm-finsts dm) nil))
                                     
                                     (let ((finsts (remove-old-dm-finsts dm)))
                                       
                                       (cond ((or (and (eq t (third (car recent)))   ;; = request t
                                                       (eq (caar recent) '=)) 
                                                  (and (null (third (car recent)))   ;; - request nil
                                                       (eq (caar recent) '-)))
                                              
                                              ;; only those chunks marked are available
                                              
                                              (setf chunk-list (intersection (mapcar 'car finsts) chunk-list))
                                              
                                              (command-output "Only recently retrieved chunks: ~s" chunk-list))
                                             (t
                                              
                                              (command-output "Removing recently retrieved chunks:")
                                              
                                              (setf chunk-list 
                                                (remove-if (lambda (x)
                                                             (when (member x finsts :key 'car :test 'eq-chunks-fct)
                                                               (command-output "~s" x)
                                                               t))
                                                           chunk-list))))))))))
                        
                        (when (member :mp-value requested-slots)
                          (let ((mp-value (chunk-spec-slot-spec request :mp-value)))
                            (cond ((> (length mp-value) 1)
                                   (invalid '("Invalid retrieval request." ":mp-value parameter used more than once.")))
                                  ((not (eq '= (caar mp-value)))
                                   (invalid '("Invalid retrieval request." ":mp-value parameter's modifier can only be =.")))
                                  ((not (numornil (third (car mp-value))))
                                   (invalid '("Invalid retrieval request." ":mp-value parameter's value can only be nil or a number.")))
                                  
                                  (t ;; it's a valid request
                                   (setf mp (third (car mp-value))))))))
                      
                    
                    
                    ;; don't print the activation trace info (should it still?, but if so
                    ;; what to do about which 'trace' to use since this is a command it
                    ;; should go to the command stream but activation computations go to
                    ;; the model stream...
                    
                    (let ((best-val nil)
                          (best nil)
                          (chunk-set 
                           (cond ((or (null esc) (null mp))
                                  (let ((found nil))
                                    (dolist (name chunk-list found)
                                      (if (match-chunk-spec-p name request)
                                          (progn
                                            (command-output "Chunk ~s matches" name)
                                            (push-last name found))
                                        (command-output "Chunk ~s does not match" name)))))
                                 (t ;; partial matching
                                  ;; everything that fits the general pattern:
                                  ;; filled and empty slots (already handled)
                                  ;; also test the inequalities >, <, >=, and <=                         
                                  (let* ((extra-spec (mapcan (lambda (x)
                                                               (unless (or (eq (car x) '=) (eq (car x) '-) (keywordp (second x))) 
                                                                 x))
                                                       (chunk-spec-slot-spec request)))
                                         (matches (if extra-spec
                                                      (find-matching-chunks (define-chunk-spec-fct extra-spec) :chunks chunk-list)
                                                    (nreverse chunk-list)))
                                         (non-matches (set-difference chunk-list matches)))
                                    
                                    (dolist (c non-matches)
                                      (command-output "Chunk ~s does not match" c))
                                    matches)))))
            
                      (if esc
                          (dolist (x chunk-set)
                            (compute-activation dm x request :params-provided t
                                  :act nil :sact nil :mp mp :esc esc :offsets offsets 
                                  :blc blc :bll bll :ol ol :act-scale act-scale
                                  :sa sa :spreading-hook spreading-hook :w-hook w-hook :sji-hook sji-hook 
                                  :mas mas :nsji nsji :pm-hook pm-hook :ms ms :md md :sim-hook sim-hook
                                  :cache-sim-hook cache-sim-hook :noise-hook noise-hook :ans ans :bl-hook bl-hook)
                            
                            (cond ((null best-val)
                                   (setf best-val (chunk-activation x))
                                   (push x best)
                                   (command-output "Chunk ~s has the current best activation ~f" x best-val))
                                  ((= (chunk-activation x) best-val)
                                   (push x best)
                                   (command-output "Chunk ~s matches the current best activation ~f" x best-val))
                                  ((> (chunk-activation x) best-val)
                                   (setf best-val (chunk-activation x))
                                   (setf best (list x))
                                   (command-output "Chunk ~s is now the current best with activation ~f" x best-val))
                                  (t
                                  (command-output "Chunk ~s has activation ~f" x (chunk-activation x)))))
                        (setf best chunk-set))
                      
                      (when (> (length best) 1)
                        (if er
                            (let ((b (random-item best)))
                              (setf best (cons b (remove b best))))
                          (setf best (sort best 'string<))))
            
                      (cond ((or (null best) 
                                 (and esc
                                      (< best-val rt)))
                             (if (null best)
                                 (command-output "No matching chunk found retrieval failure")
                               (command-output "No chunk above the retrieval threshold: ~f" rt)))
                   
                            ((= (length best) 1)
                             (command-output "Chunk ~s with activation ~f is the best" (car best) (chunk-activation (car best))))
                            
                            (t
                             (let ((best1 (car best)))
                               (command-output "Chunk ~s chosen among the chunks with activation ~f" best1 (chunk-activation best1)))))
                      
                      (sort chunk-set (lambda (x y) 
                                        (if (= (chunk-activation x)
                                               (chunk-activation y))
                                            (let ((p1 (position x best))
                                                  (p2 (position y best)))
                                              (or (and p1 p2 (< p1 p2))
                                                  (and p1 (null p2))))
                                          (> (chunk-activation x)
                                             (chunk-activation y))))))))
                (print-warning "Invalid request specification passed to simulate-retrieval-request.")))
                      
                      
            (progn ;; restore the parameters that were cleared/saved
              (no-output 
               (setf end-seed (car (sgp :seed))) 
               (sgp-fct (list :seed seed)))
              (bt:with-lock-held ((dm-state-lock dm))
                (setf (dm-finsts dm) starting-finsts))))
           end-seed))
      (print-warning "No declarative memory module available.  Simulate-retrieval-request cannot perform the request."))))


(defun external-simulate-retrieval-request (&rest spec)
  (simulate-retrieval-request-fct (decode-string-names spec)))

(defun external-simulate-retrieval-request-fct (spec)
  (simulate-retrieval-request-fct (decode-string-names spec)))

(defun external-simulate-retrieval-request-plus-seed-fct (spec)
  (simulate-retrieval-request-plus-seed-fct (decode-string-names spec)))


(add-act-r-command "simulate-retrieval-request" 'external-simulate-retrieval-request "Given the specification of a retrieval request, output the activation trace of what would happen if that request were made now. Params: 'request-details*'.")
(add-act-r-command "simulate-retrieval-request-fct" 'external-simulate-retrieval-request-fct "Given the specification of a retrieval request, output the activation trace of what would happen if that request were made now. Params: '(request-details*)'.")
(add-act-r-command "simulate-retrieval-request-plus-seed-fct" 'external-simulate-retrieval-request-plus-seed-fct "Given the specification of a retrieval request, output the activation trace of what would happen if that request were made now. Params: '(request-details*)'.")



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
