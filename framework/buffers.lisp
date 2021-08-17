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
;;; Filename    : buffers.lisp
;;; Version     : 6.0
;;; 
;;; Description : Functions that define the operation of buffers.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [] Finish documentation.
;;;               [] Investigate the copy sematics and probably optimize things.
;;;               [] Crazy idea - why not treat buffer parameters the same as
;;;                  chunk parameters and allow them to be user defined?
;;;               [] Have all the schedule-* functions allow a details keyword.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.20 Dan
;;;             : Creation
;;;
;;; 2004.12.13 Dan
;;;             : Added :details to the scheduled buffer events to clean up the
;;;             : traces.
;;;             : Reduced lines down to max of 80 chars.
;;; 2005.01.17 Dan
;;;             : * Removed calls to format in the scheduling.
;;; 2005.01.18 Dan
;;;             : * Removed call to get-parameters.
;;; 2005.02.01 Dan
;;;             : * Modified buffer-chunk so it prints the chunks as well when
;;;             :   specific buffers requested.
;;; 2005.02.03 Dan
;;;             : * Changed the default output for some functions to 'medium
;;;             :   or 'low to play friendly with the new detail level.
;;; 2005.02.04 Dan
;;;             : * Taking advantage of the fast-* chunk accessors.
;;; 2005.04.19 Dan
;;;             : * Added buffers-module-name to add to the API.
;;; 2005.04.23 Dan
;;;             : * Added the buffer-status command to print out the queries
;;;             :   for buffers.  Works basically like buffer-chunk.
;;;             : * Added the status-printing option to buffer definition
;;;             :   list (the fifth item) so a module can print extra status
;;;             :   info with buffer-status.
;;;             : * Added the requested keyword to the set-buffer-chunk 
;;;             :   command because the requested (formerly stuffed) status
;;;             :   of the buffer is being maintained internally now.
;;;             : * Updated the query-buffer command to handle buffer
;;;             :   requested/unrequested instead of passing it to the module.
;;;             : * For now, an overwrite always sets the requested to nil,
;;;             :   but maybe that needs to be user defineable too.
;;; 2005.05.11 Dan
;;;             : * Adjusted the params in the schedule-set-buffer-chunk
;;;             :   call so that requested chunks don't have to show that
;;;             :   in the trace - only unrequested chunks are marked.
;;; 2005.08.10 Dan
;;;             : * Added the uninstall-buffers function to support the 
;;;             :   undefine-modules function.  This is not designed to be used
;;;             :   any other way - buffer removal is a dangerous thing.
;;;             : * Updated the version to 1.0.
;;; 2005.08.16 Dan
;;;             : * Changes necessary to allow query of error t/nil without
;;;             :   any change to the module code - a module just has to 
;;;             :   respond to the "state error" query and the query-module
;;;             :   function handles the mapping now.  The change here is
;;;             :   to add error to the +required-buffer-queries+ list
;;;             :   and to now test that on doesn't try to override a required
;;;             :   query.
;;; 2006.01.17 Dan
;;;             : * Updated the module version to 1.0 since there haven't been 
;;;             :   any problems it's time to drop the "a".
;;; 2006.01.25 Dan
;;;             : * Added an option to allow details in schedule-module-request
;;;             :   to overwrite the default.   Should make that change for all
;;;             :   of the schedule- functions at some point, but for now I 
;;;             :   only need it for the request one...
;;; 2006.07.16 Dan
;;;             : * Fixed an issue with buffer-chunk-fct because now that
;;;             :   more chunk accessors print a warning for nil it has to
;;;             :   be more careful to avoid lots of warnings.
;;; 2006.11.20 Dan
;;;             : * Added require-module-warning? and module-warning to support
;;;             :   the warning mechanism added to the modules.
;;; 2007.01.30 Dan
;;;             : * Changed set-buffer-chunk so that it returns the name of the
;;;             :   copied chunk that was put into the buffer on a success and 
;;;             :   nil on any failure (instead of the requested value like it was).
;;; 2007.04.10 Dan
;;;             : * Cleaned up the warning strings for several of the schedule-*
;;;             :   functions because they had references to the function without
;;;             :   the schedule- on the front.
;;; 2007.04.13 Dan
;;;             : * Added a check for the value of the :cbct parameter to determine
;;;             :   if buffer copy actions should show a line in the trace.
;;; 2007.05.03 Dan
;;;             : * Fixed a bug in parse-buffers (which is used for module definition)
;;;             :   which would allow one to create a buffer that had a buffer
;;;             :   spread parameter that matched the name of a parameter in
;;;             :   another module.
;;; 2008.05.02 Dan
;;;             : * Added the show-buffer-chunks command for use by run-step.
;;;             :   May want to consider that a user command eventually.
;;; 2008.07.13 Dan
;;;             : * Fixed the return value for overwrite-buffer-chunk and added
;;;             :   a requested keyword parameter to it like set-buffer-chunk has
;;;             :   except the default value is nil.
;;; 2008.07.14 Dan 
;;;             : * Adjusted the event for schedule-overwrite... so that it 
;;;             :   only prints requested when it's nil like set-buffer-chunk.
;;; 2008.10.30 Dan
;;;             : * Took out the sgp for :cbct and use (show-copy-buffer-trace)
;;;             :   instead.
;;; 2009.02.10 Dan
;;;             : * Added an optional parameter to module-mod-request and
;;;             :   schedule-module-mod-request to allow 'invalid' modifications
;;;             :   to be passed to the module, but the default is to still check.
;;; 2009.09.09 Dan [1.1a]
;;;             : * Very experimental addition - buffers which can have a set
;;;             :   of potential chunks.  Those potential chunks can be put into
;;;             :   the buffer without copying.  The important thing to note is that
;;;             :   there can still only be one chunk "in the buffer" at any point 
;;;             :   in time.  Not all chunks can be used in that fashion.  Chunks
;;;             :   which have been cleared from a buffer are invalid for a buffer set 
;;;             :   because modules other than the owner may now have reference to
;;;             :   them.  It's also possible for a module to mark its chunks as
;;;             :   invalid for being put into any buffer set by setting the
;;;             :   chunk's buffer-set-invalid parameter to t.  If a chunk that
;;;             :   is already in a potential set gets marked as invalid it will
;;;             :   copied if it is attempted to be set in the buffer.
;;;             :   Eventually, but not yet, productions will be able to search 
;;;             :   through that set of chunks to find a matching one insead of only
;;;             :   matching the "current" chunk in the buffer.
;;;             :   The new constructs will be called a multi-buffer and a searchable
;;;             :   buffer.  A searchable buffer will always be a multi-buffer, 
;;;             :   but not all multi-buffers have to be searchable.  The type
;;;             :   of a buffer can be specified when creating it for a module 
;;;             :   (see the updates to define-module in the modules file) and a 
;;;             :   standard buffer will be the default.
;;;             : * There are 3 new commands which can be used for multi-buffers:
;;;             :   - store-m-buffer-chunk which takes a buffer name and a chunk
;;;             :   name and adds that chunk to the set of chunks available for
;;;             :   that buffer if the chunk isn't marked as invalid for a buffer set.
;;;             :   - remove-m-buffer-chunk which takes a buffer name and a chunk
;;;             :   name and removes that chunk from the set of chunks available
;;;             :   for that buffer.
;;;             :   - get-m-buffer-chunks takes a buffer name and then returns
;;;             :   a list of the chunk names for the chunks in that buffer's
;;;             :   set of available chunks if it is a multi-buffer.
;;;             : * The following commands are modified to work with multi-buffers:
;;;             :   - set-buffer-chunk - if the buffer is a multi-buffer and the
;;;             :   chunk being set is in the current set for that buffer then
;;;             :   it is not copied before putting it into the buffer.  See
;;;             :   also the change to clear-buffer since setting a buffer has
;;;             :   an implicit clearing.
;;;             :   - overwrite-buffer-chunk - same as set-buffer-chunk except
;;;             :   of course that there is no implicit clear with overwrite.
;;;             :   - clear-buffer - if the buffer is a multi-buffer and the chunk 
;;;             :   being cleared is in the current set it is removed from
;;;             :   the current set.  Whether or not the chunk is coming from a
;;;             :   multi-buffer it will have its buffer-set-invalid parameter
;;;             :   set to t.
;;;             :   buffer-chunk - the buffer-chunk command will also show
;;;             :   the set of potential chunks when displaying info about a multi-
;;;             :   buffer.
;;;             : * One other change has been made - buffer-instance no longer
;;;             :   does the mp and model tests since it's always called from
;;;             :   code that's already done so.
;;; 2009.09.29 Dan
;;;             : * Adding an erase-buffer command which takes a chunk out of a
;;;             :   buffer without clearing it.  Essentially a null overwrite.
;;;             :   Useful for modules which work with multi-buffers and want to
;;;             :   do a "soft clear" without losing the chunk to DM.  It does
;;;             :   not have a corresponding schedule- function since it's 
;;;             :   intended only for modules' internal use.
;;; 2010.01.14 Dan
;;;             : * Extending the multi buffer options so that one can have a
;;;             :   multi-buffer which does copy its chunks.  When creating a
;;;             :   module now the options for the buffer are :multi, :search,
;;;             :   :multi-copy, and :search-copy.  The :multi-copy option 
;;;             :   doesn't seem all that useful, but the :search-copy could
;;;             :   allow a module to keep a set of chunks to search over and
;;;             :   not have to be "careful" about how they are used.
;;;             : * Added a new command for working with multi-buffers:
;;;             :   remove-all-m-buffer-chunks.  It takes a buffer name and if
;;;             :   that names a multi-buffer it removes all chunks from its 
;;;             :   current set.
;;; 2010.01.18 Dan
;;;             : * Fixed a typo in parse-buffers.
;;; 2010.12.08 Dan
;;;             : * Added a check to parse-buffers to make sure a buffer's name
;;;             :   doesn't end in >, doesn't start with =, and that it isn't a 
;;;             :   keyword because those sorts of things will cause problems
;;;             :   in production parsing.
;;; 2011.04.27 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions.
;;; 2011.04.28 Dan
;;;             : * Suppress warnings about extending chunks at initial load.
;;; 2013.01.28 Dan
;;;             : * Removed a call to valid-slot-name and replaced it with the
;;;             :   command valid-chunk-type-slot.
;;; 2013.04.10 Dan
;;;             : * Added an additional keyword parameter to schedule-mod-buffer-chunk,
;;;             :   :extend.  If specified as t (default is nil) then it will 
;;;             :   first extend the chunk in the buffer if needed before making
;;;             :   the modifications.  This replaces the extend-buffer-chunk
;;;             :   command which was used separately in the production code to
;;;             :   make things more general and easily provide the extension
;;;             :   capability to modules through buffer modification requests.
;;; 2013.05.20 Dan [1.2]
;;;             : * Extend-buffer-chunk now passes the chunk name to extend-
;;;             :   chunk-type-slots since with static chunks we want to change
;;;             :   the chunk's type directly instead of "adding" a new slot.
;;;             : * Valid-chunk-modification now tests for possible slots instead
;;;             :   of valid-slots since a static chunk doesn't need to be 
;;;             :   extended again just because it doesn't currently have such
;;;             :   a slot.
;;; 2013.10.18 Dan
;;;             : * Changed buffer-spread so that it doesn't have to call sgp
;;;             :   and no-output.  No need for a module to use the external
;;;             :   interfaces from an internal function.
;;; 2014.02.26 Dan [2.0]
;;;             : * Use the new chunk/chunk-type functions for chunks without
;;;             :   type information.
;;; 2014.03.17 Dan
;;;             : * Test the request parameters up front in a module-request.
;;; 2014.03.18 Dan
;;;             : * Let modifications take a chunk-spec or list of mods and 
;;;             :   always send a chunk-spec to a module for modification requests.
;;; 2014.03.25 Dan
;;;             : * Added valid-request-params-for-buffer for production parsing
;;;             :   to use.
;;;             : * Added valid-buffer-queries for production parsing too.
;;; 2014.06.06 Dan
;;;             : * Fixed the declaim for define-chunk-spec-fct to include the
;;;             :   optional parameters.
;;; 2014.11.07 Dan
;;;             : * Schedule-query-buffer now tests the query spec passed in
;;;             :   eventhough it's not used.
;;; 2014.11.12 Dan
;;;             : * Changed the warning for get-m-buffer-chunks to say buffer set
;;;             :   for consistency with the docs and other warnings.
;;; 2014.12.17 Dan
;;;             : * Changed the declaims for define-*-spec-fct because they
;;;             :   didn't indicate multiple return values and some Lisps care
;;;             :   about that.
;;;             : * Removed the declaim for define-and-extend-chunk-spec-fct 
;;;             :   since it's not used now.
;;; 2015.03.18 Dan [2.1]
;;;             : * Adding a new buffer query value: failure.
;;;             :   This works with empty and full such that the three are 
;;;             :   mutially exclusive.  For a buffer to be marked as failure
;;;             :   requires calling the new set-buffer-failure command which
;;;             :   will clear the buffer (if it's full) and then set the failure
;;;             :   flag.  Clearing the buffer will clear the failure flag.
;;;             :   This is not a replacement for state error/error t because
;;;             :   those still depend on the module to report the information,
;;;             :   but presumably any module which sets its internal error flag
;;;             :   will also set the buffer's flag as well.  
;;;             :   The reason for adding this is so that a model has a way to
;;;             :   test and then clear the error without having to make a module
;;;             :   request to do so (isa clear or other such mechanisms).
;;; 2015.03.20 Dan
;;;             : * Added the backward compatability check so that "buffer empty"
;;;             :   returns true as long as the buffer is empty if back. compat.
;;;             :   is on.
;;; 2015.03.23 Dan
;;;             : * Added two keyword flags for set-buffer-failure to allow it
;;;             :   to not warn if the buffer is full :ignore-if-full (just ingore 
;;;             :   setting the flag) default of nil and :requested which sets
;;;             :   the buffer's act-r-buffer-requested slot to the value and the
;;;             :   defualt is t.  
;;;             : * Queries for requested/unrequested now work for either a full
;;;             :   buffer or one for which the failure flag is set.
;;; 2015.03.26 Dan
;;;             : * Check the backward compatibility flag and don't allow the
;;;             :   requested/unrequested to work with failure in 6.0 mode.
;;; 2015.06.04 Dan
;;;             : * Add a :time-in-ms parameter to all the scheduling functions.
;;; 2015.06.05 Dan
;;;             : * Use schedule-event-now where appropriate.
;;; 2015.07.28 Dan
;;;             : * Removed the *act-r-6.0-compatibility* hack.
;;; 2015.07.30 Dan
;;;             : * Changed the declaims for the schedule-* functions to add
;;;             :   the precondition keyword param.
;;; 2015.09.09 Dan [3.0]
;;;             : * Module requests and mod-requests can now be "tracked" by
;;;             :   the requester if desired.  There's an optional parameter
;;;             :   in the functions to make the requests and a keyword parameter
;;;             :   called tracked in the scheduling functions which if specified
;;;             :   as true will return a spec to the caller which can be used
;;;             :   to check the completed status of the request.  This means
;;;             :   that modules now also have to report when they "complete"
;;;             :   or "finish" the request which could mean various things
;;;             :   depending on the module i.e. it doesn't have to track the
;;;             :   busy/free status and could refer to something else if desired
;;;             :   (like the release of a finger for the keypress extension
;;;             :   which could span press and release requests).
;;;             : * When creating a buffer it can be marked as trackable or not.
;;;             :   If a seventh element in the buffer description list is non-nil
;;;             :   then the module is responsible for indicating when a request
;;;             :   to that buffer is "completed".  If the seventh element is nil
;;;             :   or not provided then all requests to that buffer will be 
;;;             :   considered as "completed" immediately upon receipt.
;;;             : * A define-buffer command is now available to replace the 
;;;             :   buffer definition list.  It has keyword parameters for the
;;;             :   components and returns an appropriately built list to specify
;;;             :   the buffer.
;;; 2015.09.14 Dan
;;;             : * Changed define-buffer to define-buffer-fct to indicate that
;;;             :   it is a function, and now providing a macro form as well.
;;;             : * Also fixed a bug with how it generated the list with respect
;;;             :   to the parameter and default.
;;; 2015.12.16 Dan
;;;             : * The scheduling request functions still need to return the
;;;             :   event, not just the trackable spec.  So, now they return 2
;;;             :   values.  The first is the event and the second is the spec
;;;             :   if the request specified tracking was desired.  Going to
;;;             :   refer to the returned spec as a "tag" and indcate that the
;;;             :   representation of a tag is unspecified to avoid confusion
;;;             :   or abuse and also allow for future changes.
;;; 2016.03.08 Dan
;;;             : * Fixed a bug with schedule-overwrite-buffer-chunk because it
;;;             :   could not generate a "requested" overwrite even though it
;;;             :   generated an event that made it look that way.
;;; 2016.11.22 Dan
;;;             : * Make buffer-chunk available through the dispatcher.
;;; 2016.11.29 Dan [4.0]
;;;             : * Buffer-chunk now returns a list of lists instead of a list
;;;             :   of cons cells.  The lists will have 1 or 2 elements.  If
;;;             :   the buffer is empty the list will only have the buffer name,
;;;             :   but if the buffer contains a chunk the list will have the
;;;             :   buffer name and the chunk name.
;;; 2016.12.01 Dan
;;;             : * Buffer-chunk-internal now accepts symbols or strings for the
;;;             :   names of the buffers.
;;; 2017.01.18 Dan
;;;             : * Removed the echo-act-r-output from buffer-chunk.  The assumption
;;;             :   now is that echoing is handled by the user.
;;;             : * Use handle-evaluate-results.
;;; 2017.01.26 Dan
;;;             : * Make buffer-status available remotely.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.06 Dan
;;;             : * Fixed buffer-status to accept string names.
;;; 2017.02.07 Dan
;;;             : * Make buffer-read and clear-buffer available remotely.
;;; 2017.02.08 Dan
;;;             : * Reworked the local versions of the remote commands to not
;;;             :   go out through the dispatcher.
;;; 2017.02.09 Dan [5.0]
;;;             : * Added a remote set-buffer-chunk and changed the requested
;;;             :   parameter from keyword to optional because of that.
;;; 2017.02.15 Dan
;;;             : * If buffer-read gets a string for the buffer name then return
;;;             :   a string of the chunk name.
;;; 2017.05.09 Dan
;;;             : * Wrap all the buffer access with lock protection to avoid
;;;             :   problems with remote and/or concurrent internal access.
;;; 2017.06.29 Dan
;;;             : * Put a lock on the *buffers-table* itself.
;;; 2017.07.13 Dan
;;;             : * Buffer-read and clear-buffer don't need to be single instance.
;;;             : * Protected all access to act-r-buffer-chunk.
;;; 2017.08.03 Dan
;;;             : * Protect access to act-r-buffer-multi slot with the lock.
;;; 2017.08.09 Dan
;;;             : * Added printed-buffer-chunk and printed-buffer-status to replace 
;;;             :   calls to capture-output for that info.
;;;             : * That requires changing the buffer-status-printing function
;;;             :   for a module to now return a string instead of directly
;;;             :   outputting the results.
;;; 2017.09.05 Dan
;;;             : * Add remote versions of buffers, printed-buffer-chunk, and
;;;             :   printed-buffer-status.         
;;; 2017.09.06 Dan
;;;             : * Updated buffer-status and printed-buffer-status to release
;;;             :   the buffer lock before calling the optional module status
;;;             :   function in case it needs to access the buffer too (and it
;;;             :   could be running remotely which would mean a separate thread).
;;;             :   Also make sure to use the symbol of the name for the list of
;;;             :   return values and output even if given a string.
;;;             : * Query-buffer also needs to release the lock before calling
;;;             :   the module function.
;;; 2017.10.06 Dan
;;;             : * Added an optional parameter to buffers which if provided as
;;;             :   non-nil will sort the list alphabetically before returning it.
;;; 2017.12.06 Dan
;;;             : * Updated the doc strings on some remote commands.
;;;             : * Added the schedule-simple-set and mod buffer remote commands.
;;; 2017.12.07 Dan
;;;             : * Added a parse-remote-buffers and fixed a bug in parse-buffers.
;;;             : * Have buffer-status use dispatch apply on the status fn.
;;; 2017.12.11 Dan
;;;             : * Decode the priority values and module name for the simple scheduling.
;;; 2018.01.26 Dan
;;;             : * Fixed the warning for printed-buffer-chunk when there is no
;;;             :   current model since it still said buffer-chunk.
;;; 2018.04.13 Dan
;;;             : * The buffer-lock is now recursive.
;;; 2018.06.08 Dan
;;;             : * Module-request now records the id for a tracked action from
;;;             :   calling track-request and that's what gets returned to the
;;;             :   caller instead of the spec itself.
;;; 2018.06.12 Dan
;;;             : * Only the external commands translate strings.
;;; 2018.06.13 Dan
;;;             : * Fixed a couple bugs introduced with the last update.
;;; 2018.06.14 Dan
;;;             : * Removed parse-remote-buffers and adjust parse-buffers to
;;;             :   allow remote test fns.
;;; 2018.06.27 Dan
;;;             : * Fixed a bug with printed-buffer-chunk-external since it 
;;;             :   needed to apply printed-buffer-chunk to the list (both take
;;;             :   &rest parameters).
;;; 2018.07.10 Dan
;;;             : * Changed query-list-or-spec->valid-query-spec to accept a
;;;             :   chunk-spec-id.
;;; 2018.07.11 Dan
;;;             : * Add external schedule-set/mod-buffer-chunk commands that use
;;;             :   options lists to replace the 'simple' commands.
;;; 2018.07.12 Dan
;;;             : * Allow the module-request functions to accept a chunk-spec id.
;;; 2018.07.25 Dan
;;;             : * Added external module-request and schedule-module-request
;;;             :   commands.
;;;             : * Added an external buffer-spread and buffers-module-name commands.
;;; 2018.07.26 Dan
;;;             : * Fixed schedule-module-request and the module-mod-request 
;;;             :   commands so they return the tracking id instead of the spec
;;;             :   itself (as was the previous mechanism).
;;; 2018.07.26 Dan [6.0]
;;;             : * Eliminating the request tracking mechanism because it is
;;;             :   unused as far as I know and it has a huge performance cost
;;;             :   since it operates regardless of need (can't 'know' if it's
;;;             :   being used so always has to happen).
;;;             : * Removed some lingering 'simple' commands.
;;; 2019.02.08 Dan
;;;             : * Keep track of an index and mask in the buffers and update
;;;             :   a bitvector in the model with full/empty buffers.
;;; 2019.04.03 Dan
;;;             : * Use the module instance stored in the buffer struct now to
;;;             :   call the module functions.
;;; 2019.05.03 Dan
;;;             : * Added a new command, max-buffer-index, which returns the
;;;             :   current *buffer-index* value.
;;; 2019.06.06 Dan
;;;             : * Added a multi-buffer-p command.
;;;             : * Added a remote get-m-buffer-chunks.
;;; 2019.09.25 Dan
;;;             : * Added a remote mod-buffer-chunk command.
;;; 2020.02.10 Dan [6.1]
;;;             : * Added a remote set-buffer-failure.
;;;             : * Added remote overwrite-buffer-chunk and schedule-overwrite-
;;;             :   buffer chunk and made the requested parameter for set-...
;;;             :   optional like set-buffer-chunk.
;;;             : * Added remote module-mod-request and schedule-module-mod-request.
;;; 2020.04.21 Dan
;;;             : * Added list-of-buffers-p because that's a useful test for
;;;             :   parameters.
;;; 2020.07.06 Dan
;;;             : * Don't need the show-buffer-copy fn since the event can just
;;;             :   specify nil because it's the details that matter.
;;; 2020.08.25 Dan
;;;             : * Replaced constants that had *...* with +...+ to avoid the
;;;             :   warnings in SBCL.
;;;             : * Switch from defconstant to the define-constant macro to 
;;;             :   avoid issues with SBCL (instead of redefining defconstant
;;;             :   for SBCL as was done previously).
;;;             : * Removed an unnecessary case in a cond to avoid a warning
;;;             :   from SBCL.
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

(declaim (ftype (function () t) show-copy-buffer-trace))
(declaim (ftype (function () t) current-model))
(declaim (ftype (function (t t &key (:maintenance t) (:module t) (:destination t) (:priority t) (:params t) (:details t) (:output t) (:time-in-ms t) (:precondition t)) t) schedule-event-relative))
(declaim (ftype (function (t &key (:maintenance t) (:module t) (:destination t) (:priority t) (:params t) (:details t) (:output t) (:precondition t)) t) schedule-event-now))
(declaim (ftype (function (t &optional t t) (values t t)) define-chunk-spec-fct))
(declaim (ftype (function (t) (values t t)) define-query-spec-fct))
(declaim (ftype (function (t) t) id-to-chunk-spec))

(suppress-extension-warnings)

;; Flag to prevent a chunk that's been cleared from a buffer set being 
;; put back into one.

(extend-chunks buffer-set-invalid)
(unsuppress-extension-warnings)

(define-constant +required-buffer-queries+ '(state buffer error))

(defvar *buffers-table* (make-hash-table))

(defvar *buffers-table-lock* (bt:make-lock "buffers-table"))

(defvar *buffer-index* -1)



(defun buffers (&optional sorted)
  (bt:with-lock-held (*buffers-table-lock*)
    (if sorted
        (sort (hash-table-keys *buffers-table*) 'string< :key 'symbol-name)
      (hash-table-keys *buffers-table*))))

(add-act-r-command "buffers" 'buffers "Return a list with all the currently defined buffers' names. Params {sorted}.")

(defun buffer-exists (name)
  (multiple-value-bind (buffer present)
      (bt:with-lock-held (*buffers-table-lock*)
        (gethash name *buffers-table*))
    (declare (ignore buffer))
    present))

(defmacro define-buffer (name &key param-name param-default request-params queries status-fn search multi copy)
  `(define-buffer-fct ',name :param-name ',param-name :param-default ',param-default
     :request-params ',request-params :queries ',queries :status-fn ',status-fn :search ',search
     :multi ',multi :copy ',copy))

(defun define-buffer-fct (name &key param-name param-default request-params queries status-fn search multi copy)
  (list name (if param-name 
                 (list param-name param-default) 
               (if param-default 
                   (list (read-from-string (format nil ":~S-activation" name)) param-default) 
                 nil))
        request-params queries status-fn 
        (cond ((and search copy) :search-copy)
              (search :search)
              ((and multi copy) :multi-copy)
              (multi :multi)
              (t nil))))


(defun parse-buffers (buffer-list)
  (let ((res nil))
    (dolist (buffer-def buffer-list res)
      
      ;; Adding a pre-check to make sure the name is a symbol, not a keyword, and that it
      ;; doesn't end in > to avoid crazy issues with production parsing
      
      (let ((name (if (atom buffer-def) buffer-def (car buffer-def))))
        
        (unless (and (symbolp name) (not (keywordp name)))
          (print-warning "Buffer name ~s is not valid because a buffer name must be a non-keyword symbol" name)
          (return-from parse-buffers :error))
        (let ((sym (symbol-name name)))
          (when (eql #\> (aref sym (1- (length sym))))
            (print-warning "Buffer name ~s is not valid because a buffer name must not end with the character '>'" name)
            (return-from parse-buffers :error))
          (unless (alphanumericp (char sym 0))
            (print-warning "Buffer name ~s is not valid because a buffer name must not start with an alphanumeric character" name)
            (return-from parse-buffers :error))))

      (cond ((or (atom buffer-def)
                 (and (listp buffer-def) (= (length buffer-def) 1)))
             (when (listp buffer-def)
               (setf buffer-def (car buffer-def)))
             (cond ((buffer-exists buffer-def)
                    (print-warning "Buffer name ~S already used, cannot reuse it.")
                    (return-from parse-buffers :error))
                   (t
                    (push (make-act-r-buffer 
                           :name buffer-def
                           :queries +required-buffer-queries+
                           :copy t
                           :parameter-name 
                           (read-from-string (format nil ":~S-activation" buffer-def)))
                          res))))
            ((and (listp buffer-def)
                  (buffer-exists (car buffer-def)))
             (print-warning "Buffer name ~S already used, cannot reuse it." buffer-def)
             (return-from parse-buffers :error))
            ; must be true since it's not an atom
            ;((not (listp buffer-def))
            ;  (print-warning "Invalid buffer specification: ~S" buffer-def)
            ;  (return-from parse-buffers :error))

            ((<= (length buffer-def) 7)
             (let (param-name param-default requests queries print-status multi searchable
                   (copy t))
               (if (and (second buffer-def) (listp (second buffer-def)))
                   (cond ((or 
                           (> (length (second buffer-def)) 2)
                           (not (keywordp (first (second buffer-def))))
                           (not (numberp (second (second buffer-def)))))
                          (print-warning "Invalid buffer specification: ~S" buffer-def)
                          (return-from parse-buffers :error))
                         (t
                          (setf param-name (first (second buffer-def)))
                          (setf param-default (second (second buffer-def)))))
                 (cond ((keywordp (second buffer-def))
                        (setf param-name (second buffer-def)))
                       ((null (second buffer-def))
                        (setf param-name
                          (read-from-string (format nil ":~S-activation" (first buffer-def)))))
                       (t
                        (print-warning "Invalid buffer specification: ~S" buffer-def)
                        (return-from parse-buffers :error))))
               (when (third buffer-def)
                 (cond ((and (listp (third buffer-def))
                             (every 'keywordp (third buffer-def)))
                        (setf requests (third buffer-def)))
                       (t
                        (print-warning "Invalid buffer specification: ~S" buffer-def)
                        (return-from parse-buffers :error))))
               (when (fourth buffer-def)
                 (cond ((and (listp (fourth buffer-def))
                             (every (lambda (x) 
                                      (and (symbolp x) (not (keywordp x))
                                           (not (find x +required-buffer-queries+))))
                                    (fourth buffer-def)))
                        (setf queries (fourth buffer-def)))
                       (t
                        (print-warning "Invalid buffer specification: ~S" buffer-def)
                        (return-from parse-buffers :error))))
               (when (fifth buffer-def)
                 (cond ((local-or-remote-function-or-nil (fifth buffer-def))
                        (setf print-status (fifth buffer-def)))
                       (t
                        (print-warning "Invalid buffer specification: ~S" buffer-def)
                        (print-warning "status function not valid")
                        (return-from parse-buffers :error))))
               
               (when (sixth buffer-def)
                 (case (sixth buffer-def)
                   (:multi (setf multi t)
                           (setf copy nil))
                   (:search
                    (setf multi t)
                    (setf searchable t)
                    (setf copy nil))
                   (:multi-copy 
                    (setf multi t))
                   (:search-copy
                    (setf multi t)
                    (setf searchable t))
                   (t
                    (print-warning "Invalid multi-buffer specification ~S for buffer ~s." buffer-def (first buffer-def))
                    (return-from parse-buffers :error))))
               
               (when (valid-parameter-name param-name) 
                 (print-warning "Buffer spread parameter ~S is already the name of a parameter." param-name)
                 (return-from parse-buffers :error))
               
               (push (make-act-r-buffer 
                      :name (first buffer-def)
                      :queries (append queries +required-buffer-queries+)
                      :requests requests
                      :parameter-name param-name
                      :spread param-default
                      :status-printing print-status
                      :multi multi
                      :searchable searchable
                      :copy copy)
                     res)))
            (t
             (print-warning "Invalid buffer specification: ~S" buffer-def)
             (return-from parse-buffers :error))))))


(defun max-buffer-index ()
  (bt:with-lock-held (*buffers-table-lock*)
    *buffer-index*))

(defun install-buffers (module-name buffers)
   (bt:with-lock-held (*buffers-table-lock*)
     (dolist (buffer buffers)
       (setf (gethash (act-r-buffer-name buffer) *buffers-table*) buffer)
       (setf (act-r-buffer-module buffer) module-name)
       (setf (act-r-buffer-index buffer) (incf *buffer-index*))
       (setf (act-r-buffer-mask buffer) (expt 2 *buffer-index*))
       (install-parameters 'buffer-params 
                           (list (define-parameter (act-r-buffer-parameter-name buffer)
                                     :owner t :valid-test 'numberp
                                   :default-value (if (null (act-r-buffer-spread buffer))
                                                      0
                                                    (act-r-buffer-spread buffer))
                                   :warning "a number"
                                   :documentation (format nil "source spread for the ~S buffer" (act-r-buffer-name buffer))))))))


(defun uninstall-buffers (buffers)
  "Necessary for undefining a module"
  (bt:with-lock-held (*buffers-table-lock*)
    (dolist (buffer buffers)
      (remhash (act-r-buffer-name buffer) *buffers-table*)
      (remove-parameter (act-r-buffer-parameter-name buffer)))))


(defstruct buffer-param-module (table (make-hash-table)) (lock (bt:make-lock "buffer-param-lock")))

(defun create-buffer-param-module (model-name)
  (declare (ignore model-name))
  (make-buffer-param-module))

(defun buffer-params-handler (instance param)
  (bt:with-lock-held ((buffer-param-module-lock instance))
    (if (consp param)
        (setf (gethash (car param) (buffer-param-module-table instance)) (cdr param))
      (gethash param (buffer-param-module-table instance)))))


(define-module buffer-params nil nil :version "1.0"
  :documentation "Module to hold and control the buffer parameters"
  :creation create-buffer-param-module
  :params buffer-params-handler)



(defun buffer-instance (buffer-name)
  (let ((model (current-model-struct)))
    (bt:with-lock-held ((act-r-model-buffers-lock model))
      (gethash buffer-name (act-r-model-buffers model)))))


(defun store-m-buffer-chunk (buffer-name chunk)
   (verify-current-model
    "store-m-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "store-m-buffer-chunk called with an invalid buffer name ~S" buffer-name)
        (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
          (cond ((null (act-r-buffer-multi buffer))
                 (print-warning "store-m-buffer-chunk cannot store a chunk in buffer ~s because it is not a multi-buffer" buffer-name))
                ((null (chunk-p-fct chunk))
                 (print-warning "store-m-buffer-chunk cannot store ~s in the buffer set because it does not name a chunk" chunk))
                ((chunk-buffer-set-invalid chunk)
                 (print-warning "store-m-buffer-chunk cannot store ~s in a buffer set because it has been marked as invalid for a buffer set most likely because it has been previously cleared from a buffer" chunk))
                (t
                 (setf (gethash chunk (act-r-buffer-chunk-set buffer)) t)
                 chunk)))))))

(defun remove-m-buffer-chunk (buffer-name chunk)
   (verify-current-model
    "remove-m-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "remove-m-buffer-chunk called with an invalid buffer name ~S" buffer-name)
        (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
          (cond ((null (act-r-buffer-multi buffer))
                 (print-warning "remove-m-buffer-chunk cannot remove a chunk from buffer ~s because it is not a multi-buffer" buffer-name))
                ((null (chunk-p-fct chunk))
                 (print-warning "remove-m-buffer-chunk cannot remove ~s from the buffer set because it does not name a chunk" chunk))
                (t
                 (remhash chunk (act-r-buffer-chunk-set buffer))
                 chunk)))))))

(defun get-m-buffer-chunks (buffer-name)
   (verify-current-model
    "remove-m-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "get-m-buffer-chunks called with an invalid buffer name ~S" buffer-name)
        (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
          (cond ((null (act-r-buffer-multi buffer))
                 (print-warning "get-m-buffer-chunks cannot return a buffer set for buffer ~s because it is not a multi-buffer" buffer-name))
                (t
                 (hash-table-keys (act-r-buffer-chunk-set buffer)))))))))

(defun external-get-m-buffer-chunks (buffer-name)
  (get-m-buffer-chunks (string->name buffer-name)))

(add-act-r-command "get-m-buffer-chunks" 'external-get-m-buffer-chunks "Return a list of possible chunks for specified multi-buffer. Params: buffer-name." nil)

(defun remove-all-m-buffer-chunks (buffer-name)
   (verify-current-model
    "remove-all-m-buffer-chunks called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "remove-all-m-buffer-chunks called with an invalid buffer name ~S" buffer-name)
        (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
          (cond ((null (act-r-buffer-multi buffer))
                 (print-warning "remove-all-m-buffer-chunks cannot remove a chunk from buffer ~s because it is not a multi-buffer" buffer-name))
                (t
                 (clrhash (act-r-buffer-chunk-set buffer))
                 t)))))))

(defmacro buffer-chunk (&rest buffer-names)
  `(buffer-chunk-fct ',buffer-names))

  
(defun buffer-chunk-fct (buffer-names-list)
   (verify-current-model
    "buffer-chunk called with no current model."
    (let ((res nil))
      (dolist (buffer-name (if buffer-names-list
                               buffer-names-list
                             (buffers))
                           res)
        (let ((buffer (buffer-instance buffer-name)))
          (if buffer
              (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
                (let ((chunk (act-r-buffer-chunk buffer)))
                  (command-output "~S: ~S ~@[[~s]~]~@[ {~{~s~^ ~}}~]" buffer-name chunk
                                  (when chunk (chunk-copied-from-fct chunk))
                                  (when (act-r-buffer-multi buffer)
                                    (get-m-buffer-chunks buffer-name)))
                  (when buffer-names-list
                    (pprint-chunks-fct (list chunk)))
                  (push-last (if buffer-names-list
                                 chunk
                               (if chunk
                                   (list buffer-name chunk)
                                 (list buffer-name)))
                             res)))
            (push-last (if buffer-names-list
                           :error
                         (list :error)) 
                       res)))))))

(defun buffer-chunk-external (&rest buffer-names-list)
  (buffer-chunk-fct (string->name-recursive buffer-names-list)))

(add-act-r-command "buffer-chunk" 'buffer-chunk-external "Print the contents of specified buffers. Params: {buffer-name}*.")

(defun printed-buffer-chunk (&rest buffer-names-list)
   (verify-current-model
    "printed-buffer-chunk called with no current model."
    (let ((s (make-string-output-stream)))
      (dolist (buffer-name (if buffer-names-list
                               buffer-names-list
                             (buffers)))
        (let ((buffer (buffer-instance buffer-name)))
          (when buffer
            (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
              (let ((chunk (act-r-buffer-chunk buffer)))
                (format s "~S: ~S ~@[[~s]~]~@[ {~{~s~^ ~}}~]~%"
                  buffer-name chunk (when chunk (chunk-copied-from-fct chunk))
                  (when (act-r-buffer-multi buffer)
                    (get-m-buffer-chunks buffer-name)))
                (when buffer-names-list
                  (format s "~a" (printed-chunk chunk))))))))
      (get-output-stream-string s))))


(defun printed-buffer-chunk-external (&rest buffer-names-list)
  (apply 'printed-buffer-chunk (string->name-recursive buffer-names-list)))

(add-act-r-command "printed-buffer-chunk" 'printed-buffer-chunk-external "Return a string of the printed output for the contents of specified buffers. Params: {buffer-name}*.")

(defun show-buffer-chunks ()
  (dolist (buffer-name (buffers))
    (let* ((chunk (buffer-read buffer-name)))
          (if chunk
              (progn
                (command-output "~S:" buffer-name)
                (pprint-chunks-fct (list chunk)))
            (command-output "~S: empty" buffer-name)))))


(defmacro buffer-status (&rest buffer-names)
  `(buffer-status-fct ',buffer-names))

(defun buffer-status-fct (buffer-names-list)
   (verify-current-model
    "buffer-status called with no current model."
    (let ((res nil))
      (dolist (buffer-name (aif buffer-names-list it (buffers)) res)
        (let ((buffer (buffer-instance buffer-name))
              (status nil))
          (if buffer
              (progn
                (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
                  (command-output "~S:" buffer-name)
                  (command-output "  buffer empty          : ~S"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer empty))))
                  (command-output "  buffer full           : ~S"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer full))))
                  (command-output "  buffer failure        : ~S"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer failure))))
                  
                  (command-output "  buffer requested      : ~S"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer requested))))
                  (command-output "  buffer unrequested    : ~S"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer unrequested))))
                  (command-output "  state free            : ~S"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(state free))))
                  (command-output "  state busy            : ~S"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(state busy))))
                  (command-output "  state error           : ~S"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(state error))))
                  (setf status (act-r-buffer-status-printing buffer)))
                
                (awhen status (command-output (dispatch-apply it)))
                
                (push-last buffer-name res))
            (push-last :error res)))))))

(defun printed-buffer-status (&rest buffer-names-list)
   (verify-current-model
    "printed-buffer-status called with no current model."
    (let ((s (make-string-output-stream)))
      (dolist (buffer-name (aif buffer-names-list it (buffers)))
        (let ((buffer (buffer-instance buffer-name))
              (status nil))
          (when buffer
            (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
              (format s "~S:~%" buffer-name)
              (format s "  buffer empty          : ~S~%"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer empty))))
              (format s "  buffer full           : ~S~%"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer full))))
              (format s "  buffer failure        : ~S~%"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer failure))))
              
              (format s "  buffer requested      : ~S~%"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer requested))))
              (format s "  buffer unrequested    : ~S~%"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(buffer unrequested))))
              (format s "  state free            : ~S~%"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(state free))))
              (format s "  state busy            : ~S~%"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(state busy))))
              (format s "  state error           : ~S~%"  (query-buffer-no-lock buffer (define-chunk-spec-fct '(state error))))
              (setf status (act-r-buffer-status-printing buffer)))
              
            (awhen status (format s "~a~%" (dispatch-apply it))))))
      (get-output-stream-string s))))

(defun buffer-status-external (&rest buffer-names-list)
  (buffer-status-fct (string->name-recursive buffer-names-list)))

(defun printed-buffer-status-external (&rest buffer-names-list)
  (apply 'printed-buffer-status (string->name-recursive buffer-names-list)))

(add-act-r-command "buffer-status" 'buffer-status-external "Print the status information for a buffer and its module. Params: {buffer-name}*.")

(add-act-r-command "printed-buffer-status" 'printed-buffer-status-external "Return a string of the printed output for the status of specified buffers. Params: {buffer-name}*.")


(defun buffer-read (buffer-name)  
   (verify-current-model
    "buffer-read called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if buffer 
          (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
            (act-r-buffer-chunk buffer))
        (print-warning "Buffer-read called with an invalid buffer name ~S" buffer-name)))))

(defun buffer-read-external (buffer-name)
  (buffer-read (string->name buffer-name)))

(add-act-r-command "buffer-read" 'buffer-read-external "Return the name of the chunk in a buffer. Params: buffer-name." nil)


(defun schedule-buffer-read (buffer-name time-delta &key (module :none) (priority 0) (output t) time-in-ms)
   (verify-current-model
    "schedule-buffer-read called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-buffer-read called with an invalid buffer name ~S" buffer-name))
            ((not (numberp time-delta))
             (print-warning "schedule-buffer-read called with a non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-buffer-read called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta 'buffer-read-action 
                                      :time-in-ms time-in-ms
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name)
                                      :output output))))))

(defun buffer-read-report (buffer-name &key (module :none))
   (verify-current-model
    "buffer-read-report called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "buffer-read-report called with an invalid buffer name ~S" buffer-name))
            (t
             (schedule-event-now 'buffer-read-action 
                                 :module module
                                 :priority :max 
                                 :params (list buffer-name)
                                 :output t)
             (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
               (act-r-buffer-chunk buffer)))))))


(defun buffer-read-action (buffer-name)
  (declare (ignore buffer-name)))

(defun query-buffer-no-lock (buffer query-spec)
  (do ((module (act-r-buffer-module-instance buffer))
       (queries (act-r-chunk-spec-slots query-spec) (cdr queries)))
      ((null queries) t)
    (let* ((test (act-r-slot-spec-modifier (car queries)))
           (query (act-r-slot-spec-name (car queries)))
           (value (act-r-slot-spec-value (car queries)))
           (result (cond ((eq query 'buffer)
                          (case value 
                            ;; full, empty, and failure are
                            ;; mutially exclusive.  Full doesn't
                            ;; need to check the failure flag because
                            ;; it gets cleared automatically when a
                            ;; chunk is set in the buffer.  Empty needs
                            ;; to check however since the flag may or
                            ;; may not be set when it's empty.  
                            ;;
                            ;; requested/unrequested can now also be set
                            ;; when there's a failure.  
                            ;; Not currently used by the modules, but the
                            ;; idea is to flag when it's an unrequested
                            ;; error incase the model cares i.e. a visual
                            ;; reencoding which finds nothing.
                            
                            (full (act-r-buffer-chunk buffer))
                            (empty (not (or (act-r-buffer-chunk buffer) (find :failure (act-r-buffer-flags buffer)))))
                            (failure (find :failure (act-r-buffer-flags buffer)))
                            
                            (requested (and (or (act-r-buffer-chunk buffer) (find :failure (act-r-buffer-flags buffer)))
                                            (act-r-buffer-requested buffer)))
                            (unrequested (and (or (act-r-buffer-chunk buffer) (find :failure (act-r-buffer-flags buffer)))
                                              (null (act-r-buffer-requested buffer))))
                            (t 
                             (model-warning "Unknown buffer query for 'buffer ~S'" value)
                             (return-from query-buffer-no-lock nil))))
                         (t (query-module module (act-r-buffer-name buffer) query value)))))
      (when (or (and (eql test '=) (null result))
                (and (eql test '-) result))
        (return-from query-buffer-no-lock nil)))))


(defun query-buffer (buffer-name queries-list-or-spec)
   (verify-current-model
    "query-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "query-buffer called with an invalid buffer name ~S" buffer-name)
        (let ((query-spec (query-list-or-spec->valid-query-spec queries-list-or-spec)))
          (cond ((not (act-r-chunk-spec-p query-spec))
                 (print-warning "Invalid query-buffer of buffer ~s with queries-list-or-spec ~s" buffer-name queries-list-or-spec))
                ((not (every (lambda (x)
                               (member (act-r-slot-spec-name x) (act-r-buffer-queries buffer)))
                             (act-r-chunk-spec-slots query-spec)))
                 (print-warning "Invalid query-buffer ~S.  Available queries to buffer ~S are ~S." queries-list-or-spec buffer-name (act-r-buffer-queries buffer)))
                (t
                 (let (buffer-chunk flags requested)
                   (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
                     (setf buffer-chunk (act-r-buffer-chunk buffer)
                       flags (act-r-buffer-flags buffer)
                       requested (act-r-buffer-requested buffer)))
                   (do ((module (act-r-buffer-module-instance buffer))
                        (queries (act-r-chunk-spec-slots query-spec) (cdr queries)))
                       ((null queries) t)
                     (let* ((test (act-r-slot-spec-modifier (car queries)))
                            (query (act-r-slot-spec-name (car queries)))
                            (value (act-r-slot-spec-value (car queries)))
                            (result (cond ((eq query 'buffer)
                                           (case value 
                                             ;; full, empty, and failure are
                                             ;; mutially exclusive.  Full doesn't
                                             ;; need to check the failure flag because
                                             ;; it gets cleared automatically when a
                                             ;; chunk is set in the buffer.  Empty needs
                                             ;; to check however since the flag may or
                                             ;; may not be set when it's empty.  
                                             ;;
                                             
                                             ;; requested/unrequested can now also be set
                                             ;; when there's a failure.  
                                             ;; Not currently used by the modules, but the
                                             ;; idea is to flag when it's an unrequested
                                             ;; error incase the model cares i.e. a visual
                                             ;; reencoding which finds nothing.
                                             
                                             (full buffer-chunk)
                                             (empty (not (or buffer-chunk (find :failure flags))))
                                             (failure (find :failure flags))
                                             
                                             (requested (and (or buffer-chunk (find :failure flags))
                                                             requested))
                                             (unrequested (and (or buffer-chunk (find :failure flags))
                                                               (null requested)))
                                             (t 
                                              (model-warning "Unknown buffer query for 'buffer ~S'" value)
                                              (return-from query-buffer nil))))
                                          (t (query-module module buffer-name query value)))))
                       (when (or (and (eql test '=) (null result))
                                 (and (eql test '-) result))
                         (return-from query-buffer nil))))))))))))

(defun schedule-query-buffer (buffer-name queries-list-or-spec time-delta &key (module :none) (priority 0) (output t) time-in-ms)
   (verify-current-model
    "schedule-query-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name))
          (query-spec (query-list-or-spec->valid-query-spec queries-list-or-spec)))
      (cond ((not (act-r-chunk-spec-p query-spec))
             (print-warning "schedule-query-buffer called with an invalid query specification ~s" queries-list-or-spec))
            ((null buffer)
             (print-warning "schedule-query-buffer called with an invalid buffer name ~S" buffer-name))
            ((not (numberp time-delta))
             (print-warning "schedule-query-buffer called with non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-query-buffer called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta 'query-buffer-action 
                                      :time-in-ms time-in-ms
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name queries-list-or-spec)
                                      :details (concatenate 'string (symbol-name 'query-buffer-action) " " (symbol-name buffer-name))
                                      :output output))))))

(defun query-buffer-action (buffer-name queries)
  (declare (ignore buffer-name queries)))

(defun clear-buffer (buffer-name)
  (verify-current-model
   "clear-buffer called with no current model."
   (let ((buffer (buffer-instance buffer-name)))
     (cond ((null buffer)
            (print-warning "clear-buffer called with an invalid buffer name ~S" buffer-name))
           (t
            (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
               (prog1
                  (clear-buffer-process buffer buffer-name)
                  (let ((m (current-model-struct)))
                    (bt:with-lock-held ((act-r-model-buffers-lock m))
                       (setf (act-r-model-buffer-state m) (logandc2 (act-r-model-buffer-state m) (act-r-buffer-mask buffer))))))))))))


(defun clear-buffer-process (buffer bn)
  "This is what happens when the buffer clears.  Done separately because may need to be done from inside something that's already locked and don't want a recursive lock."
  ;; clear all the flags
  (setf (act-r-buffer-flags buffer) nil)
  
  (let ((chunk (act-r-buffer-chunk buffer)))
    
    (when chunk
      
      ;; remove the chunk
      (setf (act-r-buffer-chunk buffer) nil)
      
      ;; mark it invalid for a buffer-set and take it out of this one if needed
      (setf (chunk-buffer-set-invalid chunk) t)
      (when (act-r-buffer-multi buffer)
        (remhash chunk (act-r-buffer-chunk-set buffer)))
      
      ;; Pass it off to any module that wants to know
      (dolist (module (notified-modules))
        (notify-module module bn chunk)))
    
    chunk))

(defun clear-buffer-external (buffer-name)
  (clear-buffer (string->name buffer-name)))

(add-act-r-command "clear-buffer" 'clear-buffer-external "Clear the chunk from a buffer. Params: buffer-name." nil)


(defun schedule-clear-buffer (buffer-name time-delta &key (module :none) (priority 0) (output 'low) time-in-ms)
   (verify-current-model
    "schedule-clear-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-clear-buffer called with an invalid buffer name ~S" buffer-name))
            ((not (numberp time-delta))
             (print-warning "schedule-clear-buffer called with a non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-clear-buffer called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta 'clear-buffer 
                                      :time-in-ms time-in-ms
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name)
                                      :output output))))))


(defun erase-buffer (buffer-name)
   (verify-current-model
    "erase-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "erase-buffer called with an invalid buffer name ~S" buffer-name)
        
            (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
              (prog1
                  (let ((chunk (act-r-buffer-chunk buffer)))
                    
                    ;; erasing the buffer also clears all the flags
                    (setf (act-r-buffer-flags buffer) nil)
                    (setf (act-r-buffer-chunk buffer) nil)
                    chunk)
                (let ((m (current-model-struct)))
                  (bt:with-lock-held ((act-r-model-buffers-lock m))
                     (setf (act-r-model-buffer-state m) (logandc2 (act-r-model-buffer-state m) (act-r-buffer-mask buffer)))))))))))


(defun erase-buffer-external (buffer-name)
  (erase-buffer (string->name buffer-name)))

(add-act-r-command "erase-buffer" 'erase-buffer-external "Remove a chunk from a buffer without clearing the buffer. Params: buffer-name")



(defun set-buffer-failure (buffer-name &key (ignore-if-full nil) (requested t))
   (verify-current-model
    "set-buffer-failure called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "set-buffer-failure called with an invalid buffer name ~S" buffer-name)
        (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
          (if (act-r-buffer-chunk buffer)
              (unless ignore-if-full
                (print-warning "cannot set the failure flag when there is a chunk in the ~s buffer" buffer-name))
            (progn
              (pushnew :failure (act-r-buffer-flags buffer))
              (setf (act-r-buffer-requested buffer) requested)
              t)))))))


(defun set-buffer-failure-external (buffer-name &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'set-buffer-failure '(:ignore-if-full :requested))
    (when valid 
      (apply 'set-buffer-failure (string->name buffer-name) ol))))

(add-act-r-command "set-buffer-failure" 'set-buffer-failure-external "Set the failure flag for a buffer. Params: buffer-name {< ignore-if-full , requested >}")


(defun set-buffer-chunk (buffer-name chunk-name &optional (requested t))
  "Forces a copy unless it's a multi-buffer and this chunk is in the set"
   (verify-current-model
    "set-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "set-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (get-chunk chunk-name))
             (print-warning "set-buffer-chunk called with an invalid chunk name ~S" chunk-name))
            (t
             (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
               (when (act-r-buffer-chunk buffer)
                 (clear-buffer-process buffer buffer-name))
               
               (setf (act-r-buffer-requested buffer) requested)
               
               (let ((copy-name (if (or (act-r-buffer-copy buffer) (chunk-buffer-set-invalid chunk-name) (null (gethash chunk-name (act-r-buffer-chunk-set buffer))))
                                    (copy-chunk-fct chunk-name)
                                  chunk-name)))
                 (when (and (show-copy-buffer-trace) (not (eq copy-name chunk-name)))
                   (schedule-event-now nil :maintenance t :module 'buffer 
                                       :priority :max 
                                       :details (concatenate 'string "Buffer " (string buffer-name) " copied chunk " (string chunk-name) " to " (string copy-name)) 
                                       :output 'medium))
               
               ;; setting the buffer clears the failure flag
                 (setf (act-r-buffer-flags buffer) (remove :failure (act-r-buffer-flags buffer)))
                 
                 (let ((m (current-model-struct)))
                   (bt:with-lock-held ((act-r-model-buffers-lock m))
                     (setf (act-r-model-buffer-state m) (logior (act-r-model-buffer-state m) (act-r-buffer-mask buffer)))))
                 
               (setf (act-r-buffer-chunk buffer) copy-name))))))))

(defun set-buffer-chunk-external (buffer-name chunk-name &optional (requested t))
  (set-buffer-chunk (string->name buffer-name) (string->name chunk-name) requested))

(add-act-r-command "set-buffer-chunk" 'set-buffer-chunk-external "Copy a chunk directly into a buffer. Params: buffer-name chunk-name {requested?}")


(defun schedule-set-buffer-chunk (buffer-name chunk-name time-delta &key (module :none) (priority 0) (output 'low) (requested t) time-in-ms)
   (verify-current-model
    "schedule-set-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-set-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (get-chunk chunk-name))
             (print-warning "schedule-set-buffer-chunk called with an invalid chunk name ~S" chunk-name))
            ((not (numberp time-delta))
             (print-warning "schedule-set-buffer-chunk called with time-delta that is not a number: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-set-buffer-chunk called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta 'set-buffer-chunk 
                                      :time-in-ms time-in-ms
                                      :module module
                                      :priority priority 
                                      :params (if requested
                                                  (list buffer-name chunk-name)
                                                (list buffer-name chunk-name nil))
                                      :output output))))))


(defun external-schedule-set-buffer-chunk (buffer-name chunk-name time-delta &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'schedule-set-buffer-chunk '(:module :priority :output :requested :time-in-ms))
    (when valid 
      (apply 'schedule-set-buffer-chunk (string->name buffer-name) (string->name chunk-name) time-delta (convert-options-list-items ol '(:module :priority :output) nil)))))

(add-act-r-command "schedule-set-buffer-chunk" 'external-schedule-set-buffer-chunk 
                   "Create an event to occur at the specified amount of time from now to place a chunk in a buffer. Params: buffer-name chunk-name time-delay { <  module,  priority,  output, time-in-ms, requested > }."
                   nil)



(defun overwrite-buffer-chunk (buffer-name chunk-name &optional (requested nil))
  "Also forces a copy of the chunk unless it's in the set of a multi-buffer"
  (verify-current-model
   "overwrite-buffer-chunk called with no current model."
   (let ((buffer (buffer-instance buffer-name)))
     (cond ((null buffer)
            (print-warning "overwrite-buffer-chunk called with an invalid buffer name ~S" buffer-name))
           ((null (get-chunk chunk-name))
            (print-warning "overwrite-buffer-chunk called with an invalid chunk name ~S" chunk-name))
           (t
            (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
              
              (setf (act-r-buffer-requested buffer) requested)
              (let ((copy-name (if (or (act-r-buffer-copy buffer) (chunk-buffer-set-invalid chunk-name) (null (gethash chunk-name (act-r-buffer-chunk-set buffer))))
                                   (copy-chunk-fct chunk-name)
                                 chunk-name)))
                (when (and (show-copy-buffer-trace) (not (eq copy-name chunk-name)))
                  (schedule-event-now nil :maintenance t :module 'buffer 
                                      :priority :max 
                                      :details (concatenate 'string "Buffer " (string buffer-name) " copied chunk " (string chunk-name) " to " (string copy-name)) 
                                      :output 'medium))
                
                (let ((m (current-model-struct)))
                  (bt:with-lock-held ((act-r-model-buffers-lock m))
                    (setf (act-r-model-buffer-state m) (logior (act-r-model-buffer-state m) (act-r-buffer-mask buffer)))))
                
                (setf (act-r-buffer-chunk buffer) copy-name))))))))



(defun overwrite-buffer-chunk-external (buffer-name chunk-name &optional (requested t))
  (overwrite-buffer-chunk (string->name buffer-name) (string->name chunk-name) requested))

(add-act-r-command "overwrite-buffer-chunk" 'overwrite-buffer-chunk-external "Put a chunk in a buffer without clearing the buffer first. Params: buffer-name chunk-name {requested}")



(defun schedule-overwrite-buffer-chunk (buffer-name chunk-name time-delta &key (module :none) (priority 0) (output 'low) (requested nil) time-in-ms)
   (verify-current-model
    "overwrite-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-overwrite-buffer-chunk called with an invalid buffer name ~S" buffer-name))
            ((null (get-chunk chunk-name))
             (print-warning "schedule-overwrite-buffer-chunk called with an invalid chunk name ~S" chunk-name))
            ((not (numberp time-delta))
             (print-warning "schedule-overwrite-buffer-chunk called with a non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-overwrite-buffer-chunk called with an invalid priority ~S" priority))
            (t
             (schedule-event-relative time-delta  'overwrite-buffer-chunk 
                                      :time-in-ms time-in-ms
                                      :module module
                                      :priority priority 
                                      :params (list buffer-name chunk-name requested)
                                      :details (when requested
                                                   (format nil "~s ~s ~s" 'overwrite-buffer-chunk buffer-name chunk-name))
                                      :output output))))))



(defun external-schedule-overwrite-buffer-chunk (buffer-name chunk-name time-delta &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'schedule-overwrite-buffer-chunk '(:module :priority :output :requested :time-in-ms))
    (when valid 
      (apply 'schedule-overwrite-buffer-chunk (string->name buffer-name) (string->name chunk-name) time-delta (convert-options-list-items ol '(:module :priority :output) nil)))))

(add-act-r-command "schedule-overwrite-buffer-chunk" 'external-schedule-overwrite-buffer-chunk 
                   "Create an event to occur at the specified amount of time from now to place a chunk in a buffer without clearing the buffer first. Params: buffer-name chunk-name time-delay { <  module,  priority,  output, time-in-ms, requested > }."
                   nil)



(defun module-warning (buffer-name chunk-spec)
   (verify-current-model
    "module-warning called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "module-warning called with an invalid buffer name ~S" buffer-name))
            ((null (act-r-chunk-spec-p chunk-spec))
             (print-warning "module-warning called with an invalid chunk-spec ~S" chunk-spec))
            (t
             (warn-module (act-r-buffer-module-instance buffer) buffer-name chunk-spec))))))


(defun require-module-warning? (buffer-name)
  (verify-current-model
   "require-module-warning? called with no current model."
   (let ((buffer (buffer-instance buffer-name)))
     (cond ((null buffer)
            (print-warning "require-module-warning? called with an invalid buffer name ~S" buffer-name))
           (t
            (warn-module? (act-r-buffer-module-instance buffer)))))))


(defun module-request (buffer-name chunk-spec)
  (verify-current-model
   "module-request called with no current model."
   (let ((buffer (buffer-instance buffer-name)))
     (cond ((null buffer)
            (print-warning "module-request called with an invalid buffer name ~S" buffer-name))
           ((numberp chunk-spec)
            (aif (id-to-chunk-spec chunk-spec)
                 (module-request buffer-name it)
                 (print-warning "module-request called with an invalid chunk-spec id ~s" chunk-spec)))
           ((not (act-r-chunk-spec-p chunk-spec))
            (print-warning "module-request called with an invalid chunk-spec ~S" chunk-spec))
           ((not (zerop (logandc2 (act-r-chunk-spec-request-param-slots chunk-spec) (act-r-buffer-requests-mask buffer))))
            (print-warning "module-request to buffer ~s has invalid request parameters ~{~S~^, ~}" buffer-name
                           (slot-mask->names (logandc2 (act-r-chunk-spec-request-param-slots chunk-spec) (act-r-buffer-requests-mask buffer)))))  
           (t
            (request-module (act-r-buffer-module-instance buffer) buffer-name chunk-spec))))))

(defun external-module-request (buffer-name chunk-spec)
  (module-request (string->name buffer-name) chunk-spec))
  

(add-act-r-command "module-request" 'external-module-request "Send a request to a module. Params: buffer-name chunk-spec." nil)


(defun schedule-module-request (buffer-name chunk-spec time-delta &key (module :none) (priority 0) (output 'medium) (details nil) time-in-ms)
   (verify-current-model
    "schedule-module-request called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-module-request called with an invalid buffer name ~S" buffer-name))
            ((numberp chunk-spec)
             (aif (id-to-chunk-spec chunk-spec)
                  (schedule-module-request buffer-name it time-delta :module module :priority priority :output output :details details :time-in-ms time-in-ms)
                  (print-warning "schedule-module-request called with an invalid chunk-spec id ~s" chunk-spec)))
            ((null (act-r-chunk-spec-p chunk-spec))
             (print-warning "schedule-module-request called with an invalid chunk-spec ~S" chunk-spec))
            ((not (numberp time-delta))
             (print-warning "schedule-module-request called with a non-number time-delta: ~S" time-delta))
            ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
             (print-warning "schedule-module-request called with an invalid priority ~S" priority))
            ((not (zerop (logandc2 (act-r-chunk-spec-request-param-slots chunk-spec) (act-r-buffer-requests-mask buffer))))
             (print-warning "module-request to buffer ~s has invalid request parameters ~{~S~^, ~}" buffer-name
                            (slot-mask->names (logandc2 (act-r-chunk-spec-request-param-slots chunk-spec) (act-r-buffer-requests-mask buffer)))))
            (t
             (schedule-event-relative time-delta  'module-request 
                                          :time-in-ms time-in-ms
                                          :module module
                                          :priority priority 
                                          :params (list buffer-name chunk-spec)
                                          :details
                                          (if (stringp details)
                                              details
                                            (concatenate 'string (symbol-name 'module-request) " " (symbol-name buffer-name)))
                                          :output output))))))


(defun external-schedule-module-request (buffer-name chunk-spec time-delta &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'schedule-module-request '(:module :priority :output :details :time-in-ms))
    (when valid 
      (apply 'schedule-module-request (string->name buffer-name) (id-to-chunk-spec chunk-spec) time-delta (convert-options-list-items ol '(:module :priority :output) nil)))))

(add-act-r-command "schedule-module-request" 'external-schedule-module-request
                   "Create an event to occur at the specified amount of time from now to send a request to a module. Params: buffer-name chunk-spec time-delay { <  module,  priority,  output, time-in-ms, details > }."
                   nil)



(defun module-mod-request (buffer-name mod-list-or-spec)
   (verify-current-model
    "module-mod-request called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "module-mod-request called with an invalid buffer name ~S" buffer-name))
            ((null (bt:with-recursive-lock-held ((act-r-buffer-lock buffer)) (act-r-buffer-chunk buffer)))
             (print-warning "module-mod-request called with no chunk in buffer ~s" buffer-name))
            (t
             (let ((spec (mod-list-or-spec->valid-mod-spec mod-list-or-spec)))
               (cond ((null spec)
                      (print-warning "module-mod-request called with an invalid modification ~s" mod-list-or-spec))
                     ((not (zerop (logandc2 (act-r-chunk-spec-request-param-slots spec) (act-r-buffer-requests-mask buffer))))
                      (print-warning "module-mod-request to buffer ~s has invalid request parameters ~{~S~^, ~}" buffer-name
                            (slot-mask->names (logandc2 (act-r-chunk-spec-request-param-slots spec) (act-r-buffer-requests-mask buffer)))))
                     (t
                      (buffer-mod-module (act-r-buffer-module-instance buffer) buffer-name spec)))))))))


(defun external-module-mod-request (buffer-name mod-list)
  (module-mod-request (string->name buffer-name) (decode-string-names mod-list)))
  
(add-act-r-command "module-mod-request" 'external-module-mod-request "Send a modification request to a module. Params: buffer-name 'mod-spec'." nil)


(defun schedule-module-mod-request (buffer-name mod-list-or-spec time-delta &key (module :none) (priority 0) (output 'medium) time-in-ms)
   (verify-current-model
    "schedule-module-mod-request called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "schedule-module-mod-request called with invalid buffer name ~S" buffer-name))
            ((null (bt:with-recursive-lock-held ((act-r-buffer-lock buffer)) (act-r-buffer-chunk buffer)))
             (print-warning "schedule-module-mod-request called with no chunk in buffer ~s" buffer-name))
            (t
             (let ((spec (mod-list-or-spec->valid-mod-spec mod-list-or-spec)))
               (cond ((null spec)
                      (print-warning "schedule-module-mod-request called with an invalid modification ~s" mod-list-or-spec))
                     ((not (zerop (logandc2 (act-r-chunk-spec-request-param-slots spec) (act-r-buffer-requests-mask buffer))))
                      (print-warning "module-mod-request to buffer ~s has invalid request parameters ~{~S~^, ~}" buffer-name
                                     (slot-mask->names (logandc2 (act-r-chunk-spec-request-param-slots spec) (act-r-buffer-requests-mask buffer)))))
                     ((not (numberp time-delta))
                      (print-warning "schedule-module-mod-request called with a non-number time-delta: ~S" time-delta))
                     ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
                      (print-warning "schedule-module-mod-request called with an invalid priority ~S" priority))
                     (t
                      (schedule-event-relative time-delta  'module-mod-request 
                                                 :time-in-ms time-in-ms
                                                 :module module
                                                 :priority priority 
                                                 :params (list buffer-name spec)
                                                 :details
                                                 (concatenate 'string (symbol-name 'module-mod-request) " " (symbol-name buffer-name))
                                                 :output output)))))))))
  


(defun external-schedule-module-mod-request (buffer-name mod-list-or-spec time-delta &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'schedule-module-mod-request '(:module :priority :output :time-in-ms))
    (when valid 
      (apply 'schedule-module-mod-request (string->name buffer-name) (decode-string-names mod-list-or-spec) time-delta (convert-options-list-items ol '(:module :priority :output) nil)))))

(add-act-r-command "schedule-module-mod-request" 'external-schedule-module-mod-request
                   "Create an event to occur at the specified amount of time from now to send a modification request to a module. Params: buffer-name mod-spec time-delay { <  module,  priority,  output, time-in-ms > }."
                   nil)


(defun mod-list-or-spec->valid-mod-spec (list-or-spec)
  (if (act-r-chunk-spec-p list-or-spec)
      (and
       (zerop (act-r-chunk-spec-duplicate-slots list-or-spec))
       (zerop (act-r-chunk-spec-negated-slots list-or-spec))
       (zerop (act-r-chunk-spec-relative-slots list-or-spec))
       list-or-spec)
    (aif (and (listp list-or-spec) (define-chunk-spec-fct list-or-spec))
         (mod-list-or-spec->valid-mod-spec it)
         (mod-list-or-spec->valid-mod-spec (id-to-chunk-spec list-or-spec)))))

(defun query-list-or-spec->valid-query-spec (list-or-spec)
  (if (act-r-chunk-spec-p list-or-spec)
      (and
       (zerop (act-r-chunk-spec-relative-slots list-or-spec))
       (null (act-r-chunk-spec-slot-vars list-or-spec))
       list-or-spec)
    (aif (and (listp list-or-spec) (define-query-spec-fct list-or-spec))
         (query-list-or-spec->valid-query-spec it)
         (when (numberp list-or-spec)
           (query-list-or-spec->valid-query-spec (id-to-chunk-spec list-or-spec))))))

(defun mod-buffer-chunk (buffer-name mod-list-or-spec)
   (verify-current-model
    "mod-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "mod-buffer-chunk called with an invalid buffer name ~S" buffer-name)  
      (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
        (cond ((null (act-r-buffer-chunk buffer))
               (print-warning "mod-buffer-chunk called with no chunk in buffer ~s" buffer-name))
              (t
               (let ((spec (mod-list-or-spec->valid-mod-spec mod-list-or-spec)))
                 (cond ((null spec)
                        (print-warning "mod-buffer-chunk called with an invalid modification ~s" mod-list-or-spec))
                       (t
                        (mod-chunk-with-spec-fct (act-r-buffer-chunk buffer) spec)))))))))))


(defun external-mod-buffer-chunk (buffer-name mods)
  (mod-buffer-chunk (string->name buffer-name) (decode-string-names mods)))

(add-act-r-command "mod-buffer-chunk" 'external-mod-buffer-chunk 
                   "Modify a chunk in a buffer. Params: buffer-name 'modifications'."
                   nil)



(defun schedule-mod-buffer-chunk (buffer-name mod-list-or-spec time-delta &key (module :none) (priority 0) (output 'low) time-in-ms)
   (verify-current-model
    "schedule-mod-buffer-chunk called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (if (null buffer)
          (print-warning "schedule-mod-buffer-chunk called with an invalid buffer name ~S" buffer-name)  
        (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
          (cond ((null (act-r-buffer-chunk buffer))
                 (print-warning "schedule-mod-buffer-chunk called with no chunk in buffer ~s" buffer-name))
                ((not (numberp time-delta))
                 (print-warning "schedule-mod-buffer-chunk called with non-number time-delta: ~S" time-delta))
                ((and (not (numberp priority)) (not (eq priority :max)) (not (eq priority :min)))
                 (print-warning "schedule-mod-buffer-chunk called with an invalid priority ~S" priority))
                (t
                 (let ((spec (mod-list-or-spec->valid-mod-spec mod-list-or-spec)))
                   (if (null spec)
                       (print-warning "schedule-mod-buffer-chunk called with an invalid modification ~s" mod-list-or-spec)
                     (schedule-event-relative time-delta 'mod-buffer-chunk 
                                              :time-in-ms time-in-ms
                                              :module module
                                              :priority priority 
                                              :params (list buffer-name spec)
                                              :details (concatenate 'string (symbol-name 'mod-buffer-chunk) " " (symbol-name buffer-name))
                                              :output output))))))))))




(defun external-schedule-mod-buffer-chunk (buffer-name mods time-delta &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'schedule-mod-buffer-chunk '(:module :priority :output :time-in-ms))
    (when valid 
      (apply 'schedule-mod-buffer-chunk (string->name buffer-name) (decode-string-names mods) time-delta (convert-options-list-items ol '(:module :priority :output) nil)))))

(add-act-r-command "schedule-mod-buffer-chunk" 'external-schedule-mod-buffer-chunk 
                   "Create an event to occur at the specified amount of time from now to modify a chunk in a buffer. Params: buffer-name 'modifications' time-delay { <  module,  priority,  output, time-in-ms > }."
                   nil)
  
(defun buffer-spread (buffer-name)
   (verify-current-model
    "buffer-spread called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "buffer-spread called with an invalid buffer name ~S" buffer-name))
            (t
             (buffer-params-handler (get-module buffer-params) (act-r-buffer-parameter-name buffer)))))))


(defun external-buffer-spread (buffer-name)
  (buffer-spread (string->name buffer-name)))

(add-act-r-command "buffer-spread" 'external-buffer-spread "Return the current activation source spread value for a buffer. Params: buffer." nil)

(defun buffers-module-name (buffer-name)
   (verify-current-model
    "buffers-module-name called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "invalid buffer name ~S" buffer-name))
            (t
             (let ((module (act-r-buffer-module buffer)))
               (if module
                   module
                 (print-warning "Could not find a module for buffer ~S" buffer-name))))))))

(defun external-buffers-module-name (buffer-name)
  (buffers-module-name (string->name buffer-name)))

(add-act-r-command "buffers-module-name" 'external-buffers-module-name "Return the name of the moduel which provides the indicated buffer. Params: buffer." nil)


(defun searchable-buffer (buffer-name)
   (verify-current-model
    "searchable-buffer called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "invalid buffer name ~S" buffer-name))
            (t
             (act-r-buffer-searchable buffer))))))

(defun multi-buffer-p (buffer-name)
   (verify-current-model
    "multi-buffer-p called with no current model."
    (let ((buffer (buffer-instance buffer-name)))
      (cond ((null buffer)
             (print-warning "invalid buffer name ~S" buffer-name))
            (t
             (act-r-buffer-multi buffer))))))

(defun external-multi-buffer-p (buffer-name)
  (multi-buffer-p (string->name buffer-name)))

(add-act-r-command "multi-buffer-p" 'external-multi-buffer-p "Check if the named buffer is a multi-buffer. Params: buffer" nil)

(defun valid-request-params-for-buffer (buffer-name request-vector)
  (verify-current-model
   "valid-request-params-for-buffer called with no current model."
   (let ((buffer (buffer-instance buffer-name)))
     (and buffer (zerop (logandc2 request-vector (act-r-buffer-requests-mask buffer)))))))


(defun valid-buffer-queries (buffer-name)
  (verify-current-model
   "valid-buffer-queries called with no current model."
   (let ((buffer (buffer-instance buffer-name)))
     (and buffer (act-r-buffer-queries buffer)))))



(defun buffer-index (buffer-name)
  (let ((buffer (buffer-instance buffer-name)))
    (if (null buffer)
        (print-warning "buffer-index called with an invalid buffer name ~S" buffer-name)
        
      (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
        (act-r-buffer-index buffer)))))


(defun buffer-mask (buffer-name)
  (let ((buffer (buffer-instance buffer-name)))
    (if (null buffer)
        (print-warning "buffer-mask called with an invalid buffer name ~S" buffer-name)
        
      (bt:with-recursive-lock-held ((act-r-buffer-lock buffer))
        (act-r-buffer-mask buffer)))))


;;; Useful as a parameter test

(defun list-of-buffers-p (x)
  (or (null x) 
      (and (listp x) 
           (let ((b (buffers))) 
             (every (lambda (y) (find y b)) x)))))

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
