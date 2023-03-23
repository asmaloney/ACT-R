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
;;; Filename    : declarative-memory.lisp
;;; Version     : 7.0
;;; 
;;; Description : Implements the declarative memory module.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [x] Better storage of activation quantities (computed act.,
;;;             :     base-level, and Sji's) for reference since right now
;;;             :     it's all "on the fly" and only available in the trace.
;;;             : [ ] Consider a hook before the module merges cleared buffer
;;;             :     chunks into DM so that someone could have a chance to
;;;             :     touch them first, or maybe prioritize the notify fn's
;;;             :     so that people could hook into things better there.
;;;             : [ ] Consider allowing :recently-retrieved to be partial
;;;             :     matched.  Possibly a separate parameter which specifies
;;;             :     the penalty value.
;;; 
;;; ----- History -----
;;;
;;; 2004.10.15 Dan
;;;             : Creation.
;;; 2004.12.06 Dan
;;;             : Fixed some issues with the hook fns when reset.
;;;             : Brought the line length down to 80.
;;; 2004.12.18 Dan
;;;             : Added the tracing info.
;;; 2005.01.09 Dan
;;;             : Playing around with some optimization ideas for merging
;;;             : since that really kills models with lots of buffer action...
;;;             :
;;;             : The first thought is to hash the chunk contents - the down
;;;             : side is that any mod-chunk to a chunk already in DM is going
;;;             : to cause problems with that.
;;;             : 
;;;             : Add the chunk-hash-table to dm structure.
;;;             : Modify the add-chunk-into-dm and merge-chunk-into-dm
;;;             : to take advantage of that.
;;;             :
;;;             : Seems to work and cuts zbrodoff time almost in half.
;;;             :
;;;             : So, adding it as an option with a new parameter for those
;;;             : that still want to mod-chunk DM.
;;; 2005.01.12 Dan
;;;             : * Updated version to 1.0a2
;;;             : * Shifted the commands to a dm-commands file in tools leaving
;;;             :   this file as solely responsible for the module interface.
;;; 2005.01.17 Dan
;;;             : * Removed calls to format in the scheduling functions.
;;; 2005.01.18 Dan
;;;             : * Added the ability to reset the dm finsts by passing reset
;;;             :   as the value of the :recently-retrieved request parameter.
;;; 2005.01.21 Dan
;;;             : * Did some minor optimizing by having DM organize its chunks
;;;             :   by chunk-type.  That seemed to be the bottle neck in most
;;;             :   of the tutorial models, so trying to speed that up some. 
;;;             : * Don't worry about setting fan unless spreading activation
;;;             :   is enabled.
;;; 2005.01.26 Dan
;;;             : * Changed chunk-base-level usage so that it gets set every
;;;             :   time a base-level is computed regardless of circumstances.
;;;             : * Added the spource-spread parameter to chunks to hold the
;;;             :   activation spread component of the computation.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium" and ":output 'low" to some of the 
;;;             :   events that are scheduled to play friendly with the 
;;;             :   new detail level.
;;; 2005.02.04 Dan
;;;             : * Taking advantage of the fast-* chunk accessors.
;;; 2005.02.10 Dan
;;;             : * Switched to the use of expt-coerced, exp-coerced, and 
;;;             :   log-coerced.
;;; 2005.04.01 Dan
;;;             : * Changed the fast-mergeing mechanism to work over chunks
;;;             :   that have merged (but unchanged) chunks in their slots
;;;             :   by using true-chunk-name hash-chunk-contents.
;;; 2005.04.14 Dan
;;;             : * Fixed the parameter setting for the hook functions so that
;;;             :   a value of nil clears it completely.
;;; 2005.04.23 Dan
;;;             : * Added recently-retrieved as a possible query and added
;;;             :   a buffer-status function to show it.
;;;             : * However, the query may not be what people want - the
;;;             :   query returns whether the chunk in the buffer has a
;;;             :   declarative-finst on it at the time of the query which 
;;;             :   may not be the same as when the request was made, but
;;;             :   this seems like the "right" thing to return.
;;; 2005.06.10 Dan
;;;             : * Fixed a bug - terminating an ongoing retrieval didn't
;;;             :   properly clear the events if they had already been 
;;;             :   scheduled to occur.
;;; 2005.06.14 Dan
;;;             : * Fixed an issue with start-retrieval related to :recently-
;;;             :   retrieved reset because what happened is that it cleared
;;;             :   the finsts, but then wanted to find one that had a finst
;;;             :   that matched "reset" which always resulted in failure.
;;; 2005.06.16 Dan
;;;             : * Changed the reference count to only store as many as
;;;             :   are needed...
;;; 2005.06.28 Dan
;;;             : * Removed the :sa parameter and instead changed :mas to
;;;             :   act as both the switch and the value.
;;; 2005.07.29 Dan
;;;             : * Fixed base-level-activation so that if it tries to compute
;;;             :   the activation for a chunk with no references (not yet in
;;;             :   DM) for some reason it prints a warning and returns a very
;;;             :   large negative value.
;;; 2005.08.01 Dan
;;;             : * Fixed a bug that resulted in BLC being added repeatedly
;;;             :   to the activation value.
;;;             :   Side effect is that right now sdp doesn't show the base-
;;;             :   level value (other than the blc) when bll is on.
;;;             :   See the Todo above...
;;;             : * Updated the version to 1.0.
;;; 2005.08.02 Dan
;;;             : * Modified the priority of start-retrieval so that it doesn't
;;;             :   start until the goal buffer has been set.
;;; 2006.06.01 Dan
;;;             : * Added a note to the to do list as a reminder of some thoughts
;;;             :   on an issue that someone might want/need in the future 
;;;             :   (dealing with chunks before they merge into DM) based on 
;;;             :   Mike's intrest in it now.
;;; 2006.07.10 Dan
;;;             : * Changed a call to true-chunk-name to true-chunk-name-fct in
;;;             :   hash-chunk-contents because true-chunk-name is now the macro.
;;; 2006.07.12 Dan
;;;             : * Actually removed the :sa parameter to get rid of the warning.
;;; 2006.08.08 Dan
;;;             : * Fixed spreading-activation so that it doesn't print a warning
;;;             :   when a buffer that should spread activation is empty.
;;; 2006.08.30 Dan
;;;             : * Fixed a bug when partial matching is on - it wasn't taking
;;;             :   the comparison tests (>, <, >=, and <=) into account for a
;;;             :   retrieval request!
;;;             : * Also removed some of the comments that were no longer valid
;;;             :   which were there from the "sample" module details in the
;;;             :   old framework spec.
;;;             : * Reordered how the activation trace prints for partial
;;;             :   matching so things are a bit cleaner when there's a hook.
;;; 2006.09.08 Dan
;;;             : * Changed some parameter checks from posnum to nonneg and
;;;             :   updated the warnings appropriately.
;;; 2006.11.28 Dan
;;;             : * Took an unnecessary get-module out of the query function.
;;; 2006.11.29 Dan
;;;             : * The :pm parameter is now depricated - use :mp as both the
;;;             :   flag and value like :bll and :mas.
;;; 2006.11.30 Dan
;;;             : * Removed the fan parameter from chunks since it wasn't used
;;;             :   for anything now (the fan-list is what's important).
;;;             : * Updated chunks-similarity to use chunk-slot-equal instead
;;;             :   of equal for comparing non-chunk values.
;;; 2006.12.01 Dan
;;;             : * Cleaned up some comments in/around the base-level calculation.
;;; 2006.12.04 Dan
;;;             : * Added the last-base-level chunk parameter and changed the default
;;;             :   for the base-level chunk parameter to nil.
;;;             : * Modified the base-level calculation to set last-base-level and
;;;             :   so that the user setting overrides the :blc when :bll is nil
;;;             :   instead of being addative (makes it like ACT-R 4/5 now).
;;; 2006.12.05 Dan
;;;             : * Minor formatting changes - no real change.
;;; 2006.12.06 Dan
;;;             : * Added the retrieval-activation and retrieval-time 
;;;             :   parameters to the chunk to record the activation value
;;;             :   that it had during a retrieval request and the time at which
;;;             :   that request occured.
;;;             : * Updated the version to 1.1.
;;; 2007.08.15 Dan
;;;             : * Changed start-retrieval so that the chunk which will be
;;;             :   retrieved is the first element in the list passed to the
;;;             :   retrieval-set-hook function.
;;; 2008.02.27 Dan
;;;             : * Changed the parameter check to only throw the warning
;;;             :   when the "critical" parameters are adjusted.
;;; 2008.07.12 Dan 
;;;             : * Changed the text printed for the activation trace during
;;;             :   similarity calculations to avoid the potential confusion
;;;             :   that the similarity values are actually changing.
;;; 2008.07.12 Dan
;;;             : * Added the :w-hook parameter to allow one to differentially
;;;             :   specify the Wkj values.
;;; 2008.07.23 Dan
;;;             : * Added call to the new register-subsymbolic-parameters to
;;;             :   note which declarative parameters should trigger the warning.
;;; 2008.08.14 Dan
;;;             : * Fixed a bug with the retrieval-set-hook - when there was a
;;;             :   retrieval failure it got called with (nil) instead of just
;;;             :   nil.
;;; 2008.11.13 Dan
;;;             : * Added a secondary reset function to set the :dcsc-hook
;;;             :   parameter so that fast-mergeing works right in the context
;;;             :   of chunk normalizing i.e. the module can rehash the chunks
;;;             :   that change.
;;; 2008.12.10 Dan
;;;             : * Changed how the fan values are recorded to improve both
;;;             :   space and time.  Instead of a list on the j's now it's 
;;;             :   split into a fan-out and a fan-in.  The fan-out is just a
;;;             :   count of the total fan 'out' of the chunk (the length of
;;;             :   the old fan list) and the fan-in is the list of the chunk's
;;;             :   slot contents at the time it entered DM with thier counts.
;;;             :   So, now it doesn't have to search or count item on the fan-list
;;;             :   to do the calculation.  It instead searchs the fan-in list
;;;             :   which presumably is shorter than the old fan-list for most
;;;             :   chunks in most models and even if its not it doesn't have
;;;             :   to do the counting every time since that was set initially.
;;;             : * It also now addresses the issue of chunks which merge into DM
;;;             :   after they have had references credited to them.  Previously,
;;;             :   those other references were lost if the chunk actually merged
;;;             :   with an existing chunk in DM, but now they get tracked 
;;;             :   appropriately.
;;; 2008.12.11 Dan
;;;             : * Fixed a bug with the last change - a chunk didn't get saved
;;;             :   on its own fan-in list.
;;;             : * There also isn't likely any space savings in general - only
;;;             :   when there's duplicate instances of a value in slots (a 
;;;             :   fan-in other than 1).
;;; 2009.08.05 Dan
;;;             : * Added a new hook for the activation calculations.  The
;;;             :   :activation-offsets hook holds a list of functions to call
;;;             :   during activation computation.  A function on the hook will 
;;;             :   be called with the chunk name during the activation calculation
;;;             :   and if the function returns a number that value will be
;;;             :   added to the activation value for the chunk.  
;;;             :   This hook will make it easier to extend the activation
;;;             :   calculation without having to completely redefine one of the
;;;             :   existing components via the other hooks.
;;; 2009.08.13 Dan
;;;             : * Added an optional parameter to chunks-similarity to control
;;;             :   whether or not the activation trace info is displayed since
;;;             :   it can also be called by the user in which case the trace
;;;             :   is undesirelable (and since it's model output can't be shut
;;;             :   off with no-output).
;;; 2009.09.09 Dan
;;;             : * With the addition of the multi-buffers and buffer sets DM
;;;             :   needs to flag any chunk it has as invalid for use in such a
;;;             :   set to avoid unintentional modification of DM chunks.
;;; 2010.08.30 Dan
;;;             : * Extending the :act parameter to take a level like trace-detail
;;;             :   so one can get a smaller set of info when desired.  High is
;;;             :   the same as t and nil is still off.  Medium turns off all of
;;;             :   the "doesn't match" indicators but otherwise shows everything
;;;             :   and low only shows the total activation.
;;; 2011.02.09 Dan
;;;             : * Added a merge-dm command which works like add-dm except that
;;;             :   the chunks are merged in as if cleared from a buffer at the
;;;             :   current time.  It merges them in an order that guarantees 
;;;             :   any chunks used in slots of other chunks get merged first,
;;;             :   but if there are circularities it just does them in the 
;;;             :   order provided.
;;; 2011.04.25 Dan
;;;             : * Added an ignore declaration to the secondary reset to
;;;             :   avoid a warning at load time.
;;;             : * Updated the time values to be milliseconds internally, but
;;;             :   still use seconds for 'user' interaction.
;;; 2011.04.26 Dan
;;;             : * Switched the finsts to use the ms time as well.
;;;             : * Added a check for negative time deltas when computing 
;;;             :   activations with :ol # because that could lead to complex
;;;             :   results.
;;; 2011.04.28 Dan
;;;             : * Suppress the warnings about redefining functions when 
;;;             :   adding the chunk parameters.
;;; 2011.06.22 Dan
;;;             : * Added the :sact parameter which allows the activation
;;;             :   trace info to be saved and all the necessary support to
;;;             :   actually save that info.
;;; 2011.06.23 Dan
;;;             : * Added dummy sact-trace structs before and after a retrieval
;;;             :   so other activation info doesn't overwrite the saved data.
;;;             : * Changed it so that it now stores arbitrary activation 
;;;             :   computations along with retrieval requests.
;;; 2011.07.17 Dan [1.2]
;;;             : * Added a crude retrieval buffer stuffing mechanism.
;;;             :   Setting the :declarative-stuffing parameter to a number
;;;             :   will enable the mechanism.  At time 0 and then when ever
;;;             :   the module is not busy and either:
;;;             :   - the retrieval buffer empties
;;;             :   - :declarative-stuffing seconds pass since a stuffed 
;;;             :     chunk was placed into the buffer
;;;             :   a new stuffing attempt will be performed.  
;;;             :   The stuffed chunk will be the chunk with the highest activation
;;;             :   among all chunks in DM. 
;;;             :   For now there is no attempt to be efficient about the 
;;;             :   process so it will recompute all the activations of all
;;;             :   chunks each time there is a stuffing attempt.  It will not
;;;             :   overwrite a requested chunk, nor will it ever terminate an
;;;             :   ongoing requested retrieval.
;;; 2011.09.12 Dan 
;;;             : * Took some unused variables out of check-declarative-stuffing
;;;             :   to eliminate some warnings.
;;; 2012.01.27 Dan
;;;             : * Changed the activation trace so that it also shows the non-
;;;             :   matching chunks under the high detail trace when partial
;;;             :   matching is enabled.
;;; 2012.02.03 Dan
;;;             : * Added the new command remove-old-dm-finsts because there are
;;;             :   a few places that happens and this is better than having the
;;;             :   code to do it there everytime.
;;; 2012.02.06 Dan
;;;             : * Added a dummy dm-current-sact-chunk at reset time because
;;;             :   things like set-base-levels force an activation calculation
;;;             :   indirectly at load time and it needs some place to 'save'
;;;             :   those values (though they're not really saved).
;;; 2012.02.21 Dan
;;;             : * Removed some debugging output from sort-for-binding-dm.
;;; 2012.04.19 Dan [1.3]
;;;             : * Added a new request parameter for retrievals :mp-value.
;;;             :   It can take the same values as the :mp parameter and if
;;;             :   provided will temporarily override the current :mp setting
;;;             :   for this retrieval request if :mp has already been enabled 
;;;             :   i.e. it can't temporarily turn on partial matching but can 
;;;             :   temporarily turn it off.
;;;             : * Changed it so that when an invalid request terminates a
;;;             :   retrieval the module goes back to free instead of staying
;;;             :   stuck at busy.
;;; 2012.08.06 Dan
;;;             : * Added a new parameter :nsji which controls whether or not
;;;             :   the Sji values are allowed to go negative from the S-log(fan)
;;;             :   calculation.  Possible values are t (meaning let them) or
;;;             :   nil which will prevent them and also display a model warning
;;;             :   when such a situation occurs.
;;; 2013.04.17 Dan
;;;             : * Added the last-request slot to the module and use that to
;;;             :   record the time and chunk-spec of the most recent request
;;;             :   to support the new whynot-dm command.
;;; 2013.05.21 Dan
;;;             : * Allow the :mp-value parameter to enable partial matching
;;;             :   when it is off and removed :mp from the parameters that warn
;;;             :   about being changed on the fly.
;;; 2013.09.05 Dan
;;;             : * Changed :nsji to take 3 possible values: t, nil, and warn.
;;;             :   T works as before to allow negative sji values, warn works
;;;             :   the way nil used to, cap values at 0 and print a warning,
;;;             :   and nil now caps the value at 0 and doesn't print the warning.
;;;             :   The new default is warn.
;;; 2013.10.09 Dan
;;;             : * Warn if :bll is set to >=1 if :ol is enabled (can't test 
;;;             :   for it in the valid function because that would require
;;;             :   calling sgp to test :ol which may or may not be set when 
;;;             :   a model is created since :bll could go before :ol).
;;; 2014.03.06 Dan [2.0]
;;;             : * Make the changes necessary to work with the now typeless
;;;             :   chunks.
;;; 2014.03.17 Dan 
;;;             : * Changed the query-buffer call to be consistent with the new
;;;             :   internal code.
;;; 2014.03.21 Dan
;;;             : * Use the common circular-references function from misc-utils.
;;; 2014.04.01 Dan
;;;             : * Used slots-vector-match-signature instead of doing the bit
;;;             :   tests directly.
;;; 2014.06.16 Dan
;;;             : * Changed how the initial chunk list in a request is created
;;;             :   so that it doesn't have to walk the list twice.
;;; 2014.08.28 Dan
;;;             : * Fixed a bug in start-retrieval when the request had modifiers
;;;             :   other than - and =.
;;; 2014.10.24 Dan
;;;             : * Removed the :pm parameter since it was depricated in 6.0.
;;; 2015.03.20 Dan
;;;             : * Failures now set the buffer failure flag using set-buffer-failure.
;;; 2015.06.04 Dan
;;;             : * Use safe-seconds->ms when setting :declarative-finst-span.
;;;             : * Use :time-in-ms t for all scheduled events.
;;; 2015.06.05 Dan
;;;             : * Compute-activation-latency now returns a time in ms and the
;;;             :   :lf parameter is stored as a ms value internally (it does
;;;             :   the scaling).
;;; 2015.06.08 Dan
;;;             : * When computing activations don't convert the references
;;;             :   to seconds, just scale it at the end using a value computed
;;;             :   when :bll gets set and stored in dm-act-scale.
;;;             : * When printing the activation trace print the times accurately
;;;             :   using print-time-in-seconds and store the ms values in the
;;;             :   saved structure.
;;;             : * The saved activation trace info is recorded at the ms time not
;;;             :   seconds.
;;; 2015.06.09 Dan
;;;             : * Record last-request time in ms.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2015.09.15 Dan [3.0]
;;;             : * Module now has a trackable buffer for use with the new 
;;;             :   utility learning mechanism and reports when requests are
;;;             :   finished with complete-request.
;;; 2016.06.30 Dan
;;;             : * Added an optional parameter to add-chunk-into-dm so that 
;;;             :   merge-chunk-into-dm can pass the key in since that can be
;;;             :   costly to create.
;;; 2016.09.01 Dan
;;;             : * The activation trace didn't actually print the chunk name when
;;;             :   the retrieval set hook forced an override.
;;; 2017.02.15 Dan [4.0]
;;;             : * Allow the sim-hook to be set to a string and use the dispatch-
;;;             :   apply function to call it so it can go out to an external
;;;             :   system if necessary.
;;;             : * Add a remote version of add-dm.
;;; 2017.02.17 Dan
;;;             : * Sim-hook test is now local-or-remote-function-or-nil.
;;; 2017.03.15 Dan [5.0]
;;;             : * Cache-sim-hook-results parameter added which adds a hash-table
;;;             :   for lookup of values returned by the sim-hook used before 
;;;             :   calling the hook-fn when enabled.  Helps a lot when the 
;;;             :   function returns consistent values and gets called through
;;;             :   the dispatcher.
;;; 2017.08.03 Dan [5.1]
;;;             : * Adding locks to make it all thread safe with the direct
;;;             :   approach of protecting the chunk table and the parameters.
;;;             :   May need to reconsider that based on performance.
;;; 2017.08.08 Dan
;;;             : * Also need a lock for the 'state' of the module which covers
;;;             :   the slots not related to chunks or params.  This should 
;;;             :   finish the thread safety.
;;; 2017.08.09 Dan
;;;             : * Update the buffer-status function to return the string.
;;; 2017.09.01 Dan
;;;             : * Added all-dm-chunk-types function for use by the environment.
;;; 2017.09.22 Dan
;;;             : * Fixed some bugs in the code that saves the activation trace.
;;;             : * Fixed a bug with parameter for the retrieval-set-hook fns.
;;; 2017.11.07 Dan
;;;             : * Changed the warning for :sim-hook to specify the string needs
;;;             :   to name a command.
;;; 2017.12.06 Dan
;;;             : * Changed the doc strings for the remote add-dm commands.
;;; 2018.01.26 Dan
;;;             : * Changed the buffer-status function to omit the extra newline.
;;; 2018.04.17 Dan [6.0]
;;;             : * Allow all the hook parameters to be dispatched commands.
;;; 2018.04.18 Dan
;;;             : * Convert the chunk-specs to ids for the hooks that use them.
;;;             : * Sjis are now stored as 2 element lists instead of conses.
;;;             : * Similarities are now also lists instead of conses.
;;; 2018.04.25 Dan
;;;             : * Added remote merge-dm.
;;; 2018.06.12 Dan
;;;             : * Have the external add/merge -dm commands do the string
;;;             :   processing since define-chunks won't.
;;; 2018.06.27 Dan [6.1]
;;;             : * Fix a bug that's been around for a long time with merging
;;;             :   into DM being case sensitive for strings whereas all other
;;;             :   chunk matching is case insensitive (including the general
;;;             :   merging action).
;;; 2018.07.26 Dan [6.2]
;;;             : * Removing the request completion support.
;;; 2018.08.24 Dan [6.3]
;;;             : * Added the add-dm-chunks{-fct} and merge-... commands which
;;;             :   take chunk names for adding or merging instead of definitions.
;;; 2019.01.25 Dan
;;;             : * Move the (setf (chunk-source-spread chunk) ...) inside of
;;;             :   the if since it defaults to 0 thus no need to keep setting
;;;             :   it there if spreading-activation is off and that's a parameter
;;;             :   which shouldn't change during a run.
;;; 2019.04.02 Dan
;;;             : * Have hash-chunk-contents use the underlying chunk info 
;;;             :   instead of going through the accessors.
;;; 2019.04.04 Dan
;;;             : * Cache the result of chunk-spec-slots in the retrieval
;;;             :   request since that's costly don't want to do it twice.
;;; 2019.04.15 Dan
;;;             : * Depend on splice-into-position-des being destructive and 
;;;             :   don't setf with the returned value.
;;; 2019.05.17 Dan [6.4]
;;;             : * Allow the sim-hook cache to persist across a reset.  Only
;;;             :   clear the table when a new hook function is set.
;;; 2019.08.15 Dan 
;;;             : * Fixed a bug with recording the matching chunks for the 
;;;             :   retrieval-set-hook and whynot-dm if :esc is nil.
;;; 2019.10.22 Dan
;;;             : * Merge-reference-list was using the chunk-lock instead
;;;             :   of the param-lock to protect access to the dm-ol value.
;;; 2020.01.10 Dan [6.5]
;;;             : * Removed the #' and lambdas from the module interface 
;;;             :   functions since that's not allowed in the general system
;;;             :   now.
;;;             : * Use set-or-remove-hook-parameter for all the hook parameters.
;;; 2020.02.12 Dan
;;;             : * Add tests to the parameter functions for reference-list and
;;;             :   permanent-noise so that they verify that the declarative
;;;             :   module is the one expected.
;;; 2020.04.21 Dan [6.6]
;;;             : * Added the :ignore-buffers parameter which allows one to
;;;             :   specify buffers whose contents doesn't need to be stored in
;;;             :   DM for efficiency.
;;; 2020.08.26 Dan
;;;             : * Removed the path for require-compiled since it's not needed
;;;             :   and results in warnings in SBCL.
;;; 2020.09.04 Dan
;;;             : * Use the merge-value-function for fan-out, reference-list,
;;;             :   and reference-count to avoid needing to get the values (also
;;;             :   makes the reference-* fns useable for other parameters).
;;; 2020.10.26 Dan [6.7]
;;;             : * Added an :rt-value request parameter that can be used to set
;;;             :   a new :rt value to use during this retrieval, just like the
;;;             :   :mp-value changes the :mp parameter.
;;; 2021.03.10 Dan [6.8]
;;;             : * Set the :do-not-query parameter for goal buffer now.
;;; 2021.06.04 Dan [7.0]
;;;             : * When merging chunks from a buffer need to check if it's
;;;             :   storable or not if it's being added as a new chunk.
;;; 2021.10.18 Dan
;;;             : * Changed call to buffers to model-buffers instead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The declarative memory module has one buffer called retrieval.
;;;
;;; The declarative memory module collects the chunks that have been cleared
;;; from all buffers.  It merges newly cleared chunks with those that have been
;;; previously cleared.  This set of chunks is refered to as the declarative
;;; memory (DM for short).  Requests to the declarative module are attempts to
;;; find a chunk in DM which matches the request.  If such a chunk is found
;;; it is placed into the retrieval buffer.  If no such chunk is found, then
;;; it reports an error state.  It can only process one request at a time.  If
;;; a new request comes in prior to the completion of a previous request the
;;; older request is terminated immediately. The timing of a request's
;;; completion along with how the matching chunk is found are controled by
;;; several parameters and the following equations:
;;;
;;;
;;; In addition, to that, there are three request parameters which may be used -
;;; :recently-retrieved, :mp-value, and :rt-value.  
;;; :recently-retrieved  may be passed a value of t or nil.  The declarative
;;; memory module records which chunks it has returned as the result of a
;;; request and the recently-retrieved request parameter may be used to exclude
;;; chunks based on that information.  There are two parameters that control
;;; how the recently-retrieved designation occurs.  The :dm-finsts parameter
;;; indicates how many chunks will be marked as recently-retrieved and the
;;; :dm-finsts-decay parameter indicates for how many seconds each of those
;;; designations will persist.
;;; :mp-value can be used to temporarily change the setting of the :mp parameter
;;; while the request is processed.  :mp-value can be used whether the :mp 
;;; parameter has been enabled for the model or not i.e. it can temporarily
;;; enable partial matching even if it is off.
;;; :rt-value can be used to temporarily change the setting of the :rt parameter
;;; while the request is processed.
;;;
;;; The declarative memory module does not support buffer modification requests.
;;;
;;; The declarative memory module responds to the required queries as follows:
;;;
;;; State free will respond t if there is no request pending or nil if there is
;;;       i.e. the module is not free between the time of a request and when
;;;       the chunk from that request is placed into the buffer.
;;; State busy will respond t if there is a pending request and nil if not.
;;; State error will respond with t if no chunk matching the most recent request 
;;;       could be found or nil otherwise.  The error t will not be indicated 
;;;       until after the time for failure has passed.
;;; Buffer requested will be t if the chunk in the buffer is the result of a request 
;;;       and nil if it is the result of the buffer stuffing mechanism or the 
;;;       buffer is empty.
;;; Buffer unrequested will be t if the chunk in the buffer was put there by the
;;;       buffer stuffing mechanims and it will be nil if it is the result of a
;;;       retrieval request or the buffer is empty.
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
;;; One thing that is going in from the beginning is lots of hooks into the
;;; equations.  Every component of the activation equation will have an
;;; "over-ride" function basically like the similarity-hook-fn of the 
;;; older system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Relies on :esc, :ol, and :er so make sure they exist first

(require-compiled "CENTRAL-PARAMETERS")

;;; structure to hold the details for the last request

(defstruct last-request time spec finst finst-chunks best matches rt invalid)


;;; Start by defining a structure to hold the instance of the module

(defstruct dm "an instance of the declarative memory module"
  chunks  ; the alist of chunks that are in declarative memory
  
  (busy nil)    ; record whether the module is busy
  (failed nil)  ; record whether the last request failed
  
  ;;; keep track of the central parameters internally for ease of access
  
  esc er ol
  
  ;;; slots for the various parameters from previous versions
  ;;; that control the subsymbolc
  
  blc ans pas lf le mp ms md rt bll mas 
  
  ;;; add a new parameter to act as a switch for spreading
  ;;; since relying on ga to be zero won't work
  
  sa
    
  ;;; only one of the old traces still really matters
  ;;; or in some cases is even possible - basically show all
  ;;; components that are enabled with this switch
  
  act
  
  ;;; parameters for the declarative finsts count and duration 
  
  num-finsts finst-span
  
  ;;; a list to hold the finsts
  
  finsts
  
  ;;; replace the global hook functions with parameters
  ;;; 
  ;;; Start with the "low-level" hooks that over-ride
  ;;; internal values
  
  sim-hook ;; the old similarity hook 
           ;; called with the two values and returns similarity or nil
  sji-hook ;; same thing now for Sji values - passed the two chunk names 
  
  w-hook   ;; Allows one to override the Wkj values in spreading activation.
  
  ;;; Higher level hooks over-ride the entire internal computiation for each component 
  ;;; the chunk name is what gets passed to all
  
  bl-hook         ;; let the user redefine the base-level computation
  spreading-hook  ;; let the user redefine the spreading component
  partial-matching-hook ;; redefinition of the partial matching component
  noise-hook      ;; redefine the transient noise computation
  
  offsets         ;; adds optional additional components to the
                  ;; activation equation
  
  ;;; some retrieval hooks like the conflict resolution
  ;;; system has.  Could get some of this from the main
  ;;; event hooks, but seems cleaner to just hook where
  ;;; one wants when possible.  All of these parameters
  ;;; have a list of functions internally which get called.
  ;;; Each setting is pushed onto that list.  No way to remove
  ;;; one other than through a reset.  This is so something
  ;;; like the environment can add such a hook without the
  ;;; user being able to "break" things.
  
  retrieval-request-hook ;; called at the initation of the request with the spec
  
  retrieval-set-hook     ;; called with the chunks that matched the spec. The
                         ;; activations have already been computed but a non-nil
                         ;; return overrides that - like the conflict-set hook.
                         ;; If the return value is a cons of chunk-name and time
                         ;; that is used instead of the computation and if it is
                         ;; just a number, then a failure is scheduled with that
                         ;; latency.
                         ;; If more than one returns a non-nil a warning is
                         ;; signaled and none of those effects occur.
  
  retrieved-chunk-hook   ;; called with the retrieved chunk or nil on failure.
                         ;; The call occurs at the time of the actual retrieval
                         ;; but before the buffer setting - the general event 
                         ;; hook should be used to detect that.
  
  
  ;;; a couple of hooks to support user extensions
  
  chunk-merge-hook  ;; called after a chunk has been merged into dm
  chunk-add-hook    ;; called after a chunk has been newly added to dm
  
  
  ;;; A hash-table of chunks in DM referenced by contents
  ;;; to speed the merging calculation
  
  chunk-hash-table
  
  ;;; Saving the activation trace details for later recreation purposes
  
  sact ; the switch
  trace-table ; stored results
  current-trace ; build the info as it goes
  current-sact-chunk 
  stuff stuff-event
  
  nsji ;; whether the S-log(fan) is allowed to return negatives
  
  ;; save the last request and time for use with the whynot-dm command
  last-request
  
  ;; the value (log (expt 1/1000 (- :bll))) used to scale the 
  ;; activation values into seconds
  act-scale
  
  
  ;; Specify buffers whose clearing you don't want stored in DM
  
  ignore-buffers
  
  ;; Before calling the sim-hook check for the values in a hash-table
  ;; and if it is called then store the returned result.
  cache-sim-hook-results
  (sim-hook-cache (make-hash-table :test 'equalp))
  
  last-sim-hook
  
  (chunk-lock (bt:make-lock "dm-chunks")) ;; chunks and chunk-hash-table
  (param-lock (bt:make-lock "dm-params")) ;; things through sgp
  (state-lock (bt:make-lock "dm-state"))  ;; other slots
  )



;;; 2 structures for saving the activation trace details
;;; This one holds the general info.
(defstruct sact-trace
  only-recent remove-recent recents matches no-matches chunks esc result-type result)

;;; This one holds the info for a single chunk.

(defstruct sact-chunk
  bl-style bl-result name total offsets blc base-level zero-ref bl-count bl-refs bl-ct decay ol
  sa sa-value sa-buffers
  pm pm-value pm-tests
  noise noise-p noise-t)


;;; Add the necessary parameters to the chunk definitions

;;; Set to the computed value when necessary

(suppress-extension-warnings)
 
(extend-chunks activation :default-value 0)

;;; Record the chunks in which a chunk occurs

;;; Splitting this into a fan-in and a fan-out now
;;; to potentially save time and space in the computation
;;; and storage.
;;; Also fixes the 'bug' that the older version ignored -
;;; chunk merging of sources after a chunk which uses
;;; the source has also merged.

(extend-chunks in-dm :default-value nil)

(defun merge-fan-outs (c1 c2)
  (+ c1 c2))


(extend-chunks fan-out :default-value 0 :merge-value-function merge-fan-outs)

(extend-chunks c-fan-out :default-value 0 :copy-from-chunk-function chunk-fan-out)

(extend-chunks fan-in :default-value nil)


;;; Not really going to be "creation" time but entry to DM time
;;; which is what will be used in the computations.

(extend-chunks creation-time :default-value 0)


;;; This holds the user set base-level which is only meaningful when
;;; :bll is nil.

(extend-chunks base-level :default-value nil :copy-function identity)

;;; This holds the last computed base-level value

(extend-chunks last-base-level :default-value 0 :copy-function identity)


;;; holds the computed spreading activation component of the chunk

(extend-chunks source-spread :default-value 0 :copy-function identity)

;;; Merging results in one new reference for the "existing" chunk

(defun merge-reference-list (chunk1 chunk2)
  (declare (ignore chunk2))
  (let* ((dm (get-module declarative))
         (ol (and (dm-p dm) (bt:with-lock-held ((dm-param-lock dm)) (dm-ol dm)))))
    (cond ((null ol)
           (cons (mp-time-ms) chunk1))
          ((eq ol t)
           nil)
          (t ;; ol is a number
           (subseq (cons (mp-time-ms) chunk1)
                   0 (min ol (1+ (length chunk1))))))))

(defun merge-reference-count (chunk1 chunk2)
  (declare (ignore chunk2))
  (+ 1 chunk1))

(extend-chunks reference-list :default-value nil :copy-function copy-list :merge-value-function merge-reference-list)

(extend-chunks reference-count :default-value 0 :copy-function identity :merge-value-function merge-reference-count)

;;; Keep the similarities with the chunks
;;; at least for now.

(extend-chunks similarities :default-value nil :copy-function copy-tree)
 
;;; compute the permanent noise as needed

(defun default-permanent-noise (chunk)
  (declare (ignore chunk))
  
  (let* ((dm (get-module declarative))
         (pas (and (dm-p dm) (bt:with-lock-held ((dm-param-lock dm)) (dm-pas dm)))))
    (if pas
        (act-r-noise pas)
      0.0)))

(extend-chunks permanent-noise :default-function default-permanent-noise :copy-function identity)


;;; store user define Sji values with the chunk

(extend-chunks sjis :default-value nil :copy-function copy-tree)


;;; store the last activation used in a retrieval request and
;;; the time that request occured

(extend-chunks retrieval-activation :default-value nil)
(extend-chunks retrieval-time :default-value nil)


;;; fast-merge assumption violated by :dcnn so need
;;; to be able to rehash when that happens

(extend-chunks fast-merge-key)

(unsuppress-extension-warnings)

(defun dm-fm-rh (chunk)
  (awhen (chunk-fast-merge-key chunk)
         (let ((dm (get-module declarative))
               (new-key (hash-chunk-contents chunk)))
           (bt:with-lock-held ((dm-chunk-lock dm))
             (remhash it (dm-chunk-hash-table dm))
             (setf (chunk-fast-merge-key chunk) new-key)
             (setf (gethash new-key (dm-chunk-hash-table dm)) chunk)))))

;;; A function for converting a chunk to a list of its info

(defun hash-chunk-contents (chunk)
 (let ((c (get-chunk chunk)))
    (when c
      (bt:with-recursive-lock-held ((act-r-chunk-lock c))
        (cons (act-r-chunk-filled-slots c)
              (mapcar (lambda (x)
                        (let ((val (cdr x)))
                          (if (stringp val) 
                              (string-upcase val)
                            (true-chunk-name-fct val))))
                (sort (copy-list (act-r-chunk-slot-value-lists c))
                      #'< :key (lambda (x) (act-r-slot-index (car x))))))))))
  

(defun reset-dm-module (dm)
  
  ;; Set all of the slots of this instance to their initial values.
  
  (bt:with-lock-held ((dm-chunk-lock dm))
    (setf (dm-chunks dm) nil)
    (setf (dm-chunk-hash-table dm) 
      (make-hash-table :test #'equal)))
  
  (bt:with-lock-held ((dm-state-lock dm))
    (setf (dm-busy dm) nil)
    (setf (dm-failed dm) nil)
  
    (setf (dm-finsts dm) nil)
    
    (setf (dm-last-request dm) nil)
  
    (setf (dm-trace-table dm)
      (make-hash-table :test #'equal))
  
    ;; set a dummy sact marker so that activation calculations
    ;; which occur before the first retrieval have somewhere to
    ;; write their values.  Could just use that as the first one,
    ;; but for now just letting this one go away.
    
    (setf (dm-current-sact-chunk dm) (make-sact-chunk :name nil))
    
    (setf (dm-current-trace dm) nil))
  
  #| Don't do that at reset
     only when a new hook function gets
     set so that results persist across a reset for
     a given function.
   (bt:with-lock-held ((dm-param-lock dm))
    ;; clear the sim-hook cache
    (clrhash (dm-sim-hook-cache dm)))
   |#
  )

(defun secondary-reset-dm-module (dm)
  (declare (ignore dm))
  
  (sgp :dcsc-hook dm-fm-rh :do-not-query retrieval))


(defun tertiary-reset-dm-module (dm)
  (when (dm-stuff dm)
    (schedule-event-now 'check-declarative-stuffing :module 'declarative :destination 'declarative :output 'low :maintenance t)))

(defun remove-old-dm-finsts (dm)
  (let ((span (bt:with-lock-held ((dm-param-lock dm)) (dm-finst-span dm)))
        (ct (mp-time-ms)))
    
    (bt:with-lock-held ((dm-state-lock dm))
      
      (setf (dm-finsts dm)
        (remove-if (lambda (time)
                     (> (- ct time) span))
                   (dm-finsts dm) :key #'cdr)))))


(defun dm-query-request (dm buffer slot value)
  (case slot
    (state
     (bt:with-lock-held ((dm-state-lock dm))
       (case value
         (busy
          (dm-busy dm))
         (free
          (not (dm-busy dm)))
         (error
             (dm-failed dm))
         (t (print-warning  
             "Invalid query made of the ~S buffer with slot ~S and value ~S" 
             buffer slot value)))))
    (recently-retrieved
     (remove-old-dm-finsts dm)
     
     (let ((c (buffer-read buffer)))
       (and c
            (chunk-copied-from-fct c)
            (if value 
                (find (chunk-copied-from-fct c)
                      (bt:with-lock-held ((dm-state-lock dm)) (dm-finsts dm))
                      :key 'car)
              (not (find (chunk-copied-from-fct c)
                         (bt:with-lock-held ((dm-state-lock dm)) (dm-finsts dm))
                         :key 'car))))))))


(defun dm-request (dm buffer request)
  (declare (ignore buffer)) ;; It is always going to be retrieval
  
  ;; just lock out all the state changes in setting up the request
  
  (bt:with-lock-held ((dm-state-lock dm))
  
 
    ;; Save the current request info (don't store the rt now to avoid having to grab the param lock)
    
    ;; If the module has not completed the last request
 
    (when (dm-busy dm)
    
      ;; Report a warning about that and remove the unexecuted event 
      ;; from the queue.
      
      (model-warning "A retrieval event has been aborted by a new request")
      (delete-event (dm-busy dm))
      
      )
    
  
    ;; Clear the failed attempt flag of the module
  
    (setf (dm-failed dm) nil)
  
    ;; Schedule an event to start the retrieval at the current time
    ;; but with a priority of -2000 and save that as the busy flag
    ;; instead of immediately attempting the retrieval to make sure
    ;; buffers have settled if called from a production to get 
    ;; proper sources of activation.
    
    (setf (dm-busy dm)
      (schedule-event-now 'start-retrieval
                          :module 'declarative
                          :destination 'declarative
                          :details "start-retrieval"
                          :priority -2000
                          :params (list request)
                          :output 'medium))))


(defun dm-act-level (act level)
  (and act
       (case level
         (high (or (eq act 'high) (eq act t)))
         (medium (not (eq act 'low)))
         (low t))))
                  
;;; Start-retrieval
;;;
;;; This function is called to actually attempt a retrieval.
;;;
;;; The parameters it receives are an instance of the module and the 
;;; chunk-spec of the request.
;;;
;;; It either schedules the setting of the retrieval buffer or indication of
;;; an error depending on whether or not it finds a chunk that matches the
;;; request.
;;;
;;; There are several parameters that determine how the "best" matching chunk
;;; is selected and how long that action will take.


(defun start-retrieval (dm request)
  (let (esc rt rrh sact act er set-hook
            mp offsets blc bll ol act-scale
            sa spreading-hook w-hook sji-hook mas nsji
            pm-hook ms md sim-hook cache-sim-hook
            noise-hook ans lf le bl-hook)
  
    
    (bt:with-lock-held ((dm-state-lock dm))
      (when (dm-stuff-event dm)
        (delete-event (dm-stuff-event dm))
        (setf (dm-stuff-event dm) nil)))
    
    
    (bt:with-lock-held ((dm-param-lock dm))
      (setf
       act (dm-act dm)
       rt (dm-rt dm)
       sact (dm-sact dm)
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
       rrh (dm-retrieval-request-hook dm)
       er (dm-er dm)
       set-hook (dm-retrieval-set-hook dm)
       lf (dm-lf dm)
       le (dm-le dm)
       bl-hook (dm-bl-hook dm)))
    
    
    (when rrh
      (let ((id (chunk-spec-to-id request)))
        (dolist (x rrh)
          (dispatch-apply x id))
        (release-chunk-spec-id id)))
    
    (when sact
      (bt:with-lock-held ((dm-state-lock dm))
        (setf (dm-current-trace dm) (make-sact-trace :esc esc))
        (setf (gethash (mp-time-ms) (dm-trace-table dm)) (dm-current-trace dm))))
  
    (let* ((last-request (make-last-request :time (mp-time-ms) :spec request :rt rt))
           (filled (chunk-spec-filled-slots request))
           (empty (chunk-spec-empty-slots request))
           (chunk-list (mapcan (lambda (x) 
                                 (if (slots-vector-match-signature (car x) filled empty)
                                     (copy-list (cdr x))
                                   nil))
                         (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunks dm))))
           )
    
    (flet ((invalid (reason warnings)
                      (bt:with-lock-held ((dm-state-lock dm))
                        (setf (dm-busy dm) nil)
                        (setf (last-request-invalid last-request) reason)
                        (setf (dm-last-request dm) last-request)
                        )
                      (dolist (x warnings)
                        (print-warning x))
                      (return-from start-retrieval)))
      
      (let ((requested-slots (chunk-spec-slots request)))
        (when (member :recently-retrieved requested-slots)
          (let ((recent (chunk-spec-slot-spec request :recently-retrieved)))
            (cond ((> (length recent) 1)
                   (invalid :too-many '("Invalid retrieval request." ":recently-retrieved parameter used more than once.")))
                  ((not (or (eq '- (caar recent)) (eq '= (caar recent))))
                   (invalid :bad-modifier '("Invalid retrieval request." ":recently-retrieved parameter's modifier can only be = or -.")))
                  ((not (or (eq t (third (car recent)))
                            (eq nil (third (car recent)))
                            (and (eq 'reset (third (car recent)))
                                 (eq '= (caar recent)))))
                   (invalid :bad-value '("Invalid retrieval request." ":recently-retrieved parameter's value can only be t, nil, or reset.")))
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
                              
                              ;; save that info for whynot
                              (setf (last-request-finst last-request) :marked)
                              (setf (last-request-finst-chunks last-request) chunk-list)
                              
                              (when sact
                                (bt:with-lock-held ((dm-state-lock dm))
                                  (setf (sact-trace-only-recent (dm-current-trace dm)) t)
                                  (setf (sact-trace-recents (dm-current-trace dm)) chunk-list)))
                              
                              (when (dm-act-level act 'high)
                                (model-output "Only recently retrieved chunks: ~s" chunk-list)))
                             (t
                              ;; simply remove the marked items
                              ;; may be "faster" to do this later
                              ;; once the set is trimed elsewise, but
                              ;; for now keep things simple
                              (unwind-protect
                                  (progn
                                    (when sact
                                      (bt:acquire-lock (dm-state-lock dm))
                                      (setf (sact-trace-remove-recent (dm-current-trace dm)) t))
                                    
                                    (when (dm-act-level act 'high)
                                      (model-output "Removing recently retrieved chunks:"))
                                    
                                    (setf (last-request-finst last-request) :unmarked)
                                    
                                    (setf chunk-list 
                                      (remove-if (lambda (x)
                                                   (when (member x finsts :key 'car :test 'eq-chunks-fct)
                                                     
                                                     (when sact
                                                       (push-last x (sact-trace-recents (dm-current-trace dm))))
                                                     
                                                     (when (dm-act-level act 'high)
                                                       (model-output "~s" x))
                                                     
                                                     (push x (last-request-finst-chunks last-request))
                                                     t))
                                                 chunk-list)))
                                (when sact
                                  (bt:release-lock (dm-state-lock dm))))))))))))
        
        
        (when (member :mp-value requested-slots)
          (let ((mp-value (chunk-spec-slot-spec request :mp-value)))
            (cond ((> (length mp-value) 1)
                   (invalid :mp-multi '("Invalid retrieval request." ":mp-value parameter used more than once.")))
                  ((not (eq '= (caar mp-value)))
                   (invalid :mp-modifier '("Invalid retrieval request." ":mp-value parameter's modifier can only be =.")))
                  ((not (numornil (third (car mp-value))))
                   (invalid :mp-not-num '("Invalid retrieval request." ":mp-value parameter's value can only be nil or a number.")))
                  
                  (t ;; it's a valid request
                   (setf mp (third (car mp-value)))))))
        
        (when (member :rt-value requested-slots)
          (let ((rt-value (chunk-spec-slot-spec request :rt-value)))
            (cond ((> (length rt-value) 1)
                   (invalid :rt-multi '("Invalid retrieval request." ":rt-value parameter used more than once.")))
                  ((not (eq '= (caar rt-value)))
                   (invalid :rt-modifier '("Invalid retrieval request." ":rt-value parameter's modifier can only be =.")))
                  ((not (numberp (third (car rt-value))))
                   (invalid :rt-not-num '("Invalid retrieval request." ":rt-value parameter's value must be a number.")))
                  
                  (t ;; it's a valid request
                   (setf rt (third (car rt-value))))))))
          
      (let ((best-val nil)
            (best nil)
            (return-val nil)
            (chunk-set 
             (cond ((or (null esc) (null mp)) ;; exact matches only
                    ;; do them individually for tracing purposes
                    (unwind-protect
                        (let ((found nil))
                          (when sact
                            (bt:acquire-lock (dm-state-lock dm)))
                        
                          (dolist (name chunk-list found)
                            (cond ((match-chunk-spec-p name request)
                                   (when sact
                                     (push-last name (sact-trace-matches (dm-current-trace dm))))
                                   (when (dm-act-level act 'medium)
                                     (model-output "Chunk ~s matches" name)) 
                                   (push-last name found))
                                  (t
                                   (when sact
                                     (push-last name (sact-trace-no-matches (dm-current-trace dm))))
                                   (when (dm-act-level act 'high)
                                     (model-output "Chunk ~s does not match" name))))
                            ))
                      (when sact
                        (bt:release-lock (dm-state-lock dm)))))
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
                                      ;; reverse it to keep the ordering the same
                                      ;; relative to the older version and so that
                                      ;; things are consistent with different requests
                                      (nreverse chunk-list)))
                           (non-matches (when (or act sact)
                                          (set-difference chunk-list matches))))
                          
                          (when (dm-act-level act 'high)
                            (dolist (c non-matches)
                              (model-output "Chunk ~s does not match" c)))
                          
                          (when sact
                            (bt:with-lock-held ((dm-state-lock dm))
                              (setf (sact-trace-matches (dm-current-trace dm)) matches)
                              (setf (sact-trace-no-matches (dm-current-trace dm)) non-matches)))
                          matches)))))
      
            
        (setf (last-request-matches last-request) chunk-set)
        
           
        (if esc
            (dolist (x chunk-set)
              (compute-activation dm x request :params-provided t
                                  :act act :sact sact :mp mp :esc esc :offsets offsets 
                                  :blc blc :bll bll :ol ol :act-scale act-scale
                                  :sa sa :spreading-hook spreading-hook :w-hook w-hook :sji-hook sji-hook 
                                  :mas mas :nsji nsji :pm-hook pm-hook :ms ms :md md :sim-hook sim-hook
                                  :cache-sim-hook cache-sim-hook :noise-hook noise-hook :ans ans :bl-hook bl-hook)
              
              (setf (chunk-retrieval-activation x) (chunk-activation x))
              (setf (chunk-retrieval-time x) (mp-time-ms))
                  
              (cond ((null best-val)
                     (setf best-val (chunk-activation x))
                     (push x best)
                     (when (dm-act-level act 'medium)
                       (model-output "Chunk ~s has the current best activation ~f" x best-val)))
                    ((= (chunk-activation x) best-val)
                     (push x best)
                     (when (dm-act-level act 'medium)
                       (model-output "Chunk ~s matches the current best activation ~f" x best-val)))
                    ((> (chunk-activation x) best-val)
                     (setf best-val (chunk-activation x))
                     (setf best (list x))
                     (when (dm-act-level act 'medium)
                       (model-output "Chunk ~s is now the current best with activation ~f" x best-val)))))
          (setf best (copy-list chunk-set)))
            
        (when (> (length best) 1)
          (if er
              (let ((b (random-item best)))
                (setf best (cons b (remove b best))))
            (setf best (sort best 'string<)))) ;; deterministic but unspecified...
        
        (setf (last-request-best last-request) best)
                        
        (when (car set-hook)
          (let ((chunk-set-with-best (when best (cons (car best) (remove (car best) chunk-set)))))
            (dolist (x set-hook)
              (let ((val (dispatch-apply x chunk-set-with-best)))
                (when val
                  (if return-val
                      (progn 
                        (print-warning "multiple set-hook functions returned a value - none used")
                        (setf return-val :error))
                    (setf return-val val)))))))
            
        (bt:with-lock-held ((dm-state-lock dm))
          (setf (dm-last-request dm) last-request)
          
          (cond ((and (listp return-val) (numberp (second return-val))
                      (chunk-p-fct (decode-string (first return-val))))
                 (let ((c (decode-string (first return-val))))
                   (setf (dm-busy dm) (schedule-event-relative (second return-val) 'retrieved-chunk
                                                               :module 'declarative 
                                                               :destination 'declarative 
                                                               :params (list c)
                                                               :details (concatenate 'string
                                                                          (symbol-name 'retrieved-chunk)
                                                                          " "
                                                                          (symbol-name (car return-val)))
                                                               :output 'medium))
                 
                   (when sact
                     (setf (sact-trace-result-type (dm-current-trace dm)) :force)
                     (setf (sact-trace-result (dm-current-trace dm)) c))
                   
                   (when (dm-act-level (dm-act dm) 'low)
                     (model-output "Retrieval-set-hook function forced retrieval of chunk ~s" c))))
                
                ((numberp return-val)
                 (setf (dm-busy dm) (schedule-event-relative return-val 'retrieval-failure
                                                             :module 'declarative 
                                                             :destination 'declarative 
                                                             :output 'low))
                 
                 (when sact
                   (setf (sact-trace-result-type (dm-current-trace dm)) :force-fail))
                 
                 (when (dm-act-level (dm-act dm) 'low)
                   (model-output "Retrieval-set-hook function forced retrieval failure")))
                
                ((or (null best) 
                     (and esc
                          (< best-val rt)))
                 
                 (setf (dm-busy dm) (schedule-event-relative (if esc
                                                                 (compute-activation-latency rt lf le)
                                                               0)
                                                             'retrieval-failure
                                                             :time-in-ms t 
                                                             :module 'declarative 
                                                             :destination 'declarative
                                                             :output 'low))
                 
                 (when sact
                   (setf (sact-trace-result-type (dm-current-trace dm)) :fail)
                   (setf (sact-trace-result (dm-current-trace dm)) (when best rt)))
                 
                 (when (dm-act-level act 'low)
                   (if best 
                       (model-output "No chunk above the retrieval threshold: ~f" rt)
                     (model-output "No matching chunk found retrieval failure"))))
                
                ((= (length best) 1)
                 (setf (dm-busy dm) (schedule-event-relative (if esc
                                                                 (compute-activation-latency (chunk-activation (car best)) lf le)
                                                               0)
                                                             'retrieved-chunk
                                                             :time-in-ms t 
                                                             :module 'declarative 
                                                             :destination 'declarative 
                                                             :params best
                                                             :details 
                                                             (concatenate 'string
                                                               (symbol-name 'retrieved-chunk)
                                                               " "
                                                               (symbol-name (car best)))
                                                             :output 'medium))
                 
                 (when sact
                   (setf (sact-trace-result-type (dm-current-trace dm)) :single)
                   (setf (sact-trace-result (dm-current-trace dm)) (cons (car best) (chunk-activation (car best)))))
                 
                 (when (dm-act-level act 'low)
                   (model-output "Chunk ~s with activation ~f is the best" (car best) (chunk-activation (car best)))))
                (t
                 (let ((best1 (car best)))
                   
                   (setf (dm-busy dm) (schedule-event-relative (if esc
                                                                   (compute-activation-latency (chunk-activation best1) lf le)
                                                                 0)
                                                               'retrieved-chunk
                                                               :time-in-ms t 
                                                               :module 'declarative 
                                                               :destination 'declarative 
                                                               :params (list best1)
                                                               :details 
                                                               (concatenate 'string
                                                                 (symbol-name 'retrieved-chunk)
                                                                 " "
                                                                 (symbol-name best1))
                                                               :output 'medium))
                   (when sact
                     (setf (sact-trace-result-type (dm-current-trace dm)) :multi)
                     (setf (sact-trace-result (dm-current-trace dm)) (cons best1 (chunk-activation best1))))
                   
                   (when (dm-act-level act 'low)
                     (model-output "Chunk ~s chosen among the chunks with activation ~f" best1 (chunk-activation best1))))))
          
          (when sact
            (setf (dm-current-trace dm) nil))))))))
    
  

(defun check-declarative-stuffing (dm)
  
  (let ((best-val nil)
        (best nil)
        ;(return-val nil)
        (chunk-set 
         ;; For now just consider everything, but this should be
         ;; smarter in the future...
         (all-dm-chunks dm))
        
        esc rt sact act er 
            mp offsets blc bll ol act-scale
            sa spreading-hook w-hook sji-hook mas nsji
            pm-hook ms md sim-hook cache-sim-hook
            noise-hook ans stuff lf le bl-hook)
  
  
   (bt:with-lock-held ((dm-param-lock dm))
      (setf
       act (dm-act dm)
       rt (dm-rt dm)
       sact (dm-sact dm)
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
       stuff (dm-stuff dm)
       lf (dm-lf dm)
       le (dm-le dm)
       bl-hook (dm-bl-hook dm)))
            
    (if esc
        (dolist (x chunk-set)
          (compute-activation dm x (define-chunk-spec isa chunk) :params-provided t
                              :act act :sact sact :mp mp :esc esc :offsets offsets 
                              :blc blc :bll bll :ol ol :act-scale act-scale
                              :sa sa :spreading-hook spreading-hook :w-hook w-hook :sji-hook sji-hook 
                              :mas mas :nsji nsji :pm-hook pm-hook :ms ms :md md :sim-hook sim-hook
                              :cache-sim-hook cache-sim-hook :noise-hook noise-hook :ans ans :bl-hook bl-hook)
          
          (cond ((null best-val)
                 (setf best-val (chunk-activation x))
                 (push x best))
                ((= (chunk-activation x) best-val)
                 (push x best))
                ((> (chunk-activation x) best-val)
                 (setf best-val (chunk-activation x))
                 (setf best (list x)))))
      (setf best chunk-set))
    
    (when (> (length best) 1)
      (if er
          (let ((b (random-item best)))
            (setf best (cons b (remove b best))))
        (setf best (sort (copy-list best) #'string<))))
    
    (cond ((or (null best) 
               (and esc
                    (< best-val rt)))
           (bt:with-lock-held ((dm-state-lock dm))
             (setf (dm-stuff-event dm)
               (schedule-event-relative stuff 'check-declarative-stuffing
                                        :time-in-ms t 
                                        :module 'declarative 
                                        :destination 'declarative
                                        :output 'low 
                                        :maintenance t))))
          
          ((= (length best) 1)
           (bt:with-lock-held ((dm-state-lock dm))
             (setf (dm-stuff-event dm) 
               (schedule-event-relative (if esc
                                            (compute-activation-latency (chunk-activation (car best)) lf le)
                                          0)
                                        'stuff-declarative-chunk
                                        :time-in-ms t 
                                        :module 'declarative 
                                        :destination 'declarative 
                                        :params best
                                        :details 
                                        (concatenate 'string
                                          (symbol-name 'stuffing-chunk)
                                          " "
                                          (symbol-name (car best)))
                                        :output 'low))))
          (t
           (let ((best1 (car best)))
             (bt:with-lock-held ((dm-state-lock dm))
               (setf (dm-stuff-event dm) (schedule-event-relative (if esc
                                                                      (compute-activation-latency (chunk-activation best1) lf le)
                                                                    0)
                                                                  'stuff-declarative-chunk
                                                                  :time-in-ms t 
                                                                  :module 'declarative 
                                                                  :destination 'declarative 
                                                                  :params (list best1)
                                                                  :details 
                                                                  (concatenate 'string
                                                                    (symbol-name 'stuffing-chunk)
                                                                    " "
                                                                    (symbol-name best1))
                                                                  :output 'low))))))))


;;; Retrieved-chunk
;;;
;;; Called as an event when a chunk has been retrieved and is ready to be placed
;;; into the buffer.
;;;
;;; The parameters are an instance of the module and the name of the chunk 
;;; to put in the buffer.

(defun retrieved-chunk (dm chunk)
  (let (rch)
    
    ;; Clear the busy flag
    
    (bt:with-lock-held ((dm-state-lock dm))
      (setf (dm-busy dm) nil))
    
    (bt:with-lock-held ((dm-param-lock dm))
      (setf rch (dm-retrieved-chunk-hook dm)))
    
    
    (when (car rch)
      (dolist (x rch)
        (dispatch-apply x chunk)))
  
    ;; Schedule an event to put the chunk into the buffer right now instead of
    ;; placing it there directly to comply with the guideline that buffer changes
    ;; should be scheduled.
    
    (schedule-set-buffer-chunk 'retrieval chunk 0 :time-in-ms t :module 'declarative :priority :max)
    
    ;; update the marker for having retrieved this chunk
    
    (update-declarative-finsts dm chunk)))


(defun stuff-declarative-chunk (dm chunk)
  
  (schedule-set-buffer-chunk 'retrieval chunk 0 :time-in-ms t :module 'declarative :priority :max  :requested nil)
  (bt:with-lock-held ((dm-state-lock dm))
    (setf (dm-stuff-event dm)
      (schedule-event-relative (bt:with-lock-held ((dm-param-lock dm)) (dm-stuff dm)) 'check-declarative-stuffing
                               :time-in-ms t 
                               :module 'declarative 
                               :destination 'declarative
                               :output 'low 
                               :maintenance t))))


(defun update-declarative-finsts (dm chunk)
  (bt:with-lock-held ((dm-state-lock dm))
    (setf (dm-finsts dm) (remove chunk (dm-finsts dm) :key 'car :test 'eq-chunks-fct))
    (push (cons chunk (mp-time-ms)) (dm-finsts dm))
    (setf (dm-finsts dm) (subseq (dm-finsts dm) 0 (min (length (dm-finsts dm)) (bt:with-lock-held ((dm-param-lock dm)) (dm-num-finsts dm)))))))

;;; Retrieval-failure
;;;
;;; Called as an event when a chunk failed to be found in response to a request.
;;;
;;; The parameter is an instance of the module.

(defun retrieval-failure (dm)
  
  ;; Clear the busy flag and set the failure flag.
  
  (bt:with-lock-held ((dm-state-lock dm))
    (setf (dm-busy dm) nil)
    (setf (dm-failed dm) t))
  
  
  (let ((rch (bt:with-lock-held ((dm-param-lock dm)) (dm-retrieved-chunk-hook dm))))
    (when (car rch)
      (dolist (x rch)
        (dispatch-apply x nil))))
  
  (set-buffer-failure 'retrieval))


;;; Dm-params
;;;

(defun dm-params (dm param)
  (bt:with-lock-held ((dm-param-lock dm))
    (cond ((consp param)
         ;; Warn about params that shouldn't be changed on the fly
           (when (and (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunks dm)) (find (car param) '(:esc :ol :bll :mas :ignore-buffers)))
             (print-warning "Changing declarative parameters with chunks in dm not supported.")
             (print-warning "Results may not be what one expects."))
           
           (case (car param)
             (:esc (setf (dm-esc dm) (cdr param)))
             (:er (setf (dm-er dm) (cdr param)))
             (:ol (setf (dm-ol dm) (cdr param)))
             
             (:blc (setf (dm-blc dm) (cdr param)))
             (:ans (setf (dm-ans dm) (cdr param)))
             (:pas (setf (dm-pas dm) (cdr param)))
             (:lf (setf (dm-lf dm) (seconds->ms (cdr param))) (cdr param))
             (:le (setf (dm-le dm) (cdr param)))
             (:mp (setf (dm-mp dm) (cdr param)))
             (:ms (setf (dm-ms dm) (cdr param)))
             (:md (setf (dm-md dm) (cdr param)))
             (:rt (setf (dm-rt dm) (cdr param)))
             (:bll 
              (when (and (dm-ol dm) (cdr param) (>= (cdr param) 1.0))
                (print-warning "Setting :bll to a value >= 1 when optimized learning is enabled will result in complex number activations and probably errors."))
              (when (cdr param) (setf (dm-act-scale dm) (log (expt 1/1000 (- (cdr param))))))
              (setf (dm-bll dm) (cdr param)))
             (:mas (setf (dm-mas dm) (cdr param))
                   (setf (dm-sa dm) (cdr param)))
             
             (:act (setf (dm-act dm) (cdr param)))
             (:sact (setf (dm-sact dm) (cdr param)))
             
             (:declarative-num-finsts (setf (dm-num-finsts dm) (cdr param)))
             (:declarative-finst-span 
              (setf (dm-finst-span dm) (safe-seconds->ms (cdr param) 'sgp))
              (cdr param))
             
             (:sim-hook 
              
              ;; If a different sim-hook is set clear the cache
              (when (cdr param)
                (when (and (dm-last-sim-hook dm)
                           (cdr param)
                           (not (equalp (cdr param) (dm-last-sim-hook dm))))
                  (clrhash (dm-sim-hook-cache dm)))
                (setf (dm-last-sim-hook dm) (cdr param)))
              
              (setf (dm-sim-hook dm) (cdr param)))
             
             (:sji-hook (setf (dm-sji-hook dm) (cdr param)))
             (:w-hook (setf (dm-w-hook dm) (cdr param)))
             
             (:bl-hook (setf (dm-bl-hook dm) (cdr param)))
             (:spreading-hook (setf (dm-spreading-hook dm) (cdr param)))
             (:partial-matching-hook (setf (dm-partial-matching-hook dm) (cdr param)))
             (:noise-hook (setf (dm-noise-hook dm) (cdr param)))
             
             (:declarative-stuffing (setf (dm-stuff dm) (if (numberp (cdr param)) (safe-seconds->ms (cdr param) 'sgp) (cdr param)))
                                    (cdr param))
             
             (:activation-offsets
              (setf (dm-offsets dm)
                (set-or-remove-hook-parameter :activation-offsets (dm-offsets dm) (cdr param))))
             
             (:retrieval-request-hook 
              (setf (dm-retrieval-request-hook dm) 
                (set-or-remove-hook-parameter :retrieval-request-hook (dm-retrieval-request-hook dm) (cdr param))))
             
             (:retrieval-set-hook 
              (setf (dm-retrieval-set-hook dm) 
                (set-or-remove-hook-parameter :retrieval-set-hook (dm-retrieval-set-hook dm) (cdr param))))
             
             (:retrieved-chunk-hook 
              (setf (dm-retrieved-chunk-hook dm) 
                (set-or-remove-hook-parameter :retrieved-chunk-hook (dm-retrieved-chunk-hook dm) (cdr param))))
             
             (:chunk-merge-hook 
              (setf (dm-chunk-merge-hook dm) 
                (set-or-remove-hook-parameter :chunk-merge-hook (dm-chunk-merge-hook dm) (cdr param))))
             
             (:chunk-add-hook 
              (setf (dm-chunk-add-hook dm) 
                (set-or-remove-hook-parameter :chunk-add-hook (dm-chunk-add-hook dm) (cdr param))))
             
             (:nsji (setf (dm-nsji dm) (cdr param)))
             
             (:ignore-buffers
              (setf (dm-ignore-buffers dm) (cdr param)))
             
             (:cache-sim-hook-results 
              (setf (dm-cache-sim-hook-results dm) (cdr param)))))
          (t 
           (case param
             
             (:blc (dm-blc dm))
             (:ans (dm-ans dm))
             (:pas (dm-pas dm))
             (:lf (ms->seconds (dm-lf dm)))
             (:le (dm-le dm))
             (:mp (dm-mp dm))
             (:ms (dm-ms dm))
             (:md (dm-md dm))
             (:rt (dm-rt dm))
             (:bll (dm-bll dm))
             (:mas (dm-mas dm))
             (:sa (print-warning "The :SA parameter is no longer used"))
             (:act (dm-act dm))
             (:sact (dm-sact dm))
             (:declarative-num-finsts (dm-num-finsts dm))
             (:declarative-finst-span (ms->seconds (dm-finst-span dm)))
             
             (:sim-hook (dm-sim-hook dm))
             (:sji-hook (dm-sji-hook dm))
             (:w-hook (dm-w-hook dm))
             
             (:bl-hook (dm-bl-hook dm))
             (:spreading-hook (dm-spreading-hook dm))
             (:partial-matching-hook (dm-partial-matching-hook dm))
             (:noise-hook (dm-noise-hook dm))
             
             (:activation-offsets (dm-offsets dm))
             
             (:declarative-stuffing (if (numberp (dm-stuff dm)) (ms->seconds (dm-stuff dm)) (dm-stuff dm)))
             
             (:retrieval-request-hook (dm-retrieval-request-hook dm))
             (:retrieval-set-hook (dm-retrieval-set-hook dm))
             (:retrieved-chunk-hook (dm-retrieved-chunk-hook dm))
             
             (:chunk-merge-hook (dm-chunk-merge-hook dm))
             (:chunk-add-hook (dm-chunk-add-hook dm))
             (:nsji (dm-nsji dm))
             
             (:ignore-buffers (dm-ignore-buffers dm))
             
             (:cache-sim-hook-results (dm-cache-sim-hook-results dm)))))))
  
  
;;; Merge-chunk-into-dm
;;;
;;; This function will be called automatically each time a buffer is cleared.
;;;
;;; The parameters are an instance of the module, the name of the buffer that 
;;; was cleared, and the name of the chunk that was in the buffer.
;;;
;;; This module adds that chunk to declarative memory and increments its 
;;; reference count.  If a matching chunk already exists in declarative memory,
;;; then those chunks are merged together.  If this is the first occurrence of 
;;; the chunk, then its initial parameters are set accordingly.

(defun merge-chunk-into-dm (dm buffer chunk &optional (ignore-stuffing nil))
  
  (let (stuff cmh ignore)
    (bt:with-lock-held ((dm-param-lock dm))
      (setf stuff (dm-stuff dm)
        cmh (dm-chunk-merge-hook dm)
        ignore (dm-ignore-buffers dm)))
  
    (unless (find buffer ignore)
      (when (and (eq buffer 'retrieval) (not ignore-stuffing) stuff)
        (bt:with-lock-held ((dm-state-lock dm))
          (unless (dm-stuff-event dm)
            (setf (dm-stuff-event dm)
              (schedule-event-now 'check-declarative-stuffing
                                  :module 'declarative 
                                  :destination 'declarative
                                  :output 'low 
                                  :priority :min
                                  :maintenance t)))))
      
      ;; Find any existing matching chunk
      
      (let* ((key (hash-chunk-contents chunk))
             (existing (gethash key (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunk-hash-table dm)))))
        
        (if existing
            (progn
              (merge-chunks-fct existing chunk)  ;; merging functions handle params
              
              (when (car cmh)
                (dolist (x cmh)
                  (dispatch-apply x chunk))))
          
          ;; otherwise add it to the list
          
          (if (chunk-not-storable chunk)
              (add-chunk-into-dm dm (copy-chunk-fct chunk) key)
            (add-chunk-into-dm dm chunk key)))))))

;; add-chunk-into-dm
;;;
;;; works like merge-chunk-into-dm but without doing any merging i.e. it
;;; makes the chunk part of dm and sets it's initial parameters regardless
;;; of whether it is a perfect match to an existing member
;;;

(defun add-chunk-into-dm (dm chunk &optional slot-key)
  (let (sa cah)
    (bt:with-lock-held ((dm-param-lock dm))
      (setf sa (dm-sa dm)
        cah (dm-chunk-add-hook dm)))
    
    ;; make it immutable
    
    (make-chunk-immutable chunk)
    
    (bt:with-lock-held ((dm-chunk-lock dm))
      (aif (assoc (chunk-slots-vector chunk) (dm-chunks dm))
           (push chunk (cdr it)) 
           (push (cons (chunk-slots-vector chunk) (list chunk)) (dm-chunks dm)))
  
      ;; Add it to the merge table
  
      (let ((key (aif slot-key it (hash-chunk-contents chunk))))
        (setf (chunk-fast-merge-key chunk) key)
        (setf (gethash key (dm-chunk-hash-table dm)) chunk)))
  
    ;; set the parameters
  
    (setf (chunk-in-dm chunk) t)
  
    (setf (chunk-creation-time chunk) (mp-time-ms))
    (setf (chunk-reference-list chunk) (list (mp-time-ms)))
    (setf (chunk-reference-count chunk) 1)
  
    ;; mark it as invalid for a buffer set now
  
    (setf (chunk-buffer-set-invalid chunk) t)
  
    ;; when spreading activation is on set the fan-out and fan-in values
  
    (when sa
    
      ;; Increment its fan-out for itself
    
      (incf (chunk-fan-out chunk))
  
      ;; set the fan-in values
    
      (let ((new-fans (mapcan (lambda (slot)
                                (let ((val (fast-chunk-slot-value-fct chunk slot)))
                                  (when (chunk-p-fct val)
                                    (list val))))
                        (chunk-filled-slots-list-fct chunk))))
      
        (dolist (j new-fans)
          (incf (chunk-fan-out j)))
      
        (setf (chunk-fan-in chunk)
          (mapcar (lambda (x) (cons x (count x new-fans))) (remove-duplicates new-fans))))
  
      (aif (assoc chunk (chunk-fan-in chunk))
           (incf (cdr it))
           (push (cons chunk 1) (chunk-fan-in chunk))))
  
    (when (car cah)
      (dolist (x cah)
        (dispatch-apply x chunk)))))


;;; Add-dm
;;; Add-dm-fct
;;;
;;; User level function for creating chunks and placing them directly into the
;;; declarative memory of the declarative memory module of the current model.
;;;
;;; It takes a parameter which is a chunk definition list like define-chunk-fct
;;; takes.  Those chunks are created and then added to the declarative memory 
;;; list with the current creation time and 1 reference.

(defmacro add-dm (&rest chunk-list)
  `(add-dm-fct ',chunk-list))

(defun add-dm-fct (chunk-definitions)
  
  ;; Need to find the current instance of the declarative module
  
  (let ((dm (get-module declarative)))  

    ;; if there is one, create the chunks and set the parameters

    (if (dm-p dm)  

        ;; pass the list of chunk defs off to define-chunks 
        ;; to do the creation

        (let ((chunks (define-chunks-fct chunk-definitions)))

          ;; Then iterate over those chunks and add them to the module

          (dolist (chunk chunks chunks)
            (add-chunk-into-dm dm chunk)))
          
       ;; otherwise report a warning to the meta-process because there may not
       ;; be a current model 

      (print-warning "Could not create chunks because no declarative module was found"))))

(defun add-dm-external (&rest chunk-defs)
  (add-dm-fct (decode-string-names chunk-defs)))

(defun add-dm-fct-external (chunk-defs)
  (add-dm-fct (decode-string-names chunk-defs)))

(add-act-r-command "add-dm" 'add-dm-external "Create chunks in the current model and add them to the model's declarative memory. Params: {'chunk-description'}*.")
(add-act-r-command "add-dm-fct" 'add-dm-fct-external "Create chunks in the current model and add them to the model's declarative memory. Params: ({'chunk-description'}*).")


(defmacro add-dm-chunks (&rest chunk-list)
  `(add-dm-chunks-fct ',chunk-list))

(defun add-dm-chunks-fct (chunk-list)
  (let ((dm (get-module declarative)))  
    
    ;; if there is one, create the chunks and set the parameters
    
    (if (dm-p dm)  
        (let (res)
          (dolist (c chunk-list)
            (when (and (chunk-p-fct c) (not (chunk-in-dm c))) 
              (add-chunk-into-dm dm c)
              (push-last c res)))
          res)
      (print-warning "Could not add chunks to DM because no declarative module was found"))))


(defun add-dm-chunks-external (&rest chunk-defs)
  (add-dm-chunks-fct (decode-string-names chunk-defs)))

(defun add-dm-chunks-fct-external (chunk-defs)
  (add-dm-chunks-fct (decode-string-names chunk-defs)))

(add-act-r-command "add-dm-chunks" 'add-dm-chunks-external "Add existing chunks to the current model's declarative memory. Params: chunk-name*.")
(add-act-r-command "add-dm-chunks-fct" 'add-dm-chunks-fct-external "Add existing chunks to the current model's declarative memory. Params: (chunk-name*).")



(defmacro merge-dm-chunks (&rest chunk-list)
  `(merge-dm-chunks-fct ',chunk-list))

(defun merge-dm-chunks-fct (chunk-list)
  (let ((dm (get-module declarative)))  
    
    ;; if there is one, create the chunks and set the parameters
    
    (if (dm-p dm)  
        (let (res)
          (dolist (c chunk-list)
            (when (and (chunk-p-fct c) (not (chunk-in-dm c)))
              (merge-chunk-into-dm dm nil c)
              (push-last c res)))
          res)
      (print-warning "Could not merge chunks to DM because no declarative module was found"))))

(defun merge-dm-chunks-external (&rest chunk-defs)
  (merge-dm-chunks-fct (decode-string-names chunk-defs)))

(defun merge-dm-chunks-fct-external (chunk-defs)
  (merge-dm-chunks-fct (decode-string-names chunk-defs)))

(add-act-r-command "merge-dm-chunks" 'merge-dm-chunks-external "Merge existing chunks to the current model's declarative memory. Params: chunk-name*.")
(add-act-r-command "merge-dm-chunks-fct" 'merge-dm-chunks-fct-external "Merge existing chunks to the current model's declarative memory. Params: (chunk-name*).")




;;; merge-dm
;;; merge-dm-fct
;;;
;;; User level function for creating chunks and merging them into the declarative 
;;; memory of the current model.
;;;
;;; It takes a parameter which is a chunk definition list like define-chunk-fct
;;; takes.  Those chunks are created and then merged into the declarative memory 
;;; of the model in the same way a cleared buffer is:  if the chunk is a match
;;; to an existing chunk then that existing chunk gets a new reference, but if
;;; there is no matching chunk then that chunk is added as a new element at the
;;; current time.


(defun sort-for-binding-dm (ordering)
  (let ((result nil))
    (dolist (x ordering result)
      (aif (position-if (lambda (y) (find (car x) (cdr y))) result)
           (splice-into-position-des result it x)
           (push-last x result)))))
        

(defmacro merge-dm (&rest chunk-list)
  `(merge-dm-fct ',chunk-list))

(defun merge-dm-fct (chunk-definitions)
  
  ;; Need to find the current instance of the declarative module
  
  (let ((dm (get-module declarative)))  

    ;; if there is one, create the chunks and set the parameters

    (if (dm-p dm)  

        ;; pass the list of chunk defs off to define-chunks 
        ;; to do the creation

        (let* ((chunks (define-chunks-fct chunk-definitions))
               (ordering (mapcar (lambda (x) 
                                   (cons x (mapcan (lambda (slot)
                                                           (let ((val (fast-chunk-slot-value-fct x slot)))
                                                             (when (and (chunk-p-fct val) (find val chunks))
                                                               (list val))))
                                                   (chunk-filled-slots-list-fct x))))
                           chunks)))
          
          (if (circular-references ordering)
              (progn
                (model-warning "Chunks in call to merge-dm have circular references.")
                (model-warning "  Because of that there is no safe order for merging and they will be merged in the order provided.")
                (dolist (chunk chunks chunks)
                  (merge-chunk-into-dm dm nil chunk t)))
            
            (progn
              (setf chunks (mapcar 'car (sort-for-binding-dm ordering)))
              (dolist (chunk chunks chunks)
                (merge-chunk-into-dm dm nil chunk t)))))
          
       ;; otherwise report a warning to the meta-process because there may not
       ;; be a current model 

      (print-warning 
       "Could not create chunks because no declarative module was found"))))  

(defun merge-dm-external (&rest chunk-defs)
  (merge-dm-fct (decode-string-names chunk-defs)))

(defun merge-dm-fct-external (chunk-defs)
  (merge-dm-fct (decode-string-names chunk-defs)))

(add-act-r-command "merge-dm" 'merge-dm-external "Merge chunks into the current model's declarative memory. Params: {'chunk-description'}*.")
(add-act-r-command "merge-dm-fct" 'merge-dm-fct-external "Merge chunks into the current model's declarative memory. Params: ({'chunk-description'}*).")


(defun retrieval-buffer-status ()
  (format nil "  recently-retrieved nil: ~S~%  recently-retrieved t  : ~S" (query-buffer 'retrieval '(recently-retrieved  nil))
    (query-buffer 'retrieval '(recently-retrieved  t))))

(defun act-value-test (val)
  (or (null val) 
      (eq val t) 
      (eq val 'high) 
      (eq val 'medium) 
      (eq val 'low)))

(defun nsji-value-test (x)
  (or (tornil x) (eq x 'warn)))

(defun create-declarative-module (x)
  (declare (ignore x))
  (make-dm))

;;; Call define-module to hook the module into the framework.

(define-module-fct 'declarative 
    (list (define-buffer-fct 'retrieval 
            :request-params (list :recently-retrieved :mp-value :rt-value)
            :queries (list 'recently-retrieved)
            :status-fn 'retrieval-buffer-status)) 
  (list (define-parameter :esc :owner nil)
        (define-parameter :er :owner nil)
        (define-parameter :ol :owner nil)
        
        (define-parameter :blc :valid-test 'numberp :default-value 0.0 :warning "a number" :documentation "Base Level Constant")
        (define-parameter :ans :valid-test 'posnumornil :default-value nil :warning "a positive number or nil" :documentation "Activation Noise S")
        (define-parameter :pas :valid-test 'posnumornil :default-value nil :warning "a positive number or nil" :documentation "Permanent Activation noise S")
        (define-parameter :lf :valid-test 'nonneg :default-value 1.0 :warning "a non-negative number" :documentation "Latency Factor")
        (define-parameter :le :valid-test 'nonneg :default-value 1.0 :warning "a non-negative number" :documentation "Latency Exponent")
        (define-parameter :mp :valid-test 'numornil :default-value nil :warning "a number or nil" :documentation "Mismatch Penalty")
        (define-parameter :ms :valid-test 'numberp :default-value 0.0 :warning "a number" :documentation "Maximum Similarity")
        (define-parameter :md :valid-test 'numberp :default-value -1.0 :warning "a number" :documentation "Maximum Difference")
        (define-parameter :rt :valid-test 'numberp :default-value 0.0 :warning "a number" :documentation "Retrieval Threshold")
        (define-parameter :bll :valid-test 'posnumornil :default-value nil :warning "a positive number or nil" :documentation "Base Level Learning")        
        (define-parameter :mas :valid-test 'numornil :default-value nil :warning "a number or nil" :documentation "Maximum Associative Strength")
        (define-parameter :declarative-stuffing :valid-test 'posnumornil :default-value nil :warning "a positive number or nil" 
          :documentation "Period of declarative buffer stuffing attempts")
        
        (define-parameter :act :valid-test 'act-value-test
          :default-value nil :warning "T, nil, high, medium, or low" :documentation "Activation Trace")
        
        (define-parameter :sact :valid-test 'act-value-test
          :default-value nil :warning "T, nil, high, medium, or low" :documentation "Save Activation Trace")
        
        (define-parameter :declarative-num-finsts :valid-test 'posnum :default-value 4 :warning "positive number" 
          :documentation "Number of declarative finst markers")
                
        (define-parameter :declarative-finst-span :valid-test 'posnum :default-value 3.0 :warning "positive number" 
          :documentation "Duration of declarative finst markers in seconds")
        
        (define-parameter :sim-hook :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "a function, string naming a command, or nil" :documentation "Similarity hook")
        (define-parameter :sji-hook :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "a function, string naming a command, or nil" :documentation "Sji hook")
        (define-parameter :w-hook :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "a function, string naming a command, or nil" :documentation "Wkj hook")
        
        (define-parameter :bl-hook :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "a function, string naming a command, or nil" 
          :documentation "Baselevel component hook")
        (define-parameter :spreading-hook :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "a function, string naming a command, or nil" 
          :documentation "Spreading component hook")
        (define-parameter :partial-matching-hook :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "a function, string naming a command, or nil" 
          :documentation "Partial matching component hook")
        (define-parameter :noise-hook :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "a function, string naming a command, or nil" :documentation "Noise component hook")
        
        (define-parameter :activation-offsets :valid-test 'local-or-remote-function-or-nil :default-value nil :warning "a function, string naming a command, or nil" 
          :documentation "Add additional activation equation components")
        
        (define-parameter :retrieval-request-hook 
          :valid-test 'local-or-remote-function-or-remove
          :default-value nil :warning "a function, string naming a command, nil, or (:remove <item>)" 
          :documentation "Retrieval notification hook")
        
        (define-parameter :retrieval-set-hook 
          :valid-test 'local-or-remote-function-or-remove 
          :default-value nil :warning "a function, string naming a command, nil, or (:remove <item>)" 
          :documentation "Prospective retrievals hook")
        
        (define-parameter :retrieved-chunk-hook :valid-test 'local-or-remote-function-or-remove
          :default-value nil :warning "a function, string naming a command, nil, or (:remove <item>)" 
          :documentation "Retrieval completion hook")
        
        (define-parameter :chunk-merge-hook :valid-test 'local-or-remote-function-or-remove
          :default-value nil :warning "a function, string naming a command, nil, or (:remove <item>)" 
          :documentation "Hook called when a chunk is merged into dm")
        
        (define-parameter :chunk-add-hook :valid-test 'local-or-remote-function-or-remove
          :default-value nil :warning "a function, string naming a command, nil, or (:remove <item>)" 
          :documentation "Hook called when a chunk is added to dm")
        
        
        (define-parameter :ignore-buffers :valid-test 'list-of-buffers-p
          :default-value nil :warning "a list of buffer names" 
          :documentation "Buffers whose chunks don't need to be stored in DM for this model")
        
        (define-parameter :nsji :valid-test 'nsji-value-test :default-value 'warn
          :warning "T, warn, or nil" :documentation "Indicate whether S-log(fan) is allowed to return negative values")
        (define-parameter :cache-sim-hook-results :valid-test 'tornil :default-value nil
          :warning "T or nil" :documentation "Whether the results of calling a sim-hook function should be cached to avoid future calls to the hook function"))
  
  :version "7.0" 
  :documentation "The declarative memory module stores chunks from the buffers for retrieval"
  
  ;; The creation function returns a new dm structure
  ;; that doesn't require knowing the current model's name
  
  :creation 'create-declarative-module
    
  :reset '(reset-dm-module secondary-reset-dm-module tertiary-reset-dm-module)
  :query 'dm-query-request
  :request 'dm-request
  :params 'dm-params
  :notify-on-clear 'merge-chunk-into-dm)

;;; Note which parameters should signal a warning if :esc is nil

(register-subsymbolic-parameters :blc :ans :pas :lf :le :mp :ms :md :rt :bll :mas :sa)

;;; Functions to compute activations and latency

(defun compute-activation (dm chunk request &key params-provided
                              act sact mp esc offsets blc bll ol act-scale
                              sa spreading-hook w-hook sji-hook mas nsji
                              pm-hook ms md sim-hook cache-sim-hook
                              noise-hook ans bl-hook)
  
  (unless params-provided
    (bt:with-lock-held ((dm-param-lock dm))
      (setf
       act (dm-act dm)
       sact (dm-sact dm)
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
       bl-hook (dm-bl-hook dm))))
  
  (let (clear-trace)
    
    (when (dm-act-level act 'medium)
      (model-output "Computing activation for chunk ~s" chunk))
    
    (when sact
      (bt:with-lock-held ((dm-state-lock dm))
        (unless (dm-current-trace dm)
        
          (setf clear-trace t)
        
          ;; If there's already one for this time use it
        
          (aif (gethash (mp-time-ms) (dm-trace-table dm))
               (setf (dm-current-trace dm) it)
               (progn
                 (setf (dm-current-trace dm) (make-sact-trace :esc esc))
                 (setf (gethash (mp-time-ms) (dm-trace-table dm)) (dm-current-trace dm)))))
    
        (setf (dm-current-sact-chunk dm) (make-sact-chunk :name chunk))))
    
    (setf (chunk-activation chunk) 
      (+ (base-level-activation dm chunk act bl-hook sact blc bll ol act-scale)
         (spreading-activation dm chunk sa act spreading-hook sact w-hook sji-hook mas nsji)
         (partial-matching dm chunk request mp act pm-hook sact ms md sim-hook cache-sim-hook)
         (activation-noise dm chunk noise-hook act sact ans)
         (activation-offsets chunk offsets act)))
    
    (when sact
      (bt:with-lock-held ((dm-state-lock dm))
        (setf (sact-chunk-total (dm-current-sact-chunk dm)) (chunk-activation chunk))
        
        ;; if this chunk is already recorded don't do so again
        (unless (find chunk (sact-trace-chunks (dm-current-trace dm)) :key 'sact-chunk-name)
          (push-last (dm-current-sact-chunk dm) (sact-trace-chunks (dm-current-trace dm))))
        
        (when clear-trace
          (setf (dm-current-trace dm) nil))))
    
    (when (dm-act-level act 'low)
      (model-output "Chunk ~s has an activation of: ~f" chunk (chunk-activation chunk)))))


(defun activation-offsets (chunk offsets act)
  (let ((offset 0))
    (when offsets
      (dolist (x offsets)
        (let ((res (dispatch-apply x chunk)))
          (when (numberp res)
            (when (dm-act-level act 'medium)
              (model-output "Adding offset from ~a: ~f" x res))
            (incf offset res)))))
    offset))
                   
(defun base-level-activation (dm chunk act bl-hook sact blc bll ol act-scale)
  
  (when (dm-act-level act 'medium)
    (model-output "Computing base-level"))
  
  (let ((base-level nil))
    
    (when bl-hook
      (setf base-level (dispatch-apply bl-hook chunk)))
    
    (cond ((numberp base-level)
           
           (when sact
             (bt:with-lock-held ((dm-state-lock dm))
               (setf (sact-chunk-bl-style (dm-current-sact-chunk dm)) :hook)
               (setf (sact-chunk-blc (dm-current-sact-chunk dm)) blc)))
           
           (when (dm-act-level act 'medium)
             (model-output "base-level hook returns: ~f" base-level)))
          (t
           (setf base-level 
             (cond (bll
                    (when sact
                      (bt:with-lock-held ((dm-state-lock dm))
                        (setf (sact-chunk-bl-style (dm-current-sact-chunk dm)) :learn)
                        (setf (sact-chunk-blc (dm-current-sact-chunk dm)) blc)))
                    
                    (+ (progn
                         (when (dm-act-level act 'medium)
                           (model-output "Starting with blc: ~f" blc))
                         blc)
                       (cond ((zerop (chunk-reference-count chunk))
                              (when sact
                                (bt:with-lock-held ((dm-state-lock dm))
                                  (setf (sact-chunk-zero-ref (dm-current-sact-chunk dm)) t)))
                              (model-warning "Cannot compute base-level for a chunk with no references.")
                              -999999.0)
                             (t ;; just use the ACT-R 5 function basically as is for now
                              (compute-references dm (chunk-reference-count chunk)
                                                  (chunk-reference-list chunk) (chunk-creation-time chunk)
                                                  (- bll) act ol act-scale sact)))))
                   (t ;; bll nil
                    
                    (when sact
                      (bt:with-lock-held ((dm-state-lock dm))
                        (setf (sact-chunk-bl-style (dm-current-sact-chunk dm)) :simple)
                        (setf (sact-chunk-blc (dm-current-sact-chunk dm)) blc)
                        (setf (sact-chunk-base-level (dm-current-sact-chunk dm)) (chunk-base-level chunk))))
                    
                    (if (chunk-base-level chunk)
                        (progn
                          (when (dm-act-level act 'medium)
                            (model-output "User provided chunk base-level: ~f" (chunk-base-level chunk)))
                          (chunk-base-level chunk))
                      (progn
                        (when (dm-act-level act 'medium)
                          (model-output "Starting with blc: ~f" blc))
                        blc)))))
           
           
           (when (dm-act-level act 'medium)
             (model-output "Total base-level: ~f" base-level))))
    
    (when sact
      (bt:with-lock-held ((dm-state-lock dm))
        (setf (sact-chunk-bl-result (dm-current-sact-chunk dm)) base-level)))
    
    (setf (chunk-last-base-level chunk) base-level)))


#|
Interesting note on spreading activation and the retrieval buffer.
Currently, the retrieval buffer will never be an active source of
activation because the buffer will clear before the request is made.
That is the desired current implementation - no buffers get treated
special and all production actions are allowed to occur before the
retrieval is attempted.

That may need to be revisited at some point, but for now parsimony
of operation is more important.

|#

(defun spreading-activation (dm chunk dm-sa act spreading-hook sact w-hook sji-hook mas nsji)
  
  (if dm-sa
      (setf (chunk-source-spread chunk)
        (let ((sa nil))
          (when (dm-act-level act 'medium)
            (model-output "Computing activation spreading from buffers"))
          
          (when spreading-hook
            (setf sa (dispatch-apply spreading-hook chunk)))
          
          (cond ((numberp sa)
                 
                 (when sact
                   (bt:with-lock-held ((dm-state-lock dm))
                     (setf (sact-chunk-sa (dm-current-sact-chunk dm)) :hook)
                     (setf (sact-chunk-sa-value (dm-current-sact-chunk dm)) sa)))
                 
                 (when (dm-act-level act 'medium)
                   (model-output "spreading activation hook returns: ~f" sa))
                 sa)
                
                (t
                 (let ((total-spread 0.0))
                   (dolist (buffer (model-buffers))
                     (let ((buffer-chunk (buffer-read buffer))
                           (spread (buffer-spread buffer)))
                       (when (and buffer-chunk (not (zerop spread)))
                         
                         (when sact
                           (bt:with-lock-held ((dm-state-lock dm))
                             (push (list buffer buffer-chunk spread)
                                   (sact-chunk-sa-buffers (dm-current-sact-chunk dm)))))
                         
                         (when (dm-act-level act 'medium)
                           (model-output "  Spreading ~f from buffer ~s chunk ~s" spread buffer buffer-chunk))
                         
                         (let ((js (mapcan (lambda (slot)
                                             (let ((val (fast-chunk-slot-value-fct buffer-chunk slot)))
                                               (when (chunk-p-fct val)
                                                 (list (cons val slot)))))
                                     (chunk-filled-slots-list-fct buffer-chunk))))
                           
                           (when (dm-act-level act 'medium)
                             (model-output "    sources of activation are: ~s" (mapcar 'car js)))
                           
                           (dolist (j js)
                             (let* ((sji (compute-sji (car j) chunk sji-hook mas nsji))
                                    (level (if w-hook
                                               (let ((val (dispatch-apply w-hook buffer (cdr j))))
                                                 (if (numberp val)
                                                     (progn 
                                                       (when (dm-act-level act 'medium)
                                                         (model-output "    Wkj hook returns level ~f" val))
                                                       val)
                                                   (/ spread (length js))))
                                             (/ spread (length js))))
                                    (total (* level sji)))
                               
                               (when sact
                                 (bt:with-lock-held ((dm-state-lock dm))
                                   (push-last (list (car j) total level sji)
                                              (car (sact-chunk-sa-buffers (dm-current-sact-chunk dm))))))
                               
                               (when (dm-act-level act 'medium)
                                 (model-output "    Spreading activation  ~f from source ~s level  ~f times Sji ~f" total (car j) level sji))
                               
                               (incf total-spread total)))))))
                   
                   (when (dm-act-level act 'medium)
                     (model-output "Total spreading activation: ~f" total-spread)) 
                   
                   (when sact
                     (bt:with-lock-held ((dm-state-lock dm))
                       (setf (sact-chunk-sa (dm-current-sact-chunk dm)) :full)
                       (setf (sact-chunk-sa-value (dm-current-sact-chunk dm)) total-spread)))
                   
                   total-spread)))))
    0.0))


(defun compute-sji (j i sji-hook mas nsji)
  (let ((sji (if sji-hook
                 (dispatch-apply sji-hook j i)
               nil)))
    (if (numberp sji)
        sji
      (if (assoc j (chunk-sjis i) :test 'eq-chunks-fct)
          (cadr (assoc j (chunk-sjis i) :test 'eq-chunks-fct))
        (aif (assoc j (chunk-fan-in i) :test 'eq-chunks-fct)
             (let ((val (- mas (log-coerced (chunk-fan-j-to-i j it)))))
               (if (eq nsji t)
                   val
                 (progn
                   (when (and nsji (minusp val))
                     (model-warning "Calculated Sji value between ~s and ~s is negative, but using a value of 0." j i))
                   (max val 0.0))))
             0.0)))))

(defun chunk-fan-j-to-i (j i-spread)
  (if (chunk-in-dm j)
      (/ (chunk-fan-out j) (cdr i-spread))
    (/ (+ (chunk-fan-out j) (max 1 (chunk-c-fan-out j))) (cdr i-spread))))


(defun partial-matching (dm chunk request mp act pm-hook sact ms md sim-hook cache-sim-hook)
  (if mp
      (progn
        (when (dm-act-level act 'medium)
          (model-output "Computing partial matching component"))
        (let ((pm (when pm-hook
                    (let ((id (chunk-spec-to-id request)))
                      (prog1
                          (dispatch-apply pm-hook chunk request)
                        (release-chunk-spec-id id))))))
          
          (cond ((numberp pm) 
                 
                 (when sact
                   (bt:with-lock-held ((dm-state-lock dm))
                     (setf (sact-chunk-pm (dm-current-sact-chunk dm)) :hook)
                     (setf (sact-chunk-pm-value (dm-current-sact-chunk dm)) pm)))
                 
                 (when (dm-act-level act 'medium)
                   (model-output "partial matching hook returns: ~f" pm))
                 pm)
                (t
                 (let ((total-sim 0.0))
                   (dolist (k (chunk-spec-slot-spec request))
                     (when (and (or (eq (car k) '=)
                                    (eq (car k) '-))
                                (not (keywordp (second k))) ;; skip request parameters
                                (not (chunk-spec-variable-p (third k)))) ;Why? How could it ever be a variable?
                       (let ((value (fast-chunk-slot-value-fct chunk (second k))))
                       
                       (when (dm-act-level act 'medium)
                         (model-output "  comparing slot ~S" (second k))
                         (model-output "  Requested: ~s ~s  Chunk's slot value: ~s" (first k) (third k) value))
                       
                       (let* ((sim (chunks-similarity dm (third k) value sim-hook cache-sim-hook act ms md t))
                              
                              (sim-dif (case (car k) 
                                         (= (* mp sim))
                                         
                                         (- (cond ((= sim ms)
                                                   (* mp md))
                                                  (t
                                                   (when (dm-act-level act 'medium)
                                                     (model-output "  negation test with similarity not ms has no effect"))
                                                   0))))))
                         
                         (when (dm-act-level act 'medium)
                           (model-output "  effective similarity value is ~f" sim-dif))
                         
                         (when sact
                           (bt:with-lock-held ((dm-state-lock dm))
                             (push-last (list (second k) (first k) (third k) value sim sim-dif)
                                        (sact-chunk-pm-tests (dm-current-sact-chunk dm)))))
                         
                         (incf total-sim sim-dif)))))
                     
                   (when sact
                     (bt:with-lock-held ((dm-state-lock dm))
                       (setf (sact-chunk-pm (dm-current-sact-chunk dm)) :full)
                       (setf (sact-chunk-pm-value (dm-current-sact-chunk dm)) total-sim)))
                   
                   (when (dm-act-level act 'medium)
                     (model-output "Total similarity score ~f" total-sim))
                   
                   total-sim)))))
    0.0))


(defun chunks-similarity (dm chunk1 chunk2 sim-hook cache-sim-hook act ms md &optional (trace nil))
  (let ((sim (if sim-hook
                 (if cache-sim-hook
                     (multiple-value-bind (value exists) (gethash (cons chunk1 chunk2) (bt:with-lock-held ((dm-param-lock dm)) (dm-sim-hook-cache dm)))
                       (if exists 
                           value
                         (let ((result (dispatch-apply sim-hook chunk1 chunk2)))
                           (bt:with-lock-held ((dm-param-lock dm)) (setf (gethash (cons chunk1 chunk2) (dm-sim-hook-cache dm)) result)))))
                   (dispatch-apply sim-hook chunk1 chunk2))
               nil)))
    (cond ((numberp sim)
           (when (and (dm-act-level act 'medium) trace)
             (model-output "  similarity hook returns: ~f" sim))
           sim)
          (t (setf sim (cond ((not (and (chunk-p-fct chunk1)
                                        (chunk-p-fct chunk2)))
                              (if (chunk-slot-equal chunk1 chunk2)
                                  ms md))
                             ((assoc chunk1 (chunk-similarities chunk2) :test 'eq-chunks-fct)
                              (cadr (assoc chunk1 (chunk-similarities chunk2) :test 'eq-chunks-fct)))
                             ((eq-chunks-fct chunk1 chunk2)
                              ms)
                             (t md)))
             (when (and (dm-act-level act 'medium) trace)
               (model-output "  similarity: ~f" sim))
             sim))))


(defun activation-noise (dm chunk noise-hook act sact ans)
  (let ((noise (when noise-hook
                   (dispatch-apply noise-hook chunk))))
                 
    (cond ((numberp noise)
           (when (dm-act-level act 'medium)
             (model-output "noise hook returns: ~f" noise))
           (when sact
             (bt:with-lock-held ((dm-state-lock dm))
               (setf (sact-chunk-noise (dm-current-sact-chunk dm)) :hook)
               (setf (sact-chunk-noise-p (dm-current-sact-chunk dm)) noise)))
           noise)
          (t
           (setf noise (if ans
                           (act-r-noise ans)
                         0.0))
           
           (when sact
             (bt:with-lock-held ((dm-state-lock dm))
               (setf (sact-chunk-noise (dm-current-sact-chunk dm)) :full)
               (setf (sact-chunk-noise-p (dm-current-sact-chunk dm)) (chunk-permanent-noise chunk))
               (setf (sact-chunk-noise-t (dm-current-sact-chunk dm)) noise)))
           
           (when (dm-act-level act 'medium)
             (model-output "Adding transient noise ~f" noise)
             (model-output "Adding permanent noise ~f" (chunk-permanent-noise chunk)))
      
           (+ noise (chunk-permanent-noise chunk))))))
    

(defun compute-activation-latency (activation lf le)
  (round (* lf
            (exp-coerced (* -1 le activation)))))


;;; based on Christian's function from ACT-R 5 but now do calcs
;;; other than full optimized with ms times and adjust

(defun compute-references (dm n references creation-time minus-decay act ol act-scale sact)
  "Computes generalized decay formula from number and list of references,
   creation time and minus the decay rate."
  
  (when (dm-act-level act 'medium)
    (model-output "Computing base-level from ~d references (~{~/print-time-in-seconds/~^ ~})" n references)
    (model-output "  creation time: ~/print-time-in-seconds/ decay: ~f  Optimized-learning: ~s" creation-time (- minus-decay) ol))
  
  (let ((value 0.0)
        (last-reference 0)
        (current-time (mp-time-ms)))
    (when references
      (dolist (reference references)
        (incf value (expt-coerced (max 50 (- current-time reference)) minus-decay))
        (setf last-reference reference)))
    
    (if ol
        (let ((denominator (+ 1.0 minus-decay)))
          (if (numberp ol)
              (progn
                (when (> n ol)
                  (when (or (< current-time creation-time)
                            (< current-time last-reference))
                    (print-warning "Activation calculation problem because time has moved backwards.  Assuming a 0 time delay to avoid calculation error."))
                  (incf value (/ (* (- n ol)
                                    (- (expt-coerced (max 0 (- current-time creation-time)) denominator)
                                       (expt-coerced (max 0 (- current-time last-reference)) denominator)))
                                 (* (max 50 (- last-reference creation-time)) denominator))))
                (setf value (+ (log value) act-scale)))
            
            (setf value (log (/ (* n (expt-coerced (max .05 (ms->seconds (- current-time creation-time))) minus-decay))
                                denominator)))))
      (setf value (+ (log value) act-scale)))
    
    (when sact
      (bt:with-lock-held ((dm-state-lock dm))
        (setf (sact-chunk-base-level (dm-current-sact-chunk dm)) value)
        (setf (sact-chunk-bl-count (dm-current-sact-chunk dm)) n)
        (setf (sact-chunk-bl-refs (dm-current-sact-chunk dm)) references)
        (setf (sact-chunk-bl-ct (dm-current-sact-chunk dm)) creation-time)
        (setf (sact-chunk-decay (dm-current-sact-chunk dm)) (- minus-decay))
        (setf (sact-chunk-ol (dm-current-sact-chunk dm)) ol)))
    
    (when (dm-act-level act 'medium)
      (model-output "base-level value: ~f" value))
    value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some potentially useful Declarative module accessing tools


(defun all-dm-chunks (dm)
  (mapcan (lambda (x) (copy-list (cdr x))) (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunks dm))))

(defun all-dm-chunk-types (dm)
  (mapcar (lambda (x) (slot-mask->names (car x))) (bt:with-lock-held ((dm-chunk-lock dm)) (dm-chunks dm))))

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
