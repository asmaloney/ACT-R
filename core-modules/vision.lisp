;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2007 Mike Byrne/Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : vision.lisp
;;; Version     : 9.1
;;; 
;;; Description : Source code for the ACT-R Vision Module.  
;;;
;;; Bugs        : [X] Should object-present-p do a chunk match to determine if
;;;                   the object is still the "same"?  I'm doing that now, but
;;;                   may want to consider something else long term...
;;;                   UPDATE: not doing the chunk test now because it results 
;;;                   in differences where a change is not re-encoded that 
;;;                   which should be with update-attended-loc.  May still
;;;                   want to consider other options.
;;;             : [ ] Not so much a bug, but an inconsistency with the old
;;;                   version.  Find-best-feature doesn't "short circuit"
;;;                   as often with the legacy devices as the old system which
;;;                   means that it calls random-item at times where the same
;;;                   model under the old module didn't.  The behavior of this
;;;                   module is the same regardless, but the extra call to
;;;                   act-r-random will result in different "random" results
;;;                   downstream from the extra call in other modules or code.
;;;                   Only really matters if one has set the seed explicitly
;;;                   to produce a specific output (the testing traces).  Shouldn't
;;;                   cause problems otherwise.  
;;;             : [X] Current module doesn't work quite the same as the old version
;;;                   when items overlap in the display.  Particularly, a screen
;;;                   change during an attention shift for items at the same location
;;;                   in the old module you got the "old" attended item but in the
;;;                   current module you get the "new" item.  Not sure what is
;;;                   really right there.  Switched it so that now you get the
;;;                   old item as well.
;;;
;;; Todo        : [ ] Check the icon-entry code again now that the feats get
;;;                   mapped from old to new when there's no clear because it
;;;                   probably doesn't need to do the "feat match" check anymore...
;;;             : [X] Add in the support for other scales/synthesizing letters from
;;;                   primitives and re-enable the :optimize-visual parameter.
;;;             : [X] Add the tracking code in.
;;;             : [ ] Add hooks into proc-display and find-location to let users
;;;                   configure things and filter the matching (pre and post default)
;;;                   as needed.
;;;             : [ ] Consider generalizing the scale parameter so that the devices
;;;                   can take advantage of it for other uses.
;;;             : [ ] What's the right attended status for something that's no
;;;                   longer in the visicon or for overlapping new items?
;;;                   *Essentially added an "undefined" attended status for
;;;                   *items that are no longer in the visicon - it always
;;;                   *returns nil for the query regardless of the request
;;;                   *which varies from the old module because the old one would 
;;;                   *consider items no longer in the display as "attended nil".
;;;                   -- changed this because if it's a visual-location then it
;;;                   should return "attended nil" as t if it's truely not attended.
;;;                   However, this and the old module now differ because if
;;;                   attention goes back to one of these "old" locations the
;;;                   "old" location chunk reports as attended t in the current
;;;                   module, but it was attended nil in all past versions.
;;;             : [ ] Fix the add/delete/update-visicon-item code to work right
;;;                   when deleteing visicon chunks and test-feats is true.
;;;                   Pretty low priority since most people use add/delete/update
;;;                   for performance and test-feats can have a noticeable cost.
;;;             : [X] There is no feat-table now! 
;;;                   Should the feat-table be purged other than on a clear, for
;;;                   instance if an object goes away without a clear?
;;;             : [ ] Does test-feats do the right thing in enter-into-visicon
;;;                   with respect to checking the real-visual-value i.e. does
;;;                   that always get set correctly elsewhere or can that break
;;;                   with a custom device?
;;; 
;;; ----- History ----- [look also at function comments]
;;;
;;; 2007.05.23 Dan 
;;;             : * Start of a rewrite to use chunks internally for the visicon
;;;             :   and as the objects themselves.
;;;             : * Will work in conjunction with updated device interface, devices,
;;;             :   and motor (mouse cursor stuff) files.
;;; 2007.06.20 Dan
;;;             : * Works in the abstract case at this point (modeler supplied
;;;             :   device which handles everything explicitly).
;;;             : * Took the nearest slot out of visual-locations and made it
;;;             :   a request parameter - should have happened in the old system
;;;             :   long ago...
;;;             : * Also took the userprop slots out and put height and width in.
;;; 2007.07.05 Dan
;;;             : * Modified visicon-chunks to sort based on x-y coordinates to
;;;             :   provide more consistency across platforms/Lisps since the
;;;             :   visicon hash-table doesn't maintain the entry order.  It's
;;;             :   a slight performance hit, but consistency in the tutorial 
;;;             :   and elsewhere is worth it (for now at least, and it could
;;;             :   be put onto a parameter to check if people don't like it).
;;; 2007.07.06 Dan
;;;             : * Added a default case to icon-entry to just return the
;;;             :   chunks explicit chunk-visicon-entry value when the "matching"
;;;             :   process fails (fixes a bug in re-encoding of the screen when
;;;             :   a "close" but new object is the one that gets re-encoded).
;;; 2007.07.06 Dan
;;;             : * Added the text-feats function even though it's not really
;;;             :   needed yet...
;;; 2007.07.09 Dan
;;;             : * Fixed a bug when move tolerance was 0.0 - needed to add
;;;             :   a couple chunk-visicion-entry calls around chunk names in a
;;;             :   couple of places to make sure the "right" chunk was being
;;;             :   referenced.
;;; 2007.07.12 Dan
;;;             : * Fixed a bug with the move-attention requests.  It didn't
;;;             :   verify that the chunk for the screen-pos was of type visual-
;;;             :   location before sending it off for further processing.
;;; 2007.10.04 Dan
;;;             : * Added the pieces to handle the "special" processing of
;;;             :   text items into sub-features (:optimize-visual nil), and
;;;             :   the processing of scale {phrase or word} in the move-attention
;;;             :   requests for text items.
;;;             : * Updated the version to a2.
;;; 2007.11.01 Dan
;;;             : * Fixed bugs in attend-visual-coordinates and enter-into-visicon.
;;; 2007.11.16 Dan
;;;             : * Added a check of moving-atteniton to the drop-out check for
;;;             :   update-attended-loc.
;;; 2007.12.11 Dan
;;;             : * Added the randomizing of times for the reencoding events.
;;;             :   That was a bug introduced in the 5->6 transition.
;;; 2007.12.17 Dan
;;;             : * Finished first pass at adding the tracking code in.
;;;             :   It works with either the old "screen-obj" style mechanism
;;;             :   (though that's now depricated) or through the visual-location
;;;             :   feature chunk (must return the same location chunk to move
;;;             :   the item for the model).
;;; 2007.12.18 Dan
;;;             : * Clean-up of the tracking code to fix some of the cases
;;;             :   that weren't signaled or handled correctly.
;;; 2007.12.19 Dan
;;;             : * Minor fix for update-tracking-mth - make sure vis-loc-to-obj
;;;             :   called with the current device or object as needed.
;;; 2007.12.20 Dan
;;;             : * Changed object-present-p to not do the chunk testing for
;;;             :   checking for a similar object because it prevented some
;;;             :   re-encodings that should have happened.  (Also see the note
;;;             :   under bugs above.)
;;; 2007.12.21 Dan
;;;             : * Changed find-current-locs-with-spec to catch the case where
;;;             :   :nearest current is specified but there isn't yet a currently
;;;             :   attended location.  To be backward compatible it assumes 0,0
;;;             :   and prints a warning about it.
;;; 2008.01.07 Dan
;;;             : * Added merging functions for visicon-entry, visual-object, and
;;;             :   real-visual-value chunk parameters so that the new chunk's
;;;             :   values are set in the merged chunk.  Vision needs the newest
;;;             :   info available to properly align with the visicon and current
;;;             :   device...
;;; 2008.01.09 Dan
;;;             : * To maintain the determinism of things the chunks to be
;;;             :   stuffed are first sorted by name before a random one is 
;;;             :   picked.  Shouldn't be too much of a performance hit and
;;;             :   it's needed if the test models are to always produce the
;;;             :   same traces.
;;; 2008.01.16 Dan
;;;             : * Fixed a bug in icon-entry that could throw a warning if the
;;;             :   visual-location being used in an attention shift hadn't 
;;;             :   orignially come from the vision module.
;;; 2008.01.31 Dan
;;;             : * Added in a version of the add-screen-object and delete-screen-object
;;;             :   commands for those that used them in the old module.  The
;;;             :   delete method differs from the old version in that it takes
;;;             :   the same object as add does.
;;; 2008.02.14 Dan
;;;             : * Adding a new request to the visual-location buffer - set-visloc-default.
;;;             :   That allows the model to configure the buffer stuffing test as
;;;             :   would be done with the set-visloc-default command without needing
;;;             :   to resort to a !eval!.
;;;             : * Re-fixed a bug that got lost somewhere along the way!
;;;             :   find-current-locs-with-spec mis-tested :attended.
;;;             :   Was fixed on 2007.12.07 (it's in the commit logs) but that
;;;             :   change and note isn't here now so somewhere along the way
;;;             :   I modified and committed an old version...
;;;             : * Added some more saftey tests to set-visloc-default command.
;;; 2008.02.14 Dan [3.0b1]
;;;             : * Corrected an oversight in the visual-location matching.
;;;             :   The value slot wasn't being compared to the "real" visual
;;;             :   value for something like text where the visual-location
;;;             :   chunk's value isn't the same as what print-visicon shows.
;;;             :   Thus, unlike the old version of the module, requests like
;;;             :   this wouldn't work as expected prior to this fix:
;;;             :   +visual-location> isa visual-location value "a"
;;;             :   for "standard" text display items because the default operation 
;;;             :   for text items is to set the value slot to text in the 
;;;             :   visual-location chunk and that's what the comparison was using.
;;;             :   Now, when the chunk-real-visual-value parameter is set, that's
;;;             :   what gets used for the value slot tests.
;;; 2008.02.15 Dan
;;;             : * Added a remove-finst function since update-cursor-feat 
;;;             :   called it!
;;; 2008.03.13 Dan
;;;             : * Changed the priority on the set-visloc-default request
;;;             :   action that gets scheduled to have a value of 9 which puts
;;;             :   it just below the implicit buffer clearing due to the 
;;;             :   request.  That makes it very unlikely that a cooccuring
;;;             :   proc-display will sneak in between the clearing of the 
;;;             :   buffer and the changing of the defaults and thus stuff
;;;             :   an "unwanted" chunk in there.  It could avoid that by
;;;             :   scheduling the priority higher than the clear, but I don't
;;;             :   think that's a good practice in general.
;;; 2008.03.21 Dan
;;;             : * Fixed a bug in test-attended because it assumed the chunk
;;;             :   for which the attended status was being checked was still
;;;             :   a valid member of the visicon.  Brings up a question which
;;;             :   I've added under bugs - what's the right status for such
;;;             :   a chunk?  Should it just always "fail" the query?  
;;;             : * A related question the test model brought up is what 
;;;             :   about when there are "overlapping" features - should they
;;;             :   share a finst?
;;;             : * Fixed an issue with enter-into-visicon because it didn't
;;;             :   consider the "real" value of the items when eliminating
;;;             :   possible duplicates.
;;; 2008.03.31 Dan
;;;             : * Changed visicon-chunks so that it has an optional parameter
;;;             :   which indicates when to sort things.  Only using that for
;;;             :   the print-visicon command and find-location requests now to 
;;;             :   improve performance and still maintain the "cross platform"
;;;             :   consistency.
;;; 2008.04.01 Dan
;;;             : * Added add-visicon-item and delete-visicon-item as more
;;;             :   "user" level equivalents of add-screen-object and delete-...
;;;             :   because they don't require passing the vision module itself
;;;             :   as a parameter.  Otherwise they're the same commands.
;;; 2008.04.07 Dan
;;;             : * Added an automatic call to stuff-visloc-buffer when a
;;;             :   set-visloc-default request is made to trigger any possible 
;;;             :   stuffing based on the new settings. 
;;;             : * Adding a new parameter :auto-attend.
;;;             :   When it is set to t (defaults to nil) it will cause the 
;;;             :   vision module to automatically start a move-attention after
;;;             :   a successful visual-location request completes to attend
;;;             :   to that visual-location.
;;;             :   Basically it's a modeling shortcut for simple situations
;;;             :   where the result of the visual-location request aren't
;;;             :   that important and one is just going to attend it anyway.
;;;             :   Saves some productions in the model but doesn't affect the
;;;             :   timing because the request is delayed 50ms to compensate for 
;;;             :   the skipped production.
;;;             :   The module is not marked as busy during the 50ms delay.  So
;;;             :   one should be careful about using explicit move-attention
;;;             :   requests when this parameter is enabled.  The assumption is
;;;             :   that you won't be using explicit requests if you turn this
;;;             :   option on.
;;; 2008.04.08 Dan
;;;             : * Fixed an issue with re-encoding - it didn't mark the chunk
;;;             :   being set into the buffer as unrequested.  Now it does.
;;; 2008.04.10 Dan
;;;             : * Added a new parameter :test-feats which can allow for some
;;;             :   significant improvements in run time for proc-display calls
;;;             :   if it is set to nil (the default value is t).  There are 
;;;             :   limited situations where one can safely set it to nil:
;;;             :   - All proc-display calls specify :clear t
;;;             :   - If each visual item in the device returns the "same" chunk 
;;;             :     from build-vis-locs-for every time (by same it means same 
;;;             :     name though the contents could differ)
;;;             :   The next step is to update the built in devices so that 
;;;             :   they satisfy the second situation and thus work correctly
;;;             :   with the parameter set to nil.
;;; 2008.05.22 Dan
;;;             : * Added a warning to add-finst since it can be called by the
;;              :   model through an assign-finst request with a "bad" value.
;;; 2008.06.20 Dan
;;;             : * Removed the -fct from slot-specs-to-chunk-spec-list and
;;;             :   slot-specs-to-chunk-spec since the macros were removed from 
;;;             :   the chunk-spec code.
;;; 2008.07.02 Dan [3.0b2]
;;;             : * Added code to delete the "internal" visicon chunks when a
;;;             :   proc-display occurs.
;;;             :   Requires locking the device during visual-location buffer
;;;             :   settings to avoid possible deletion of a chunk that hasn't
;;;             :   yet been copied for the buffer.
;;; 2008.07.14 Dan
;;;             : * Fixed an issue when test-feats was t that overlapping text
;;;             :   could persist - the old icon entry may stick around when 
;;;             :   :clear t not given to proc-display.
;;; 2008.07.15 Dan
;;;             : * Changed the output setting for find-loc-failure so that it
;;;             :   prints in the low detail trace.
;;; 2008.07.22 Dan
;;;             : * Fixing the overlapping text issue on the 14th re-introduced
;;;             :   a bug where overlapping text items with different values
;;;             :   were "merged" into one feature.  Re-fixed that issue.
;;; 2008.07.24 Dan
;;;             : * Adding yet another parameter  - :delete-visicon-chunks.
;;;             :   Defaults to t, and when on the visicon chunks get purged
;;;             :   during proc-display.
;;; 2008.08.11 Dan [3.0b3]
;;;             : * Adding a new query to the module to allow one to detect
;;;             :   visual changes other than through the visual-location buffer
;;;             :   stuffing. 
;;;             :   
;;;             :   There's a new parameter called :scene-change-threshold
;;;             :   and if during a proc-display the computed change equals
;;;             :   or exceeds that setting the scene-change query of the visual
;;;             :   buffer will go true.
;;;             :   It will stay true for a time equal to the :visual-onset-span 
;;;             :   value.
;;;             :   It can also be explicitly cleared by using the new request
;;;             :   which takes no parameters and no time:
;;;             :     +visual>
;;;             :       isa clear-scene-change
;;;             :   It is also cleared with the existing clear request:
;;;             :     +visual>
;;;             :       isa clear 
;;;             :   The change computation is computed as a proportion of
;;;             :   items which differ between the two sets and the setting of the 
;;;             :   :scene-change-threshold must be a value in the range 0.0-1.0.
;;;             :   
;;;             :   Here's the computation used to determine the change value:
;;;             :   
;;;             :   ov = the set of items in the visicon when proc-display called.
;;;             :   o  = the number of items in ov.
;;;             :   d  = number of items from ov not in the new visicon.
;;;             :   n  = number of items in the new visicon which were not in ov.
;;;             :   
;;;             :   change = (d+n)/(o+n)
;;;             :   
;;;             :   If o and n are both 0 then the change is 0.    
;;; 2008.08.13 Dan
;;;             : * Added a new query for the visual buffer: scene-change-value.
;;;             :   It's mostly there as a debugging aid to show what the last
;;;             :   scene-change proprotion was through the buffer-status 
;;;             :   command.  However, it can be queried normally.  The value
;;;             :   given should be a number.  If the last scene-change had a
;;;             :   proportion of change greater than that value the query will
;;;             :   respond as true.  It does not time out like the scene-change
;;;             :   query does (after the onset span time).
;;; 2009.03.31 Dan [3.0]
;;;             : * Seems stable enough now to take the beta version off...
;;;             : * Added a hash-table based compare mechanism like DM uses for
;;;             :   fast-merging to the process-display code.  About the same
;;;             :   speed-wise for small (0~15) displays, but significantly
;;;             :   faster as they get larger.
;;;             : * Added a new command remove-visual-finsts since people have 
;;;             :   been using proc-display :clear t to do that even without
;;;             :   "changing" the display.
;;;             : * Fixed a bug related to "proc-display :clear t" - when the
;;;             :   same chunks were returned by the build-vis-locs-for method(s)
;;;             :   as it had previously any finsts on those items may have
;;;             :   persisted once the item was no longer "new" again.
;;; 2009.04.27 Dan
;;;             : * Added a check to see if there's a lock set in stuff-visloc-
;;;             :   buffer so it doesn't try to add more than one item at a time.
;;;             :   It was already setting and clearing a lock, but just wasn't
;;;             :   testing for it...
;;; 2009.04.29 Dan
;;;             : * Changed the phrase parsing so that the phrase! chunk gets a 
;;;             :   color based on the majority color among the individual words.
;;;             :   Also added a colors slot to hold the list of item colors
;;;             :   corresponding to the words and objects slots.
;;; 2009.08.27 Dan
;;;             : * Changed visicon-update so that it has an optional parameter
;;;             :   to determine whether or not it needs to count the visicon 
;;;             :   items for return.
;;;             : * Modified delete-screen-object (and thus delete-visicon-item)
;;;             :   so that the internal chunk gets purged if :delete-visicon-chunks
;;;             :   is true.
;;; 2009.08.28 Dan
;;;             : * Added optional update parameters to the internal add-screen-object
;;;             :   and delete-screen-object and the user commands add-visicon-item
;;;             :   and delete-visicon-item so that multiple adds & deletes can avoid
;;;             :   all having to run the updating code.  One benefit to that is that 
;;;             :   it won't always be the first item added to the screen that gets 
;;;             :   stuffed if one doesn't want that.
;;;             : * Added an update-visicon-item command to go along with the 
;;;             :   add- and delete- ones.  It has a required parameter of the object
;;;             :   an optional parameter for updating (like add and delete now have)
;;;             :   and two possible keword parameters :same-chunks and :chunks.
;;;             :   It works in one of three ways based on which if any of the 
;;;             :   keyword parameters are provided (if the parameters are invalid
;;;             :   then it just prints a warning and does nothing):
;;;             :
;;;             :   - If neither is given then it does the same thing as if the object 
;;;             :   had been deleted and then added again.
;;;             :
;;;             :   - If :chunks is non-nil and is either a symbol naming a chunk or a 
;;;             :   list of chunk names all of which correspond to the chunk(s) that
;;;             :   were originally added to the visicon for the object with add-visicon-
;;;             :   item then it will update the module's internal representation of 
;;;             :   those features.
;;;             :
;;;             :   - If :chunks is nil but :same-chunks is t then it will call 
;;;             :   build-vis-locs-for for that object and treat the list of chunks
;;;             :   that are returned the same as if the :chunks parameter had been
;;;             :   given with that list.
;;;             :
;;;             :   One note on using update-visicon-item is that you must set :test-feats
;;;             :   to nil for now otherwise it will not work.  That means if you want
;;;             :   overlapping duplicate features removed you will have to handle that
;;;             :   explicitly outside of the module.
;;; 2009.08.31 Dan
;;;             : * Added more warnings since delete-visicon-item also has a potential
;;;             :   issue with test-feats if it's set to purge chunks.
;;; 2009.11.18 Dan
;;;             : * Fixed an issue with a '(lambda ...)  in synthesize-phrase
;;;             :   because LispWorks doesn't like that construct.
;;; 2010.02.04 Dan
;;;             : * Make sure tracking keeps a finst on something tracked for 
;;;             :   longer than the finst duration time.
;;; 2010.02.12 Dan
;;;             : * Let a move-attention break tracking without having to
;;;             :   explicitly clear it first.  The issue was that tracking 
;;;             :   keeps the module busy so the move-attention would jam, but
;;;             :   now it ignores that jam if it's due to tracking.
;;; 2010.02.18 Dan
;;;             : * Fixed a bug which could cause problems with a device that
;;;             :   was modifying the chunks for its features prior to a 
;;;             :   proc-display call.  Within-move was returning the original
;;;             :   chunk and not the internal copy and featlis-to-chunks was
;;;             :   using that orignial chunk to set the icon-entry for the
;;;             :   object.  Now within-move uses the internal copy and featlis-to-
;;;             :   chunks gets the appropriate visicon-entry from that.
;;; 2010.02.19 Dan
;;;             : * Modified featlis-to-focus and determine-focus-dmo to go
;;;             :   along with the last couple of fixes to make sure everything
;;;             :   stays in sync.
;;; 2010.03.02 Dan
;;;             : * Fixed a bug with featlis-to-focus that could cause problems
;;;             :   when an attended item leaves the screen.
;;; 2010.03.25 Dan
;;;             : * Added a purge-chunk to enter-into-visicon for those which
;;;             :   already existed in the table since they wouldn't have been
;;;             :   purged previously.
;;; 2010.04.30 Dan
;;;             : * Adjusted how :auto-attend works to now mark the module as
;;;             :   busy from the time of the find-location all the way through
;;;             :   the encoding-complete.
;;;             : * Also added an event to show in the trace and make sure that
;;;             :   it is using the buffer's chunk and not the internal chunk
;;;             :   to avoid problems with deleting visicon chunks.
;;; 2010.05.03 Dan
;;;             : * Changed the :output of the "No visual-object found" event
;;;             :   from 'high to 'medium since that's how it has always printed
;;;             :   anyway because of a bug in filter-test.
;;; 2010.08.03 Dan
;;;             : * Changed update-tracking-loc-chunk to also set the current-marker
;;;             :   so that the focus ring updates as the model tracks the item.
;;;             : * Also set current-marker in update-tracking-mth because there
;;;             :   could be a model issue that prevents update-tracing-loc-chunk
;;;             :   from being called (cases 6, 7, 8, and 9).
;;;             : * Don't set the object's screen-pos slot in update-tracking-mth
;;;             :   now except in the cases where it won't get set via update-tracking-loc-chunk.
;;;             :   That prevents an issue with deleting that visicion chunk 
;;;             :   later in most cases.  May want to not set it at all in those
;;;             :   cases.
;;; 2011.01.19 Dan
;;;             : * Changed the attended querying so that chunks which aren't
;;;             :   part of the visicon now may still report that they are
;;;             :   attended nil instead of having all the attended queries fail
;;;             :   as was done previously and noted in the todo.  This differs
;;;             :   from the old vision because the old one only allowed the attended
;;;             :   tests for chunks which came from vision at some point -- thus
;;;             :   arbitrary locations would have been in the all queries fail
;;;             :   situation which the new version no longer has.  Neither however
;;;             :   addresses issue of attention "returning".
;;;             : * Depricating update-tracking now in preparation for fixing some
;;;             :   of the lingering finst/object issues in tracking.
;;;             : * Added unlock-tracking since unlock-device used to call
;;;             :   update-tracking.
;;;             : * Changed update-tracking-mth to account for the fact that things
;;;             :   should always be valid when it starts which avoids some warnings
;;;             :   and problems that used to happen when it tried to update an
;;;             :   'object' on the fly.
;;;             : * Update-attended-loc schedules it's state change so that the
;;;             :   buffer trace will always catch it.
;;; 2011.02.04 Dan [3.1]
;;;             : * A reworking of how the visicon gets maintained with respect to
;;;             :   recording features.  Instead of two tables, one for feature
;;;             :   comparison and one for the visicon, there's only a visicon
;;;             :   table and the keys are the feature lists when :test-feats is
;;;             :   set to t, otherwise the device's loc chunk is the key.
;;;             : * Fixes some issues with re-attending for an object that moves.
;;;             : * Update cursor doesn't modify the existing feature anymore
;;;             :   and instead just creates a new one.
;;;             : * Find-current-loc-with-spec now uses the real-visual-value
;;;             :   value of the "current" when 'value current' is requested if
;;;             :   there is such a value.
;;;             : * Added a new chunk parameter visual-feature-name since the
;;;             :   visicon-entry can now be a list of features instead of the
;;;             :   device's chunk name.
;;;             : * Updated the synth-feat code so that the colors get set
;;;             :   based on the majority color of the lower level when :optimize-
;;;             :   visual is nil too.
;;;             : * Fixed an issue with how the features are marked for a phrase
;;;             :   when optimize-visual is nil.
;;; 2011.03.28 Dan
;;;             : * Fixed a bug with how current-marker gets set during tracking.
;;; 2011.04.01 Dan
;;;             : * Very minor change to tracking code to avoid an unnecessary
;;;             :   copy.
;;; 2011.04.26 Dan
;;;             : * Changed finst and new/change values to use the ms timing.
;;; 2011.04.28 Dan
;;;             : * Added a declaim to avoid a compiler warning about an
;;;             :   undefined function and added some declares to ignore unused
;;;             :   variables.
;;;             : * Suppress the warnings about extending the chunks.
;;; 2011.05.16 Dan
;;;             : * Replaced pm-warning calls with model-warning.
;;; 2011.11.09 Dan
;;;             : * Commented out the *enable-package-locked-errors* line for ACL 
;;;             :   because it no longer seems to be necessary and it sets up a 
;;;             :   dangerous situation for users since that setting enables the
;;;             :   redefinition of CL functions without warning.
;;;             : * Added a new function add-word-characters which allows the 
;;;             :   user to modify how text strings get broken into "words" 
;;;             :   by specifying additional characters to include in sequences 
;;;             :   along with those that are alphanumericp.
;;; 2011.11.14 Dan
;;;             : * Added a new request parameter to visual-location requests
;;;             :   and two additional options for the :nearest specification.
;;;             :   The new :nearest options are clockwise and counterclockwise.
;;;             :   That is computed relative to a center point specified
;;;             :   by the new request parameter :center which should be either
;;;             :   a visual-location or visual-object.  If :center is not 
;;;             :   specified then the default center point is used which is
;;;             :   0,0 but can be changed with the set-visual-center-point 
;;;             :   command (note that open-exp-window will automatically set
;;;             :   the visual center point to the center of the window it opens).
;;;             : * Added current-x, current-y, clockwise, and counterclockwise
;;;             :   to the set of pm-constant chunks the module creates.
;;; 2011.12.19 Dan
;;;             : * Added a string< test to loc-sort so that when the x and y
;;;             :   coordinates are the same there's still a consistent sorting
;;;             :   to the features.
;;; 2012.01.24 Dan
;;;             : * Added a warning to process-display to note that it should
;;;             :   not be called more than once at a time because that can cause 
;;;             :   issues with the re-attending, but since people may be using
;;;             :   that as a hack to force a particular chunk to buffer stuff
;;;             :   in the vis-loc buffer it's a model warning so it can be
;;;             :   turned off.
;;; 2012.01.25 Dan
;;;             : * Changed the order in which the events are scheduled in 
;;;             :   update-attended-loc because it can affect when the next
;;;             :   conflict-resolution occurs.  This doesn't address the
;;;             :   bigger issue, but it at least fixes the only such case I
;;;             :   could find in the default modules.
;;; 2012.02.15 Dan
;;;             : * Added some code to avoid a warning about not having a current
;;;             :   location when one isn't really needed anyway.
;;; 2012.06.26 Dan
;;;             : * The currently-attended and current-marker are both cleared 
;;;             :   if the model processes a new device to avoid re-encoding
;;;             :   something from the new window that happens to be at the
;;;             :   same coordinates as the item last attended in the previous
;;;             :   device.
;;; 2012.06.29 Dan
;;;             : * Added gray as one of the default color chunks.
;;; 2012.08.09 Dan
;;;             : * The state change for update-attended-loc now happens both
;;;             :   immediately and in an event to avoid issues with multiple
;;;             :   simultaneous updates and to still allow for proper recording
;;;             :   of the state transitions.
;;; 2012.12.19 Dan
;;;             : * Added xyz-loc to get the vector of screen-x, screen-y, and
;;;             :   distance. 
;;;             : * Use xyz-loc in the nearest calculation so that distance 
;;;             :   matters too (doesn't change anything for default 2D devices).
;;; 2013.07.24 Dan
;;;             : * Added a run-time warning if a move-attention request doesn't
;;;             :   have a screen-pos value.
;;; 2013.10.02 Dan
;;;             : * Commented out the optimize proclaim since that persists and
;;;             :   may or may not be useful anyway.
;;; 2013.10.14 Dan
;;;             : * Fixed a bug with the find-best-feature function because it
;;;             :   specified a variable-character of #\$ instead of #\& in the
;;;             :   find-matching-chunks call.
;;;             : * Clear the found-locs table on reset.
;;; 2014.01.24 Dan
;;;             : * Changed build-string-feats so that it automatically handles
;;;             :   newlines so the results are consistent among the devices.
;;;             :   Has two additional keyword params now: 
;;;             :    line-height - how much to increase the y-pos for each newline
;;;             :    x-fct - an optional function which can be used to determine
;;;             :      the starting x coordinate for the current line.  If provided
;;;             :      it will be called once for each line of text being passed
;;;             :      three values: the string of the line of text, the value of 
;;;             :      the start-x parameter, and the obj parameter.  If it returns
;;;             :      a number that will override start-x.
;;; 2014.02.12 Dan
;;;             : * The request chunk-types are no longer subtypes of vision-command
;;;             :   since that wasn't actually used for anything.
;;;             : * Removed the abstract-letter and abstract-number chunk-types
;;;             :   since they aren't being used.
;;;             : * Changed the chunk-types for text, empty-space, oval, and
;;;             :   cursor to add a slot to each which is the same as the name 
;;;             :   of the type with a default value of t.  That makes them 
;;;             :   distinct types under the inheritance of the static typing 
;;;             :   mechanism.
;;; 2014.02.14 Dan [3.2]
;;;             : * The default distance for visual locations is now measured in
;;;             :   pixels to be consistent with screen-x and screen-y since
;;;             :   distance is used in nearest calculations.
;;;             : * To do that monitor pixels-per-inch as well and just set the
;;;             :   view-dist to be the right value when it or viewing-distance
;;;             :   change.
;;;             : * Test the location provided for :nearest in a visual-location
;;;             :   request to make sure it has all the coordinates needed and
;;;             :   if not substitute in 0s for x,y and view-dist for distance.
;;;             : * Allow current-distance as a possible value for :nearest.
;;; 2014.02.18 Dan
;;;             : * Fixed a bug with visual-location request's processing of the
;;;             :   :center parameter.
;;; 2014.03.17 Dan [4.0]
;;;             : * Start the change to being consistent with chunks without types.
;;;             : * Changed the module warning function to indicate that it's a
;;;             :   chunk-spec being passed in even though it's ignored.
;;;             : * Changed the query-buffer call to be consistent with the new
;;;             :   internal code.
;;; 2014.05.20 Dan
;;;             : * Start the real conversion to chunks without types.  Change 
;;;             :   the request method and initial chunk-type specifications.
;;;             : * Lots of minor cleanup along the way.
;;;             : * Added an update-new to visicon-update and removed the 
;;;             :   update-new and check-finsts from find-location.
;;; 2014.05.21 Dan
;;;             : * Within-move now considers distance when computing the angle
;;;             :   difference instead of just using an xy pixel distance.
;;;             : * Featlis-to-chunks doesn't use the loc parameter so take it
;;;             :   out.
;;; 2014.05.23 Dan
;;;             : * When creating the sub-letter features for items automatically
;;;             :   create chunks for the feature values if they are symbols.
;;; 2014.05.30 Dan
;;;             : * Use the test-for-clear-request function in pm-module-request.
;;; 2014.07.07 Dan
;;;             : * The doc string doesn't need to indicate that it uses chunks
;;;             :   internally anymore.
;;; 2014.08.13 Dan
;;;             : * The set-visloc-default-request needs to strip the SET-VISLOC-DEFAULT
;;;             :   slot from the spec.
;;; 2014.10.29 Dan
;;;             : * Changed the last-command reported for a visual-location 
;;;             :   buffer 'find' request from visual-location to find-location.
;;; 2014.10.29 Dan
;;;             : * Added safety tests to set-char-feature-set and returns t/nil
;;;             :   now instead of the char-feats object.
;;; 2014.12.08 Dan
;;;             : * Added chunks for the colors pink, light-blue, purple, and
;;;             :   brown to cover all the ones used by the AGI.
;;; 2014.12.15 Dan
;;;             : * Fixed a bug with chop-string that caused problems if there
;;;             :   was an empty line in the input string.
;;; 2015.03.18 Dan
;;;             : * Explicitly turn off the pretty printer during print-icon-
;;;             :   feature to avoid weird line breaks in the visicon output.
;;; 2015.03.20 Dan
;;;             : * Failures for both buffers now set the buffer failure flag
;;;             :   using set-buffer-failure.
;;; 2015.03.23 Dan
;;;             : * The visual buffer failures are now marked as to whether it
;;;             :   was a requested encoding or an automatic one which failed.
;;; 2015.04.21 Dan [4.1]
;;;             : * New parameter :unstuff-visual-location which indicates 
;;;             :   whether or not stuffed visual-location chunks should be cleared
;;;             :   automatically by the module.  Defaults to nil.  Can be set to
;;;             :   t which means clear it after :VISUAL-ONSET-SPAN or a number
;;;             :   which is the time to clear in seconds.
;;; 2015.04.22 Dan
;;;             : * Also allow a value of new for :unstuff-visual-location which
;;;             :   means that it can be overwritten by a new chunk to stuff.
;;; 2015.05.20 Dan
;;;             : * Move-attention requests now work better when given a location
;;;             :   chunk that wasn't returned by vision because it tests the
;;;             :   features in that chunk against the items in the visicon 
;;;             :   which are within the move distance and picks the item from
;;;             :   those which match the most features instead of just whichever
;;;             :   was closest.  For example if one makes this request now 
;;;             :   where X and Y are the coordinates of a button it will always
;;;             :   return the text of the button instead of randomly choosing
;;;             :   either the button or the text (since they were equally close):
;;;             :     (define-chunks (loc screen-x X screen-y Y kind text))
;;;             :     (module-request 'visual (define-chunk-spec isa move-attention screen-pos loc))
;;;             : * Added the vis-loc-slots slot to the vision module and the
;;;             :   method record-vis-loc-slots so that the device can tell the
;;;             :   vision module what the valid coordinate slots are after
;;;             :   calling vis-loc-coordinate-slots when a device is installed.
;;; 2015.05.29 Dan
;;;             : * Added the clear-all-finsts request to the visual buffer.
;;; 2015.06.04 Dan
;;;             : * Use safe-seconds->ms for converting the params now.
;;;             : * Use ms internally for :visual-attention-latency.
;;; 2015.06.05 Dan
;;;             : * All scheduling done in ms or with the new schedule-event-now.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;             : * Changed default of :unstuff-visual-location to t.
;;; 2015.07.29 Dan [4.2]
;;;             : * Changed the way the check-unstuff-buffer action is scheduled
;;;             :   because that's now the precondition and unstuff-buffer is the
;;;             :   actual action.
;;;             : * Splitting the unstuff- parameter into two parameters:
;;;             :   :unstuff-visual-location can be nil, t, or # and indicates
;;;             :     whether to take a stuffed chunk out of the buffer and
;;;             :     defaults to t.
;;;             :   :overstuff-visual-location determines whether or not to 
;;;             :     stuff a new chunk overtop of a previously stuffed chunk
;;;             :     and defaults to nil.
;;;             : * When an unstuff is scheduled save it so that it can be
;;;             :   deleted if another gets scheduled.
;;; 2015.08.03 Dan
;;;             : * Find best features uses the internal visaul vis-loc-slots
;;;             :   instead of calling vis-loc-coordiate-slots again.
;;; 2015.08.17 Dan
;;;             : * Changed the randomize-time calls which were used with times
;;;             :   in ms to randomize-time-ms.
;;; 2015.08.19 Dan
;;;             : * Tracking needs to use chunk-difference-to-chunk-spec instead
;;;             :   of just chunk-name-to-chunk-spec because it may need to remove
;;;             :   slots from the current buffer chunk, but those wouldn't show
;;;             :   up in the spec for the new loc/obj chunk.
;;; 2015.08.20 Dan
;;;             : * Fixed a safety check in process-display for dividing by 0,
;;;             :   which shouldn't happen anyway.
;;;             : * Only schedule clear-process-display-called if process-display
;;;             :   isn't set to avoid multiple events.
;;; 2015.09.16 Dan [5.0]
;;;             : * Changed the visual buffer to be trackable and record the
;;;             :   request so that it can be sent to complete-request.  The
;;;             :   clear action will automatically clear everything because it
;;;             :   now calls complete-all-module-requests with the module's name,
;;;             :   but all the others will need to handle that.
;;;             : * Added the last-visual-request slot to the module class to
;;;             :   hold the request for completion.
;;;             : * The start-tracking request completes eventhough it leaves
;;;             :   the module busy because that seems like the right way to
;;;             :   handle that.
;;; 2016.07.08 Dan [5.1]
;;;             : * If the module is tracking something and the tracked object
;;;             :   chunk in the visual buffer has a screen-pos slot that matches
;;;             :   the chunk in the visual-location buffer then it should 
;;;             :   update that visual-location chunk even if there's a feature
;;;             :   mismatch otherwise -- could happen because of some custom
;;;             :   device code using modified chunks.
;;;             : * Added the parameter :tracking-clear which if set to t clears
;;;             :   the attended-loc when tracking fails and if it's set to nil
;;;             :   the loc remains which means it will attend the nearest thing
;;;             :   to where it left off (assuming it's within the move tolerance).
;;;             :   The default is t to be backward compatible.
;;; 2016.07.15 Dan
;;;             : * The unstuff-buffer event didn't specify the module as vision
;;;             :   but does now.
;;; 2016.07.20 Dan [6.0]
;;;             : * CLOF is back!  It turns out that with delete-visicon-chunks
;;;             :   enabled it's possible to end up in a situation where the 
;;;             :   current-marker chunk gets deleted before it gets a replacement.
;;;             :   There are a couple of ways to address that, but it seems the
;;;             :   easiest is to bring back a separate slot to hold the position
;;;             :   information outside of a chunk so it'll still be available
;;;             :   even if the chunk is deleted.
;;;             : * Encoding-complete and get-obj-at-location now take the position
;;;             :   vector (clof) as a parameter in addition to the location chunk
;;;             :   since the chunk could be deleted by the time the event occurs.
;;;             : * Clear clof in the reset function and the clear action.
;;; 2016.07.21 Dan
;;;             : * Updated featlis-to-focus to check whether it has a real chunk
;;;             :   for the location or not and use the clof value (converted to
;;;             :   a chunk) if needed.
;;; 2016.12.14 Dan [7.0]
;;;             : * Start of work to scrap the whole device concept!  It doesn't
;;;             :   really work in a distributed system -- can't create a Lisp
;;;             :   object in external system and having a stub on the Lisp side
;;;             :   reduces the flexibility.  Also moving away from passing 
;;;             :   chunks into the module.  
;;;             :   The current plan is to only expose the add/delete/modify feature
;;;             :   interface.
;;;             :   The features are provided as slot-value triples to allow for
;;;             :   the 'real visaul value' approach to be used for any feature,
;;;             :   and if a kind is provided then it uses the "default" object
;;;             :   creation code of isa <kind> (minus positional slots) (plus any 
;;;             :   other slots with third or second value).
;;;             :   The module creates the chunk internally to avoid issues with
;;;             :   having to copy things repeatedly and it automatically schedules
;;;             :   the proc-display whenever there's a change.  To replace the 
;;;             :   vis-loc-to-obj method when the "default" approach doesn't work
;;;             :   there'll be a parameter to specify a function to call through
;;;             :   the dispatcher to return an object description given a feature.
;;; 2016.12.15 Dan
;;;             : * Need to add a lock for visicon access because the feature
;;;             :   updating functions could be called in parallel from external
;;;             :   code with each other or proc-display.
;;;             : * Moved the lock-device functionality here from the device
;;;             :   interface since it was only vision anyway.
;;; 2016.12.16 Dan
;;;             : * Handle the scheduling of proc-display automatically, and have
;;;             :   that call process-display if it's OK to do so.  Requires 
;;;             :   adding some slots and removing a few from the module to better
;;;             :   track things now -- only schedule one proc-display at a time
;;;             :   no matter how many visicon changes happen and don't schedule
;;;             :   another until that actually processes the display.  The 
;;;             :   proc-display in the event queue may not actually correspond
;;;             :   to a visicon update because it might be an unlock-vision 
;;;             :   that happens later which actually results in calling process-
;;;             :   display.  There's a new event, visicon-update, which is 
;;;             :   going to call a dispatcher function (called "visicon-update")
;;;             :   that corresponds to the actual change.
;;;             : * Lots of updates to process-display to handle the new simplified
;;;             :   feature mechanism -- synthed feats probably don't work right 
;;;             :   at this point in several places so don't try to use that yet.
;;; 2016.12.21 Dan
;;;             : * Lots of modifications to the attending code to work with the
;;;             :   new approach.  Many places are 'unfinished' because tracking
;;;             :   and optimize-visual nil (char-primitives and synthisizing)
;;;             :   are currently not being dealt with.  There's also just a stub
;;;             :   for specifying a conversion function -- the slot feature-to-object
;;;             :   is intended to hold a string with a dispatch command name for
;;;             :   specifying the function (maybe through a parameter, but it could
;;;             :   also be part of a new 'install-device' mechanism).
;;; 2017.01.03 Dan
;;;             : * Check-finsts doesn't use the synthed-from flag and just checks
;;;             :   whether the items still have an id in the visicon (not quite
;;;             :   the same as before and need to fix the synthed features at
;;;             :   some point).
;;;             : * Fixed duplicate feature output from print-visicon.
;;; 2017.01.04 Dan
;;;             : * Fixed bug in insert-into-visicon because the splicing result
;;;             :   still needs to be recorded.
;;;             : * Pad the locations in print-visicon on the left.
;;; 2017.01.05 Dan
;;;             : * Add the code to create an interface for vision along with the
;;;             :   init and reset functions which do things previously handled in
;;;             :   install-device.
;;;             : * Shadow the :show-focus parameter -- it should probably be 
;;;             :   moved here eventually.
;;; 2017.01.06 Dan
;;;             : * Build-string-feats now just creates the feature list as 
;;;             :   used by add-visicon-features including the extra single
;;;             :   quotes for strings.
;;;             : * Find-location doesn't try to remap the value slot now since
;;;             :   that's already taken care of in the feature creation.
;;; 2017.01.11 Dan
;;;             : * Moved the show-focus parameter here instead of just shadowing.
;;; 2017.01.18 Dan
;;;             : * Removed the echo-act-r-output from commands.  The assumption
;;;             :   now is that echoing is handled by the user.
;;;             : * Use handle-evaluate-results.
;;; 2017.01.20 Dan
;;;             : * Don't need to get the vision module in the init function if
;;;             :   it's not doing anything...
;;; 2017.01.24 Dan
;;;             : * Only call device-update-attended-loc if the clof actually
;;;             :   changes for efficiency since it goes out through the central
;;;             :   dispatcher.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.08 Dan
;;;             : * Reworked the local versions of some remote commands to not
;;;             :   go out through the dispatcher.
;;;             : * Define-interface now uses optional parameters instead of 
;;;             :   keyword parameters.
;;; 2017.02.09 Dan
;;;             : * Added a new chunk-type, line-location, because the default
;;;             :   feature creation mechanisms now expose those slots for the
;;;             :   find-location action.  If the virtual device were built 
;;;             :   differently that wouldn't be necessary, but it uses the
;;;             :   simplest mechanisms at this point so adding this in for now.
;;; 2017.02.24 Dan
;;;             : * The clear method now calls set-current-marker instead of
;;;             :   directly modifying the clof.
;;; 2017.03.06 Dan
;;;             : * When creating the visual-object chunk carry the 'distance'
;;;             :   slot over with the height and width since that's necessary
;;;             :   for computing the effective width for Fitts' law in a cursor
;;;             :   movement.
;;; 2017.03.07 Dan
;;;             : * Removed the update-cursor-feat method.
;;; 2017.03.08 Dan
;;;             : * Fixed a bug with remove-visicon-interface because the fct
;;;             :   to delete all the visicon items is delete-all-visicon-features.
;;;             : * Fixed bugs in modify-visicon-features and process-display.
;;; 2017.03.16 Dan
;;;             : * Fixed an issue with delete-visicon-features because the 
;;;             :   simple check for an item being a chunk and then delaying
;;;             :   the real test until proc-display doesn't work if the item
;;;             :   is being removed because of removing the device upon reset.
;;;             : * Add a warning to build-string-feats about non-optimized not
;;;             :   being available (and use char to avoid a compiler warning).
;;; 2017.06.02 Dan
;;;             : * Wrap the evaluate-act-r-command calls in handle-evaluate-results.
;;; 2017.06.14 Dan
;;;             : * The remove-vision-interface function needs to return true for
;;;             :   remove-device to accept it.
;;; 2017.06.19 Dan
;;;             : * Adding in locks to protect access to parameters, the vis-loc-slots,
;;;             :   and the build-string-feats use of other-word-chars.
;;; 2017.06.22 Dan
;;;             : * Visicon-lock protects visicon-updates and visicon itself.
;;;             : * proc-display-lock protects scheduled-proc-display, proc-display-locks,
;;;             :   and blocked-proc-display.
;;; 2017.06.23 Dan
;;;             : * Added names for all the locks.
;;;             : * Made the proc-display-lock recursive for now because it's
;;;             :   easier than figuring out where they're overlapping.
;;; 2017.06.28 Dan
;;;             : * Protect access to new-span and finst-span in order to make
;;;             :   print-visicon safe.
;;;             : * Commented out object-present-p and removed icon-entry since
;;;             :   they aren't used (kept object-present-p for reference since
;;;             :   EMMA uses it and will need to update that at some point).
;;; 2017.08.09 Dan
;;;             : * Replaced a call to capture-model-output with printed-chunk-spec
;;;             :   instead.
;;;             : * Update the buffer-status functions to return the string.
;;;             : * Added printed-visicon to return the output string.
;;;             : * Update the reset and clear code to directly change slot values
;;;             :   instead of relying on the attn-module methods.
;;;             : * Also don't use set/clear-attended methods.
;;; 2017.08.10 Dan
;;;             : * Finish protecting the slots/methods currently accessed by
;;;             :   the code made available through the interface.
;;; 2017.08.11 Dan
;;;             : * Fixed a missing ) in find-current-locs-with-specs.
;;; 2017.08.23 Dan
;;;             : * Removed the dummy functions for external 'signals'.
;;; 2017.09.06 Dan
;;;             : * Make printed-visicon available remotely.
;;; 2017.09.13 Dan
;;;             : * Fixed a bug with how build-string-feats determined the width
;;;             :   of words.
;;; 2017.09.21 Dan
;;;             : * Schedule visicon-update signal directly without the dummy fn.
;;; 2017.12.19 Dan
;;;             : * Figured out why there was an extra random call relative to
;;;             :   previous versions -- find-best-feature was trying to determine
;;;             :   if a chunk had been modified using chunk-copied-from-fct but
;;;             :   the chunk it was copied from had been deleted.  Using both
;;;             :   return values from chunk-copied-from-fct works however.
;;;             : * And also need to set the merge function for visual-tstamp
;;;             :   to be the greater of the two stamps.
;;; 2018.01.02 Dan
;;;             : * Fixed a bug with the previous update because the tstamp merge
;;;             :   function was only set on the real-visual-value and not the
;;;             :   tstamp and was also setting the real-visual-value to the tstamp.
;;; 2018.01.26 Dan
;;;             : * Changed the buffer-status functions to omit the extra newline.
;;; 2018.01.30 Dan
;;;             : * Fixed a bug with convert-visicon-chunk-to-vis-loc when there
;;;             :   is no difference in the features.
;;; 2018.02.07 Dan
;;;             : * Fixed a bug with stuff-visloc-buffer because it was calling
;;;             :   purge-chunk instead of purge-chunk-fct.
;;; 2018.03.07 Dan
;;;             : * Reworking how vis-loc-slots are handled.  Now, it's a param
;;;             :   stored with the chunk itself, but since the vision module is
;;;             :   creating the chunks it isn't really usable yet.  Needs to be
;;;             :   provided with add-visicon-features somehow.
;;;             : * Don't require the vision module for the approach-width fn
;;;             :   and return nil if it can't be computed.
;;; 2018.03.08 Dan
;;;             : * Move the chunk-type and chunks for cursor items to the cursor
;;;             :   handling code.
;;; 2018.03.12 Dan
;;;             : * Chunk-to-visual-position can be used to convert an object or
;;;             :   loc chunk to an x,y,z list of the item's position and is 
;;;             :   available remotely.
;;; 2018.03.14 Dan
;;;             : * Update-attended-loc signal now gets model and loc.
;;; 2018.03.16 Dan
;;;             : * Build-string-feats now takes an x and y offset value which
;;;             :   is added to the location of each one.
;;; 2018.03.23 Dan
;;;             : * Need to record which features have been updated during
;;;             :   proc-display because need update-attended-loc to be able to
;;;             :   detect that.
;;; 2018.04.26 Dan [7.1]
;;;             : * Removed the test-feats parameter and class slot since it
;;;             :   isn't used anymore (maybe there'll be a use for it again
;;;             :   at some point, but not currently).
;;; 2018.04.27 Dan
;;;             : * Removed visual-object-type chunk parameter since it wasn't
;;;             :   being used and replaced get-obj-at-location with one that
;;;             :   doesn't use scale since it's unused anyway.
;;;             : * Protect all the tracking related slots with the marker-lock.
;;;             : * Automatically adjust size for updated visicon items.
;;;             : * Changed the trace details for assign-finst request to show
;;;             :   only the object or location and not the module.
;;; 2018.05.02 Dan 
;;;             : * Adjusted how valid-vis-loc-chunk works to improve performance.
;;;             :   The module is required and the loc-slot list can be passed in
;;;             :   as an optional.
;;; 2018.05.03 Dan [7.2]
;;;             : * Reworked how add-features-to-visicon gets called to make it
;;;             :   more like define-chunk, add-dm, etc.
;;;             : * Now store three chunks in the vision module for the features:
;;;             :   the visual-location and visual-object like before and now
;;;             :   have the "feature" chunk which is the visicon entry have the
;;;             :   searchable slot values so I don't need to juggle those things
;;;             :   when finding and creating the object.  The object definition
;;;             :   is currently stored in chunk-real-visual-value for creation
;;;             :   when necessary and the location chunk is in the chunk-feature-
;;;             :   values parameter.
;;;             : * Print-visicon now sorts the slots based on how many chunks
;;;             :   have a value so that the sparse slots are to the right.
;;; 2018.05.04 Dan
;;;             : * Renamed the chunk parameters from feature-values -> visual-loc
;;;             :   and real-visaul-value -> visual-obj-spec to make it easier
;;;             :   to understand.
;;;             : * Allow some 'parameters' in add-features-to-visicon so that
;;;             :   one can specify the :x-slot, :y-slot, :z-slot, and :width-fn
;;;             :   on a feature-by-feature basis.
;;; 2018.05.07 Dan
;;;             : * Fixed a bug in convert-loc-to-object with the warning output.
;;;             : * Fixed a bug in process-display with the updates because the
;;;             :   obj-spec needs to keep the nils, but flatten drops them...
;;; 2018.05.16 Dan
;;;             : * Add-visicon-features needs to add the default slot values from
;;;             :   the isas if there are any and not already specified.
;;; 2018.05.17 Dan
;;;             : * The set-visloc-default request does not need the type slot
;;;             :   since an isa in the request could be subtyped from any type
;;;             :   to include the defaults (the only reason for specifying a 
;;;             :   type at this point).
;;;             : * Find location needs to use the visicon-entry of the current
;;;             :   marker for mapping 'current' values since they may not be
;;;             :   present in the location chunk itself.
;;;             : * Ignore features with non-numeric values when using lowest
;;;             :   and highest instead of just failing.
;;; 2018.05.18 Dan
;;;             : * Actually check whether or not there are any matching features
;;;             :   before attempting to apply the lowest/highest constraints.
;;;             : * Adjust how the obj-spec is created for modifications since
;;;             :   the nil values when a list provided means don't apply.
;;;             : * Add-visicon-features needs to set the custom loc slot names
;;;             :   before computing default size.
;;; 2018.05.21 Dan
;;;             : * Create a device that can be installed to specify the custom
;;;             :   feature-to-object command.  The device is called object-
;;;             :   creator and the details must be the command.  Protect the
;;;             :   feature-to-object slot with the vision module's param lock.
;;; 2018.05.22 Dan
;;;             : * Removed the distance slot from the visual-object chunk-type.
;;;             : * Changed the parameters for remove-visual-finsts from keyword
;;;             :   to optional and added a remote version.
;;;             : * Added a remote set-visloc-default which does the string
;;;             :   to name parsing (the Lisp function itself doesn't).
;;;             : * Added remote set-visual-center-point and have the main
;;;             :   function return a list of x,y instead of a vector.
;;; 2018.05.23 Dan
;;;             : * Need to set the approach-width param for the object chunk
;;;             :   from the location/feature.
;;;             : * Wrap a handler-case around the approach-width computation
;;;             :   and use the default calculation if the custom width-fn 
;;;             :   doesn't return a number.
;;;             : * Modify-visicon-features needs to make sure that the object
;;;             :   value for a slot is maintained in the feature if only the
;;;             :   location value is changed.
;;;             : * Only allow the feature-id for deletion and not the loc-chunk
;;;             :   name (why did I have that option?).
;;; 2018.06.01 Dan
;;;             : * Param-lock was moved to the pm-module class.
;;; 2018.06.12 Dan
;;;             : * Only external commands should do string->name mapping.
;;; 2018.06.13 Dan
;;;             : * Fix build-string-feats since it was doing the double quoting
;;;             :   for strings.
;;; 2018.06.14 Dan
;;;             : * The visicon-entry and visual-loc chunk params now merge with
;;;             :   :second-if instead of :second.
;;; 2018.06.28 Dan
;;;             : * Fixed a bug with process-display because it would try to
;;;             :   transfer a finst for items marked new.
;;; 2018.07.26 Dan [7.3]
;;;             : * Don't handle tracked requests anymore.
;;; 2018.09.13 Dan [7.3.1]
;;;             : * Always generate the update-attended-loc signal regardless
;;;             :   of whether :show-focus is set.
;;;             : * Send the window updates as notifications instead of that being
;;;             :   handled by a monitor in the AGI -- it shouldn't need to know
;;;             :   about how any module works.
;;;             : * Make sure the marker-lock is held when calling set-current-
;;;             :   marker because it doesn't grab it directly.
;;; 2018.09.17 Dan
;;;             : * Don't need the declaim since it color->name was moved to
;;;             :   misc-utils.
;;; 2018.09.18 Dan
;;;             : * Notify the device when it's installed of the currently attended
;;;             :   location if there is one.
;;;             : * Generate a signal of "installing-visual-device" with the device
;;;             :   for all installs.
;;; 2018.09.19 Dan [7.3.2]
;;;             : * Adding a hook function parameter for overriding the default
;;;             :   encoding time :visual-encoding-hook.
;;;             :   Only allow one command to be set and warn if changed.
;;;             :   It will pass four parameters: the currently attended location 
;;;             :   as an x,y list (if there is one) or nil if there is not,
;;;             :   the new location as an x,y list, the feature chunk, and the
;;;             :   scale.  There are 3 return value options:
;;;             :    - nil
;;;             :         this means use the normal operations
;;;             :    - a number 
;;;             :         this represents a time in seconds after which the encoding-
;;;             :         complete will be scheduled
;;;             :    - anything else
;;;             :         this means do not schedule an encoding-complete and it
;;;             :         is up to the hook function's mechanisms to do so using
;;;             :         the schedule-encoding-complete command.
;;;             : * Added the schedule-encoding-complete command which takes one
;;;             :   parameter which is a delay time in seconds.  It schedules the
;;;             :   encoding-complete action to occur after that delay based on
;;;             :   the features during the last move-attention request.
;;; 2018.09.20 Dan
;;;             : * Fixed a bug with the device installation function.
;;;             : * Added the "visual-clear" signal which gets called if a clear
;;;             :   request is made.
;;;             : * Changed the signal to installing-vision-device since that's
;;;             :   the interface name.
;;; 2018.09.21 Dan
;;;             : * Update-attended-loc also needs to check the encoding-hook.
;;; 2018.09.24 Dan
;;;             : * Update-attended-loc can't hold the marker-lock when it calls
;;;             :   the encoding-hook fn since that hook might want to call
;;;             :   schedule-encoding-complete itself -- which also means that 
;;;             :   the last-attention-params need to be set before calling the
;;;             :   hook.
;;; 2018.10.25 Dan
;;;             : * Fixed a bug in the warning for approach-width.
;;; 2018.11.01 Dan [7.3.3]
;;;             : * Need to return the default width if approach-width fails 
;;;             :   because a value of nil will likely lead to errors elsewhere.
;;;             : * To get that, going to monitor the param.
;;; 2018.12.06 Dan 
;;;             : * Add a safety check to add-visicon-features to catch if a
;;;             :   slot spec has a list with more than two values.
;;; 2018.12.11 Dan
;;;             : * Work around for a bug in format with ccl 1.12-dev.
;;; 2019.02.22 Dan [8.0]
;;;             : * Fix a bug in within-move when items have different z coords.
;;;             :   Now it just calculates the angle without having to go to
;;;             :   pixels.  In the 2D space, the previous one assumed a 
;;;             :   calculation based on the points being equidistant from a
;;;             :   center "viewing" position, but now the assumption is that
;;;             :   it's the angle based on the "viewing axis" being through the
;;;             :   given point along the z axis.  Both are simplifications to 
;;;             :   avoid having to know head and eye geometry as well as head
;;;             :   position.
;;; 2019.03.22 Dan
;;;             : * Changed add-visicon-features to not use valid-vis-loc-chunk
;;;             :   since it can do the tests while processing other things and
;;;             :   save the chunk lookups.
;;; 2019.04.12 Dan 
;;;             : * Changed set-default-vis-loc-slots to a macro and made a -fct
;;;             :   and remote command for it, added some safety checks to attend-
;;;             :   visual-coordinate, and made chunk-to-visual-position return
;;;             :   just nil for 'bad' chunks.
;;; 2019.04.15 Dan
;;;             : * Depend on splice-into-position-des being destructive and 
;;;             :   don't setf with the returned value.
;;; 2019.05.21 Dan
;;;             : * When checking to unstuff the vis-loc buffer, don't unstuff
;;;             :   it if the chunk is being tracked.
;;; 2020.01.10 Dan [8.1]
;;;             : * Removed the #' and lambdas from the module interface 
;;;             :   functions since that's not allowed in the general system
;;;             :   now.
;;; 2020.03.31 Dan
;;;             : * Just changed some indenting for readability.
;;; 2020.06.01 Dan [9.0]
;;;             : * Moved the chunk-type and chunk definitions to the creation 
;;;             :   function instead of the reset function.
;;; 2020.07.06 Dan [9.1]
;;;             : * Got rid of the find-loc-failure and no-visual-object-found
;;;             :   dummy functions since can just schedule nil and have the
;;;             :   details shown in the trace.
;;; 2020.08.26 Dan
;;;             : * Removed the path for require-compiled since it's not needed
;;;             :   and results in warnings in SBCL.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GENERAL-PM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass vision-module (attn-module)
  ((visicon :accessor visicon :initarg :visicon :initform nil)
   (visicon-lock :accessor visicon-lock :initform (bt:make-recursive-lock "visicon-lock")) ;; protect visicon, visicon-updates, and finst-lst
   (proc-display-locks :accessor proc-display-locks :initform 0)
   (visicon-updates :accessor visicon-updates :initarg :visicon-updates :initform nil)
   (scheduled-proc-display :accessor scheduled-proc-display :initform nil)
   (blocked-proc-display :accessor blocked-proc-display :initform nil)
   (optimize-visual :accessor optimize-p :initarg :optimize-p :initform t)
   (move-attention-latency :accessor move-attn-latency :initarg :move-attn-latency :initform 85)
   (tracked-object :accessor tracked-obj :initarg :tracked-obj :initform nil)
   (tracked-object-last-location :accessor tracked-obj-lastloc :initarg :tracked-obj-lastloc :initform nil)
   (tracked-object-last-obj :accessor tracked-obj-last-obj :initarg :tracked-obj-last-obj :initform nil)
   (last-scale :accessor last-scale :initarg :last-scale :initform nil)
   (moving-attention :accessor moving-attention :initarg :moving-attention :initform nil)
   (move-allowance :accessor move-allowance :initarg :move-allowance :initform 0)
   (synthd-objs :accessor synthd-objs :initarg :synthd-objs :initform (make-hash-table)) 
   (found-locs :accessor found-locs :initarg :found-locs :initform (make-hash-table))
   (feature-sets :accessor feature-sets :initarg :feature-sets :initform (all-feature-sets))
   (active-cfs :accessor active-cfs :initarg :active-cfs :initform nil)
   (num-finst :accessor num-finst :initarg :num-finst :initform 4)
   (finst-lst :accessor finst-lst :initarg :finst-lst :initform nil)
   (finst-span :accessor finst-span :initarg :finst-span :initform 3.0)
   (new-span :accessor new-span :initarg :new-span :initform 0.5)
   (default-spec :accessor default-spec :initarg :default-spec :initform nil)  
   (visual-lock :accessor visual-lock :initform nil)
   (last-obj :accessor last-obj :initform nil) ;; added for use in tracking
   (auto-attend :accessor auto-attend :initform nil)
   (purge-visicon :accessor purge-visicon :initform t)
   (scene-change :accessor scene-change :initform nil)
   (change-threshold :accessor change-threshold :initform 0.25)
   (view-dist :accessor view-dist :initform nil)
   (ppi :accessor ppi :initform nil)
   (viewing-distance :accessor viewing-distance :initform nil)
   ;(feat-table :accessor feat-table :initform (make-hash-table :test 'equal))
   (current-cursor :accessor current-cursor :initform nil)
   (other-word-chars :accessor other-word-chars :initform nil)
   (center-point :accessor center-point :initform (vector 0 0))
   (vis-loc-slots :accessor vis-loc-slots :initform '(screen-x screen-y distance))
   (tracking-clear :accessor tracking-clear :initform nil)
   (clof :accessor clof :initform nil)
   (feature-to-object :accessor feature-to-object :initform nil)
   (show-focus-p :accessor show-focus-p :initform nil)
   (visicon-updated :accessor visicon-updated :initform nil)
   (encoding-hook :accessor encoding-hook)
   (last-attention-params :accessor last-attention-params :initform nil)
   (default-width :accessor default-width :initform nil)
   
   (marker-lock :accessor marker-lock :initform (bt:make-recursive-lock "visual-markers")) ;; clof, current-marker, currently-attended, last-obj, tracked-*, failure slots, and scene-change
   (text-parse-lock :accessor text-parse-lock :initform (bt:make-lock "visual-text-parse")) ;; other-word-chars
   (vis-loc-lock :accessor vis-loc-lock :initform (bt:make-lock "visual-vis-loc")) ;; vis-loc-slots and center-point
   (proc-display-lock :accessor proc-display-lock :initform (bt:make-recursive-lock "visual-proc-display")) ;; protect the proc-display-locks, scheduled-proc-display, and blocked-proc-display slots
   )
  (:default-initargs
      :name :VISION
    :version-string "9.0"))


(defun visual-location-slots (chunk vis-m)
  (or (chunk-vis-loc-slots chunk) (bt:with-lock-held ((vis-loc-lock vis-m)) (vis-loc-slots vis-m))))





(defun greater-tstamp (c1 c2) 
  (let ((a (chunk-visual-tstamp c1))
        (b (chunk-visual-tstamp c2)))
    (cond ((and (numberp a) (numberp b)) (if (> a b) a b))
          ((numberp a) a)
          ((numberp b) b)
          (t a))))


(defun greater-tstamp-value (c1 c2) 
  (let ((a (chunk-visual-tstamp c1))
        (b (chunk-visual-tstamp c2)))
    (cond ((and (numberp a) (numberp b)) (if (> a b) (chunk-visual-obj-spec c1) (chunk-visual-obj-spec c2)))
          ((numberp a) (chunk-visual-obj-spec c1))
          ((numberp b) (chunk-visual-obj-spec c2))
          (t (chunk-visual-obj-spec c1)))))

(suppress-extension-warnings)

(extend-chunks synth-feat :copy-function identity)

(extend-chunks visual-new-p)

(extend-chunks visicon-entry :copy-function identity :merge-function :second-if)

(extend-chunks visual-tstamp :copy-function identity :merge-function greater-tstamp)


(extend-chunks visual-obj-spec :copy-function identity :merge-function greater-tstamp-value)

(extend-chunks visual-loc :copy-function identity :merge-function :second-if)

(extend-chunks visual-approach-width-fn :copy-function identity)

(extend-chunks vis-loc-slots :copy-function identity)

(unsuppress-extension-warnings)


(defun convert-loc-to-object (vis-mod loc)
  (let* ((f->o (bt:with-recursive-lock-held ((param-lock vis-mod)) (feature-to-object vis-mod)))
         (result (when f->o (string->name (dispatch-apply f->o loc))))
         (c (if (and result (chunk-p-fct result))
                result
              (progn
                (when (and f->o result)
                  (print-warning "Invalid result ~s from object-creator command ~s.  Using default approach." result f->o))
                (car (define-chunks-fct (list (chunk-visual-obj-spec loc))))))))
    (if c
        (progn
          (setf (chunk-visicon-entry c) (chunk-visicon-entry loc))
          (setf (chunk-visual-approach-width-fn c) (chunk-visual-approach-width-fn loc))
          c)
      (print-warning "Failed to convert location ~s to a chunk using default mechanism." loc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create a device for specifying the feature-to-object
;;; command instead of making it yet another parameter.
;;;
;;; The device is called object-creator and the details must
;;; be the command to install.  Only one can be installed at
;;; a time, but currently only the interface can specify that
;;; not the device so have to deal with the details here (at
;;; least for now).  Instead of having vision notify the device,
;;; which would be the typical approach, this device just sets
;;; the slot in the vision module to have the function called.


(defun init-object-creator (device-list)
  (cond ((not (= 3 (length device-list)))
         (print-warning "Object-creator device requires a creation command as the third value."))
        ((not (local-or-remote-function-p (third device-list)))
         (print-warning "Object-creator device requires a valid function or command as the third value."))
        ((not (string-equal (first device-list) "vision"))
         (print-warning "Object-creator device can only be installed for the vision interface."))
        (t
         (let* ((vis-mod (get-module :vision))
                (current (bt:with-recursive-lock-held ((param-lock vis-mod)) (feature-to-object vis-mod))))
           (when current
             (remove-device (list "vision" "object-creator" current))
             (print-warning "Removing previous object-creator device because only one can be installed."))
           (bt:with-recursive-lock-held ((param-lock vis-mod)) (setf (feature-to-object vis-mod) (third device-list)))))))

(add-act-r-command "init-object-creator" 'init-object-creator "Internal command for installing an object-creator device. Do not call directly.")


(defun remove-object-creator (device-list)
  (let* ((vis-mod (get-module :vision))
         (current (bt:with-recursive-lock-held ((param-lock vis-mod)) (feature-to-object vis-mod))))
    (when current
      (if (equalp (third device-list) current)
          (bt:with-recursive-lock-held ((param-lock vis-mod)) 
            (setf (feature-to-object vis-mod) nil)
            t)  
        (print-warning "Attempt to remove an object-creator with command ~s which doesn't match currently installed ~s."
                       (third device-list) current)))))
  
(add-act-r-command "remove-object-creator" 'remove-object-creator "Internal command for installing an object-creator device. Do not call directly.")
                            
(define-device "object-creator" "init-object-creator" "remove-object-creator")



;; Since can't test the type use this function to determine if something
;; is valid as a visual-location chunk.  At this point the only constraints
;; are that it have x and y coordinates

(defun valid-vis-loc-chunk (chunk vis-m &optional slots)
  (unless slots (setf slots (visual-location-slots chunk vis-m)))
  (and (chunk-p-fct chunk)
       (fast-chunk-slot-value-fct chunk (first slots))
       (fast-chunk-slot-value-fct chunk (second slots))))

(defmacro set-default-vis-loc-slots (x y z)
  `(set-default-vis-loc-slots-fct ',x ',y ',z))

(defun set-default-vis-loc-slots-fct (x y z)
  (let ((vis-m (get-module :vision)))
    (if vis-m
        (if (every 'valid-slot-name (list x y z))
            (if (or (eq x y) (eq y z) (eq x z))
                (print-warning "Duplicate slot names provided for set-default-vis-loc-slots: ~s, ~s, ~s" x y z)
              (bt:with-lock-held ((vis-loc-lock vis-m)) (setf (vis-loc-slots vis-m) (list x y z))))
          (dolist (name (list x y z))
            (unless (valid-slot-name name)
              (print-warning "Slot ~s is not valid in call to set-default-vis-loc-slots." name))))
      (print-warning "No vision module available in call to set-default-vis-loc-slots."))))

(defun remote-set-default-vis-loc-slots-fct (x y z)
  (set-default-vis-loc-slots-fct (string->name x) (string->name y) (string->name z)))

(add-act-r-command "set-default-vis-loc-slots" 'remote-set-default-vis-loc-slots-fct "Change the names of the default location slots for visual locations. Params: x-slot y-slot z-slot.")

(defmethod set-current-marker ((vis-mod vision-module) marker &optional lof)
  (let ((old-clof (clof vis-mod)))
    (cond ((null marker)
           (setf (current-marker vis-mod) nil)
           (setf (clof vis-mod) nil))
          ((chunk-p-fct marker)
           (setf (clof vis-mod) (xyz-loc marker vis-mod))
           (setf (current-marker vis-mod) marker))
          (lof
           (setf (current-marker vis-mod) marker)
           (setf (clof vis-mod) lof))
          (t
           (print-warning "Invalid current-marker encountered in vision module: ~s" marker)
           (setf (current-marker vis-mod) nil)
           (setf (clof vis-mod) nil)
           (return-from set-current-marker)))
    (when (not (equalp old-clof (clof vis-mod))) 
      (evaluate-act-r-command "update-attended-loc" (current-model) (coerce (clof vis-mod) 'list))
      (awhen (bt:with-recursive-lock-held ((param-lock vis-mod)) (show-focus-p vis-mod))
             ;; send window handler messages
             (let ((update-list
                    (if (clof vis-mod)
                        (list "attention" (px (clof vis-mod)) (py (clof vis-mod)) (color->name it "red"))
                      (list "clearattention"))))
               ;; should keep track internally, but for now just rely upon the main installer
               (dolist (d (current-devices "vision"))
                 (notify-device d update-list)))))))


(add-act-r-command "update-attended-loc" nil "Signal called when visual attention moves which can be monitored. Should not be called. Params: model-name [xyz-list | nil]")


;; Vision module needs to notify the device about the attended location when it
;; is installed if the focus is being shown.


(defun visual-device-install (device)
  (let ((vis-mod (get-module :vision)))
    (awhen (bt:with-recursive-lock-held ((param-lock vis-mod)) (show-focus-p vis-mod))
           (when (bt:with-lock-held ((marker-lock vis-mod)) (clof vis-mod))
             (notify-device device (list "attention" (px (clof vis-mod)) (py (clof vis-mod)) (color->name it "red"))))))
  (dispatch-apply "installing-vision-device" device))
  
(add-act-r-command "installing-vision-device" nil "Signal that a new device is being installed for the vision interface.  Params: device")

(add-act-r-command "visual-device-install" 'visual-device-install "Notify a newly installed visual device of the attended location and generate installing-visual-device signal. Do not call.")
   
(define-interface "vision" "visual-device-install" nil)





(defmethod lock-vision ((vis-mod vision-module))
  "Place a lock on proc-display to prevent it from actually occuring"
  (bt:with-recursive-lock-held ((proc-display-lock vis-mod))
    (incf (proc-display-locks vis-mod))))

(defmethod unlock-vision ((vis-mod vision-module))
  "Remove one of the locks from proc-display and run it now if all locks are
   removed and there was a blocked call"
  (bt:with-recursive-lock-held ((proc-display-lock vis-mod))
    (unless (zerop (proc-display-locks vis-mod))
      (decf (proc-display-locks vis-mod)))
    
    (when (and (zerop (proc-display-locks vis-mod)) (blocked-proc-display vis-mod))
      (proc-display vis-mod))))


(defmethod schedule-proc-display ((vis-mod vision-module))
  (bt:with-recursive-lock-held ((proc-display-lock vis-mod))
    (unless (scheduled-proc-display vis-mod)
      (setf (scheduled-proc-display vis-mod) 
        (schedule-event-now 'proc-display :module :vision :destination :vision :priority -100 :output 'high :maintenance t)))))

(defmethod proc-display ((vis-mod vision-module))
  (bt:with-recursive-lock-held ((proc-display-lock vis-mod))
    (cond ((zerop (proc-display-locks vis-mod))
           (setf (scheduled-proc-display vis-mod) nil)
           (setf (blocked-proc-display vis-mod) nil)
           (process-display vis-mod))
          (t
           (setf (blocked-proc-display vis-mod) t)))))


(defun visicon-less-than (c1 xyz-c1 c2 xyz-c2)
  (or (< (px xyz-c1) (px xyz-c2))
      (and (= (px xyz-c1) (px xyz-c2))
           (< (py xyz-c1) (py xyz-c2)))
      (and (= (px xyz-c1) (px xyz-c2))
           (= (py xyz-c1) (py xyz-c2))
           (< (pz xyz-c1) (pz xyz-c2)))
      (and (= (px xyz-c1) (px xyz-c2))
           (= (py xyz-c1) (py xyz-c2))
           (= (pz xyz-c1) (pz xyz-c2))
           (string< (symbol-name c1) (symbol-name c2)))))


(defmethod insert-into-visicon ((vis-mod vision-module) chunk &optional xyz-loc)
  ;; keep visicon ordered by location at all times now
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (if (null (visicon vis-mod))
        (setf (visicon vis-mod) (list chunk))
      (progn
        (when (null xyz-loc)
          (setf xyz-loc (xyz-loc chunk vis-mod)))
        
        (let ((pos (position-if (lambda (c)
                                  (visicon-less-than chunk xyz-loc c (xyz-loc c vis-mod)))
                                (visicon vis-mod))))
          (cond ((null pos)
                 (push-last chunk (visicon vis-mod)))
                ((zerop pos)
                 (push chunk (visicon vis-mod)))
                (t
                 (splice-into-position-des (visicon vis-mod) pos chunk))))))))



(defmethod process-display ((vis-mod vision-module))
  "Build a new visicon and initiate any buffer stuffing that may result"
  
  (let ((deleted nil)
        (transfer-finsts nil)
        (updates nil)
        (tempicon nil)
        o
        (n 0)
        (d 0)
        (c nil))
    
        
    ;; Verify that they're all valid visicon items (chunks with first 2 coordinates)
    ;; and make sure that they've got size and distance values
    
    (bt:with-recursive-lock-held ((visicon-lock vis-mod))
      
      (setf (visicon-updated vis-mod) nil)
      
      (setf o (length (visicon vis-mod)))
      
      (dolist (x (visicon-updates vis-mod))
        (case (car x)
          (:add
           (let ((visicon-chunk (cdr x)))
             (setf (chunk-visual-tstamp visicon-chunk) (mp-time-ms))
             (setf (chunk-visual-new-p visicon-chunk) 'new)
             (push visicon-chunk tempicon)))
          
          (:delete
           (let ((visicon-chunk (cdr x)))
             
             (cond ((find visicon-chunk (visicon vis-mod))
                    (incf d)
                    (setf (visicon vis-mod) (remove visicon-chunk (visicon vis-mod)))
                    (when (eq t (feat-attended visicon-chunk vis-mod))
                      (push visicon-chunk transfer-finsts)))
                   ((find visicon-chunk tempicon)
                    (setf tempicon (remove visicon-chunk tempicon)))
                   (t 
                    (print-warning "Attempt to delete visicon feature ~s which is not in the visicon." visicon-chunk)))
             (push visicon-chunk deleted)))
          
          (:delete-all
           
           (dolist (y (visicon vis-mod))
             (incf d) 
             (push y deleted)
             (when (eq (feat-attended y vis-mod) t)
               (push y transfer-finsts)))
           
           (setf (visicon vis-mod) nil)
           (setf tempicon nil)
           (setf updates nil))
          
          (:update
           (push-last (cdr x) updates))))
      
      (setf (visicon-updates vis-mod) nil)
    
      (dolist (x updates)
        (let* ((visicon-chunk (first x))
               (loc-chunk (chunk-visual-loc visicon-chunk))
               (mods (rest x))
               (specs (do ((l mods (cddr l))
                           (r nil (push-last (cons (first l) (if (listp (second l)) (second l) (list (second l)))) r)))
                          ((null l) r)))
               (loc-slots (visual-location-slots visicon-chunk vis-mod))
               (loc-mods (mapcan (lambda (x) 
                                   (if (= (length x) 2)
                                       (list (first x) (second x))
                                     (when (second x)  ;; don't want nil second values when the length is 3
                                       (list (first x) (second x))))) 
                           specs)) 
               (obj-mods (mapcan (lambda (x) 
                                   (unless (find (first x) loc-slots)
                                     (if (= 3 (length x))                         ;; don't want nil values here
                                         (when (third x)
                                           (list (cons (first x) (third x))))
                                       (list (cons (first x) (second x))))))
                           specs))
               
               (obj-spec (do ((l (chunk-visual-obj-spec visicon-chunk) (cddr l))
                              (r nil (push-last (cons (first l) (second l)) r)))
                             ((null l) r)))
               (visicon-mods (mapcan (lambda (x) 
                                       (list (first x) (or (third x)
                                                           (and (= (length x) 3)
                                                                (cdr (assoc (first x) obj-spec)))
                                                           (second x)))) ;; don't want nil values here unless both nil
                               specs))
               (previous-pos (xyz-loc loc-chunk vis-mod)))
          
          (mod-chunk-fct visicon-chunk visicon-mods)
          (mod-chunk-fct loc-chunk loc-mods)
          
          ;; have to merge the object spec with mods directly since it's not a chunk
          
          (let ()
            
            (dolist (m obj-mods)
              (aif (assoc (car m) obj-spec)
                   (if (cdr m)
                       (setf (cdr it) (cdr m))
                     (setf obj-spec (remove it obj-spec)))
                   (push-last m obj-spec)))

            ;; flatten the conses keeping the nils...
            
            (setf (chunk-visual-obj-spec visicon-chunk) (do ((final-spec nil)
                                                             (feats obj-spec (rest feats)))
                                                            ((null feats) final-spec)
                                                          (push-last (caar feats) final-spec)
                                                          (push-last (cdar feats) final-spec))))
            
            
          ;; set default size and dist. if not already
          
          (let ((view-dist (bt:with-recursive-lock-held ((param-lock vis-mod)) (view-dist vis-mod))))                                                         
            (unless (numberp (fast-chunk-slot-value-fct loc-chunk (third loc-slots)))
              (set-chunk-slot-value-fct loc-chunk (third loc-slots) view-dist))
            (unless (numberp (fast-chunk-slot-value-fct visicon-chunk (third loc-slots)))
              (set-chunk-slot-value-fct visicon-chunk (third loc-slots) view-dist)))
                                            
          ;; always recompute the size to be safe
          (compute-vis-loc-size loc-chunk vis-mod)
          (compute-vis-loc-size visicon-chunk vis-mod)
          
          
          (when (find visicon-chunk (visicon vis-mod))
            
            (pushnew visicon-chunk (visicon-updated vis-mod))
            
            (pushnew visicon-chunk c) ;; only count changed once
            
            (let ((new-pos (xyz-loc loc-chunk vis-mod)))
              (unless (equalp previous-pos new-pos)
                (setf (visicon vis-mod) (remove visicon-chunk (visicon vis-mod)))
                (insert-into-visicon vis-mod visicon-chunk new-pos))))))
      
      (dolist (x tempicon)
        (insert-into-visicon vis-mod x))

      (dolist (x transfer-finsts)
        (awhen (find x (visicon vis-mod) :test 'equal-chunks-fct)
               (transfer-finst x it vis-mod))))
    
    (when (purge-visicon vis-mod)
      (dolist (x deleted)
        (when (chunk-p-fct x)
          (when (chunk-p-fct (chunk-visual-loc x))
            (purge-chunk-fct (chunk-visual-loc x)))
          (purge-chunk-fct x))))
    
    (let (tracked)
      (bt:with-lock-held ((marker-lock vis-mod))
        
        (when (tracked-obj vis-mod)
          (setf tracked t))
        
        ;; Compute the change value and determine if the flag should be
        ;; set at this time.
        
        (setf n (length tempicon))
        (setf c (length c))
        
        (let ((change-val (if (zerop (+ o n))
                              0
                            (* 1.0 (/ (+ d n c) (+ o n))))))
          (setf (scene-change vis-mod) (cons change-val (mp-time-ms)))))
      
      (when tracked
        (update-tracking-mth vis-mod t)))
    
    (update-new vis-mod)
    (check-finsts vis-mod)
    
    (update-attended-loc vis-mod)
    (stuff-visloc-buffer vis-mod)
    
    ;; a non-maintenance event to possibly trigger conflict-resolution
    ;; just incase a "non-change" (no buffer stuffing or other events were
    ;; generated) might trigger something
    
    (schedule-event-now "visicon-update" :module :vision :output 'medium)))


(add-act-r-command "visicon-update" nil "Indicate when the vision module updates the visicon for monitoring purposes. No params. Do not call directly.")


(defmethod visicon-chunks ((vis-mod vision-module) &optional sorted)
  (declare (ignore sorted))
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (visicon vis-mod)))


(defun loc-sort (i1 i2 vis-mod)
  (let* ((slots1 (visual-location-slots i1 vis-mod))
         (slots2 (visual-location-slots i2 vis-mod))
         (x1 (chunk-slot-value-fct i1 (first slots1)))
         (x2 (chunk-slot-value-fct i2 (first slots2)))
         (y1 (chunk-slot-value-fct i1 (second slots1)))
         (y2 (chunk-slot-value-fct i2 (second slots2))))
    (and (numberp x1) (numberp x2) (numberp y1) (numberp y2)
         (or
          (< x1 x2)
          (and (= x1 x2)
               (< y1 y2))
          (and (= x1 x2) (= y1 y2) (string< (symbol-name i1) (symbol-name i2)))))))



(defun xy-loc (chunk &optional vis-m)
  (unless vis-m (setf vis-m (get-module :vision)))
  (let ((slots (visual-location-slots chunk vis-m)))
    (vector (fast-chunk-slot-value-fct chunk (first slots))
            (fast-chunk-slot-value-fct chunk (second slots)))))

(defun xyz-loc (chunk &optional vis-m)
  (unless vis-m (setf vis-m (get-module :vision)))
  (let ((slots (visual-location-slots chunk vis-m)))
    (vector (fast-chunk-slot-value-fct chunk (first slots))
            (fast-chunk-slot-value-fct chunk (second slots))
            (fast-chunk-slot-value-fct chunk (third slots)))))


(defun chunk-to-visual-position (chunk)
  (verify-current-model
   "chunk-to-visual-position requires a current model."
   (when (chunk-p-fct chunk)
     (let* ((screen-pos (chunk-slot-value-fct chunk 'screen-pos))
            (pos-chunk (and (chunk-p-fct screen-pos) screen-pos))
            (loc (coerce (xyz-loc (or pos-chunk chunk)) 'list)))
       (when (every 'numberp loc) loc)))))

(defun external-chunk-to-visual-position (chunk)
  (chunk-to-visual-position (string->name chunk)))

(add-act-r-command "chunk-to-visual-position" 'external-chunk-to-visual-position "If a chunk represents a visual location or visual object return the x,y,z position. Params: chunk-name")

(defgeneric update-attended-loc (vis-mod)
  (:documentation  "If the attended location needs an update, update."))

(defmethod update-attended-loc ((vis-mod vision-module))
  (let (attended tracked moving marker clof scale)
    (bt:with-lock-held ((marker-lock vis-mod))
      (setf attended (currently-attended vis-mod))
      (setf tracked (tracked-obj vis-mod))
      (setf moving (moving-attention vis-mod))
      (setf marker (current-marker vis-mod))
      (setf clof (clof vis-mod))
      (setf scale (last-scale vis-mod))
      
      ;; set them here incase the hook calls
      ;; schedule-encoding-complete
      (setf (last-attention-params vis-mod) (list marker clof scale :requested nil)))
    
    
    ;; if we're tracking or moving around, ignore this 
    (when (or tracked moving (eq 'BUSY (exec-s vis-mod)))
      (return-from update-attended-loc nil))
    
    ;; when do we update?
    ;; [1] when we're looking at an object and it's gone
    ;; [2] when we're looking at nothing and something appears 
    ;; [3] NEW we're looking at an object and it's been changed
    ;;     (previously not possible)
    
    
    (let ((visicon-chunk (when (chunk-p-fct attended)
                           (chunk-visicon-entry attended))))
      (when (or (and attended
                     (not (find visicon-chunk (bt:with-recursive-lock-held ((visicon-lock vis-mod)) (visicon vis-mod)))))
                
                (and marker
                     (null attended)
                     (within-move vis-mod clof))
                
                (and attended
                     (find visicon-chunk (bt:with-recursive-lock-held ((visicon-lock vis-mod)) (visicon-updated vis-mod)))))
        
        ;; Change it now to avoid problems with simultaneous non-scheduled updates
        
        (change-state vis-mod :exec 'busy)
        
        ;; Still want to schedule the change so that it gets recorded properly
        ;; for module tracking purposes.
        
        (schedule-event-now 'change-state :params (list vis-mod :exec 'busy) :module :vision :output nil :priority :max)
        
        ;; need to check with the encoding-hook
        
        (let (hook override)
          (bt:with-recursive-lock-held ((param-lock vis-mod))
            (setf hook (encoding-hook vis-mod)))
          
          (when hook 
            (setf override (dispatch-apply hook (coerce clof 'list) (coerce clof 'list) marker scale)))
             
          (bt:with-lock-held ((marker-lock vis-mod))
            (cond ((null override)
                   (setf (last-attention-params vis-mod) nil)
                   (schedule-event-relative (randomize-time-ms (move-attn-latency vis-mod)) 'encoding-complete
                                            :time-in-ms t :destination :vision :module :vision 
                                            :params (list marker clof scale :requested nil)
                                            :output 'medium
                                            :details (concatenate 'string "Encoding-complete " (symbol-name marker) " "  (symbol-name scale))))
                ((numberp override)
                 (when (minusp override)
                   (print-warning "Encoding hook returned a negative encoding time ~f. Using 0 instead." override)
                   (setf override 0))
                 (setf (last-attention-params vis-mod) nil)
                 (schedule-event-relative (randomize-time-ms (safe-seconds->ms override 'move-attention)) 'encoding-complete
                                          :time-in-ms t :destination :vision :module :vision 
                                          :params (list marker clof scale :requested nil)
                                          :output 'medium
                                          :details (concatenate 'string "Encoding-complete " (symbol-name marker) " "  (symbol-name scale))))
                (t ;;  
                 ))))))))




(defmethod check-unstuff-buffer ((module vision-module) buffer chunk)
  (let ((current (buffer-read buffer)))
    (and
     current
     (multiple-value-bind (copy was-copy) (chunk-copied-from-fct current)
       (declare (ignore copy))
       (and was-copy                                     ;; unchanged
            (eq chunk (chunk-visicon-entry current))     ;; matches the stuffed entry
            (query-buffer buffer '(buffer unrequested))  ;; and was actually stuffed
            (let ((tracked (bt:with-lock-held ((marker-lock module))
                             (tracked-obj-lastloc module))))
              (or (null tracked)
                  (not (eq current tracked)))))))))


(defun convert-visicon-chunk-to-vis-loc (chunk)
  (aif (chunk-visual-loc chunk)
       (progn
         (setf (chunk-visual-tstamp it) (chunk-visual-tstamp chunk))
        it)
       chunk))

(defmethod stuff-visloc-buffer ((vis-mod vision-module))
  (let ((old (buffer-read 'visual-location)))
    (when (and (or (null old)
                   (and (overstuff-loc vis-mod)
                        ;; just check unmodified and stuffed
                        ;; since can't compare to visicon because
                        ;; the old feature may be gone now
                        (multiple-value-bind (x unmodified-copy) (chunk-copied-from-fct old) (declare (ignore x)) unmodified-copy)
                        (query-buffer 'visual-location '(buffer unrequested))))
               (zerop (bt:with-recursive-lock-held ((proc-display-lock vis-mod)) (proc-display-locks vis-mod)))
               (not (bt:with-lock-held ((marker-lock vis-mod)) (tracked-obj vis-mod))))
      (awhen (find-current-locs-with-spec vis-mod (default-spec vis-mod))
             (let* ((chunk (random-item (sort (objs-max-val it 'chunk-visual-tstamp) 'string< :key 'symbol-name)))
                    (vl-chunk (convert-visicon-chunk-to-vis-loc chunk))
                    (unstuff (bt:with-recursive-lock-held ((param-lock vis-mod)) (unstuff-loc vis-mod))))
               (if (and old (equal-chunks-fct old vl-chunk)) ;; don't stuff the same chunk again
                   (purge-chunk-fct vl-chunk)
                 
                 (progn
                   (if old
                       (schedule-overwrite-buffer-chunk 'visual-location vl-chunk 0 :time-in-ms t :module :vision :requested nil :priority 10)
                     (schedule-set-buffer-chunk 'visual-location vl-chunk 0 :time-in-ms t :module :vision :requested nil :priority 10))
                   
                   (lock-vision vis-mod)
                   
                   (schedule-event-now 'unlock-vision :module :vision :destination :vision :priority 9 :output nil :maintenance t)
                   ;(schedule-event-now 'delete-chunk-fct :module :vision :params (list vl-chunk) :priority 8 :output nil :maintenance t)
                   
                   
                   (when unstuff
                     (let ((delay (if (numberp unstuff) unstuff (bt:with-recursive-lock-held ((param-lock vis-mod)) (new-span vis-mod)))))
                       
                       (awhen (unstuff-event vis-mod)
                              (delete-event it))
                       (setf (unstuff-event vis-mod)
                         (schedule-event-relative delay 'unstuff-buffer :maintenance t :params (list 'visual-location chunk) 
                                                  :destination :vision :module :vision :output nil :time-in-ms t
                                                  :precondition 'check-unstuff-buffer)))))))))))


(defun test-attended (vm attended-spec chunk)
  "Assume it's a visual-location chunk, but could be either
   the key or value from the visicon or it could be an unrelated location chunk"
  
  ;; Let the unrelated chunk match to attended nil now
  ;; instead of failing all queries as was the case previously
  
  (let* ((visicon-value chunk)
         (value (third attended-spec))
         (result nil)
         (marker (feat-attended visicon-value vm)))
    
    (cond ((eq value 'new)
           (setf result (eq marker 'new)))
          ((null value)
           (setf result (or (eq marker 'new) (eq marker nil))))
          (t
           (setf result (eq marker t))))
    
    (if (eq (car attended-spec) '-)
        (not result)
      result)))

(defun matching-attended-chunks (vm attended-spec chunks)
  (remove-if-not (lambda (x) (test-attended vm attended-spec x)) chunks))

(defun set-visual-center-point (x y)
  (aif (get-module :vision)
       (if (and (numberp x) (numberp y))
           (bt:with-lock-held ((vis-loc-lock it)) (setf (center-point it) (vector x y)) (list x y))
         (print-warning "X and Y values must be numbers for set-visual-center-point."))
       (print-warning "No vision module available so cannot set center point.")))

(add-act-r-command "set-visual-center-point" 'set-visual-center-point "Set the default reference point about which the vision module determines clockwise and couterclockwise. Params: x y")

;;; Because screen y coordinates are "upside down" the results are backwards and
;;; nearest clockwise is max angle-between and nearest counterclockwise is min angle-between
;;; where p1 is the start point and p2 is the target computed relative to center.
;;; Could compensate for that by inverting the y calculations but doesn't seem necessary.

(defun 2-points->angle (center point)
  (atan (- (py point) (py center)) (- (px point) (px center))))

(defun angle-between (p1 p2 center)
  (let ((a (- (2-points->angle center p1) (2-points->angle center p2))))
    (if (minusp a)
        (+ (* 2 pi) a)
      a)))


(defmethod find-current-locs-with-spec ((vis-mod vision-module) spec)
  "Assume that it's a valid visual-location chunk-spec with at most 1
   attended slot specification one nearest spec and one center spec"
  (let (current clof)
    (bt:with-lock-held ((marker-lock vis-mod))
      (setf current (current-marker vis-mod)
        clof (clof vis-mod)))
    
    (when current
      (setf current (chunk-visicon-entry current)))
    
  (let* ((attended (first (chunk-spec-slot-spec spec :attended)))
         (slots (remove-if 'keywordp (chunk-spec-slot-spec spec) :key 'spec-slot-name))
         (current-valid (chunk-p-fct current))
         (current-slots (when (and current current-valid) (chunk-filled-slots-list-fct current)))
         (nearest (spec-slot-value (first (chunk-spec-slot-spec spec :nearest))))
         (min-max-tests nil))
    
    ;; Remap all current values to the current chunk
    
    (dolist (x slots)
      (when (eq (spec-slot-value x) 'current)
        (if (find (spec-slot-name x) current-slots)
            (setf (spec-slot-value x) (chunk-slot-value-fct current (spec-slot-name x)))
          (progn
            (if current-valid
                (print-warning "Current visual-location does not have a slot named ~S so it is ignored in the request." (spec-slot-name x))
              (print-warning "There is no currently attended location.  So, request specifying ~S as current is being ignored." (spec-slot-name x)))
            (setf slots (remove x slots))))))
    
    ;; Remove all tests for highest and lowest for later
    
    (dolist (x slots)
      (when (or (eq (spec-slot-value x) 'lowest)
                (eq (spec-slot-value x) 'highest))
        (push-last x min-max-tests)
        (setf slots (remove x slots))))
    
    ;; update the finsts and new markers if attended is needed
    
    (when attended
      (update-new vis-mod)
      (check-finsts vis-mod))
    
    ;; find the chunks that match
    
    (let ((possible-chunks (if attended
                               (matching-attended-chunks vis-mod attended (visicon-chunks vis-mod t))
                             (visicon-chunks vis-mod t)))
          )
      
      
      (let ((matching-chunks (find-matching-chunks (slot-specs-to-chunk-spec slots)
                                                   :chunks possible-chunks :variable-char #\&)))
        ;; apply all of the lowest/highest constraints
        ;; in the order provided
        
        (when matching-chunks
          (dolist (x min-max-tests)
            (let ((value :fail)
                  (op (spec-slot-op x))
                  (slot (spec-slot-name x))
                  (test (spec-slot-value x)))
              
              ;; find the min/max value
              (dolist (y matching-chunks)
                (let ((cur-val (fast-chunk-slot-value-fct y slot)))
                  (when (numberp cur-val)
                    ;(setf value :fail)
                    ;(print-warning "Cannot apply ~S constraint because not all chunks have a numerical value." x)
                    ;(return))
                    (when (or (eq value :fail) 
                              (and (eq test 'lowest)
                                   (< cur-val value))
                              (and (eq test 'highest)
                                   (> cur-val value)))
                      (setf value cur-val)))))
              
              (if (eq value :fail)
                  (print-warning "Cannot apply ~S constraint because no chunks which match the other features have a numerical value." x)
                (setf matching-chunks (find-matching-chunks (define-chunk-spec-fct (list op slot value)) :chunks matching-chunks))))))
        
        ;; if there's a nearest constraint then
        ;; apply that filter now
        
        (when (and nearest matching-chunks)
          (cond ((find nearest '(current-x current-y current-distance))
                 (let* ((view-dist (bt:with-recursive-lock-held ((param-lock vis-mod)) (view-dist vis-mod)))
                        (current-val (aif clof 
                                          (case nearest 
                                            (current-x (px clof)) 
                                            (current-y (py clof)) 
                                            (current-distance (pz clof)))
                                          (progn 
                                            (model-warning "No location has yet been attended so current is assumed to be at 0,0,~d." view-dist)
                                            (case nearest (current-x 0) (current-y 0) (current-distance view-dist))))))
                   ;; find those matching min value
                   
                   (setf matching-chunks (objs-min-val matching-chunks 
                                                       (lambda (x) 
                                                         (let ((slots (visual-location-slots x vis-mod)))
                                                           (abs (- current-val (chunk-slot-value-fct x
                                                                                                     (case nearest 
                                                                                                       (current-x (first slots)) 
                                                                                                       (current-y (second slots)) 
                                                                                                       (current-distance (third slots))))))))))))
                
                ((find nearest '(clockwise counterclockwise))
                 (let* ((center-spec (spec-slot-value (first (chunk-spec-slot-spec spec :center))))
                        (center (cond ((or (null center-spec)
                                           (not (chunk-p-fct center-spec)))
                                       (bt:with-lock-held ((vis-loc-lock vis-mod)) (center-point vis-mod)))
                                      ((valid-vis-loc-chunk center-spec vis-mod)
                                       (xy-loc center-spec vis-mod))
                                      ((and (chunk-slot-value-fct center-spec 'screen-pos)
                                            (valid-vis-loc-chunk (chunk-slot-value-fct center-spec 'screen-pos) vis-mod))
                                       (xy-loc (chunk-slot-value-fct center-spec 'screen-pos) vis-mod))
                                      (t
                                       (bt:with-lock-held ((vis-loc-lock vis-mod)) (center-point vis-mod)))))
                        (current-point (aif clof 
                                            it
                                            (progn 
                                              (model-warning "No location has yet been attended so current is assumed to be at 0,0.")
                                              (vector 0 0)))))
                   
                   (setf matching-chunks (if (eq nearest 'clockwise)
                                             (objs-max-val matching-chunks 
                                                           (lambda (x) 
                                                             (angle-between current-point (xy-loc x vis-mod) center)))
                                           (objs-min-val matching-chunks 
                                                         (lambda (x) 
                                                           (angle-between current-point (xy-loc x vis-mod) center)))))))     
                ((eq nearest 'current)
                 (let ((nearest-coords (aif clof 
                                            it
                                            (let ((view-dist (bt:with-recursive-lock-held ((param-lock vis-mod)) (view-dist vis-mod))))
                                                   (model-warning "No location has yet been attended so current is assumed to be at 0,0,~d." view-dist)
                                                   (vector 0 0 view-dist)))))
                   
                   (setf matching-chunks (objs-min-val matching-chunks 
                                                       (lambda (x) 
                                                         (dist (xyz-loc x vis-mod) nearest-coords))))))
                
                ((valid-vis-loc-chunk nearest vis-mod)
                 (let* ((coord-slots (visual-location-slots nearest vis-mod))
                        (nearest-coords (if (numberp (chunk-slot-value-fct nearest (third coord-slots)))
                                            (xyz-loc nearest vis-mod)
                                          (vector (chunk-slot-value-fct nearest (first coord-slots))
                                                  (chunk-slot-value-fct nearest (second coord-slots))
                                                  (bt:with-recursive-lock-held ((param-lock vis-mod)) (view-dist vis-mod))))))
                   
                   (setf matching-chunks (objs-min-val matching-chunks 
                                                       (lambda (x) 
                                                         (dist (xyz-loc x vis-mod) nearest-coords))))))
                
                (t
                 (print-warning "Nearest test in a visual-location request must be current, current-x, current-y, clockwise, counterclockwise, or a chunk with valid coordinates.")
                 (print-warning "Ignoring nearest request for ~S." nearest))))
        
        matching-chunks)))))





;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Supporting NEW tags


(defun tstamp-elapsed (vis-loc)
  (- (mp-time-ms) (chunk-visual-tstamp vis-loc)))

(defun checknew (vis-loc vis-mod)
  (when (and (eq (chunk-visual-new-p vis-loc) 'NEW)
             (> (tstamp-elapsed vis-loc) (bt:with-recursive-lock-held ((param-lock vis-mod)) (new-span vis-mod))))
    (setf (chunk-visual-new-p vis-loc) nil))
  vis-loc)

(defmethod update-new ((vis-mod vision-module))
  (bt:with-recursive-lock-held ((visicon-lock vis-mod)) (mapc (lambda (vl) (checknew vl vis-mod)) (visicon vis-mod))))


;;;; FInsts

(defclass finst ()
  ((id :accessor id :initarg :id :initform nil) ;; the visicon key 
   (tstamp :accessor tstamp :initarg :tstamp :initform 0.0)
   (synthed-from :accessor synthed-from :initarg :synthed-from :initform nil)))

(defgeneric check-finsts (vis-mod)
  (:documentation  "Update finsts against what's on the display."))

(defmethod check-finsts ((vis-mod vision-module))
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (setf (finst-lst vis-mod)
      (delete-if (lambda (f) 
                   (or 
                    ; not quite sure why the first one is like that, but neither useful currently
                    ;(and (not (synthed-from f))
                    ;     (find (id f) (visicon vis-mod)))
                    ;(and (synthed-from f)
                    ;     (not (some (lambda (x) (find x (visicon vis-mod))) (synthed-from f))))
                    
                    ; instead just check whether it's in the visicon now and worry about
                    ; synthed feats at this point
                    (not (find (id f) (visicon vis-mod)))
                    (> (- (mp-time-ms) (tstamp f))
                       (bt:with-recursive-lock-held ((param-lock vis-mod)) (finst-span vis-mod)))))
                 (finst-lst vis-mod)))))


(defgeneric feat-attended (feat vis-mod)
  (:documentation  "Return the attended status of a visicon feature object."))

(defmethod feat-attended (loc vis-mod)
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (if (find loc (finst-lst vis-mod) :key (lambda (x) (cons (id x) (synthed-from x))) :test (lambda (item list) (member item list :test 'equal)))
        t
      (when (find loc (visicon vis-mod))
        (if (eq (chunk-visual-new-p loc) 'NEW)
            'NEW
          nil)))))

(defun add-finst (vis-mod loc-or-obj) 
  (let* ((current (chunk-visicon-entry loc-or-obj)))
    (if current
        (bt:with-recursive-lock-held ((visicon-lock vis-mod))
          (aif (find current (finst-lst vis-mod) :key 'id :test 'equal)
               (setf (tstamp it) (mp-time-ms))
               
               (push
                (make-instance 'finst :id current :tstamp (mp-time-ms)
                  :synthed-from (chunk-synth-feat loc-or-obj))
                (finst-lst vis-mod)))
          
          (aif (chunk-synth-feat loc-or-obj)
               (dolist (x it)
                 ;; This is broken, and will need to be fixed when synthed feats come back
                 (setf (chunk-visual-new-p (gethash x (visicon vis-mod))) nil)
                 )
               (setf (chunk-visual-new-p current) nil))
          
          (when (> (length (finst-lst vis-mod)) (num-finst vis-mod))
            (sort-finsts vis-mod)
            (pop (finst-lst vis-mod))))
      (model-warning "~S does not name an object or feature in the current visicon. No finst created." loc-or-obj))))

(defun transfer-finst (from to vis-mod)
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (let ((f (find from (finst-lst vis-mod) :key (lambda (x) (cons (id x) (synthed-from x))) :test (lambda (item list) (member item list :test 'equal)))))
      (setf (chunk-visual-tstamp to) (tstamp f))
      (setf (chunk-visual-new-p to) (chunk-visual-new-p from))
      
      ;; synthed-from isn't being handled yet...
      (when (eq from (id f))
        (setf (id f) to)))))


(defun remove-finst (vis-mod loc-or-obj) 
  (let ((name (chunk-visicon-entry loc-or-obj)))
    (bt:with-recursive-lock-held ((visicon-lock vis-mod))
      (setf (finst-lst vis-mod) (remove name (finst-lst vis-mod) :key 'id :test 'equal))
      (setf (finst-lst vis-mod) (remove-if (lambda (x) (find name (synthed-from x) :test 'equal)) (finst-lst vis-mod))))))


(defgeneric assign-finst (vis-mod &key object location)
  (:documentation "Assign a finst to an object or location."))

(defmethod assign-finst ((vm vision-module) &key object location)
  
  (if (and (null object) (null location))
      (print-warning "ASSIGN-FINST called without a valid object or location")
    (add-finst vm (if object object location))))


(defgeneric sort-finsts (vis-mod)
  (:documentation  "Sort finsts according to time stamp."))

(defmethod sort-finsts ((vis-mod vision-module))
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (setf (finst-lst vis-mod) (sort (finst-lst vis-mod) '< :key 'tstamp))))





;;; WITHIN-MOVE      [Method]
;;; Date        : 99.03.29
;;; Description : Simply walk the icon and accumulate features that are within
;;;             : the movement tolerance.

;;; 2014.05.21
;;; The assumption with how this was is that the viewing vector was perpendicular
;;; to the viewing surface because the move distance was a circle about the target
;;; point.  Adding the distance dimension into the calculation assumes the same thing --
;;; the viewing vector is along the distance-axis through the xy-loc of the item.
;;; Then the items that match are those which fall within the cone defined by the 
;;; move-allowance angle.  It's a little ugly, but avoids having to determine the
;;; head position and deal with the fact that the screen is a plane (not all items
;;; are really at the same distance) and other real 3d stuff...

;;; 2019.02.22
;;; The previous calculation didn't take z into account correctly.  There's no need
;;; to go to pixels because it's just the angle that we care about.

(defgeneric within-move (vis-mod xyz-loc)
  (:documentation "Return a list of icon feature within the move allowance of loc."))

(defmethod within-move ((vis-mod vision-module) xyz-loc)
  (let ((accum nil)
        )
    (bt:with-recursive-lock-held ((visicon-lock vis-mod))
      (dolist (value (visicon vis-mod))
        (when (or (and (= (move-allowance vis-mod) 0)
                       (let ((coord-slots (visual-location-slots value vis-mod)))
                         (and (= (px xyz-loc) (fast-chunk-slot-value-fct value (first coord-slots))) 
                              (= (py xyz-loc) (fast-chunk-slot-value-fct value (second coord-slots)))
                              (= (pz xyz-loc) (fast-chunk-slot-value-fct value (third coord-slots))))))
                  (and (> (move-allowance vis-mod) 0)
                       (>= (move-allowance vis-mod) (angle-between-vectors xyz-loc (xyz-loc value vis-mod)))))
          (push value accum))))
      accum))

;; assume that the current viewing angle is along the z axis through the 
;; initial point, v1, to simplify things, and then can consider the 
;; first angle as 0,0,1 and translate the other accordingly.

(defun angle-between-vectors (v1 v2)
  (rad->deg (acos (/ (pz v2) (sqrt (+ (expt (- (px v2) (px v1)) 2)
                                      (expt (- (py v2) (py v1)) 2)
                                      (expt (pz v2) 2)))))))


(defmethod feat-match-xyz (feat-list xyz-loc vis-m)
  (let ((outlis nil)
        (x (px xyz-loc))
        (y (py xyz-loc))
        (z (pz xyz-loc)))
    (dolist (chunk feat-list)
      (let ((coord-slots (visual-location-slots chunk vis-m)))
        (when (and (= x (fast-chunk-slot-value-fct chunk (first coord-slots))) 
                   (= y (fast-chunk-slot-value-fct chunk (second coord-slots)))
                   (= z (fast-chunk-slot-value-fct chunk (third coord-slots))))
          (push chunk outlis))))
      outlis))


;;; ENCODING-COMPLETE      [Method]
;;; Description : Several things to do when focusing attention on a location.

(defgeneric encoding-complete (vis-mod loc-dmo position scale &key requested)
  (:documentation "When MOVE-ATTENTION completes, focus on a place with this."))

(defmethod encoding-complete ((vis-mod vision-module) loc position scale &key (requested t))
  
  (setf (moving-attention vis-mod) nil)
  (change-state vis-mod :exec 'free :proc 'free)
  
  (let ((return-obj (get-obj-at-location vis-mod loc position scale)))
    
      
      (if return-obj
          (progn
            (bt:with-lock-held ((marker-lock vis-mod))
              (set-current-marker vis-mod loc position)
              (setf (currently-attended vis-mod) return-obj))
            (attend-to-object vis-mod return-obj :requested requested)
            return-obj)
        
        (bt:with-lock-held ((marker-lock vis-mod))
          (set-current-marker vis-mod loc position)
          (setf (currently-attended vis-mod) nil)
          (setf (last-obj vis-mod) nil)
          (set-buffer-failure 'visual :ignore-if-full t :requested requested)
          (setf (attend-failure vis-mod) t)
          
          (schedule-event-now nil :maintenance t :module :vision :output 'medium :details "No visual-object found")
          
          nil))))




(defmethod attend-to-object ((vis-mod vision-module) obj &key (requested t))
  
  ;;; put the chunk in the buffer
  
  (schedule-set-buffer-chunk 'visual obj 0 :time-in-ms t :module :vision :priority 10 :requested requested)
  
  ;; record the object for tracking purposes
  
  (bt:with-lock-held ((marker-lock vis-mod))
    (setf (last-obj vis-mod) obj))
  
  ;; update the time-stamp on the finst if it's already attended or
  ;; add a new finst if it's not
  
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (aif (member obj (finst-lst vis-mod) :key 'id :test 'equal)
         
         (setf (tstamp (first it)) (mp-time-ms))
         
         (add-finst vis-mod obj))))


(defgeneric get-obj-at-location (vis-mod loc xyz-loc scale)
  (:documentation  "Given a location and a scale, return a chunk representing what's there."))

#|

Ignoring all the scale details for now...

(defmethod get-obj-at-location ((vis-mod vision-module) loc xyz-loc scale)
  (cond ((eq scale 'PHRASE)
         (get-phrase-at vis-mod loc))
        ((and (eq scale 'WORD) (not (bt:with-recursive-lock-held ((param-lock vis-mod)) (optimize-p vis-mod))))
         (get-word-at-noopt vis-mod loc))
        (t
         (let ((feat-lis (within-move vis-mod xyz-loc)))
           (when (eq scale 'WORD)
             (setf feat-lis (text-feats feat-lis)))
           (when feat-lis
             (featlis-to-focus vis-mod loc xyz-loc feat-lis))))))
|#

(defmethod get-obj-at-location ((vis-mod vision-module) loc xyz-loc scale)
  (declare (ignore scale))
  (let ((feat-lis (within-move vis-mod xyz-loc)))
    (when feat-lis
      (featlis-to-focus vis-mod loc xyz-loc feat-lis))))



(defun text-feats (feat-lst)
  "Given a list, return only those features which are TEXT features."
  (remove-if (lambda (f) (not (eq (chunk-slot-value-fct f 'kind) 'text))) feat-lst))


;;; FEATLIS-TO-FOCUS      [Method]

(defgeneric featlis-to-focus (vis-mod loc xyz-loc feat-lis)
  (:documentation  "Given the source location, the attended position, and a list of features within the movement tolerance of that position, return the chunk that should be the focus."))


(defmethod featlis-to-focus ((vis-mod vision-module) loc xyz-loc feat-lis)
  (let* ((best-feature (find-best-feature vis-mod feat-lis 
                                          (if (chunk-p-fct loc)
                                              loc
                                            (car (define-chunks-fct (list (mapcan (lambda (x y) (list x y)) '(screen-x screen-y distance) (coerce xyz-loc 'list))))))
                                          xyz-loc))
         
         
         (object ;; unlike older systems this must return a single chunk
          ;; not a list of everything that happens to be co-located
          (if (bt:with-recursive-lock-held ((param-lock vis-mod)) (optimize-p vis-mod))
              ;; just convert the best feature
              (featlis-to-object-chunks vis-mod (list best-feature))
            ;; otherwise there could be char-primitives that need to be synthisized into a character (or word?)
            ;; should make sure the best is char-primitive as well before doing that...
            (featlis-to-object-chunks vis-mod (feat-match-xyz feat-lis (xyz-loc best-feature vis-mod) vis-mod)))))
    
    ;; don't mark everything just the one that's being returned   (dolist (obj dmo-lis)
    
    (when object
      (set-chunk-slot-value-fct object 'screen-pos
                                (if (chunk-p-fct loc)
                                    loc ;; use the loc provided regardless
                                  ;;;  Otherwise use the cannonical loc for that item
                                  (chunk-visual-loc best-feature))))
    object))



;;; FEATLIS-TO-chunks      [Method]
;;; Date        : 99.03.29
;;; Description : Actually, some of the features could be CHAR-PRIMITIVE 
;;;             : features, in which case they're part of characters.  Save 
;;;             : those out and make a character out of 'em.

(defgeneric featlis-to-object-chunks (vis-mod feat-lis)
  (:documentation  "Given a list of features, make a chunk for each."))

(defmethod featlis-to-object-chunks ((vis-mod vision-module) (feat-lis list))
  (let ( ;; (primitive-feats nil)
        (chunk-lis nil))
    (dolist (feat feat-lis)
#|    ;; If it's a char primitive, save it, otherwise just convert it.
      (if (eq (chunk-slot-value-fct feat 'kind) 'char-primitive)
          (push feat primitive-feats)
|#  
      
      (let ((obj (convert-loc-to-object vis-mod feat)))
        (when (chunk-p-fct obj)
          (setf (chunk-visicon-entry obj) (chunk-visicon-entry feat))
          (push obj chunk-lis))))
#|    )
      (when primitive-feats ;; need to double-check there wasn't anything else -- what about a button with the oval and the primitives...
        (push (synthesize-letter vis-mod primitive-feats) chunk-lis))
|#
    ;; should only be one...
    (first chunk-lis)))




;;; FIND-BEST-FEATURE      [Method]
;;; Date        : 99.03.29
;;; Description : The "best" feature is the one with the same ID as the spec. 
;;;             : The next best feature is one that matches the spec and is
;;;             : nearest to that location.  Failing any matches to the spec,
;;;             : then return the nearest feature.


(defun find-best-feature (vis-mod feat-lis loc xyz)
  (let ((entry (chunk-visicon-entry loc)))
    (if (and entry (chunk-p-fct entry) (find entry feat-lis) (some 'identity (multiple-value-list (chunk-copied-from-fct loc))) (= (chunk-visual-tstamp loc) (chunk-visual-tstamp entry)))
        ;; If the chunk has an entry from the viscion, that entry is in the list of features, the chunk
        ;; itself hasn't been modified, and it's got the same timestamp as the visicon chunk
        ;; (the feature hasn't been changed either) then that entry is the best possible feature.
        entry
      ;; otherwise just find the feature among those within range which
      ;; best match the location chunk provided keeping in mind that the
      ;; visicon chunks hold the 'object' values while the provided chunk
      ;; should have the 'feature' values.
      (let ((matches (let* ((spec nil)
                            (chunk (if (chunk-p-fct (chunk-visicon-entry loc)) (chunk-visicon-entry loc) loc))
                            (positions (visual-location-slots chunk vis-mod))) ;; skip location of the target

                       (dolist (s (chunk-filled-slots-list-fct chunk))
                         (unless (find s positions)
                           (push (cons s (fast-chunk-slot-value-fct chunk s)) spec)))
                       
                       
                       (objs-max-val feat-lis (lambda (x) 
                                                (let ((c 0)) 
                                                  (dolist (s spec c)
                                                    (when (chunk-slot-equal (cdr s) (chunk-slot-value-fct x (car s)))
                                                      (incf c)))))))))
        (if matches
            (random-item (nearest-feat vis-mod matches xyz))
          (random-item (nearest-feat vis-mod feat-lis xyz)))))))


(defun nearest-feat (vis-mod feat-list xyz-loc)
  "Returns list of features nearest to a given location"
  (unless (vectorp xyz-loc)
    (setf xyz-loc (xyz-loc xyz-loc vis-mod)))
  (when feat-list
    (objs-min-val feat-list (lambda (x) (dist (xyz-loc x vis-mod) xyz-loc)))))


;;; UPDATE-TRACKING-MTH      [Method]
;;; Date        : 97.05.15
;;; Description : Updating is kind of a pain.  First, if the tracked object
;;;             : hasn't moved, then do nothing.  If it has moved, however,
;;;             : there's a lot of bookkeeping to be done:

#|

Additional ACT-R 6 mechanism to properly deal with buffer issues.

Whenever there's a change to the display the buffers will be updated as follows:

   First, if the build-features-for returns nil then assume
   that the tracked object is out of sight now.  As with an
   encoding failure, clear visual, set the error state, and
   stop the tracking.

  If both buffers are empty:

   A new location is created and the object is modified
   to have that as its screen-pos.  Both chunks (the
   new loc. and the object) are stuffed into the buffers.

  If vis-loc empty, visual holds the tracked item:

   Create a new location chunk and stuff it into
   the visual-location buffer.  Modify the chunk in the
   visual buffer to have that screen-pos.

  vis-loc holds tracked item's loc. and visual is empty:

   Modify the chunk in the visual-location buffer with
   the new info and put the object back into the visual
   buffer (it shouldn't have to be modified at all since
   its loc. is in the vis-loc buffer).

  both buffers hold the appropriate chunks:

   Modify the chunk in the visual-location buffer,
   no other changes necessary.

  If either buffer holds a chunk that isn't related
  to the tracked item:

   Make the appropriate updates to the underlying chunks
   as needed (modify the chunk in the buffer if it's
   the tracked one or create/modify the internal one)
   but don't overwrite a non-tracked chunk in a buffer.
|#


(defgeneric start-tracking (vis-mod)
  (:documentation  "Begin tracking currently attended object"))

(defmethod start-tracking ((vis-mod vision-module))
  (let* ((current (bt:with-lock-held ((marker-lock vis-mod)) (currently-attended vis-mod)))
         (current-visicon-entry (when (chunk-p-fct current) (chunk-visicon-entry current))))
    (bt:with-recursive-lock-held ((visicon-lock vis-mod))
      (cond ((null current)
             (print-warning "Request to track object but nothing is currently being attended."))
            ((null current-visicon-entry)
             (print-warning "Last attended object does not have a visicon entry and cannot be tracked."))
            ((not (find current-visicon-entry (visicon vis-mod)))
             (print-warning "Last attended object no longer in the visicon and cannot be tracked."))
            (t 
             (change-state vis-mod :exec 'BUSY)
             (bt:with-lock-held ((marker-lock vis-mod))
               (setf (tracked-obj vis-mod) current-visicon-entry)
               (setf (tracked-obj-lastloc vis-mod) nil)  ;; don't assume anything about vis-loc buffer at this point
               (let ((vis-obj (buffer-read 'visual))) ;; should always be empty since the request clears it but
                 ;; if not record it for later checking
                 (if (and vis-obj (eq (chunk-copied-from-fct vis-obj) (last-obj vis-mod)))
                     (setf (tracked-obj-last-obj vis-mod) vis-obj)
                   (setf (tracked-obj-last-obj vis-mod) nil))))
             
             (update-tracking-mth vis-mod)
        
             (tracked-obj vis-mod))))))


;;; When tracking it may be necessary to get the name of
;;; the chunk from the vis-loc buffer to set the internal
;;; information correctly.  This method does that, but
;;; it must be scheduled approproately to ensure the right
;;; chunk gets recorded.  
;;; It could be possible for something else to intrude and
;;; mess this up, but the default mechanisms shouldn't 
;;; result in problems.

(defmethod update-tracking-loc-chunk ((vis-mod vision-module) &optional (modify nil))
  (let ((vis-loc (buffer-read 'visual-location)))
    
    (bt:with-lock-held ((marker-lock vis-mod))
      (setf (tracked-obj-lastloc vis-mod) vis-loc)
      (setf (current-marker vis-mod) vis-loc))
    
    (when modify 
      (mod-buffer-chunk 'visual (list 'screen-pos vis-loc)))))

;;; Record the visual object chunk placed into the buffer
;;; for later comparisons when needed

(defmethod update-tracking-obj-chunk ((vis-mod vision-module))
  (bt:with-lock-held ((marker-lock vis-mod))
    (setf (tracked-obj-last-obj vis-mod) (buffer-read 'visual))))


(defgeneric update-tracking-mth (vis-mod &optional from-proc-display)
  (:documentation  "Update the state of a tracked object"))

(defmethod update-tracking-mth ((vis-mod vision-module) &optional from-proc-display)
  
  (flet ((tracking-failed ()
                          (schedule-clear-buffer 'visual 0 :time-in-ms t :module :vision :output 'high :priority 13)
                          (change-state vis-mod :exec 'free :proc 'free)
                          (bt:with-lock-held ((marker-lock vis-mod))
                            (setf (currently-attended vis-mod) nil)
                            (setf (attend-failure vis-mod) t)
                            (setf (tracked-obj vis-mod) nil)
                            (setf (last-obj vis-mod) nil)
                          
                            (when (tracking-clear vis-mod)
                              (set-current-marker vis-mod nil)))
                          
                          (set-buffer-failure 'visual :ignore-if-full t)
                          (return-from update-tracking-mth nil)))
    
    (bt:with-recursive-lock-held ((visicon-lock vis-mod))      
      (let (tracked
            still-available
            old-loc
            old-obj
            tracked-loc)
        
        (bt:with-lock-held ((marker-lock vis-mod))
          (setf tracked (tracked-obj vis-mod))
          (setf still-available (find tracked (visicon vis-mod)))
          (setf old-loc (tracked-obj-lastloc vis-mod))
          (setf old-obj (tracked-obj-last-obj vis-mod)))
        
        (unless still-available
          (tracking-failed))
        
        (setf tracked-loc (chunk-visual-loc tracked))
        
        (let ((vis-loc-chunk (buffer-read 'visual-location))
              (vis-obj-chunk (buffer-read 'visual)))
          
        ;; we don't have an old-loc but the one currently in the
        ;; visual-location buffer is the screen-pos of the old-obj
        ;; should make that the old-loc.  This is the 5.1 fix.
        
          (when (and (null old-loc) old-obj vis-loc-chunk
                     (chunk-slot-equal vis-loc-chunk (fast-chunk-slot-value-fct old-obj 'screen-pos)))
            (setf old-loc vis-loc-chunk))
        
          
        
          (let ((new-obj (convert-loc-to-object vis-mod tracked)))
          
            (unless new-obj ;; for some reason we have a location but no object
              (tracking-failed))
          
            (bt:with-lock-held ((marker-lock vis-mod))
              (set-current-marker vis-mod tracked)
              (setf (last-obj vis-mod) new-obj))
          
          ;; For the following events need to set priority of the buffer setting
          ;; so that if there's a find-location scheduled but not completed this 
          ;; happens first, so that the find-loc overwrites.  Thus the priorities > 10.
          
          (flet ((set-vis-loc ()
                              (schedule-set-buffer-chunk 'visual-location tracked-loc 0 :time-in-ms t :module :vision 
                                                         :output 'high :requested nil :priority 15)
                              ;; need to make sure the chunk being set in the buffer isn't deleted before it gets there
                              (when from-proc-display
                                (lock-vision vis-mod)
                                (schedule-event-now 'unlock-vision :module :vision
                                                    :destination :vision :priority 14
                                                    :output nil :maintenance t)))
                 (mod-vis-loc ()
                              (schedule-mod-buffer-chunk 'visual-location (chunk-difference-to-chunk-spec tracked-loc vis-loc-chunk) 0
                                                         :time-in-ms t :module :vision :output 'high :priority 15))
                 (set-vis-obj ()
                              (schedule-set-buffer-chunk 'visual new-obj 0 :time-in-ms t :module :vision 
                                                         :output 'high :requested nil :priority 14))
                 (mod-vis-obj ()
                              (schedule-mod-buffer-chunk 'visual (chunk-difference-to-chunk-spec new-obj vis-obj-chunk) 0
                                                         :time-in-ms t :module :vision :output 'high :priority 14))
                 (update-loc (mod)
                             (schedule-event-now 'update-tracking-loc-chunk :module :vision
                                                 :destination :vision :params (if mod (list t) nil) :priority 13 :output nil))
                 (update-obj ()
                             (schedule-event-now 'update-tracking-obj-chunk :module :vision
                                                 :destination :vision :params nil :priority 12 :output nil)))
            
            ;;; Make sure there's still a finst on the tracked item
            
            (aif (member tracked (finst-lst vis-mod) :key 'id :test 'equal)
                 (setf (tstamp (first it)) (mp-time-ms))
                 (add-finst vis-mod tracked))
            
            (cond ((and (null vis-loc-chunk)
                        (null vis-obj-chunk))
                   ;; Stuff both buffers and then update the obj with the buffer-chunk's name
                   (set-vis-loc)
                   (set-vis-obj)
                   (update-loc t)
                   (update-obj))
                  
                  ((and (null vis-loc-chunk)
                        (eq vis-obj-chunk old-obj))
                   ;; stuff the new location and modify the visual buffer with the new info
                   (set-vis-loc)
                   (mod-vis-obj)
                   (update-loc t))
                  
                  ((null vis-loc-chunk) 
                   ;; stuff a new location chunk and don't touch the chunk in the visual buffer
                   (set-vis-loc)
                   (update-loc nil))
                  
                  ((and (eq vis-loc-chunk old-loc)
                        (null vis-obj-chunk))
                   ;; Modify the chunk in the visual-location buffer put new obj into visual buffer
                   (mod-vis-loc)
                   (set-vis-obj)
                   (update-loc t)
                   (update-obj))
                  
                  ((and (eq vis-loc-chunk old-loc)
                        (eq vis-obj-chunk old-obj))
                   ;; Modify both chunks and make sure the obj points to the right loc just to be safe.
                   (mod-vis-loc)
                   (mod-vis-obj)
                   (update-loc t))
                  
                  ((eq vis-loc-chunk old-loc) 
                   ;; just modify the loc and don't know about the visual buffer
                   (mod-vis-loc))
                  
                  ((null vis-obj-chunk) 
                   ;; Don't know about the vis-loc buffer just put the new object in place 
                   ;; setting the screen-pos if it isn't
                   
                   (unless (chunk-slot-value-fct new-obj 'screen-pos)
                     (set-chunk-slot-value-fct new-obj 'screen-pos tracked-loc))
                   
                   (set-vis-obj)
                   (update-obj))
                  
                  ((eq vis-obj-chunk old-obj) 
                   ;; Just modify the object chunk and set the screen-pos if necessary
                   
                   (unless (chunk-slot-value-fct new-obj 'screen-pos)
                     (set-chunk-slot-value-fct new-obj 'screen-pos tracked-loc))
                   (mod-vis-obj))
                  
                  (t ;; Don't do anything 
                   ))))))
    nil)))


;;; REMOVE-TRACKING      [Method]
;;; Date        : 97.05.15
;;; Description : When tracking stops, the slots for tracked objects need to
;;;             : to be cleared, and the ACT hook functions need to be cleared.

(defgeneric remove-tracking (vis-mod)
  (:documentation  "Clears out all the tracking stuff"))

(defmethod remove-tracking ((vis-mod vision-module))
  (bt:with-lock-held ((marker-lock vis-mod))
    (setf (tracked-obj-lastloc vis-mod) nil)
    (setf (tracked-obj-last-obj vis-mod) nil)
    (setf (tracked-obj vis-mod) nil))
  (change-state vis-mod :exec 'FREE))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod clear :after ((vis-mod vision-module))
  (bt:with-lock-held ((marker-lock vis-mod))
    (setf (currently-attended vis-mod) nil)
      ;; handles clof and current-marker
    (set-current-marker vis-mod nil)
    
    (setf (last-obj vis-mod) nil)
  
    (setf (loc-failure vis-mod) nil)
    (setf (attend-failure vis-mod) nil)
  
    (setf (scene-change vis-mod) nil)
    (setf (last-attention-params vis-mod) nil))
  
  (remove-tracking vis-mod)
  (dispatch-apply "visual-clear"))

(add-act-r-command "visual-clear" nil "Signal to indicate that the vision module has executed a clear request.  No params.")

;;; FIND-LOCATION      [Method]
;;; Date        : 98.08.11R (rewrite), delta 99.06.24
;;; Description : To find a location, we need to search the icon.  To do this,
;;;             : construct a feature spec based on the parameters passed,
;;;             : and filter the list to those that match the spec.  This 
;;;             : should be faster for large icons because it makes only
;;;             : one pass over the icon.  Regardless, some
;;;             : post-processing of that list might be necessary if LOWEST
;;;             : or HIGHEST was passed as a coordinate or if NEAREST is 
;;;             : true.
;;;             : Once we have a list, randomly pick one (that may change in
;;;             : the future) feature and build a location out of it.

(defgeneric find-location (vis-mod chunk-spec)
  (:documentation  "Given a set of constraints, build a DMO for that screen location if one matches."))

(defmethod find-location ((vis-mod vision-module) chunk-spec)
  (let ((loc (awhen (find-current-locs-with-spec vis-mod chunk-spec)
                    (construct-location vis-mod (random-item (objs-max-val it 'chunk-visual-tstamp)) chunk-spec))))
    (if loc
        (let ((vl-loc (convert-visicon-chunk-to-vis-loc loc)))
          (bt:with-lock-held ((marker-lock vis-mod)) (setf (loc-failure vis-mod) nil))
          (schedule-set-buffer-chunk 'visual-location vl-loc 0 :time-in-ms t :module :vision :priority 10)
          (lock-vision vis-mod)
          (schedule-event-now 'unlock-vision 
                              :module :vision
                              :destination :vision
                              :priority 9
                              :output nil
                              :maintenance t)
          ;(schedule-event-now 'delete-chunk-fct :module :vision :params (list vl-loc) :priority 8 :output nil :maintenance t)
          )
      (progn
        (set-buffer-failure 'visual-location)
        (bt:with-lock-held ((marker-lock vis-mod)) (setf (loc-failure vis-mod) t))
        (schedule-event-now nil :module :vision :output 'low :details "find-loc-failure")))
    
    (when (and loc (auto-attend vis-mod))
      (schedule-event-now 'visual-auto-attend :destination :vision :output t
                          :module :vision :details (concatenate 'string "automatically attending " (symbol-name loc))))
    loc))


(defmethod visual-auto-attend ((vis-mod vision-module))
  (aif (buffer-read 'visual-location)
       (progn   
         ;; mark the module as busy between now and the attention shift
         (change-state vis-mod :exec 'BUSY :proc 'BUSY)
         
         ;; schedule the attention shift to be 50ms from now - should probably use the default action time
         ;; instead but not going to do that at this point...
         
         (schedule-event-relative 50 'move-attention :time-in-ms t :params (list :location it) :destination :vision :output 'medium
                                  :module :vision :details (concatenate 'string "Move-attention " (symbol-name it)) :priority 0)
         
         ;; Hack to clear the module state just before the attention shift so as not to 
         ;; jam things.
         
         (schedule-event-relative 50 'change-state :time-in-ms t :params (list :exec 'free :proc 'free) :destination :vision :output nil
                                  :module :vision :priority 1))
       
       (model-warning "Auto-attend failed because visual-location buffer was empty.")))





(defgeneric construct-location (vis-mod feat spec)
  (:documentation  "Find or build a DMO based on a feature and the spec used to generate that feature."))

(defmethod construct-location ((vis-mod vision-module) loc spec)
  
  ;; record that this loc was found with this spec
  (setf (gethash (chunk-visicon-entry loc) (found-locs vis-mod)) spec)
  loc)

;;; MOVE-ATTENTION      [Method]
;;; Date        : 97.02.09
;;; Description : This is a toplevel command for the Vision Module, causing
;;;             : attention to move.  Latencey is 185 ms, so nothing actually
;;;             : happens right away other than setting state variables.
;;;             : A method is dispatched based on the scale that's passed.

(defgeneric move-attention (vis-mod &key location scale)
  (:documentation "Shift attention to a particular location at a particular scale."))

(defmethod move-attention ((vis-mod vision-module) &key location scale)
  (let (tracked old-clof clof latency hook override)
    
    (bt:with-lock-held ((marker-lock vis-mod))
      (setf tracked (tracked-obj vis-mod))
      
      
      (when (and (eq (exec-s vis-mod) 'BUSY) (not (tracked-obj vis-mod)))
        (model-warning "Attention shift requested at ~S while one was already in progress." (mp-time))
        (return-from move-attention))
      
      (setf (moving-attention vis-mod) t)
           
      (when tracked (remove-tracking vis-mod))
      (setf (currently-attended vis-mod) nil)
      (setf old-clof (clof vis-mod))
             
      (set-current-marker vis-mod location)
      
      (setf clof (clof vis-mod))
      (setf (last-scale vis-mod) scale)
      (setf (attend-failure vis-mod) nil)
      
      (setf (last-attention-params vis-mod) (list location clof scale)))
           
    (bt:with-recursive-lock-held ((param-lock vis-mod))
      (setf latency (move-attn-latency vis-mod))
      (setf hook (encoding-hook vis-mod)))
             
    (when hook 
      (setf override (dispatch-apply hook (coerce old-clof 'list) (coerce clof 'list) location scale)))
             
    (cond ((null override)
           (bt:with-lock-held ((marker-lock vis-mod))
             (setf (last-attention-params vis-mod) nil))
           (schedule-event-relative (randomize-time-ms latency) 'encoding-complete
                                    :time-in-ms t :destination :vision :module :vision :params (list location clof scale) :output 'medium
                                    :details (concatenate 'string "Encoding-complete " (symbol-name location) " " (symbol-name scale))))
          ((numberp override)
           (when (minusp override)
             (print-warning "Encoding hook returned a negative encoding time ~f. Using 0 instead." override)
             (setf override 0))
           (bt:with-lock-held ((marker-lock vis-mod))
             (setf (last-attention-params vis-mod) nil))
           (schedule-event-relative (randomize-time-ms (safe-seconds->ms override 'move-attention)) 'encoding-complete
                                    :time-in-ms t :destination :vision :module :vision :params (list location clof scale) :output 'medium
                                    :details (concatenate 'string "Encoding-complete " (symbol-name location) " " (symbol-name scale))))
          (t ;; 
           ))
    
    (change-state vis-mod :exec 'BUSY :proc 'BUSY)))


(defun schedule-encoding-complete (delay)
  (let ((vis-mod (get-module :vision)))
    (when vis-mod
      (bt:with-lock-held ((marker-lock vis-mod))
        (aif (last-attention-params vis-mod)
            (progn
              (unless (and (numberp delay) (not (minusp delay)))
                (print-warning "Invalid delay time passed to schedule-encoding-complete ~s. Using 0 instead." delay)
                (setf delay 0))
              (schedule-event-relative (randomize-time-ms (safe-seconds->ms delay 'schedule-encoding-complete)) 'encoding-complete
                                             :time-in-ms t :destination :vision :module :vision :params it :output 'medium
                                       :details (concatenate 'string "Encoding-complete " (symbol-name (first it)) " " (symbol-name (third it))))
              (setf (last-attention-params vis-mod) nil))
             (print-warning "Schedule-encoding-complete called but there are no attention parameters available."))))))

(add-act-r-command "schedule-encoding-complete" 'schedule-encoding-complete "Command for a visual encoding hook function to call to schedule the encoding-complete action. Params: delay.")
        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-vision-module (model-name)
  (declare (ignore model-name))
  
  (chunk-type visual-object screen-pos value status color height width)
  
  (chunk-type (text (:include visual-object)) (text t))
  (chunk-type (oval (:include visual-object)) (oval t))
  (chunk-type (line (:include visual-object)) (line t) end1-x end1-y end2-x end2-y)
  
  ;; locations
  
  (chunk-type visual-location screen-x screen-y distance kind color value height width size)
  (chunk-type (line-location (:include visual-location)) end1-x end1-y end2-x end2-y)
  
  
  ;; loc request
  
  (chunk-type (set-visloc-default (:include visual-location)) (set-visloc-default t))
  
  ;; obj requests
  
  (chunk-type vision-command cmd)
  (chunk-type (move-attention (:include vision-command)) (cmd move-attention) screen-pos scale)
  (chunk-type (start-tracking (:include vision-command)) (cmd start-tracking))
  (chunk-type (assign-finst (:include vision-command)) (cmd assign-finst) object location)
  (chunk-type (clear-scene-change (:include vision-command)) (cmd clear-scene-change))
  (chunk-type (clear-all-finsts (:include vision-command)) (cmd clear-all-finsts))
  
  (chunk-type color color)
  
  (chunk-type (phrase! (:include visual-object)) (phrase! t) objects words colors)
  (chunk-type (char-primitive (:include visual-location)) (kind char-primitive) left right)
  (chunk-type (empty-space (:include visual-object)) (empty-space t))
  
  ;; chunks for features and requests
  
  (dolist (chunk '(lowest highest current current-x current-distance
                          current-y clockwise counterclockwise external
                          internal text box line oval char-primitive 
                          new find-location))
    (unless (chunk-p-fct chunk)
      (define-chunks-fct `((,chunk name ,chunk)))))
  
  (dolist (chunk '(move-attention assign-finst start-tracking clear-scene-change set-visloc-default clear-all-finsts))
    (unless (chunk-p-fct chunk)
      (define-chunks-fct `((,chunk isa ,chunk)))))
  
  ;; default colors
  
  (dolist (chunk '(black red blue green white magenta yellow
                         cyan dark-green dark-red dark-cyan dark-blue
                         dark-magenta dark-yellow light-gray dark-gray gray
                         pink light-blue purple brown))
    (unless (chunk-p-fct chunk)
      (define-chunks-fct `((,chunk color ,chunk)))))
  
  (make-instance 'vision-module))

(defun reset-vision-module (vis-mod)
  
  (reset-pm-module vis-mod)
  
  (bt:with-lock-held ((marker-lock vis-mod))
    (setf (currently-attended vis-mod) nil)
    (setf (current-marker vis-mod) nil)
    (setf (clof vis-mod) nil)
    
    (setf (last-obj vis-mod) nil)
      
    (setf (loc-failure vis-mod) nil)
    (setf (attend-failure vis-mod) nil)
    (setf (scene-change vis-mod) nil)
    (setf (last-attention-params vis-mod) nil))
  
  (remove-tracking vis-mod)
  
  (bt:with-recursive-lock-held ((proc-display-lock vis-mod))
    (setf (proc-display-locks vis-mod) 0) 
    (setf (scheduled-proc-display vis-mod) nil)
    (setf (blocked-proc-display vis-mod) nil))
  
  
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (setf (visicon vis-mod) nil)
    (setf (visicon-updates vis-mod) nil)
    (setf (finst-lst vis-mod) nil))
  
  (clrhash (found-locs vis-mod))
  
  (setf (current-cursor vis-mod) nil)
  
  
  (setf (last-scale vis-mod) nil)
  
  #| not used for now
  (set-cfs-mth vis-mod :RM-ORIG)
  |#
  
  (setf (synthd-objs vis-mod) (clrhash (synthd-objs vis-mod)))
  

  (bt:with-lock-held ((text-parse-lock vis-mod))
    (setf (other-word-chars vis-mod) nil))
  
  (bt:with-lock-held ((vis-loc-lock vis-mod))
    (setf (center-point vis-mod) (vector 0 0)))
  
  
  (setf (default-spec vis-mod) (define-chunk-spec screen-x lowest :attended new)))

(defun clear-scene-change (vis-mod)
  (bt:with-lock-held ((marker-lock vis-mod))
    (setf (scene-change vis-mod) nil)))

(defun clear-all-finsts (vis-mod)
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (setf (finst-lst vis-mod) nil)))

(defun query-vision-module (vis-mod buffer slot value)
  (bt:with-lock-held ((marker-lock vis-mod))
    (case buffer
      (visual
       (cond ((and (eq slot 'state) (eq value 'error))
              (attend-failure vis-mod))
             ((eq slot 'scene-change)
              (let ((change-detect (and (numberp (car (scene-change vis-mod)))
                                        (>= (car (scene-change vis-mod)) (change-threshold vis-mod))
                                        (<= (mp-time-ms) (+ (cdr (scene-change vis-mod)) (bt:with-recursive-lock-held ((param-lock vis-mod)) (new-span vis-mod)))))))
                (or (and change-detect value)
                    (null (or change-detect value)))))
             ((eq slot 'scene-change-value)
              (and (numberp value)
                   (numberp (car (scene-change vis-mod)))
                   (>= (car (scene-change vis-mod)) value)))
             (t (generic-state-query vis-mod buffer slot value))))
      (visual-location
       (case slot
         (state
          (case value
            (busy nil) ;; visual-location requests are always free
            (free t)
            (error (loc-failure vis-mod))
            (t (print-warning "Invalid query made of the visual-location buffer with slot ~S and value ~S" slot value))))
         (attended
          (let ((vis-loc-chunk (buffer-read 'visual-location)))
            (when vis-loc-chunk
              (let* ((old-loc  (chunk-visicon-entry vis-loc-chunk))
                     (loc (if (chunk-p-fct old-loc) old-loc vis-loc-chunk)))
                (update-new vis-mod)
                (check-finsts vis-mod)
                
                (test-attended vis-mod (list '= :attended value) loc))))))))))


(defmethod warn-vision ((vis-mod vision-module) buffer-name chunk-spec)
  (declare (ignore chunk-spec))
  (when (and (eq buffer-name 'visual)
             (null (visual-lock vis-mod)))
    (lock-vision vis-mod)
    (setf (visual-lock vis-mod) t)))


(defmethod pm-module-last-cmd-name ((vis-mod vision-module) buffer-name chunk-spec)
  (declare (ignorable vis-mod))
  (case buffer-name
    (visual-location (if (chunk-spec-slot-spec chunk-spec 'set-visloc-default)
                         'set-visloc-default
                       'find-location))
    ;; shouldn't happen since they've all got a cmd slot...
    (visual :bad-command)))





(defmethod pm-module-request ((vis-mod vision-module) buffer-name chunk-spec)
  (case buffer-name
    (visual
     
     (when (visual-lock vis-mod)
       (setf (visual-lock vis-mod) nil)
       (schedule-event-now 'unlock-vision :module :vision :destination :vision
                           :priority :min :output nil :maintenance t))
     
     (cond ((test-for-clear-request chunk-spec)
            (schedule-event-now 'clear :module :vision :destination :vision :output 'low))
           
           ((= (length (chunk-spec-slot-spec chunk-spec 'cmd)) 1)
            
            
            (let ((cmd (spec-slot-value (first (chunk-spec-slot-spec chunk-spec 'cmd)))))
              (case cmd
                (clear-all-finsts
                 (schedule-event-now 'clear-all-finsts :module :vision :destination :vision :output 'low))
                (clear-scene-change
                 (schedule-event-now 'clear-scene-change :module :vision :destination :vision :output 'low))
                (start-tracking
                 (schedule-event-now 'start-tracking :destination :vision :module :vision :output 'medium))
                (assign-finst
                 (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                                   (verify-single-explicit-value chunk-spec 'object :vision 'assign-finst)
                                 nil))
                       (location (if (slot-in-chunk-spec-p chunk-spec 'location)
                                     (verify-single-explicit-value chunk-spec 'location :vision 'assign-finst)
                                   nil)))
                   
                   (if (or object location)
                       (schedule-event-now 'assign-finst :params (list vis-mod :object object :location location)
                                           :module :vision :output 'medium :details (format nil "assign-finst ~a ~a" (if object 'object 'location) (if object object location)))
                     (print-warning "An object or location is required for an assign-finst request"))))
                (move-attention
                 (let ((sp (verify-single-explicit-value chunk-spec 'screen-pos :vision 'move-attention))
                       (scale (if (slot-in-chunk-spec-p chunk-spec 'scale)
                                  (verify-single-explicit-value chunk-spec 'scale :vision 'move-attention)
                                nil)))
                   (if (valid-vis-loc-chunk sp vis-mod)
                       (schedule-event-now 'move-attention 
                                           :params (list vis-mod :scale scale :location sp)
                                           :details (concatenate 'string "Move-attention " (symbol-name sp)" " (symbol-name scale))
                                           :module :vision)
                     (progn
                       (print-warning "screen-pos value ~s in a move-attention request was not a valid chunk" sp)))))
                (t
                 (print-warning "Invalid command ~a sent to the visual buffer" cmd)))))
           (t
            (print-warning "Invalid request sent to the visual buffer~%~a" (printed-chunk-spec chunk-spec)))))
    
    (visual-location
     
     (cond ((> (length (chunk-spec-slot-spec chunk-spec :attended)) 1)
            (print-warning ":attended specification only allowed once in a visual-location request."))
           ((> (length (chunk-spec-slot-spec chunk-spec :nearest)) 1)
            (print-warning ":nearest specification only allowed once in a visual-location request."))
           ((> (length (chunk-spec-slot-spec chunk-spec :center)) 1)
            (print-warning ":center specification only allowed once in a visual-location request."))
           ((chunk-spec-slot-spec chunk-spec 'set-visloc-default)
            (let* ((specs (chunk-spec-slot-spec chunk-spec 'set-visloc-default))
                   (spec (first specs))
                   
                   )
              (cond ((> (length specs) 1)
                     (print-warning "set-visloc-default slot can only be specified once in a visual-location request."))
                    ((not (eq '= (spec-slot-op spec)))
                     (print-warning "set-visloc-default slot must use the = test in a visual-location request."))
                    (t
                     (schedule-event-now 'set-visloc-default-request :module :vision 
                                         :destination :vision 
                                         :details "Set-visloc-default" 
                                         :output 'medium
                                         :priority 9 ; just below the buffer clearing by the production
                                         :params (list chunk-spec))))))
           (t
            (schedule-event-now 'find-location :module :vision 
                                :destination :vision 
                                :details "Find-location" 
                                :output 'medium
                                :params (list chunk-spec)))))))



(defun params-vision-module (vis-mod param)
  (bt:with-recursive-lock-held ((param-lock vis-mod))
    (if (consp param)
        (case (car param)
          (:visual-encoding-hook
           (when (and (cdr param) (encoding-hook vis-mod))
             (print-warning ":visual-encoding-hook parameter being changed from ~s to ~s." (encoding-hook vis-mod) (cdr param)))
           (setf (encoding-hook vis-mod) (cdr param)))
          (:optimize-visual
           (if (cdr param)
               (setf (optimize-p vis-mod) (cdr param))
             (print-warning "Vision module does not currently support :optimize-visual set to nil")))
          (:viewing-distance (setf (viewing-distance vis-mod) (cdr param))
                             (when (and (numberp (viewing-distance vis-mod)) (numberp (ppi vis-mod)))
                               (setf (view-dist vis-mod) (round (* (viewing-distance vis-mod) (ppi vis-mod))))))
          (:pixels-per-inch (setf (ppi vis-mod) (cdr param))
                            (when (and (numberp (viewing-distance vis-mod)) (numberp (ppi vis-mod)))
                              (setf (view-dist vis-mod) (round (* (viewing-distance vis-mod) (ppi vis-mod))))))
          (:visual-attention-latency
           (setf (move-attn-latency vis-mod) (safe-seconds->ms (cdr param) 'sgp))
           (cdr param))
          (:scene-change-threshold
           (setf (change-threshold vis-mod) (cdr param)))
          (:visual-finst-span
           (setf (finst-span vis-mod) (safe-seconds->ms (cdr param) 'sgp))
           (check-finsts vis-mod)
           (cdr param))   
          (:visual-movement-tolerance
           (setf (move-allowance vis-mod) (cdr param)))
          (:visual-num-finsts
           (setf (num-finst vis-mod) (cdr param))
           (check-finsts vis-mod)
           (cdr param))
          (:visual-onset-span
           (setf (new-span vis-mod) (safe-seconds->ms (cdr param) 'sgp))
           (cdr param))
          (:auto-attend
           (setf (auto-attend vis-mod) (cdr param)))
          (:delete-visicon-chunks
           (setf (purge-visicon vis-mod) (cdr param)))
          (:unstuff-visual-location
           (setf (unstuff-loc vis-mod)
             (if (numberp (cdr param))
                 (safe-seconds->ms (cdr param) 'sgp)
               (cdr param)))
           (cdr param))
          (:overstuff-visual-location
           (setf (overstuff-loc vis-mod) (cdr param)))
          (:tracking-clear
           (setf (tracking-clear vis-mod) (cdr param)))
          (:show-focus
           (setf (show-focus-p vis-mod) (cdr param)))
          (:default-target-width
           (setf (default-width vis-mod) (cdr param))))
      (case param
        (:visual-encoding-hook
         (encoding-hook vis-mod))
        (:optimize-visual
         (optimize-p vis-mod))  
        (:scene-change-threshold
         (change-threshold vis-mod))
        (:visual-attention-latency
         (ms->seconds (move-attn-latency vis-mod)))
        (:visual-finst-span
         (ms->seconds (finst-span vis-mod)))   
        (:visual-movement-tolerance
         (move-allowance vis-mod))
        (:visual-num-finsts
         (num-finst vis-mod))
        (:visual-onset-span
         (ms->seconds (new-span vis-mod)))
        (:auto-attend
         (auto-attend vis-mod))
        (:delete-visicon-chunks
         (purge-visicon vis-mod))
        (:unstuff-visual-location
         (if (numberp (unstuff-loc vis-mod))
             (ms->seconds (unstuff-loc vis-mod))
           (unstuff-loc vis-mod)))
        (:overstuff-visual-location (overstuff-loc vis-mod))
        (:tracking-clear (tracking-clear vis-mod))
        (:show-focus (show-focus-p vis-mod))))))


(defun visual-location-status ()
  (format nil "  attended new          : ~S~%  attended nil          : ~S~%  attended t            : ~S"
    (query-buffer 'visual-location '(attended  new))
    (query-buffer 'visual-location '(attended  nil))
    (query-buffer 'visual-location '(attended  t))))

(defun visual-buffer-status ()
  (let ((v (get-module :vision)))
    (concatenate 'string (print-module-status v)
      (format nil "~%  scene-change          : ~S~%" (query-buffer 'visual '(scene-change t)))
      (format nil "  scene-change-value    : ~S" (car (scene-change v))))))

(defun scene-change-value-test (x)
  (and (numberp x) (<= 0.0 x 1.0)))

(defun show-focus-value-test (x) 
  (or (tornil x) (symbolp x) (stringp x)))

(define-module-fct :vision 
    (list (define-buffer-fct 'visual-location 
              :request-params (list :attended :nearest :center) 
            :queries '(attended)
            :status-fn 'visual-location-status)
          (define-buffer-fct 'visual 
              :queries '(scene-change-value scene-change modality preparation execution processor last-command)
            :status-fn 'visual-buffer-status))
  (list 
   (define-parameter :scene-change-threshold
       :valid-test 'scene-change-value-test 
     :default-value 0.25
     :warning "a number in the range [0.0,1.0]"
     :documentation "Proportion of visicon which must change to signal a scene change")
   (define-parameter :optimize-visual
       :valid-test 'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "If set to nil the default devices process text into sub-letter features")
   (define-parameter :visual-attention-latency
       :valid-test 'nonneg 
     :default-value 0.085
     :warning "a non-negative number"
     :documentation "Time for a shift of visual attention")
   (define-parameter :visual-finst-span
       :valid-test 'nonneg 
     :default-value 3.0
     :warning "a non-negative number"
     :documentation "Lifespan of a visual finst")
   (define-parameter :visual-movement-tolerance
       :valid-test 'nonneg 
     :default-value 0.5
     :warning "a non-negative number"
     :documentation 
     "How far something can move while still being seen as the same object.")
   (define-parameter :visual-num-finsts
       :valid-test 'posnum 
     :default-value 4
     :warning "a positive number"
     :documentation "Number of visual finsts.")
   (define-parameter :visual-onset-span
       :valid-test 'nonneg 
     :default-value 0.5
     :warning "a non-negative number"
     :documentation "Lifespan of new visual objects being marked as NEW")
   (define-parameter :auto-attend
       :valid-test 'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Whether visual-location requests automatically generate an attention shift")
   
   (define-parameter :visual-encoding-hook
       :valid-test 'local-or-remote-function-or-nil 
     :default-value nil
     :warning "Local or remote function or NIL"
     :documentation "User hook for overriding the encoding time of an attention shift")
   
   (define-parameter :delete-visicon-chunks
       :valid-test 'tornil 
     :default-value T
     :warning "T or NIL"
     :documentation "Whether proc-display should delete and unintern the name of old chunks that were in the visicon")
   (define-parameter :unstuff-visual-location
       :valid-test 'numorbool 
     :default-value t
     :warning "T, NIL, or a number"
     :documentation "Whether chunks stuffed into the visual-location buffer should be automatically cleared by the module if unused")
   (define-parameter :overstuff-visual-location
       :valid-test 'tornil
     :default-value nil
     :warning "T or NIL"
     :documentation "Whether a chunk previously stuffed into the visual-location buffer can be overwritten by a new chunk to be stuffed")
   (define-parameter :tracking-clear
       :valid-test 'tornil
     :default-value t
     :warning "T or NIL"
     :documentation "Whether a tracking failure clears the currently attended location")
   (define-parameter :show-focus
     :valid-test 'show-focus-value-test
     :default-value nil
     :warning "T, NIL, a symbol, or string"
     :documentation "Show the focus ring on the GUI?  T, a string, or symbol will enable, and some devices will use the value to indicate a color.")
   (define-parameter :viewing-distance :owner nil)
   (define-parameter :pixels-per-inch :owner nil)
   (define-parameter :default-target-width :owner nil))
  
  :version (version-string (make-instance 'vision-module))
  :documentation "A module to provide a model with a visual attention system"
  :creation 'create-vision-module
  :reset 'reset-vision-module
  :query 'query-vision-module
  :request 'pm-module-request
  :params 'params-vision-module
  :warning 'warn-vision)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; BUILD-STRING-FEATS      [Method]
;;; Date        : 2014.01.24
;;; Description : In order to build all the features for a string, there is a
;;;             : great deal of stuff that is needed from the interface beyond
;;;             : just the string itself.  Need to know the y coordinate, the
;;;             : starting x coordinate, the text height, the line height, the 
;;;             : associated screen object, and some way of determining the width 
;;;             : (in pixels) of a string.  May also provide a function for 
;;;             : computing a different starting x coordinate for each line.
;;;             : 
;;;             : There are two ways to do it, with and without optimizing. 
;;;             : With optimizing it's easy, just accumulate words.  When
;;;             : optimzing is off, though, have to walk the string
;;;             : character-by-character and build features for each one.



(defun build-string-feats (&key text start-x y-pos width-fct height obj (line-height 0) x-fct (x-off 0) (y-off 0))
  (declare (fixnum start-x y-pos)  (string text) (number height) (number line-height))
  (when (not (and text start-x y-pos width-fct height line-height))
    (error "NIL passed to BUILD-STRING-FEATS."))
  (unless (equal text "")
    (let* ((vis-mod (get-module :vision))
           (f-accum nil)
           (spc-wdth (dispatch-apply width-fct " "))
           (coord-slots (bt:with-lock-held ((vis-loc-lock vis-mod)) (vis-loc-slots vis-mod)))
           (x-slot (first coord-slots))
           (y-slot (second coord-slots)))
      (dolist (line (string-to-lines text) (nreverse f-accum))
        (let ((curr-x (if (or (functionp x-fct) (and (symbolp x-fct) (fboundp x-fct)))
                          (let ((x (funcall x-fct line start-x obj)))
                            (if (numberp x) x start-x))
                        start-x))
              (curr-width nil))
          (if (bt:with-recursive-lock-held ((param-lock vis-mod)) (optimize-p vis-mod))
              
              ;; if optimizing is on, then carve the string into words (strings)
              ;; and space runs (numbers)
              (dolist (word (chop-string vis-mod line))
                (when (stringp word)
                  (setf curr-width (dispatch-apply width-fct word))
                  (let ((vl `(isa (visual-location text)
                              ,y-slot ,(+ y-pos y-off) height ,height
                              value (text ,word)
                              width ,curr-width
                              ,x-slot ,(+ curr-x x-off (round curr-width 2)))))
                    (push vl f-accum)
                    ))
                (incf curr-x (if (stringp word) curr-width (* word spc-wdth))))
            
            ;; if not optimizing, then blast it character-by-character
            (let ((char nil))
              (print-warning "Non-optimized text features not currently available.")
              char
                             
              #| non-optimized needs to be corrected for new interface
                (dotimes (idx (length line))
                (setf char (char line idx))
                (setf curr-width (dispatch-apply width-fct (subseq line idx (1+ idx))))
                (cond ((alphanumericp char)
                       (push (char-to-features vis-mod (char-upcase char) curr-x
                                               (- (+ curr-x curr-width) 1) y-pos
                                               height obj) 
                             f-accum))
                      ((and (graphic-char-p char)
                            (not (char-equal char #\space)))
                       
                       (let ((vl (car (define-chunks-fct `((isa visual-location
                                                                ,y-slot ,y-pos height ,height value text
                                                                kind text width ,curr-width ,x-slot ,(+ curr-x (round curr-width 2))))))))
                         (push vl f-accum)
                         (setf (chunk-visual-obj-spec vl) (mkstr char))))
                      (t nil))
                (incf curr-x curr-width))
              |#
              ))
          (incf y-pos line-height))))))

(defun chop-string (vis-mod str)
  (declare (string str))
  (unless (string= str "")
    (bt:with-lock-held ((text-parse-lock vis-mod))
      (let* ((oldstate (char->state vis-mod (char str 0)))
             (state nil)
             (chr nil)
             (wrd "")
             (cnt 0)
             (accum nil))
        (dotimes (i (length str))
          (setf chr (char str i))
          (setf state (char->state vis-mod chr))
          (cond
           ;; if we're accumulating chars and the new char matches our state,
           ;; just concat the char
           ((or (and (eq state :WORD) (eq oldstate :WORD))
                (and (eq state :MISC) (eq oldstate :MISC)))
            (setf wrd (mkstr wrd chr))
            (setf cnt 0))
           ;; If we get a state change that finishes a word, then grab the
           ;; word.
           ((and (or (eq oldstate :WORD) (eq oldstate :MISC))
                 (not (eq oldstate state)))
            (push wrd accum)
            (if (not (eq state :SPACE))
                (setf wrd (mkstr chr))
              (setf wrd "")))
           ;; when switching from spaces to words, push the counter and start
           ;; the word
           ((and (eq oldstate :SPACE) (not (eq state oldstate)))
            (push (1+ cnt) accum)
            (setf wrd (mkstr wrd chr)))
           ;; from whitespace to whitespace, inc the counter
           (t (incf cnt)))
          (setf oldstate state))
        (when (not (string= wrd ""))
          (push wrd accum))
        (setf accum (nreverse accum))
        (when (numberp (first accum))
          (decf (first accum)))
        accum))))
  
(defun add-word-characters (&rest chars)
  (let ((vis-mod (get-module :vision)))
    (if vis-mod
        (bt:with-lock-held ((text-parse-lock vis-mod))
          (dolist (x chars (other-word-chars vis-mod))
            (cond ((characterp x)
                   (pushnew x (other-word-chars vis-mod)))
                  ((and (stringp x) (= (length x) 1))
                   (pushnew (char x 0) (other-word-chars vis-mod)))
                  ((and (symbolp x) (= (length (symbol-name x)) 1))
                   (pushnew (char (symbol-name x) 0) (other-word-chars vis-mod))))))
      (print-warning "No vision module available could not add new word characters."))))

(add-act-r-command "add-word-characters" 'add-word-characters "Indicate non-alphanumeric characters which the AGI text items will consider part of a 'word'. Params: char*")

(defun char->state (vis-mod char)
  "Given a character, return :WORD, :SPACE, or :MISC"
  (declare (character char))
  (cond ((or (alphanumericp char) (find char (other-word-chars vis-mod))) :WORD) ;; only called from chop-string which protects it
        ((whitespace-p char) :SPACE)
        (t :MISC)))

(defun whitespace-p (char)
  "Returns T if <char> is a whitespace character (non-printing or space)"
  (declare (character char))
  (or (not (graphic-char-p char))
      (eq char #\Space)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun compute-vis-loc-size (vis-loc vis-m)
  (set-chunk-slot-value-fct vis-loc 'size
                            (aif (simple-size vis-loc vis-m)
                                 it
                                 1.0)))
(defun simple-size (vis-loc vis-m)
  (let ((w (chunk-slot-value-fct vis-loc 'width))
        (h (chunk-slot-value-fct vis-loc 'height))
        (d (chunk-slot-value-fct vis-loc (third (visual-location-slots vis-loc vis-m)))))
    (when (and (numberp w) (numberp h) (numberp d))
      (* 0.01 (round
               (* (pm-pixels-to-angle w d)
                  (pm-pixels-to-angle h d))
               0.01)))))


;;; APPROACH-WIDTH      [Method]
;;; Date        : 99.03.30
;;; Description : Remember in high school when someone asked in trig why we
;;;             : needed to know this crap?  Well, this is why.  I'm pretty
;;;             : sure this is all the right trig, but there could be some
;;;             : missed math in here.


(defun approach-width (vis-loc theta x y)
  (flet ((default ()
             (let ((v (get-module :vision)))
               (bt:with-recursive-lock-held ((param-lock v))
                 (default-width v)))))
    (handler-case 
        (let ((custom (awhen (chunk-visual-approach-width-fn vis-loc) (dispatch-apply it (chunk-visicon-entry vis-loc) theta x y))))
          (or (and (numberp custom) custom)
              (let* ((loc-slots (visual-location-slots vis-loc (get-module :vision)))
                     (x (chunk-slot-value-fct vis-loc 'width))
                     (y (chunk-slot-value-fct vis-loc 'height))
                     (z (chunk-slot-value-fct vis-loc (third loc-slots))))
                (if (and (numberp x) (numberp y))
                    (let ((critical-theta (atan y x))
                          (theta (abs theta))
                          (ret-width nil))
                      (when (> theta (/ pi 2))
                        (setf theta (- pi theta)))
                      (setf ret-width
                        (cond ((= theta 0) x)
                              ((= theta (/ pi 2)) y)
                              ;((= theta critical-theta) (sqrt (+ (* y y) (* x x))))
                              ((<= theta critical-theta) (/ x (cos theta)))
                              (t (/ y (cos (- (/ pi 2) theta))))))
                      (pm-pixels-to-angle ret-width (when (numberp z) z)))
                  (default)))))
      ((or error condition) (x)
       (print-warning "Computing width for visicon feature ~s from chunk ~s produced error ~/print-error-message/ and default width being used." (chunk-visicon-entry vis-loc) vis-loc x)
       (default)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-visicon-features (&rest feats)
  (let ((results nil)
        (vis-mod (get-module :vision)))
    (when vis-mod
      (let ((vis-loc-slots (bt:with-lock-held ((vis-loc-lock vis-mod)) (vis-loc-slots vis-mod)))
            (view-dist (bt:with-recursive-lock-held ((param-lock vis-mod)) (view-dist vis-mod))))
        
        (flet ((invalid-feature (bad)
                                (model-warning "Visicon feature ~s did not contain a valid feature and was not created." bad)
                                (push-last nil results)))
          (dolist (f feats)
            (if (and f (listp f) (evenp (length f)))
                (handler-case 
                    (let* ((d f)
                           (specs (do ((l d (cddr l))
                                       (r nil (push-last (cons (first l) (if (listp (second l)) (second l) (list (second l)))) r)))
                                      ((null l) r)))
                           (chunk-specs (remove-if 'keywordp specs :key 'first))
                           (param-specs (remove-if-not 'keywordp specs :key 'first))
                           (loc-slots (copy-list vis-loc-slots))
                           (width-fct nil))
                      
                      (if (or (notevery (lambda (x) (or (and (eq (first x) :x-slot) (valid-slot-name (second x)))
                                                        (and (eq (first x) :y-slot) (valid-slot-name (second x)))
                                                        (and (eq (first x) :z-slot) (valid-slot-name (second x)))
                                                        (and (eq (first x) :width-fn) (local-or-remote-function-or-nil (second x)))))
                                        param-specs)
                              (notevery (lambda (x) (and (listp x) (<= 1 (length x) 3))) chunk-specs))
                          (invalid-feature f)
                        
                        (progn
                          
                          ;; get parameter settings
                                                    
                          (dolist (x param-specs)
                            (case (first x)
                              (:x-slot
                               (setf (first loc-slots) (second x)))
                              (:y-slot
                               (setf (second loc-slots) (second x)))
                              (:z-slot
                               (setf (third loc-slots) (second x)))
                              (:width-fn
                               (setf width-fct (second x)))))
                          
                          (if (<= (count 'isa chunk-specs :key 'first) 1)
                              (let ((isa (find 'isa chunk-specs :key 'first)))
                                (if (notevery 'chunk-type-p-fct (cdr isa))
                                    (invalid-feature f)
                                  (progn
                                    
                                    (when isa
                                      (setf chunk-specs (remove isa chunk-specs))
                                      (push isa chunk-specs)
                                      
                                      (when (and t ; for now always add it... (add-kind-slot vis-mod)
                                                 (= 3 (length isa))
                                                 (not (find 'kind chunk-specs :key 'first)))
                                        (push-last (list 'kind (third isa) nil) chunk-specs))
                                      (let ((loc-defaults (when (second isa)
                                                            (mapcar 'cdr 
                                                              (chunk-spec-slot-spec (define-chunk-spec-fct (list 'isa (second isa)))))))
                                            (obj-defaults (when (third isa)
                                                            (mapcar 'cdr 
                                                              (chunk-spec-slot-spec (define-chunk-spec-fct (list 'isa (third isa))))))))
                                        
                                        (dolist (ld loc-defaults)
                                          (unless (find (first ld) chunk-specs :key 'first)
                                            (push-last (list (first ld) (second ld)) chunk-specs)))
                                        (dolist (od obj-defaults)
                                          (let ((exists (find (first od) chunk-specs :key 'first)))
                                            (if exists
                                                (when (= (length exists) 2)
                                                  (let ((e (remove exists chunk-specs)))
                                                    (setf chunk-specs (push-last (list (first exists) (second exists) (second od))
                                                                                 e))))
                                              (push-last (list (first od) nil (second od)) chunk-specs))))))
                                    
                                    (let* ((has-x nil)
                                           (has-y nil)
                                           (loc-spec (mapcan (lambda (x) 
                                                               (when (second x) 
                                                                 (if (eq (first x) (first loc-slots))
                                                                     (setf has-x (numberp (second x)))
                                                                   (when (eq (first x) (second loc-slots))
                                                                     (setf has-y (numberp (second x)))))
                                                                 (list (first x) (second x))))
                                                       chunk-specs))
                                           (obj-spec (mapcan (lambda (x) 
                                                               (unless (find (first x) loc-slots)
                                                                 (if (and (third x) (= 3 (length x))) 
                                                                     (list (first x) (third x))
                                                                   (when (and (= (length x) 2) (second x))
                                                                     (list (first x) (second x))))))
                                                       chunk-specs))
                                           (visicon-spec (mapcan (lambda (x) 
                                                                   (unless (eq (first x) 'isa)
                                                                     (list (first x) (or (third x) (second x))))) chunk-specs))
                                           (loc-chunk (car (define-chunks-fct (list loc-spec))))
                                           (visicon-chunk (car (define-chunks-fct (list (push (new-name "visicon-id") visicon-spec))))))
                                      
                                      
                                      (if (and loc-chunk visicon-chunk has-x has-y)
                                          
                                          (progn
                                            (setf (chunk-vis-loc-slots loc-chunk) loc-slots)
                                            (setf (chunk-vis-loc-slots visicon-chunk) loc-slots)
                                            
                                            ;; set default distance and size if not provided as numbers
                                            
                                            (unless (numberp (fast-chunk-slot-value-fct loc-chunk (third loc-slots)))
                                              (set-chunk-slot-value-fct loc-chunk (third loc-slots) view-dist))
                                            (unless (numberp (fast-chunk-slot-value-fct visicon-chunk (third loc-slots)))
                                              (set-chunk-slot-value-fct visicon-chunk (third loc-slots) view-dist))
                                            
                                            (unless (numberp (fast-chunk-slot-value-fct loc-chunk 'size))
                                              (compute-vis-loc-size loc-chunk vis-mod))
                                            (unless (numberp (fast-chunk-slot-value-fct visicon-chunk 'size))
                                              (compute-vis-loc-size visicon-chunk vis-mod))
                                            
                                            
                                            (setf (chunk-visual-loc visicon-chunk) loc-chunk)
                                            (if (null obj-spec)
                                                (setf (chunk-visual-obj-spec visicon-chunk) '(isa visual-object))
                                              (setf (chunk-visual-obj-spec visicon-chunk) obj-spec))
                                            
                                            (setf (chunk-visicon-entry loc-chunk) visicon-chunk)
                                            (setf (chunk-visicon-entry visicon-chunk) visicon-chunk)
                                                                                        
                                            (setf (chunk-visual-approach-width-fn loc-chunk) width-fct)
                                            (setf (chunk-visual-approach-width-fn visicon-chunk) width-fct)
                                            
                                            (push-last visicon-chunk results))
                                        (progn
                                          (when (chunk-p-fct loc-chunk)
                                            (delete-chunk-fct loc-chunk))
                                          (when (chunk-p-fct visicon-chunk)
                                            (delete-chunk-fct visicon-chunk))
                                          (invalid-feature f)))))))
                            (invalid-feature f)))))
                  ((or error condition) (x)
                   (push-last nil results)
                   (model-warning "Visicon feature ~s produced error ~/print-error-message/ and was not created." f x)))
              (invalid-feature f)))
          
          (bt:with-recursive-lock-held ((visicon-lock vis-mod))
            (dolist (x results)
              (when x
                (push-last (cons :add x) (visicon-updates vis-mod)))))
          
          (schedule-proc-display vis-mod))))
    results))

(defun external-add-visicon-features (&rest feats)
  (apply 'add-visicon-features (decode-string-names feats)))

(add-act-r-command "add-visicon-features" 'external-add-visicon-features "Add visual features to the current model's visicon. Params: 'feature-spec'*.")


(defun delete-visicon-features (&rest feats)
  (let ((results nil)
        (mod (get-module :vision)))
    (when mod
      (flet ((invalid-feature (bad)
                              (model-warning "Feature ~s is not a valid feature name and cannot be removed." bad)
                              (push-last nil results)))
        (handler-case 
              (bt:with-recursive-lock-held ((visicon-lock mod))
                
                (dolist (x feats)
                  ;; actually determine if it's a valid feature because it
                  ;; can't delay that if the feature goes away because of 
                  ;; the window closing on reset -- can't guarantee the order
                  ;; of reset and the chunks are already gone by that point
                  ;; so it would always fail with the simple chunk-p test...
                  
                  (cond ((or (find x (visicon mod))
                             (find-if (lambda (y) (and (eq (car y) :add) (eq (cdr y) x))) (visicon-updates mod)))
                         (push-last x results))
                        
                        ;; why did I allow this? (and (chunk-p-fct x) (chunk-visicon-entry x) (find (chunk-visicon-entry x) (visicon mod)))
                        ;; and should it come back?
                        
                        (t (invalid-feature x))))
                
                (dolist (x results)
                  (when x
                    (push-last (cons :delete x) (visicon-updates mod))))
              
              (schedule-proc-display mod))
          
          ((or error condition) (x)
           (model-warning "Cannot delete visicon features because trying to build feature names from ~s produced error ~/print-error-message/." feats x)))))
    results))

(defun external-delete-visicon-features (&rest feats)
  (apply 'delete-visicon-features (string->name-recursive feats)))

(add-act-r-command "delete-visicon-features" 'external-delete-visicon-features "Remove visual features from the current model's visicon. Params: feature-name*.")


(defun delete-all-visicon-features ()
  (let ((mod (get-module :vision)))
    (when mod
      (bt:with-recursive-lock-held ((visicon-lock mod))
        (push-last (cons :delete-all nil) (visicon-updates mod)))
      (schedule-proc-display mod)
      t)))


(add-act-r-command "delete-all-visicon-features" 'delete-all-visicon-features "Remove all visual features from the current model's visicon. No params.")



(defun modify-visicon-features (&rest feats)
  (let ((results nil)
        (updates nil)
        (mod (get-module :vision)))
    (when mod
      (flet ((invalid-feature (bad)
                              (model-warning "Provided update ~s does not contain a valid visual feature modification." bad)
                              (push-last nil results)))
        (dolist (f feats)
          (if (and f (listp f) (oddp (length f)) (> (length f) 1))
              (handler-case 
                  (let* ((d f)
                         (slots (mapcan (let ((c 0)) (lambda (x) (prog1 (when (evenp c) (list x)) (incf c)))) (rest d))))
                    (if (or (find 'isa slots)
                            (find 'kind slots)
                            (some 'keywordp slots)
                            (not (chunk-p-fct (first d))) 
                            (not (or (find (first d) (visicon-chunks mod))
                                     (find (cons :add (first d)) (bt:with-recursive-lock-held ((visicon-lock mod)) (visicon-updates mod)) :test 'equalp)))
                            (not (= (length slots) (length (remove-duplicates slots)))))
                        (invalid-feature f)
                      (let* ((visicon-chunk (first d))
                             (specs (rest d))
                             (locs (visual-location-slots visicon-chunk mod))
                             (p1 (and (find (first locs) slots)
                                      (do* ((l specs (cddr l))
                                            (count 0 (incf count 2)))
                                           ((eq (first l) (first locs)) count))))
                             (p2 (and (find (second locs) slots)
                                      (do* ((l specs (cddr l))
                                            (count 0 (incf count 2)))
                                           ((eq (first l) (second locs)) count)))))
                        
                        (if (or (and p1 (let ((val (nth (1+ p1) specs))) (or (and (atom val) (not (numberp val)))
                                                                             (and (listp val) (not (numberp (first val)))))))
                                (and p2 (let ((val (nth (1+ p2) specs))) (or (and (atom val) (not (numberp val)))
                                                                             (and (listp val) (not (numberp (first val))))))))
                            (invalid-feature f)
                          (progn
                            (push-last visicon-chunk results)
                            (push-last d updates))))))
                ((or error condition) (x)
                 (push-last nil results)
                 (model-warning "Visicon modification ~s produced error ~/print-error-message/ and was not applied." f x)))
            (invalid-feature f)))
        
        (bt:with-recursive-lock-held ((visicon-lock mod))
          (dolist (x updates)
            (push-last (cons :update x) (visicon-updates mod))))
        
        (schedule-proc-display mod)))
    results))


(defun external-modify-visicon-features (&rest feats)
  (apply 'modify-visicon-features (decode-string-names feats)))

(add-act-r-command "modify-visicon-features" 'external-modify-visicon-features "Change a visual feature in the current model's visicon. Params: 'visicon-mod-spec'*.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod set-visloc-default-request ((vm vision-module) spec)
  "Assumes all the checking has been done already for validity"
  (setf (default-spec vm) 
    (define-chunk-spec-fct (flatten (remove 'set-visloc-default (chunk-spec-slot-spec spec) :key 'spec-slot-name))))
  (update-new vm)
  (check-finsts vm)
  (stuff-visloc-buffer vm))

(defmacro set-visloc-default (&rest params)
  "Macro to set the default specification for visual location buffer stuffing."
  `(set-visloc-default-fct ',params))

(defun set-visloc-default-fct (params)
  "Function to set the default specification for visual location buffer stuffing."
   (verify-current-model 
    "No current model.  Cannot set visloc defaults."
    (if (get-module :vision)
        (let ((chunk-spec (funcall 'define-chunk-spec-fct params)))
          (if chunk-spec
              (cond ((> (length (chunk-spec-slot-spec chunk-spec :attended)) 1)
                     (print-warning ":attended specification only allowed once in set-visloc-default.")
                     (print-warning "Visloc defaults not changed."))
                    ((> (length (chunk-spec-slot-spec chunk-spec :nearest)) 1)
                     (print-warning ":nearest specification only allowed once in set-visloc-default.")
                     (print-warning "Visloc defaults not changed."))
                    (t
                     (progn (setf (default-spec (get-module :vision)) chunk-spec) t)))
            (print-warning "Invalid chunk specification.  Default not changed.")))
      (print-warning "No vision module found.  Cannot set visloc defaults."))))

(defun external-set-visloc-default (&rest params)
  (set-visloc-default-fct (decode-string-names params)))

(add-act-r-command "set-visloc-default" 'external-set-visloc-default "Set the specification of how to pick the chunk to stuff into the visual-location buffer. Params: '{{modifier} slot value}'*")

(defun print-visicon ()
  "Print the Vision Module's visicon. For debugging."
  (command-output "~a" (printed-visicon))
  nil)

(add-act-r-command "print-visicon" 'print-visicon "Print all the attributes of the features in the model's visicon. No params.")

#| ccl 1.12-dev doesn't like the ~{~v,a~} in the format string
   and I need to use 1.12-dev to run things on 10.14...
   so have an "unrolled" version below.


(defun printed-visicon ()
  "Return the Vision Module's visicon output in a string."
  (aif (get-module :vision)  ;; Test that there is a vision module
       (let ((s (make-string-output-stream)))
         (update-new it)
         (check-finsts it) 
         
         (bt:with-recursive-lock-held ((visicon-lock it))
           (let ((attributes nil)
                 (name 4)
                 (loc (list 0 0 0)))
             (dolist (x (visicon it))
               (let* ((name-len (length (symbol-name (chunk-visual-loc x))))
                     (vis-loc-slots (visual-location-slots x it)))
                 
                 (when (> name-len name)
                   (setf name name-len))
                 
                 (dolist (y (chunk-filled-slots-list-fct x))
                   (let ((w (length (format nil "~s" (fast-chunk-slot-value-fct x y)))))
                     (aif (position y vis-loc-slots)
                          (when (> w (nth it loc))
                            (setf (nth it loc) w))
                          (aif (find y attributes :key 'second)
                               (progn
                                 (when (> w (first it))
                                   (setf (first it) w))
                                 (incf (third it)))
                               (push (list (max w (length (symbol-name y))) y 1) attributes)))))))
             
             (setf attributes (mapcar 'butlast (stable-sort attributes #'> :key 'third)))
                          
               
             (format s "~v,a  Att  ~v,a  ~:(~{~{~v,a~}~^  ~}~)~%"
               name "Name" (+ (reduce '+ loc) 4) "Loc" attributes)
             
             (format s "~v,,,'-a  ---  ~v,,,'-a  ~{~{~v,,,'-a~}~^  ~}~%" name "" (+ (reduce '+ loc) 4) ""
               (mapcar (lambda (x) (list (first x) "")) attributes))
             
             
             (let ((*print-pretty* nil))
               (dolist (x (visicon it))
                 (let ((vis-loc-slots (visual-location-slots x it)))
                   (format s "~v,a  ~3,a  (~v,@a ~v,@a ~v,@a)  ~{~{~:[~v,a~;~v,s~]~}~^  ~}~%"
                     name (chunk-visual-loc x) (feat-attended x it) 
                     (first loc) (fast-chunk-slot-value-fct x (first vis-loc-slots))
                     (second loc) (fast-chunk-slot-value-fct x (second vis-loc-slots))
                     (third loc) (fast-chunk-slot-value-fct x (third vis-loc-slots))
                     (mapcar (lambda (y)
                               (let ((v (chunk-slot-value-fct x (second y))))
                                 (list v (first y) (if v v ""))))
                       attributes)))))))
         (get-output-stream-string s))
       "#|Warning: No vision module found. |#"))
|#

(defun printed-visicon ()
  "Return the Vision Module's visicon output in a string."
  (aif (get-module :vision)  ;; Test that there is a vision module
       (let ((s (make-string-output-stream)))
         (update-new it)
         (check-finsts it) 
         
         (bt:with-recursive-lock-held ((visicon-lock it))
           (let ((attributes nil)
                 (name 4)
                 (loc (list 0 0 0)))
             (dolist (x (visicon it))
               (let* ((name-len (length (symbol-name (chunk-visual-loc x))))
                     (vis-loc-slots (visual-location-slots x it)))
                 
                 (when (> name-len name)
                   (setf name name-len))
                 
                 (dolist (y (chunk-filled-slots-list-fct x))
                   (let ((w (length (format nil "~s" (fast-chunk-slot-value-fct x y)))))
                     (aif (position y vis-loc-slots)
                          (when (> w (nth it loc))
                            (setf (nth it loc) w))
                          (aif (find y attributes :key 'second)
                               (progn
                                 (when (> w (first it))
                                   (setf (first it) w))
                                 (incf (third it)))
                               (push (list (max w (length (symbol-name y))) y 1) attributes)))))))
             
             (setf attributes (mapcar 'butlast (stable-sort attributes #'> :key 'third)))
                          
               
             (format s "~v,a  Att  ~v,a  "
               name "Name" (+ (reduce '+ loc) 4) "Loc" )
             
             (dolist (z attributes)
               (format  s "~v,a  " (first z) (second z)))
             (format s "~%")
             

             
             
             (format s "~v,,,'-a  ---  ~v,,,'-a  ~{~{~v,,,'-a~}~^  ~}~%" name "" (+ (reduce '+ loc) 4) ""
               (mapcar (lambda (x) (list (first x) "")) attributes))
             
             
             (let ((*print-pretty* nil))
               (dolist (x (visicon it))
                 (let ((vis-loc-slots (visual-location-slots x it)))
                   (format s "~v,a  ~3,a  (~v,@a ~v,@a ~v,@a)  "
                     name (chunk-visual-loc x) (feat-attended x it) 
                     (first loc) (fast-chunk-slot-value-fct x (first vis-loc-slots))
                     (second loc) (fast-chunk-slot-value-fct x (second vis-loc-slots))
                     (third loc) (fast-chunk-slot-value-fct x (third vis-loc-slots))
                     )
                   
                   (dolist (z (mapcar (lambda (y)
                                        (let ((v (chunk-slot-value-fct x (second y))))
                                          (list v (first y) (if v v ""))))
                                attributes))
                     (if (first z) 
                         (format  s "~v,s  " (second z) (third z))
                         (format  s "~v,a  " (second z) (third z))))
                   (format s "~%"))))))
         (get-output-stream-string s))
       "#|Warning: No vision module found. |#"))

(add-act-r-command "printed-visicon" 'printed-visicon "Return a string with the printed output of all of the attributes of the features in the model's visicon. No params.")

(defun attend-visual-coordinates (x y &optional distance)
  "Tells the Vision Module to start with attention at a certain location."
  (aif (get-module :vision)
       (if (and (numberp x) (numberp y) (or (null distance) (numberp distance)))
           (let ((vis-loc-slots (bt:with-lock-held ((vis-loc-lock it)) (vis-loc-slots it))))
             (bt:with-lock-held ((marker-lock it))
               (set-current-marker it 
                                   (car (define-chunks-fct `((isa visual-location
                                                                  ,(first vis-loc-slots) ,x
                                                                  ,(second vis-loc-slots) ,y
                                                                  ,(third vis-loc-slots) ,(if (numberp distance) distance (bt:with-recursive-lock-held ((param-lock it)) (view-dist it)))))))))
             t)
         (print-warning "Invalid position value in call to attend-visual-coordinates: ~s ~s ~s" x y distance))
       (print-warning "No vision module found.  Cannot set visual coordinates.")))

(add-act-r-command "attend-visual-coordinates" 'attend-visual-coordinates "Set the current position of the model's visual attention. Params: x y {z}.")

(defun remove-visual-finsts (&optional set-new restuff)
  (let ((vis-m (get-module :vision)))
    (if vis-m
        (bt:with-recursive-lock-held ((visicon-lock vis-m))
          
          (setf (finst-lst vis-m) nil)
          (if set-new
              (mapc (lambda (x)
                      (setf (chunk-visual-tstamp x) (mp-time-ms))
                      (setf (chunk-visual-new-p x) 'new))
                (visicon vis-m))
            (mapc (lambda (x)
                    (setf (chunk-visual-new-p x) 
                      (if (<= (tstamp-elapsed x) (bt:with-recursive-lock-held ((param-lock vis-m)) (new-span vis-m)))
                          'new 
                        nil)))
              (visicon vis-m)))
          (when restuff           
            (stuff-visloc-buffer vis-m))
          nil)
      (print-warning "No vision module found.  Cannot remove the visual finsts."))))

(add-act-r-command "remove-visual-finsts" 'remove-visual-finsts "Remove the finsts from all visicon features and possibly mark everything as new. Params: {set-new {restuff-location?}}")

#| Stuff that isn't being used and probably not updated appropriately but kept around for now...



(extend-chunks visual-feature-name :copy-function identity :merge-function :second-if)




(defun hash-visual-chunk-contents (chunk)
  (let (res)
    (dolist (slot (chunk-filled-slots-list-fct chunk t) res)
      (push (cons slot 
                  (aif (and (eq slot 'value) (chunk-visual-obj-spec chunk))
                       it
                       (true-chunk-name-fct (fast-chunk-slot-value-fct chunk slot))))
            res))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun set-char-feature-set (setname)
  "Sets the feature set used to represent characters when optimizing is off. <setname> should be a keyword."
  (verify-current-mp
   "No current meta-process.  Cannot set a char feature set."
   (verify-current-model 
    "No current model.  Cannot set a char feature set."
    (aif (get-module :vision)
         (if (set-cfs-mth it setname) t nil)
         (print-warning "No vision module found.  Cannot set a char feature set.")))))


(defgeneric set-cfs-mth (vis-mod kwrd)
  (:documentation "Set the current character feature set."))

(defmethod set-cfs-mth ((vis-mod vision-module) kwrd)
  (aif (get-cfs-mth vis-mod kwrd)
       (setf (active-cfs vis-mod) (first it))
       (model-warning "Feature set ~S is unknown" kwrd)))

(defgeneric get-cfs-mth (vis-mod kwrd)
  (:documentation "Given a keyword, find a character feature set."))

(defmethod get-cfs-mth ((vis-mod vision-module) kwrd)
  (member kwrd (feature-sets vis-mod) :key 'name))






;;; CHAR-TO-FEATURES      [Method]
;;; Date        : 99.03.30
;;; Description : For each character, there will usually be many of those
;;;             : CHAR-PRIMITIVE features.  Grab the list of features 
;;;             : associated with a character, and build 'em.

(defgeneric char-to-features (vis-mod char left right y height obj)
  (:documentation  "Returns a list of basic visual-location chunks for a characer"))

(defmethod char-to-features ((vis-mod vision-module) (char character) 
                             (left number) (right number) (y number) 
                             (height number) obj)
  (declare (ignore obj))
  (let* ((xpos (+ left (round (- right left) 2)))
         (width (1+ (- right left)))
         (features (pairlis
                    (getfeats (active-cfs vis-mod) char)
                    (get-icon-feats (active-cfs vis-mod) char)))
         (accum nil)
         (coord-slots (bt:with-lock-held ((vis-loc-lock vis-mod)) (vis-loc-slots vis-mod)))
         (x-slot (first coord-slots))
         (y-slot (second coord-slots)))
    (dolist (feats features accum)
      (when (and (symbolp (car feats)) (not (chunk-p-fct (car feats))))
        (define-chunks-fct `((,(car feats)))))
      (when (and (symbolp (cdr feats)) (not (chunk-p-fct (cdr feats))))
        (define-chunks-fct `((,(cdr feats)))))
      (let ((vl (car (define-chunks-fct `((isa char-primitive
                                               ,y-slot ,y height ,height value ,(cdr feats)
                                               width ,width ,x-slot ,xpos left ,left right ,right))))))
        (push vl accum)
        (setf (chunk-visual-obj-spec vl) (car feats))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Synthesizing 


;;; GET-WORD-AT-NOOPT      [Method]
;;; Date        : 99.04.02
;;; Description : Getting a word when optimizing is off involves calling
;;;             : SYNTHESIZE-WORD method, but we need to collect the 
;;;             : locations first, and also check the return if what was
;;;             : sent in was a DMO.

(defgeneric get-word-at-noopt (vis-mod loc-dmo)
  (:documentation  "Synthesize a word at the given location and synch it with the location."))

(defmethod get-word-at-noopt ((vis-mod vision-module) (loc symbol))
  (let ((xyz-loc (xyz-loc loc vis-mod)))
    (multiple-value-bind (locs xmin xmax) (adjoining-led-locs vis-mod xyz-loc)
      (when locs
        (let ((rtn-chunk (synthesize-word vis-mod locs (- xmax xmin) xyz-loc)))
          (when rtn-chunk
            (set-chunk-slot-value-fct rtn-chunk 'screen-pos loc)
            (values rtn-chunk xmin xmax)))))))


(defmethod get-word-at-noopt ((vis-mod vision-module) (loc vector))
  (multiple-value-bind (locs xmin xmax) (adjoining-led-locs vis-mod loc)
    (when locs
      (let ((rtn-chunk (synthesize-word vis-mod locs  (- xmax xmin) loc)))
        (when rtn-chunk
          (set-chunk-slot-value-fct rtn-chunk 'screen-pos loc)
          (values rtn-chunk xmin xmax))))))


;;; GET-WORD-DMOS-NOOPT      [Method]
;;; Date        : 99.04.02
;;; Description : OK, when optimizing is off and a phrase needs to be built,
;;;             : the tricky bit is figuring out which locations you need
;;;             : to grab words from, since if you hit every x location, 
;;;             : you'll generate multiple copies of each word.

(defgeneric get-word-dmos-noopt (vis-mod x-lst y z)
  (:documentation  "Return a list of DMOs representing words at the given xlocs, with optimizing off."))

(defmethod get-word-dmos-noopt ((vis-mod vision-module) (x-ls list) y z)
  (let ((rtn-dmos nil)
        (curr-x -1))
    (dolist (x x-ls (remove nil (nreverse rtn-dmos)))
      (when (> x curr-x)
        (multiple-value-bind (word min max)
            (get-word-at-noopt vis-mod (vector x y z))
          (declare (ignore min))
          (push word rtn-dmos)
          (setf curr-x max))))))


;;; GET-WORD-DMOS-OPT      [Method]
;;; Date        : 99.04.02
;;; Description : This is simpler when optimizing--just walk the xlist
;;;             : and accumulate words.
;;;             : Might be vestigial as of beta 6.

(defgeneric get-word-dmos-opt (vis-mod x-lst y z)
  (:documentation  "Return a list of DMOs representing words at the given xlocs, with optimizing on."))

(defmethod get-word-dmos-opt ((vis-mod vision-module) (x-ls list) y z)
  (let (accum)
    (dolist (x x-ls (nreverse accum))
      (dolist (feat (text-feats (feat-match-xyz (visicon-chunks vis-mod) (vector x y z) vis-mod)))
        (setf accum (append (featlis-to-object-chunks vis-mod (list feat)) accum))))))




;;; SYNTHESIZE-LETTER      [Method]
;;; Date        : 98.07.27
;;; Description : From a list of LED-style icon features, synthesize a letter
;;;             : feature and note the synthesis.  The real worker here is
;;;             : Mike Matessa's FIND-BEST-OBJECT function, which is based on
;;;             : "rational categorization" of the features into a letter.

(defgeneric synthesize-letter (vis-mod feats)
  (:documentation  "Build a DMO representing a letter from a list of LED features."))

(defmethod synthesize-letter ((vis-mod vision-module) (feats list))
  (let* ((base-feat (first feats))
         (letter (prob-best-character (active-cfs vis-mod) (mapcar (lambda (x) (chunk-slot-value-fct x 'value)) feats)))
         (return-chunk nil)
         (colors (mapcar (lambda (x) (chunk-slot-value-fct x 'color)) feats))
         (color (caar (sort (mapcar (lambda (x) (cons x (count x colors))) (remove-duplicates colors)) '> :key 'cdr))))
    
    (setf return-chunk
      (car (define-chunks-fct (list (list 'isa 'text 'value letter 'color color)))))
    
    (setf (chunk-visual-feature-name return-chunk) base-feat)
    (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry base-feat))
    (setf (chunk-synth-feat return-chunk) (mapcan (lambda (x) (list (chunk-visicon-entry x))) feats))
    return-chunk))


;;; SYNTHESIZE-WORD      [Method]
;;; Date        : 98.07.27
;;; Description : Given the list of contiguous locations, get the letter 
;;;             : at each location, and then build the word from the list
;;;             : of letters.

(defgeneric synthesize-word (vis-mod loc-lis width center)
  (:documentation  "Build a DMO representing a word from a location."))

(defmethod synthesize-word ((vis-mod vision-module) (loc-lis list) (width number) (center vector))
  (let ((return-chunk nil)
        (letter-chunks nil)
        (used-feats nil))
    
    (dolist (xloc loc-lis)
      (let* ((feats (feat-match-xyz (visicon-chunks vis-mod) xloc vis-mod))
             (letter-chunk (synthesize-letter vis-mod feats)))
        
        (when (stringp (chunk-slot-value-fct letter-chunk 'value))
          (push letter-chunk letter-chunks)
          (setf used-feats (append feats used-feats)))))
    
    (when letter-chunks
      (let* ((colors (mapcar (lambda (x) (chunk-slot-value-fct x 'color)) letter-chunks))
             (color (caar (sort (mapcar (lambda (x) (cons x (count x colors))) (remove-duplicates colors)) '> :key 'cdr))))
        
        (setf letter-chunks (nreverse letter-chunks))
        (setf return-chunk
          (car (define-chunks-fct `((isa text color ,color value ,(word-accum 
                                                                   (mapcar (lambda (x) (chunk-slot-value-fct x 'value))
                                                                     letter-chunks)))))))
        
        
        (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry (car used-feats)))
        (setf (chunk-visual-feature-name return-chunk) (car used-feats))
        (setf (chunk-synth-feat return-chunk) (mapcan (lambda (x) (list (chunk-visicon-entry x))) used-feats))
        
        return-chunk))))


(defun word-accum (lis)
  "Accumulate a list of letters into a word, and downcase it."
  (let ((accum (first lis)))
    (dolist (item (rest lis) (string-downcase accum))
      (setf accum (mkstr accum item)))))

;;; GET-PHRASE-AT      [Method]
;;; Date        : 98.07.27
;;; Description : The only way to get a phrase is to synthesize one, there is
;;;             : no "phrase" primitive in RPM.


(defmethod get-phrase-at ((vis-mod vision-module) (loc symbol))
  (awhen (find-matching-chunks (define-chunk-spec-fct `(isa visual-location ,(second (visual-location-slots loc vis-mod)) ,(py (xy-loc loc vis-mod))))
                               :chunks (visicon-chunks vis-mod))
         
         (synthesize-phrase vis-mod it loc)))


(defgeneric synthesize-phrase (vis-mod feature-locs loc)
  (:documentation  "Build a DMO representing a phrase."))

#|  Need to update this with appropriate visual-location-slots change, but need to figure it out first!

(defmethod synthesize-phrase ((vis-mod vision-module) (feature-locs list) (loc symbol))
  (let* ((xyz-loc (xyz-loc loc vis-mod))
         (x-slot (first (visual-location-slots loc vis-mod)))
         (x-locs (mapcar (lambda (x) (chunk-slot-value-fct x x-slot)) feature-locs))
         (word-chunks nil) 
         (words nil)
         (colors nil)
         (return-chunk nil))
    (setf x-locs (sort (remove-duplicates x-locs) '<))
    
    (if (bt:with-recursive-lock-held ((param-lock vis-mod)) (optimize-p vis-mod))
        (setf word-chunks (get-word-dmos-opt vis-mod x-locs (py xyz-loc) (pz xyz-loc)))
      (setf word-chunks (get-word-dmos-noopt vis-mod x-locs (py xyz-loc) (pz xyz-loc))))
    
    (when word-chunks
      (setf words (mapcar (lambda (x)
                            (chunk-slot-value-fct x 'value)) 
                    word-chunks))
      
      (dolist (w word-chunks)
        (let ((c (chunk-slot-value-fct w 'color)))
          
          (aif (assoc c colors) 
               (incf (cdr it))
               (push-last (cons c 1) colors))))
      
      (setf return-chunk
        (car (define-chunks-fct 
                 `((isa phrase! value ,(phrase-accum words)
                        objects ,word-chunks 
                        colors ,(mapcar (lambda (x) (chunk-slot-value-fct x 'color)) word-chunks)
                        words ,words
                        color ,(car (rassoc (apply 'max (mapcar 'cdr colors)) colors))
                        screen-pos ,loc)))))
      
      (setf (chunk-visicon-entry return-chunk) (chunk-visicon-entry loc))
      (setf (chunk-visual-feature-name return-chunk) loc) 
      
      (setf (chunk-synth-feat return-chunk) 
        (if (bt:with-recursive-lock-held ((param-lock vis-mod)) (optimize-p vis-mod))
            (mapcan (lambda (x) (list (chunk-visicon-entry x))) word-chunks)
          (mapcan (lambda (x) (chunk-synth-feat x)) word-chunks)))
      
      return-chunk)))
|#

(defun phrase-accum (lis)
  "Accumulate a list of words into a phrase."
  (let ((accum (first lis)))
    (dolist (item (rest lis) accum)
      (setf accum (mkstr accum " " item)))))




;;;;;;;;;;
;;;; ---------------------------------------------------------------------- ;;;;
;;;;  Those wacky LED features for letters.
;;;; ---------------------------------------------------------------------- ;;;;


;;; ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : To synthesize a word, we need to know which locations have
;;;             : LED-LINE features at adjoining locations.  However, 
;;;             : because letter width is variable, the icon has to be
;;;             : searched for LED-LINE locations that have features that
;;;             : are near the boundaries.  Also, because we need to know the
;;;             : width of the word, return the min and max x values, too.

(defgeneric adjoining-led-locs (vis-mod loc)
  (:documentation  "Return a list of locations adjoining <loc>, as well as the max and min x locs."))

#| Needs to be reworked with appropriate vis-loc-slots call, but for what?  should it default to screen-y only?

(defmethod adjoining-led-locs ((vis-mod vision-module) (loc vector))
  (let ((feat-ls (feat-match-xyz (visicon-chunks vis-mod) loc vis-mod))
        (feat nil)
        (y-slot (second (visual-location-slots ??? vis-mod)))))
    (while (and (null feat) feat-ls)
      (setf feat (pop feat-ls))
      (unless (eq (chunk-slot-value-fct feat 'kind) 'char-primitive)
        (setf feat nil)))
    (when feat
      (setf feat-ls (find-matching-chunks (define-chunk-spec-fct `(isa visual-location ,y-slot ,(chunk-slot-value-fct feat y-slot)))
                                          :chunks (visicon-chunks vis-mod)))
      (multiple-value-bind 
            (lowlocs xmin) (left-adjoining-led-locs feat-ls vis-mod (chunk-slot-value-fct feat 'left) nil)
        (multiple-value-bind
              (hilocs xmax) (right-adjoining-led-locs feat-ls vis-mod (chunk-slot-value-fct feat 'right) nil)
          (values
           (append lowlocs (list loc) hilocs)
           xmin xmax))))))
|#

;;; RIGHT-ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : Recursively go right and accumulate locations that share
;;;             : the boundary.

(defgeneric right-adjoining-led-locs (feat-ls vis-mod x accum)
  (:documentation  "Return a list of all the right-adjoining locs with led features and the min x."))

(defmethod right-adjoining-led-locs ((feat-ls list) (vis-mod vision-module) x accum)
  (dolist (feat feat-ls)
    (when (eq (chunk-slot-value-fct feat 'kind) 'char-primitive)
      (when (> 1.5 (abs (- (chunk-slot-value-fct feat 'left) x)))
        (return-from right-adjoining-led-locs
          (right-adjoining-led-locs feat-ls vis-mod (chunk-slot-value-fct feat 'right)
                                    (append accum (list (xyz-loc feat vis-mod))))))))
  (values accum x))


;;; LEFT-ADJOINING-LED-LOCS      [Method]
;;; Date        : 99.04.02
;;; Description : Recursively go left and accumulate locations that share
;;;             : the boundary.

(defgeneric left-adjoining-led-locs (feat-ls vis-mod x accum)
  (:documentation  "Return a list of all the left-adjoining locs with led features and the max x."))

(defmethod left-adjoining-led-locs ((feat-ls list) (vis-mod vision-module) x accum)
  (dolist (feat feat-ls)
    (when (eq (chunk-slot-value-fct feat 'kind) 'char-primitive)
      (when (> 1.5 (abs (- (chunk-slot-value-fct feat 'right) x)))
        (return-from left-adjoining-led-locs
          (left-adjoining-led-locs feat-ls vis-mod (chunk-slot-value-fct feat 'left)
                                   (append (list (xyz-loc feat vis-mod)) accum))))))
  (values accum x))




;;   Not used now, but keeping around since EMMA used it and will need it
;;   for reference when updating that.

;;; OBJECT-PRESENT-P      [Method]
;;; Description : A visual object is present if its ID is still in the icon
;;;             : or if all the features from which it was synthesized are
;;;             : still in the icon.

(defgeneric object-present-p (vis-mod obj-id)
  (:documentation  "Returns NIL if the object ID passed to it is no longer in the icon."))

(defmethod object-present-p ((vis-mod vision-module) obj-id)
  (bt:with-recursive-lock-held ((visicon-lock vis-mod))
    (aif (chunk-synth-feat obj-id)
         ;; For an object synthesized from features check all the features
         ;; broken now since visicon not a hash table
         (every (lambda (x) (gethash (chunk-visicon-entry x) (visicon vis-mod))) it)
         (gethash (chunk-visicon-entry obj-id) (visicon vis-mod)))))


|#



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
