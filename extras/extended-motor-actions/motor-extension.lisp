;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2014 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : motor-extension.lisp
;;; Version     : 6.1
;;; 
;;; Description : Collection of extensions for the motor module which include:
;;;             : separate processor and/or execution stages for each hand, buffers
;;;             : to track the separate hand usage, actions for holding keys,
;;;             : and "press-key like" actions for holding keys and for hitting
;;;             : a key without recoiling to the home row.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Should a point-finger action send a notification?
;;;             :     Currently the normal point-hand actions don't so not
;;;             :     doing that here either, but if that changes need to make
;;;             :     the change here as well.
;;;             :     
;;; 
;;; ----- History -----
;;; 2014.04.24 Dan [1.0a1]
;;;             : * Initial creation.  Combining the separate extensions that
;;;             :   were used for modeling Space Fortress, cleaning them up so
;;;             :   that the "regular" motor actions work correctly with them,
;;;             :   and adding the new "typing" actions.
;;; 2014.05.16 Dan [1.1a1]
;;;             : * This version is for ACT-R 6.1.
;;;             : * Fixed all the calls to check-specs to pass the name.
;;;             : * Commented out the handle-extended-style method and use
;;;             :   handle-style-request for the actions instead.
;;;             : * Changed the verify-single-explicit-value calls because of
;;;             :   the different way it works now.
;;; 2014.06.16 Dan [2.0a1]
;;;             : * Fix prepare-movement for typeless chunk mechanism so that
;;;             :   setting of the hands' last command values still works.
;;; 2015.06.05 Dan
;;;             : * Change all the scheduling to ms.
;;; 2015.08.06 Dan [2.0a2]
;;;             : * Adding a release-all-fingers action which is special in that
;;;             :   it doesn't jam the module because it will reschedule itself
;;;             :   if the preparation is busy repeatedly until it becomes free
;;;             :   and then after the init cost it will call the release action
;;;             :   for each finger which is down.
;;; 2015.08.07 Dan
;;;             : * Cleaning up the distinction between up/down and busy/free
;;;             :   for a finger -- those were conflated before because it
;;;             :   said set-finger-up but what it really meant was set finger
;;;             :   free.  There wasn't a real indication of being down and one
;;;             :   could mistake a busy finger for down.
;;;             :   Since busy/free is measured at the "hand" level the fingers
;;;             :   don't need to really indicate busy/free, just up or down,
;;;             :   but it may be necessary to detect intermediate states at the
;;;             :   finger level i.e. that a finger is making an action even 
;;;             :   though the key isn't up or down yet.  So actually tracking
;;;             :   both separately for the fingers.  A query for a finger of
;;;             :   busy means that it's between the initiation and finish 
;;;             :   stages of an action (like the hand would indicate) whereas
;;;             :   a query of up or down indicates where the finger is with
;;;             :   respect to the key it's over.
;;;             : * Actually implementing device-handle-keyrelease and device-
;;;             :   handle-click-release methods instead of only calling the
;;;             :   event handler functions directly so that a new device can
;;;             :   receive that info appropriately.
;;;             : * The replacement motor module now has a slot for the extension
;;;             :   module.
;;;             : * Instead of a style directly scheduling the output-key and 
;;;             :   release-key methods it needs to call the key-up and key-down 
;;;             :   methods which will then call output-key and release-key.
;;; 2015.08.10 Dan
;;;             : * Fixing bugs with the previous updates.
;;; 2015.08.13 Dan
;;;             : * The device-handle-click-release method only has one parameter
;;;             :   now, the device, like device-handle-click.
;;;             : * The finger up/down queries are now available.
;;;             : * Changed rand-time calls to randomize-time.
;;; 2015.08.19 Dan
;;;             : * Only generate the press or release if the finger isn't in
;;;             :   that state i.e. can't have two presses without a release
;;;             :   between them.
;;; 2015.09.23 Dan [3.0]
;;;             : * Make the necessary updates to be compatible with the new
;;;             :   request completion changes to the movement styles and
;;;             :   change the new action functions to work with those updates.
;;; 2015.09.25 Dan
;;;             : * Release-all-fingers wasn't recording the request for completion.
;;; 2018.02.19 Dan [3.1]
;;;             : * Delay checking that the finger is down for the release actions
;;;             :   until execution time because it could prepare it while the
;;;             :   finger was still executing the down stroke.
;;; 2018.10.09 Dan [4.0]
;;;             : * Updating this for 7.x.  Lots of changes necessary.
;;;             : * Adding the delayed-punch extension in as well.
;;; 2018.10.11 Dan [4.1]
;;;             : * Added another parameter: :cursor-drag-delay which slows down
;;;             :   cursor movement when a finger on the same hand is also held
;;;             :   down.  Defaults to nil (off), can be set to t (.8) or a #
;;;             :   which is added to the control order for the Fitts coefficient
;;;             :   calculation.  The .8 value seems to be a reasonable value
;;;             :   for fitting data from the paper below with other defaults:
;;;             :   I. Scott MacKenzie, Abigail Sellen, and William A. S. Buxton. 1991. 
;;;             :   A comparison of input devices in element pointing and dragging tasks.
;;;             :   In Proceedings of the SIGCHI Conference on Human Factors in 
;;;             :   Computing Systems (CHI '91), Scott P. Robertson, Gary M. Olson,
;;;             :   and Judith S. Olson (Eds.). ACM, New York, NY, USA, 161-166. 
;;;             :   DOI: http://dx.doi.org/10.1145/108844.108868 
;;; 2018.10.12 Dan
;;;             : * Fixed the status functions since they need to return a string
;;;             :   now.
;;;             : * Fixed the cursor-ply class because it needs a cursor slot.
;;;             : * Changed all the 'old' style methods to be on motor-mod class
;;;             :   instead of the new class to avoid oddities in finding the
;;;             :   right method.
;;;             : * It's much worse than that... Basically, I can't fall back to
;;;             :   a general dual queue-output-events because the 'old' specific
;;;             :   queue-output-events get in the way.  So I have to define 
;;;             :   every method which was defined for the old version here too
;;;             :   (or go through some ugly remove-method hoops to get rid of
;;;             :   what I don't want, or start from scratch and ignore the
;;;             :   movement-style class at the top, or probably some other 
;;;             :   ugly thing I haven't considered).
;;; 2018.10.15 Dan
;;;             : * Fixed an issue with the mouse drag timing (it was checking
;;;             :   for busy instead of down).
;;; 2018.10.16 Dan
;;;             : * All-home needs to just move the fingers because if it uses
;;;             :   point-finger it will jam if there're multiple fingers to
;;;             :   move, and it has to do it at the finish time so that any
;;;             :   fingers which are down are released first.
;;;             : * Fixed a bug in queue-output events for a peck-recoil.
;;;             : * The style name for all peck and peck-recoil type actions is
;;;             :   just peck (as is true for the normal actions).
;;;             : * Renamed key-up and key-down set-finger-up and set-finger-down.
;;; 2018.10.17 Dan
;;;             : * Adjust how I handle the :peck-strike-distance calculation
;;;             :   to not assume linear motion and fix a bug in the calculation.
;;;             :   Now it uses the minimum jerk trajectory profile to find the
;;;             :   appropriate time to get to the striking distance, and it's
;;;             :   using a simple search using minjerk-dist to find that time
;;;             :   since it's a 5th order polynomial so there's no closed form
;;;             :   solution for time and a "fancy" solution is going to be at
;;;             :   least as costly as a few calls (typically all that's needed
;;;             :   to get close enough: +/- .01 key width).
;;; 2019.08.08 Dan
;;;             : * Fixed a typo in a parameter doc string.
;;; 2020.01.10 Dan [4.2]
;;;             : * Removed the #' and lambdas from param valid functions since 
;;;             :   that's not allowed in the general system now.
;;; 2020.04.06 Dan [5.0]
;;;             : * Updated with the changes necessary to track the separation 
;;;             :   of current position and next starting position as well as
;;;             :   add the request time to the standard events as the normal
;;;             :   motor module does now.
;;;             : * Changed point-finger to use an exec-time that matches when
;;;             :   the finger is "at" the key (init + fitts time).  That means
;;;             :   there's an extra chance for a conflict-resolution at that
;;;             :   time relative to before, which seems reasonable -- when the
;;;             :   finger gets there maybe something else could happen.
;;;             : * Changed all-fingers-to-home to both mark all the fingers as
;;;             :   busy during the action and to have a time that's the max of
;;;             :   200ms and the times for each hand to do a hand-ply back to
;;;             :   home position.
;;;             : * The release on a punch doesn't include the extra burst cost
;;;             :   now to be consistent with the timing of peck and peck-recoil
;;;             :   actions (a 90ms down time was too long).
;;;             : * The timing of the release-all-fingers event now matches 
;;;             :   when the release happens instead of a burst time later.
;;;             : * Go back to a single exec queue which now is searched for
;;;             :   an available action, possibly for each hand, since the two
;;;             :   queue approach had issues with skipping over a both when
;;;             :   one of the hands was free.
;;; 2020.04.20 Dan
;;;             : * Updated to work with the new location information in the
;;;             :   keyboard.
;;; 2020.05.21 Dan [5.1]
;;;             : * Added a new parameter, :relay-hand-requests which if set to
;;;             :   t will pass requests to manual-left and manual-right on to
;;;             :   the manual buffer.  The requests must still include a hand
;;;             :   feature when required because they can't just add one in
;;;             :   since it isn't possible to know when that is required in
;;;             :   advance.  If a hand feature is included in the request, it
;;;             :   must match the buffer to be relayed.  Enabling this switch
;;;             :   will affect the buffer trace information because the request
;;;             :   for an action will only be recorded for the specific buffer
;;;             :   which makes the request and not the buffer which corresponds
;;;             :   to the hand that is used (for example a press-key request
;;;             :   which only generates the hand after preparing the features
;;;             :   could be attributed to the wrong buffer).
;;; 2020.06.24 Dan [6.0]
;;;             : * Updated the create-motor-module function because now it must
;;;             :   also specify the types.
;;; 2020.10.12 Dan [6.1]
;;;             : * Adjust the actions that are scheduling the prepare-movement
;;;             :   so it displays in the low detail trace.  Prior to the fix
;;;             :   for preparation free they were all just calling it and had
;;;             :   the event shown automatically in the low detail trace.
;;; 2021.01.08 Dan 
;;;             : * Didn't update the module version with the last change.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Combining a lot of updates to the motor module together in one place which
;;; should be useable as a replacement for the default module without breaking
;;; any existing models which use motor actions.  
;;; 
;;; The big changes with the update:
;;;
;;; The model can now generate actions for pressing and releasing keys which can
;;; be detected by the modeler.  The actions from the existing module (punch,
;;; peck, peck-recall, and press-key) all now result in both a press and release
;;; of the key/button.  
;;;
;;; With the addition of keys being able to be held down the previous motor module
;;; actions (the styles punch, peck, peck-recoil, and point-hand and all the 
;;; higher-level actions which use them press-key, click-mouse, hand-to-home,
;;; hand-to-mouse, and point-hand-at-key) have been modified so that if the finger(s)
;;; used to perform the action is currently holding a key down there's an extra 
;;; burst-time cost to execute the release of that key before performing the
;;; specified action. 
;;;
;;; The trace of the motor actions for preparing, initiating, and finishing an
;;; action now indicate which action and hand it corresponds to since there may
;;; be parallel actions on going. (The old module also could have some overlapping
;;; actions which occasionally lead to confusion in the trace, but it didn't 
;;; happen very often).
;;;
;;; The new motor module now has the option of using separate execution and/or 
;;; processor stages for the two hands which allows motor actions on different 
;;; hands to potentially operate in parallel.  There is still only one preparation
;;; stage however.  The default operation of the new module is for one execution
;;; stages and one processor stage, like the standard motor module, but that can
;;; be changed with parameters described below.
;;;
;;; There are two new sets of motor actions which can be used.  The first set
;;; provides separate low-level actions for holding and releasing keys.  The other 
;;; set is more high-level actions like press-key which perform touch typing 
;;; operations with holding keys and without always recoiling to the home row.
;;; Details on the new actions are found below.
;;;
;;; There is a new module created, named motor-extension, which adds two new
;;; buffers, manual-left and manual-right.  These can be used for tracking the 
;;; operation of the model's hands separately and also allows for querying 
;;; whether individual fingers are currently busy i.e. holding down a key.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; release-key (model key)
;;;
;;; the signal that can be monitored to detect the release of a key just like
;;; output-key notes the pressing.  Generated by the new default keyboard device
;;; included with this extension.
;;;
;;; release-mouse (model position finger)
;;; 
;;; The signal that can be monitored to detect the release of a button on
;;; the mouse like click-mouse notes the press.  Generated by the new default
;;; mouse cursor device included with this extension.
;;; 
;;; 
;;; New model parameters:
;;;
;;; :key-closure-time (default value .01)
;;; 
;;; The time in seconds between when a key is contacted (the corresponding movement 
;;; action has started pressing it) and when it registers as being hit.  By default
;;; only used for the timing of punch actions, but see the :peck-strike-distance
;;; parameter below for how it may affect peck/peck-recoil actions as well.  Note,
;;; this value has always existed within the device interface, but was not previously
;;; made directly available to the modeler.
;;;
;;; :key-release-time (default value .04)
;;;
;;; The time in seconds between when the action which is releasing a key is started
;;; and when that key actually registers as being released.  Used in the timing of
;;; all the actions which include a releasing of a key.  The assumption is that there
;;; is some distance over which the finger must be moved before the key stops being
;;; pressed.  All of the release actions are based on a burst-time cost, thus this
;;; parameter should be set to a value no greater than the value of the :MOTOR-BURST-TIME
;;; parameter.  
;;;
;;; NOTE: Those two parameters should really be modeled as a distance instead of a time
;;; cost, but since the motor movement code isn't modeled at that fine grain of detail
;;; (exact finger positions in 3-space) a simple cost assumption is used.
;;;
;;; :dual-processor-stages (default value nil)
;;;
;;; A boolean indicating whether the motor module should use separate processor
;;; stages for each hand.
;;;
;;; :dual-execution-stages (default value nil)
;;;
;;; A boolean indicating whether the motor module should use separate execution
;;; stages for each hand.
;;;
;;; :peck-strike-distance (default 0.0)
;;; 
;;; The distance from the center of a key at which a peck action is considered
;;; to make contact with the key and start depressing it.  The value can be
;;; from 0 to .5 (the width of a key is 1.0 and movement is computed from 
;;; center of key to center of key thus the finger movement crosses at least
;;; half of the target key when striking it).  This affects when the output-key
;;; action occurs.  The default motor module assumes that the action occurs 
;;; after the entire movement completes and ignores the key closure time which
;;; is what will still happen if the value is set to nil.  
;;; 
;;; If it is set to a non-nil value then the peck and peck-recoil actions
;;; will generate the output-key action at the time: ct + t(d - psd) where
;;; ct is the key-closure-time, t(x) is the time along a minimum jerk velocity
;;; profile for the whole movement at which the distance is x, d is the
;;; length of the whole movement and psd is the value of :peck-strike-distance.
;;; 
;;; :default-punch-delay (.075s default)
;;; 
;;; This is the delay time for a delayed-punch action when the model doesn't 
;;; specify a delay.
;;; 
;;; :fast-punch-delay (.05s default)
;;; 
;;; This is the delay time for a delayed-punch action when the model specifies
;;; a delay of fast.
;;; 
;;; :slow-punch-delay (.1s default)
;;; 
;;; This is the delay time for a delayed-punch action when the model specifies
;;; a delay of slow.
;;;
;;;
;;; :cursor-drag-delay (nil default)
;;; 
;;; Indicates whether to slow down cursor drag actions, and if so by how much.
;;; If set to t or a number then that number is added to the control order
;;; value of the cursor before calculating the time with Fitts' Law.  A value
;;; of t sets it to .8 which provides a reasonable match to data from this paper:
;;; 
;;; I. Scott MacKenzie, Abigail Sellen, and William A. S. Buxton. 1991. 
;;; A comparison of input devices in element pointing and dragging tasks.
;;; In Proceedings of the SIGCHI Conference on Human Factors in 
;;; Computing Systems (CHI '91), Scott P. Robertson, Gary M. Olson,
;;; and Judith S. Olson (Eds.). ACM, New York, NY, USA, 161-166. 
;;; DOI: http://dx.doi.org/10.1145/108844.108868 

;;;
;;; New Buffers for tracking hands: 
;;; 
;;; manual-left and manual-right
;;; 
;;; Each buffer allows for the same queries as the manual buffer except for
;;; the preparation query which it does not track.  The processor and execution
;;; queries of these buffers are busy if the corresponding hand is performing an
;;; action (or if either hand is when there aren't separate stages).  The state
;;; is busy if there is an action being prepared or if the processor or execution
;;; for this hand is busy.
;;; Each also adds additional queries for the fingers on that hand.  A finger
;;; query of busy is true if the finger is currently involved in an action which
;;; is occurring (actions which affect the whole hand do not involve the individual
;;; fingers) otherwise busy is false and free is true.
;;; A query of the finger for up will be true if the finger is not currently holding
;;; down a key and false if it is not.
;;; A query of the finger for down will be true if the finger is currently holding
;;; down a key and false if it is not.
;;; Down and up are mutually exclusive, eventhough there is potentially a non-zero
;;; time period during which the key and finger is transitioning from up to down or
;;; vice-versa the state changes at the same time as the corresponding output method
;;; gets called i.e. at the "real" make/break point of the press.
;;;
;;; Here is the output of the buffer-status call for the manual-left buffer for
;;; reference:
;;;
;;; MANUAL-LEFT:
;;;   buffer empty          : T
;;;   buffer full           : NIL
;;;   buffer failure        : NIL
;;;   buffer requested      : NIL
;;;   buffer unrequested    : NIL
;;;   state free            : T
;;;   state busy            : NIL
;;;   state error           : NIL
;;;   processor free        : T
;;;   processor busy        : NIL
;;;   execution free        : T
;;;   execution busy        : NIL
;;;   last-command          : ALL-HOME
;;;   index  free           : T
;;;   index  down           : NIL
;;;   middle free           : T
;;;   middle down           : NIL
;;;   ring   free           : T
;;;   ring   down           : NIL
;;;   pinkie free           : T
;;;   pinkie down           : NIL
;;;   thumb  free           : T
;;;   thumb  down           : NIL
;;;
;;;
;;; Neither buffer does anything with requests which it receives nor are there any 
;;; chunks placed into either one.
;;;
;;;
;;;
;;; New request actions for the manual buffer.
;;;
;;; Here are low-level actions for directly manipulating the fingers:
;;;
;;; cmd hold-punch
;;;  hand [left | right]
;;;  finger [index | middle | ring | pinkie | thumb]
;;; 
;;; Press and hold down the indicated finger.  If the finger
;;; is already being held release it and press down again.
;;;
;;;
;;; cmd hold-peck
;;;  hand [left | right]
;;;  finger [index | middle | ring | pinkie | thumb]
;;;  r distance
;;;  theta direction
;;;
;;; Move the finger based on the distance and direction and hold it down 
;;; at that location.  If the finger is currently being held release it
;;; before performing this movement.
;;;
;;;
;;; cmd release
;;;  hand [left | right]
;;;  finger [index | middle | ring | pinkie | thumb]
;;;
;;; Stop holding down the indicated finger and leave it where it
;;; currently is.  If the finger is not being held down do nothing and
;;; output a warning.
;;;
;;;
;;; cmd release-recoil
;;;  hand [left | right]
;;;  finger [index | middle | ring | pinkie | thumb]
;;;  r distance
;;;  theta direction
;;;
;;; Stop holding down the indicated finger and then move it based on the
;;; distance and direction given without performing a press when it 
;;; completes the movement.  If the finger is not being held down do nothing and
;;; output a warning.
;;;
;;; cmd delayed-punch
;;;  hand [ left | right ]
;;;  finger [ index | middle | ring | pinkie | thumb ]
;;;  {delay [ fast | slow | # ] }
;;; 
;;; Press down and then release the indicated finger in its current
;;; location.  The release will occur delay seconds after the press
;;; if delay is provided as a number.  If delay is not provided then 
;;; the default delay time is used.  If delay is either fast or slow, 
;;; then the value is taken from the corresponding parameter.  If
;;; :randomize-time is enabled then the delay will be randomized
;;; regardless of how it was indicated.  If the finger is currently being
;;; held release it before performing this movement.

;;;
;;; cmd point-finger
;;;  hand [left | right]
;;;  finger [index | middle | ring | pinkie | thumb]
;;;  r distance
;;;  theta direction
;;;
;;; Move the finger based on the distance and direction without performing a
;;; press at that location.  If the finger is currently being held down release 
;;; it before performing this movement.
;;;
;;; 
;;; Here are the new high-level actions for the updated keyboard and mouse
;;; devices which generate the corresponding low-level movements as necessary.
;;;
;;;
;;; isa type-key
;;;  key key
;;;
;;; Move the appropriate finger to the key and press and release it.  The
;;; finger is moved from where it currently is (unlike press-key which assumes
;;; it's on the home position) and returns to where it was after striking the key
;;; (which may not be the home row position). If the finger is currently being 
;;; held release it before performing this movement.

;;;
;;;
;;; isa hit-key
;;;  key key
;;;
;;; Move the appropriate finger to the key and press and release it.  The
;;; finger is moved from where it currently is (unlike press-key which assumes
;;; it's on the home position) and stays over the key which is hit.  If the finger
;;; is currently being held release it before performing this movement.

;;;
;;;
;;; isa hold-key
;;;  key key
;;;
;;; Move the appropriate finger to the key and press it keeping it held down.  The
;;; finger is moved from where it currently is (unlike press-key which assumes
;;; it's on the home position).  If the finger is currently being held release it
;;; before performing this movement.

;;;
;;;
;;; isa release-key
;;;  key key
;;;
;;; If the appropriate finger for that key is currently holding it down release it
;;; and leave the finger at that position.  If the finger is not holding down that
;;; key print a warning and do nothing.
;;;
;;;
;;; isa release-key-to-home
;;;  key key
;;;
;;; If the appropriate finger for that key is currently holding it down release it
;;; and move the finger back to its home position without striking the corresponding
;;; home key.  If the finger is not holding down that key print a warning and do nothing.
;;;
;;;
;;; isa move-to-key
;;;  key key
;;;
;;; Move the appropriate finger to that key without pressing it.  The finger is moved
;;; from where it currently is (unlike press-key which assumes it's on the home position).
;;; If the finger is currently being held release it before performing this movement.

;;;
;;; isa move-finger-to-home
;;;  hand [left | right]
;;;  finger [index | middle | ring | pinkie | thumb]
;;;
;;; Move the specified finger to its home position without pressing it.  If the finger
;;; is currently holding a key down release it before moving.  If the finger is already
;;; at the home position do nothing and print a warning.
;;;
;;; 
;;; isa all-fingers-to-home
;;;  {hand [left | right]}
;;;
;;; Move all fingers on the specified hand, or both hands if no hand specified,
;;; to their home positions.  Like move-finger-to-home any finger which is holding
;;; a key will be released first.  If all the fingers are on the home positions
;;; do nothing and print a warning.  This action has a fixed cost of 200ms.  It 
;;; does not move the hand(s) only the fingers.
;;;
;;;
;;; isa hold-mouse
;;;
;;; Execute a hold-punch with the right index finger to depress the primary mouse button.
;;; If the hand is not located on the mouse print a warning and do nothing.
;;;
;;;
;;; isa release-mouse
;;;
;;; Release the right index finger to release the primary mouse button.  If the 
;;; finger is not holding the button down or the hand is not located on the
;;; mouse do nothing and print a warning.
;;;
;;; isa release-all-fingers
;;; 
;;; Causes all fingers which are currently being held down to release.  The
;;; action doesn't jam and will wait until preparation is free to start the 
;;; action.  After the init time passes all fingers which are held will be 
;;; released after the key-release-time and then an additional burst cost will
;;; be applied before it is finished.
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
;;; Define a new keyboard and make it the default.  This keyboard extends the
;;; available notifications to handle the releases and generates a release-key
;;; signal.

(defclass keyboard-with-release (virtual-keyboard)
  nil)

(set-default-keyboard 'keyboard-with-release)

(defmethod other-notifications ((k keyboard-with-release) device features)
  (let ((m (second (find 'model features :key 'first)))
        (l (second (find 'loc features :key 'first)))
        (s (second (find 'style features :key 'first))))
    (if (and m l s (find s '(release)))
        (aif (loc->key k l)
             (with-model-eval m
               (schedule-event-now "release-key" :params (list m it)  :output t :module 'keyboard  :maintenance t))
             (print-warning "Keyboard device did not get signaled with a valid keyboard location for release action: ~s" features))
      (print-warning "keyboard device can not process notification from device ~s with features ~s." device features))))


(add-act-r-command "release-key" nil "Command called when a key on the virtual keyboard is released by the model which can be monitored.  It is passed 2 parameters: model-name and key-name. Should not be called directly.")


;;; Define a new mouse cursor and make it the default.

(defclass mouse-with-release (mouse)
  nil)

(set-default-mouse 'mouse-with-release)

(defmethod other-notifications ((mouse mouse-with-release) device features)
  (let ((m (second (find 'model features :key 'first)))
        (s (second (find 'style features :key 'first)))
        (h (second (find 'hand features :key 'first)))
        (l (second (or (find 'loc features :key 'first)
                       (find 'new-loc features :key 'first)))))
    
    (if (and m s h l (find s '(release))) ;; the only valid notification that should get here
        (let (mx my xyz)
          (bt:with-lock-held ((cursor-lock mouse))
            (setf mx (px (cursor-pos mouse))
              my (py (cursor-pos mouse))
              xyz (cursor-loc mouse)))
          
          (let* ((x (px l)) (y (py l))
                 (direction (if (eq h 'left) -1 1))
                 (f (cond ((and (= x mx) (= y my))
                           'index)
                          ((and (= x (+ mx direction)) (= y my))
                           'middle)
                          ((and (= x (+ mx (* direction 2))) (= y my))
                           'ring)
                          ((and (= x (+ mx (* direction 3))) (= y my))
                           'pinkie)
                          ((and (= x (- mx direction)) (= y (+ my 2)))
                           'thumb)
                          (t
                           (print-warning "Invalid mouse release finger position ~s." l)))))
            (when f
              (with-model-eval m
                (schedule-event-now "release-mouse" :params (list m xyz f) :maintenance t :module 'mouse)))))
      (print-warning "mouse device can not process notification from device ~s with features ~s." device features))))


(add-act-r-command "release-mouse" nil "Command called when a button on the virtual mouse is released by the model which can be monitored.  It is passed 3 parameters: model-name mouse-position finger. Should not be called directly.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code to implement a replacement for the standard motor module
;;; which tracks the processor and execution stages separately for
;;; left and right hand actions.  There is still only one preparation
;;; stage for the whole module.
;;; 
;;; There isn't an easy way to "extend" the default module without
;;; replacing it since this changes some fundamental operations which
;;; can't just be provided through some new hooks into the default
;;; module.  
;;;
;;; Instead of undefining and creating a new one however, this is 
;;; going to be based on how it's actually defined (which means that
;;; changes to the module will still likely require changes here too).
;;; Do this by subclassing the motor-module class, redefine the creation
;;; function for the motor module to instantiate this class instead, 
;;; and then define the new and updated methods on that new class.
;;;
;;; The only functional difference is that now the proc-s and exec-s slots
;;; hold a list of two items (left and right) respectively instead of just one
;;; and actions for the exec-queue must have a hand specified (either left,
;;; right, or both).
;;;
;;; Also adding slots for holding the new parameters that are available
;;; for configuring the operation and adjusting the timing of the
;;; keypress actions as well as for holding the instance of the companion
;;; module for tracking the hands for convenience.


(defclass dual-execution-motor-module (motor-module) 
  ((key-closure-time :accessor key-closure-time)
   (key-release-time :accessor key-release-time)
   (two-exec :accessor two-exec)
   (two-proc :accessor two-proc)
   (peck-strike :accessor peck-strike :initform nil)
   (extension :accessor extension)
   (d-punch-default :accessor d-punch-default)
   (d-punch-fast :accessor d-punch-fast)
   (d-punch-slow :accessor d-punch-slow)
   (drag-delay :accessor drag-delay)
   (relay-hand-requests :accessor relay-hand-requests))
  (:default-initargs
    :version-string "6.1-dual"))

;;; Redefine the creation function for the motor module to create the new class.

(defun create-motor-module (model-name)
  (declare (ignore model-name)) 
  
  (chunk-type motor-command (cmd "motor action"))
  (chunk-type (prepare (:include motor-command)) (cmd prepare) style hand finger r theta)
  
  (unless (chunk-p prepare)
    (define-chunks (prepare isa prepare)))
  
  
  (dolist (c '(left right index middle ring pinkie thumb))
    (unless (chunk-p-fct c)
      (define-chunks-fct (list (list c 'name c)))))
  
  
  (let ((instance (make-instance 'dual-execution-motor-module)))
    
    ;; Define the chunk-types for the specified extensions
    ;; Note this means that for new extensions to be applied the
    ;; model must be redefined/reloaded not just reset
    
    (maphash (lambda (name value)
               (let ((type (chunk-type-fct (car value))))
                 (if type
                     (unless (chunk-p-fct name)
                       (define-chunks-fct (list (list name 'isa name))))
                   (print-warning "Failed to extend motor capabilities with chunk-type definition: ~s" (car value)))))
             (bt:with-lock-held ((requests-table-lock instance)) (new-requests-table instance)))
    
    instance))


;;; Set necessary slots to lists after the regular methods happen

(defmethod clear :after ((module dual-execution-motor-module))
  (bt:with-recursive-lock-held ((state-lock module))
    (setf (exec-queue module) nil)))

(defmethod reset-pm-module :after ((module dual-execution-motor-module))
  (bt:with-recursive-lock-held ((state-lock module))
    (setf (proc-s module) (list 'free 'free))
    (setf (exec-s module) (list 'free 'free))
    (setf (exec-queue module) nil))
  
  ;; store it for convenience in the style methods
  ;; don't need to lock protect it since it's internal,
  ;; only set here, and constant
  (setf (extension module) (get-module motor-extension)))

;;; When changing state consider which hand the action is using

(defmethod change-state ((module dual-execution-motor-module) &key proc exec prep last)
  (bt:with-recursive-lock-held ((state-lock module))
    (bt:with-recursive-lock-held ((param-lock module))
      (when proc 
        (if (listp proc)
            (cond ((or (null (two-proc module)) (eq (car proc) 'both))
                   (setf (first (proc-s module)) (second proc))
                   (setf (second (proc-s module)) (second proc)))
                  ((eq (car proc) 'left)
                   (setf (first (proc-s module)) (second proc)))
                  ((eq (car proc) 'right)
                   (setf (second (proc-s module)) (second proc)))
                  (t 
                   (print-warning "Invalid proc setting for the dual execution motor module: ~S" proc)))
          (print-warning "Invalid proc setting for the dual execution motor module: ~S" proc)))
      
      (when exec 
        (if (listp exec)
            (cond ((or (null (two-exec module)) (eq (car exec) 'both))
                   (setf (first (exec-s module)) (second exec))
                   (setf (second (exec-s module)) (second exec)))
                  ((eq (car exec) 'left)
                   (setf (first (exec-s module)) (second exec)))
                  ((eq (car exec) 'right)
                   (setf (second (exec-s module)) (second exec)))
                  (t 
                   (print-warning "Invalid exec setting for the dual execution motor module: ~S" exec)))
          (print-warning "Invalid exec setting for the dual execution motor module: ~S" exec))))
    
    (when prep (setf (prep-s module) prep))
    (when last (setf (last-cmd module) last))
    
    (if (or (eq (first (proc-s module)) 'busy) 
            (eq (second (proc-s module)) 'busy) 
            (eq (first (exec-s module)) 'busy)
            (eq (second (exec-s module)) 'busy)
            (eq (prep-s module) 'busy))
        (setf (mode-s module) 'busy)
      (setf (mode-s module) 'free))
    
    (setf (state-change module) t)))


;;; For testing state the processor and execution are busy if either
;;; hand's stage is busy and free only if both hands' stages are free.

;;; Can't check based on hand with the manual buffer, but there are
;;; two additional buffers (manual-left and manual-right) which allow 
;;; for testing that if desired, and those buffers also work for 
;;; tracking BOLD data by hand.


(defmethod print-module-status ((mod dual-execution-motor-module))
  (bt:with-recursive-lock-held ((state-lock mod))
    (concatenate 'string
      (format nil "  preparation free      : ~S~%" (eq (prep-s mod) 'free))
      (format nil "  preparation busy      : ~S~%" (eq (prep-s mod) 'busy))
      (format nil "  processor free        : ~S~%" (and (eq (first (proc-s mod)) 'free)
                                                      (eq (second (proc-s mod)) 'free)))
      (format nil "  processor busy        : ~S~%" (or (eq (first (proc-s mod)) 'busy)
                                                     (eq (second (proc-s mod)) 'busy)))
      (format nil "  execution free        : ~S~%" (and (eq (first (exec-s mod)) 'free)
                                                      (eq (second (exec-s mod)) 'free)))
      (format nil "  execution busy        : ~S~%" (or (eq (first (exec-s mod)) 'busy)
                                                     (eq (second (exec-s mod)) 'busy)))
      (format nil "  last-command          : ~S~%" (last-cmd mod)))))

(defmethod generic-state-query ((module dual-execution-motor-module) buffer slot value)
  (bt:with-recursive-lock-held ((state-lock module))
    (case slot
      ((state modality)
       (case value
         (busy
          (eq (mode-s module) 'busy))
         (free
          (eq (mode-s module) 'free))
         (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
      (execution
       (case value
         (busy
          (or (eq (first (exec-s module)) 'busy)
              (eq (second (exec-s module)) 'busy)))
         (free
          (and (eq (first (exec-s module)) 'free)
               (eq (second (exec-s module)) 'free)))
         (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
      (preparation
       (case value
         (busy
          (eq (prep-s module) 'busy))
         (free
          (eq (prep-s module) 'free))
         (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
      (processor
       (case value
         (busy
          (or (eq (first (proc-s module)) 'busy)
              (eq (second (proc-s module)) 'busy)))
         (free
          (and (eq (first (proc-s module)) 'free)
               (eq (second (proc-s module)) 'free)))
         (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
      (last-command 
       (eql (last-cmd module) value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change the style mechanisms:
;;;
;;; Give the movement style the slots for indicating whether or not
;;; the movement requires two "executions" (a press and release) and
;;; to indicate if it is a finger based action i.e. it has a finger
;;; attribute and should set that finger busy and free.
;;; 
;;; A finger based action will mark the finger as busy when the
;;; initiation starts (preparation completes) and free when it 
;;; finishes the movement.
;;;
;;; Whether the finger is up or down is handled by the set-finger-up
;;; and set-finger-down methods which should be called to send the
;;; actual notification to the device.
;;; 
;;; Add a method for computing the second execution time.
;;;
;;; Send the hand information to change-state.
;;;
;;; Make dummy requests to the manual-left and manual-right buffers
;;; as appropriate so that an action is recorded for the buffer trace.
;;;
;;; Indicate style and hand in the motor action events' output.
;;;
;;; The new slots are used as follows:
;;;
;;; two-exec-p defaults to nil, but if set to t indicates that the
;;; compute-second-exec-time method should be called.
;;;
;;; exec2-time stores the result of calling compute-second-exec-time.
;;;
;;; release-if-down indicates whether or not the finger used for
;;; the action should produce a release event if it is down when the
;;; execution of this action starts.  The possible values are:
;;; nil which means do not generate a release event.
;;; :free (the default) which means to generate the release event and
;;; not charge a time cost for it.
;;; :penalty generates the release event and adds a burst-time cost
;;; to each of the exec, exec2, and finish times of the action.
;;;

;;; Subclass movement-style for 

(defclass dual-hand-movement-style (movement-style)
  ((two-exec-p :accessor two-exec-p :initform nil :initarg :two-exec-p)
   (exec2-time :accessor exec2-time :initform 0 :initarg :exec2-time)
   
   (finger-based-style :accessor finger-based-style :initarg :finger-based-style :initform t)
   (release-if-down :accessor release-if-down :initarg :release-if-down :initform :free)))


(defgeneric compute-second-exec-time (module movement)
  (:documentation "Return the release time of <movement>."))

(defmethod compute-second-exec-time ((module pm-module) (mvmt dual-hand-movement-style))
  (error "No method defined for COMPUTE-SECOND-EXEC-TIME."))


(defmethod prepare-movement ((module dual-execution-motor-module) (mvmt dual-hand-movement-style))
  (bt:with-recursive-lock-held ((state-lock module))
    (change-state module :prep 'BUSY :proc (list (hand mvmt) 'BUSY))
    (prepare-features module mvmt)
    (setf (fprep-time mvmt) 
      (randomize-time (compute-prep-time module mvmt)))
    (setf (last-prep module) mvmt)
    
    (schedule-event-relative (seconds->ms (fprep-time mvmt)) 'preparation-complete :time-in-ms t 
                             :destination (my-name module) :module (my-name module)
                             :details (concatenate 'string (symbol-name 'preparation-complete) " " (princ-to-string (request-time mvmt))
                                        " style " (symbol-name (type-of mvmt)) " hand " (symbol-name (hand mvmt))))
    
    ;; send dummy requests so that the buffer trace records things appropriately by hand
    
    (unless (relay-hand-requests module) ;; if the buffers sent them to manual don't send them back
      (when (or (eq (hand mvmt) 'left) (eq (hand mvmt) 'both))
        (schedule-event-now 'module-request 
                            :module (my-name module)
                            :params (list 'manual-left
                                          (define-chunk-spec-fct (list 'cmd (type-of mvmt))))
                            :details (concatenate 'string (symbol-name 'module-request) " " (symbol-name 'manual-left))
                            :output nil
                            :maintenance t))
      
      (when (or (eq (hand mvmt) 'right) (eq (hand mvmt) 'both))
        (schedule-event-now 'module-request 
                            :module (my-name module)
                            :params (list 'manual-right
                                          (define-chunk-spec-fct (list 'cmd (type-of mvmt))))
                            :details (concatenate 'string (symbol-name 'module-request) " " (symbol-name 'manual-right))
                            :output nil
                            :maintenance t)))))



(defmethod initiation-complete ((module dual-execution-motor-module))
  (bt:with-recursive-lock-held ((state-lock module))
    (when (last-prep module)
      (case (hand (last-prep module))
        (left 
         (change-state module :proc '(left FREE)))
        (right 
         (change-state module :proc '(right FREE)))
        (both
         (change-state module :proc '(left FREE))
         (change-state module :proc '(right FREE)))
        (t
         (print-warning "Non-handed action taken with the two-handed motor module: ~s" (last-prep module)))))))

(defmethod preparation-complete ((module dual-execution-motor-module))
  (bt:with-recursive-lock-held ((state-lock module))
    (bt:with-recursive-lock-held ((param-lock module))
      (change-state module :prep 'free)
      (when (last-prep module)
        (if (exec-immediate-p (last-prep module))
            (let ((hand (hand (last-prep module))))
              (cond ((or (null (two-exec module)) 
                         (eq hand 'left) 
                         (eq hand 'right)
                         (eq hand 'both))
                     (push-last (last-prep module) (exec-queue module)))
                    (t
                     (print-warning "Non-handed action taken with the dual-execution motor module: ~s ~s" (last-prep module) (hand (last-prep module)))))
              (when (updated-pos (last-prep module))
                (update-prepared module)))
          
          (progn
            (when (or (minusp (init-stamp module))
                      (and (plusp (init-stamp module))
                           (>= (mp-time-ms) (+ (init-stamp module) (seconds->ms (init-time module))))))
              (change-state module :proc (list (hand (last-prep module)) 'FREE))))))
      (maybe-execute-movement module))))

#| don't need to redefine this since there's only one
   exec queue again

(defmethod execute ((module dual-execution-motor-module) request)
  (bt:with-recursive-lock-held ((state-lock module))
    (cond ((not (last-prep module))
           (model-warning "Motor Module has no movement to EXECUTE."))
          ((eq (prep-s module) 'BUSY)
           (model-warning "Motor Module cannot EXECUTE features being prepared."))
          (t
           (setf (request-spec (last-prep module)) request)
           (setf (exec-queue module)
             (append (exec-queue module) (mklist (last-prep module))))
           (when (updated-pos (last-prep module))
             (update-prepared module))
           (maybe-execute-movement module)))))
|#

(defmethod update-prepared ((mtr-mod dual-execution-motor-module))
  (when (find 'hand (feature-slots (last-prep mtr-mod)))
    (case (hand (last-prep mtr-mod))
      (left (set-hand-position mtr-mod 'left (updated-pos (last-prep mtr-mod)) :current nil))
      (right (set-hand-position mtr-mod 'right (updated-pos (last-prep mtr-mod)) :current nil))
      (both (set-hand-position mtr-mod 'left (first (updated-pos (last-prep mtr-mod))) :current nil)
            (set-hand-position mtr-mod 'right (second (updated-pos (last-prep mtr-mod))) :current nil)))))

(defmethod maybe-execute-movement ((module dual-execution-motor-module))
  ;; If only one exec system just pop the next one like normal.
  ;; Otherwise, if there's an execution stage available check actions to 
  ;; perform starting from the front until one can be executed for each
  ;; available stage or a both is found (regardless of whether it can
  ;; be executed because if it can't be executed then you can't skip 
  ;; over it).
  ;; 
  
  (bt:with-recursive-lock-held ((state-lock module))
    (bt:with-recursive-lock-held ((param-lock module))
      
      (when (exec-queue module)
        (if (null (two-exec module))
            (when (eq (first (exec-s module)) 'FREE)
              (perform-movement module (pop (exec-queue module))))
          
          (when (or (eq (first (exec-s module)) 'FREE)
                    (eq (second (exec-s module)) 'FREE))
            (do* ((left-available (eq (first (exec-s module)) 'FREE))
                  (right-available (eq (second (exec-s module)) 'FREE))
                  (queue (exec-queue module) (cdr queue))
                  (evt (car queue) (car queue)))
                 ((or (null evt)
                      (and (null left-available)
                           (null right-available))))
              (case (hand evt)
                (left
                 (when left-available
                   (setf left-available nil)
                   (setf (exec-queue module) (remove evt (exec-queue module)))
                   (perform-movement module evt)))
                (right
                 (when right-available
                   (setf right-available nil)
                   (setf (exec-queue module) (remove evt (exec-queue module)))
                   (perform-movement module evt)))
                (both
                 (when (and left-available right-available)
                   (setf (exec-queue module) (remove evt (exec-queue module)))
                   (perform-movement module evt))
                 ;; whether a both executes or not it blocks any
                 ;; further actions because you can't reorder the
                 ;; actions for a hand
                 (setf left-available nil)
                 (setf right-available nil))))))))))

(defmethod perform-movement ((module dual-execution-motor-module) (mvmt movement-style))
  (bt:with-recursive-lock-held ((state-lock module)) ; don't need the lock the whole time but more efficient than grabbing twice
    (bt:with-recursive-lock-held ((param-lock module))
      (let ((extension (extension module))
            (last (last-prep module)))
        
        ;; Check releases now because they could have been going down while this was being
        ;; prepared but if it's not down now just stop.
        
        ;; Shouldn't happen now because of the preparation checks, but just to be safe
        ;; leaving it here.
        
        (when (and (or (typep last 'release)
                       (typep last 'release-recoil))
                   
                   (not (finger-down extension (hand last) (finger last))))
          
          (print-warning "RELEASE action ignored because the ~s ~s finger is not held down." (hand (last-prep module)) (finger (last-prep module)))
          (return-from perform-movement))
        
        
        (let ((hands (if (eq (hand mvmt) 'both) (list 'left 'right) (list (hand mvmt))))
              (fingers (let ((f (when (find 'finger (feature-slots mvmt))
                                  (finger mvmt))))
                         (if (and f (not (eq f :dummy))) 
                             (list f) 
                           '(index middle ring pinkie thumb)))))
          
          (schedule-event-relative (seconds->ms (init-time module)) 'INITIATION-COMPLETE :destination (my-name module)
                                   :time-in-ms t :module (my-name module) 
                                   :details (concatenate 'string (symbol-name 'INITIATION-COMPLETE) " " (princ-to-string (request-time mvmt))
                                              " style " (symbol-name (type-of mvmt)) " hand " (symbol-name (hand mvmt))))
          
          (change-state module :proc (list (hand mvmt) 'BUSY) :exec (list (hand mvmt) 'BUSY))
                    
          (setf (init-stamp module) (mp-time-ms))
  
          (setf (exec-time mvmt) (compute-exec-time module mvmt))
          (setf (finish-time mvmt) (compute-finish-time module mvmt))
          (when (two-exec-p mvmt)
            (setf (exec2-time mvmt) (compute-second-exec-time module mvmt)))
  
  
          ;;; Automatically release the fingers involved in an action
          ;;; and charge a burst-time cost if they're down at the start
          ;;; of the action and the style says they need to be released.
          
          (when (release-if-down mvmt)
    
            (let ((adjust nil))
      
              (dolist (hand hands)
                (dolist (finger fingers)
                  (when (finger-down extension hand finger)
                    (setf adjust t)
                    (schedule-event-relative (seconds->ms (+ (init-time module) (key-release-time module)))
                                             'set-finger-up :time-in-ms t :destination :motor :module :motor :output nil
                                             :params (list 'release hand finger (finger-loc-m module hand finger))))))
      
              (when (and adjust (eq (release-if-down mvmt) :penalty))
                (incf (exec-time mvmt) (burst-time module))
                (incf (finish-time mvmt) (burst-time module))
                (when (two-exec-p mvmt)
                  (incf (exec2-time mvmt) (burst-time module))))))
  
          (queue-output-events module mvmt)
          (queue-finish-event module mvmt)
  
          ;; indicate that the fingers are busy
  
          (bt:with-recursive-lock-held ((hand-tracker-lock extension))
            (when (finger-based-style mvmt)
              (dolist (hand hands)
                (dolist (finger fingers)
                  (setf (gethash (list hand finger) (hand-tracker-finger-busy extension)) t))))))))))


(defmethod queue-finish-event ((module dual-execution-motor-module) (mvmt movement-style))
  (schedule-event-relative (seconds->ms (finish-time mvmt)) 'finish-movement-dual :destination (my-name module)
                           :time-in-ms t :params (list mvmt) :module (my-name module) 
                           :details (concatenate 'string (symbol-name 'finish-movement) " " (princ-to-string (request-time mvmt))
                                              " style " (symbol-name (type-of mvmt)) " hand " (symbol-name (hand mvmt)))))


(defmethod finish-movement-dual ((module dual-execution-motor-module) (mvmt movement-style))
  (change-state module :exec (list (hand mvmt) 'free))
  
  ;;; If the style says it's finger based then set the fingers back to free
  
  (when (finger-based-style mvmt)
    (let ((extension (extension module))
          (hands (if (eq (hand mvmt) 'both) (list 'left 'right) (list (hand mvmt))))
          (fingers (let ((f (when (find 'finger (feature-slots mvmt)) 
                              (finger mvmt))))
                     (if (and f (not (eq f :dummy))) 
                         (list f) 
                       '(index middle ring pinkie thumb)))))
      (bt:with-recursive-lock-held ((hand-tracker-lock extension))
        (dolist (hand hands)
          (dolist (finger fingers)
            (remhash (list hand finger) (hand-tracker-finger-busy extension)))))))
  
  (maybe-execute-movement module))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code for recording and testing hand execution with separate buffers.
;;;
;;; With the new buffers the fingers can also be tested to determine if 
;;; they're currently "holding" a key.  If the finger is up it is 
;;; not holding a key and if it is down then it is holding a key.  That
;;; is independent of free/busy which indicates that it is performing
;;; an action, and holding a key does not make it busy once the finger
;;; is down.


(defstruct hand-tracker 
  manual left-cmd right-cmd relay
  (lock (bt:make-recursive-lock "hand-tracker"))
  (finger-busy (make-hash-table :size 10 :test 'equalp))
  (finger-down (make-hash-table :size 10 :test 'equalp)))

(defun track-hand-requests (module buffer spec)
  (let (relay)
    (bt:with-recursive-lock-held ((hand-tracker-lock module))
      
      (setf relay (hand-tracker-relay module))
      
      (if (eq buffer 'manual-left)
          (setf (hand-tracker-left-cmd module) (spec-slot-value (car (chunk-spec-slot-spec spec 'cmd))))
        (setf (hand-tracker-right-cmd module) (spec-slot-value (car (chunk-spec-slot-spec spec 'cmd))))))
    
    (when relay
      (let ((includes-hand (chunk-spec-slot-spec spec 'hand)))
        
        (if (or (null includes-hand)
                (and includes-hand
                     (= (length includes-hand) 1)
                     (eq (spec-slot-op (car includes-hand)) '=) 
                     (or (and (eq buffer 'manual-left) (eq (spec-slot-value (car includes-hand)) 'left))
                         (and (eq buffer 'manual-right) (eq (spec-slot-value (car includes-hand)) 'right)))))
            (module-request 'manual spec)
          (print-warning "Request to ~s buffer has an invalid hand slot ~s." buffer includes-hand))))))

(defun reset-hand-tracker (module)
  (bt:with-recursive-lock-held ((hand-tracker-lock module))
    (setf (hand-tracker-manual module) (get-module :motor))
    (setf (hand-tracker-left-cmd module) 'none)
    (setf (hand-tracker-right-cmd module) 'none)
    (clrhash (hand-tracker-finger-busy module))
    (clrhash (hand-tracker-finger-down module))))


(defun finger-down (extension hand finger &key (current t))
  (if current
      (bt:with-recursive-lock-held ((hand-tracker-lock extension))
        (gethash (list hand finger) (hand-tracker-finger-down extension)))
    (let* ((hand-pos (hand-position hand :current nil))
           (fingers (find 'extended-fingers (hand-pos-other hand-pos) :key 'first))
           (f (find finger (rest fingers) :key 'first)))
      (if f
          (second f)
        (bt:with-recursive-lock-held ((hand-tracker-lock extension))
          (gethash (list hand finger) (hand-tracker-finger-down extension)))))))


(defun any-finger-down (extension hand &key (current t))
  
  (if current
      (bt:with-recursive-lock-held ((hand-tracker-lock extension))
        (some (lambda (x) (gethash (list hand x) (hand-tracker-finger-down extension)))
              '(index middle ring pinkie thumb)))
    
    (let* ((hand-pos (hand-position hand :current nil))
           (fingers (find 'extended-fingers (hand-pos-other hand-pos) :key 'first))
           (states (rest fingers)))
      (if states
          (progn
            (unless (= 5 (length states))
              (print-warning "Invalid extended-fingers information stored in hand position for any-finger-down."))
            (some 'second states))
        (bt:with-recursive-lock-held ((hand-tracker-lock extension))
          (some (lambda (x) (gethash (list hand x) (hand-tracker-finger-down extension)))
                '(index middle ring pinkie thumb)))))))


(defun finger-busy (extension hand finger)
  (bt:with-recursive-lock-held ((hand-tracker-lock extension))
    (gethash (list hand finger) (hand-tracker-finger-busy extension))))

(defmethod print-hand-buffer-status ((mod hand-tracker) hand)
  (let ((manual (hand-tracker-manual mod)))
    (bt:with-recursive-lock-held ((state-lock manual))
      (with-output-to-string (s)
        (format s "  processor free        : ~S~%"
          (if (eq hand 'manual-left)
              (eq (first (proc-s manual)) 'free)
            (eq (second (proc-s manual)) 'free)))
        (format s "  processor busy        : ~S~%"
          (if (eq hand 'manual-left)
              (eq (first (proc-s manual)) 'busy)
            (eq (second (proc-s manual)) 'busy)))
        (format s "  execution free        : ~S~%"
          (if (eq hand 'manual-left)
              (eq (first (exec-s manual)) 'free)
            (eq (second (exec-s manual)) 'free)))
        (format s "  execution busy        : ~S~%"
          (if (eq hand 'manual-left)
              (eq (first (exec-s manual)) 'busy)
            (eq (second (exec-s manual)) 'busy)))
        (format s "  last-command          : ~S~%"
          (if (eq hand 'manual-left)
              (hand-tracker-left-cmd mod)
            (hand-tracker-right-cmd mod)))
        
        (dolist (x '(index middle ring pinkie thumb))
          (format s "  ~(~6a~) free           : ~s~%" x
            (null (finger-busy mod (if (eq hand 'manual-left) 'left 'right) x)))
          (format s "  ~(~6a~) down           : ~s~%" x
            (finger-down mod (if (eq hand 'manual-left) 'left 'right) x)))))))

(defmethod generic-state-query ((module hand-tracker) buffer slot value)
  (let ((manual (hand-tracker-manual module)))
    (bt:with-recursive-lock-held ((state-lock manual))
      (case slot
        (preparation ;; same for both hands
         (case value
           (busy
            (eq (prep-s module) 'busy))
           (free
            (eq (prep-s module) 'free))
           (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
      
        (state
         (case value
           (busy (case buffer
                   (manual-left
                    (or (eq (first (exec-s manual)) 'busy) (eq (first (proc-s manual)) 'busy)))
                   (manual-right
                    (or (eq (second (exec-s manual)) 'busy) (eq (second (proc-s manual)) 'busy)))))
           (free (case buffer
                   (manual-left
                    (and (eq (first (exec-s manual)) 'free) (eq (first (proc-s manual)) 'free)))
                   (manual-right
                    (and (eq (second (exec-s manual)) 'free) (eq (second (proc-s manual)) 'free)))))
           (error nil)
           (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
        (execution
         (case value
           (busy (case buffer
                   (manual-left
                    (eq (first (exec-s manual)) 'busy))
                   (manual-right
                    (eq (second (exec-s manual)) 'busy))))
           (free (case buffer
                   (manual-left
                    (eq (first (exec-s manual)) 'free))
                   (manual-right
                    (eq (second (exec-s manual)) 'free))))
           (error nil)
           (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
      
        (processor
         (case value
           (busy (case buffer
                   (manual-left
                    (eq (first (proc-s manual)) 'busy))
                   (manual-right
                    (eq (second (proc-s manual)) 'busy))))
           (free (case buffer
                   (manual-left
                    (eq (first (proc-s manual)) 'free))
                   (manual-right
                    (eq (second (proc-s manual)) 'free))))
           (error nil)
           (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value))))
        (last-command 
         (case buffer
           (manual-left
            (eql (hand-tracker-left-cmd module) value))
           (manual-right
            (eql (hand-tracker-right-cmd module) value))))
        ((index middle ring pinkie thumb)
         (bt:with-recursive-lock-held ((hand-tracker-lock module))
           (case value
             (busy
              (gethash (list (if (eq buffer 'manual-left) 'left 'right) slot) (hand-tracker-finger-busy module)))
             (free
              (null (gethash (list (if (eq buffer 'manual-left) 'left 'right) slot) (hand-tracker-finger-busy module))))
             (down
              (gethash (list (if (eq buffer 'manual-left) 'left 'right) slot) (hand-tracker-finger-down module)))
             (up
              (null (gethash (list (if (eq buffer 'manual-left) 'left 'right) slot) (hand-tracker-finger-down module))))
             (t (print-warning "Invalid query made of the ~S buffer with slot ~S and value ~S" buffer slot value)))))))))


(defun track-hand-params (instance param)
  ;; The parameter values are actually stored in the motor module
  ;; and not the extension module.
  (let ((manual (aif (hand-tracker-manual instance) it (get-module :motor))))
    (bt:with-recursive-lock-held ((param-lock manual))
      (cond ((consp param)
             (case (car param)
               (:key-closure-time
                (setf (key-closure-time manual) (cdr param)))
               (:key-release-time
                (setf (key-release-time manual) (cdr param)))
               (:dual-processor-stages
                (setf (two-proc manual) (cdr param)))
               (:dual-execution-stages
                (setf (two-exec manual) (cdr param)))
               (:peck-strike-distance
                (setf (peck-strike manual) (cdr param)))
               (:default-punch-delay 
                (setf (d-punch-default manual) (cdr param)))
               (:fast-punch-delay
                (setf (d-punch-fast manual) (cdr param)))
               (:slow-punch-delay
                (setf (d-punch-slow manual) (cdr param)))
               (:cursor-drag-delay 
                (setf (drag-delay manual) (cdr param)))
               (:relay-hand-requests 
                (setf (relay-hand-requests manual) (cdr param))
                (bt:with-recursive-lock-held ((hand-tracker-lock instance))
                  (setf (hand-tracker-relay instance) (cdr param))))))
            (t
             (case param
               (:key-release-time
                (key-release-time manual))
               (:dual-processor-stages
                (two-proc manual))
               (:dual-execution-stages
                (two-exec manual))
               (:peck-strike-distance
                (peck-strike manual))
               (:default-punch-delay
                (d-punch-default manual))
               (:fast-punch-delay
                (d-punch-fast manual))
               (:slow-punch-delay
                (d-punch-slow manual))
               (:cursor-drag-delay 
                (drag-delay manual))
               (:relay-hand-requests 
                (bt:with-recursive-lock-held ((hand-tracker-lock instance))
                  (hand-tracker-relay instance)))))))))

(defun left-hand-buffer-status ()
  (print-hand-buffer-status (get-module motor-extension) 'manual-left))

(defun right-hand-buffer-status ()
  (print-hand-buffer-status (get-module motor-extension) 'manual-right))

(defun peck-strike-value-test (x)
  (or (null x) (and (numberp x) (<= 0 x .5))))

(defun create-motor-extension-module (x)
  (declare (ignore x))
  (make-hand-tracker))

(define-module-fct 'motor-extension 
    (list (list 'manual-left nil nil '(execution processor last-command index middle ring pinkie thumb)
                'left-hand-buffer-status)
          (list 'manual-right nil nil '(execution processor last-command index middle ring pinkie thumb)
                'right-hand-buffer-status))
  (list
   (define-parameter :default-punch-delay
     :valid-test 'posnum
     :warning "a number"
     :default-value 0.075
     :documentation "Default time for a delayed-punch request.")
   (define-parameter :fast-punch-delay
     :valid-test 'posnum
     :warning "a number"
     :default-value 0.050
     :documentation "Time for a delayed-punch request with a delay of fast.")
   (define-parameter :slow-punch-delay
     :valid-test 'posnum
     :warning "a number"
     :default-value 0.1
     :documentation "Time for a delayed-punch request with a delay of slow.")
   (define-parameter :cursor-drag-delay
     :valid-test 'posnumorbool 
     :warning "T, nil, or a positive number"
     :default-value nil
     :documentation "Control order increment for cursor drag actions.")
   
   (define-parameter :key-release-time
     :valid-test 'nonneg
     :warning "a number"
     :default-value 0.04
     :documentation "Time between when a key that is held down is released and when it registers as being released.")
   
   (define-parameter :key-closure-time  
     :owner nil)
   
   (define-parameter :dual-processor-stages
     :valid-test 'tornil
     :warning "T or nil"
     :default-value nil
     :documentation "Whether the motor module has separate processor stages for the hands.")
   (define-parameter :dual-execution-stages
     :valid-test 'tornil
     :warning "T or nil"
     :default-value nil
     :documentation "Whether the motor module has separate execution stages for the hands.")
   (define-parameter :peck-strike-distance
     :valid-test 'peck-strike-value-test
     :warning "Nil or a number between 0 and .5"
     :default-value nil
     :documentation "Distance from center of the key at which time a peck starts to depress the key.")
   (define-parameter :relay-hand-requests
     :valid-test 'tornil
     :warning "T or nil"
     :default-value nil
     :documentation "Whether manual-left and manual-right pass requests to manual"))
  
  :params 'track-hand-params
  :request 'track-hand-requests
  :query 'generic-state-query 
  :creation 'create-motor-extension-module
  :reset (list nil nil 'reset-hand-tracker)
  :version "6.1"
  :documentation "Extends motor module with dual processor and/or execution states and finger holding actions.")


;;; Compute when a peck or peck-recoil indicates that the key is hit
;;; based on the peck-strike-distance paramter.
;;; 
;;; For the peck and peck-recoil compute the timing a little differently
;;; now because I want to consider the time between striking and releasing
;;; the key.  So, send the output-key before the movement time is "done"
;;; to simulate the key-closure-time as punch does i.e. the key gets 
;;; detected before the motion is complete.  To do that I'm assuming that
;;; the key starts to move once the finger gets within the peck-strike-distance
;;; of the target (center of the key).  Just using a simple binary serach to find
;;; the adjusted target time at this point instead of a complicated numerical
;;; method.
;;;


(defun movement-strike-time (mtr-mod tt r)
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (cond ((null (peck-strike mtr-mod))
           tt)
          ((zerop (peck-strike mtr-mod))
           (+ tt (key-closure-time mtr-mod)))
          (t
           (ms-round (+ (key-closure-time mtr-mod)
                        (do* ((target (- r (peck-strike mtr-mod)))
                              (width (* .25 tt) (/ width 2))
                              (guess (* tt .75) (+ (* dir width) guess))
                              (value (minjerk-dist guess r tt)
                                     (minjerk-dist guess r tt))
                              (dir (if (< value target) 1 -1)
                                   (if (< value target) 1 -1)))
                             ((< (abs (- target value)) .01) guess))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create methods to handle the finger movement, the notifications are
;;; generated by these instead of directly in the output event.

(defmethod set-finger-up ((module dual-execution-motor-module) style hand finger position)
  (declare (ignore style))
  (let ((extension (extension module)))
    (bt:with-recursive-lock-held ((hand-tracker-lock extension))
      (when (gethash (list hand finger) (hand-tracker-finger-down extension))
        (remhash (list hand finger) (hand-tracker-finger-down extension))
        (notify-motor-device (let ((h (case hand
                                        (left (left-hand module))
                                        (right (right-hand module)))))
                               (bt:with-lock-held ((hand-lock h))
                                 (device h)))
                             `((model ,(current-model)) (style release) (hand ,hand) (loc ,(coerce position 'list))))))))


(defmethod set-finger-down ((module dual-execution-motor-module) style hand finger position)
  (let ((extension (extension module)))
    (bt:with-recursive-lock-held ((hand-tracker-lock extension))
      
      (unless (gethash (list hand finger) (hand-tracker-finger-down extension))
        (setf (gethash (list hand finger) (hand-tracker-finger-down extension)) t)
        
        (notify-motor-device (let ((h (case hand
                                        (left (left-hand module))
                                        (right (right-hand module)))))
                               (bt:with-lock-held ((hand-lock h))
                                 (device h)))
                             `((model ,(current-model)) (style ,style) (hand ,hand) (loc ,(coerce position 'list))))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Redefine the standard motor actions: punch, peck, and peck-recoil
;;; to deal with calling the release event and indicate that they
;;; should automatically release the key (paying a burst-time cost) and 
;;; indicate the actions are finger based (the default so don't really set).
;;;
;;; The release time is determined to be key-release-time after the 
;;; "striking" action is over i.e. the burst or movement time.  So it
;;; happens somewhere during the final burst-time.
;;;
;;; Only redefine the methods needed -- most exec and finish time
;;; methods for original actions are still fine.
;;;
;;; Using a symbol for style-name instead of a keyword so it can
;;; be passed to the notification directly.
;;;
;;; Create a base class for hand and finger movements since there are
;;; other related styles now instead of only punches, and that allows
;;; for a general queue-output-events!
;;; 
;;; A big NOPE to that ability to have a general queue-output-events
;;; since the old specific methods get in the way...
;;; Leaving it here, but also cut-and-pasting it for specific
;;; methods.

(defstyle hf-movement dual-hand-movement-style hand finger)

(defStyle hfrt-movement hf-movement hand finger r theta)

(defmethod queue-output-events ((mtr-mod motor-module) (self hf-movement))
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'set-finger-down :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc-m mtr-mod (hand self) (finger self))))

  (schedule-event-relative (seconds->ms (exec2-time self))
                           'set-finger-up :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc-m mtr-mod (hand self) (finger self)))))



;;; Method to adjust the state of a finger given a starting position

(defgeneric adjust-finger-position (mtr-mod self starting-point finger-state)
  )

(defmethod adjust-finger-position ((mtr-mod dual-execution-motor-module) (self dual-hand-movement-style) starting-point finger-state)
  (let* ((hand (hand self))
         (others (copy-seq (hand-pos-other starting-point)))
         (extension (extension mtr-mod))
         (current (find 'extended-fingers (hand-pos-other starting-point) :key 'first))
         (current-fingers (rest current)))
    (when current
      (setf others (remove current others)))
           
    (let ((updated-fingers (list 'extended-fingers)))
             
      ;; don't really need to set all of them, but
      ;; for consistency always going to do so
             
      (dolist (finger '(index middle ring pinkie thumb)) 
        (push-last (if (eq finger (finger self))
                       (list finger finger-state) 
                     (let ((previous (find finger current-fingers :key 'first)))
                       (if previous
                           (list finger (second previous))
                         (list finger (finger-down extension hand finger))))) ;; know it's not in :current nil
                   updated-fingers))
        
      (push updated-fingers others))
    (setf (hand-pos-other starting-point) others)
    (setf (updated-pos self) starting-point)))
  
;;; Updated punch 

(defclass punch (hf-movement)
  nil
  (:default-initargs
    :style-name 'punch
    :two-exec-p t
    :release-if-down :penalty))

(defmethod prepare-features ((mtr-mod motor-module) (self punch))
  (adjust-finger-position mtr-mod self (copy-hand-pos (hand-position (hand self) :current nil)) nil))


(defmethod compute-second-exec-time ((mtr-mod motor-module) (self punch))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       ; don't add this to be consistent with peck and peck-recoil actions (burst-time mtr-mod)
       (key-release-time mtr-mod))))

(defmethod queue-output-events ((mtr-mod motor-module) (self punch))
  
  ;; don't need to schedule a setting of the new hand position
  ;; since the "current" finger info isn't stored in there but do it
  ;; for consistency and use exec2 since that's when the finger is really up.
  
  (schedule-event-relative (seconds->ms (exec2-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'set-finger-down :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self))))

  (schedule-event-relative (seconds->ms (exec2-time self))
                           'set-finger-up :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self)))))


;;; Updated peck
;;; add a move-time slot to pecks to save the calculation

(defclass peck (hfrt-movement) 
  ((move-time :accessor move-time))
  (:default-initargs
    :style-name 'peck
    :two-exec-p t
    :release-if-down :penalty))

(defmethod prepare-features ((mtr-mod motor-module) (self peck))
  (adjust-finger-position mtr-mod self (move-a-finger mtr-mod (hand self) (finger self) (r self) (theta self) :current nil) nil))

(defmethod compute-exec-time ((mtr-mod motor-module) (self peck))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (setf (move-time self)
      (max (burst-time mtr-mod)
           (randomize-time (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self)))))
  
    (+ (init-time mtr-mod)
       (movement-strike-time mtr-mod  (move-time self) (r self)))))

(defmethod compute-finish-time ((mtr-mod motor-module) (self peck))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (move-time self) 
       (burst-time mtr-mod))))

(defmethod compute-second-exec-time ((mtr-mod motor-module) (self peck))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (move-time self)
       (key-release-time mtr-mod))))

(defmethod queue-output-events ((mtr-mod motor-module) (self peck))
  
  (schedule-event-relative (seconds->ms (exec2-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'set-finger-down :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self))))
  
  (schedule-event-relative (seconds->ms (exec2-time self))
                           'set-finger-up :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self)))))

;;; Updated peck-recoil
;;; for a peck-recoil record the base movement time and the specific (noisy) first movement times

(defclass peck-recoil (hfrt-movement)
  ((move-time :accessor move-time)
   (first-move-time :accessor first-move-time))
  (:default-initargs
    :style-name 'peck
    :two-exec-p t
    :release-if-down :penalty))

(defmethod prepare-features ((mtr-mod motor-module) (self peck-recoil))
  (adjust-finger-position mtr-mod self (copy-hand-pos (hand-position (hand self) :current nil)) nil))

(defmethod compute-exec-time ((mtr-mod motor-module) (self peck-recoil))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (setf (move-time self)
      (max (burst-time mtr-mod)
           (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self))))
    
    (setf (first-move-time self)
      (max (burst-time mtr-mod) 
           (randomize-time (move-time self))))
    
    (+ (init-time mtr-mod)
       (movement-strike-time mtr-mod (first-move-time self) (r self)))))

(defmethod compute-finish-time ((mtr-mod motor-module) (self peck-recoil))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (first-move-time self) 
       (burst-time mtr-mod) 
       (max (burst-time mtr-mod) (randomize-time (move-time self))))))


(defmethod compute-second-exec-time ((mtr-mod motor-module) (self peck-recoil))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (first-move-time self)
       (key-release-time mtr-mod))))

(defmethod queue-output-events ((mtr-mod motor-module) (self peck-recoil))
  
  (schedule-event-relative (seconds->ms (exec2-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  
  (let ((target-loc (polar-move-xy (finger-loc (updated-pos self) (finger self))
                                   (vector (r self) (theta self)))))
        
    (schedule-event-relative (seconds->ms (exec-time self))
                             'set-finger-down :time-in-ms t :destination :motor :module :motor :output nil
                             :params (list (style-name self) (hand self) (finger self) target-loc))
    
    (schedule-event-relative (seconds->ms (exec2-time self))
                             'set-finger-up :time-in-ms t :destination :motor :module :motor :output nil
                             :params (list (style-name self) (hand self) (finger self) target-loc))))


;;; Make sure that mouse movement doesn't change the state of the
;;; mouse button

(defclass cursor-ply (ply)
  ((target-coords :accessor target-coords :initarg :target-coords)
   (control-order :accessor control-order :initarg :control-order :initform 0)
   (cursor :accessor cursor :initarg :cursor :initform nil))
  (:default-initargs
    :hand 'RIGHT
    :fitts-coeff 
    (let ((mtr-mod (get-module :motor)))
      (when mtr-mod
        (bt:with-recursive-lock-held ((param-lock mtr-mod))
          (mouse-fitts-coeff mtr-mod))))
    :release-if-down nil
    :finger-based-style nil))

;; Since it doesn't adjust the extended custom features for the hand-position
;; (doesn't change finger states) the original methods will still work.
;; However, to account for the drag-delay need to change the :before
;; method on the exec-time.

(defmethod compute-exec-time :before ((mtr-mod motor-module) (self cursor-ply))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (let ((offset 0)
          (extension (extension mtr-mod))
          (hand (hand self)))
      
      (when (drag-delay mtr-mod)
        (when (any-finger-down extension hand :current nil) ;; down when move will happen
          (setf offset (if (eq (drag-delay mtr-mod) t) .8 (drag-delay mtr-mod)))))
      
      (setf (fitts-coeff self) 
        (* (expt-coerced 2 (+ offset (control-order self))) (fitts-coeff self))))))


;;; New styles for actions which hold a finger down or release a held finger

(defclass hold-punch (punch)
  nil
  (:default-initargs
      :style-name 'punch
    :two-exec-p nil
    :release-if-down :penalty))

(defmethod prepare-features ((mtr-mod motor-module) (self hold-punch))
  (adjust-finger-position mtr-mod self (copy-hand-pos (hand-position (hand self) :current nil)) t))

(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self hold-punch))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (key-closure-time mtr-mod))))

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self hold-punch))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (burst-time mtr-mod))))

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (self hold-punch))
  
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'set-finger-down :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self)))))


(defmethod hold-punch ((mtr-mod dual-execution-motor-module) &key hand finger request-spec)
  (unless (or (check-jam mtr-mod) (check-specs 'hold-punch hand finger))
    (prepare-movement mtr-mod (make-instance 'hold-punch :hand hand :finger finger :request-spec request-spec))))

(extend-manual-requests (hold-punch hand finger) handle-style-request)


(defclass hold-peck (peck)
  nil
  (:default-initargs
      :style-name 'peck
    :two-exec-p nil
    :release-if-down :penalty))

(defmethod prepare-features ((mtr-mod motor-module) (self hold-peck))
  (adjust-finger-position mtr-mod self (move-a-finger mtr-mod (hand self) (finger self) (r self) (theta self) :current nil) t))


(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self hold-peck))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (setf (move-time self)
      (max (burst-time mtr-mod)
           (randomize-time (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self)))))
    
    (+ (init-time mtr-mod)
       (movement-strike-time mtr-mod (move-time self) (r self)))))

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self hold-peck))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (move-time self))))
     
(defmethod queue-output-events ((mtr-mod motor-module) (self hold-peck))
  
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'set-finger-down :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self)))))
  

(defmethod hold-peck ((mtr-mod motor-module) &key hand finger r theta request-spec)
  (unless (or (check-jam mtr-mod) (check-specs 'hold-peck hand finger r theta))
    (when (symbolp theta)
      (setf theta (symbol-value theta)))
    (prepare-movement mtr-mod (make-instance 'hold-peck :hand hand :finger finger :r r :theta theta :request-spec request-spec))))

(extend-manual-requests (hold-peck hand finger r theta) handle-style-request)



(defclass release (punch)
  nil
  (:default-initargs
      :style-name 'punch
    :two-exec-p nil
    :release-if-down nil))

(defmethod prepare-features ((mtr-mod motor-module) (self release))
  (adjust-finger-position mtr-mod self (copy-hand-pos (hand-position (hand self) :current nil)) nil))

(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self release))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (key-release-time mtr-mod))))

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self release))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (burst-time mtr-mod))))

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (self release))
  
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'set-finger-up :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self)))))

(defmethod release ((mtr-mod dual-execution-motor-module) &key hand finger request-spec)
  (unless (or (check-jam mtr-mod) (check-specs 'release hand finger))
    (if (not (finger-down (extension mtr-mod) hand finger :current nil))
        (print-warning "RELEASE action ignored because the ~s ~s finger is not held down or already being released." hand finger)
    (prepare-movement mtr-mod (make-instance 'release :hand hand :finger finger :request-spec request-spec)))))


(extend-manual-requests (release hand finger) handle-style-request)


(defclass release-recoil (peck)
  nil
  (:default-initargs
      :style-name 'peck
    :two-exec-p nil
    :release-if-down nil))

;; the same prepare as peck is fine

(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self release-recoil))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (key-release-time mtr-mod))))

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self release-recoil))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (setf (move-time self)
      (max (burst-time mtr-mod)
           (randomize-time (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self)))))
  
    (+ (init-time mtr-mod) 
       (move-time self) 
       (burst-time mtr-mod))))
               
(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (self release-recoil))
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  
  (let ((current-position (finger-loc (hand self) (finger self))))
    
    (schedule-event-relative (seconds->ms (exec-time self))
                             'set-finger-up :time-in-ms t :destination :motor :module :motor :output nil
                             :params (list (style-name self) (hand self) (finger self) current-position))))
  
(defmethod release-recoil ((mtr-mod dual-execution-motor-module) &key hand finger r theta request-spec)
  (unless (or (check-jam mtr-mod) (check-specs 'release-recoil hand finger r theta))
    (when (symbolp theta)
      (setf theta (symbol-value theta)))
    (prepare-movement mtr-mod (make-instance 'release-recoil :hand hand :finger finger :r r :theta theta :request-spec request-spec))))
  
(extend-manual-requests (release-recoil hand finger r theta) handle-style-request)  


;; Style for moving a finger without striking the key

(defclass point-finger (peck)
  nil
  (:default-initargs
      :style-name 'peck
      :two-exec-p nil
      :release-if-down :penalty))



(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self point-finger))
  ;; compute a real exec time now to send the change in hand position
  
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (setf (move-time self)
      (max (burst-time mtr-mod)
           (randomize-time (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self)))))
  
    (max (init-time mtr-mod)
         (move-time self))))

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self point-finger))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (move-time self))))
                                                                             
(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (self point-finger))
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self))))

(defmethod point-finger ((mtr-mod motor-module) &key hand finger r theta request-spec)
  (unless (or (check-jam mtr-mod) (check-specs 'point-finger hand finger r theta))
    (when (symbolp theta)
      (setf theta (symbol-value theta)))
    (prepare-movement mtr-mod (make-instance 'point-finger :hand hand :finger finger :r r :theta theta :request-spec request-spec))))

(extend-manual-requests (point-finger hand finger r theta) handle-style-request)


;;; These are the new "macro" actions which take a key name and then generate 
;;; the appropriate low-level style as needed based on current finger position and
;;; spcified target. 

;;; Start with some functions that return useful keyboard information.

;;; Determine which hand and finger is used to strike the key based on the
;;; touch typing commands predefined for the keyboad.

(defun key->hand&finger (keyboard key)
  (unless (stringp key)
    (setf key (princ-to-string key)))
         
  (let ((command (key->cmd keyboard key)))
    (if (null (first command))
        (print-warning "No hand and finger mapping available for key ~s" key)
      (let* ((hand-index (position :hand command))
             (hand-name (nth (1+ hand-index) command))
             (finger-index (position :finger command))
             (finger-name (nth (1+ finger-index) command)))
        (values hand-name finger-name)))))
  

;;; Get a finger's home position.

(defun home-position (keyboard hand finger)
  (let* ((positions
          (case hand
            (left (keyboard-hand-location keyboard 'left-home))
            (right (keyboard-hand-location keyboard 'right-home))))
         (h-pos (find 'hand positions :key 'first))
         (f-pos (find finger positions :key 'first)))
    (if (and f-pos h-pos)
        (coerce (mapcar '+ (rest f-pos) (rest h-pos)) 'vector)
      (print-warning "No home-position available for hand ~s and finger ~s." hand finger))))

;;; Here's the general pattern for many of the new finger-based keyboard actions

(defmethod extended-finger-keyboard ((mtr-mod dual-execution-motor-module) spec name 
                                     &key (same-loc-action 'punch) (move-loc-action 'peck-recoil)
                                     (same-loc-key-only nil) (move-loc-key-only nil))
                                     
  (if (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
      (aif (current-keyboard)
           (let ((key (verify-single-explicit-value spec 'key :motor name)))
             (if key
                 (progn 
                   (unless (stringp key)
                     (setf key (princ-to-string key)))
                   
                   (multiple-value-bind (hand finger) (key->hand&finger it key)
                     (if (and hand finger)
                         (let ((device (device-for-hand hand :current nil)))
                           (if (string-equal (second device) "keyboard")
                               (let ((key-loc (key->loc it key))
                                     (finger-loc (finger-loc-m mtr-mod hand finger :current nil)))
                                 (if (vpt= key-loc finger-loc)
                                     (if same-loc-key-only
                                         (funcall same-loc-action key)
                                       (funcall same-loc-action mtr-mod :hand hand :finger finger :request-spec spec))
                                   (let ((vector (xy-to-polar finger-loc key-loc)))
                                     (if move-loc-key-only
                                         (funcall move-loc-action key)
                                       (funcall move-loc-action mtr-mod :hand hand :finger finger :r (vr vector) :theta (vtheta vector) :request-spec spec)))))
                             (print-warning "The ~s hand is not (or will not be) on the keyboard device for ~s request." hand name)))
                       (print-warning "Could not determine a hand and finger for key ~s in ~s request." key name))))
               (print-warning "No valid key value found in ~s request." name)))
           (print-warning "No keyboard device available for ~s request." name))
    (print-warning "Keyboard device not installed for motor interface in ~s request." name)))

;; type-key
;; Similar to the press-key command except that it doesn't assume the finger
;; starts from the home row.  It uses the appropriate finger's starting position
;; to determine the movement necessary to strike the key and return to where it
;; started (which may or may not be the home row).


(defmethod type-key ((mtr-mod dual-execution-motor-module) spec)
  (extended-finger-keyboard mtr-mod spec 'type-key))

(extend-manual-requests (type-key key) type-key)


;; hit-key
;; Similar to the press-key command except that it doesn't assume the finger
;; starts from the home row (it uses the current position) and it stays at the
;; key which was hit instead of returning to the home position.

(defmethod hit-key ((mtr-mod dual-execution-motor-module) spec)
  (extended-finger-keyboard mtr-mod spec 'type-key :move-loc-action 'peck))

(extend-manual-requests (hit-key key) hit-key)


;; hold-key
;; Like the hit-key action except that it continues to hold the key that is struck.

(defmethod hold-key ((mtr-mod dual-execution-motor-module) spec)
  (extended-finger-keyboard mtr-mod spec 'type-key :same-loc-action 'hold-punch :move-loc-action 'hold-peck))

(extend-manual-requests (hold-key key) hold-key)

;; release-key
;; If the designated key is being held down release it and leave the finger over it.

(defmethod release-key ((mtr-mod dual-execution-motor-module) spec)
  (extended-finger-keyboard mtr-mod spec 'release-key :same-loc-action 'release 
                            :move-loc-key-only t
                            :move-loc-action (lambda (key)
                                               (print-warning "Cannot release the ~s key because the appropriate finger is not on that key in release-key request." key))))

(extend-manual-requests (release-key key) release-key)


;; release-key-to-home
;; If the designated key is being held down release it and return the finger to the
;; home position.

(defmethod release-key-to-home ((mtr-mod dual-execution-motor-module) spec)
  
  (extended-finger-keyboard mtr-mod spec 'release-key-to-home 
                            :same-loc-action (lambda (m-mod &key hand finger request-spec)
                                               (let* ((home-pos (home-position (current-keyboard) hand finger))
                                                      (finger-loc (finger-loc-m m-mod hand finger :current nil))
                                                      (vector (xy-to-polar finger-loc home-pos)))
                                                 (if (zerop (vr vector))
                                                     (release m-mod :hand hand :finger finger :request-spec request-spec)
                                                   (release-recoil m-mod :hand hand :finger finger :r (vr vector) :theta (vtheta vector) :request-spec request-spec)))
                                               )
                            :move-loc-key-only t
                            :move-loc-action (lambda (key)
                                               (print-warning "Cannot release the ~s key because the appropriate finger is not on that key in release-key-to-home request." key))))


(extend-manual-requests (release-key-to-home key) release-key-to-home)


;; move-to-key
;; Position the appropriate finger over the designated key without striking it.

(defmethod move-to-key ((mtr-mod dual-execution-motor-module) spec)
  (extended-finger-keyboard mtr-mod spec 'move-to-key 
                            :same-loc-key-only t
                            :same-loc-action (lambda (key)
                                               (print-warning "Move-to-key request for key ~s ignored because appropriate finger is already at that key." key))
                            :move-loc-action 'point-finger))

(extend-manual-requests (move-to-key key) move-to-key)


;; move-finger-to-home 
;; Return a specified finger to the home position without striking that key.

;; could probably get tricky and add the home-key for the given finger to the spec 
;; and use the extended-finger-keyboard request, but don't want to mess around with
;; altering specs.

(defmethod move-finger-to-home ((mtr-mod dual-execution-motor-module) spec)
  
  (if (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
      (aif (current-keyboard)
           (let ((hand (verify-single-explicit-value spec 'hand :motor 'move-finger-to-home))
                 (finger (verify-single-explicit-value spec 'finger :motor 'move-finger-to-home)))
             
             (if (and hand finger)
                 (let ((device (device-for-hand hand :current nil)))
                   (if (string-equal (second device) "keyboard")
                       
                       (let ((home-pos (home-position it hand finger))
                             (finger-loc (finger-loc-m mtr-mod hand finger :current nil)))
                         (if (vpt= home-pos finger-loc)
                             (print-warning "Cannot move the ~s ~s finger to home position because it is already there in move-finger-to-home request." hand finger)
                           (let ((vector (xy-to-polar finger-loc home-pos)))
                             (point-finger mtr-mod :hand hand :finger finger :r (vr vector) :theta (vtheta vector) :request-spec spec))))
                     (print-warning "The ~s hand is not (or will not be) on the keyboard device for move-finger-to-home request." hand)))
               (print-warning "Hand and finger not provided for move-finger-to-home request.")))
           (print-warning "No keyboard device available for move-finger-to-home request."))
    (print-warning "Keyboard device not installed for motor interface in move-finger-to-home request.")))
             
(extend-manual-requests (move-finger-to-home hand finger) move-finger-to-home)


;;; all-fingers-to-home
;;; A style to move all of the fingers on one or both hands to the home row.
;;; The cost for the action is the max of 200ms, the minimum time set for a 
;;; fitts action, and the times for each hand to perform a hand-ply to the home location.
;;; The init-time is not added to the exec time, only to the finish time.
;;;
;;;
;;; Does mark the fingers as busy in the process. (prior to version 5.0 it did not).
;;;

(defclass all-fingers-to-home (hfrt-movement)
  nil
  (:default-initargs
    :style-name :all-home
    :hand 'both
    :finger-based-style t ; so the fingers are marked busy and released
    :finger :dummy))

(defmethod num-to-prepare :around ((self all-fingers-to-home))
  ;; don't want to count the direction as a possible feature
  ;; to match previous version
  (- (call-next-method) 1))


(defmethod prepare-features ((mtr-mod motor-module) (self all-fingers-to-home))
  ;; Assuming that the need for a movement has already been determined
  ;; and the hand is set appropriately (only one even if both specified
  ;; but one is already there) as well as the installation of a keyboard.
  
  (let ((hand (hand self))
        (keyboard-device (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal))
        (k (current-keyboard))
        l-pos r-pos)
    
    (when (or (eq hand 'both)
              (eq hand 'left))
      (let* ((keyboard-hand (keyboard-hand-location k 'left-home))
             (home-pos (coerce (rest (find 'hand keyboard-hand :key 'first)) 'vector))
             (home-offsets (mapcar (lambda (x)
                                     (list (first x) (vector (second x) (third x))))
                             (remove 'hand keyboard-hand :key 'first)))
             (hand-pos (hand-position 'left :current nil))
             (vector (xy-to-polar (hand-pos-loc hand-pos) home-pos))
             (device (device-for-hand 'left :current nil)))
        
        (setf l-pos (list (make-hand-pos :loc home-pos
                                         :fingers home-offsets
                                         ;; none are down so just set that 
                                         :other (let ((start (remove 'extended-fingers (hand-pos-other hand-pos) :key 'first)))
                                                  (push (list 'extended-fingers
                                                              (list 'index nil)
                                                              (list 'middle nil)
                                                              (list 'ring nil)
                                                              (list 'pinkie nil)
                                                              (list 'thumb nil))
                                                        start)
                                                  start)
                                         :device (if (equalp device keyboard-device)
                                                     nil
                                                   device))
                          (vr vector)))))
    
    (when (or (eq hand 'both)
              (eq hand 'right))
      (let* ((keyboard-hand (keyboard-hand-location k 'right-home))
             (home-pos (coerce (rest (find 'hand keyboard-hand :key 'first)) 'vector))
             (home-offsets (mapcar (lambda (x)
                                     (list (first x) (vector (second x) (third x))))
                             (remove 'hand keyboard-hand :key 'first)))
             (hand-pos (hand-position 'right :current nil))
             (vector (xy-to-polar (hand-pos-loc hand-pos) home-pos))
             (device (device-for-hand 'right :current nil)))
        
        (setf r-pos (list (make-hand-pos :loc home-pos
                                         :fingers home-offsets
                                         ;; none are down so just set that 
                                         :other (let ((start (remove 'extended-fingers (hand-pos-other hand-pos) :key 'first)))
                                                  (push (list 'extended-fingers
                                                              (list 'index nil)
                                                              (list 'middle nil)
                                                              (list 'ring nil)
                                                              (list 'pinkie nil)
                                                              (list 'thumb nil))
                                                        start)
                                                  start)
                                         :device (if (equalp device keyboard-device)
                                                     nil
                                                   device))
                          (vr vector)))))
    
    (case hand
      (left
       (setf (updated-pos self) (first l-pos))
       (setf (r self) (second l-pos)))
      (right
       (setf (updated-pos self) (first r-pos))
       (setf (r self) (second r-pos)))
      (both
       (setf (updated-pos self) (list (first l-pos) (first r-pos)))
       (setf (r self) (max (second l-pos) (second r-pos)))))))


(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self all-fingers-to-home))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (max 
     (init-time mtr-mod) ;; to be safe since not adding it here
     (randomize-time
      (max .2 (fitts mtr-mod (mouse-fitts-coeff mtr-mod) (r self) 4.0))))))
                

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self all-fingers-to-home))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (exec-time self))))


(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (self all-fingers-to-home))
  ;; just need to set the hand-position and the device when needed
  ;; any release events are automatically generated now
  
  (when (or (eq (hand self) 'left) (eq (hand self) 'both))
    (let ((new-pos (if (eq (hand self) 'both)
                       (first (updated-pos self))
                     (updated-pos self))))
      
      (when (hand-pos-device new-pos)
        (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-device :time-in-ms t
                                 :module :motor :params (list 'left (hand-pos-device new-pos)) :priority 1 :output nil))
      
      (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                               :destination :motor :params (list 'left new-pos)
                               ;; Show an action now
                               :details (concatenate 'string (symbol-name 'all-fingers-to-home) " " (symbol-name 'left)))))
  
  (when (or (eq (hand self) 'both) (eq (hand self) 'right))
    (let ((new-pos (if (eq (hand self) 'both)
                       (second (updated-pos self))
                     (updated-pos self))))
      
      (when (hand-pos-device new-pos)
        (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-device :time-in-ms t
                                 :module :motor :params (list 'right (hand-pos-device new-pos)) :priority 1 :output nil))
      
      (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                               :destination :motor :params (list 'right new-pos)
                               ;; Show an action now
                               :details (concatenate 'string (symbol-name 'all-fingers-to-home) " " (symbol-name 'right))))))
  


;; all-fingers-to-home
;; Return all the fingers on the specified hand (or both) to the home positions
;; without striking any keys.

(defmethod all-fingers-to-home ((mtr-mod dual-execution-motor-module) spec)
  (let ((key (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)))
    (if key
      (aif (current-keyboard)
           (let* ((slot-specs (and (slot-in-chunk-spec-p spec 'hand) (chunk-spec-slot-spec spec 'hand)))
                  (hands (cond ((zerop (length slot-specs)) '(left right))
                               ((and (= (length slot-specs) 1)
                                     (eql '= (caar slot-specs))
                                     (or (eq (third (car slot-specs)) 'left)
                                         (eq (third (car slot-specs)) 'right)
                                         (eq (third (car slot-specs)) 'both)))
                                (if (eq (third (car slot-specs)) 'both)
                                    '(left right)
                                  (list (third (car slot-specs)))))
                                
                               (t (print-warning "Invalid hand specified in all-fingers-to-home request.  No action taken."))))
                  (any-to-move nil))
             
             (when hands
               (dolist (hand hands)
                 
                 ;; need to check the hand position before all of the fingers
                 ;; because it could be in an odd state of the hand in the
                 ;; wrong place but with finger offsets to put them in the
                 ;; home positions...
                 (let* ((hand-pos (hand-position hand :current nil))
                        (keyboard-hand (if (eq hand 'left) 
                                           (keyboard-hand-location it 'left-home) 
                                         (keyboard-hand-location it 'right-home)))
                        (home-pos (coerce (rest (find 'hand keyboard-hand :key 'first)) 'vector))
                        (vector (xy-to-polar (hand-pos-loc hand-pos) home-pos))
                        (device (device-for-hand 'left :current nil)))
                   (if (or (not (equalp device key))
                           (not (zerop (vr vector)))
                           (any-finger-down (extension mtr-mod) hand :current nil))
                       ;; hand not on device, not in start position, or some finger down
                       ;; it needs to be moved
                       (push hand any-to-move)
                     
                     ;; otherwise check all the fingers for matching position
                     ;; know hand is in home position so easiest to check position
                     
                     (dolist (finger '(index middle ring pinkie thumb))
                       (let ((home-pos (home-position it hand finger))
                             (finger-loc (finger-loc hand-pos finger)))
                         (unless (vpt= home-pos finger-loc)
                           (pushnew hand any-to-move))))))))
               
               (if any-to-move
                   (unless (check-jam mtr-mod) 
                     (prepare-movement mtr-mod (make-instance 'all-fingers-to-home
                                                 :hand (if (= (length any-to-move) 2)
                                                           'both
                                                         (first any-to-move))
                                                 :request-spec spec)))
                 (print-warning "All-fingers-to-home request ignored because fingers are already at home positions.")))
           
           (print-warning "No keyboard device available for all-fingers-to-home request."))
    (print-warning "Keyboard device not installed for motor interface in all-fingers-to-home request."))))


(extend-manual-requests (all-fingers-to-home hand) all-fingers-to-home)
       

;; hold-mouse
;; Like click-mouse except that it does not release the button.
;; Just use hold-punch with the right index finger.

(defmethod hold-mouse ((mtr-mod dual-execution-motor-module) spec)
  (let* ((model (current-model))
         (mouse (find-model-cursor (list model 'mouse))))
    (if mouse
        (if (equalp (device-for-hand 'right :current nil) 
                    (list "motor" "cursor" "mouse"))
            (hold-punch mtr-mod :hand 'right :finger 'index :request-spec spec)
          (model-warning "Hold-mouse requested when right hand not at mouse."))
      (model-warning "Hold-mouse requested but no mouse device available."))))
  

(extend-manual-requests (hold-mouse) hold-mouse)

;; release-mouse
;; If the mouse button is being held release it (release does
;; the check for whether the finger is down or not).

(defmethod release-mouse-button ((mtr-mod dual-execution-motor-module) spec)
  (let* ((model (current-model))
         (mouse (find-model-cursor (list model 'mouse))))
    (if mouse
        (if (equalp (device-for-hand 'right :current nil) 
                    (list "motor" "cursor" "mouse"))
            (release mtr-mod :hand 'right :finger 'index :request-spec spec)
          (model-warning "Release-mouse requested when right hand not at mouse."))
      (model-warning "Release-mouse requested but no mouse device available."))))
    

(extend-manual-requests (release-mouse) release-mouse-button)


;; release-all-fingers
;; 
;; Will delay if preparation not free.
;; Checks whether either hand requires lifting fingers:
;;  - if neither do then it's a no-op 
;;  - if only one does then only that hand requires execution
;;  - if both do then requires execution of both
;;

;; Let the default release mechanism handle the actual releasing.
;; So only need to schedule the update the position.

(defclass release-all-fingers (punch)
  nil
  (:default-initargs
    :style-name :release-all-fingers
    :hand 'both
    :two-exec-p nil
    :feature-slots '(hand)
    :release-if-down :free
    :finger-based-style t
    :finger :dummy
    ))

(defmethod num-to-prepare :around ((self release-all-fingers))
  ;; don't want to count the hand to match previous version
  (- (call-next-method) 1))

(defmethod prepare-features ((mtr-mod motor-module) (self release-all-fingers))
  ;; Assuming that the need for a release has already been determined
  ;; and the hand is set appropriately (only one or both if needed).
  
  (let ((hand (hand self))
        left
        right)
    
    (when (or (eq hand 'both)
              (eq hand 'left))
      (setf left (copy-hand-pos (hand-position 'left :current nil)))
      
      ;; just note that all fingers are up
      
      (let ((start (remove 'extended-fingers (hand-pos-other left) :key 'first)))
        (push (list 'extended-fingers
                    (list 'index nil)
                    (list 'middle nil)
                    (list 'ring nil)
                    (list 'pinkie nil)
                    (list 'thumb nil))
              start)
        (setf (hand-pos-other left) start)))
    
    (when (or (eq hand 'both)
              (eq hand 'right))
      (setf right (copy-hand-pos (hand-position 'right :current nil)))
      
      ;; just note that all fingers are up
      
      (let ((start (remove 'extended-fingers (hand-pos-other right) :key 'first)))
        (push (list 'extended-fingers
                    (list 'index nil)
                    (list 'middle nil)
                    (list 'ring nil)
                    (list 'pinkie nil)
                    (list 'thumb nil))
              start)
        (setf (hand-pos-other right) start)))
    
    (case hand
      (left
       (setf (updated-pos self) left))
      (right
       (setf (updated-pos self) right))
      (both
       (setf (updated-pos self) (list left right))))))


(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self release-all-fingers))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (key-release-time mtr-mod))))

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self release-all-fingers))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (* 2 (burst-time mtr-mod)))))

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (self release-all-fingers))
  
  (when (or (eq (hand self) 'left) (eq (hand self) 'both))
    (let ((new-pos (if (eq (hand self) 'both)
                       (first (updated-pos self))
                     (updated-pos self))))
      
      (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                               :destination :motor :params (list 'left new-pos)
                               :output nil)))
  
  (when (or (eq (hand self) 'both) (eq (hand self) 'right))
    (let ((new-pos (if (eq (hand self) 'both)
                       (second (updated-pos self))
                     (updated-pos self))))
      
      (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                               :destination :motor :params (list 'right new-pos)
                               :output nil)))  
  
  ;; for backward compatibility keep the same notice in the trace instead of adding it to
  ;; the set-hand-position actions even though this function doesn't need to do anything now.
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'releasing-all-fingers :time-in-ms t :module :motor :output 'medium
                           :params nil))

(defun releasing-all-fingers ())
   
(defmethod release-all-fingers ((mtr-mod dual-execution-motor-module) request-spec)
  (if (eq (bt:with-lock-held ((state-lock mtr-mod)) (prep-s mtr-mod)) 'busy)
      (schedule-event-after-module :motor 'release-all-fingers :maintenance t :output 'medium 
                                   :destination :motor :module :motor
                                   :params (list request-spec)
                                   :details "delayed-release-all-fingers")
    (let (left right)
      (when (any-finger-down (extension mtr-mod) 'left :current nil)
        (setf left t))
      (when (any-finger-down (extension mtr-mod) 'right :current nil)
        (setf right t))
      (if (or left right)
          (schedule-event-now 'prepare-movement :module :motor
                              :destination :motor
                              :params (list (make-instance 'release-all-fingers 
                                              :hand (cond ((and left right) 'both)
                                                          (left 'left)
                                                          (t 'right))
                                              :request-spec request-spec))
                              :output 'low
                              :details "release-all-fingers")
        (print-warning "Release-all-fingers action called when no fingers need to be released.")))))
      
(extend-manual-requests (release-all-fingers) release-all-fingers)



;;; A punch with a release based on given time
;;; doesn't use the key-release-time and assumes
;;; that is accounted for in the delay.

(defclass delayed-punch (punch)
  ((delay :accessor delay :initarg :delay :initform .050))
  (:default-initargs
      :finger-based-style t
      :release-if-down :penalty))

;; the prepare for a punch works fine

;; as does the initial execution time

(defmethod compute-second-exec-time ((mtr-mod dual-execution-motor-module) (self delayed-punch))
 (bt:with-recursive-lock-held ((param-lock mtr-mod))
   (setf (delay self) (randomize-time (delay self)))
   (+ (init-time mtr-mod) 
      (key-closure-time mtr-mod)
      (delay self))))

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self delayed-punch))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (key-closure-time mtr-mod)
       (delay self)
       (burst-time mtr-mod))))

(defmethod queue-output-events ((mtr-mod dual-execution-motor-module) (self delayed-punch))
  
  (schedule-event-relative (seconds->ms (exec2-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  
  (schedule-event-relative (seconds->ms (exec-time self))
                           'set-finger-down :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self))))

  (schedule-event-relative (seconds->ms (exec2-time self))
                           'set-finger-up :time-in-ms t :destination :motor :module :motor :output nil
                           :params (list (style-name self) (hand self) (finger self) (finger-loc (updated-pos self) (finger self)))))


(defun parse-delay-time (module delay)
  (bt:with-recursive-lock-held ((param-lock module))
    (cond ((or (null delay) (eq delay 'default)) (d-punch-default module))
          ((and (numberp delay) (posnum delay))
           delay)
          ((numberp delay)
           (model-warning "Non-positive delays not allowed in a delayed-punch. Using default instead.")
           (d-punch-default module))
          ((eq delay 'slow) (d-punch-slow module))
          ((eq delay 'fast) (d-punch-fast module))
          (t (model-warning "Invalid delay value ~s to a delayed-punch. Using default instead" delay)
             (d-punch-default module)))))


(defmethod handle-delayed-punch-request ((mtr-mod dual-execution-motor-module) chunk-spec)
  (let* ((hand (verify-single-explicit-value 
                chunk-spec 'hand
                :motor 'delayed-punch))
         (finger (verify-single-explicit-value 
                  chunk-spec 'finger
                  :motor 'delayed-punch))
         (given-delay (when (slot-in-chunk-spec-p chunk-spec 'delay)
                        (verify-single-explicit-value 
                         chunk-spec 'delay
                         :motor 'delayed-punch)))
         (delay (parse-delay-time mtr-mod given-delay)))
    (if (and hand finger delay)
        (unless (check-jam mtr-mod)
          (schedule-event-now 'prepare-movement 
                              :destination :motor :module :motor 
                              :params (list (make-instance 'delayed-punch :hand hand :finger finger :delay delay :request-spec chunk-spec))
                              :output 'low
                              :details (format nil "~s ~s ~s ~s ~s ~s ~f" 'delayed-punch 'hand hand 'finger finger 'delay delay)))
      (print-warning "Invalid delayed-punch request with chunk-spec ~S" chunk-spec))))

(extend-manual-requests (delayed-punch hand finger delay) handle-delayed-punch-request)


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
