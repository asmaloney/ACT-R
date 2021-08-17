;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2010 Mike Byrne/Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : motor.lisp
;;; Version     : 7.4
;;; 
;;; Description : Source code for the ACT-R Motor Module.
;;; 
;;; Bugs        : 
;;; To do       : [X] Consider a mechanism for extending the commands that are
;;;             :     accepted by the motor module without having to redefine the
;;;             :     the request function.
;;;             : [ ] Should a jam signal an error state?
;;;             : [ ] Let subtypes of prepare be passed in as a request
;;;             :     and generalize how it pulls the keyword values out of
;;;             :     the slots so that new styles with different features
;;;             :     can also be prepared.
;;;             : [X] Figure out a good way for move-cursor to be abstracted
;;;             :     away from needing to access the internals of vision.
;;;             : [ ] When the hand move has finger offsets it probably should
;;;             :     test if the cost of any necessary finger move is greater
;;;             :     than the hand move and adjust the time accordingly.
;;;             : [ ] Consider not testing the features in handle-style-request
;;;             :     for efficiency since defstyle already includes a check-specs
;;;             :     call.
;;;             : [ ] Should a point-hand action send a notification to the device?
;;;
;;; 
;;; ----- History -----
;;; 2004.12.23 Dan [First pass at moving to ACT-R 6]
;;;             : Changed name to motor and reset version to 1.0a1
;;;             :
;;;             : Eliminated the state chunk
;;;             :
;;;             : Making the requests explicit instead of using a mapping
;;;             :   mechanism as was done in ACT-R 5 (at least for now).
;;;             : 
;;;             : Added :from :motor to the queue-command calls that
;;;             :  are events for the device.
;;;             :
;;;             : Doesn't flag any error states at this point (jams or 
;;;             : otherwise) nor does it put a "response" chunk into the buffer
;;;             : which has been a suggestion for a while.
;;;             :
;;;             : Removed the defgeneric for loc->key, check-specs, and
;;;             :   polar-move-xy.
;;;             :
;;;             : Renamed reset-module reset-pm-module.
;;;             :
;;; 2004.12.28 Dan
;;;             : Added pm-start-hand-at-mouse here. [Should all the names
;;;             : have the pm- taken off?]
;;;             :
;;;             : In move-cursor, because the chunk name of the copy passed
;;;             : as the location doesn't match the original
;;;             : stored by vision, loc-to-feat ends up returning a random
;;;             : feature because (gethash (id loc) (found-locs vis-m))
;;;             : ends up returning nil.
;;;             : So instead, since all move-cursor cares about is the xy
;;;             : I'm pulling that out directly in a dummy feature.
;;;             : May have to do a similar thing with the object, but
;;;             : I'm not sure on that one.
;;; 2005.01.11 mdb
;;;             : * Changed some of the request/query interaction.
;;;             : * Added some toplevel commands.
;;;             : * Added doc strings for parameters.
;;; 2005.01.12 Dan
;;;             : * Moved the device's parameters to the device module.
;;; 2005.02.03 Dan
;;;             : * Added ":output 'medium"  or ":output 'low" to some of the 
;;;             :   events scheduled to play friendly with the new detail level.
;;; 2005.02.10 Dan
;;;             : * Switched expt to expt-coerced in compute-exec-time and
;;;             :   log to log-coerced in fitts.
;;; 2005.04.23 Dan
;;;             : * Added the status printing function to the module's buffer
;;;             :   definition.
;;;             : * Noticed that the last-command query is unhandled...
;;; 2005.05.02 Dan
;;;             : * Added another command from the old rpm-toplevel file - 
;;;             :   set-hand-location and moved the pm- functions to the
;;;             :   backwards.lisp file.
;;; 2005.05.11 Dan
;;;             : * Added ":output 'medium" to the output-key command
;;;             :   events so they show in the medium trace level.
;;;             : * Probably want to do that for other things too at some
;;;             :   point...
;;; 2005.09.28 Dan
;;;             : * Slight change in the reset-motor-module function so that
;;;             :   the chunk-types are defined before calling reset-pm-module
;;;             :   to allow for extensions to define the reset-pm-module
;;;             :   method for adding chunk-types and let them use the motor-
;;;             :   command chunk-type.
;;; 2005.12.16 Dan
;;;             : * Added a new command to the manual to prepare features:
;;;             :   +manual> isa prepare style <style>.
;;;             :   effectively the same as calling prepare-motor, but it
;;;             :   only allows for the style to be prepared.  May want to
;;;             :   extnd that at some point, but currently one would have to
;;;             :   prespecify all possible features to be accepted in the
;;;             :   chunk-type which isn't ideal...
;;; 2006.08.22 Dan
;;;             : * Fixed the prepare request because it wasn't actually 
;;;             :   making the module busy during the preparation - I called 
;;;             :   the wrong pm method to do the preparation.
;;; 2006.08.24 Dan
;;;             : * Undoing an old change (from 2004.12.28) because the
;;;             :   assumption was wrong (that only the xy matters) and now
;;;             :   loc-to-feat works right anyway.  So, move-cursor does use
;;;             :   loc-to-feat again.
;;; 2006.08.29 Dan
;;;             : * Added a features slot to the prepare chunk-type so that
;;;             :   the rest of the features can be passed into a prepare
;;;             :   request - it must be a list of the features as would be
;;;             :   passed to the prepare command not counting the style (which
;;;             :   must be specified in the style slot:
;;;             :   +manual> isa prepare style punch features (:hand right)
;;;             :   as an example.
;;; 2006.09.08 Dan
;;;             : * Changed some of the param tests to nonneg instead of posnum.
;;;             : * Cleaned up noisy-loc? so that it doesn't call act-r-noise
;;;             :   with 0.
;;; 2006.09.13 Dan
;;;             : * Changed the defstyle of peck to add the keywords so that
;;;             :   an explict peck request will work (not sure if there is
;;;             :   a bug in defstyle but this was sufficient to fix things).
;;; 2006.09.15 Dan
;;;             : * Changed move-finger to test that r was not 0 instead of
;;;             :   theta before progressing (which I assume is how it should
;;;             :   be).
;;; 2007.01.05 Dan
;;;             : * Synched all the versions to 2.3 (file, object, module).
;;;             : * Changed the module doc string to remove the "first pass".
;;; 2007.01.08 Dan
;;;             : * Adding in the execute request - since prepartion now allows
;;;             :   for a full movement specification it seems to make sense to
;;;             :   actually have this now.
;;; 2007.01.09 Dan
;;;             : * Fixed a bug with home-hands - the quoted lists caused problems
;;;             :   in some Lisps (treated like a constant so once changed it
;;;             :   stayed that way).
;;;             : * Patched press-key so that if an invalid key is specified
;;;             :   it prints a warning instead of throwing an error.
;;;             : * Made point-hand-at-key check to make sure that there's a
;;;             :   valid position for the key before issueing the command.
;;;             : * Changed the prepare request so that hand, finger, r, and theta
;;;             :   are supplied directly instead of in a list because all the
;;;             :   "styles" only use those arguments.
;;;             : * Made start-hand-at-mouse, set-cursor-position, and set-hand-
;;;             :   location all better check for current mp/model/module.
;;;             : * Changed the home position for the left thumb to be offset
;;;             :   by (1 2) instead of (-1 2) which is the setting of the right
;;;             :   thumb (they shouldn't have been the same and the values for
;;;             :   the right thumb seemed correct to me...)
;;; 2007.01.15 Dan
;;;             : * Added a saftey check to the fitts method to catch situations
;;;             :   where a 0 or - width is provided to print a warning and then
;;;             :   assume 1 pixel width for the target.
;;; 2007.01.16 Dan
;;;             : * Fixed a warning in the handling of a prepare request.
;;; 2007.06.04 Dan
;;;             : * Added the extend-manual-requests command to allow people to
;;;             :   add new requests to the manual system without having to 
;;;             :   modify the request and reset functions.
;;; 2007.06.21 Dan
;;;             : * Modified move-cursor to deal with the new chunk based feature
;;;             :   representation.
;;; 2008.06.11 Dan 
;;;             : * Changed noisy-loc? to more accurately reflect the 4%
;;;             :   chance of missing by only computing one distance instead
;;;             :   of doing it separately for each axis.
;;; 2008.10.28 Dan
;;;             : * Fixed some issues with move-cursor relative to the possible
;;;             :   deletion of chunks by the vision module.
;;; 2010.10.06 mdb
;;;             : * Changed movement to use mininmum jerk velocity profile for
;;;             :   cursor moves. 
;;;             : * Allow incremental mouse moves to update at schedules
;;;             :   other than 50 ms.
;;; 2010.10.06 mdb
;;;             : * Added missing MINJERK-DIST function.
;;; 2011.02.07 Dan
;;;             : * Updated move-cursor to to correspond to the change in how
;;;             :   vision stores it's info, but really need to abstract that
;;;             :   since motor shouldn't have to be dependent on the internals
;;;             :   of vision and need to call its methods.
;;;             : * Also added a warning to move-cursor if it aborts because of
;;;             :   a 0 pixel move.
;;; 2011.05.16 Dan
;;;             : * Replaced pm-warning calls with model-warning.
;;; 2011.05.17 Dan
;;;             : * Replaced queue-command calls with schedule-event-relative.
;;; 2013.10.01 Dan
;;;             : * Update move-cursor so that incremental movements with a
;;;             :   noisy location are still a straight line to fix an issue 
;;;             :   reported by Melissa Gallagher.
;;; 2014.02.12 Dan
;;;             : * The request chunk-types are no longer subtypes of motor-command
;;;             :   since that wasn't actually used for anything.
;;; 2014.04.24 Dan
;;;             : * Added a create-motor-module function instead of using a
;;;             :   lambda in the definition so that it can be changed for
;;;             :   easier extension of the module.
;;; 2014.05.16 Dan [3.0]
;;;             : * Start the conversion to chunks without types.
;;;             : * Remove the require for "DMI" since it's not needed.
;;;             : * Removing a lot of old code that was commented out.
;;;             : * Have all the built-in requests except for clear and prepare
;;;             :   created using the extend-manual-requests command.  That way
;;;             :   everything is treated the same and the request function is
;;;             :   much cleaner.
;;; 2014.05.19 Dan
;;;             : * Have extend-manual-requests create the chunks which match
;;;             :   the type as isa that type.
;;; 2014.05.30 Dan
;;;             : * Use the test-for-clear-request function in pm-module-request.
;;; 2014.10.31 Dan
;;;             : * Define prepare as 'isa preapare' instead of 'name prepare'.
;;; 2014.12.01 Dan
;;;             : * Automatically define chunks named: left, right, index, 
;;;             :   middle, ring, pinkie, and thumb for the motor module if not
;;;             :   already chunks.
;;; 2015.05.20 Dan
;;;             : * Updates to the xy-loc calls and removing specific coordinate
;;;             :   slot names and use those indicated for the device in the mouse
;;;             :   movement method.
;;;             : * Approach-width needs to pass the vision module in as well.
;;; 2015.06.04 Dan
;;;             : * Convert exec-times to ms explicitly and then use :time-in-ms t 
;;;             :   in all scheduled events.
;;;             : * Changed incremental-mouse-p to ms at setting time and do the
;;;             :   math in ms.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2015.08.13 Dan
;;;             : * Changed all rand-time calls to randomize-time.
;;; 2015.08.31 Dan [3.1]
;;;             : * The point-hand-at-key style now includes an optional feature
;;;             :   (which doesn't increase prep cost) that indicates the state
;;;             :   of the fingers at the target location.  Previously it just
;;;             :   kept the finger offsets that were currently set for the hand,
;;;             :   but peck actions could have adjusted those so that they aren't
;;;             :   right for something like a hand-to-mouse if the index finger
;;;             :   had been offset previously. 
;;;             :   The offsets feature can be:
;;;             :    - nil (the default and value if not provided) which means
;;;             :      to leave the current finger offsets in place
;;;             :    - standard which means the normal home position offsets of
;;;             :      0,0 for index and increasing/decreasing by one per finger
;;;             :      and -/+1,2 for the thumb.
;;;             :    - a list of three element lists where each sublist specifies
;;;             :      a finger name and then the x and y offset for that finger
;;;             :      when the action completes.
;;;             : *** When there are offsets that require moving the fingers
;;;             : *** it should probably compute the move time as the max of 
;;;             : *** the hand movement and the individual finger movements or
;;;             : *** especially if the hand move is to the same loc, but for
;;;             : *** now not changing that i.e. the finger movements are zero
;;;             : *** cost with the hand move.
;;;             : * The hand-to-home, hand-to-mouse, and set-hand-location 
;;;             :   actions set the standard finger offsets now.
;;; 2015.09.01 Dan
;;;             : * Added a hand-to-keypad request and a start-hand-at-keypad
;;;             :   command.  The finger positions are set over 4, 5, 6, enter,
;;;             :   and 0 which will be used to create the actions for some new
;;;             :   press-key shortcuts.
;;; 2015.09.21 Dan [4.0]
;;;             : * Manual buffer now marked as trackable.
;;;             : * When preparing an action set the prepare-spec slot.
;;;             : * All the 'extended' motor action functions set the request-
;;;             :   spec of the style if they generate one.
;;;             : * All of the "simple-command" request functions are now passed
;;;             :   the chunk-spec because they need to indicate when it is complete.
;;; 2015.09.22 Dan
;;;             : * The "pseudo style" actions of point-hand, press-key, and
;;;             :   move-cursor have been updated to deal with the request completion
;;;             :   now.
;;; 2015.09.23 Dan
;;;             : * All the special actions make sure to complete the request in
;;;             :   the event of a jam or nop as well.
;;; 2017.01.13 Dan [5.0]
;;;             : * Start the work to remove the old device object interface and
;;;             :   replace it with the new general device mechanism for remote
;;;             :   interface use.
;;;             :  - Took the keyboard specific code out.
;;;             :  - Define an interface which has no init or remove methods
;;;             :    at this point.
;;;             :  - Each hand now specifies the device with which it is interacting
;;;             :    if there is one.
;;;             :  - The base actions now signal that device instead of assuming that
;;;             :    it's a keyboard (or mouse button). 
;;; 2017.01.18 Dan
;;;             : * Fixed a bug because an invalid cmd value in a request left
;;;             :   it uncompleted.
;;; 2017.01.20 Dan
;;;             : * Have peck and peck-recoil also notify the device as their
;;;             :   output action like punch does.
;;; 2017.03.02 Dan
;;;             : * Removed all the mouse code because that's now being handled 
;;;             :   by a mouse device.
;;; 2017.03.06 Dan
;;;             : * Removed the eff-cursor-loc slot as well.
;;;             : * Put the cursor-ply style back in here because devices other
;;;             :   than a mouse may be implemented and want to use it.
;;;             : * A virtual-cursor class should be used to create any devices
;;;             :   that can be accessed via move-cursor (mouse, joystick, etc).
;;;             :   That style assumes some details of that class and that the
;;;             :   device can return an instance when signaled with 'cursor-object.
;;;             :   The device->order code is no longer needed since the class
;;;             :   will have that as a slot.
;;;             : * Point-hand action now takes a device as well and if provided
;;;             :   the hand will be set to that device as part of the movement.
;;;             : * Set-cursor-position can be used for any installed cursor
;;;             :   device.
;;; 2017.03.07 Dan
;;;             : * Updated set-hand-location-fct to take an optional device.
;;;             : * Moved start-hand-at-mouse to the mouse device file.
;;;             : * Fixed bugs in set-cursor-position.
;;; 2017.03.08 Dan
;;;             : * Don't have a general cursor class since that wouldn't be
;;;             :   usable by an 'external' device.  Instead, a cursor has to
;;;             :   respond to a bunch of signals for the information.
;;;             : * xy-to-polar is just a function instead of a method and can
;;;             :   work on any sequence given.
;;; 2017.08.10 Dan
;;;             : * Adding a hand-lock slot to the hand class to protect access
;;;             :   to all the slots of the hand.
;;; 2017.08.11 Dan
;;;             : * Fixed a missing ) in queue-output-events for hand-ply objs.
;;; 2018.03.05 Dan
;;;             : * Signal-device was renamed to notify-device and renamed the
;;;             :   notify-device function that was here notify-motor-device.
;;; 2018.03.07 Dan
;;;             : * Adjusted move-cursor to not need the vision module because
;;;             :   there are now functions to get that info without it.
;;; 2018.05.02 Dan
;;;             : * Approach-width now gets passed the x and y of the starting
;;;             :   point as well to allow for better custom calculations.
;;; 2018.05.30 Dan [6.0]
;;;             : * Actually put some locks in the module to protect the params
;;;             :   and requests (internals are covered by the pm-module's locks).
;;;             : * Monitor key-closure-time, mouse-fitts-coeff,  instead of having to get it from
;;;             :   the device interface directly.
;;; 2018.06.01 Dan
;;;             : * Param-lock moved to the pm-module class.
;;; 2018.06.04 Dan
;;;             : * Set-cursor-position moved to the mouse code file.
;;;             : * Set-hand-location changed to take explicit x and y parameters
;;;             :   as well as an optional device, added remote versions, and
;;;             :   added more parameter testing.
;;; 2018.06.12 Dan
;;;             : * Only the external set-hand-location commands should do the
;;;             :   string conversion.
;;; 2018.07.26 Dan [6.1]
;;;             : * Removing the support for request completion.
;;; 2018.10.04 Dan [7.0]
;;;             : * Using the interface notification function to provide access
;;;             :   for getting and updating hand/finger info so devices don't 
;;;             :   need the motor module to manipulate it directly (like keyboard 
;;;             :   and cursor do currently).
;;; 2018.10.08 Dan
;;;             : * Added more interface opitons for getting internal info about
;;;             :   hands and fingers.
;;;             : * Should there be an extend-motor-notifications command like
;;;             :   extend-manual-requests?
;;; 2018.10.10 Dan
;;;             : * Change the version string setting to use create-motor-module
;;;             :   instead of specifically creating an instance of motor-module
;;;             :   so that extensions which subclass the module can report a
;;;             :   different version number.
;;; 2018.10.15 Dan
;;;             : * Schedule the call to set-hand-device for ply action because
;;;             :   the switch shouldn't happen until the execution finishes
;;;             :   (which matters now that devices typically refer to a single
;;;             :   thing e.g. keyboard or mouse instead of a monolitic object).
;;; 2018.10.25 Dan
;;;             : * Fixed the description of the default-target-width parameter
;;;             :   because it's not in DVA -- it's the explicit width used when
;;;             :   none provided.
;;;             : * Also using that value when a zero or negative width is given
;;;             :   to fitts function instead of 1 (still use 1 as the default
;;;             :   width regardless of the default-target-width parameter).
;;; 2018.10.31 Dan [7.1]
;;;             : * Added calls to notify-device-hand-set and -unset whenever
;;;             :   the hand changes devices.
;;;             : * Added a name slot to the hand class to indicate which it is.
;;; 2018.11.01 Dan
;;;             : * Moved the :default-target-width parameter to the device
;;;             :   interface and just monitor it here.
;;; 2018.11.02 Dan
;;;             : * Motor-interface function needs to be more careful about
;;;             :   re-stringing a device, but may need a better solution in
;;;             :   general.
;;; 2019.03.04 Dan
;;;             : * Distance= fixed so that it handles obtuse differences 
;;;             :   correctly -- when the difference in angles is > pi radians
;;;             :   need to check the other angle because previously this was
;;;             :   the result which is wrong:
;;;             :     1[5]: (DIRECTION= -2.7610862 2.8501358)
;;;             :     1[5]: returned NIL
;;; 2019.04.23 Dan
;;;             : * Create the chunk-type definition for motor extensions when
;;;             :   they're added instead of at every reset.
;;; 2019.10.01 Dan [7.2]
;;;             : * Cursor positions are now always x,y,z but move-cursor still
;;;             :   assumes movement in x,y only and warns if the z values differ.
;;;             : * xy-to-polar now makes sure to only use x,y coords since it may
;;;             :   be given x,y,z.
;;; 2020.01.10 Dan [7.3]
;;;             : * Removed the #' and lambdas from the module interface 
;;;             :   functions since  that's not allowed in the general system
;;;             :   now.
;;; 2020.03.23 Dan [7.4]
;;;             : * Move to using separate hand positions for features and 
;;;             :   actions because there're potential problems with the single
;;;             :   position situation that used to exist.
;;;             : * Add the time to the notify-motor-device event in the trace.
;;;             : * Point-hand has a keyword :style to specify a subclass of a
;;;             :   hand-ply when needed.
;;;             : * Added functions for the standard-offsets instead of creating
;;;             :   the lists over and over.
;;;             : * Handle-style-request and handle-partial-style-request now 
;;;             :   print a warning when they abort because of a missing or
;;;             :   over-specified feature.
;;; 2020.04.20 Dan
;;;             : * Fixed an issue with some Lisps not liking a default value 
;;;             :   in the keyword of a defgeneric.
;;; 2020.06.01 Dan [8.0]
;;;             : * Moved the chunk-type and chunk definitions (including the
;;;             :   actions from the table) to the creation function instead of
;;;             :   the reset function since that's allowed now.  That means 
;;;             :   extend-manual-requests now must be called before a model is
;;;             :   defined not just reloaded.
;;; 2020.08.26 Dan
;;;             : * Removed the path for require-compiled since it's not needed
;;;             :   and results in warnings in SBCL.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;;
;;; DAN 
;;; Start by making sure the general-pm file has been loaded
;;;

(require-compiled "GENERAL-PM")


;;;; ---------------------------------------------------------------------- ;;;;
;;;; the Motor Module class itself
;;;; ---------------------------------------------------------------------- ;;;;


(defclass motor-module (pm-module)
  ((left-hand :accessor left-hand :initarg :left-hand :initform (make-instance 'hand :name 'left))
   (right-hand :accessor right-hand :initarg :right-hand :initform (make-instance 'hand :name 'right))
   (requests-table-lock :accessor requests-table-lock :initform (bt:make-lock "motor-requests") :allocation :class)
   (key-closure-time :accessor key-closure-time :initform 0.01)
   (mouse-fitts-coeff :accessor mouse-fitts-coeff :initform 0.1)
   (feature-prep-time :accessor feat-prep-time  :initarg :feat-prep-time :initform 0.050)
   (movement-initiation-time :accessor init-time :initarg :init-time :initform 0.050)
   (default-target-width :accessor default-target-width :initarg :default-target-width :initform 1.0)
   (peck-fitts-coeff :accessor peck-fitts-coeff :initarg :peck-fitts-coeff :initform 0.075) 
   (min-fitts-time :accessor min-fitts-time :initarg :min-fitts-time :initform 0.100)
   (incremental-mouse-p :accessor incremental-mouse-p :initarg :incremental-mouse-p :initform nil)
   (cursor-noise :accessor cursor-noise :initarg :cursor-noise :initform nil)
   (new-requests-table :accessor new-requests-table :initform (make-hash-table) :allocation :class))
  (:default-initargs
    :version-string "8.0"
    :name :MOTOR))


;;; A struct to hold hand position information so that current, and next for
;;; feature preparation can be more easily maintained.

(defstruct hand-pos loc fingers other device)

;;; Instead of creating the lists again and again have functions
;;; that return a completely fresh set of values to avoid issues
;;; with constant lists/vectors being manipulated.

(defun left-standard-offsets ()
  (list (list 'index (vector 0 0))
        (list 'middle (vector -1 0))
        (list 'ring (vector -2 0))
        (list 'pinkie (vector -3 0))
        (list 'thumb (vector 1 2))))

(defun right-standard-offsets ()
  (list (list 'index (vector 0 0))
        (list 'middle (vector 1 0))
        (list 'ring (vector 2 0))
        (list 'pinkie (vector 3 0))
        (list 'thumb (vector -1 2))))

;; Some of these could be (or are) also available as user commands, but
;; putting them in the interface keeps things consistent.

(defun motor-interface (params)
  (labels ((string-or-list-of-strings (x)
                                      (cond ((stringp x)
                                             t)
                                            ((listp x)
                                             (every (lambda (z) (string-or-list-of-strings z)) x))
                                            (t nil)))
           (make-it-strings (x)
                            (cond ((stringp x)
                                   x)
                                  ((and x (listp x))
                                   (mapcar 'make-it-strings x))
                                  (t
                                   (princ-to-string x))))
                                   
           (restore-device-string (x)
                                  (if (string-or-list-of-strings x)
                                      x
                                    (make-it-strings x))))
    (let ((motor (get-module :motor)))
      (if motor
          (if (listp params)
              (let ((cmd (first params))
                    (p (rest params)))
                (case cmd 
                  (set-hand-device ; hand device-list
                   (let ((hand (case (first p)
                                 (left (left-hand motor))
                                 (right (right-hand motor)))))
                     (if hand
                         (if (and (second p) (listp (second p)))
                             (let ((device (restore-device-string (second p))))
                               (if (find device (current-devices "motor") :test 'equalp)
                                   (set-hand-device hand device)
                                 (print-warning "Device ~s not installed for motor interface when notifying motor interface for set-hand-device." device)))
                           (print-warning "Invalid device list ~s when notifying motor interface for set-hand-device." (second p)))
                       (print-warning "Invalid hand ~s provided when notifying motor interface for set-hand-device." (first p)))))
                  (set-hand-position ; hand x y 
                   (if (and (numberp (second p)) (numberp (third p)))
                       (case (first p)
                         (left 
                          (let ((hand (left-hand motor)))
                            (bt:with-lock-held ((hand-lock hand))
                              (setf (loc hand) (make-hand-pos :loc (vector (second p) (third p))
                                                              :fingers (left-standard-offsets))))))
                         (right 
                          (let ((hand (right-hand motor)))
                            (bt:with-lock-held ((hand-lock hand))
                              (setf (loc hand) (make-hand-pos :loc (vector (second p) (third p))
                                                              :fingers (right-standard-offsets))))))
                         (t
                          (print-warning "Invalid hand ~s provided when notifying motor interface for set-hand-position." (first p))))
                     (print-warning "Invalid x and/or y position ~s and ~s provided when notifying motor interface for set-hand-position." 
                                    (second p) (third p))))
                     
                  (set-finger-offset ; hand (finger x-off y-off)*
                   (if (every (lambda (x) 
                                (and (numberp (second x)) (numberp (third x))
                                     (find (first x) '(index middle ring pinkie thumb))))
                              (rest p))
                       (let ((hand (case (first p)
                                     (left (left-hand motor))
                                     (right (right-hand motor)))))
                         (if hand
                            (bt:with-lock-held ((hand-lock hand))
                              (let ((new-offsets (mapcar (lambda (x) (list (first x) (vector (second x) (third x)))) (rest p))))
                                (dolist (f (hand-pos-fingers (loc hand)))
                                  (unless (find (first f) new-offsets :key 'first)
                                    (push (list (first f) (vector (px (second f)) (py (second f)))) new-offsets)))
                                (setf (hand-pos-fingers (loc hand)) new-offsets)))
                           (print-warning "Invalid hand ~s provided when notifying motor interface for set-finger-offset." (first p))))
                     (print-warning "Invalid finger offset provided in ~s when notifying motor interface for set-finger-offset." (rest p))))
                  (get-hand-device ; hand
                   (let ((hand (case (first p)
                                 (left (left-hand motor))
                                 (right (right-hand motor)))))
                     (if hand
                         (bt:with-lock-held ((hand-lock hand))
                           (device hand))
                       (print-warning "Invalid hand ~s provided when notifying motor interface for get-hand-device." (first p)))))
                  (get-hand-position ; hand
                   (let ((hand (case (first p)
                                 (left (left-hand motor))
                                 (right (right-hand motor)))))
                     (if hand
                         (bt:with-lock-held ((hand-lock hand))
                           (coerce (hand-pos-loc (loc hand)) 'list))
                       (print-warning "Invalid hand ~s provided when notifying motor interface for get-hand-position." (first p)))))
                  (get-finger-position ; hand finger
                   (let ((hand (case (first p)
                                 (left (left-hand motor))
                                 (right (right-hand motor))))
                         (finger (second p)))
                     (if hand
                         (if (find finger '(index middle ring pinkie thumb))
                             (coerce (finger-loc hand finger) 'list)
                           (print-warning "Invalid finger ~s provided when notifying motor interface for get-finger-position." finger))
                       (print-warning "Invalid hand ~s provided when notifying motor interface for get-hand-position." (first p)))))
                  (t
                   (print-warning "Motor interface received invalid action ~s with parameters ~s" cmd p))))
            (print-warning "Motor interface notification requires a list of parameters but got ~s." params))
        (print-warning "No motor module available for motor interface notification.")))))


(add-act-r-command "motor-interface" 'motor-interface "Internal command for handling motor interface notifications.  Do not call.")

(defun motor-interface-install (device)
  (declare (ignore device))
  t)

(add-act-r-command "motor-install" 'motor-interface-install "Internal command for handling motor interface device installation. Do not call.")

(define-interface "motor" "motor-install" nil nil "motor-interface")

(defmethod initialize-instance :after ((mtr-mod motor-module) &key)
  (set-default-hand-positions mtr-mod))


;;; RESET-PM-MODULE      [Method]
;;; Date        : 97.03.31
;;; Description : Resetting a motor module includes clearing the motor
;;;             : feature memory and resetting the hands to the 'home row'
;;;             : position.

(defmethod reset-pm-module :after ((mtr-mod motor-module))
    
  (set-default-hand-positions mtr-mod))



(defmethod update-prepared ((mtr-mod motor-module))
  (when (find 'hand (feature-slots (last-prep mtr-mod)))
    (set-hand-position mtr-mod (hand (last-prep mtr-mod))
                       (updated-pos (last-prep mtr-mod)) :current nil)))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; the HAND class
;;;; ---------------------------------------------------------------------- ;;;;


(defclass hand ()
  ((name :accessor name :initarg :name :initform nil)
   (hand-lock :accessor hand-lock :initform (bt:make-lock "hand-lock"))
   (location :accessor loc)
   (device :accessor device :initform nil)
   (next-location :accessor next-loc)))

(defmethod set-default-hand-positions ((mtr-mod motor-module))
  
  ;; Put them where the default keyboard home row will be
  ;; just to have some starting point.  Installing a keyboard
  ;; will move them there explicitly as well as set the device.
  (let ((lh (left-hand mtr-mod))
        (rh (right-hand mtr-mod)))
    
    (bt:with-lock-held ((hand-lock lh))
      (setf (device lh) nil)
      (setf (next-loc lh) nil)
      (setf (loc lh) (make-hand-pos :loc #(4 4)
                                    :fingers (left-standard-offsets))))
      
    (bt:with-lock-held ((hand-lock rh))
      (setf (device rh) nil)
      (setf (next-loc rh) nil)
      (setf (loc rh) (make-hand-pos :loc #(7 4)
                                    :fingers (right-standard-offsets))))))


(defgeneric finger-loc (the-hand finger &key current)
  (:documentation  "Return the absolute XY coordinate of <finger> at current or next location"))

(defmethod finger-loc ((the-hand hand) finger &key (current t))
  (bt:with-lock-held ((hand-lock the-hand))
    (let* ((location (if (or current (null (next-loc the-hand))) (loc the-hand) (next-loc the-hand)))
           (fngr-offset (second (assoc finger (hand-pos-fingers location)))))
      (vector (+ (px (hand-pos-loc location)) (px fngr-offset))
              (+ (py (hand-pos-loc location)) (py fngr-offset))))))
  

(defmethod finger-loc ((location hand-pos) finger &key (current t))
  (declare (ignore current))
  (let* ((fngr-offset (second (assoc finger (hand-pos-fingers location)))))
      (vector (+ (px (hand-pos-loc location)) (px fngr-offset))
              (+ (py (hand-pos-loc location)) (py fngr-offset)))))

(defmethod finger-loc ((hand-name symbol) finger &key (current t))
  (let ((h (ecase hand-name
             (right (right-hand (get-module :motor)))
             (left (left-hand (get-module :motor))))))
    (finger-loc h finger :current current)))
  
(defgeneric move-finger (the-hand finger r theta &key current)
  (:documentation  "Computes the new position based on indicated starting point."))

(defmethod move-finger ((the-hand hand) finger r theta &key (current t))
  (bt:with-lock-held ((hand-lock the-hand))
    (let ((hand-pos (if (or current (null (next-loc the-hand))) (loc the-hand) (next-loc the-hand))))
      (unless (= r 0)
        (setf hand-pos (copy-hand-pos hand-pos))
        
        (let ((new-offsets (mapcar (lambda (x) 
                                     (if (eq (first x) finger)
                                         (list (first x) 
                                               (polar-move-xy (second x) (vector r theta)))
                                       x))
                             (hand-pos-fingers hand-pos))))
          (setf (hand-pos-fingers hand-pos) new-offsets)))
      hand-pos)))

(defgeneric hand-loc (the-hand &key current)
  (:documentation  "Return the xy vector of <hand> at current or next location (current if not set)"))

(defmethod hand-loc ((the-hand hand) &key (current t))
  (bt:with-lock-held ((hand-lock the-hand))
    (let ((location (if (or current (null (next-loc the-hand))) (loc the-hand) (next-loc the-hand))))
      (copy-seq (hand-pos-loc location)))))

(defmethod hand-loc ((location hand-pos) &key (current t))
  (declare (ignore current))
  (copy-seq (hand-pos-loc location)))

(defmethod hand-loc ((hand symbol) &key (current t))
  (let ((h (ecase hand
             (right (right-hand (get-module :motor)))
             (left (left-hand (get-module :motor))))))
    (hand-loc h :current current)))

(defgeneric hand-position (the-hand &key current)
  (:documentation  "Return the position structure of the hand at current or next location (current if not set)"))

(defmethod hand-position ((the-hand hand) &key (current t))
  (bt:with-lock-held ((hand-lock the-hand))
    (let ((location (if (or current (null (next-loc the-hand))) (loc the-hand) (next-loc the-hand))))
      location)))

(defmethod hand-position ((hand symbol) &key (current t))
  (let ((h (ecase hand
             (right (right-hand (get-module :motor)))
             (left (left-hand (get-module :motor))))))
    (hand-position h :current current)))


(defgeneric move-hand (the-hand r theta &key current)
  (:documentation  "Computes the new position based on indicated starting point."))

(defmethod move-hand ((the-hand hand) r theta &key (current t))
  (bt:with-lock-held ((hand-lock the-hand))
    (let ((hand-pos (copy-hand-pos (if (or current (null (next-loc the-hand))) (loc the-hand) (next-loc the-hand)))))
      (setf (hand-pos-loc hand-pos) (polar-move-xy (hand-pos-loc hand-pos) (vector r theta)))
      hand-pos)))

(defgeneric set-hand-device (hand device)
  (:documentation  "Set the current device for a hand"))

(defmethod set-hand-device ((the-hand hand) device)
  (bt:with-lock-held ((hand-lock the-hand))
    (internal-set-hand-device the-hand device)))

(defun internal-set-hand-device (hand device)
  ;; only called when lock held
  (awhen (device hand)
         (notify-device-hand-unset it (name hand)))
  (setf (device hand) device)
  (when device
    (notify-device-hand-set device (name hand))))

(defgeneric get-hand-device (hand &key current)
  (:documentation  "Get the device for a hand"))

(defmethod get-hand-device ((the-hand hand) &key (current t))
  (bt:with-lock-held ((hand-lock the-hand))
    (if current
        (device the-hand)
      (or (and (next-loc the-hand) (hand-pos-device (next-loc the-hand)))
          (device the-hand)))))

(defun device-for-hand (hand &key (current t))
   (verify-current-model
    "No current model. Cannot report a hand's device."
    (let ((mod (get-module :motor)))
      (if mod
          (let ((h (case hand
                     (right (right-hand mod))
                     (left (left-hand mod))
                     (t (print-warning "Invalid hand ~s no device available." hand)))))
            (when h
              (get-hand-device h :current current)))
        (print-warning "Cannot report a hand's device because no motor module available.")))))
        
              

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Support methods
;;;; ---------------------------------------------------------------------- ;;;;


;;; MOVE-A-FINGER      [Method]
;;; Date        : 97.02.14
;;; Description : Moves a finger on the specified hand.

(defgeneric move-a-finger (mtr-mod hand finger r theta &key current)
  (:documentation  "Moves a finger, returning the new XY location"))

(defmethod move-a-finger ((mtr-mod motor-module) hand finger r theta &key (current t))
  (ecase hand
    (right (move-finger (right-hand mtr-mod) finger r theta :current current))
    (left (move-finger (left-hand mtr-mod) finger r theta :current current))))


;;; MOVE-A-HAND      [Method]
;;; Date        : 97.02.25
;;; Description : Moves a hand to the specified location

(defgeneric move-a-hand (mtr-mod hand r theta &key current)
  (:documentation  "Moves a hand, returning the new XY location"))

(defmethod move-a-hand ((mtr-mod motor-module) hand r theta &key (current t))
  (ecase hand
    (right (move-hand (right-hand mtr-mod) r theta :current current))
    (left (move-hand (left-hand mtr-mod) r theta :current current))))


(defgeneric set-hand-position (mtr-mod hand pos &key current)
  (:documentation  "Moves a hand given a hand-pos struct"))

(defmethod set-hand-position ((mtr-mod motor-module) hand pos &key (current t))
  (let ((h (ecase hand
             (right (right-hand mtr-mod))
             (left (left-hand mtr-mod)))))
    (bt:with-lock-held ((hand-lock h))
      (if current
          (setf (loc h) pos)
        (setf (next-loc h) pos))))
  nil)


;;; FINGER-LOC-M      [Method]
;;; Date        : 97.02.17
;;; Description : Module-level call to determine the location of a finger.

(defgeneric finger-loc-m (mtr-mod hand finger &key current)
  (:documentation  "Return the XY location of the specified finger"))

(defmethod finger-loc-m ((mtr-mod motor-module) hand finger &key (current t))
  (ecase hand
    (right (finger-loc (right-hand mtr-mod) finger :current current))
    (left (finger-loc (left-hand mtr-mod) finger :current current))))




;;;; ---------------------------------------------------------------------- ;;;;
;;;; How the device gets the actions, but if no device available then just
;;;; call a dummy event for the trace.

(defun motor-action-performed ())

(defun notify-motor-device (device params &optional time)
  (if device
      (notify-device device params) ;; assume it will generate events for the trace
    (if time
        (schedule-event-now 'motor-action-performed :module :motor :output 'medium
                            :details (concatenate 'string (symbol-name 'motor-action-performed)
                                       " " (princ-to-string time)))
      (schedule-event-now 'motor-action-performed :module :motor :output 'medium))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Utility functions


(defun distance= (d1 d2)
  "Two distances are not equal if they are more than two degrees apart."
  (when (and d1 d2)
    (> 2.0 (abs (- d1 d2)))))


(defun direction= (d1 d2)
  "Two directions are not equal if they are more than pi/4 apart."
  (when (and d1 d2)
    (let ((delta (abs (- d1 d2))))
      (or (> (/ pi 4) delta)       
          ;; need to check when > 180 for close the other way
          (> delta (* 7/4 pi))))))


(defun xy-to-polar (from to)
  (let ((distance (dist (subseq from 0 2) (subseq to 0 2))))
    (vector distance
            (if (zerop distance)
              0.0
              (atan (- (py to) (py from)) 
                    (- (px to) (px from)))))))



;;; FITTS      [Function]
;;; Date        : 97.02.14, delta 99.06.18
;;; Description : Return Fitts law time for a given coefficient and distance,
;;;             : optionally supplying the target width (default is 1)

(defgeneric fitts (mtr-mod coef d &optional w)
  (:documentation  "Fitts law time for movement"))

(defmethod fitts ((mtr-mod motor-module) coef d &optional (w 1.0))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (when (or (zerop w) (minusp w))
      (let ((default (default-target-width mtr-mod)))
        (print-warning "Fitts time computation received a negative or zero width.  Using the default-target-width setting ~s." default)
        (setf w default)))
    (max (min-fitts-time mtr-mod)         ; 99.06.18
         (* coef (log-coerced (+ (/ d w) 0.5) 2)))))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; PUNCH class and methods

(defStyle punch () hand finger)


(defmethod compute-exec-time ((mtr-mod motor-module) (self punch))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod)
       (key-closure-time mtr-mod))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self punch))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod) 
       (burst-time mtr-mod)
       (burst-time mtr-mod))))

(defmethod feat-differences ((p1 punch) (p2 punch))
  (cond ((not (eq (hand p1) (hand p2))) 2)
        ((not (eq (finger p1) (finger p2))) 1)
        (t 0)))

(defmethod queue-output-events ((mtr-mod motor-module) (self punch))
  (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device :time-in-ms t :module :motor :output nil
                           :params `(,(device-for-hand (hand self)) 
                                       ((model ,(current-model)) (style punch) (hand ,(hand self))
                                        ; trust the position at start of execution
                                        (loc ,(coerce (finger-loc-m mtr-mod (hand self) (finger self)) 'list)))
                                        ,(request-time self))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; HAND-FINGER-R-THETA class and methods


(defStyle hfrt-movement () hand finger r theta)

(defmethod feat-differences ((m1 hfrt-movement) (m2 hfrt-movement))
  (let ((maxfeats (num-possible-feats m1)))
    (cond ((not (eq (hand m1) (hand m2))) (1- maxfeats))
          ((not (eq (finger m1) (finger m2))) (- maxfeats 2))
          (t
           (let ((nfeats 0))
             (unless (distance= (r m1) (r m2)) (incf nfeats))
             (unless (direction= (theta m1) (theta m2)) (incf nfeats))
             nfeats)))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; PECK class and methods

(defStyle peck hfrt-movement hand finger r theta) 

;; finger will be moved to the target location

(defmethod prepare-features ((mtr-mod motor-module) (self peck))
  (setf (updated-pos self)
    (move-a-finger mtr-mod (hand self) (finger self) (r self) (theta self) :current nil)))


(defmethod compute-exec-time ((mtr-mod motor-module) (self peck))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod)
       (max (burst-time mtr-mod)
            (randomize-time 
             (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self)))))))   ; 99.06.18


(defmethod queue-output-events ((mtr-mod motor-module) (self peck))
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor :output nil
                           :destination :motor :params (list (hand self) (updated-pos self)))
  (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device :time-in-ms t :module :motor :output nil
                           :params `(,(device-for-hand (hand self)) ((model ,(current-model)) (style peck) (hand ,(hand self)) 
                                      (loc ,(coerce (finger-loc (updated-pos self) (finger self)) 'list)))
                                      ,(request-time self))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; PECK-RECOIL class and methods

(defclass peck-recoil (hfrt-movement)
  ((move-time :accessor move-time))
  (:default-initargs
    :style-name :PECK))


(defmethod compute-exec-time ((mtr-mod motor-module) (self peck-recoil))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (setf (move-time self)
      (max (burst-time mtr-mod)
           (fitts mtr-mod (peck-fitts-coeff mtr-mod) (r self))))   ; 99.06.18
    (+ (init-time mtr-mod)
       (max (burst-time mtr-mod) 
            (randomize-time (move-time self))))))

(defmethod compute-finish-time ((mtr-mod motor-module) (self peck-recoil))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (exec-time self) 
       (burst-time mtr-mod)
       (max (burst-time mtr-mod)
            (randomize-time (move-time self))))))

(defmethod queue-output-events ((mtr-mod motor-module) (self peck-recoil))
  (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device :time-in-ms t :module :motor :output nil
                           :params `(,(device-for-hand (hand self)) 
                                       ((model ,(current-model)) (style peck-recoil) (hand ,(hand self)) 
                                        ;; trust current position at start of execution
                                        ;; and don't actually change the finger position                          
                                        (loc ,(coerce (polar-move-xy (finger-loc-m mtr-mod (hand self) (finger self))
                                                                     (vector (r self) (theta self)))
                                                      'list)))
                                       ,(request-time self))))

(defmethod peck-recoil ((mtr-mod motor-module) &key hand finger r theta request-spec)
  (unless (check-jam mtr-mod)
    (when (symbolp theta)
      (setf theta (symbol-value theta)))
    (prepare-movement mtr-mod (make-instance 'peck-recoil :hand hand :finger finger :r r :theta theta :request-spec request-spec))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; PLY classes and methods

(defclass ply (hfrt-movement)
  ((fitts-coeff :accessor fitts-coeff :initarg :fitts-coeff)
   (target-width :accessor target-width :initarg :target-width
                 :initform (let ((mtr-mod (get-module :motor)))
                             (when mtr-mod
                               (bt:with-recursive-lock-held ((param-lock mtr-mod))
                                 (default-target-width mtr-mod))))))
  (:default-initargs
    :finger :dummy
    :style-name :PLY
    :fitts-coeff (let ((mtr-mod (get-module :motor)))
                   (when mtr-mod
                     (bt:with-recursive-lock-held ((param-lock mtr-mod))
                       (peck-fitts-coeff mtr-mod))))))


;; finger will be moved to the target location

(defmethod prepare-features ((mtr-mod motor-module) (self ply))
  (setf (updated-pos self)
    (move-a-finger mtr-mod (hand self) (finger self) (r self) (theta self) :current nil)))


(defmethod queue-output-events ((mtr-mod motor-module) (self ply))
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                           :destination :motor :params (list (hand self) (updated-pos self))
                           ;; This is the action that used to be shown
                           :details (format nil "~a ~a ~a ~a ~a" 'move-a-finger (hand self) (finger self) (r self) (theta self))))

(defmethod compute-exec-time ((mtr-mod motor-module) (self ply))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod)
       (randomize-time (fitts mtr-mod (fitts-coeff self) (r self)
                              (target-width self))))))


(defmethod num-possible-feats :around ((self ply))
  (- (call-next-method) 1))



(defclass hand-ply (ply)
  ((offsets :accessor offsets :initarg :offsets :initform nil)
   (device :accessor device :initarg :device :initform nil))
  (:default-initargs
      :fitts-coeff (let ((mtr-mod (get-module :motor)))
                   (when mtr-mod
                     (bt:with-recursive-lock-held ((param-lock mtr-mod))
                       (mouse-fitts-coeff mtr-mod))))))


(defmethod prepare-features ((mtr-mod motor-module) (self hand-ply))
  (let ((new-pos (move-a-hand mtr-mod (hand self) (r self) (theta self) :current nil)))
    (when (offsets self)
      (if (eq (offsets self) 'standard)
          (setf (hand-pos-fingers new-pos) (ecase (hand self)
                                             (right 
                                              (right-standard-offsets))
                                             (left
                                              (left-standard-offsets))))
        (let ((new-offsets (mapcar (lambda (x)
                                     (list (first x) (vector (second x) (third x))))
                             (offsets self))))
          
          (dolist (f (hand-pos-fingers new-pos))
            (unless (find (first f) new-offsets :key 'first)
              (push (list (first f) (vector (px (second f)) (py (second f)))) new-offsets)))
          (setf (hand-pos-fingers new-pos) new-offsets))))
    (setf (hand-pos-device new-pos) (device self))
    (setf (updated-pos self) new-pos)))


(defmethod queue-output-events ((mtr-mod motor-module) (self hand-ply))
  (let ((hand (ecase (hand self)
                (right (right-hand mtr-mod))
                (left (left-hand mtr-mod)))))
    
    ;; update the device for the hand in an event since
    ;; it's location is important now that actions go to the
    ;; current device and if it changes they need to go to
    ;; the 'current' one until the hand is at the new one
    
    (when (device self)
      (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-device :time-in-ms t
                               :module :motor :params (list hand (device self)) :priority 1 :output nil)
      )
    
    (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                             :destination :motor :params (list (hand self) (updated-pos self))
                             ;; This is the action that used to be shown
                             :details (format nil "~a ~a ~a ~a" 'move-a-hand (hand self) (r self) (theta self)))))



(defmethod point-hand ((mtr-mod motor-module) &key hand r theta twidth offsets request-spec device style)
  (unless (or (check-jam mtr-mod) (check-specs 'point-hand hand r theta))
    
    (unless (or (null style)
                (subtypep style 'hand-ply))
      (model-warning "Invalid custom style ~s in a point-hand action." style)
      
      (return-from point-hand))
    
    (unless (or (null offsets)
                (eq offsets 'standard)
                (every (lambda (x) (and (listp x) (= (length x) 3) (find (first x) '(thumb index middle ring pinkie))
                                        (numberp (second x)) (numberp (third x))))
                       offsets))
      (model-warning "Invalid offsets ~s in a point-hand action." offsets)
      
      (return-from point-hand))
    
    (prepare-movement mtr-mod
                      (make-instance (if style style 'hand-ply) :hand hand :r r :theta theta
                        :target-width (aif twidth it (bt:with-recursive-lock-held ((param-lock mtr-mod))
                                                       (default-target-width mtr-mod)))
                        :offsets offsets
                        :device device
                        :request-spec request-spec))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; CURSOR-PLY classes and methods
;;;
;;; A style for cursor movement to be used by devices that move cursors.
;;; Assumes that the cursor device supports the appropriate signals:
;;; 


(defclass cursor-ply (ply)
  ((target-coords :accessor target-coords :initarg :target-coords)
   (control-order :accessor control-order :initarg :control-order :initform 0)
   (cursor :accessor cursor :initarg :cursor :initform nil))
  (:default-initargs
    :hand 'RIGHT
    :fitts-coeff (let ((mtr-mod (get-module :motor)))
                   (when mtr-mod
                     (bt:with-recursive-lock-held ((param-lock mtr-mod))
                       (mouse-fitts-coeff mtr-mod))))))

(defmethod compute-exec-time :before ((mtr-mod motor-module) (self cursor-ply))
  (setf (fitts-coeff self) 
    (* (expt-coerced 2 (control-order self)) (fitts-coeff self))))


;;; Technically the hand doesn't move for a cursor-ply because the cursor
;;; is in a fixed location.  So there isn't a new position unlike a ply, but
;;; we need to store the target-coords in the other slot of the position
;;; because back-to-back cursor-ply actions need the correct starting point
;;; for feature generation.

(defmethod prepare-features ((mtr-mod motor-module) (self cursor-ply))
  (let* ((starting-point (copy-hand-pos (hand-position (hand self) :current nil)))
         (others (copy-seq (hand-pos-other starting-point))))
    (awhen (find 'target-coords (hand-pos-other starting-point) :key 'first)
         (setf others (remove it others)))
    (push (list 'target-coords (target-coords self)) others)
    (setf (hand-pos-other starting-point) others)
    (setf (updated-pos self) starting-point)))

;;; QUEUE-OUTPUT-EVENTS      [Method]
;;; Date        : 99.04.02
;;; Description : New output queueing for cursor moves.  The move is broken
;;;             : up into pieces to provide the illusion of smooth movement.
;;;             : Queue up the pieces.  The last submovement doesn't move
;;;             : with a polar move, though, because small rounding errors
;;;             : accumulate and the target will be missed by a couple
;;;             : pixels.  

(defmethod queue-output-events ((mtr-mod motor-module) (self cursor-ply))
  ;; if making incremental moves, do all the necessary computations there
  (when (incremental-mouse-p mtr-mod)
    (let* ((d (seconds->ms (exec-time self)))
           (steptime (bt:with-recursive-lock-held ((param-lock mtr-mod))
                       (if (numberp (incremental-mouse-p mtr-mod))
                           (incremental-mouse-p mtr-mod)
                         50)))
           (nsteps (max 1 (round d steptime)))
           (startpos (notify-device (cursor self) (list 'current-position)))
           (curdist 0) 
           (px-move nil))
      (when (> nsteps 1)
        (dotimes (idx (- nsteps 1))
          (setf curdist (minjerk-dist (* (1+ idx) steptime) (r self) d))
          (setf px-move (polar-move-xy startpos 
                                       (vector (pm-angle-to-pixels curdist)
                                               (theta self))))
          (schedule-event-relative (* (1+ idx) steptime) 'notify-motor-device :time-in-ms t :output nil :module :motor 
                                   :params `(,(cursor self) ((model ,(current-model)) (style move-cursor) (hand ,(hand self)) 
                                                                                      (loc ,(coerce px-move 'list)))
                                               ,(request-time self)))))))
        ;; make the final movement
  (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device :time-in-ms t :output nil :module :motor 
                           :params `(,(cursor self) ((model ,(current-model)) (style move-cursor) (hand ,(hand self)) 
                                                                              (loc ,(coerce (target-coords self) 'list)))
                                       ,(request-time self)))
  
  ;; make sure to set the updated position as well so the target-coords are stored for computing future features
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                             :destination :motor :params (list (hand self) (updated-pos self)) :output nil))

(defun minjerk-dist (tm a d)
  "Returns distance moved at time tm, given max distance a and total move time d."
  (cond ((>= tm d) a)
        ((< tm 0) (error "Negative time passed to MINJERK-DIST"))
        ((= tm 0) 0)
        ((<= d 0) 0)
        (t
         (let ((td (/ tm d)))
           (* a
              (+ (* 10 (expt td 3)) (* -15 (expt td 4)) (* 6 (expt td 5))))))))


;;; MOVE-CURSOR      [Method]
;;; Date        : 99.04.02
;;; Description : To get the movement time right, the effective width of the
;;;             : target has to be computed.  Gotta have the target object
;;;             : for that, which should be available through the supplied
;;;             : location or object.

(defclass move-cursor (movement-style)
  nil
  (:default-initargs
      :style-name :MOVE-CURSOR
    :feature-slots '(loc object hand)))

(defmethod move-cursor ((mtr-mod motor-module) &key loc object (hand 'right) request-spec)
  (let* ((real-hand (case (string->name hand) 
                      (right (right-hand mtr-mod)) 
                      (left (left-hand mtr-mod)) 
                      (t (print-warning "Hand value for move-cursor action must be left or right but given ~s." hand))))
         (real-device (get-hand-device real-hand :current nil)))
    (when (or (null real-device) (not (string-equal (second real-device) "cursor")))
      (print-warning "Hand is not on a cursor device in request to move-cursor.")
      
      (return-from move-cursor nil))
    
    (unless (or (check-jam mtr-mod) (check-specs 'move-cursor (or loc object) hand))
      (let ((target (or (and (chunk-p-fct object) object) (and (chunk-p-fct loc) loc)))
            (target-loc (or (and object (chunk-to-visual-position object))
                            (and loc (chunk-to-visual-position loc)))))
        
        (unless (and target target-loc)
          (print-warning "No valid location could be generated from ~s or ~s when trying to move a cursor." object loc)
          
          (return-from move-cursor nil))
        
        ;; Use the location stored with the hand if available because that
        ;; reflects pending actions whereas the device's position does not
        ;; which could lead to problems when using preparation free.
        
        (let* ((real-starting-point (hand-position hand :current nil))
               (known-loc (when real-starting-point (awhen (assoc 'target-coords (hand-pos-other real-starting-point)) (coerce (second it) 'list))))
               (cursor-loc (if known-loc known-loc (notify-device real-device (list 'current-position))))
               (r-theta (xy-to-polar cursor-loc target-loc)))
          
          (unless (= (third cursor-loc) (third target-loc))
            (model-warning "Move-cursor request has different z coordinates for start ~s and target ~s locations, but motor module assumes a cursor moves in only two dimensions." cursor-loc target-loc))
          
          (if (and (= 0 (vr r-theta)) (= (pz cursor-loc) (pz target-loc)))        ; r=0 is a no-op 
              (model-warning "Move-cursor action aborted because cursor is at requested target ~S" target)
              (let* ((w (approach-width target (vtheta r-theta) (px cursor-loc) (py cursor-loc)))
                     (noisy-target-cords (noisy-loc? mtr-mod (coerce target-loc 'vector) w))
                     (r-theta-new (xy-to-polar cursor-loc noisy-target-cords)))
                
                (prepare-movement mtr-mod
                                  (make-instance 'cursor-ply
                                    :request-spec request-spec
                                    :r (pm-pixels-to-angle (vr r-theta-new))
                                    :theta (vtheta r-theta-new)
                                    :target-width w
                                    :hand hand
                                    :cursor real-device
                                    :target-coords noisy-target-cords
                                    :control-order (notify-device real-device (list 'control-order)))))))))))


(defgeneric noisy-loc? (mtr-mod xy-loc w)
  (:documentation "If the Motor Module is set up for it, make the output location noisy."))


;;; NOISY-LOC?      [Method]
;;; Description : Adds noise to the output location if noise is on.  Uses 
;;;             : logistic rather than normal [like other ACT-R].  7.8 is
;;;             : the width of the 96% interval of the unit logistic [after
;;;             : MacKenzie].

(defmethod noisy-loc? ((mm motor-module) (xy-loc vector) (w number))
  (if (not (bt:with-recursive-lock-held ((param-lock mm)) (cursor-noise mm)))
    xy-loc
    (let ((pixw (pm-angle-to-pixels w)))
      (if (zerop pixw)
          xy-loc
        (polar-move-xy xy-loc (vector (act-r-noise (/ pixw 7.8)) 
                                      (act-r-random pi)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dan 
;;; everything below here is additional stuff for the ACT-R 6 interface


(defun query-motor-module (motor buffer slot value)
  (if (and (eq slot 'state) (eq value 'error))
    nil
    (generic-state-query motor buffer slot value)))

;;; Commands for extending and removing motor actions.
;;; This is how the internal actions are specified now as well
;;; instead of with one big ugly request method.

(defmacro extend-manual-requests (chunk-type function-name)
  `(extend-manual-requests-fct ',chunk-type ',function-name))

(defun extend-manual-requests-fct (chunk-type function-name)
  (cond ((not (listp chunk-type))
         (print-warning "Invalid chunk-type specification ~s.  Manual requests not extended." chunk-type))
        ((not (fboundp function-name))
         (print-warning "~s does not name a function.  Manual requests not extended." function-name))
        (t
         (let ((ct-name (if (listp (first chunk-type)) (car (first chunk-type)) (first chunk-type)))
               (dummy-module (make-instance 'motor-module)))
           (bt:with-lock-held ((requests-table-lock dummy-module))
             (if (gethash ct-name (new-requests-table dummy-module))
                 (print-warning "Request ~s is already an extension of the manual buffer.  To redefine you must remove it first with remove-manual-request." ct-name)
               (let ((type-list
                      (if (listp (first chunk-type))
                          (append chunk-type (list (list 'cmd ct-name)))
                        (append (list (list ct-name '(:include motor-command))) (rest chunk-type) (list (list 'cmd ct-name))))))                
                 
                 (setf (gethash ct-name (new-requests-table dummy-module))
                   (cons type-list function-name))
                 t)))))))

(defmacro remove-manual-request (chunk-type)
  `(remove-manual-request-fct ',chunk-type))

(defun remove-manual-request-fct (chunk-type)
  (let ((dummy-module (make-instance 'motor-module)))
    (bt:with-lock-held ((requests-table-lock dummy-module))
      (if (gethash chunk-type (new-requests-table dummy-module))
          (remhash chunk-type (new-requests-table dummy-module))
        (print-warning "~s is not a previously extended request for the manual module." chunk-type)))))
           

(defmethod pm-module-request ((motor motor-module) buffer-name chunk-spec)
  (declare (ignore buffer-name))
  
  (cond ((test-for-clear-request chunk-spec)
         (schedule-event-now 'clear :module :motor :destination :motor :output 'medium))
        
        ((= (length (chunk-spec-slot-spec chunk-spec 'cmd)) 1)
         (let ((cmd (spec-slot-value (first (chunk-spec-slot-spec chunk-spec 'cmd)))))
           (if (eq cmd 'prepare)
               (let* ((style (verify-single-explicit-value chunk-spec 'style :motor 'prepare))
                      (params-list (mapcan (lambda (x)
                                             (when (slot-in-chunk-spec-p chunk-spec x) 
                                               (aif (verify-single-explicit-value chunk-spec x :motor style)
                                                    (list (sym->key x) it)
                                                    (progn
                                                      (print-warning "Invalid prepare command to motor module with invalid specification for ~s slot." x)
                                                      (return-from pm-module-request nil)))))
                                     '(hand finger r theta))))
                 (when style
                   (bt:with-lock-held ((prep-spec-lock motor)) (setf (prepare-spec motor) chunk-spec))
                   (schedule-event-now 'prepare :destination :motor :params (push style params-list) :module :motor :output 'low)))
             
             ;; otherwise look it up in the extended table
             (aif (bt:with-lock-held ((requests-table-lock motor)) (gethash cmd (new-requests-table motor)))
                  (funcall (cdr it) motor chunk-spec)
                  (print-warning "Invalid command ~a sent to the manual buffer" cmd)))))
        
        ((chunk-spec-slot-spec chunk-spec 'cmd)
         
         (print-warning "Invalid command to motor module specifies the cmd slot multiple times."))
        (t
         
         (print-warning "Invalid command to motor module does not specify the cmd slot."))))
    

(defmethod handle-simple-command-request ((mtr-mod motor-module) chunk-spec &optional (output 'low))
  (let ((command (spec-slot-value (first (chunk-spec-slot-spec chunk-spec 'cmd)))))
    (when (and command (symbolp command) (fboundp command))
      (schedule-event-now command :destination :motor :module :motor :output output :params (list chunk-spec)
                          :details (format nil "~a" command)))))

(defun medium-output-command-request (m s)
  (handle-simple-command-request m s 'medium))

(defmethod handle-style-request ((mtr-mod motor-module) chunk-spec)
  (let* ((style (spec-slot-value (first (chunk-spec-slot-spec chunk-spec 'cmd))))
         (dummy (make-instance style))
         (params-list (mapcan (lambda (x)
                                (aif (verify-single-explicit-value chunk-spec x :motor style)
                                     (list (sym->key x) it)
                                     (progn 
                                       (model-warning "~s request aborted because feature ~s is missing or specified multiple times" style x)
                                       (return-from handle-style-request nil))))
                        (feature-slots dummy))))
    (schedule-event-now style :destination :motor :params (append (list :request-spec chunk-spec) params-list)
                        :module :motor :output 'low
                        :details (format nil "~a ~{~a~^ ~}" style params-list))))

(defmethod handle-partial-style-request ((mtr-mod motor-module) chunk-spec)
  (let* ((style (spec-slot-value (first (chunk-spec-slot-spec chunk-spec 'cmd))))
         (dummy (make-instance style))
         (params-list (mapcan (lambda (x)
                                (when (slot-in-chunk-spec-p chunk-spec x) 
                                  (aif (verify-single-explicit-value chunk-spec x :motor style)
                                       (list (sym->key x) it)
                                       (progn 
                                         (model-warning "~s request aborted because feature ~s is missing or specified multiple times" style x)
                                         (return-from handle-partial-style-request nil)))))
                        (feature-slots dummy))))
    (schedule-event-now style :destination :motor :params (append (list :request-spec chunk-spec)
                                                                  params-list)
                        :module :motor :output 'low
                        :details (format nil "~a ~{~a~^ ~}" style params-list))))


(extend-manual-requests (execute) medium-output-command-request)

(extend-manual-requests (punch hand finger) handle-style-request)
(extend-manual-requests (peck hand finger r theta) handle-style-request)
(extend-manual-requests (peck-recoil hand finger r theta) handle-style-request)


(extend-manual-requests (move-cursor object loc hand) handle-partial-style-request)


(defun reset-motor-module (instance)
  
  ;; Moved so that extensions which define new motor
  ;; commands can specialize the reset method to add the 
  ;; required chunk-types and include motor-command.
  
  (reset-pm-module instance)
  
  )

(defun params-motor-module (motor param)
  (bt:with-recursive-lock-held ((param-lock motor))
    (if (consp param)
        (case (car param)
          (:cursor-noise
           (setf (cursor-noise motor) (cdr param)))
          (:default-target-width
              (setf (default-target-width motor) (cdr param))) 
          (:incremental-mouse-moves
           (setf (incremental-mouse-p motor) (if (numberp (cdr param))
                                                 (safe-seconds->ms (cdr param))
                                               (cdr param)))
           (cdr param))
          (:min-fitts-time
           (setf (min-fitts-time motor) (cdr param)))
          (:motor-burst-time
           (setf (burst-time motor) (cdr param)))
          (:motor-initiation-time
           (setf (init-time motor) (cdr param)))
          (:motor-feature-prep-time
           (setf (feat-prep-time motor) (cdr param)))
          
          (:peck-fitts-coeff
           (setf (peck-fitts-coeff motor) (cdr param)))
          
          (:cursor-fitts-coeff
           (setf (mouse-fitts-coeff motor) (cdr param)))
          
          (:key-closure-time
           (setf (key-closure-time motor) (cdr param))))
      (case param
        (:cursor-noise
         (cursor-noise motor))
        (:default-target-width
            (default-target-width motor))  
        (:incremental-mouse-moves
         (if (numberp (incremental-mouse-p motor))
             (ms->seconds (incremental-mouse-p motor))
           (incremental-mouse-p motor)))
        (:min-fitts-time
         (min-fitts-time motor))
        (:motor-burst-time
         (burst-time motor))
        (:motor-initiation-time
         (init-time motor))
        (:motor-feature-prep-time
         (feat-prep-time motor))
        
        (:peck-fitts-coeff
         (peck-fitts-coeff motor))))))
  
(defun create-motor-module (model-name)
  (declare (ignore model-name)) 
  
  (chunk-type motor-command (cmd "motor action"))
  (chunk-type (prepare (:include motor-command)) (cmd prepare) style hand finger r theta)
  
  (unless (chunk-p prepare)
    (define-chunks (prepare isa prepare)))
  
  
  (dolist (c '(left right index middle ring pinkie thumb))
    (unless (chunk-p-fct c)
      (define-chunks-fct (list (list c 'name c)))))
  
  
  (let ((instance (make-instance 'motor-module)))
    
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

(defun manual-buffer-status ()
  (print-module-status (get-module :motor)))

(define-module-fct :motor 
    (list (define-buffer-fct 'manual 
              :queries '(modality preparation execution processor last-command)
            :status-fn 'manual-buffer-status))
  (list 
   (define-parameter :cursor-noise
     :valid-test 'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Is there noise in the final cursor location.")
   (define-parameter :default-target-width
     :owner nil)
   (define-parameter :incremental-mouse-moves
     :valid-test 'posnumorbool 
     :default-value nil
     :warning "T, NIL, or a non-negative number"
     :documentation "Output mouse moves in stages?")
   (define-parameter :min-fitts-time
     :valid-test 'nonneg 
     :default-value 0.1
     :warning "a non-negative number"
     :documentation "Minimum movement time for an aimed [Fitts's] movement.")
   (define-parameter :motor-burst-time
     :valid-test 'nonneg 
     :default-value 0.05
     :warning "a non-negative number"
     :documentation "Minimum time for any movement.")
   (define-parameter :motor-initiation-time
     :valid-test 'nonneg 
     :default-value .05
     :warning "a non-negative number"
     :documentation "Time to initiate a motor movement.")
   (define-parameter :motor-feature-prep-time
     :valid-test 'nonneg 
     :default-value 0.05
     :warning "a non-negative number"
     :documentation "Time to prepare a movement feature.")
   (define-parameter :peck-fitts-coeff
     :valid-test 'nonneg 
     :default-value 0.075
     :warning "a non-negative number"
     :documentation "b coefficient in Fitts's equation for PECK movements.")
   
   
   (define-parameter :cursor-fitts-coeff :owner nil)
   (define-parameter :key-closure-time :owner nil))
  
  :version (version-string (make-instance 'motor-module))
  :documentation "Module to provide a model with virtual hands"
  :creation 'create-motor-module
  :reset 'reset-motor-module
  :query 'query-motor-module
  :request 'pm-module-request
  :params 'params-motor-module)

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Other toplevel commands


(defmacro prepare-motor (&rest lis)
  "Tells the Motor Module to prepare the supplied movement."
  `(pm-prepare-mvmt-mth (get-module :motor) ',lis))


(defmacro set-hand-location (hand x y &optional (device nil devicep))
  "Sets the location of the given hand to LOC"
  `(if ,devicep
       (set-hand-location-fct ',hand ',(list x y) ',device)
     (set-hand-location-fct ',hand ',(list x y))))

(defun set-hand-location-fct (hand loc &optional (device nil devicep))
  "Function to set the location of the given hand to LOC"
  (verify-current-model 
   "No current model.  Cannot set hand location."
   (aif (get-module :motor)
        (cond ((or (not (listp loc)) (not (= (length loc) 2))
                   (not (every 'numberp loc)))
               (print-warning "Invalid location ~s provided to set-hand-location-fct." loc))
              ((or (null devicep) (null device) (find device (current-devices "motor") :test 'equalp))
               (setf loc (coerce loc 'vector))
               
               (case hand
                 (left 
                  (let ((h (left-hand it)))
                            (bt:with-lock-held ((hand-lock h))
                              (setf (loc h) (make-hand-pos :loc loc
                                                              :fingers (left-standard-offsets)))
                              (when devicep (internal-set-hand-device h device)))
                    t))
                 (right 
                  (let ((h (right-hand it)))
                    (bt:with-lock-held ((hand-lock h))
                      (setf (loc h) (make-hand-pos :loc loc
                                                   :fingers (right-standard-offsets)))
                      (when devicep (internal-set-hand-device h device)))
                    t))
                 
                 (t
                  (print-warning "Invalid hand ~s specified in set-hand-location-fct." hand))))
              (t
               (print-warning "Invalid device ~s given to set-hand-location-fct." device)))
        (print-warning "No motor module found.  Cannot set hand location."))))

(defun external-set-hand-location (hand x y &optional (device nil devicep))
  (if devicep
      (set-hand-location-fct (string->name hand) (list x y) device)
    (set-hand-location-fct (string->name hand) (list x y))))
 
(defun external-set-hand-location-fct (hand loc &optional (device nil devicep))
  (if devicep
      (set-hand-location-fct (string->name hand) loc device)
    (set-hand-location-fct (string->name hand) loc)))

(add-act-r-command "set-hand-location" 'external-set-hand-location "Set the position of the indicated hand. Params: hand x-pos y-pos {device}")
(add-act-r-command "set-hand-location-fct" 'external-set-hand-location-fct "Set the position of the indicated hand. Params: hand (x-pos y-pos) {device}")


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
