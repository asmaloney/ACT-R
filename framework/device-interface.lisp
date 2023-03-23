;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Authors     : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)1998-2005 Mike Byrne & Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LICENSE.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : device-interface.lisp
;;; Version     : 7.3
;;; 
;;; Description : File for managing the device interface.
;;; 
;;; Bugs        : None known.
;;; 
;;; Todo        : Nothing pending.
;;; 
;;; ----- History -----
;;; 2004.10.19 Dan [Moved into ACT-R 6]
;;;             : Lots of changes - first being a reset of version to 1.0a1
;;;
;;;             : Added the package switches for the act-r package and
;;;             : the "clean" packaging
;;;
;;;             : Removed the following:
;;;                      pm-get-time, pm-timed-event, pm-delayed-event
;;;                         new functions replace these: mp-time and all of the
;;;                         new scheduling functions
;;;                         may want to put back as depricated to ease transition
;;;
;;;                      reset-module 
;;;                         because a model reset calls reset-device directly
;;;                      
;;;                      run-events, new-message 
;;;                         everything goes through the central scheduler
;;;                         instead of separate event queues
;;;
;;;                      pm-install-module, update-module, silent-events
;;;                         master-process functionality replaced
;;;
;;;                      process-display and update-cursor-feat
;;;                         moved specific methods to the vision module since 
;;;                         that's really more the critical class and the
;;;                         device preceeds the specific modules now
;;;
;;;             : Added:
;;;                      current-device-interface, install-device, 
;;;                      current-device, proc-display
;;;
;;;             : Moved these from elsewhere to here
;;;                      pm-angle-to-pixels, pm-pixels-to-angle  
;;; 2005.01.11 mdb
;;;             : Added a couple toplevel functions for backward compatibility.        
;;;
;;; 2005.01.12 Dan
;;;             : Made the device an actual module so that it can have its
;;;             : own parameters.
;;;             :
;;;             : Moved the newly added backward compatibility functions to the
;;;             : backward file in support.
;;; 2005.01.13 Dan
;;;             : * Added the the two new methods lock-device and unlock-device
;;;             :   along with the appropriate slots and changes to reset-device
;;;             :   to allow other modules to block and unblock proc-display 
;;;             :   from actually going.  [Right now, that's a problem with
;;;             :   productions potentially jamming vision if a proc-display
;;;             :   happens between the time of the query for free and the
;;;             :   time that the +visual command is actually sent.]
;;; 2005.02.17 Dan
;;;             : * Fixed the references in the *pixels-per-inch-* settings
;;;             :   so that ACL gets the packaging correct.
;;; 2005.05.11 Dan
;;;             : * Added the :vwt paramter and corresponding virtual-trace
;;;             :   slot to the device interface to control the output of the
;;;             :   << ... >> printing from the vw-output command used by the
;;;             :   virtual window device.
;;; 2005.08.10 Dan
;;;             : * Removed a duplicate definition of prod-display.
;;; 2005.11.02 Dan
;;;             : * Updated unlock-device so that tracking updates can be
;;;             :   recognized and handled as well as full proc-displays.
;;; 2006.03.07 Dan
;;;             : * Fixed an old bug which I don't know how it got back in...
;;;             :   the theta for hyphen in the key mapping should be -1.11.
;;; 2006.03.08 Dan
;;;             : * Made the key-to-loc and key-to-command tables consistent
;;;             :   with respect to - and hyphen.  Both tables now have entries
;;;             :   for both symbols mapped to the same things.
;;; 2006.09.11 Dan
;;;             : * Changed valid test for :mouse-fitts-coeff from posnum to nonneg.
;;; 2006.12.14 Dan
;;;             : * Changed current-device-interface so that it only returns
;;;             :   one value.
;;;             : * Reset all the versions to 1.1...
;;; 2006.12.18 Dan
;;;             : * Removed the :speech-hook parameter since it was depricated
;;;             :   long ago.
;;;             : * Also removed the :output-speech parameter because it doesn't
;;;             :   really have a purpose now - the default devices all have a
;;;             :   device-speak-string method even if it doesn't do anything.
;;; 2006.12.28 Dan
;;;             : * Made proc-display better check for current mp/model/device.
;;; 2007.01.09 Dan
;;;             : * Make output-key check the keyboard array boundaries to avoid
;;;             :   errors when bad key-locations come in.
;;; 2007.05.24 Dan
;;;             : * Removed the device-hook slot and call from the update-device
;;;             :   method since it was depricated long ago.
;;; 2007.06.20 Dan
;;;             : * Changed dmo-to-xy to xy-loc for use with the new vision
;;;             :   module.
;;;             : * Replaced the generic build-features-for and cursor-to-feature
;;;             :   with build-vis-loc-for and cursor-to-vis-loc.
;;; 2007.07.16 Dan
;;;             : * Fixed a bug in the default device-speak-string method that
;;;             :   would cause it to throw the "wrong" error.
;;; 2008.08.11 Dan [1.2]
;;;             : * Added a new parameter :stable-loc-names which controls
;;;             :   whether or not the virtual device sorts the subviews to
;;;             :   guarantee the same names on different runs.  The default is
;;;             :   t which means it still does the sorting.  Setting it to nil
;;;             :   should improve performance when using the virtual devices.
;;;             : * Also took the package setting out of the mode line at the
;;;             :   top since there isn't a specific package for any of the
;;;             :   source files.
;;; 2009.09.08 Dan
;;;             : * Fixed the key-to-command mapping for the period.
;;; 2010.08.26 Dan
;;;             : * Why is the space bar represented as space in the the
;;;             :   key->command table but spc in the key->loc table, and
;;;             :   why is it entered multiple times when they're all just
;;;             :   overwriting the same entry?  Adding an entry for space
;;;             :   into the key->loc table at 6,6 so there's at least a
;;;             :   way to relate things.
;;; 2011.01.19 Dan
;;;             : * Changed unlock-device to use unlock-tracking instead of
;;;             :   update-tracking since that has been depricated.
;;; 2011.04.28 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions.
;;; 2011.05.16 Dan
;;;             : * Added a parameter to allow enabling trace-mouse-p and added
;;;             :   a function, get-mouse-trace, to return the list of postions
;;;             :   and times that have been recorded.
;;; 2011.11.21 Dan
;;;             : * Removing *actr-enabled-p* since it doesn't really do much
;;;             :   of anything important, but its use in the tutorial models
;;;             :   makes it seem like it's something "special" and that requires 
;;;             :   edit/save/reloads which aren't really needed.  
;;;             :   Instead, replacing it with a command model-generated-action 
;;;             :   which returns true during the handling of the device methods
;;;             :   that get called as a result of the model performing the 
;;;             :   action so that they can be distinguished from a person if
;;;             :   needed.  However, this can't really be used at the "user"
;;;             :   level in general because it may or may not be valid during
;;;             :   the rpm-window- handler or the button action functions
;;;             :   when "real windows" are being used.
;;; 2012.06.26 Dan [1.3]
;;;             : * An installed device no longer persists across a reset.
;;;             : * Update-attended-loc now only gets called when the attention
;;;             :   actually changes or when the device changes.
;;;             : * Update-attended-loc can now get called with nil for the location
;;;             :   when the vision module "stops" attending to something which
;;;             :   can happen when there's an explicit visual clear request or
;;;             :   when the device changes, and it also gets called with nil
;;;             :   when the window gets initially installed.
;;;             : * There is a slot in the device interface for holding the 
;;;             :   attention marker which can be used so that each model has
;;;             :   its own: it can be accessed via the visual-fixation-marker 
;;;             :   function which takes no parameters and there is a setf function 
;;;             :   defined for it.  It returns the value for that slot from the
;;;             :   current model's device interface. It defaults to nil when
;;;             :   the model is initialized, but isn't changed at reset time
;;;             :   since the device itself could persist.
;;; 2012.08.27 Dan
;;;             : * Changed the indicate-model-generated macro to actually setf
;;;             :   the variable instead of using a let so that it "works" when
;;;             :   different processes may need to check the value since 
;;;             :   the rpm-window-key-event-handler may be called from an event
;;;             :   thread instead of in the "ACT-R" thread.
;;; 2012.09.07 Dan
;;;             : * Removed the test for :actr-env-alone in the pixels-per-inch
;;;             :   setting for ACL.
;;; 2014.04.24 Dan
;;;             : * Added commands to the press-key table so that all the keys
;;;             :   in the "primary area" have a mapping, but it does assume that
;;;             :   the model has a long right pinkie and can hit things 3+ keys
;;;             :   away without moving the hand...
;;;             : * Renamed some of the keys in the loc table because there were
;;;             :   duplicates between the "primary area" and the keypad.  Now
;;;             :   all of the keypad keys which appear in both areas are 
;;;             :   prefixed with "keypad-".
;;; 2014.05.21 Dan [2.0]
;;;             : * Just now noticed that the pixels-to-angle code isn't quite 
;;;             :   right given that we compute the location of something as
;;;             :   it's center point.  It should be 2*(atan (/ (w/2) d) not
;;;             :   just (atan (/ w d)) because that assumes a view from one
;;;             :   end.  Of course, it's even odder than that because we also
;;;             :   consider everything on the screen as the same distance, but
;;;             :   really that's only true if the display is a spherical
;;;             :   surface centered on the viewing position...
;;;             :   The difference in values between those 2 calculations is
;;;             :   generally negligible for the typical sizes and default 
;;;             :   viewing distance.  However, since I'm adding in code to
;;;             :   allow for distances other than the default I'm going to
;;;             :   switch to the "better" calculation.
;;; 2014.10.31 Dan
;;;             : * Changed synch-mouse so that it has an optional parameter
;;;             :   for indicating whether the action was "model generated"
;;;             :   instead of treating all usage like it was.
;;; 2015.05.20 Dan 
;;;             : * Adding a new method for devices: vis-loc-coordinate-slots.
;;;             :   It needs to return a list of three values which indicate the
;;;             :   names of the slots used to hold the x, y, and z coordinates
;;;             :   respectively for the visual location chunks created by the
;;;             :   device.  The default is '(screen-x screen-y distance), but
;;;             :   now one can use something else instead.
;;;             :   It is only called when the device is installed thus it can't
;;;             :   change dynamically and must be consistent for all features 
;;;             :   in the device.
;;; 2015.05.21 Dan
;;;             : * Fixed the decalim for xy-loc.
;;; 2015.09.01 Dan
;;;             : * Added press-key shortcuts for the keypad assuming that the
;;;             :   hand has been moved to it with hand-to-keypad or start-hand-
;;;             :   at-keypad.
;;; 2016.04.04 Dan [2.1]
;;;             : * Allow :show-focus to also take string and symbol values for
;;;             :   true.  Those values MAY be used by the device to indicate
;;;             :   a color for the fixation ring.
;;; 2016.07.20 Dan
;;;             : * Update-device now sends the clof value to device-update-attended-loc
;;;             :   since the current-marker value may not be a chunk anymore.
;;; 2016.12.15 Dan [3.0]
;;;             : * Start of the work to basically eliminate the device approach
;;;             :   to PM interfacing to better support remote modeling.  For now
;;;             :   all I've done is remove proc-display and the lock/unlock-device
;;;             :   commands since those really belong to vision.
;;; 2016.12.16 Dan
;;;             : * Removed the declaim for unlock-tracking and the pending-procs
;;;             :   slot.
;;; 2016.12.19 Dan
;;;             : * Remove the generic process-display method.
;;; 2017.01.05 Dan 
;;;             : * Complete overhaul.  This is no longer tied to any particular
;;;             :   modules by default.  A module must register as an interface
;;;             :   if it needs access to "devices".  Devices must be registered
;;;             :   as well.  Both the interface and the device indicate init
;;;             :   and remove commands (if needed) when registered.  Install-
;;;             :   device takes a list of 2 or 3 items: module device {details}.
;;;             :   When installing a device:
;;;             :    - if that device and details are already installed for
;;;             :       indicated module then do nothing
;;;             :    - else  
;;;             :     - if the indicated module only allows a single device and
;;;             :       there is one already installed
;;;             :         call the remove commands for the module then device
;;;             :     - call the init commands for the module and device
;;;             :     - store the record for this device being installed
;;; 2017.01.11 Dan
;;;             : * Removed the show-focus parameter and moved it to vision.
;;; 2017.01.13 Dan
;;;             : * Added the interface function to a device definition and the
;;;             :   signal-device command for communicating with it through that
;;;             :   function.
;;;             : * Removed the keyboard related code.
;;; 2017.01.18 Dan
;;;             : * Removed the echo-act-r-output from commands.  The assumption
;;;             :   now is that echoing is handled by the user.
;;;             : * Use handle-evaluate-results.
;;; 2017.01.26 Dan
;;;             : * Removed output-speech.  That happens directly from speech
;;;             :   module now.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.08 Dan
;;;             : * Reworked the local versions of most remote commands to not
;;;             :   go out through the dispatcher.
;;;             : * Fixed bugs with the last change.
;;; 2017.03.02 Dan
;;;             : * Removed all the mouse code.
;;; 2017.03.03 Dan
;;;             : * Remove-device now returns t when remove is successful.
;;; 2017.03.07 Dan [4.0]
;;;             : * Signal-device command can't be single instance because it
;;;             :   may be possible for a device being signaled to signal another
;;;             :   which would cause a deadlock (the mouse signaling the exp-window
;;;             :   is where it occurred).
;;;             : * Reset and delete need to remove any installed devices.
;;;             : * Adding a delete function to handle that now.
;;;             : * When installing a device put it on the list before calling
;;;             :   the init functions so that it can be referenced from there.
;;;             :   Not certain that's the best way to handle things yet, but it
;;;             :   fixes an issue with a window installing a mouse that then 
;;;             :   needs to tell the window to draw the mouse...
;;; 2017.06.02 Dan
;;;             : * Wrapped some handle-evaluate-results around the evaluate 
;;;             :   calls to catch errors.
;;; 2017.06.16 Dan
;;;             : * Adding a lock for protecting the device slot of the interface,
;;;             :   a lock for the device-table and interface-table, a lock for the
;;;             :   visual-fixation-marker, and a lock for the params.
;;;             : * Removed the mouse tracing param and slots.
;;; 2017.06.20 Dan
;;;             : * Rewored install-, remove-, and signal- device to only lock as
;;;             :   needed to avoid issues with devices that install devices (like
;;;             :   experiment windows) and a recursive lock won't help because 
;;;             :   since the interface/device actions themselves could be dispatched.
;;;             : * Also marking them as not single instance.
;;; 2017.06.23 Dan
;;;             : * Added names for all the locks.
;;; 2017.08.03 Dan
;;;             : * Actually store the name of the interface in the structure.
;;; 2017.08.09 Dan
;;;             : * Remove all the declaims since they're unnecessary now. 
;;; 2017.08.17 Dan
;;;             : * Fixed a potential deadlock with respect to delete-device
;;;             :   because it called the remove functions for devices and 
;;;             :   interfaces while it held the device module's locks.
;;; 2017.10.11 Dan
;;;             : * Eliminate the vwt parameter and virtual-trace slot.
;;; 2018.01.29 Dan
;;;             : * Reformating the remove-device code since I was trying to
;;;             :   figure out a problem with removing a mouse and realized that
;;;             :   the indentation was off for remove-device eventhough it did
;;;             :   work correctly.
;;; 2018.02.08 Dan
;;;             : * Don't abort from delete-device just because some device fails.
;;;             :   Print a warning about that and continue with the rest.
;;; 2018.03.05 Dan
;;;             : * Rename signal-device notify-device since I call commands
;;;             :   with no function signals...
;;;             : * Why does notify-device have to go through dispatcher?  Is
;;;             :   that necessary -- should it be monitorable?
;;; 2018.03.12 Dan [5.0]
;;;             : * Added available-devices and available-interfaces functions.
;;;             : * Reworking it so that it's possible to install more than
;;;             :   one of the same device -- that simplifies things with
;;;             :   respect to a mouse because the device can be a cursor and
;;;             :   the details can then specify mouse, joystick1, or joystick2
;;;             :   and the same move-cursor command can just use whichever
;;;             :   cursor device the hand is on.  It also allows more than
;;;             :   one exp-window to be installed which means the cursor
;;;             :   can be in global coordinates and if the windows translate
;;;             :   things appropriately multiple windows could be available
;;;             :   to vision at the same time.
;;;             : * Interface fns all get whole device-list.
;;; 2018.03.14 Dan
;;;             : * Needs-mouse param can now be a symbol or string and will be
;;;             :   converted to a color for the outline like show-focus does
;;;             :   since each model has its own mouse now.  It also defaults to
;;;             :   nil instead of t to avoid having to deal with it unless it's
;;;             :   actually needed.
;;; 2018.04.25 Dan [6.0]
;;;             : * Eliminate the *pixels-per-inch-?* variables and just default
;;;             :   to 72.
;;;             : * Removed the visual-fixation-marker support and the cursor-loc.
;;;             : * Renamed :mouse-fitts-coeff to :cursor-fitts-coeff.
;;; 2018.04.26 Dan
;;;             : * Current-devices now prints a warning if an invalid interface
;;;             :   is provided.
;;;             : * Added a remote current-devices.
;;;             : * Defined-devices and defined-interfaces don't require a current
;;;             :   model to operate.
;;; 2018.05.21 Dan
;;;             : * Renamed the slot from act-r-device-interface to -notification
;;;             :   since it's called from notify-device to keep the terminology
;;;             :   consistent.
;;; 2018.05.30 Dan
;;;             : * Put the key-closure-time param here instead of it being a
;;;             :   fixed value (it was being added in the motor extensions).
;;; 2018.06.13 Dan
;;;             : * Updated the doc strings for remote commands to match the 
;;;             :   current spec style.
;;; 2018.10.04 Dan [7.0]
;;;             : * An interface can now also provide a notification function 
;;;             :   and there's a notify-interface command.  That allows for
;;;             :   a better separation of device from module (for example
;;;             :   keyboard and cursor device implementations currently access
;;;             :   the motor module directly).
;;;             : * Create a global variable with a default device interface to
;;;             :   avoid having to create one for the non-model based commands.
;;;             : * Notify-device and notify-interface decode the features provided
;;;             :   from external calls with the embedded string mechanism, but
;;;             :   don't decode the device or interface names.
;;; 2018.11.01 Dan [7.1]
;;;             : * A device now has two additional optional functions it can
;;;             :   specify which are called by the motor module when a hand is
;;;             :   set to the device and when the hand is unset from the device.
;;;             :   They are called with two parameters: the device-list and the
;;;             :   hand (left or right).
;;;             : * Moved the default-target-width parameter here from motor.
;;; 2020.01.10 Dan [7.2]
;;;             : * Removed the #' and lambdas from the module interface functions
;;;             :   since that's not allowed in the general system now.
;;; 2021.10.19 Dan [7.3]
;;;             : * Set the required flag for the module to a list of :motor, 
;;;             :   :vision, and :speech, but it may be needed at other times
;;;             :   since it handles all devices.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Class for the device-interface and a default one for convenience.

(defclass device-interface ()
  ((pixels-per-inch :accessor ppi :initarg :ppi :initform 72)
   (viewing-distance :accessor viewing-distance :initform 15.0)
   (device :accessor device :initform nil)
   (device-lock :accessor device-lock :initform (bt:make-lock "device-lock"))
   (key-closure-time :accessor key-closure-time :initform 0.010)
   (with-cursor-p :accessor with-cursor-p :initform nil)
   (mouse-fitts-coeff :accessor mouse-fitts-coeff :initform 0.1)
   (show-focus-p :accessor show-focus-p :initarg :show-focus-p :initform nil)
   (needs-mouse-p :accessor needs-mouse-p :initarg :needs-mouse-p :initform t)
   (version-string :accessor version-string :initarg :version-string :initform "7.3")
   (stable-names :accessor stable-names :initarg :stable-names :initform t)
   (param-lock :accessor device-param-lock :initform (bt:make-lock "device-param"))
   (device-tables-lock :accessor device-tables-lock :initform (bt:make-lock "device-tables") :allocation :class)
   (interface-table :accessor interface-table :initform (make-hash-table :test 'equalp) :allocation :class)
   (default-target-width :accessor default-target-width :initarg :default-target-width :initform 1.0)
   (device-table :accessor device-table :initform (make-hash-table :test 'equalp) :allocation :class)))

(defvar *default-device-interface* (make-instance 'device-interface))



(defstruct interface name init remove single-device notification)
(defstruct act-r-device init remove notification set unset) 

(defun define-interface (name &optional init remove single? notification)
  (let ((di *default-device-interface*))
    (bt:with-lock-held ((device-tables-lock di))
      (if (gethash name (interface-table di))
          (print-warning "Interface ~s already exists and cannot be created." name)
        (progn
          (when init 
            (unless (check-act-r-command init)
              (print-warning "Init function ~s specified for interface ~s is not available.  Cannot create interface." init name)
              (return-from define-interface)))
          (when remove 
            (unless (check-act-r-command remove)
              (print-warning "Remove function ~s specified for interface ~s is not available.  Cannot create interface." remove name)
              (return-from define-interface)))
          (when notification 
            (unless (check-act-r-command notification)
              (print-warning "Notification function ~s specified for interface ~s is not available.  Cannot create interface." notification name)
              (return-from define-interface)))
          (setf (gethash name (interface-table di)) (make-interface :name name :init init :remove remove :single-device single? :notification notification))
          t)))))

(add-act-r-command "define-interface" 'define-interface "Command to add a new interface. Params: name {init-command {remove-command {single-device {notification}}}}")

(defun undefine-interface (name)
  (let ((di *default-device-interface*))
    (bt:with-lock-held ((device-tables-lock di))
      (when (gethash name (interface-table di))
        (remhash name (interface-table di))
        t))))

(add-act-r-command "undefine-interface" 'undefine-interface "Command to remove an interface. Params: name")



(defun notify-interface (name features)
  "Send the provided features to the named interface"
  (let* ((di *default-device-interface*)
         (interface (bt:with-lock-held ((device-tables-lock di)) (gethash name (interface-table di)))))
    (if interface
        (aif (interface-notification interface)
             (dispatch-apply it features)
             (print-warning "Interface ~s does not accept notifications. Ignoring notification features ~s." name features))
      (print-warning "Interface ~s could not be found when calling notify-interface with features ~s." name features))))


(defun external-notify-interface (name features)
  (notify-interface name (decode-string-names features)))


(add-act-r-command "notify-interface" 'external-notify-interface "Send features to the indicated interface. Params: interface 'features'" nil)

(defun define-device (name &optional init remove notification set-hand unset-hand)
  (let ((di *default-device-interface*))
    (bt:with-lock-held ((device-tables-lock di))
      (if (gethash name (device-table di))
          (print-warning "Device ~s already exists and cannot be created." name)
        (progn
          (when init 
            (unless (check-act-r-command init)
              (print-warning "Init function ~s specified for device ~s is not available.  Cannot define device."
                             init name)
              (return-from define-device)))
          (when remove 
            (unless (check-act-r-command remove)
              (print-warning "Remove function ~s specified for device ~s is not available.  Cannot define device."
                             remove name)
              (return-from define-device)))
          (when notification 
            (unless (check-act-r-command notification)
              (print-warning "Notification function ~s specified for device ~s is not available.  Cannot define device."
                             notification name)
              (return-from define-device)))
          (when set-hand 
            (unless (check-act-r-command set-hand)
              (print-warning "Set-hand function ~s specified for device ~s is not available.  Cannot define device."
                             set-hand name)
              (return-from define-device)))
          (when unset-hand 
            (unless (check-act-r-command unset-hand)
              (print-warning "Unset-hand function ~s specified for device ~s is not available.  Cannot define device."
                             unset-hand name)
              (return-from define-device)))
          (setf (gethash name (device-table di)) (make-act-r-device :init init :remove remove :notification notification :set set-hand :unset unset-hand))
          t)))))

(add-act-r-command "define-device" 'define-device "Command to add a new device. Params: name {init-command {remove-command {notification-command {hand-set {hand-unset}}}}")

        

(defun undefine-device (name)
  (let ((di *default-device-interface*))
    (bt:with-lock-held ((device-tables-lock di))
      (when (gethash name (device-table di))
        (remhash name (device-table di))
        t))))

(add-act-r-command "undefine-device" 'undefine-device "Command to remove a device. Params: name")



(defun install-device (device-list)
  "Set a device with which a model will interact"
  (verify-current-model
   "install-device called with no current model."
      
      (if (and (listp device-list) 
               (<= 2 (length device-list) 3))
          (let ((interface-name (first device-list))
                (device-name (second device-list))
                (devin (current-device-interface))
                interface device)
            
            (bt:with-lock-held ((device-tables-lock devin))
              (setf interface (gethash interface-name (interface-table devin)))
              (setf device (gethash device-name (device-table devin))))
            
            (cond ((null interface)
                   (print-warning "Invalid interface ~s in call to install-device with ~s. No device installed."
                                  interface-name device-list))
                  ((null device)
                   (print-warning "Invalid device ~s in call to install-device with ~s. No device installed."
                                  device-name device-list))
                  (t
                   (let ((d (bt:with-lock-held ((device-lock devin))
                              (device devin))))
                     
                     (if (find device-list d :test 'equalp)
                         (print-warning "The device ~s is already installed. No device installed." device-list)
                       (progn
                         (awhen (and (interface-single-device interface)
                                     (find interface-name d :key 'first :test 'string-equal))
                                (when (interface-remove interface)
                                  (unless (handle-evaluate-results (evaluate-act-r-command (interface-remove interface) it))
                                    (return-from install-device nil)))
                                (when (act-r-device-remove device)
                                  (unless (handle-evaluate-results (evaluate-act-r-command (act-r-device-remove device) it))
                                    (return-from install-device nil)))
                                (bt:with-lock-held ((device-lock devin))
                                  (setf d (setf (device devin) (remove it (device devin))))))
                         
                         ;; should be there before trying to init 
                         ;; but needs to be removed if errors occur
                           
                         (bt:with-lock-held ((device-lock devin))
                           (push device-list (device devin)))
                         
                         (when (interface-init interface)
                           (unless (handle-evaluate-results (evaluate-act-r-command (interface-init interface) device-list))
                             (bt:with-lock-held ((device-lock devin))
                               (setf (device devin) (remove device-list (device devin))))
                             (return-from install-device nil)))
                         (when (act-r-device-init device)
                           (unless (handle-evaluate-results (evaluate-act-r-command (act-r-device-init device) device-list))
                             (bt:with-lock-held ((device-lock devin))
                               (setf (device devin) (remove device-list (device devin))))
                             (return-from install-device nil)))
                         device-list))))))
        (print-warning "Invalid device-list provided ~s.  No device installed." device-list))))

(add-act-r-command "install-device" 'install-device "Command to install a device for an interface. Params: (interface device {device-detail})" nil)

(defun remove-device (device-list)
  "Remove a device with which a model is interacting"
   (verify-current-model
    "remove-device called with no current model."
    (let* ((devin (current-device-interface))
           (device (bt:with-lock-held ((device-lock devin)) (device devin)))
           (exists (find device-list device :test 'equalp)))
          
      (if exists
          (let ((interface-name (first device-list))
                (device-name (second device-list))
                interface device)
            (bt:with-lock-held ((device-tables-lock devin))
              (setf interface (gethash interface-name (interface-table devin)))
              (setf device (gethash device-name (device-table devin))))
                
            ;; The warning cases shouldn't happen since it was installed
            ;; but it's possible that one was undefined after the install 
            ;; which doesn't get caught currently.
            
            (cond ((null interface)
                   (print-warning "Invalid interface ~s in call to remove-device with ~s. No device removed."
                                  interface-name device-list))
                  ((null device)
                   (print-warning "Invalid device ~s in call to remove-device with ~s. No device removed."
                                  device-name device-list))
                  (t
                   (when (interface-remove interface)
                     (unless (handle-evaluate-results (evaluate-act-r-command (interface-remove interface) device-list))
                           (return-from remove-device nil)))
                   (when (act-r-device-remove device)
                     (unless (handle-evaluate-results (evaluate-act-r-command (act-r-device-remove device) device-list))
                       (return-from remove-device nil)))
                   (bt:with-lock-held ((device-lock devin))
                     (setf (device devin) (remove exists (device devin))))
                   t)))
        (print-warning "Device ~s is not currently installed and cannot be removed." device-list)))))

(add-act-r-command "remove-device" 'remove-device "Command to remove a device for an interface. Params: (interface device {device-detail})" nil)

(defun current-devices (interface)
  "Return the devices for the named interface in the current model"
  (verify-current-model
   "current-devices called with no current model."
   (let ((devin (current-device-interface)))
     (bt:with-lock-held ((device-lock devin))
       (if (gethash interface (interface-table devin))
           (remove-if-not (lambda (x) (string-equal interface (first x))) (device devin))
         (print-warning "~s does not name a valid interface in call to current-devices." interface))))))


(add-act-r-command "current-devices" 'current-devices "Get the list of all devices installed for an interface. Params: interface" nil)

(defun current-device-interface ()
  "Return the device-interface for current model"
  (values (get-module :device)))

(defun defined-devices ()
  (let ((di *default-device-interface*))
    (bt:with-lock-held ((device-tables-lock di))
      (hash-table-keys (device-table di)))))
     

(defun defined-interfaces ()
  (let ((di *default-device-interface*))
    (bt:with-lock-held ((device-tables-lock di))
      (hash-table-keys (interface-table di)))))

(add-act-r-command "defined-devices" 'defined-devices "Command to get the list of all defined devices. No params" nil)
(add-act-r-command "defined-interfaces" 'defined-interfaces "Command to get the list of all defined interfaces. No params" nil)


(defun notify-device (device-list features)
  "Send the provided features to the named device for the specified interface in the current model in the current meta-process if such a function was provided"
  (verify-current-model
   "notify-device called with no current model."
   (let ((di (current-device-interface)))
     (if di
         (let ((d (bt:with-lock-held ((device-lock di)) (device di))))
           (if (find device-list d :test 'equalp)
               (let ((device (gethash (second device-list) (bt:with-lock-held ((device-tables-lock di)) (device-table di)))))
                 (when (and device (act-r-device-notification device))
                   (let ((res (multiple-value-list (evaluate-act-r-command (act-r-device-notification device) device-list features))))
                     (if (first res)
                         (values-list (rest res))
                       (values-list res))))
                 ;; don't warn if there isn't a device-function just fall through and return nil
                 )
             (print-warning "Device ~s is not installed when trying to notify with features ~s." device-list features)))
       (print-warning "Device-interface could not be found when trying notify device ~s." device-list)))))

(defun external-notify-device (device-list features)
  (notify-device device-list (decode-string-names features)))

(add-act-r-command "notify-device" 'external-notify-device "Send features to the given device. Params: device-list 'features'" nil)

(defun notify-device-hand-set (device-list hand)
  "Call the set-hand command for the device if it has one"
  (verify-current-model
   "notify-device-hand-set called with no current model."
   (let ((di (current-device-interface)))
     (if di
         (let ((d (bt:with-lock-held ((device-lock di)) (device di))))
           (if (find device-list d :test 'equalp)
               (let ((device (gethash (second device-list) (bt:with-lock-held ((device-tables-lock di)) (device-table di)))))
                 (awhen (and device (act-r-device-set device))
                   (dispatch-apply it device-list hand)))
             (print-warning "Device ~s is not installed when trying to notify for hand being set." device-list)))
       (print-warning "Device-interface could not be found when trying notify device ~s for hand being set." device-list)))))

(defun notify-device-hand-unset (device-list hand)
  "Call the unset-hand command for the device if it has one"
  (verify-current-model
   "notify-device-hand-set called with no current model."
   (let ((di (current-device-interface)))
     (if di
         (let ((d (bt:with-lock-held ((device-lock di)) (device di))))
           (if (find device-list d :test 'equalp)
               (let ((device (gethash (second device-list) (bt:with-lock-held ((device-tables-lock di)) (device-table di)))))
                 (awhen (and device (act-r-device-unset device))
                   (dispatch-apply it device-list hand)))
             (print-warning "Device ~s is not installed when trying to notify for hand being unset." device-list)))
       (print-warning "Device-interface could not be found when trying notify device ~s for hand being unset." device-list)))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; some simple device-interface methods.


(defmethod my-name ((mod device-interface))
  :DEVICE)

(defgeneric angle->pixels-mth (devin angle &optional dist)
  (:documentation  "Determine the number of pixels subtending a visual angle."))

(defmethod angle->pixels-mth ((devin device-interface) (angle number) &optional dist)
  (bt:with-lock-held ((device-param-lock devin))
    (if (numberp dist)
        (round (* 2 dist (tan (deg->rad (/ angle 2)))))
      (round (* 2 (viewing-distance devin) (ppi devin) (tan (deg->rad (/ angle 2))))))))


(defgeneric pixels->angle-mth (devin pixels &optional dist)
  (:documentation  "Determine the amount of visual angle subtended by <pixels>."))

(defmethod pixels->angle-mth ((devin device-interface) (pixels number) &optional dist)
  (bt:with-lock-held ((device-param-lock devin))
    (if (numberp dist)
        (rad->deg (* 2 (atan (/ pixels 2) dist)))
      (rad->deg (* 2 (atan (/ pixels 2) (* (ppi devin) (viewing-distance devin))))))))


(defgeneric find-viewing-dist-mth (devin angle pixels &optional in-pixels)
  (:documentation  "Given the number of pixels an angle subtends, what's the viewing distance?"))

(defmethod find-viewing-dist-mth ((devin device-interface) angle pixels &optional in-pixels)
  (bt:with-lock-held ((device-param-lock devin))
    (if in-pixels
        (floor (/ pixels 2) (tan (deg->rad (/ angle 2))))
      (floor (/ pixels 2) (* (tan (deg->rad (/ angle 2))) (ppi devin))))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; some simple user commands

(defun pm-pixels-to-angle (pixels &optional dist)
  "Convert <pixels> to degress of visual angle."
  (pixels->angle-mth (current-device-interface) pixels dist))


(defun pm-angle-to-pixels (angle &optional dist)
  "Convert visual <angle> in degress to pixels."
  (angle->pixels-mth (current-device-interface) angle dist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The actual module interface code.

(defmethod reset-device ((devin device-interface))
  
  (delete-device devin)
  
  ;; Clear the installed devices now
  (bt:with-lock-held ((device-lock devin))
    (setf (device devin) nil)))

(defmethod delete-device ((devin device-interface))
  (let (interface device)
    
    (dolist (device-list (bt:with-lock-held ((device-lock devin)) (device devin)))
      (bt:with-lock-held ((device-tables-lock devin))
        (setf interface (gethash (first device-list) (interface-table devin))
          device (gethash (second device-list) (device-table devin))))
      
      ;; The warning cases shouldn't happen since it was installed
      ;; but it's possible that one was undefined after the install 
      ;; which doesn't get caught currently.
      
      (cond ((null interface)
             (print-warning "Invalid interface ~s while trying to remove-device with ~s. No device removed."
                            (first device-list) device-list))
            ((null device)
             (print-warning "Invalid device ~s while trying to remove-device with ~s. No device removed."
                            (second device-list) device-list))
            (t
             (when (interface-remove interface)
               (unless (handle-evaluate-results (evaluate-act-r-command (interface-remove interface) device-list))
                 (print-warning "Interface-remove failed for ~S" device-list)))
             (when (act-r-device-remove device)
               (unless (handle-evaluate-results (evaluate-act-r-command (act-r-device-remove device) device-list))
                 (print-warning "Device-remove failed for ~S ~S" (act-r-device-remove device) device-list)))
             t)))))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Module interface functions

(defun params-device-module (device param)
    (bt:with-lock-held ((device-param-lock device))
      (if (consp param)
          (case (car param)
            (:pixels-per-inch
             (setf (ppi device) (cdr param)))
            (:process-cursor
             (setf (with-cursor-p device) (cdr param)))
            (:viewing-distance
             (setf (viewing-distance device) (cdr param)))
            
            (:cursor-fitts-coeff
             (setf (mouse-fitts-coeff device) (cdr param)))
            
            (:needs-mouse
             (setf (needs-mouse-p device) (cdr param)))
            (:default-target-width
             (setf (default-target-width device) (cdr param)))
            (:stable-loc-names
             (setf (stable-names device) (cdr param)))
            (:key-closure-time
             (setf (key-closure-time device) (cdr param)))
            )
        (case param
          (:pixels-per-inch
           (ppi device))
          (:process-cursor
           (with-cursor-p device))
          (:viewing-distance
           (viewing-distance device))
          (:cursor-fitts-coeff
           (mouse-fitts-coeff device))
          (:needs-mouse
           (needs-mouse-p device))
          (:key-closure-time
           (key-closure-time device))
          (:stable-loc-names
           (stable-names device))
          (:default-target-width
           (default-target-width device))))))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Interface functions 

(defun tornilorsymbolorstring (x) (or (tornil x) (symbolp x) (stringp x))) 

(defun create-device-module (x)
  (declare (ignore x))
  (make-instance 'device-interface))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Module definition

(define-module-fct :device
    nil
  (list 
   ;; From vision
   (define-parameter :pixels-per-inch
     :valid-test 'posnum 
     :default-value 72.0
     :warning "a non-negative number"
     :documentation "Pixels per inch of display")
   
   (define-parameter :process-cursor
     :valid-test 'tornil 
     :default-value nil
     :warning "T or NIL"
     :documentation "Should the AGI create a visicon feature for the mouse cursor?")
   
   (define-parameter :viewing-distance
     :valid-test 'posnum 
     :default-value 15.0
     :warning "a non-negative number"
     :documentation "Distance of the eyes from the display, in inches.")
   
   ;; From motor
   
   (define-parameter :key-closure-time
       :valid-test 'nonneg
       :warning "a number"
       :default-value 0.01
       :documentation "Time between when a key is contacted and it registers as being hit.")
   
   (define-parameter :cursor-fitts-coeff
     :valid-test 'nonneg 
     :default-value 0.1
     :warning "a non-negative number"
     :documentation "b coefficient in Fitts's equation for aimed movements.")
   
   (define-parameter :needs-mouse
     :valid-test 'tornilorsymbolorstring 
     :default-value nil
     :warning "T, NIL, a symbol, or string"
     :documentation "Should the AGI install a mouse device?")
   
   (define-parameter :stable-loc-names
     :valid-test 'tornil 
     :default-value t
     :warning "T or NIL"
     :documentation "Whether or not to sort the virtual window's subviews to guarantee the names always line up")
   
   (define-parameter :default-target-width
     :valid-test 'nonneg 
     :default-value 1.0
     :warning "a non-negative number"
     :documentation 
     "Width of targets with unspecified widths for approach-width calculations and ply motor actions (no units)."))
  
  :version (version-string (make-instance 'device-interface))
  :documentation "The device interface for a model"
  :creation 'create-device-module
  :reset 'reset-device
  :delete 'delete-device
  :params 'params-device-module
  :required (list :motor :vision :speech))


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
