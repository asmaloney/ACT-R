;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : visible-virtual.lisp (renamed from env-device.lisp)
;;; Version     : 5.0
;;; 
;;; Description : No system dependent code.
;;;             : This file contains the code that handles passing
;;;             : the virtual windows out through Tk - the visible
;;;             : virtuals, and the AGI support.
;;; Bugs        : [ ] What should happen if a button goes away between when the
;;;             :     model initiates an action and finishes it?
;;; 
;;; Todo        :   
;;; ----- History -----
;;;
;;; 10/01/2002  Dan
;;;             : Added this header. 
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;; 12/04/2002  Dan
;;;             : Added the definition for *use-environment-window*
;;;             : even though it's still not used because it
;;;             : does get set by the environment and generates a
;;;             : warning.
;;; 01/06/2003  Dan
;;;             : Updated it so that the colors were sent out
;;;             : properly for all supported colors and so that
;;;             : text colors worked.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; -----------------------------------------------------------------------
;;; 2005.04.13  Dan [2.0]
;;;             : * Moved over to ACT-R 6.
;;; 2005.06.07 Dan
;;;             : * Replaced a call to device-interface with 
;;;             :   current-device-interface.
;;; 2005.06.28 Dan
;;;             : * Removed a pm-proc-display call that was still lingering
;;;             :   in the device-move-cursor-to method.
;;; 2007.07.13 Dan
;;;             : * Added the sending of a button's color to the env.
;;; 2010.05.21 Dan
;;;             : * Changed the name of the dialog items to add a vv on the
;;;             :   to avoid interning a name which could shift the names
;;;             :   for things like visual chunks (text items in particular).
;;;             :   Doesn't affect performance, just keeps traces consistent
;;;             :   when shifting between visible and virtual.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.11.21 Dan
;;;             : * Using model-generated-action instead of *actr-enabled-p*
;;;             :   to detect when the button needs to be visibly "clicked".
;;; 2012.06.26 Dan
;;;             : * Adding a clearattention flag that gets sent and save the
;;;             :   window used in the visual-fixation-marker slot so that if
;;;             :   the window changes it can clear the old marker before
;;;             :   displaying the new one.
;;; 2012.06.27 Dan [3.1]
;;;             : * Start the process of supporting multiple windows on the
;;;             :   environment side.
;;;             :   - Give the window a unique id using new-symbol instead of
;;;             :     new-name.
;;;             :   - Use the symbol-name of the ids instead of the symbol since
;;;             :     if the window persists across a reset those symbols will
;;;             :     get uninterned which means they won't match what was
;;;             :     originally sent.
;;;             :   - Create the items in the context of the model in which the
;;;             :     window was created so that the names are guaranteed to be
;;;             :     unique without having to use new-symbol.
;;; 2012.07.12 Dan
;;;             : * Added a custom vw-output method for visible-virtual windows
;;;             :   because in a multiple model situation the output should
;;;             :   be based on the owning model's settings instead of whichever
;;;             :   model (if any) happens to be current.
;;; 2012.08.10 Dan
;;;             : * Support more than one fixation ring per window since 
;;;             :   multiple models may be sharing a device by using the
;;;             :   model name in a tag.
;;; 2012.08.31 Dan
;;;             : * Added env-window-click so that I can catch all the user
;;;             :   clicks on the environment side window and call the
;;;             :   rpm-window-click-event-handler appropriately.
;;; 2012.09.21 Dan
;;;             : * Grabbing the environment lock before sending the updates
;;;             :   with send-env-window-update.
;;;             : * Bad idea - can deadlock things...
;;; 2013.01.10 Dan
;;;             : * Env. sends keys over as strings now and uses a function
;;;             :   named convert-env-key-name-to-char to convert that to a
;;;             :   character instead of throwing a warning on the read of an
;;;             :   invalid character.  If it's an invalid character it still
;;;             :   prints a warning, but now that should be more informative.
;;;             : * Instead of redefining visible-virtuals-available? here the
;;;             :   function in the virtual's uwi file now calls check-with-environment-for-visible-virtuals
;;;             :   which is defined here instead.
;;; 2013.07.18 Dan
;;;             : * Fixed a potential bug in vv-click-event-handler because
;;;             :   the button could be removed between the model's click 
;;;             :   initiation and execution in which case it can't send
;;;             :   the update to the environment.  Probably shouldn't eval
;;;             :   the action either, but I'm not fixing that now.
;;; 2014.08.29 Dan
;;;             : * Changed vv-click-event-handler so that the action can be
;;;             :   either a function or a symbol naming a function.
;;; 2015.04.29 Dan
;;;             : * Changed convert-env-key-name-to-char so that it can handle
;;;             :   things like space and return.
;;; 2015.05.26 Dan
;;;             : * Added a font-size parameter to make-static-text-for-rpm-window.
;;;             :   Should be a point size for the font, defaults to 12 and the
;;;             :   height and width are based on the ratios for the previous
;;;             :   defaults which were 10 high and 7 wide for 12 point.  Then
;;;             :   pass that over to the environment to use when drawing it
;;;             :   in add-visual-items-to-rpm-window.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2016.04.01 Dan [3.2]
;;;             : * Treat human clicks in the visible virtuals the same as
;;;             :   model clicks and let the virtual window deal with the
;;;             :   processing.  Avoids a lot of crazyness on the Env. side
;;;             :   and is consistent with respect to the click event handler.
;;;             :   This also coincides to a shift on the Env. side of not
;;;             :   using real buttons in the visible virtual window so that
;;;             :   cursor and visual attention always show over items.  
;;;             : * That means that all clicks are handled by the subviews and
;;;             :   button "clicks" all need to be sent over.
;;; 2016.04.04 Dan [3.3]
;;;             : * Actually use the show-focus value to determine a color if
;;;             :   it's a valid symbol or a string.
;;; 2016.06.08 Dan
;;;             : * Change add-visual-items-to-rpm-window method so that it
;;;             :   only sends a message when it recognizes the item type
;;;             :   instead of sending a null message when it's a new type
;;;             :   of item to allow for easier customizing/updating.
;;;             : * Add the methods to handle modifying the items.  For now
;;;             :   it takes the simple approach of calling the virtual item
;;;             :   code to make the changes and then removing and redrawing
;;;             :   the item in the Env. window.
;;; 2016.08.09 Dan
;;;             : * Changed the modify methods to just specify allow-other-keys
;;;             :   since they don't directly use any of them.
;;; 2017.01.06 Dan
;;;             : * Removed vw-output and allow-event-manager.
;;; 2017.01.20 Dan
;;;             : * Updated the keypress handler to just call output-key, and
;;;             :   for now it just passes the raw string received without
;;;             :   trying to clean that up to match the keyboard names.
;;; 2017.01.23 Dan
;;;             : * Added a mapping from my Windows keyboard names to the 
;;;             :   cannonical model keyboard name where applicable.
;;; 2017.01.24 Dan
;;;             : * Set one monitoring function for visible virtual experiment
;;;             :   windows to monitor update-attended-loc so it can draw the
;;;             :   fixation ring.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.24 Dan
;;;             : * Updated visible-virtual-update-loc-monitor because now
;;;             :   it's a simple monitor which gets the params directly.
;;; 2017.03.02 Dan
;;;             : * Updated env-window-click to just call vv-click-handler
;;;             :   now since that's the appropriate underlying method.
;;; 2017.03.06 Dan
;;;             : * Have the user clicks also call click-the-mouse for monitoring.
;;; 2017.03.08 Dan
;;;             : * Use the device-move-cursor-to method to send the notice
;;;             :   to the environment, but doesn't need to do anything else.
;;; 2017.03.16 Dan
;;;             : * Added the declaims to avoid compiler warnings.
;;; 2017.05.31 Dan
;;;             : * Use new-symbol instead of new-name so there doesn't need
;;;             :   to be a current model to create GUI elements.
;;; 2017.06.07 Dan
;;;             : * Remove the vv-model slot from the window class and all
;;;             :   the code that relates to that.
;;;             : * Use the window's lock when adding or removing things.
;;; 2017.06.08 Dan
;;;             : * Fixed the with-act-r-lock usage since it uses a simpler 
;;;             :   syntax than the underlying with-lock.
;;; 2017.06.13 Dan
;;;             : * Determine-exp-window needs the agi component passed in now.
;;; 2017.06.16 Dan
;;;             : * Use call-next-method in close-rpm-window.
;;;             : * Add a lock to protect the monitor setting.
;;;             : * Add a lock to protect the *vv-table*.
;;;             : * Change the class from which the dialog items are based
;;;             :   because they need a lock slot.
;;; 2017.06.19 Dan
;;;             : * Adding lock checks around add- and remove- code.
;;; 2017.06.23 Dan
;;;             : * Added names for all the locks.
;;; 2017.09.08 Dan [4.0]
;;;             : * Update to use the new ACT-R interface instead of the old
;;;             :   Environment handler mechanisms.  This allows for anyone
;;;             :   to register to receive the updates, and multiple different
;;;             :   viewers could all be displaying simultaneously.
;;; 2017.09.11 Dan
;;;             : * Finished off the update and docs on how it works.
;;;             : * Renamed it visible-virtual.lisp and moved it to the
;;;             :   tools directory.
;;; 2017.09.13 Dan
;;;             : * The device-interface doesn't hold the show-focus value
;;;             :   anymore -- it's in the vision module so changed that to
;;;             :   just use sgp now.
;;;             : * The click option was being sent as the symbol and not a
;;;             :   lower case string.
;;;             : * Fix a bug in close-exp-window which got lost in the 
;;;             :   transition to the new file.
;;;             : * Don't need to send the clearattention twice when the 
;;;             :   current window is the same as the previous.
;;; 2017.09.22 Dan
;;;             : * Fixed a bug with the modify methods.
;;; 2018.01.17 Dan
;;;             : * Apparently reintroduced the bug with text feats differing
;;;             :   between virtual and visible...
;;;             :   Set the text-height of the items the same as for virtual
;;;             :   where it's 10/12ths the font-size and then send the real
;;;             :   font-size over for displaying.
;;; 2018.01.29 Dan
;;;             : * Generalize the add-visual-items-to-rpm-window method so
;;;             :   that it's easier to add new item types which goes along
;;;             :   with the generalization that occurred in the environment
;;;             :   handler.
;;; 2018.03.13 Dan
;;;             : * Multiple cursors allowed per window.
;;; 2018.03.15 Dan
;;;             : * Simplified the class hierarcy by removing the rpm-...
;;;             :   classes which aren't needed anymore.
;;; 2018.03.20 Dan
;;;             : * Added a slot to the virtual window class for recording
;;;             :   whether or not the fixation ring is drawn in the window
;;;             :   since multiple windows can be installed at the same time
;;;             :   and that prevents a central storage since access order
;;;             :   isn't guaranteed.
;;; 2018.05.08 Dan
;;;             : * Changed check-with-environment-for-visible-virtuals so
;;;             :   that it removes bad values automatically and prints one
;;;             :   warning about that instead of a warning everytime.
;;; 2018.06.22 Dan
;;;             : * Updated doc strings on remote commands.
;;; 2018.09.12 Dan
;;;             : * Only send the check notice once, and don't include a
;;;             :   useless parameter.
;;;             : * Allow a handler to indicate that it isn't going to draw
;;;             :   but should still be notified when changes occur if it is
;;;             :   drawn.
;;;             : * Keep track of the handlers in the window itself.
;;;             : * Don't monitor update-attended-loc -- vision is responsible
;;;             :   for notifying the device.  In general, the AGI shouldn't
;;;             :   assume anything about the modules in which it is installed.
;;;             : * That change means the handler is responsible for checking
;;;             :   the window bounds to determine whether or not to draw the
;;;             :   attention ring and whether to remove it or not.
;;; 2018.09.17 Dan
;;;             : * Don't need the visible-virtual-update-loc monitor.
;;;             : * Moved color->name to misc-utils.
;;;             : * Updated the add-virtual-window-handler docs to indicate
;;;             :   there's an optional parameter now.
;;; 2019.07.09 Dan
;;;             : * Fixed the bug where it warned about a visible handler
;;;             :   being removed but didn't actually remove it.
;;;             : * When adding one use string= to test not string-equal
;;;             :   because command strings are case sensitive.
;;; 2019.08.19 Dan
;;;             : * Fixed a typo in remove-virtual-window-handler because it
;;;             :   had string-= instead of string=.
;;; 2019.11.04 Dan
;;;             : * Remove display handlers as soon as they have a failed
;;;             :   call.
;;; 2020.02.21 Dan [5.0]
;;;             : * Remove the clear signal from remove-all-items-from-rpm-window
;;;             :   since that only gets called for a close and the handler
;;;             :   can assume the clear action from that...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface protocol
;;;
;;; To display the visible virtual windows the 'client' must first call
;;; the add-virtual-window-handler command providing the name of a command
;;; to call to handle the displaying of the window and items.
;;;
;;; If a client wants to stop displaying the windows it should call 
;;; remove-virtual-window-handler with the command that it added.
;;;
;;; The added command will be called with a list of parameters.  The first
;;; element in the list will be a string indicating an action.  The remaining
;;; elements will be the parameters necessary for performing that action.
;;; The possible actions are:
;;;
;;; check
;;;      It will be passed no additional elements.  This is called whenever 
;;;      a new visible-virtual window is about to be created to determine if 
;;;      there is a client which will display the window.  If a client
;;;      returns true then it will be sent an open action and all other actions
;;;      requested for that window.  If no client returns a true value then
;;;      a virtual window will be used instead and none of the handlers will
;;;      be sent any further notices for this window.
;;;
;;; open
;;;      It will be passed 5 additional elements.  The first will be the title
;;;      for the window which will be used to refer to it for any updates which
;;;      occur.  The next two will be the x and y coordinate (respectively) for
;;;      the location of the window as specified by the upper-left corner. The
;;;      last 2 will be the width and height of the window in pixels respectively.
;;;
;;; select
;;;      It will be passed one additional parameter which will be the title of
;;;      an opened window.  This is a request to make that window visible and
;;;      "bring it to the front" or whatever the appropriate terminology is for
;;;      the appropriate window manager in use.
;;;
;;; close
;;;      It will be passed one additional parameter which will be the title of
;;;      an opened window.  This is a request to close the window and remove it
;;;      from view.  No further commands will be sent for that window.
;;;
;;; button
;;;      It will be passed 8 additional parameters.  It is called to add a button
;;;      object to a window.  The first additional parameter is the title of the
;;;      window.  The second is a string with a name for this button.
;;;      The next two are the x and y position of the upper-left corner of the
;;;      button.  Then the width and height of the button are provided.  That
;;;      is followed by the text to display on the button, and finally a color
;;;      for the background of the button.
;;;
;;; text
;;;      It will be passed 7 additional parameters.  It is called to add text
;;;      to a window.  The first additional parameter is the title of the window.
;;;      The second is a string with a name for this text item.  The next 
;;;      two are the x and y position of the upper-left corner of the text.  A
;;;      string with the text to display is next.  That is followed by the color
;;;      and font size for the text.
;;;
;;; line
;;;      It will be passed 7 additional parameters.  It is called to draw a line
;;;      on a window.  The first additional parameter is the title of the window.
;;;      The second is a string with a name for this line.  The next 
;;;      two are the x and y position of one end of the line and then the x and
;;;      y position of the other end of the line is given.  The final value is
;;;      the color for the line.
;;;
;;; remove
;;;      It will be passed 2 additional parameters.  The first is the title of
;;;      a window and the second is the name of the item to be removed from the
;;;      window.
;;;
;;; attention 
;;;      It will be passed 5 additional items.  This is called to indicate the
;;;      location of a model's visual attention.  Typically a circle is drawn
;;;      around that location.  The first additional parameter is the title of an
;;;      opened window and the second is the name of a model.  The next 2 are
;;;      the x and y coordinates of the attention point, and the last is a string
;;;      indicating the name of a color in which any marker should be drawn.
;;;      The color value could be anything because it is user specifiable so
;;;      it is up to the handler to map that name to an appropriate color if
;;;      possible.  The default color, which should always be handled properly, 
;;;      is "red" and a good starting point for other colors to expect are those
;;;      which the vision module creates by default for a model: magenta, pink,
;;;      brown, dark-green, dark-red, gray, blue, purple, white, dark-cyan, 
;;;      dark-magenta, dark-blue, light-gray, dark-yellow, yellow, light-blue,
;;;      cyan, dark-gray, green, and black. 
;;;
;;; clearattention
;;;      It will be passed 2 additional items.  The first is the title of an
;;;      opened window and the second is the name of a model.  If the handler
;;;      has drawn a visual attention marker in the window for the named model
;;;      it should be removed when this is called.
;;;
;;; click
;;;      It will be passed 2 additional items.  When the model presses a button
;;;      that has been added to the window this action will be called with the
;;;      title of the window and the id which was given to the button as the
;;;      parameters respectively.  This is provided so that the handler can
;;;      adjust the display of the button in response to the click if desired,
;;;      but should not result in additional function calls because the 'action'
;;;      of the button is handled by the AGI code itself.
;;;
;;; cursor 
;;;      It will be passed 5 additional items.  When the model moves the mouse
;;;      in the window this action will be called with the title of the window,
;;;      the name of the model, the x position (local to the window), the y 
;;;      position (local to the window), and a color name which defaults to black.
;;;
;;; clearcursor 
;;;      It will be passed 2 additional items.  When the model moves the mouse
;;;      out of the window this action will be called with the title of the window
;;;      and the name of the model respectively.

;;; If the handler wants to send 'real' interactions back to the system then
;;; it can call output-key directly to provide keyboard input.  That should
;;; specify a model of "nil" to indicate it was not generated by a model,
;;; and all key names should be converted to the appropriate string for the
;;; model's keyboard for consistency e.g. the '-' key is called "minus" in
;;; Tcl/Tk (the language used for the ACT-R Environment which can be used to
;;; display visible virtual windows) which should be translated to the string
;;; "-" since that is the name that the default keyboard uses.  To send mouse
;;; clicks it should call the "visible-virtual-window-mouse-click" command
;;; providing three values: the window's title, the x position of the click,
;;; and the y position of the click where the x,y coordinate are relative to
;;; the window itself not the global screen coordinates i.e. clicking the 
;;; upper-left corner of the window would result in a 0,0 click.  It should 
;;; not perform any actions as a result of the click i.e. calling functions 
;;; for button presses (other than perhaps a local function which sends the 
;;; click notice to the system) because that should be determined by the 
;;; internal window handler code so that all handlers displaying a visible-
;;; virtual window will be updated the same way in response.
;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(declaim (ftype (function (t t t) t) click-the-mouse))

;;; virtual windows which can be sent out to 'handlers' for actual
;;; display -- thus "visible" virtual windows.
;;;
;;; Inherits much of the functionality from the rpm-virtual-window
;;; but needs to send the commands to remote handlers.

(defclass visible-virtual-window (rpm-virtual-window)
  ((window-fixation-shown :accessor window-fixation-shown :initform nil)
   (display-handlers :accessor display-handlers :initform nil :initarg :display-handlers))
  )


(defmethod visible-rpm-window ((win visible-virtual-window))
  t)

(defclass env-text-vdi (static-text-vdi)
  ((f-size :accessor f-size :initarg :font-size))
  (:default-initargs
      :id (string-downcase (symbol-name (new-symbol-fct "VVTEXT")))))

(defclass env-button-vdi (button-vdi)
  ()
  (:default-initargs
      :id (string-downcase (symbol-name (new-symbol-fct "VVBUTTON")))))

(defclass env-line-vdi (v-liner)
  ()
  (:default-initargs
      :id (string-downcase (symbol-name (new-symbol-fct "VVLINE")))))


(defstruct virtual-window-control
  (lock (bt:make-lock "Virtual Window Control"))
  handlers)

(defvar *vw-control* (make-virtual-window-control))

(defun add-virtual-window-handler (cmd &optional (displayer? t))
  (if (check-act-r-command cmd)
      (bt:with-lock-held ((virtual-window-control-lock *vw-control*))
        (push (cons cmd displayer?) (virtual-window-control-handlers *vw-control*))
        t)
    nil))

(defun remove-virtual-window-handler (cmd)
  (bt:with-lock-held ((virtual-window-control-lock *vw-control*))
    (if (find cmd (virtual-window-control-handlers *vw-control*) :test 'string= :key 'car)
        (progn
          (setf (virtual-window-control-handlers *vw-control*)
            (remove cmd (virtual-window-control-handlers *vw-control*) :test 'string= :key 'car))
          t)
      nil)))


(add-act-r-command "add-virtual-window-handler" 'add-virtual-window-handler "Add a handler to be notified of AGI visible virtual window updates. Params: interface-cmd {display-handler?}.")
(add-act-r-command "remove-virtual-window-handler" 'remove-virtual-window-handler "Remove a handler from AGI visible virtual window updates. Params: interface-cmd.")


(defmethod send-update-to-handlers ((win visible-virtual-window) params)
  (let (remove)
    (dolist (x (bt:with-recursive-lock-held ((window-lock win)) (display-handlers win)))
      (unless (evaluate-act-r-command x params)
        (push x remove)))
    (when remove
      (bt:with-recursive-lock-held ((window-lock win)) 
        (dolist (x remove)
          (setf (display-handlers win) (remove x (display-handlers win))))))))

(defun check-with-environment-for-visible-virtuals () 
  "Return whether or not the visible-virtuals are available"
  (bt:with-lock-held ((virtual-window-control-lock *vw-control*))
    (let ((display nil)(others nil))
      (dolist (x (virtual-window-control-handlers *vw-control*) (when display (append display others)))
        (if (and (stringp (car x)) (local-or-remote-function-p (car x)))
            (if (cdr x)
                (when (dispatch-apply (car x) (list "check"))
                  (push (car x) display))
              (push (car x) others))
          (progn
            (setf (virtual-window-control-handlers *vw-control*) (remove x (virtual-window-control-handlers *vw-control*)))
            (print-warning "Virtual window handler ~s removed because no longer valid." (car x))))))))

;; Window control actions

(defmethod initialize-instance :after ((win visible-virtual-window) &key)
  (bt:with-recursive-lock-held ((window-lock win))
    
    (setf (window-fixation-shown win) nil)

    (dolist (x (display-handlers win))
      (dispatch-apply x (list "open" (window-title win) (x-pos win) (y-pos win) (width win) (height win))))))
  

(defmethod select-rpm-window ((win visible-virtual-window))
  (send-update-to-handlers win (list "select" (window-title win))))

(defmethod close-rpm-window ((win visible-virtual-window))
  (call-next-method)
  (send-update-to-handlers win (list "close" (window-title win))))


;; Model interface actions

(defmethod vv-click-event-handler ((btn env-button-vdi) where)
  (declare (ignore where))
  (bt:with-recursive-lock-held ((dialog-item-lock btn))
    ;; Always send the click notice over
    (awhen (view-container btn)
           (send-update-to-handlers it (list "click" (window-title it)  (id btn)))))
  ;; let the button-vdi method do the real work ...
  (call-next-method))


(defmethod device-move-cursor-to ((vw visible-virtual-window) (loc vector))
  (bt:with-recursive-lock-held ((window-lock vw))
    (let* ((val (get-parameter-value :needs-mouse))
           (color (color->name val)))
      (if (and (> (length loc) 0) (point-in-vv-p vw loc))
          (progn
            (send-update-to-handlers vw (list "cursor" (window-title vw) (current-model) (- (px loc) (x-pos vw)) (- (py loc) (y-pos vw)) color))
            (pushnew (current-model) (cursor-drawn vw)))
        (when (find (current-model) (cursor-drawn vw))
          (send-update-to-handlers vw (list "clearcursor" (window-title vw) (current-model)))
          (setf (cursor-drawn vw) (remove (current-model) (cursor-drawn vw))))))))

       


;; Real interaction with the window

(defun visible-virtual-window-mouse-click (win-name x y)
  (let ((win (determine-exp-window (agi-component) win-name)))
    (if win
        (let ((global-x (+ x (x-pos win)))
              (global-y (+ y (y-pos win))))
          (click-the-mouse nil (list global-x global-y) 'index)
          ;(vv-handle-click win (vector global-x global-y))
          (vector global-x global-y))
      (print-warning "Mouse click reported in window ~s at ~s,~s but does not correspond to a valid window." win-name x y))))


(add-act-r-command "visible-virtual-window-mouse-click" 'visible-virtual-window-mouse-click "Report a real mouse click interaction with a visible virtual window. Params: window-title click-x-pos click-y-pos.")


(defmethod env-window-features ((item env-button-vdi))
  (list "button" (list (id item)
                       (x-pos item) (y-pos item) (width item) 
                       (height item) (dialog-item-text item) 
                       (color->name (color item)))))

(defmethod env-window-features ((item env-text-vdi))
  (list "text" (list (id item) 
                     (x-pos item) (y-pos item) 
                     (dialog-item-text item) 
                     (color->name (color item)) 
                     (round (f-size item)))))

(defmethod env-window-features ((item env-line-vdi))
  (list "line" (list (id item) 
                     (x-pos item) (y-pos item) 
                     (width item) (height item)
                     (color->name (color item)))))


;; Adding window elements 

(defmethod add-visual-items-to-rpm-window ((win visible-virtual-window) &rest items)
  (bt:with-recursive-lock-held ((window-lock win))
    (dolist (item items)
      (add-subviews win item)
      (let ((details (env-window-features item)))
        (send-update-to-handlers win (concatenate 'list (list (first details) (window-title win))
                                       (second details)))))))



(defmethod remove-visual-items-from-rpm-window ((win visible-virtual-window) &rest items)
  (bt:with-recursive-lock-held ((window-lock win))
    (dolist (item items)
      (remove-subviews win item)
      (send-update-to-handlers win (list "remove" (window-title win) (id item))))))


(defmethod remove-all-items-from-rpm-window ((win visible-virtual-window))
  (call-next-method)
 ; (send-update-to-handlers win (list "clear" (window-title win)))
  )



;; Creating the underlying objects

(defmethod make-button-for-rpm-window ((win visible-virtual-window) 
                                       &key (x 0) (y 0) (text "Ok")  
                                       (action nil) (height 18) (width 60) (color 'gray))
  (make-instance 'env-button-vdi
    :x-pos x 
    :y-pos y
    :dialog-item-text text
    :action action
    :height height
    :width width
    :color color
    :lock (window-lock win)))

  
(defmethod make-static-text-for-rpm-window ((win visible-virtual-window) 
                                            &key (x 0) (y 0) (text "") 
                                            (height 20) (width 80) (color 'black)
                                            font-size)
  (unless (numberp font-size)
    (setf font-size 12))
  (make-instance 'env-text-vdi
    :x-pos x 
    :y-pos y
    :dialog-item-text text
    :height height
    :width width
    :color color
    :font-size font-size
    :text-height (round font-size 12/10)
    :str-width-fct (let ((w (round font-size 12/7))) (lambda (str) (* (length str) w)))
    :lock (window-lock win)))


(defmethod make-line-for-rpm-window ((win visible-virtual-window) 
                                     start-pt end-pt &optional (color 'black))
  (make-instance 'env-line-vdi
    :color color
    :x-pos (first start-pt)
    :y-pos (second start-pt)
    :width (first end-pt)
    :height (second end-pt)
    :lock (window-lock win)))


;;; modify items


(defmethod modify-text-for-rpm-window ((text-item env-text-vdi) &key &allow-other-keys)
  "Modify a text item for the rpm window"
  (call-next-method)
  (bt:with-recursive-lock-held ((dialog-item-lock text-item))
    (awhen (view-container text-item) ;; it's on the screen
           ;; simple approach for the remote handlers -- remove it then add it back
           (remove-visual-items-from-rpm-window it text-item)
           (add-visual-items-to-rpm-window it text-item))
    text-item))


(defmethod modify-line-for-rpm-window ((line env-line-vdi) start-pt end-pt &key &allow-other-keys)
  (declare (ignorable start-pt end-pt))
  (call-next-method)
  (bt:with-recursive-lock-held ((dialog-item-lock line))
    (awhen (view-container line) ;; it's on the screen
           ;; simple approach for the remote handlers -- remove it then add it back
           (remove-visual-items-from-rpm-window it line)
           (add-visual-items-to-rpm-window it line))
    line))


(defmethod modify-button-for-rpm-window ((button-item env-button-vdi) &key &allow-other-keys)
  "Modify a button item for the rpm window"
  (call-next-method)
  (bt:with-recursive-lock-held ((dialog-item-lock button-item))
    (awhen (view-container button-item) ;; it's on the screen
           ;; simple approach for the remote handlers -- remove it then add it back
           (remove-visual-items-from-rpm-window it button-item)
           (add-visual-items-to-rpm-window it button-item))
    button-item))

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
