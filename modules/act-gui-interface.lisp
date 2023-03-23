;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2017 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : act-gui-interface.lisp
;;; Version     : 6.0
;;; 
;;; Description : Contains the functions that implement the abstract GUI
;;;             : interface used by the tutorial units and the misc functions
;;;             : that go with them (correlation and mean-deviation).  
;;;             : I'm calling it the ACT-R GUI interface (AGI) as suggested by Mike.
;;;             : It relies on the old UWI to handle the "real" interface to any
;;;             : particular windowing environment.
;;; Bugs        : 
;;; To Do       : [x] Consider making it support multiple interfaces to go with
;;;             :     multiple models.
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Renamed this file from uniform-interface-exp to 
;;;             : act-gui-interface.
;;;             : Added comments.
;;; 2002.12.17 Dan
;;;             : Modified correlation and mean-deviation so that
;;;             : the output keyword parameter is "more useful" -
;;;             : specifying a stream works right now (it doesn't try to
;;;             : open a file for it) and specifying nil suppress
;;;             : all output.
;;; 2002.12.19 Dan
;;;             : Updated add-text-to-exp-window so that it now includes
;;;             : a color option.
;;; 04.04.13 Dan [2.2]  (previous two changes also "new" as of 2.2)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : reset version to 1.0a1
;;;             : added the packaging switches
;;;             : changed permute-list to use act-r-random
;;;
;;; 04.12.17 Dan
;;;             : Added get-time as a replacement for pm-get-time.
;;;   
;;; 2005.02.25 Dan
;;;             : * Removed the ~\newline usages because that causes problems
;;;             :   when a Lisp only wants to see native new lines there.
;;; 2006.09.07 Dan
;;;             : * Changed permute-list so that it's safe when passed nil or
;;;             :   a non-list.
;;; 2007.07.13 Dan
;;;             : * Added color as an option to add-button-to-exp-window.
;;; 2007.12.13 Dan
;;;             : * Adding an add-items-to-exp-window function to compliment
;;;             :   the remove-... and to avoid the need to call the UWI
;;;             :   function when such an action is necessary.
;;; 2009.09.10 Dan
;;;             : * Moved permute-list to the random module's file.
;;; 2010.03.08 Dan
;;;             : * Changed close-exp-window so that it checks to see if the
;;;             :   window is still there before trying to close it.  Avoids
;;;             :   a problem where a user has closed an environment side
;;;             :   window.
;;; 2010.07.31 Dan
;;;             : * Changed add-line-to-exp-window to constrain the x,y positions
;;;             :   to be fixnums to avoid problems in ACL and LispWorks if 
;;;             :   a float or rational is used instead.
;;; 2011.04.26 Dan
;;;             : * Changed get-time to just return mp-time-ms.
;;; 2011.11.14 Dan
;;;             : * Make open-exp-window set the visual center to the center of
;;;             :   the window.
;;; 2011.11.21 Dan
;;;             : * Get-time changed to not use *actr-enabled-p*.  Instead
;;;             :   it uses an optional parameter to determine whether or not 
;;;             :   to use mp-time.
;;;             :   If the optional parameter is provided then a true value
;;;             :   means model time and nil means internal-real-time.  If 
;;;             :   no parameter is provided then model time will be returned.
;;; 2012.06.08 Dan [2.0a1]
;;;             : * Use a module to maintain the information for the open
;;;             :   window(s) and allow multiple open windows for a model.
;;;             :   Multiple models may have the same titled window open and
;;;             :   that should make it "easy" to have multiple models use the
;;;             :   same code to provide separate interfaces.  The real window's
;;;             :   title (which the model can't see anyway) will include the 
;;;             :   model's name to make things easier for the modeler.
;;;             : * The significant changes to the commands are:
;;;             :   - open-exp-window doesn't automatically close a window with
;;;             :     a different title when it creates a new one.   It does
;;;             :     still clear and bring to the front a window by the same
;;;             :     title within the same model.
;;;             :   - Open-exp-window will only create a window if there is a
;;;             :     current model.  The main reason for that is because it 
;;;             :     avoids a lot of hassle of dealing with "unowned"
;;;             :     windows.  It also makes things much easier with respect
;;;             :     to displaying multiple visible virtual windows through
;;;             :     the ACT-R Environment.
;;;             :   - All windows now automatically close when the owning model
;;;             :     is deleted (either explicitly or via clear-all) and if
;;;             :     the new system parameter :close-exp-windows-on-reset is set
;;;             :     it will also close them when the owning model is reset.
;;;             :     [I'd prefer the default for :close-exp-windows-on-reset
;;;             :     to be t, but unfortunately because of how the single
;;;             :     experiment window code worked doing so would likely break 
;;;             :     lots of existing models (since some in the tutorial models
;;;             :     used it in a manner that assumed the window persists).]
;;;             :   - Most of the commands now have an additional keyword 
;;;             :     parameter :window which can be used to indicate which
;;;             :     window to perform the action upon.  If it is not provided
;;;             :     and there is only one window open in the current model
;;;             :     the command will be applied to that window, otherwise it
;;;             :     will result in a warning and no action performed.
;;;             :     The window value can be specified as either the window 
;;;             :     object returned by open-exp-window or the title which
;;;             :     was specified for open-exp-window.  When specifying a title
;;;             :     as the :window parameter that will be relative to the current
;;;             :     model since different models can have windows with the same
;;;             :     title.
;;;             :   - For add-line-to-exp-window the color is now a keyword
;;;             :     parameter instead of optional so that it isn't confusing 
;;;             :     when also specifying a window.  This change is the only one
;;;             :     that's not backward compatible with the previous "single
;;;             :     window" usage and will require changing existing models.
;;; 2012.07.02 Dan
;;;             : * Open-exp-window now calls device-update-attended-loc with a
;;;             :   nil xy-loc when it reuses a window so that the attended marker
;;;             :   gets cleared.
;;; 2012.07.12 Dan
;;;             : * Added the exp-window-owner function because there are times
;;;             :   when it would be useful to know that.
;;; 2012.12.12 Dan
;;;             : * Adding a check to make sure that the text is a string in
;;;             :   add-text-to-exp-window.
;;; 2013.09.30 Dan [2.0]
;;;             : * Time to remove the a1 from the version.
;;; 2015.05.26 Dan
;;;             : * Added a :font-size parameter to add-text-to-exp-window.
;;; 2016.06.07 Dan [3.0]
;;;             : * Starting work to add create and modify actions for each of 
;;;             :   default item types.  Create is already supported by the UWI, 
;;;             :   and is being implemented first.
;;;             : * Added a check of the text for a button to make sure it's a
;;;             :   string.
;;; 2016.06.08 Dan
;;;             : * Adding a :class parameter to open-exp-window for passing off
;;;             :   to make-rpm-window to allow for easier extensions while I'm
;;;             :   in here.
;;;             : * Added the front AGI modify functions which call out to UWI
;;;             :   modify functions.
;;; 2016.06.09 Dan
;;;             : * Fixed a bug with the use of flatten and push.
;;;             : * Fixed an issue with create-line actually adding it to the 
;;;             :   display as well.
;;; 2016.11.17 Dan
;;;             : * Use current-mp instead of current-meta-process when creating
;;;             :   the table key -- don't need that at all with this version 
;;;             :   since only a single meta-process is supported, but for now
;;;             :   keeping the changes minimal.
;;; 2017.01.04 Dan [4.0]
;;;             : * Only the virtual and visible-virtual windows are used now.
;;;             : * Don't return the window object, but a list of module interface
;;;             :   items for use with the new install-device function.
;;;             : * Make open-exp-window available through the dispatcher with
;;;             :   optional parameters instead of keywords.
;;;             : * Make get-time available through dispatcher.
;;; 2017.01.05 Dan
;;;             : * Added the call to create a device (under the new definition
;;;             :   of what a device is) and assuming now that it's building a
;;;             :   subtype of virtual window underneath.
;;; 2017.01.10 Dan
;;;             : * BIG ISSUE -- right now this code is built around the notion
;;;             :   that the owning model is the only model which could install
;;;             :   the device.  That needs to be corrected, but there isn't a
;;;             :   viable solution for "current-model" in the dispatch environment
;;;             :   yet since one can't just wrap a macro around a set of rpc calls...
;;;             :   The init and remove device functions would probably need to
;;;             :   have the model passed in I think, as well as other things.
;;; 2017.01.12 Dan
;;;             : * Added a remote version of clear-exp-window.
;;;             : * Removed the clear on reset parameter -- they will always be
;;;             :   closed upon reset.
;;; 2017.01.18 Dan
;;;             : * Removed the echo-act-r-output from commands.  The assumption
;;;             :   now is that echoing is handled by the user.
;;;             : * Use handle-evaluate-results.
;;; 2017.01.20 Dan
;;;             : * Automatically install a keyboard device for motor if there
;;;             :   isn't one when a window is opened.
;;; 2017.01.26 Dan
;;;             : * Fix the call that adds the correlation function since it was
;;;             :   cut and pasted from mean-deviation and still said that.
;;; 2017.01.30 Dan
;;;             : * Change the 'local' versions of the commands so that they have
;;;             :   the same signature as the remote ones.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.08 Dan
;;;             : * Reworked the local versions of the remote commands to not
;;;             :   go out through the dispatcher.
;;;             : * Fixed some bugs with the last update.
;;; 2017.02.09 Dan
;;;             : * Add remote versions of create and add for lines.
;;;             : * Convert strings for colors to symbols in text and line functions.
;;; 2017.02.24 Dan
;;;             : * Test the parameters given to open-exp-window because this
;;;             :   (open-exp-window "" :visible t) construct from before causes
;;;             :   problems now.
;;; 2017.03.02 Dan
;;;             : * Moved the installation of a keyboard and mouse from the
;;;             :   opening of the window to the installing.
;;;             : * Updated and added the button create and add commands.
;;; 2017.03.03 Dan
;;;             : * Creating a button checks the action function for a valid
;;;             :   value at creation time now.
;;;             : * When an experiment window is installed it also installs
;;;             :   a mouse for itself (removing a previously installed mouse),
;;;             :   and it now provides a signal-interface so that the mouse
;;;             :   can send move and click notices directly instead of having
;;;             :   to catch them with a monitor.
;;; 2017.03.08 Dan
;;;             : * Removing an exp-window needs to remove all the visual
;;;             :   features it has added because can't rely on vision doing so
;;;             :   since it doesn't know who 'owns' them and can't just kill
;;;             :   all of them when the device is removed.
;;; 2017.03.09 Dan
;;;             : * Updated the modify-line-for-exp-window command and made it
;;;             :   available externally.
;;; 2017.03.10 Dan
;;;             : * Convert a string color in modify-line-for-exp-window to a
;;;             :   symbol if possible, and check if the line is actually in the
;;;             :   window before asking vision to modify the visicon features.
;;; 2017.03.16 Dan
;;;             : * Comment out most of modify-text-for-exp-window because it
;;;             :   doesn't work yet and want to avoid the compiler warnings.
;;; 2017.05.31 Dan [5.0]
;;;             : * As a first step to dealing with the model issue this is now
;;;             :   being implemented as a component instead of a module -- that
;;;             :   removes the need for a current model and should provide a
;;;             :   path for multiple models to use the same device.  
;;; 2017.06.08 Dan 
;;;             : * Fixed modify-line-for-exp-window so that it works right with
;;;             :   respect to updating all the models that have the window installed.
;;; 2017.06.13 Dan
;;;             : * The agi module itself needs a lock to protect access to its
;;;             :   internal components.
;;; 2017.06.14 Dan
;;;             : * The remove-exp-window function needs to return true for the
;;;             :   remove-device command to work.
;;;             : * Reorganized some locking in open and close because open can
;;;             :   call close and close may call remove-exp-window which all
;;;             :   need the agi lock, but remove-exp-window will be in a 
;;;             :   different thread since it is called through the dispatcher.
;;; 2017.06.16 Dan
;;;             : * Make the agi-lock directly instead of using an act-r-lock.
;;; 2017.06.23 Dan
;;;             : * Added a name for the lock.
;;; 2017.06.30 Dan
;;;             : * Got rid of the data structure since it's not used anywhere.
;;; 2017.07.13 Dan
;;;             : * Add some error detection to the file opening in mean-deviation
;;;             :   and correlation and pass output to act-r-output if there
;;;             :   isn't a current model.
;;; 2017.08.16 Dan
;;;             : * Fixed an issue with modify-line-for-exp-window because 
;;;             :   determine-exp-window doesn't really do what it's named if
;;;             :   a sub-view is passed to it.
;;; 2017.08.17 Dan
;;;             : * Fixed an issue with close-exp-window because it held the
;;;             :   agi lock while remove-device was called but that can dispatch
;;;             :   things that might need that lock through the remove function
;;;             :   of the device.
;;; 2017.09.13 Dan
;;;             : * Fixed close-all-exp-windows to use the the window list 
;;;             :   instead of the global table since that contains more than
;;;             :   windows.
;;; 2017.11.01 Dan
;;;             : * Removed the extra new-line in the correlation and mean-deviation
;;;             :   output when using command-output or act-r-output.
;;; 2017.11.16 Dan
;;;             : * Fixed a bug with valid-button-action because it looped forever
;;;             :   with nil.
;;; 2018.02.08 Dan
;;;             : * Fixed a bug with remove-items-from-exp-window and clear-exp-
;;;             :   window because they didn't remove the underlying visual features
;;;             :   from the dialog item.  They just removed the chunk name for 
;;;             :   the feature.
;;;             : * Remove-exp-window doesn't need to inform the vision module
;;;             :   about features being removed if the window isn't actually
;;;             :   installed (which can happen because of a reset while it was
;;;             :   installed -- the reset still needs to clear the features from
;;;             :   the underlying window structures but not the visicon).
;;; 2018.03.05 Dan
;;;             : * Install a microphone for the speech module if there isn't
;;;             :   already one installed.
;;; 2018.03.13 Dan
;;;             : * All coordinates must be global now because multiple windows
;;;             :   are allowed and a single mouse is used for all of them which
;;;             :   has global coordinates.
;;;             : * Because a model can have multiple windows the installed-windows
;;;             :   needs to be a table instead of just an alist.
;;; 2018.03.14 Dan
;;;             : * Opening the window installs the monitors for the signals that
;;;             :   matter (click-mouse, move-cursor, delete-cursor, and update-
;;;             :   attended-loc) and closing the window removes the monitors.
;;; 2018.03.27 Dan
;;;             : * Moved find-top-level-window to the uwi file.
;;; 2018.04.27 Dan
;;;             : * Fixed modify-text-for-exp-window and added a remote version.
;;;             : * Same for modify-button-for-exp-window.
;;; 2018.05.01 Dan
;;;             : * Fixed a problem with the modify actions when the item hadn't
;;;             :   been added to a window or didn't have visicon features yet.
;;; 2018.05.02 Dan
;;;             : * Fixed a bug with close-all-exp-windows since it held the
;;;             :   agi lock but that's needed to remove an installed window
;;;             :   when it's closed.
;;; 2018.05.03 Dan
;;;             : * Updated the modify actions to be consistent with the new
;;;             :   modify-visicon-features definition.
;;; 2018.06.13 Dan
;;;             : * Added a declaim to avoid a warning for set-cursor-position-fct.
;;; 2018.06.21 Dan
;;;             : * Return to keyword params!  Use options lists externally.
;;;             : * Don't allow a symbol for window title.
;;; 2018.06.26 Dan
;;;             : * Fixed a bug with adding the command for open-exp-window.
;;; 2018.09.13 Dan
;;;             : * Now the device has a notification function which can be used
;;;             :   to send messages to the real window handlers.  A notification
;;;             :   should be provided a list where the first item is a string
;;;             :   and the remaining items are additional parameters.  It will
;;;             :   then send the handler a command with that string, the window
;;;             :   title, current model, and then the additional parameters.
;;; 2018.11.15 Dan
;;;             : * Modify-button-for-exp-window command wasn't calling the
;;;             :   external version.
;;;             : * Bug in check for whether a button modification was valid.
;;; 2018.11.19 Dan
;;;             : * Really fixed the button modification tests now.
;;; 2019.06.19 Dan
;;;             : * Adjust close-exp-window because LispWorks implicitly locks
;;;             :   hash-tables between threads which means a maphash can't
;;;             :   call something in another thread that accesses the table.
;;; 2019.10.01 Dan
;;;             : * Fixed the typo in the monitor-move-cursor accessor.
;;; 2020.06.02 Dan
;;;             : * Removed an unnecessary with-recursive-lock-held from 
;;;             :   add-items-to-exp-window because it already had the lock.
;;; 2021.10.20 Dan [6.0]
;;;             : * Have it check before installing devices to make sure that
;;;             :   the corresponding module is avaliable.
;;; 2022.02.18 Dan 
;;;             : * Fixed a bug with modify-button-for-exp-window in how it
;;;             :   handles the modifications for passing to the underlying
;;;             :   button object since the action could be a list of things.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(declaim (ftype (function (t &optional t) t) set-cursor-position-fct))


;;; Class for the AGI component instance.  
;;; Have central tables to hold windows and subcomponents, make the windows
;;; available as a device, and track which models have installed which 
;;; window so when a window or item is changed or closed all the models can
;;; have their visicons updated.
;;; Assumes that it's an rpm virtual window (or the visible-virtual subclass).
;;; That assumption can also be satisfied by the subviews and window-lock methods
;;; being defined for the window object and all the items returned from subviews
;;; have x-pos, y-pos, and visual-features accessor/methods to return and store
;;; values and device-move-cursor-to and vv-handle-click method for responding 
;;; to mouse actions.
;;; Also assumes that the window-lock protects all of the subviews too.

(defclass agi-component ()
   ;; the global table maps a window or item identifier to the underlying object
  ((global-agi-table :accessor global-agi-table :initform (make-hash-table :test 'equalp))
   ;; table of created objects for a window -- list of actual objects
   (window-objects-table :accessor window-objects-table :initform (make-hash-table :test 'equalp))
   ;; table of objects added to a window -- list of actual objects
   (window-added-objects-table :accessor window-added-objects-table :initform (make-hash-table :test 'equalp))
   
   ;; the list of window titles
   (agi-window-list :accessor agi-window-list :initform nil)
   
   ;; table of installed windows by model
   (installed-windows :accessor installed-windows :initform (make-hash-table))
   
   ;; protect the tables
   (agi-lock :accessor agi-lock :initform (bt:make-recursive-lock "agi"))))


;;; Since there's only one component, for now keeping things simple and
;;; creating a clozure around a single agi-component object to use.

(declaim (ftype (function () t) agi-component))

(let ((agi (make-instance 'agi-component)))
  (defun agi-component () agi))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for defining the component itself.
;;;
;;; Assmumes that the interface functions are called from something that's 
;;; thread safe.

;;; Here's all I need for the component -- creation, reset, and delete 
;;; functions to initialize and close things as needed.

;;; Create just needs to return a new instance of the class.

(defun create-agi-module ()
  (agi-component))

;;; Delete just closes all of the model's windows, but doesn't need to clear the
;;; module list since the module is going away.

(defun delete-agi-module (instance)
  (dolist (x (bt:with-recursive-lock-held ((agi-lock instance)) (agi-window-list instance)))
    (close-exp-window x)))

;;; clear-all may need to close the windows (like delete) and also needs to
;;; clean up the model's list by setting it to nil to make sure
;;; things are synchronized since the delete method doesn't need
;;; to do so.

(defun clear-all-agi-module (instance)
  (reset-agi-module instance)
  (delete-agi-module instance)
  (bt:with-recursive-lock-held ((agi-lock instance))
    (setf (agi-window-list instance) nil)))

(defun delete-model-agi-module (instance model-name)
  (awhen (gethash model-name (bt:with-recursive-lock-held ((agi-lock instance)) (installed-windows instance)))
         (with-model-eval model-name
           (dolist (w it)
             (remove-device (list "vision" "exp-window" w))))))

(defun reset-agi-module (instance)
  (bt:with-recursive-lock-held ((agi-lock instance))
    (clrhash (installed-windows instance))))

(define-component agi :version "6.0" :documentation "Manager for the AGI windows"
  :creation create-agi-module
  :clear-all clear-all-agi-module
  :delete delete-agi-module
  :delete-model delete-model-agi-module
  :before-reset reset-agi-module
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create a device called "exp-window".
;;; Interface functions are all dispatched and single instance.


;;; The window identifier is the value passed in from the device installation 
;;; and removal.  It's the third value of the list returned from open-exp-window
;;; and is just the window title string.  This function maps that to the real
;;; window object from the table.

(defun window-identifier-to-window (agi id)
  (bt:with-recursive-lock-held ((agi-lock agi))
    (gethash id (global-agi-table agi))))


(defun init-exp-window (device-list)
  (let* ((agi (agi-component))
         (interface (first device-list))
         (win-identifier (third device-list))
         (w (window-identifier-to-window agi win-identifier)))
    (if (not (string-equal interface "vision"))
        (print-warning "Experiment windows can only be installed for the vision interface, but ~a given." interface)
      (cond (w ;; must be a new install since install-device blocks dups.
             
             ;; Record the model as installed for the window
             
             (bt:with-recursive-lock-held ((agi-lock agi))
               (push win-identifier (gethash (current-model) (installed-windows agi))))
             
             
             (when (get-module-fct :motor t)
               (unless (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
                 (install-device (list "motor" "keyboard"))))
             
             (when (get-module-fct :speech t)
               (unless (find "microphone" (current-devices "speech") :key 'second :test 'string-equal)
                 (install-device (list "speech" "microphone"))))
             
             (when (get-parameter-value :needs-mouse)
               (when (get-module-fct :motor t)
                 (unless (find-if (lambda (x) (string-equal (third x) "mouse") (string-equal (second x) "cursor")) (current-devices "motor"))
                   (install-device (list "motor" "cursor" "mouse"))
                   (set-cursor-position-fct (list (x-pos w) (y-pos w)) "mouse")))
               
               (when (get-module-fct :vision t)
                 (when (get-parameter-value :process-cursor)
                   (unless (find-if (lambda (x) (string-equal (third x) "mouse") (string-equal (second x) "cursor")) (current-devices "vision"))
                     (install-device (list "vision" "cursor" "mouse"))))))

                                             
             ;; Create the features for all subviews, add them to the visicon, and record them in the
             ;; visual-features for the item.  Assume that items aren't already in the visicon since
             ;; the window is just being installed now.
             
             
             ;; Probably want to change this since there's only one for vision in general...
             (set-visual-center-point (+ (x-pos w) (round (width w) 2)) (+ (y-pos w) (round (height w) 2)))
               
             (dolist (x (if (stable-names (current-device-interface))
                            (sort (subviews w) #'subview-sort)
                          (subviews w)))
               (let ((feat-list nil))
                 
                 (dolist (y (build-vis-locs-for x))
                   (push (cons (car (add-visicon-features y)) y) feat-list))
                 
                 (push (cons (current-model) feat-list) (visual-features x))))
             
                                       
             t)
          (t 
           (print-warning "Window ~s could not be found when being installed." win-identifier))))))


(defun remove-exp-window (device-list)
  (let* ((agi (agi-component))
         (win-identifier (third device-list))
         (w (window-identifier-to-window agi win-identifier)))
    
    ;; assume it's good since remove requires it was installed
    
    (when w
      (let ((installed (bt:with-recursive-lock-held ((agi-lock agi))
                         (find win-identifier (gethash (current-model) (installed-windows agi)) :test 'string-equal))))
     
        ;;; need to explicitly remove the visiual features
        ;;; since vision doesn't know who 'owns' features and
        ;;; can't just delete everything when a device is 
        ;;; uninstalled.
        ;;;
        ;;; However, that's only necessary if the window is actually
        ;;; installed.  Upon a reset we want to clear out the old features,
        ;;; but since the window isn't installed at that point we don't
        ;;; need to inform the vision module.
        
        (bt:with-recursive-lock-held ((window-lock w))
          (dolist (x (subviews w))
            (when installed
              (dolist (y (cdr (assoc (current-model) (visual-features x))))
                (delete-visicon-features (car y))))
            (setf (visual-features x) (remove (current-model) (visual-features x) :key 'car))))
      
        (bt:with-recursive-lock-held ((agi-lock agi))
          (setf (gethash (current-model) (installed-windows agi)) 
            (remove win-identifier (gethash (current-model) (installed-windows agi)) :test 'string-equal)))))
    
    t))


(defun notify-exp-window (device features)
  (aif (determine-exp-window (agi-component) device)
       (cond ((and (listp features) (stringp (first features)))
              (send-update-to-handlers it (append (list (first features)) (list (window-title it) (current-model)) (rest features))))
             ((not (listp features))
              (print-warning "Exp window notification for ~s must be a list but given ~s" device features))
             (t
              (print-warning "Exp window notification for ~s must be have a string as the first feature but given ~s" device features)))
       (print-warning "Could not notify a window with device list ~s" device)))
  

(add-act-r-command "init-exp-window" 'init-exp-window "Internal command for experiment windows being installed. Do not call.")

(add-act-r-command "remove-exp-window" 'remove-exp-window "Internal command for experiment windows being uninstalled. Do not call.")

(add-act-r-command "notify-exp-window" 'notify-exp-window "Internal command for notifying an experiment window device.  Do not call.")


(define-device "exp-window" "init-exp-window" "remove-exp-window" "notify-exp-window")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to provide the AGI windowing functions.
;;; 


;;; OPEN-EXP-WINDOW  [Function]
;;; Description : This function opens a window with the properties specified and
;;;             : returns a list of items which can be passed to install-device.
;;;             :
;;;             : If there's already a window with that title open close it first,
;;;             : then create a new window with the provided details.
;;;             : In either case the window is brough to the front (assuming that
;;;             : the UWI select-rpm-window command works properly for the window
;;;             : type).
;;;             : 
;;;             : The return value is a device list suitable for install-device and can
;;;             : also be use to reference the window.

(defun open-exp-window (title &key (visible t) (width 300) (height 300) (x 300) (y 300))
  
 ; don't allow any more
 ; (when (symbolp title)
 ;   (setf title (symbol-name title)))
  
  (if (stringp title)
      (if (and (numberp width) (numberp height) (numberp x) (numberp y))
          (let ((instance (agi-component))
                (key title))
            
            (let ((exists (bt:with-recursive-lock-held ((agi-lock instance))
                            (gethash key (global-agi-table instance)))))
              
              (when exists
                (close-exp-window key))
              
              (bt:with-recursive-lock-held ((agi-lock instance))
                (let ((win (make-rpm-window :visible visible 
                                            :title title
                                            :width width 
                                            :height height
                                            :x x
                                            :y y)))
                  (pushnew title (agi-window-list instance) :test 'string-equal)
                  (setf (gethash key (global-agi-table instance)) win)
                  
                  ;; Add the monitors necessary for the window to
                  ;; respond to mouse updates.
                  ;; Attention updates only handled by visible and added in the
                  ;; init for that class.
                    
                  (bt:with-lock-held ((window-lock win))
                    (let ((name (get-new-command-name "agi-win-click-mouse-monitor")))
                      (add-act-r-command name (lambda (model pos finger)
                                                (declare (ignore finger))
                                                (exp-window-mouse-click-handler instance win key model pos))
                                         "Experiment window command for monitoring click-mouse signals.  Do not call directly." nil)
                      (setf (monitor-click-mouse win) name)
                      (monitor-act-r-command "click-mouse" name))
                    
                    (let ((name (get-new-command-name "agi-win-move-cursor-monitor")))
                      (add-act-r-command name (lambda (model cursor loc)
                                                (exp-window-move-cursor-handler instance win key model cursor loc))
                                         "Experiment window command for monitoring move-cursor signals.  Do not call directly." nil)
                      (setf (monitor-move-cursor win) name)
                      (monitor-act-r-command "move-cursor" name))
                    
                    (let ((name (get-new-command-name "agi-win-delete-cursor-monitor")))
                      (add-act-r-command name (lambda (model cursor)
                                                (exp-window-delete-cursor-handler instance win key model cursor))
                                         "Experiment window command for monitoring delete-cursor signals.  Do not call directly." nil)
                      (setf (monitor-delete-cursor win) name)
                      (monitor-act-r-command "delete-cursor" name))
                    )
                    
                  (select-rpm-window win)
                  
                  (list "vision" "exp-window" key)))))
        (print-warning "Cannot create an experiment window with value ~s for the ~s parameter."
                       (cond ((not (numberp width)) width)
                             ((not (numberp height)) height)
                             ((not (numberp x)) x)
                             ((not (numberp y)) y))
                       (cond ((not (numberp width)) 'width)
                             ((not (numberp height)) 'height)
                             ((not (numberp x)) 'x)
                             ((not (numberp y)) 'y))))
    (print-warning "Experiment window title must be a string, but ~s was specified." title)))


(defun exp-window-mouse-click-handler (instance win key model pos)
  (if model
      ;; check if this window is currently installed
      ;; and only handle the click if it is
      (when (find key (bt:with-recursive-lock-held ((agi-lock instance)) 
                        (gethash model (installed-windows instance))) 
                  :test 'string-equal)
        (vv-handle-click win (coerce pos 'vector)))
    
    ;; no model means a person which just has to be passed on
    (vv-handle-click win (coerce pos 'vector))))

(defun exp-window-move-cursor-handler (instance win key model cursor loc)
  (when (and (string-equal cursor "mouse")
             (find key (bt:with-recursive-lock-held ((agi-lock instance)) 
                         (gethash model (installed-windows instance))) 
                   :test 'string-equal))
    ;; a mouse and this model still has this window installed
    (device-move-cursor-to win (coerce loc 'vector))))


(defun exp-window-delete-cursor-handler (instance win key model cursor)
  (when (and (string-equal cursor "mouse")
             (find key (bt:with-recursive-lock-held ((agi-lock instance)) 
                         (gethash model (installed-windows instance)))
                   :test 'string-equal))
    ;; a mouse and this model still has this window installed
    (device-move-cursor-to win (vector))))



(defun external-open-exp-window (title &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'open-exp-window '(:visible :x :y :height :width))
    (when valid 
      (apply 'open-exp-window title ol))))


(add-act-r-command "open-exp-window" 'external-open-exp-window "Create a window for a task. Params: title {< visible, width, height, x, y >}")


;;; Internal function for mapping a user provided window reference: device-list, title, or nil
;;; to some real window object if possible.
;;; Could also be a particular item -- 

(defun determine-exp-window (instance window)
  ;; if it's the device identifier list
  (aif (and (listp window) (= (length window) 3)
            (string-equal (first window) "vision")
            (string-equal (second window) "exp-window")
            (window-identifier-to-window instance (third window)))
       (values it (third window))
       (aif (window-identifier-to-window instance window)
            (values it window)
            (cond ((and window (stringp window) instance)
                   (print-warning "~s is not the title of an open window." window))
                  (window
                   (print-warning "~s is not a reference to an open window." window))
                  (t ;; window is nil
                   (if instance
                       (bt:with-recursive-lock-held ((agi-lock instance))
                         (if (= (length (agi-window-list instance)) 1)
                             (values (window-identifier-to-window instance (first (agi-window-list instance)))
                                     (first (agi-window-list instance)))
                           (print-warning "There is ~:[no~;more than one~] window to use in the AGI so a nil reference will not work." (agi-window-list instance))))
                     (print-warning "There is no AGI component available to interact with experiment windows.")))))))




;;; CLOSE-EXP-WINDOW  [Function]
;;; Description : Closes an experiment window.  The window can be specified
;;;             : by title or the install list.
;;;             : If no valid window can be found it will print a warning and return nil.

(defun close-exp-window (&optional (window nil))
  "Close the experiment window"
  
  ; don't allow symbols 
  ; (when (and window (symbolp window))
  ;   (setf window (symbol-name window))) 
  
  (let ((instance (agi-component)))
    (multiple-value-bind (wind key) (determine-exp-window instance window)
      (if wind
          ;; for every model in which it is installed it must be removed
          (let (to-remove) 
            
            
            (maphash (lambda (model windows)
                       (when (find key windows :test 'string-equal)
                         (push (cons model windows) to-remove)))
                     (bt:with-recursive-lock-held ((agi-lock instance)) (installed-windows instance)))
            
            (mapcar (lambda (x)
                      (let ((model (car x))
                            (windows (cdr x)))
                        (with-model-eval model
                          (remove-device (list "vision" "exp-window" key)))
                        
                        (bt:with-recursive-lock-held ((agi-lock instance))
                          (setf (gethash model (installed-windows instance)) (remove key windows :test 'string-equal)))))
              to-remove)
            
            
            
            ;; Close the window 
            (close-rpm-window wind)
            
            
            ;; remove all the monitors it created
            
            (bt:with-lock-held ((window-lock wind))
                    
              (when (monitor-attended-loc wind)
                (remove-act-r-command-monitor "update-attended-loc" (monitor-attended-loc wind))
                (remove-act-r-command (monitor-attended-loc wind)))
              
              (when (monitor-click-mouse wind)
                (remove-act-r-command-monitor "click-mouse" (monitor-click-mouse wind))
                (remove-act-r-command (monitor-click-mouse wind)))
              
              (when (monitor-move-cursor wind)
                (remove-act-r-command-monitor "move-cursor" (monitor-move-cursor wind))
                (remove-act-r-command (monitor-move-cursor wind)))
              
              (when (monitor-delete-cursor wind)
                (remove-act-r-command-monitor "delete-cursor" (monitor-delete-cursor wind))
                (remove-act-r-command (monitor-delete-cursor wind))))
            
            ;; remove it from the global table
            (bt:with-recursive-lock-held ((agi-lock instance))
              
              (remhash key (global-agi-table instance))
              
              ;; remove it from list of windows 
              (setf (agi-window-list instance) (remove key (agi-window-list instance) :test 'string-equal))
            
              
              ;; Remove all of its created items from the global table
              (dolist (x (gethash key (window-objects-table instance)))
                (remhash (list key (string (id x))) (global-agi-table instance)))
              
              ;; clear the created and added objects entries
              
              (remhash key (window-objects-table instance))
              (remhash key (window-added-objects-table instance))
              
              t))
      (print-warning "Could not close window ~s" window)))))


(add-act-r-command "close-exp-window" 'close-exp-window "Close the indicated experiment window or if none provided the only open one. Params: {window}")

(defun close-all-exp-windows ()
  (let ((agi (agi-component)))
    (mapcar (lambda (win)
              (close-exp-window win))
      (bt:with-recursive-lock-held ((agi-lock agi)) (agi-window-list agi)))))


(add-act-r-command "close-all-exp-windows" 'close-all-exp-windows "Close all the currently open experiment windows. No params.")

       
;;; SELECT-EXP-WINDOW  [Function]
;;; Description : Brings a window to the front.  The window can be specified
;;;             : by title in current model or the install list.
;;;             : If no window is provided then if there is only one window or only
;;;             : one window in the current model that window will be brought to the 
;;;             : front, otherwise it will print a warning.
;;;             : Returns t if a window was attempted to be brought to the front
;;;             : (success depends on select-rpm-window working) and nil otherwise.


(defun select-exp-window (&optional (window nil))
  "select the experiment window"
  
  ; don't allow anymore
  ;(when (and window (symbolp window))
  ;  (setf window (symbol-name window)))
  
  (aif (determine-exp-window (agi-component) window)
       (progn
         (select-rpm-window it)
         t)
       (print-warning "Select-exp-window failed.")))

(add-act-r-command "select-exp-window" 'select-exp-window "Bring an open experiment window to the front. Params: {window}.")


;;; ADD-ITEMS-TO-EXP-WINDOW  [Function]
;;; Description : Returns t if the items were attempted to be added
;;;             : (success depends on whether or not they were accepted by add-visual-
;;;             : items-to-rpm-window), but if there is no window provided and no
;;;             : current "default" window then a warning is printed and nil is 
;;;             : returned.


(defun add-items-to-exp-window (win &rest items)
  "Add the specified items to the experiment window"
  
  ; don't allow any more
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  
  (let ((agi (agi-component)))
    (multiple-value-bind (window key) (determine-exp-window agi win)
      (if window
          (bt:with-recursive-lock-held ((agi-lock agi))
            (let ((objects (mapcar (lambda (x) (gethash x (global-agi-table agi))) items))
                  (created (gethash key (window-objects-table agi)))
                  (added (gethash key (window-added-objects-table agi))))
                  
              
              (if (every 'identity objects)
                  (if (every (lambda (x) (and (find x created) (not (find x added)))) objects)
                      
                      (progn
                        
                        ;; have the UWI do what it needs to do first
                        (apply 'add-visual-items-to-rpm-window window objects)
                        
                        ;; for every model that has this window installed
                        
                          (maphash (lambda (model windows)
                                     (when (find key windows :test 'string-equal)
                                       (with-model-eval model
                                         
                                         (let (feat-lis)
                              
                                           ;; create the features for each item,
                                           ;; pass them off to vision module, and
                                           ;; record those features
                                           
                                           (bt:with-recursive-lock-held ((window-lock window))
                                             (dolist (item objects)
                                  
                                               (setf feat-lis (assoc model (visual-features item)))
                                  
                                               (unless feat-lis
                                                 (setf feat-lis (cons model nil))
                                                 (push-last feat-lis (visual-features item)))
                                               
                                               (dolist (y (build-vis-locs-for item))
                                                 (push (cons (car (add-visicon-features y)) y) (cdr feat-lis)))))))))
                                   
                                   (installed-windows agi))
                        
                        
                        ;; put the items into the added table for the window
                        
                        (setf (gethash key (window-added-objects-table agi))
                          (append added objects))
                        
                        
                        t)
                    (print-warning "One or more existing but invalid items in call to add-items-to-exp-window.  No items added."))
                (print-warning "One or more invalid items in call to add-items-to-exp-window.  No items added."))))
        (print-warning "No window available for add-items-to-exp-window.")))))

(add-act-r-command "add-items-to-exp-window" 'add-items-to-exp-window "Add the given items to the window provided. Params: window {item}*.")


;;; REMOVE-ITEMS-FROM-EXP-WINDOW  [Function]
;;; Description : Removes the requested items from the window provided or the "default" window if 
;;;             : null.  Returns t if the items were attempted to be removed
;;;             : (success depends on whether or not they were accepted by remove-visual-
;;;             : items-from-rpm-window), but if there is no window provided and no
;;;             : current "default" window then a warning is printed and nil is 
;;;             : returned.

(defun remove-items-from-exp-window (win &rest items)
  "Remove the specified items from the experiment window"
  
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  
  (let ((agi (agi-component)))
    (multiple-value-bind (window key) (determine-exp-window agi win)
      (if window
          
          (bt:with-recursive-lock-held ((agi-lock agi))
            (let ((objects (mapcar (lambda (x) (gethash x (global-agi-table agi))) items))
                  (created (gethash key (window-objects-table agi)))
                  (added (gethash key (window-added-objects-table agi))))
          
              (if (every 'identity objects)
                  (if (every (lambda (x) (and (find x created) (find x added))) objects)
                      
                      (progn
                        ;; for every model that has this window installed
                        
                         (bt:with-recursive-lock-held ((window-lock window))
                           (bt:with-recursive-lock-held ((agi-lock agi))
                             (maphash (lambda (model windows)
                                        (when (find key windows :test 'string-equal)
                                          (with-model-eval model
                                            (dolist (item objects)
                                              (dolist (feat (cdr (assoc model (visual-features item))))
                                                (delete-visicon-features (car feat)))
                                              (setf (visual-features item) (remove model (visual-features item) :key 'car :count 1))))))
                                      (installed-windows agi))))
                        
                        ;; remove the items from the added table
                        
                        (setf (gethash key (window-added-objects-table agi))
                          (set-difference added objects))
                        
                        ;; have the UWI do what it needs to
                        (apply 'remove-visual-items-from-rpm-window window objects)
                        
                        t)
                    (print-warning "One or more existing but invalid items in call to remove-items-from-exp-window.  No items removed."))
                (print-warning "One or more invalid items in call to remove-items-from-exp-window.  No items removed."))
              ))
      (print-warning "No window available for remove-items-from-exp-window.")))))

(add-act-r-command "remove-items-from-exp-window" 'remove-items-from-exp-window "Remove the given items from the window provided. Params: window {item}*.")


(defun clear-exp-window (&optional win)
  "Erases everything in the experiment window"
  
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  
  (let ((agi (agi-component)))
    
    (multiple-value-bind (window key) (determine-exp-window agi win)
    (if window
          (bt:with-recursive-lock-held ((agi-lock agi))
            (let ((added (gethash key (window-added-objects-table agi))))
          
               (bt:with-recursive-lock-held ((window-lock window))
                 (bt:with-recursive-lock-held ((agi-lock agi))
                             (maphash (lambda (model windows)
                                        (when (find key windows :test 'string-equal)
                                          (with-model-eval model
                                            (dolist (item added)
                                              (dolist (feat (cdr (assoc model (visual-features item))))
                                                (delete-visicon-features (car feat)))
                                              (setf (visual-features item) (remove model (visual-features item) :key 'car :count 1))))))
                                      (installed-windows agi))))
                                  
              ;; remove the added list
              
              (remhash key (window-added-objects-table agi))
              
              ;; have the UWI do what it needs to
              (apply 'remove-visual-items-from-rpm-window window added)
          
              t))
      (print-warning "No window available for clear-exp-window.")))))

(add-act-r-command "clear-exp-window" 'clear-exp-window "Remove all items from the window provided. Params: {window}.")


(defun create-text-for-exp-window (win text &key (x 0) (y 0) (color 'black) (height 20) (width 75) (font-size 12))
  "Creates a text item for the experiment window"
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  (when (stringp color)
      (setf color (aif (ignore-errors (read-from-string color)) it 'black)))
  (if (stringp text)
      (let ((agi (agi-component)))
        (multiple-value-bind (window key) (determine-exp-window agi win)
        (if window
            (let* ((item (make-static-text-for-rpm-window window
                                                          :text text 
                                                          :x x
                                                          :y y
                                                          :width width
                                                          :height height
                                                          :color color
                                                          :font-size font-size))
                   (item-key (list key (string (id item)))))
              
              (if (and agi item)
                  (bt:with-recursive-lock-held ((agi-lock agi))
                    (setf (gethash item-key (global-agi-table agi)) item)
                    (push item (gethash key (window-objects-table agi)))
                    item-key)
                (print-warning "Problem creating text item.  No item created.")))
          (print-warning "No window available for creating a text item."))))
    (print-warning "Text must be a string in create-text-for-exp-window. Cannot create with ~s." text)))


(defun external-create-text-for-exp-window (window text &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'create-text-for-exp-window '(:x :y :height :width :color :font-size))
    (when valid 
      (apply 'create-text-for-exp-window window text ol))))

(add-act-r-command "create-text-for-exp-window" 'external-create-text-for-exp-window "Create a text item for the provided experiment window with the features specified. Params: window text {<x, y, color, height, width, font-size>}.")


;;; ADD-TEXT-TO-EXP-WINDOW  [Function]
;;; Description : Build a text item based on the parameters supplied and
;;;             : add it to the specified window (or current default if
;;;             : none povided).

(defun add-text-to-exp-window (win text &key (x 0) (y 0) (color 'black) (height 20) (width 75) (font-size 12))
  "Create and display a text item in the experiment window"
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  (when (stringp color)
      (setf color (aif (ignore-errors (read-from-string color)) it 'black)))
  (let ((item-key (create-text-for-exp-window win text :x x :y y :color color :height height :width width :font-size font-size)))
    (if item-key
        (if (add-items-to-exp-window win item-key)
            item-key
          nil)
      nil)))

(defun external-add-text-to-exp-window (window text &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'add-text-to-exp-window '(:x :y :height :width :color :font-size))
    (when valid 
      (apply 'add-text-to-exp-window window text ol))))

(add-act-r-command "add-text-to-exp-window" 'external-add-text-to-exp-window "Create a text item for the provided experiment window with the features specified and place it in the window. Params: window text {< x, y, color, height, width, font-size >}.")

(defun modify-text-for-exp-window (item &key text x y color height width font-size)
  "Modify a text item for the experiment window"
  (let ((agi (agi-component)))
    (bt:with-recursive-lock-held ((agi-lock agi))
      (aif (gethash item (global-agi-table agi))
           (let* ((top-win (find-top-level-window (determine-exp-window agi (first item))))
                  (window-title (window-title top-win)) ;; probably always same as in the item, but maybe there'll be windows in windows at some point.
                  mods)
             (when x
               (if (numberp x)
                   (push (list :x x) mods)
                 (print-warning "X parameter in modify-text-for-exp-window must be a number but given ~s." x)))
             (when y
               (if (numberp y)
                   (push (list :y y) mods)
                 (print-warning "y parameter in modify-text-for-exp-window must be a number but given ~s." y)))
             (when text
               (if (stringp text)
                   (push (list :text text) mods)
                 (print-warning "Text parameter in modify-text-for-exp-window must be a string but given ~s." text)))
             (when height
               (if (numberp height)
                   (push (list :height height) mods)
                 (print-warning "Height parameter in modify-text-for-exp-window must be a number but given ~s." height)))
             (when width
               (if (numberp width)
                   (push (list :width width) mods)
                 (print-warning "Width parameter in modify-text-for-exp-window must be a number but given ~s." width)))
             (when font-size
               (if (numberp font-size)
                   (push (list :font-size font-size) mods)
                 (print-warning "Font-size parameter in modify-text-for-exp-window must be a number but given ~s." font-size)))
             (when color
               (when (stringp color)
                 (setf color (aif (ignore-errors (read-from-string color)) it 'black)))
               (push (list :color color) mods))
             
             (cond ((null mods)
                    ;; why even call it?
                    item
                    )
                   ((null (visual-features it)) ;; not in visicon so just make the changes
                    (apply 'modify-text-for-rpm-window (cons it (flatten mods)))
                    item)
                   
                   ((or (not (= (length (first (visual-features it))) 2))   ;; don't try to match multiple features
                        (and text (or (> (length (string-to-lines text)) 1)
                                          (> (length (chop-string (get-module :vision) text)) 1))))
                    (print-warning "Cannot modify a text item which has or will have multiple features."))
                   
                   (t
                    (apply 'modify-text-for-rpm-window (cons it (flatten mods)))
                    
                    (bt:with-recursive-lock-held ((window-lock top-win))
                      ;; for every model that has this item's window installed
                      (maphash (lambda (model windows)
                                 (when (find window-title windows :test 'string-equal)
                                   (with-model-eval model
                                     
                                     (let* ((current-features (cadr (assoc model (visual-features it))))
                                            (new-feats (build-vis-locs-for it))
                                            (id (first current-features)))
                                       
                                       ;; just pass all the features again to vision
                                       ;; except the ones that can't be changed assuming that
                                       ;; the first one is the isa!
                                       
                                       (modify-visicon-features (cons id (cddar new-feats)))
                                       
                                       (setf (cdr current-features) new-feats)))))
                               (installed-windows agi)))
                    item)))
               
               (print-warning "Could not modify text ~s because it does not refer to an existing interface item." item)))))


(defun external-modify-text-for-exp-window (item &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'modify-text-for-exp-window '(:text :x :y :height :width :color :font-size))
    (when valid 
      (apply 'modify-text-for-exp-window item ol))))

(add-act-r-command "modify-text-for-exp-window" 'external-modify-text-for-exp-window "Modify a text item created for an experiment window with the new features provided. Params: text-item {< text, x, y, color, height, width, font-size >}.")


          
(defun create-button-for-exp-window (win &key (text "") (x 0) (y 0) (action nil) (height 18) (width 60) (color 'gray))
  "Creates a button item for the experiment window"
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  
  (when (stringp color)
      (setf color (aif (ignore-errors (read-from-string color)) it 'gray)))
  (if (stringp text)
      (let ((agi (agi-component)))
        (multiple-value-bind (window key) (determine-exp-window agi win)
        (if window
            (let* ((item (make-button-for-rpm-window window
                                                     :x x
                                                     :y y
                                                     :text text
                                                     :action (if (valid-button-action action) action "default-button-action")
                                                     :height height
                                                     :width width :color color))
                   (item-key (list key (string (id item)))))
              
              (if (and agi item)
                  (bt:with-recursive-lock-held ((agi-lock agi))
                    (setf (gethash item-key (global-agi-table agi)) item)
                    (push item (gethash key (window-objects-table agi)))
                    
                    item-key)
                (print-warning "Problem creating button item.  No button created.")))
          (print-warning "No window available for creating a button."))))
        (print-warning "Text must be a string in create-button-for-exp-window. Cannot create with ~s." text)))

(defun external-create-button-for-exp-window (win &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'create-button-for-exp-window '(:text :x :y :height :width :color :action))
    (when valid 
      (apply 'create-button-for-exp-window win ol))))

(add-act-r-command "create-button-for-exp-window" 'external-create-button-for-exp-window "Create a button item for the provided experiment window with the features specified. Params: window {< text, x, y, action, height, width, color >}.")


;;; ADD-BUTTON-TO-EXP-WINDOW  [Function]
;;; Description : Build a button item based on the parameters supplied and
;;;             : add it to the specified window (or current default if
;;;             : none povided).

(defun add-button-to-exp-window (win &key (text "") (x 0) (y 0) (action nil) (height 18) (width 60) (color 'gray))
  "Create and display a button item in the experiment window"
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  
  (when (stringp color)
    (setf color (aif (ignore-errors (read-from-string color)) it 'gray)))
  (let ((item-key (create-button-for-exp-window win :text text :x x :y y :action action :height height :width width :color color)))
    (if item-key
        (if (add-items-to-exp-window win item-key)
            item-key
          nil)
      nil)))

(defun external-add-button-to-exp-window (win &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'add-button-to-exp-window '(:text :x :y :height :width :color :action))
    (when valid 
      (apply 'add-button-to-exp-window win ol))))

(add-act-r-command "add-button-to-exp-window" 'external-add-button-to-exp-window "Create a button item for the provided experiment window with the features specified and place it in the window. Params: window {< text, x, y, action, height, width, color >}.")

(defun valid-button-action (action)
  (and action
       (or (and (stringp action)
                (check-act-r-command action))
           (functionp action)
           (and (symbolp action) 
                (fboundp action)
                (not (macro-function action)))
           (and (listp action)
                (atom (first action))
                (valid-button-action (first action))))))
           
 
(defun modify-button-for-exp-window (item &key text x y action height width color)
  "Modify a button item for the experiment window"
  (let ((agi (agi-component)))
    (bt:with-recursive-lock-held ((agi-lock agi))
      (aif (gethash item (global-agi-table agi))
           (let* ((top-win (find-top-level-window (determine-exp-window agi (first item))))
                  (window-title (window-title top-win))
                  mods)
             (when x
               (if (numberp x)
                   (progn
                     (push x mods) 
                     (push :x mods))
                 (print-warning "X parameter in modify-button-for-exp-window must be a number but given ~s." x)))
             (when y
               (if (numberp y)
                   (progn 
                     (push y mods)
                     (push :y mods))
                 (print-warning "y parameter in modify-button-for-exp-window must be a number but given ~s." y)))
             (when text
               (if (stringp text)
                   (progn
                     (push text mods)
                     (push :text mods))
                 (print-warning "Text parameter in modify-button-for-exp-window must be a string but given ~s." text)))
             (when height
               (if (numberp height)
                   (progn
                     (push height mods)
                     (push :height mods))
                 (print-warning "Height parameter in modify-button-for-exp-window must be a number but given ~s." height)))
             (when width
               (if (numberp width)
                   (progn 
                     (push width mods)
                     (push :width mods))
                 (print-warning "Width parameter in modify-button-for-exp-window must be a number but given ~s." width)))
             (when action
               (if (valid-button-action action)
                   (progn
                     (push action mods)
                     (push :action mods))
                 (print-warning "Action parameter in modify-button-for-exp-window must be a valid command but given ~s." action)))
             (when color
               (when (stringp color)
                 (setf color (aif (ignore-errors (read-from-string color)) it 'black)))
               (push color mods)
               (push :color mods))
             
             (push it mods)
             
             (cond ((null mods)
                    ;; why even call it?
                    item
                    )
                   ((null (visual-features it)) ;; not in visicon so just make the changes
                    (apply 'modify-button-for-rpm-window mods)
                    item)
                   
                   ((> (length (first (visual-features it))) 3)
                    (print-warning "Cannot modify a button item which has multi-word text."))
                   
                   ((and (= (length (first (visual-features it))) 3) ; button and text items
                         text
                         (not (= (length (chop-string (get-module :vision) text)) 1))) ;one item on a line
                    
                    (print-warning "Cannot modify a button item which requires adding or removing features."))
                   
                   ((and (= (length (first (visual-features it))) 2) ; just the button
                         text
                         (not (zerop (length (chop-string (get-module :vision) text))))) ; any text
                    
                    (print-warning "Cannot modify a button item which requires adding or removing features."))
                   (t
                    (apply 'modify-button-for-rpm-window mods)
                    
                    (bt:with-recursive-lock-held ((window-lock top-win))
                      ;; for every model that has this item's window installed
                      (maphash (lambda (model windows)
                                 (when (find window-title windows :test 'string-equal)
                                   (with-model-eval model
                                     
                                     (let* ((current-features (cdr (assoc model (visual-features it))))
                                            (new-feats (reverse (build-vis-locs-for it))))
                                       
                                       (dotimes (i (length current-features))
                                         (let ((id (first (nth i current-features))))
                                           
                                           ;; just pass all the features again to vision
                                           ;; except the ones that can't be changed which
                                           ;; we assume are only the isa which are first 2.
                                           
                                           (modify-visicon-features (cons id (cddr (nth i new-feats))))
                                           
                                           (setf (cdr (nth i current-features)) (nth i new-feats))))))))
                               (installed-windows agi)))
                    item)))
           
           (print-warning "Could not modify button ~s because it does not refer to an existing interface item." item)))))

(defun external-modify-button-for-exp-window (item &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'modify-button-for-exp-window '(:text :x :y :height :width :color :action))
    (when valid 
      (apply 'modify-button-for-exp-window item ol))))


(add-act-r-command "modify-button-for-exp-window" 'external-modify-button-for-exp-window "Modify a button item created for an experiment window with the new features provided. Params: button {< text, x, y, action, height, width, color >}.")


;;; ADD-LINE-TO-EXP-WINDOW  [Function]
;;; Description : Build a line item based on the parameters supplied and
;;;             : add it to the specified window (or current default if
;;;             : none povided).



(defun create-line-for-exp-window (win start-pt end-pt &optional (color 'black))
  "Creates a line item for the experiment window"
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  (when (stringp color)
    (setf color (aif (ignore-errors (read-from-string color)) it 'black)))
  (if (and (listp start-pt) (listp end-pt) (= 2 (length start-pt) (length end-pt)))
      (let ((agi (agi-component)))
        (multiple-value-bind (window key) (determine-exp-window agi win)
        (if window
            
            (let* ((item (make-line-for-rpm-window window (mapcar 'round start-pt) (mapcar 'round end-pt) color))
                   (item-key (list key (string (id item)))))
              
              (if (and agi item)
                  (bt:with-recursive-lock-held ((agi-lock agi))
                    (setf (gethash item-key (global-agi-table agi)) item)
                    (push item (gethash key (window-objects-table agi)))
                                       
                    item-key)
                (print-warning "Problem creating line item.  No item created.")))
            
          (print-warning "No window available for adding a line."))))
    (print-warning "Start and end points for a line must be 2 element lists.  Cannot create a line given ~s and ~s." start-pt end-pt)))


(add-act-r-command "create-line-for-exp-window" 'create-line-for-exp-window "Create a line for the provided experiment window with the features specified. Params: window (x1 y1) (x2 y2) {color}.")


(defun add-line-to-exp-window (win start-pt end-pt &optional (color 'black))
  "Create and display a line item in the experiment window"
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  (when (stringp color)
    (setf color (aif (ignore-errors (read-from-string color)) it 'black)))
  (let ((item-key (create-line-for-exp-window win start-pt end-pt color)))
    (if item-key
        (if (add-items-to-exp-window win item-key)
            item-key
          nil)
      nil)))

(add-act-r-command "add-line-to-exp-window" 'add-line-to-exp-window "Create a line for the provided experiment window with the features specified and then place it in that window. Params: window (x1 y1) (x2 y2) {color}.")


(defun modify-line-for-exp-window (item start-pt end-pt &optional color)
  "Modify a line item for the experiment window"
  
  (let ((agi (agi-component)))
    (bt:with-recursive-lock-held ((agi-lock agi))
      (aif (gethash item (global-agi-table agi))
           (let* ((top-win (find-top-level-window (determine-exp-window agi (first item))))
                  (window-title (window-title top-win)) ;; probably always same as in the item, but maybe there'll be windows in windows at some point.
                  (c (when color
                       (if (stringp color)
                           (ignore-errors (read-from-string color))
                         color)))
                  (start (if (and start-pt (listp start-pt) (= (length start-pt) 2) (every 'numberp start-pt))
                             (mapcar 'round start-pt)
                           (when start-pt
                             (print-warning "Invalid start-point ~s specified for modify-line-for-exp-window."))))
                  (end (if (and end-pt (listp end-pt) (= (length end-pt) 2) (every 'numberp end-pt))
                             (mapcar 'round end-pt)
                           (when end-pt
                             (print-warning "Invalid end-point ~s specified for modify-line-for-exp-window.")))))
             
             
             (cond ((and (null c) (null start) (null end))
                    ;; why even call it?
                    item
                    )
                   ((null (visual-features it)) ;; not in visicon so just make the changes
                    (if c
                        (modify-line-for-rpm-window it start end :color c)
                      (modify-line-for-rpm-window it start end))
                    item)
                   (t
                    (if c
                        (modify-line-for-rpm-window it start end :color c)
                      (modify-line-for-rpm-window it start end))
                    
                    (bt:with-recursive-lock-held ((window-lock top-win))
                     
                     
                     ;; for every model that has this item's window installed
                     
                     (maphash (lambda (model windows)
                                (when (find window-title windows :test 'string-equal)
                                  (with-model-eval model
                                    
                                    (let* ((current-features (cadr (assoc model (visual-features it))))
                                           (new-feats (build-vis-locs-for it))
                                           (id (first current-features)))
                                      
                                      ;; just pass all the features again to vision
                                      ;; except the ones that can't be changed
                                      ;; assuming there's only one feature for a line item
                                      
                                      (modify-visicon-features (cons id (cddr (first new-feats))))
                                      
                                      (setf (cdr current-features) new-feats)))))
                              (installed-windows agi)))
                    item)))
               (print-warning "Could not modify line ~s because it does not refer to an existing interface item." item)))))

(add-act-r-command "modify-line-for-exp-window" 'modify-line-for-exp-window "Change the attributes of a line that was created for an experiment window. Params: line (x1 y1) (x2 y2) {color}.")


;;;; ---------------------------------------------------------------------- ;;;;
;;;; The miscelaneous functions used in the tutorial.
;;;; ---------------------------------------------------------------------- ;;;;

  
;;; GET-TIME
;;; Return time in milliseconds
;;; If the optional parameter is specified as true then model time is returned
;;; and if it is nil then get-internal-real-time is used.  
;;; If time is not based on the model it's only meaningful as a relative
;;; time.

(defun get-time (&optional (model-time t))
  (if model-time
      (mp-time-ms)
    (round (* 1000 (/ (get-internal-real-time) internal-time-units-per-second)))))

(add-act-r-command "get-time" 'get-time "Get current absolute model time or relative real time in milliseconds. Params: {model}." nil)


;;; These are the correlation and deviation functions from the scripting
;;; extensions file and the necessary support.  I figured since they are
;;; still used they should be put here because the scripting extensions 
;;; aren't part of ACT-R 5, but making people load the scripting file
;;; separately is a pain...  I also changed mean-deviation so that it
;;; actually returned the deviation.


(defmacro /-safe (number &rest dividers)
  `(/ ,number ,@(let ((max nil))
                  (dolist (divider dividers max)
                    (push-last `(if (zerop ,divider) 1 ,divider) max)))))

(defun numbers-list (structure)
  (let ((list nil))
    (cond ((arrayp structure)
           (dotimes (i (array-total-size structure))
             (let ((data (row-major-aref structure i)))
               (when (numberp data) (push data list)))))
          ((listp structure)
           (dolist (data structure)
             (cond ((listp data)
                    (setf list (append (nreverse (numbers-list data)) list)))
                   ((numberp data)
                    (push data list)))))
          ((numberp structure)
           (push structure list))
          (t (format t "~&UNKNOWN DATA FORMAT ~S NOT COMPATIBLE WITH NUMBERS LIST.~%"
               structure)))
    (nreverse list)))

(defun square-data (x)
  (* x x))

(defun sum-list (list)
  (let ((sum 0.0))
    (dolist (data list sum)
      (incf sum data))))

(defun square-list (list)
  (let ((sum 0.0))
    (dolist (data list sum)
      (incf sum (square-data data)))))

(defun product-list (list1 list2)
  (let ((sum 0.0))
    (loop
      (when (or (null list1) (null list2)) (return sum))
      (incf sum (* (pop list1) (pop list2))))))

(defun mean-deviation (results data &optional (output t))
  (let* ((results-list (numbers-list results))
         (data-list (numbers-list data))
         (n (min (length results-list) (length data-list)))
         (opened nil))
    (cond ((or (stringp output) (pathnamep output))
           (setf output (handler-case (open output :direction :output :if-exists :append :if-does-not-exist :create)
                          (error (x) (print-warning "Error ~/print-error-message/ occurred while trying to open file ~s for output from mean-deviation.  Printing as if t provided."
                                                    x output))))
           (if (null output)
               (setf output t)
             (setf opened t)))
          ((not (or (streamp output) (null output) (eq output t)))
           (print-warning "Output argument ~s to mean-deviation is not valid. It must be a string, pathname, stream, t or nil. Printing as if t provided." output)
           (setf output t)))
    
    (unless (= (length results-list) (length data-list))
      (print-warning  "~s and ~s do not have the same number of numbers." results data))
    
    (let ((result (sqrt (/ (+ (square-list results-list) (square-list data-list)
                              (* -2.0 (product-list results-list data-list)))
                           n))))
      (if (and (eq output t) (current-model))
          (command-output "MEAN DEVIATION: ~6,3F" result)
        (if (eq output t)
            (act-r-output "MEAN DEVIATION: ~6,3F" result)
          (format output "~&MEAN DEVIATION: ~6,3F~%" result)))
      (when opened (close output))
      result)))

(add-act-r-command "mean-deviation" 'mean-deviation "Compute the RMSD between two lists of numbers. Params: (results) (data) {output}." nil)


(defun correlation (results data &optional (output t))
  (let* ((results-list (numbers-list results))
         (data-list (numbers-list data))
         (n (min (length results-list) (length data-list)))
         (average-results (/-safe (sum-list results-list) n))
         (average-data (/-safe (sum-list data-list) n))
         (opened nil))
    (cond ((or (stringp output) (pathnamep output))
           (setf output (handler-case (open output :direction :output :if-exists :append :if-does-not-exist :create)
                          (error (x) (print-warning "Error ~/print-error-message/ occurred while trying to open file ~s for output from correlation.  Printing as if t provided."
                                                    x output))))
           (if (null output)
               (setf output t)
             (setf opened t)))
          ((not (or (streamp output) (null output) (eq output t)))
           (print-warning "Output argument ~s to correlation is not valid. It must be a string, pathname, stream, t or nil. Printing as if t provided." output)
           (setf output t)))
    
    (unless (= (length results-list) (length data-list))
      (print-warning  "~s and ~s do not have the same number of numbers .~%"
              results data))
    (let ((result (/-safe (- (/-safe (product-list results-list data-list) n)
                       (* average-results average-data))
                    (* (sqrt (- (/-safe (square-list results-list) n)
                                (square-data average-results)))
                       (sqrt (- (/-safe (square-list data-list) n)
                                (square-data average-data)))))))
      (if (and (eq output t) (current-model))
          (command-output "CORRELATION: ~6,3F" result)
        (if (eq output t)
            (act-r-output "CORRELATION: ~6,3F" result)
          (format output "~&CORRELATION: ~6,3F~%" result)))
      (when opened (close output))
      result)))

(add-act-r-command "correlation" 'correlation "Compute the correlation between two lists of numbers. Params: (results) (data) {output}." nil)


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
