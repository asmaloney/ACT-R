;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2017 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : keyboard.lisp
;;; Version     : 3.1
;;; 
;;; Description : Implement the virtual keyboard device for models.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2017.01.19 Dan [1.0]
;;;             : * Actually put a header on this file after splitting it out
;;;             :   from the device and device interface code.
;;; 2017.01.20 Dan
;;;             : * Finish off the motor command code.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.08 Dan
;;;             : * Changed the define-device call since it doesn't
;;;             :   take keyword parameters now that the internal is the same
;;;             :   as the remote version.
;;; 2017.02.22 Dan
;;;             : * Keyboard key names are now stored in upcase and the key->*
;;;             :   functions upcase the given value.
;;; 2017.02.27 Dan
;;;             : * Schedule "output-key" directly now instead of going through
;;;             :   the stub function, but still need the stub since human 
;;;             :   input gets routed through that and putting the remote call
;;;             :   into the interface code would be ugly.
;;; 2017.03.06 Dan
;;;             : * Updated the doc string for output-key to indicate that it
;;;             :   also gets called for real interaction with an exp-window.
;;;             : * Specify the model in the schedule-event for the output-key.
;;;             : * Don't set the hand's device directly but provide it to the
;;;             :   point-hand request for motor actions.
;;; 2017.03.08 Dan
;;;             : * When a keyboard gets installed only move hands that aren't
;;;             :   already on a device to it.
;;; 2017.03.15 Dan
;;;             : * Fixed a bug in the scheduling of output-key.
;;; 2017.07.14 Dan
;;;             : * Fixed some cut-and-paste issues in hand-to-home.
;;; 2017.08.10 Dan [2.0]
;;;             : * Instead of a single global keyboard definition, now using a
;;;             :   component to hold a table of keyboards for each model.
;;;             :   Update-keyboard-device needs to be called in the context of
;;;             :   a model and will change that model's keyboard.
;;;             : * Update all the access to hand objects to be thread safe.
;;; 2017.08.11 Dan
;;;             : * Fixed some typos in the component functions.
;;; 2017.08.23 Dan
;;;             : * Don't need the dummy function for output-key now.
;;; 2018.01.03 Dan
;;;             : * Fixed a bug with specify-key because it only set the position
;;;             :   of keys in the array for default keys.
;;; 2018.03.12 Dan
;;;             : * Adjusted the device fns to accept the whole device and now
;;;             :   check that it's a "motor" device before doing anything.
;;; 2018.03.27 Dan
;;;             : * Make sure all the hand setting actions use the whole device
;;;             :   now instead of just "keyboard".
;;; 2018.06.04 Dan
;;;             : * Added remote start-hand-at-keypad and start-hand-at-key.
;;; 2018.06.22 Dan
;;;             : * Only remote start-hand-at-key accepts a string naming hand.
;;; 2018.07.26 Dan [1.1]
;;;             : * Don't worry about completing requests.
;;; 2018.10.04 Dan [2.2]
;;;             : * Use notify-interface instead of touching the internals of 
;;;             :   the motor module for some commands (new styles still use a
;;;             :   little internal knowledge for efficiency).
;;;             : * Using the third item in the device list to name the keyboard
;;;             :   to install, and replacing update-keyboard-device with an
;;;             :   add-keyboard command that takes a string for the name and
;;;             :   then the class name as the second item.  Of course that
;;;             :   still requires one to create a keyboard device in Lisp, but
;;;             :   for now that's better than how things were before...
;;; 2018.10.08 Dan
;;;             : * Also adding a set-default-keyboard which specifies the 
;;;             :   keyboard that's installed when no third item provided.
;;;             :   That is a global change which is unaffected by clear-all!
;;;             : * Added a way to extend the notifications which are handled
;;;             :   by the keyboard through the other-notifications method 
;;;             :   since the keyboards are always Lisp objects at this point.
;;;             : * Don't create a keyboard entry in the component for models
;;;             :   automatically (only when installed).
;;;             : * Allow the keyboard to be installed for any device, not just
;;;             :   motor.  Of course, only supports actions for motor by default
;;;             :   but extensions could be made to handle others.
;;;             : * User code all uses notify-interface, but styles still use
;;;             :   motor module.
;;; 2018.10.16 Dan
;;;             : * Hand-to-keypad and hand-to-home need to not set the device.
;;; 2018.01.14 Dan
;;;             : * Fixed a bug with start-hand-at-key always printing a warning
;;;             :   about an invalid hand.
;;; 2019.09.09 Dan
;;;             : * Fixed a bug with press-key not warning when there was no
;;;             :   keyboard device installed.
;;; 2020.03.27 Dan [3.0]
;;;             : * Updated to use the new position information that records 
;;;             :   where the hands "will be" when the action starts to get the
;;;             :   correct feature info.
;;;             : * Also added tests to hand-to-home/keypad to ignore if already
;;;             :   there or going to be there.
;;;             : * Point-hand-at-key needs to check for 'standard as the offsets.
;;;             : * Press-key checks for home or keypad position before making
;;;             :   the action and warns if the hand isn't in the right starting
;;;             :   place (assumes that keys with a loc x > 18 and y > 1 are 
;;;             :   keypad position for now).
;;; 2020.04.20 Dan [3.1]
;;;             : * Generalize the possible hand positions on the keyboard and
;;;             :   include the assumed starting position for the press-key style
;;;             :   in specify-key.
;;; 2020.08.25 Dan
;;;             : * Move the keyboard-component defstruct to before any of the
;;;             :   accessors are used.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Implement the keyboard as a device under the new PM interface mechanisms.
;;; Create a single default keyboard definition with which any model can interact,
;;; but also specify a component which holds a table of keyboards so each model
;;; can have a different one.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Virtual-keyboard class and initialization methods.
;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Simplify the construction of a keyboard because the three separate methods
;;; that the old code used have always had inconsistencies and/or errors in the
;;; specifications between the methods.
;;;
;;; The motor commands related to keyboards are moved here too since the motor
;;; module itself doesn't "know" about any particular device with which it can
;;; interact.  That's a departure from the older versions where the motor 
;;; module assumed a keyboard and mouse existed and was built around them.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; A structure to hold a component for storing keyboards

(defstruct keyboard-component (lock (bt:make-lock "keyboard-component")) (table (make-hash-table)) default)

;;;; ---------------------------------------------------------------------- ;;;;
;;;; The virtual keyboard.  

(defclass virtual-keyboard ()
   ((key->cmd-ht :accessor key->cmd-ht 
                 :initform (make-hash-table :test #'equalp))
    (key->hand-start-ht :accessor key->hand-start-ht
                        :initform (make-hash-table :test #'equalp))
    (key->loc-ht :accessor key->loc-ht
                  :initform (make-hash-table :test #'equalp))
    (loc->key-array :accessor loc->key-arr :initform nil)
    (hand-loc-table :accessor hand-loc-table 
                    :initform (make-hash-table :test #'equalp))
    (left-start :accessor left-start :initform nil)
    (right-start :accessor right-start :initform nil)))

(defmethod initialize-instance :after ((vk virtual-keyboard) &key)
  (setf (loc->key-arr vk) (make-array (keyboard-size vk) :initial-element nil))
  (populate-hand-locations vk)
  (set-starting-hand-locations vk)
  (populate-key-details vk))


;; Instead of building up the three tables (name->press-key-command, position->name, 
;; and name->position) with separate methods just use one method that takes all of
;; the components and then enters the information appropriately.  This avoids
;; the inconsistencies (and errors) between tables that the old methods often
;; created.

(defgeneric specify-key (keyboard x y names &key default style hand-loc)
  (:documentation "Specify the names for keys by location, indicate whether it's the default position for that key, and
                   if there's a press-key mapping for it indicate the underlying style and features and the assumed hand-location name."))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Subclass the virtual-keyboard and these methods to represent different keyboards.

(defgeneric keyboard-size (keyboard)
  (:documentation "Specify the dimensions for the keyboard as a list of columns and rows"))

(defmethod keyboard-size ((k virtual-keyboard))
  (list 23 7))


(defgeneric populate-hand-locations (keyboard)
  (:documentation "Set all of the desired hand locations in the table"))

(defmethod populate-hand-locations ((k virtual-keyboard))
  (setf (gethash 'left-home (hand-loc-table k))
    (list (list 'hand 4 4) (list 'index 0 0) (list 'middle -1 0) (list 'ring -2 0) (list 'pinkie -3 0) (list 'thumb 1 2)))
  (setf (gethash 'right-home (hand-loc-table k))
    (list (list 'hand 7 4) (list 'index 0 0) (list 'middle 1 0) (list 'ring 2 0) (list 'pinkie 3 0) (list 'thumb -1 2)))
  (setf (gethash 'keypad (hand-loc-table k))
    (list (list 'hand 19 4) (list 'index 0 0) (list 'middle 1 0) (list 'ring 2 0) (list 'pinkie 3 1) (list 'thumb 0 2))))


(defgeneric set-starting-hand-locations (keyboard)
  (:documentation "Set the starting hand locations in the table"))

(defmethod set-starting-hand-locations ((k virtual-keyboard))
  (setf (left-start k) (gethash 'left-home (hand-loc-table k)))
  (setf (right-start k) (gethash 'right-home (hand-loc-table k))))


(defgeneric populate-key-details (keyboard)
  (:documentation  "Specify all the key information using the specify-key method."))

(defmethod populate-key-details ((k virtual-keyboard))
  ;; Top row
  (specify-key k 0 0 '("ESC" "ESCAPE"))
  
  (specify-key k 2 0 "F1")
  (specify-key k 3 0 "f2")
  (specify-key k 4 0 "f3")
  (specify-key k 5 0 "f4")
  
  (specify-key k 7 0 "F5")
  (specify-key k 8 0 "f6")
  (specify-key k 9 0 "f7")
  (specify-key k 10 0 "f8")
  
  (specify-key k 12 0 "F9")
  (specify-key k 13 0 "f10")
  (specify-key k 14 0 "f11")
  (specify-key k 15 0 "f12")

  (specify-key k 17 0 '("f13" "print-screen"))
  (specify-key k 18 0 '("f14" "scroll-lock"))
  (specify-key k 19 0 '("f15" "pause"))
  
  ;; numeric key row
  (specify-key k 0 2 '("`" "backquote") :style '(peck-recoil :hand left :finger pinkie :r 2.24 :theta -2.03) :hand-loc 'left-home)
  (specify-key k 1 2 "1" :style '(peck-recoil :hand left :finger pinkie :r 2 :theta -1.57) :hand-loc 'left-home)
  (specify-key k 2 2 "2" :style '(peck-recoil :hand left :finger ring :r 2 :theta -1.57) :hand-loc 'left-home)
  (specify-key k 3 2 "3" :style '(peck-recoil :hand left :finger middle :r 2 :theta -1.57) :hand-loc 'left-home)
  (specify-key k 4 2 "4" :style '(peck-recoil :hand left :finger index :r 2 :theta -1.57) :hand-loc 'left-home)
  (specify-key k 5 2 "5" :style '(peck-recoil :hand left :finger index :r 2.24 :theta -1.11) :hand-loc 'left-home)
  (specify-key k 6 2 "6" :style '(peck-recoil :hand right :finger index :r 2.24 :theta -2.03) :hand-loc 'right-home)
  (specify-key k 7 2 "7" :style '(peck-recoil :hand right :finger index :r 2 :theta -1.57) :hand-loc 'right-home)
  (specify-key k 8 2 "8" :style '(peck-recoil :hand right :finger middle :r 2 :theta -1.57) :hand-loc 'right-home)
  (specify-key k 9 2 "9" :style '(peck-recoil :hand right :finger ring :r 2 :theta -1.57) :hand-loc 'right-home)
  (specify-key k 10 2 "0" :style '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57) :hand-loc 'right-home)
  (specify-key k 11 2 '("-" "hyphen") :style '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -1.11) :hand-loc 'right-home)
  (specify-key k 12 2 '( "=" "equal") :style '(peck-recoil :hand right :finger pinkie :r 2.83 :theta -0.78) :hand-loc 'right-home)
  (specify-key k 13 2 "delete" :style '(peck-recoil :hand right :finger pinkie :r 3.6 :theta -0.59) :hand-loc 'right-home)
  
  (specify-key k 15 2 "help")
  (specify-key k 16 2 "home")
  (specify-key k 17 2 "pageup")
  
  (specify-key k 19 2 "clear" :style '(peck-recoil :hand right :finger index :r 2 :theta -1.57) :hand-loc 'keypad)
  (specify-key k 20 2 '("keypad-=" "keypad-equal") :style '(peck-recoil :hand right :finger middle :r 2 :theta -1.57) :hand-loc 'keypad)
  (specify-key k 21 2 '("keypad-/" "keypad-slash" "keypad-divide") :style '(peck-recoil :hand right :finger ring :r 2 :theta -1.57) :hand-loc 'keypad)
  (specify-key k 22 2 '("keypad-*" "keypad-asterisk" "keypad-times") :style '(peck-recoil :hand right :finger pinkie :r 3 :theta -1.57) :hand-loc 'keypad)

  
  ;; qwerty row
  (specify-key k 0 3 "tab" :style '(peck-recoil :hand left :finger pinkie :r 1.41 :theta -2.36) :hand-loc 'left-home)
  (specify-key k 1 3 "q" :style '(peck-recoil :hand left :finger pinkie :r 1 :theta -1.57) :hand-loc 'left-home)
  (specify-key k 2 3 "w" :style '(peck-recoil :hand left :finger ring :r 1 :theta -1.57) :hand-loc 'left-home)
  (specify-key k 3 3 "e" :style '(peck-recoil :hand left :finger middle :r 1 :theta -1.57) :hand-loc 'left-home)
  (specify-key k 4 3 "r" :style '(peck-recoil :hand left :finger index :r 1 :theta -1.57) :hand-loc 'left-home)
  (specify-key k 5 3 "t" :style '(peck-recoil :hand left :finger index :r 1.41 :theta -0.79) :hand-loc 'left-home)
  (specify-key k 6 3 "y" :style '(peck-recoil :hand right :finger index :r 1.41 :theta -2.36) :hand-loc 'right-home)
  (specify-key k 7 3 "u" :style '(peck-recoil :hand right :finger index :r 1 :theta -1.57) :hand-loc 'right-home)
  (specify-key k 8 3 "i" :style '(peck-recoil :hand right :finger middle :r 1 :theta -1.57) :hand-loc 'right-home)
  (specify-key k 9 3 "o" :style '(peck-recoil :hand right :finger ring :r 1 :theta -1.57) :hand-loc 'right-home)
  (specify-key k 10 3 "p" :style '(peck-recoil :hand right :finger pinkie :r 1 :theta -1.57) :hand-loc 'right-home)
  (specify-key k 11 3 '("[" "left-bracket") :style '(peck-recoil :hand right :finger pinkie :r 1.41 :theta -0.78) :hand-loc 'right-home)
  (specify-key k 12 3 '("]" "right-bracket") :style '(peck-recoil :hand right :finger pinkie :r 2.24 :theta -0.46) :hand-loc 'right-home)
  (specify-key k 13 3 '("\\" "backslash") :style '(peck-recoil :hand right :finger pinkie :r 3.16 :theta -0.32) :hand-loc 'right-home)
  
  (specify-key k 15 3 '("forward-delete" "del"))
  (specify-key k 16 3 "end")
  (specify-key k 17 3 "pagedown")
  
  (specify-key k 19 3 "keypad-7" :style '(peck-recoil :hand right :finger index :r 1 :theta -1.57) :hand-loc 'keypad)
  (specify-key k 20 3 "keypad-8" :style '(peck-recoil :hand right :finger middle :r 1 :theta -1.57) :hand-loc 'keypad)
  (specify-key k 21 3 "keypad-9" :style '(peck-recoil :hand right :finger ring :r 1 :theta -1.57) :hand-loc 'keypad)
  (specify-key k 22 3 '("keypad--" "keypad-minus" "keypad-hyphen") :style '(peck-recoil :hand right :finger pinkie :r 2 :theta -1.57) :hand-loc 'keypad)
  

  ;; ASDF row
  (specify-key k 0 4 "caps-lock" :style '(peck-recoil :hand left :finger pinkie :r 1.0 :theta 3.14) :hand-loc 'left-home)
  (specify-key k 1 4 "a" :style '(punch :hand left :finger pinkie) :hand-loc 'left-home)
  (specify-key k 2 4 "s" :style '(punch :hand left :finger ring) :hand-loc 'left-home)
  (specify-key k 3 4 "d" :style '(punch :hand left :finger middle) :hand-loc 'left-home)
  (specify-key k 4 4 "f" :style '(punch :hand left :finger index) :hand-loc 'left-home)
  (specify-key k 5 4 "g" :style '(peck-recoil :hand left :finger index :r 1 :theta 0) :hand-loc 'left-home)
  (specify-key k 6 4 "h" :style '(peck-recoil :hand right :finger index :r 1 :theta 3.14) :hand-loc 'right-home)
  (specify-key k 7 4 "j" :style '(punch :hand right :finger index) :hand-loc 'right-home)
  (specify-key k 8 4 "k" :style '(punch :hand right :finger middle) :hand-loc 'right-home)
  (specify-key k 9 4 "l" :style '(punch :hand right :finger ring) :hand-loc 'right-home)
  (specify-key k 10 4 '(";" "semicolon") :style '(punch :hand right :finger pinkie) :hand-loc 'right-home)
  (specify-key k 11 4 '("'" "quote") :style '(peck-recoil :hand right :finger pinkie :r 1 :theta 0) :hand-loc 'right-home)
  (specify-key k 12 4 '("return" "newline") :style '(peck-recoil :hand right :finger pinkie :r 2 :theta 0) :hand-loc 'right-home)
  (specify-key k 13 4 '("return" "newline") :default nil)
  
  (specify-key k 19 4 "keypad-4" :style '(punch :hand right :finger index) :hand-loc 'keypad)
  (specify-key k 20 4 "keypad-5" :style '(punch :hand right :finger middle) :hand-loc 'keypad)
  (specify-key k 21 4 "keypad-6" :style '(punch :hand right :finger ring) :hand-loc 'keypad)
  (specify-key k 22 4 '("keypad-+" "keypad-plus") :style '(peck-recoil :hand right :finger pinkie :r 1 :theta -1.57) :hand-loc 'keypad)
  
  ;; Z row
  (specify-key k 0 5 '("shift" "left-shift") :style '(peck-recoil :hand left :finger pinkie :r 1.41 :theta 2.36) :hand-loc 'left-home)
  (specify-key k 1 5 "z" :style '(peck-recoil :hand left :finger pinkie :r 1 :theta 1.57) :hand-loc 'left-home)
  (specify-key k 2 5 "x" :style '(peck-recoil :hand left :finger ring :r 1 :theta 1.57) :hand-loc 'left-home)
  (specify-key k 3 5 "c" :style '(peck-recoil :hand left :finger middle :r 1 :theta 1.57) :hand-loc 'left-home)
  (specify-key k 4 5 "v" :style '(peck-recoil :hand left :finger index :r 1 :theta 1.57) :hand-loc 'left-home)
  (specify-key k 5 5 "b" :style '(peck-recoil :hand left :finger index :r 1.41 :theta 0.79) :hand-loc 'left-home)
  (specify-key k 6 5 "n" :style '(peck-recoil :hand right :finger index :r 1.41 :theta 2.36) :hand-loc 'right-home)
  (specify-key k 7 5 "m" :style '(peck-recoil :hand right :finger index :r 1 :theta 1.57) :hand-loc 'right-home)
  (specify-key k 8 5 '("," "comma") :style '(peck-recoil :hand right :finger middle :r 1 :theta 1.57) :hand-loc 'right-home)
  (specify-key k 9 5 '("." "period" "dot") :style '(peck-recoil :hand right :finger ring :r 1 :theta 1.57) :hand-loc 'right-home)
  (specify-key k 10 5 '("/" "slash") :style '(peck-recoil :hand right :finger pinkie :r 1 :theta 1.57) :hand-loc 'right-home)
  (specify-key k 11 5 "right-shift" :style '(peck-recoil :hand right :finger pinkie :r 1.41 :theta 0.78) :hand-loc 'right-home)
  (specify-key k 12 5 "right-shift" :default nil)
  (specify-key k 13 5 "right-shift" :default nil)

  (specify-key k 16 5 "up-arrow")
  
  (specify-key k 19 5 "keypad-1" :style '(peck-recoil :hand right :finger index :r 1 :theta 1.57) :hand-loc 'keypad)
  (specify-key k 20 5 "keypad-2" :style '(peck-recoil :hand right :finger middle :r 1 :theta 1.57) :hand-loc 'keypad)
  (specify-key k 21 5 "keypad-3" :style '(peck-recoil :hand right :finger ring :r 1 :theta 1.57) :hand-loc 'keypad)
  (specify-key k 22 5 '("enter" "keypad-enter") :style '(punch :hand right :finger pinkie) :hand-loc 'keypad)
  
  ;; space bar row
  (specify-key k 0 6 '("control" "left-control") :style '(peck-recoil :hand left :finger pinkie :r 2.24 :theta 2.03) :hand-loc 'left-home)
  (specify-key k 1 6 '("option" "left-option") :style '(peck-recoil :hand left :finger pinkie :r 2.0 :theta 1.57) :hand-loc 'left-home)
  (specify-key k 2 6 '("command" "left-command") :style '(peck-recoil :hand left :finger ring :r 2.0 :theta 1.57) :hand-loc 'left-home)
  (specify-key k 3 6 "space" :default nil)
  (specify-key k 4 6 "space" :default nil)
  (specify-key k 5 6 "space" :style '(punch :hand left :finger thumb) :hand-loc 'left-home)
  (specify-key k 6 6 "space" :default nil)
  (specify-key k 7 6 "space" :default nil)
  (specify-key k 8 6 "space" :default nil)
  (specify-key k 9 6 "space" :default nil)
  (specify-key k 10 6 "space" :default nil)

  (specify-key k 11 6 "right-command" :style '(peck-recoil :hand right :finger pinkie :r 2.24 :theta 1.11) :hand-loc 'right-home)
  (specify-key k 12 6 "right-option" :style '(peck-recoil :hand right :finger pinkie :r 2.83 :theta 0.78) :hand-loc 'right-home)
  (specify-key k 13 6 "right-control" :style '(peck-recoil :hand right :finger pinkie :r 3.6 :theta 0.59) :hand-loc 'right-home)

  (specify-key k 15 6 "left-arrow")
  (specify-key k 16 6 "down-arrow")
  (specify-key k 17 6 "right-arrow")
  
  (specify-key k 19 6 "keypad-0" :style '(punch :hand right :finger thumb) :hand-loc 'keypad)
  (specify-key k 20 6 "keypad-0" :default nil)
  (specify-key k 21 6 '("keypad-." "keypad-dot" "keypad-period") :style '(peck-recoil :hand right :finger ring :r 2 :theta 1.57) :hand-loc 'keypad)
  (specify-key k 22 6 '("enter" "keypad-enter") :default nil))             


(defgeneric other-notifications (keyboard device features)
  (:documentation "Called when the default keyboard can't process a notification"))

(defmethod other-notifications ((k virtual-keyboard) device features)
  (print-warning "keyboard device can not process notification from device ~s with features ~s." device features))


;;;;;;; shouldn't need to subclass this method

(defmethod specify-key ((k virtual-keyboard) x y names &key (default t) style hand-loc)
  (unless (listp names)
    (setf names (list names)))
  (if (every 'stringp names)
      (progn
        (dolist (n (mapcar 'string-upcase names))
          
          (when default
            (when (gethash n (key->loc-ht k))
              (print-warning "Key name ~s already has a location and that is being overwritten." n))
            (setf (gethash n (key->loc-ht k)) (vector x y)))
          (when (and default (listp style) style)
            (when (gethash n (key->cmd-ht k))
              (print-warning "Key name ~s already has a press-key entry which is being overwritten." n))
            (setf (gethash n (key->cmd-ht k)) style)
            (if (and hand-loc (gethash hand-loc (hand-loc-table k)))
                (setf (gethash n (key->hand-start-ht k)) hand-loc)
              (print-warning "Key name ~s specifies invalid hand location ~s which is being ignored." n hand-loc))))
        (awhen (aref (loc->key-arr k) x y)
               (print-warning "Keyboard location ~d,~d already has a name ~s which is being overwritten with ~s."
                              x y it (first names)))
        (setf (aref (loc->key-arr k) x y) (first names)))
    (print-warning "Invalid key name found in provided names ~s." names)))


;;; create the default keyboard device for models

(defvar *act-r-virtual-keyboard* (make-instance 'virtual-keyboard))


;;; The methods used to get the info out of the keyboard tables.

;;; LOC-TO-KEY      [Method]
;;; Description : Given a location, return the corresponding key name.
;;;             : Accessed via the 'virtual keyboard' array.

(defgeneric loc->key (vk loc)
  (:documentation  "Given a location, return the corresponding key"))

(defmethod loc->key ((vk virtual-keyboard) (loc list))
  (aref (loc->key-arr vk) (first loc) (second loc)))

(defmethod loc->key ((vk virtual-keyboard) (loc vector))
  (aref (loc->key-arr vk) (px loc) (py loc)))

(defgeneric key->cmd (keyboard key)
  (:documentation  "Given a key, return the appropriate Motor Module command to type it."))

(defmethod key->cmd ((k virtual-keyboard) key)
  (gethash (string-upcase key) (key->cmd-ht k)))

(defgeneric key->hand-start (keyboard key)
  (:documentation  "Given a key, return the expected starting hand location information."))

(defmethod key->hand-start ((k virtual-keyboard) key)
  (gethash (gethash (string-upcase key) (key->hand-start-ht k))
           (hand-loc-table k)))

(defgeneric key->loc (keyboard key)
  (:documentation  "Given a key, return the location vector."))

(defmethod key->loc ((vk virtual-keyboard) key)
  (gethash (string-upcase key) (key->loc-ht vk)))

(defgeneric keyboard-hand-location (keyboard location-name)
  (:documentation  "Given a location name, return the list of coordinates for the hand and fingers."))

(defmethod keyboard-hand-location ((vk virtual-keyboard) location)
  (gethash location (hand-loc-table vk)))


;; Create the keyboard device and indicate the function to call
;; when a signal is sent to it.

(defun initialize-keyboard (device-list)
  (let ((k-c (get-component keyboard-table)))
    (if k-c
        (let ((k (if (null (third device-list))
                     (bt:with-lock-held ((keyboard-component-lock k-c))
                       (let ((k (aif (keyboard-component-default k-c)
                                     (make-instance it)
                                     *act-r-virtual-keyboard*)))
                         (setf (gethash (current-model) (keyboard-component-table k-c)) k)))
                   (let ((k-type (string->name (third device-list))))
                     (if (subtypep k-type 'virtual-keyboard)
                         (bt:with-lock-held ((keyboard-component-lock k-c))
                           (setf (gethash (current-model) (keyboard-component-table k-c)) (make-instance k-type)))
                       (print-warning "Invalid type ~s provided for installing a keyboard." (third device-list)))))))
          (when k
            (home-hands k)
            t))
      (print-warning "No keyboard-table component was found when trying to install a keyboard device."))))
                

(add-act-r-command "initialize-keyboard" 'initialize-keyboard "Function which sets up a keyboard when it is installed. Do not call directly.")

(defun internal-keyboard-interface (device-list features)
  (let ((k (current-keyboard)))
    (if k
      (let ((m (second (find 'model features :key 'first)))
            (l (second (find 'loc features :key 'first)))
            (s (second (find 'style features :key 'first))))
        (if (and m l s (find s '(punch peck-recoil peck))) ;; default keyboard actions
            (aif (loc->key k l)
                 (with-model-eval m
                   (schedule-event-now "output-key" :params (list m it)  :output t :module 'keyboard  :maintenance t))
                 (print-warning "Keyboard device did not get signaled with a valid keyboard location for normal actions: ~s" features))
          (other-notifications k device-list features)))
      (print-warning "No keyboard available for current model."))))


(add-act-r-command "keyboard-interface-fn" 'internal-keyboard-interface "Function which receives the signals for the keyboard device. Do not call directly.")

(define-device "keyboard" "initialize-keyboard" nil "keyboard-interface-fn")


;; This is the command that it evaluates for monitoring purposes.

(add-act-r-command "output-key" nil "Command called when a key on the virtual keyboard is pressed by the model or a real key is pressed in a visible experiment window which can be monitored.  It is passed 2 parameters: model-name and key-name. Should not be called directly.")

(defun output-key (model key)
  (dispatch-apply "output-key" model key))


;; Create some new motor styles.  Styles are defined on the motor module, which 
;; means these require motor-module methods, and since it's available using that
;; directly for efficiency.  However, a device "should" handle actions through
;; notifications to the interface.


;; Create a press-key style which evaluates the underlying action for the indicated
;; key.  Doesn't check hand position so if it's not at the home row then the wrong
;; key will likely be pressed.  

(defclass press-key (movement-style)
  nil
  (:default-initargs
      :style-name :PRESS-KEY
    :feature-slots '(key)))

(defgeneric press-key (mtr-mod &key key request-spec)
  (:documentation  "High-level interface to press a key: Look up the command and execute it."))

(defmethod press-key ((mtr-mod motor-module) &key key request-spec)
  (if (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
      (aif (current-keyboard)
           (progn
             (unless (stringp key)
               (setf key (princ-to-string key)))
           
             (let ((command (key->cmd it key)))
               (if (null (first command))
                   (print-warning "No press-key mapping available for key ~s." key)
                 (let* ((hand-index (position :hand command))
                        (hand-name (nth (1+ hand-index) command))
                        (position (hand-position hand-name :current nil))
                        (device (device-for-hand hand-name :current nil))  ;; will it be on the keyboard?
                        (home (key->hand-start it key)))
                   (if (string-equal (second device) "keyboard")
                       (progn
                         (unless (and home
                                      (= (second (first home)) (px (hand-pos-loc position)))
                                      (= (third (first home)) (py (hand-pos-loc position)))
                                      (every (lambda (x) (awhen (find (first x) (hand-pos-fingers position) :key 'first)
                                                                (and (= (px (second it)) (second x))
                                                                     (= (py (second it)) (third x)))))
                                             (rest home)))
                           (if home
                               (print-warning "Hand not at correct starting position for press-key action.")
                             (print-warning "No starting position available for key ~s for press-key action.")))
                         (apply (first command) mtr-mod (append (list :request-spec request-spec) (rest command))))
                     (print-warning "The ~s hand in a press-key action is not on the keyboard.  Press-key action ignored." hand-name))))))
           (print-warning "Could not find a keyboard to use for the current model. Press-key action ignored."))
    (print-warning "No keyboard device installed for motor module.  Press-key action ignored.")))

(extend-manual-requests (press-key key) handle-style-request)


(defclass hand-to-home (movement-style)
  nil
  (:default-initargs
      :style-name :POINT-HAND-AT-KEY
    :feature-slots '(hand)))

(defgeneric hand-to-home (mtr-mod &key request-spec hand)
  (:documentation  "Moves a hand to the home row position"))

(defmethod hand-to-home ((mtr-mod motor-module) &key request-spec hand)
  (aif (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
      (let ((k (current-keyboard)))
        (when (null hand) 
          (setf hand 'right))
        (if k
            (if (or (eq hand 'right) (eq hand 'left))
                (let* ((keyboard-hand (if (eq hand 'left) 
                                          (keyboard-hand-location k 'left-home)
                                        (keyboard-hand-location k 'right-home)))
                       (home-pos (coerce (rest (find 'hand keyboard-hand :key 'first)) 'vector))
                       (polar (xy-to-polar (hand-loc hand :current nil) home-pos))
                       (hand-pos (hand-position hand :current nil))
                       (device (device-for-hand hand :current nil)))
                  (if (and (= (vr polar) 0)
                           device
                           (string-equal (second device) "keyboard")
                           (every (lambda (x) (awhen (find (first x) (hand-pos-fingers hand-pos) :key 'first)
                                                                (and (= (px (second it)) (second x))
                                                                     (= (py (second it)) (third x)))))
                                             (rest keyboard-hand)))
                  (model-warning "HAND-TO-HOME requested but hand already is (or will be) at the home position.") 
                  (point-hand mtr-mod :hand hand :r (vr polar) :theta (vtheta polar) :twidth 4.0 :device it
                              :offsets (remove (find 'hand keyboard-hand :key 'first) keyboard-hand) 
                              :request-spec request-spec)))
              (print-warning "Invalid hand ~s in hand-to-home." hand))
          (print-warning "Invalid keyboard found cannot move hand to home position.")))
    (print-warning "Keyboard device is not installed. Cannot move hand to home position.")))

(extend-manual-requests (hand-to-home hand) handle-partial-style-request)


(defgeneric hand-to-keypad (mtr-mod request)
  (:documentation  "Moves the right hand to the keypad position"))

(defmethod hand-to-keypad ((mtr-mod motor-module) request)
  (aif (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
      (let ((k (current-keyboard)))
        (if k
            (let* ((keypad-location (keyboard-hand-location k 'keypad))
                   (keypad-pos (coerce (rest (find 'hand keypad-location :key 'first)) 'vector))
                   (polar (xy-to-polar (hand-loc 'right :current nil) keypad-pos))
                   (hand-pos (hand-position 'right :current nil))
                   (device (device-for-hand 'right :current nil)))
              (if (and (= (vr polar) 0)
                       device
                       (string-equal (second device) "keyboard")
                       (every (lambda (x) (awhen (find (first x) (hand-pos-fingers hand-pos) :key 'first)
                                                 (and (= (px (second it)) (second x))
                                                      (= (py (second it)) (third x)))))
                              (rest keypad-location)))
                  (model-warning "HAND-TO-KEYPAD requested but hand already is (or will be) at the keypad position.") 
                (point-hand mtr-mod :hand 'right :r (vr polar) :theta (vtheta polar) :twidth 4.0 :device it
                            :offsets (remove (find 'hand keypad-location :key 'first) keypad-location) :request-spec request)))
          (print-warning "Invalid keyboard found cannot move right hand to keypad position.")))
    (print-warning "Keyboard device is not installed. Cannot move right hand to keypad position.")))

(extend-manual-requests (hand-to-keypad) handle-simple-command-request)

(defclass point-hand-at-key (movement-style)
  nil
  (:default-initargs
      :style-name :POINT-HAND-AT-KEY
    :feature-slots '(hand to-key offsets)))

(defgeneric point-hand-at-key (mtr-mod &key hand to-key offsets request-spec)
  (:documentation  "Move the hand to a new location, specified by the key at which the index finger should point."))

(defmethod point-hand-at-key ((mtr-mod motor-module) &key hand to-key offsets request-spec)
  (aif (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
       (let ((k (current-keyboard)))
         (if k
             (progn
               (unless (stringp to-key)
                 (setf to-key (princ-to-string to-key)))
               
               (let ((new-loc (key->loc k to-key)))
                 (if new-loc
                     (let* ((polar (xy-to-polar (hand-loc hand :current nil) new-loc))
                            (keyboard-hand (if (eq hand 'left) 
                                               (keyboard-hand-location k 'left-home)
                                             (keyboard-hand-location k 'right-home)))
                            (target-offsets (if (eq offsets 'standard)
                                                (mapcar (lambda (x)
                                                          (list (first x) (px (second x)) (py (second x))))
                                                  (case hand
                                                    (right 
                                                     (right-standard-offsets))
                                                    (left
                                                     (left-standard-offsets))))
                                              (if offsets offsets (rest keyboard-hand))))
                            
                            (hand-pos (hand-position hand :current nil))
                            (device (device-for-hand hand :current nil)))
                       (if (and (= (vr polar) 0)
                                device
                                (string-equal (second device) "keyboard")
                                (every (lambda (x) (awhen (find (first x) (hand-pos-fingers hand-pos) :key 'first)
                                                          (and (= (px (second it)) (second x))
                                                               (= (py (second it)) (third x)))))
                                       target-offsets))
                      (model-warning "POINT-HAND-AT-KEY requested but hand already is (or will be) at the requested key position.")
                         (point-hand mtr-mod :hand hand :r (vr polar) :theta (vtheta polar) 
                                     :offsets offsets :request-spec request-spec :device it)))
                   (print-warning "No key mapping available for key ~s. Point-hand-at-key action ignored." to-key))))
           (print-warning "Invalid keyboard found cannot move hand to key.")))
       (print-warning "No keyboard device installed for motor module.  Point-hand-at-key action ignored.")))

(extend-manual-requests (point-hand-at-key hand to-key offsets) handle-partial-style-request)



;;; User actions for the keyboard go through the motor module's notification interface.

;;; HOME-HANDS      [Method]
;;; Date        : 97.02.20
;;; Description : Sets the hand and finger locations to the keyboard home row.

(defgeneric home-hands (keyboard &optional force-hand-to-device)
  (:documentation  "Sets the hand and finger locations to home row locations"))

(defmethod home-hands ((k virtual-keyboard) &optional force-hand-to-device)
  (let ((device (find "keyboard" (current-devices "motor") :test 'string-equal :key 'second)))
    (when (or force-hand-to-device (null (notify-interface "motor" '(get-hand-device right))))
      (let* ((r-home (keyboard-hand-location k 'right-home))
             (r-pos (find 'hand r-home :key 'first))
             (r-fingers (remove r-pos r-home)))
        (notify-interface "motor" (list 'set-hand-device 'right device))
        (notify-interface "motor" (list 'set-hand-position 'right (second r-pos) (third r-pos)))
        (notify-interface "motor" (append (list 'set-finger-offset 'right) r-fingers))))
      
    (when (or force-hand-to-device (null (notify-interface "motor" '(get-hand-device left))))
      (let* ((l-home (keyboard-hand-location k 'left-home))
             (l-pos (find 'hand l-home :key 'first))
             (l-fingers (remove l-pos l-home)))
        (notify-interface "motor" (list 'set-hand-device 'left device))
        (notify-interface "motor" (list 'set-hand-position 'left (second l-pos) (third l-pos)))
        (notify-interface "motor" (append (list 'set-finger-offset 'left) l-fingers))))))


(defun start-hands-at-home ()
  "Start the hands on the home row locations"
  (verify-current-model 
   "No current model.  Cannot start hands at home positions."
   
   (unless (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
     (install-device (list "motor" "keyboard"))
     (model-warning "Default keyboard device installed automatically by start-hands-at-home"))
   
   (home-hands (current-keyboard) t)
   t))

(add-act-r-command "start-hands-at-home" 'start-hands-at-home "Place the model's hands on the home row positions of a keyboard device (installing one if necessary). No params.")


(defun start-hand-at-key (hand key)
  "Start a hand on the indicated key instead of the 'home row' location"
   (verify-current-model 
    "No current model.  Cannot start hand at key."
    (unless (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
      (install-device (list "motor" "keyboard"))
      (model-warning "Default keyboard device installed automatically by start-hand-at-key."))
    
    (let ((k (current-keyboard)))
      (unless (stringp key)
        (setf key (princ-to-string key)))
      (if (or (eq hand 'right) (eq hand 'left))
          (aif (key->loc k key)
               (progn
                 (notify-interface "motor" (list 'set-hand-device hand (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)))
                 (notify-interface "motor" (list 'set-hand-position hand (px it) (py it))))
               (print-warning "Invalid key ~s in start-hand-at-key." key))
        (print-warning "Invalid hand ~s in start-hand-at-key." hand)))))


(defun external-start-hand-at-key (hand key)
  (start-hand-at-key (string->name hand) key))

(add-act-r-command "start-hand-at-key" 'external-start-hand-at-key "Place the model's indicated hand on the keyboard device with the index finger over the named key. Params: hand key-name")


(defun start-hand-at-keypad ()
  "Starts the right hand on the keypad instead of the 'home row' location"
  (verify-current-model 
    "No current model.  Cannot start hand at key."
    (unless (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)
      (install-device (list "motor" "keyboard"))
      (model-warning "Default keyboard device installed automatically by start-hand-at-keypad."))
    
   (let* ((k (current-keyboard))
          (keypad-loc (keyboard-hand-location k 'keypad))
          (pos (find 'hand keypad-loc :key 'first))
          (fingers (remove pos keypad-loc)))
     (notify-interface "motor" (list 'set-hand-device 'right (find "keyboard" (current-devices "motor") :key 'second :test 'string-equal)))
     (notify-interface "motor" (list 'set-hand-position 'right (second pos) (third pos)))
     (notify-interface "motor" (append (list 'set-finger-offset 'right) fingers)))))
                 
(add-act-r-command "start-hand-at-keypad" 'start-hand-at-keypad "Place the model's right hand on middle row of the keyboard device's keypad area. No params.")



;;;;;; Create a component for recording the keyboards for each model


(defun clear-keyboard-component (keyboard-component)
  (bt:with-lock-held ((keyboard-component-lock keyboard-component))
    (clrhash (keyboard-component-table keyboard-component))))

(defun remove-model-keyboard (k-c model)
  (bt:with-lock-held ((keyboard-component-lock k-c))
    (remhash model (keyboard-component-table k-c))))

(defun set-default-keyboard (class)
  (let ((k-c (get-component keyboard-table)))
    (if k-c
        (if (or (null class) (subtypep class 'virtual-keyboard))
            (progn
              (bt:with-lock-held ((keyboard-component-lock k-c))
                (setf (keyboard-component-default k-c) class))
              t)
          (print-warning "Set-default-keyboard requires nil or a subclass of virtual-keyboard but given ~s." class))
      (print-warning "No keyboard-table component found when calling set-default-keyboard."))))
               
(defun current-keyboard ()
  (awhen (current-model)
         (let ((k-c (get-component keyboard-table)))
           (when k-c
             (bt:with-lock-held ((keyboard-component-lock k-c))
               (gethash it (keyboard-component-table k-c)))))))

(define-component keyboard-table :version "3.1" :documentation "Record the keyboards used for each model."
  :creation make-keyboard-component
  :clear-all clear-keyboard-component
  :delete clear-keyboard-component
  :delete-model remove-model-keyboard)

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