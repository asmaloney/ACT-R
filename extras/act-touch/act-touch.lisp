;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c) 2012-4 Cogscent, LLC
;;; Availability: GNU LGPL, see LGPL.txt
;;; Address     : Cogscent, LLC
;;; 		: PMB 7431
;;;		: 2711 Centerville Rd, Ste 120
;;;		: Wilmington DE, USA 19808-1676
;;;		: frank.tamborello@cogscent.com
;;;
;;; Disclaimer	:     This library is free software; you can redistribute it and/or
;;;		: modify it under the terms of the GNU Lesser General Public
;;;		: License as published by the Free Software Foundation; either
;;;		: version 2.1 of the License, or (at your option) any later version.
;;;		:     This library is distributed in the hope that it will be useful,
;;;		: but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;		: MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;		: Lesser General Public License for more details.
;;;		:     You should have received a copy of the GNU Lesser General Public
;;;		: License along with this library; if not, write to the Free Software
;;;		: Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;		: 
;;; 
;;; Acknowledgements
;;;		: This research is sponsored by Measurement Science and 
;;;		Engineering grant 60NANB12D134 from the 
;;;		National Institute of Standards and Technology (NIST).
;;;		Special acknowledgements are due to Dr. Ross Micheals and 
;;;		Dr. Kristen K. Greene of NIST's Information Technology 
;;;		Laboratory.
;;;		Thanks also to Dr. Michael D. Byrne, upon whose experiment 
;;;		library code I based the device code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : act-touch.lisp
;;; Revision    : 16
;;; 
;;; Description : Add a new device called "touch" which represents a touch screen/pad 
;;;             : and defines new motor actions for using that device.
;;;             
;;;
;;; Usage	: Call (require-extra "act-touch") in the model file which needs it.
;;; 
;;; Bugs        : None known
;;;
;;; To do       : [ ] Add checks to tap-hold and tap-release for current finger state
;;;             :     and warn when inconsistent.
;;; 
;;;
;;; Issues	:
;;; 2014.10.05 fpt
;;;		: 1. Long-term, it'd probably make more sense to add a z-dimension to hand locations.
;;;		For now movement time methods use "z-index", a slot of the device object.
;;;
;;; 2012.06.14 fpt
;;; 		: 1. What's the target of a swipe? Is it the entire side of the
;;;		device?
;;; 2012.06.14 fpt
;;;		: 2. There's something funny about using Fitts' Law to compute
;;;		pinch execution time. How wide should the target be? That's sort
;;;		of a nonsensical question to ask in the context of a pinch, but
;;;		Fitts' Law requires a width and on the surface the pinch movement
;;;		seems fairly corrected-ballistic like other pointing movements.
;;;		For now let's assume that the thumb is the target-yes, even for
;;;		reverse-pinches-and that the typical human thumb is 1 inch (72px
;;;		@ 72ppi) wide. So the pinch class has a slot for thumb-width
;;;		initializing to 72, which gets passed to the fitts function for 
;;;		calculating pinch execution time.
;;; 2012.08.18 fpt
;;;		: 3. Does it make more sense to construct two-, three-, and 
;;;		four-fingered swipes as different movement styles instead of
;;;		one movement style with a feature specifying the number of 
;;;		fingers?
;;; 2012.08.18 fpt
;;;		: 4. What about qualitatively different swipes used for 
;;;		scrolling precisely starting and ending within one page
;;;		vs page turning?
;;; 
;;; ----- History -----
;;; 2012.06.01 fpt 1
;;;		: Inception: Extend the motor module with a movement style that 
;;;		allows ACT-R to respond to that style's request and produce some
;;;		model output: swipe.
;;; 2012.06.02 fpt 2
;;;		: Implement other movement styles: tap, pinch (also works for 
;;;		reverse-pinch), and rotate.
;;; 2012.06.07 fpt 3
;;;		: Gave tap its real execution time computation, removed the r &
;;;		theta features since it taps in place, and gave it a stub device
;;;		method.
;;; 2012.06.14 fpt 4
;;;		: Defined additional multitouch movement styles with assumed-to-
;;;		be-sensible time computations: peck-tap, peck-tap-recoil, tap-
;;;		hold, tap-release, tap-drag-release, swipe, pinch, rotate.
;;; 2012.06.19 fpt 5
;;;		: The Device is now instrumented to record things like time and
;;;		location of taps. It uses the virtual-experiment-window library,
;;;		adapted by me from Mike Byrne's experiment-window4 library.
;;;
;;; 2012.06.29 fpt 6
;;; Demo task is now a procedure-window rather than a timed-exp-window so that the
;;; scope of one "trial" is one performance of one multi-action task rather than
;;; one performance of one action.
;;;
;;; 2012.07.13 fpt 7
;;; Move-hand-touch allows the model to move its hand to a visual-location or
;;; a visual-object, just like move-cursor. ...except that noisy movement
;;; is not yet implemented.
;;;
;;; 2012.08.18 fpt 8
;;; Wrote a feat-differences method for tap since it didn't inherit it from a parent 
;;; movement-style.
;;;
;;; 2012.08.26 fpt 9
;;; Increased the length of the procedure from 1 to 8 steps & made some of the steps
;;; require other gestures besides tap, namely swipe, rotate, & pinch.
;;;
;;; 2012.09.29 fpt 10
;;; 1. Changed license from public domain to LGPL.
;;; 2. Forked the virtual multitouch display device and 
;;;demonstration model into their own files.
;;;
;;; 2013.04.03 fpt 11
;;; Quickly hacked move-hand-touch so it could generate some noisy movements
;;; probability of a miss considering index finger tip area and target area
;;;
;;; 2013.11.21 fpt 12
;;; 1. Incorporate Melissa Gallagher's dual-distribution noisy movement mechanism,
;;; based on May & Byrne.
;;; 2. Push ':virtual-multitouch-device onto *features* to ease checking for & 
;;; loading
;;;
;;; 2014.10.04 fpt 13
;;; Update to make compatible with ACT-R 6.1:
;;; 1. Move-hand-touch conforms to the new argument list of verify-single-explicit-
;;; value introduced with ACT-R 6.1.
;;;
;;; 2. A new abstraction became available in ACT-R 6.1, handle-style-request,
;;; to schedule the event for a defined movement style. Now all ACT-Touch's
;;; defined movement styles, such as tap, use that instead of their own
;;; request methods.
;;;
;;; 2014.12.01 Dan Bothell
;;; Added an (unless (fboundp 'aif) ...) around the macro definition since it
;;; should already be defined in the main ACT-R code.
;;;
;;; 2015.05.20 Dan Bothell
;;; * Use the device specific coordinate names (even though this has its own
;;; device) and make sure that xy-loc calls pass the vision module in as well.
;;; 
;;; * The approach-width call in move-hand-touch needs to pass the vision module now.
;;;
;;; 2015.06.05 Dan Bothell
;;; * Changed all scheduling calls to use milliseconds instead of seconds to
;;; avoid the scheduler doing the conversion which requires all the safety checks.
;;;
;;; 2015.08.14 Dan Bothell
;;; * Changed rand-time calls to randomize-time since rand-time was depricated
;;;   a while ago and I've finally decided to remove it.
;;; 2015.09.23 Dan Bothell
;;; * Updated the actions which don't use the "default" style mechanisms to
;;;   complete the request for use with the new utility learning credit assignment
;;;   approach.
;;; 2016.03.14 Dan Bothell
;;; * Added the provide and a compile-and-load for the multi-touch device so that
;;;   require-extra gets both.
;;;
;;; 2018.10.24 Dan Bothell
;;; * First step before updating for 7.x is to apply some changes which were made
;;;   to the 7.0 version as noted in these comments:
;;;
;;;    2017.11.04 fpt 15
;;;    I deleted index-z so that act-touch now works with devices other than objects
;;;    with an index-z slot, such as ACT-Droid.
;;;
;;;    2018.02.06 fpt 16
;;;    Implement a new feature to allow the model to "consciously" control swiping
;;;    swipe-speed: an integer from the set [1,5], where 1 is slowest and 5 is fastest.
;;;
;;;    Note that this update breaks old code calling the swipe movement command
;;;    without specifying a swipe-speed feature: ACT-R simply won't execute the swipe
;;;    command, instead proceeding otherwise.
;;; 
;;;    Note: I lack a principled way to map user intention of swiping speed to
;;;    computing its execution time, so I'll just do what's expedient and otherwise
;;;    seems at least not stupidly unreasonable: I give the Fitts function a
;;;    coefficient of difficulty that decreases with swipe-speed:
;;;    (/ 1 (expt (swipe-speed self) 2))
;;;
;;;    Note: I think swipe should allow for a nice, wide target parameter for the
;;;    Fitts function, like the entire side of the device to which the model is
;;;    swiping. For when you run with a NIL device, EG for testing, I gave it a
;;;    value of 500. Replace that in compute-exec-time with a call to the width
;;;    of your target.
;;;
;;; 2018.10.25 Dan Bothell
;;; * To better support some of those changes, also added a module so that one
;;;   can specify parameters for finger height, thumb width, default swipe speed, 
;;;   and swipe target width and so that the user setting of :pixels-per-inch is 
;;;   used instead of assuming 72.
;;; * Removed *index-finger-tip-size* since it's not used anywhere.
;;; * Added a hand slot to move-hand-touch since all the other commands allow for
;;;   either hand to perform the action, but there's no way for the model to move
;;;   its left hand to do them.
;;; * Added the width for Fitts law to the index-thumb movements, using a thumb
;;;   width for all of them as noted in issue 2.
;;; * Use the indicated hand and finger for the exec-time of a rotate, but why 
;;;   is the distance for Fitts law the distance between finger and thumb and 
;;;   not the distance traveled to rotate by the indicated angle?
;;;   Changed that so the distance is the distance traveled and the target width
;;;   is 5 degrees of the movement (target angle +/- 2.5 degrees which could
;;;   be made noisy and parameterized but keeping it simple for now).
;;; * Finger offsets need to be adjusted when initially moving the hand because
;;;   the default is a distance of 1 from each other (which is pixels for this 
;;;   interface).  Added another parameter, finger-spacing which is the distance
;;;   in inches between adjacent fingers, and the thumb is two down and one
;;;   over from the index finger (same as the default offsets for the keyboard 
;;;   but now in finger spaces).  Those are then converted to pixels for the
;;;   offsets.  Using 3/4" as the default based on the default keyboard spacing
;;;   and adding a start-hand-at-touch command to handle setting things 
;;;   appropriately.
;;; 2018.10.26 Dan Bothell [2.0] 
;;; * Track the module version for my updates.
;;; * Start the actual 7.x changes.
;;; * Add the ACT-R package checks.
;;; * Don't load the virtual-multitouch-device file anymore.
;;; * Don't bother checking for aif because it's always available with ACT-R. 
;;; * Use the module to hold the device's info as well -- only one touch device
;;;   per model and each model has its own.
;;; * Add a lock to the module and protect everything with that lock.
;;; * Keep track of finger positions directly in the module/device because the
;;;   location of the device needs to be in the standard coordinates (keyspaces)
;;;   so that moving the hands to/from the device is timed appropriately.  Could
;;;   still use the motor module's finger offsets for positions, but this is a
;;;   simpler approach.  Make the position for a touch device 6,9 (below the
;;;   space bar roughly where a touch pad on a laptop would be).
;;; * Use undocumented internal AGI functionality to access window info to both
;;;   check that something is an existing window and get its shape info.
;;; 2018.10.29 Dan Bothell
;;; * Add the signals that are generated by the device.
;;; * !! Currently using the same width of 4 as other devices for moving the 
;;;   hand to it, but should that be determined from an associated window and/or
;;;   be configurable?
;;; * !! No way to know if the hand moves away from the device -- general issue
;;    that will need to be fixed and then updated here!
;;; * Added the hand-to-touch request.
;;; 2018.10.30 Dan Bothell
;;; * Added the touch-drag-delay parameter to slow down move-hand-touch actions
;;;   if any finger on that hand is down with a value of .8 being the default for
;;;   t which works well for mouse moves to match the data from:
;;; 
;;;   I. Scott MacKenzie, Abigail Sellen, and William A. S. Buxton. 1991. 
;;;   A comparison of input devices in element pointing and dragging tasks.
;;;   In Proceedings of the SIGCHI Conference on Human Factors in 
;;;   Computing Systems (CHI '91), Scott P. Robertson, Gary M. Olson,
;;;   and Judith S. Olson (Eds.). ACM, New York, NY, USA, 161-166. 
;;;   DOI: http://dx.doi.org/10.1145/108844.108868 
;;;
;;; * Record the peck and cursor fitts-coeff params in touch so don't need to
;;;   grab two locks in exec-time computations.
;;; * Added a rotation target-width parameter.
;;; * Added all the finger position changes to the output events with some
;;;   simple assumptions.
;;; 2018.10.31 Dan Bothell
;;; * Cleaned up the device and motor action code so that the motor actions
;;;   work for other installed devices.
;;; * Start-hand-at-touch notifies motor of the change instead of calling the
;;;   motor functions directly.
;;; 2018.11.01 Dan Bothell
;;; * Finished the separation of the device from the motor commands.
;;; * Only notify the device of finger positions/moves if there is a device.
;;; 2018.11.02 Dan Bothell
;;; * Need to not hold the module's lock when notifying since the device's
;;;   interface function also needs it and just splitting it into separate 
;;;   device and parameter locks won't work since the interface function needs
;;;   access to both types of info.
;;; * Changed the AGI window notifications to be showfinger and removefinger
;;;   to make it easier for the Environment visible virtuals to handle them.
;;; * Extra safety check required because exp window needs to be installed
;;;   to be usable with touch device.
;;; 2018.11.05 Dan Bothell
;;; * A release shouldn't be charged for the Fitts' time since it's not a 
;;;   targeted action.  Instead, it's now just assessed a burst cost.
;;; * Similarly, the tap/tap-hold should really have a target so that there's
;;;   a real width for the motion instead of the default which is actually
;;;   a single pixel since the distance in pixels is used.  To adjsut for that,
;;;   using the distance in DVA instead.
;;; * Remove-touch tests that the window is still installed because there are
;;;   possible ordering issues on how they're removed (but could that leave
;;;   the images unnecessarily?).
;;; * Always round the x and y coords when setting them.
;;; 2018.11.06 Dan Bothell
;;; * Fixed a bug with specifying the hand in move-hand-touch-request.
;;; * Only apply the drag-delay if the hand is using a touch device which 
;;;   reports that there is a finger down.
;;; 2018.11.08 Dan Bothell
;;; * Provide an external command for start-hand-at-touch.
;;; 2020.01.13 Dan Bothell [2.1]
;;; * Remove lambda from module interface functions.
;;; 2020.03.30 Dan Bothell [3.0]
;;; * Apply the fixes needed to address the preparation/processor free issues.
;;; * Requires using the hand-pos struct instead of just being able to interact
;;;   with the interface.
;;; * Start-hand-at-touch actually moves the hand to the appropriate position.
;;; * Index-thumb uses any finger and passes that to the interface (basically 
;;;   a one finger swipe).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;; Feature check if someone wants it, but require-extra is the recommended
;; way to load it which will only load if needed from the default location.

(pushnew :act-touch *features*)


;;; Module to support parameters and device internals.

(defstruct touch-module installed window right-start left-start left-start-default right-start-default 
  (fingers (make-array '(2 5))) ; hand left=0 right=1,finger index=0 -- thumb=4 value is list x,y,z (z is 0 (down) or 1 (up))
  (hands (make-array 2)) ; t/nil is hand on device?
  show-fingers
  (position #(6 9))  
  height swipe-speed swipe-width ppi thumb-width finger-spacing 
  noise drag-delay
  cursor-coeff
  peck-coeff
  rotate-width
  (lock (bt:make-recursive-lock "touch")))


;;; support code and name to index functions

(defgeneric noisy-loc-em? (xy-loc w theta)
  (:documentation "If the Motor Module is set up for it, make the output location noisy."))
  
;;; NOISY-LOC-em?      [Method]
;;; Description : Adds noise to the output location if noise is on.   
;;;             : Rather than adding the same amount of error on both axis, more is on axis than 
;;;             : off axis.  uses weighted-error to determine how much it is
;;;

(defmethod noisy-loc-em? ((xy-loc vector) (w number) (theta number))
  (if (zerop w)
      xy-loc
    (weighted-error xy-loc w theta)))

;;; WEIGHTED-ERROR      [Method]
;;; Description : samples from the normal distribution with different error on axis and off axis   
;;;             : based on work by may and byrne (see eq 6)
;;;             : Treats the error area as a circle with a diameter of pixw

(defmethod weighted-error ((xy-loc vector) (pixw number) (theta number))
  ;(model-output "pixw: ~a	theta: ~a~%" pixw theta)
  (let ((on-noise (act-r-noise (*  (/ pixw 4.133) (/ (sqrt 3) pi)))) 
        (off-noise (act-r-noise (* 0.75 (/ pixw 4.133) (/ (sqrt 3) pi)))))
    (polar-move-xy
     xy-loc 
     (vector 
      (sqrt 
       (+ (* on-noise on-noise) (* off-noise off-noise))) 
      (+ theta (atan (/ off-noise on-noise)))))))


(defun valid-hand (h)
  (case h
    (left 0)
    (right 1)))

(defun valid-finger (f)
  (case f
    (index 0)
    (middle 1)
    (ring 2)
    (pinkie 3)
    (thumb 4)))


;;; Module code

(defun touch-params (module param)
  (bt:with-recursive-lock-held ((touch-module-lock module))
    (if (consp param)
        (case (car param)
          (:finger-height 
           (setf (touch-module-height module) (cdr param)))
          (:swipe-speed
           (setf (touch-module-swipe-speed module) (cdr param)))
          (:swipe-width
           (setf (touch-module-swipe-width module) (cdr param)))
          (:thumb-width
           (setf (touch-module-thumb-width module) (cdr param)))
          (:finger-spacing
           (setf (touch-module-finger-spacing module) (cdr param)))
          (:pixels-per-inch
           (setf (touch-module-ppi module) (cdr param)))
          (:left-start
           (setf (touch-module-left-start-default module) (cdr param)))
          (:right-start
           (setf (touch-module-right-start-default module) (cdr param)))
          (:show-fingers
           (setf (touch-module-show-fingers module) (cdr param)))
          (:touch-noise
           (setf (touch-module-noise module) (cdr param)))
          (:touch-drag-delay
           (setf (touch-module-drag-delay module) (cdr param)))
          (:cursor-fitts-coeff
           (setf (touch-module-cursor-coeff module) (cdr param)))
          (:peck-fitts-coeff
           (setf (touch-module-peck-coeff module) (cdr param)))
          (:touch-rotate-width
           (setf (touch-module-rotate-width module) (cdr param))))      
      (case param
        (:finger-height 
         (touch-module-height module))
        (:swipe-speed
         (touch-module-swipe-speed module))
        (:swipe-width
         (touch-module-swipe-width module))
        (:finger-spacing
         (touch-module-finger-spacing module))
        (:thumb-width
         (touch-module-thumb-width module))
        (:left-start
         (touch-module-left-start-default module))
        (:right-start
         (touch-module-right-start-default module))
        (:show-fingers
         (touch-module-show-fingers module))
        (:touch-noise
         (touch-module-noise module))
        (:touch-drag-delay
         (touch-module-drag-delay module))
        (:touch-rotate-width
           (touch-module-rotate-width module))))))


(defun create-touch-module (x)
  (declare (ignore x))
  (make-touch-module))

(defun touch-hand-start-value-test (x)
  (and (listp x)
       (= (length x) 2)
       (every 'integerp x)))

(defun swipe-speed-value-test (x)
  (and (integerp x) (<= 1 x 5)))

(define-module-fct :touch-interface nil
  (list
   (define-parameter :finger-height :default-value 1 :valid-test 'posnum :warning "A positive number" :documentation "Default height of the finger above the screen in inches.")
   (define-parameter :swipe-speed :default-value 3 :valid-test 'swipe-speed-value-test :warning "An integer from 1-5" :documentation "Default speed for a swipe action.")
   (define-parameter :swipe-width :default-value 500 :valid-test 'posnum :warning "A positive number" :documentation "Default width of the target for computing the cost with Fitts law in pixels.")
   (define-parameter :thumb-width :default-value 1 :valid-test 'posnum :warning "A positive number" :documentation "Width of the model's thumb in inches used for computing the cost with Fitts law in pinch and rotate actions.")
   (define-parameter :finger-spacing :default-value .75 :valid-test 'posnum :warning "A positive number" :documentation "Distance between finger tips measured in inches.")
   (define-parameter :left-start :default-value (list 0 300) :valid-test 'touch-hand-start-value-test
     :warning "A list of two integers" :documentation "Default starting location for left index finger.")
   (define-parameter :right-start :default-value (list 1024 300) :valid-test 'touch-hand-start-value-test
     :warning "A list of two integers" :documentation "Default starting location for right index finger.")
   (define-parameter :show-fingers :default-value nil :valid-test 'tornil :warning "t or nil" :documentation "Whether to send finger information to the associated experiment window for possible display.")
   (define-parameter :touch-noise :default-value nil :valid-test 'tornil :warning "t or nil" :documentation "Whether to add noise to move-hand-touch target location.")
   (define-parameter :touch-drag-delay
       :valid-test 'posnumorbool
     :warning "T, nil, or a positive number"
     :default-value nil
     :documentation "Control order increment for touch drag actions.")
   (define-parameter :touch-rotate-width
       :valid-test 'posnum
     :warning "positive number"
     :default-value 5
     :documentation "Size of the target width for a rotate action measured in degrees of the rotation.")
   (define-parameter :pixels-per-inch :owner nil)
   (define-parameter :cursor-fitts-coeff :owner nil)
   (define-parameter :peck-fitts-coeff :owner nil))
  :creation 'create-touch-module
  :params 'touch-params
  :version "3.0"
  :documentation "Module to support the Touch device and corresponding motor extensions.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User commands


;;; Use this to position the hand before running the model on a touch device

(defun start-hand-at-touch (&optional (hand 'right))
  (verify-current-model 
   "No current model when trying to use start-hand-at-touch."
   (let ((installed-touch (find "touch" (current-devices "motor") :test 'string-equal :key 'second)))
     (unless installed-touch
       (print-warning "Installing a default touch device for start-hand-at-touch.")
       (setf installed-touch (install-device '("motor" "touch"))))
     (let ((module (get-module :touch-interface)))
       (if module
           (if (valid-hand hand)
               (let ((pos (bt:with-lock-held ((touch-module-lock module)) (touch-module-position module))))
                 
                 (notify-interface "motor" (list 'set-hand-device hand installed-touch))
                 
                 (notify-interface "motor" (list 'set-hand-position hand (px pos) (py pos)))
                 )
             (print-warning "Invalid hand ~s in call to start-hand-at-touch." hand))
         (print-warning "Touch interface not available. Could not start hands there."))))))


(defun external-start-hand-at-touch (&optional (hand 'right))
  (start-hand-at-touch (decode-string-names hand)))

(add-act-r-command "start-hand-at-touch" 'external-start-hand-at-touch "Move a model's hand to the 'touch' device before running it. Params: { [ right | left ]}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The actual device

(defun install-touch (device-list)
  (let ((interface-name (first device-list))
        (details (third device-list)))
    (if (string-equal interface-name "motor")
        (let ((touch (get-module :touch-interface)))
          (bt:with-recursive-lock-held ((touch-module-lock touch))
            (if (touch-module-installed touch)
                (print-warning "A touch device is already installed and only one can be installed at a time.")
              (cond ((null details)
                     (setf (touch-module-window touch) nil)
                     (setf (touch-module-left-start touch)
                       (touch-module-left-start-default touch))
                     (setf (touch-module-right-start touch)
                       (touch-module-right-start-default touch))
                     (setf (aref (touch-module-hands touch) 0) nil)
                     (setf (aref (touch-module-hands touch) 1) nil)
                     (setf (touch-module-installed touch) device-list))
                    ((and (listp details)
                          (= (length details) 2)
                          (every (lambda (x)
                                   (listp x)
                                   (= (length x) 2)
                                   (every 'integerp x))
                                 details))
                     (setf (touch-module-window touch) nil)
                     (setf (touch-module-left-start touch)
                       (first details))
                     (setf (touch-module-right-start touch)
                       (second details))
                     (setf (aref (touch-module-hands touch) 0) nil)
                     (setf (aref (touch-module-hands touch) 1) nil)
                     (setf (touch-module-installed touch) device-list))
                    (t
                     (aif (determine-exp-window (agi-component) details)
                          (if (find details (current-devices "vision") :test 'equalp)
                              (progn
                                (setf (touch-module-window touch) details)
                                (setf (touch-module-left-start touch)
                                  (list (x-pos it) (+ (y-pos it) (round (height it) 2))))
                                (setf (touch-module-right-start touch)
                                  (list (+ (x-pos it) (width it)) (+ (y-pos it) (round (height it) 2))))
                                (setf (aref (touch-module-hands touch) 0) nil)
                                (setf (aref (touch-module-hands touch) 1) nil)
                                (setf (touch-module-installed touch) device-list))
                            (print-warning "Experiment window for touch device must be installed before the touch device.  Touch device not installed."))
                          (print-warning "Invalid details ~s provided when installing a touch device.  No device installed." details)))))))
      (print-warning "Touch device can only be installed for the motor interface, but given ~s." interface-name))))

(add-act-r-command "install-touch-device" 'install-touch "Internal command for installing a touch device. Do not call.")

(defun remove-touch (device-list)
  (declare (ignore device-list))
  (let ((touch (get-module :touch-interface)))
    (bt:with-recursive-lock-held ((touch-module-lock touch))
      (setf (touch-module-installed touch) nil)
      (awhen (and (touch-module-show-fingers touch) (touch-module-window touch)
                  (find (touch-module-window touch) (current-devices "vision") :test 'equalp))
             (dolist (h '(left right))
               (when (aref (touch-module-hands touch) (valid-hand h))
                 (dolist (x '(index middle ring pinkie thumb))
                   (notify-device it (list "removefinger" (string-downcase (symbol-name h)) (string-downcase (symbol-name x))))))))
      (setf (aref (touch-module-hands touch) 0) nil)
      (setf (aref (touch-module-hands touch) 1) nil))
    t))

(add-act-r-command "remove-touch-device" 'remove-touch "Internal command for uninstalling a touch device. Do not call.")

;; The device calls the actions which can be monitored, and records the finger positions.
;; In this way the extensions operate completely through notifications which means it's
;; possible for a different device to respond to those actions and you don't
;; have to install a touch device to actually use the new motor actions.
;;
;; The touch device also sends these notifications to an associated experiment window:
;; (showfinger <hand> <finger> x y z) for any move or up/down change
;; (removefinger <hand> <finger>) when hand moves away from device

(defun touch-device-interface (device-list features)
  ;; possible notifications
  ;; (finger-position hand finger)
  ;; (set-finger-position hand finger x y)
  ;; (set-finger-up hand finger)
  ;; (set-finger-down hand finger)
  ;; (finger-down hand finger)
  ;; (any-finger-down hand)
  ;; Rest will be a list with all of these:
  ;; ( (style { tap | tap-hold | tap-release | swipe | move-hand-touch | index-thumb | pinch | rotate} (hand {left | right}) 
  ;; and some of these as appropriate for the style:
  ;; (finger { index | middle | ring | pinkie | thumb}) (loc <> <>) (distance <>) (speed <>) (direction <>) (count <>)) (range <> <>))
  ;;
  (let ((touch (get-module :touch-interface))) ; since only one per model makes it easy
    (bt:with-recursive-lock-held ((touch-module-lock touch))
      (if (null (touch-module-installed touch))
          (print-warning "Notification ~s sent to touch device ~s but it is not currently installed." features device-list)
        (cond ((and (eq (first features) 'finger-position)
                    (= (length features) 3)
                    (valid-hand (second features))
                    (valid-finger (third features)))
               (subseq (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features))) 0 2))
              ((and (eq (first features) 'finger-down)
                    (= (length features) 3)
                    (valid-hand (second features))
                    (valid-finger (third features)))
               (zerop (third (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features))))))
              ((and (eq (first features) 'any-finger-down)
                    (= (length features) 2)
                    (valid-hand (second features)))
               (some (lambda (x) (zerop (third x)))
                     (mapcar (lambda (x) (aref (touch-module-fingers touch) (valid-hand (second features)) x)) '(0 1 2 3 4))))
              
              ((and (eq (first features) 'set-finger-down)
                    (= (length features) 3)
                    (valid-hand (second features))
                    (valid-finger (third features))
                    (aref (touch-module-hands touch) (valid-hand (second features))))
               (setf (third (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features)))) 0)
               (awhen (and (touch-module-show-fingers touch) (touch-module-window touch))
                      (notify-device it (append (list "showfinger" (string-downcase (symbol-name (second features))) (string-downcase (symbol-name (third features))))
                                                (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features)))))))
              ((and (eq (first features) 'set-finger-up)
                    (= (length features) 3)
                    (valid-hand (second features))
                    (valid-finger (third features))
                    (aref (touch-module-hands touch) (valid-hand (second features))))
               (setf (third (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features)))) 1)
               (awhen (and (touch-module-show-fingers touch) (touch-module-window touch))
                      (notify-device it (append (list "showfinger" (string-downcase (symbol-name (second features))) (string-downcase (symbol-name (third features))))
                                                (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features)))))))
              ((and (eq (first features) 'set-finger-position)
                    (= (length features) 5)
                    (valid-hand (second features))
                    (valid-finger (third features))
                    (aref (touch-module-hands touch) (valid-hand (second features)))
                    (integerp (fourth features))
                    (integerp (fifth features)))
               (setf (first (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features)))) (round (fourth features)))
               (setf (second (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features)))) (round (fifth features)))
               (awhen (and (touch-module-show-fingers touch) (touch-module-window touch))
                      (notify-device it (append (list "showfinger" (string-downcase (symbol-name (second features))) (string-downcase (symbol-name (third features))))
                                                (aref (touch-module-fingers touch) (valid-hand (second features)) (valid-finger (third features)))))))
              
              ((and (every 'listp features)
                    (second (find 'style features :key 'first))
                    (find (second (find 'style features :key 'first)) '(tap tap-hold tap-release swipe index-thumb move-hand-touch pinch rotate)))
               (case (second (find 'style features :key 'first))
                 (tap
                  (let* ((m (second (find 'model features :key 'first)))
                         (h (second (find 'hand features :key 'first)))
                         (f (second (find 'finger features :key 'first))))
                    (if (and m (valid-hand h) (valid-finger f))
                        (let* ((loc (aref (touch-module-fingers touch) (valid-hand h) (valid-finger f)))
                               (x (first loc))
                               (y (second loc)))
                          (with-model-eval m
                            (schedule-event-now "touch-tap" :params (list m h f x y) :output t :module :touch-interface :maintenance t)
                            
                            (awhen (touch-module-window touch) ;; if associated with an experiment window generate a mouse-click
                                   ;; signal -- any experiment window or other monitor will detect it
                                   (schedule-event-now "click-mouse" :params (list m (list x y) f) :output t :module :touch-interface :maintenance t))))
                      (print-warning "Touch device received invalid tap action having features ~s." features))))
                 (tap-hold 
                  (let* ((m (second (find 'model features :key 'first)))
                         (h (second (find 'hand features :key 'first)))
                         (f (second (find 'finger features :key 'first))))
                    (if (and m (valid-hand h) (valid-finger f))
                        (let* ((loc (aref (touch-module-fingers touch) (valid-hand h) (valid-finger f)))
                               (x (first loc))
                               (y (second loc)))
                          (with-model-eval m
                            (schedule-event-now "touch-tap-hold" :params (list m h f x y) :output t :module :touch-interface :maintenance t)))
                      (print-warning "Touch device received invalid tap-hold action having features ~s." features))))
                 (tap-release ;; assume that the motor actions determine the validity of the release action i.e. the finger is already down
                  (let* ((m (second (find 'model features :key 'first)))
                         (h (second (find 'hand features :key 'first)))
                         (f (second (find 'finger features :key 'first))))
                    (if (and m (valid-hand h) (valid-finger f))
                        (let* ((loc (aref (touch-module-fingers touch) (valid-hand h) (valid-finger f)))
                               (x (first loc))
                               (y (second loc)))
                          (with-model-eval m
                            (schedule-event-now "touch-tap-release" :params (list m h f x y) :output t :module :touch-interface :maintenance t)))
                      (print-warning "Touch device received invalid tap-release action having features ~s." features))))
                 (swipe
                  (let ((m (second (find 'model features :key 'first)))
                        (h (second (find 'hand features :key 'first)))
                        (f (second (find 'finger features :key 'first)))
                        (dist (second (find 'distance features :key 'first)))
                        (dir (second (find 'direction features :key 'first)))
                        (c (second (find 'count features :key 'first)))
                        (s (second (find 'speed features :key 'first)))
                        )
                    (if (and m (valid-hand h) (valid-finger f) (numberp dist) (numberp dir) (numberp c) (numberp s))
                        (with-model-eval m
                          (schedule-event-now "touch-swipe" :params (list m h f dist dir c s) :output t :module :touch-interface :maintenance t))
                      (print-warning "Touch device received invalid swipe action having features ~s." features))))
                 (index-thumb
                  (let ((m (second (find 'model features :key 'first)))
                        (h (second (find 'hand features :key 'first)))
                        (f (second (find 'finger features :key 'first)))
                        (dist (second (find 'distance features :key 'first)))
                        (dir (second (find 'direction features :key 'first))))
                    (if (and m (valid-hand h) (valid-finger f) (numberp dist) (numberp dir))
                        (with-model-eval m
                          (schedule-event-now "touch-index-thumb" :params (list m h f dist dir) :output t :module :touch-interface :maintenance t))
                      (print-warning "Touch device received invalid index-thumb action having features ~s." features))))
                 (move-hand-touch
                  (let* ((m (second (find 'model features :key 'first)))
                         (h (second (find 'hand features :key 'first)))
                         (loc (rest (find 'loc features :key 'first)))
                         (x (first loc))
                         (y (second loc)))
                    (if (and m (valid-hand h) (numberp x) (numberp y))
                        (with-model-eval m
                          (schedule-event-now "touch-point" :params (list m h x y) :output t :module :touch-interface :maintenance t))
                      (print-warning "Touch device received invalid move-hand-touch action having features ~s." features))))
                 (pinch
                  (let* ((m (second (find 'model features :key 'first)))
                         (h (second (find 'hand features :key 'first)))
                         (f (second (find 'finger features :key 'first)))
                         (r (rest (find 'range features :key 'first)))
                         (start (first r))
                         (end (second r)))
                    (if (and m (valid-hand h) (valid-finger f) (numberp start) (numberp end))
                        (with-model-eval m
                          (schedule-event-now "touch-pinch" :params (list m h f start end) :output t :module :touch-interface :maintenance t))
                      (print-warning "Touch device received invalid pinch action having features ~s." features))))
                 (rotate
                  (let ((m (second (find 'model features :key 'first)))
                        (h (second (find 'hand features :key 'first)))
                        (f (second (find 'finger features :key 'first)))
                        (dir (second (find 'direction features :key 'first))))
                    (if (and m (valid-hand h) (valid-finger f) (numberp dir))
                        (with-model-eval m
                          (schedule-event-now "touch-rotate" :params (list m h f dir) :output t :module :touch-interface :maintenance t))
                      (print-warning "Touch device received invalid rotate action having features ~s." features))))))
              (t 
               (print-warning "Invalid notification ~s sent to touch device ~s" features device-list)))))))

(add-act-r-command "touch-device-interface" 'touch-device-interface "Internal command for the touch device notifications. Do not call.")


(defun hand-moved-to-touch (device hand)
  (declare (ignore device))
  (let ((touch (get-module :touch-interface)))
    (when touch
      (bt:with-recursive-lock-held ((touch-module-lock touch))
        (let* ((dist (* (touch-module-ppi touch) (touch-module-finger-spacing touch)))
               (start (if (eq hand 'right) 
                          (touch-module-right-start touch) 
                        (touch-module-left-start touch)))
               (sx (round (first start)))
               (sy (round (second start)))
               (window (and (touch-module-show-fingers touch) 
                            (touch-module-window touch)))
               (finger-offsets (if (eq hand 'right)
                                   (right-standard-offsets)
                                 (left-standard-offsets))))
          
          ;; indicate the hand is on the device
          (setf (aref (touch-module-hands touch) (valid-hand hand)) t)
          
          (dolist (f finger-offsets)
            (let ((p (list (round (+ (* (px (second f)) dist) sx)) (round (+ (* (py (second f)) dist) sy)) 1)))
              (setf (aref (touch-module-fingers touch) (valid-hand hand) (valid-finger (first f))) p)
              (when window
                (notify-device window (list "showfinger" 
                                            (string-downcase (symbol-name hand))
                                            (string-downcase (symbol-name (first f)))
                                            (first p)
                                            (second p)
                                            1))))))))))

(add-act-r-command "touch-set-hand" 'hand-moved-to-touch "Internal command for the touch device. Do not call.")

(defun hand-moved-away-from-touch (device hand)
  (declare (ignore device))
  (let ((touch (get-module :touch-interface)))
    (when touch
      (bt:with-recursive-lock-held ((touch-module-lock touch))
        
        (awhen (and (aref (touch-module-hands touch) (valid-hand hand)) ;; should always be true since hand was there
                    (touch-module-show-fingers touch) 
                    (touch-module-window touch))
               (dolist (x '(index middle ring pinkie thumb))
                 (notify-device it (list "removefinger" (string-downcase (symbol-name hand)) (string-downcase (symbol-name x))))))
      
        ;; indicate the hand is no longer on device
        (setf (aref (touch-module-hands touch) (valid-hand hand)) nil)))))

(add-act-r-command "touch-unset-hand" 'hand-moved-away-from-touch "Internal command for the touch device. Do not call.")


(define-device "touch" "install-touch-device" "remove-touch-device" "touch-device-interface" "touch-set-hand" "touch-unset-hand")

(add-act-r-command "touch-index-thumb" nil "Signal that an index-thumb action has been performed on the touch device. Params: model hand finger distance direction")
(add-act-r-command "touch-point" nil "Signal that a hand has moved to a new locaiton on the touch device. Params: model hand x y")
(add-act-r-command "touch-swipe" nil "Signal that a swipe action has been performed on the touch device. Params: model hand initial-finger distance direction finger-count speed")
(add-act-r-command "touch-tap" nil "Signal that a finger tap has been performed on the touch device. Params: model hand finger x y")
(add-act-r-command "touch-tap-hold" nil "Signal that a finger tap-hold has been performed on the touch device. Params: model hand finger x y")
(add-act-r-command "touch-tap-release" nil "Signal that a finger tap-release has been performed on the touch device. Params: model hand finger x y")
(add-act-r-command "touch-pinch" nil "Signal that a pinch has been performed on the touch device. Params: model hand finger start-width end-width")
(add-act-r-command "touch-rotate" nil "Signal that a rotate has been performed on the touch device. Params: model hand finger rotate-angle")

;;;; ---------------------------------------------------------------------- ;;;;
;;;;   ACT-R Motor Module Extension
;;;; ---------------------------------------------------------------------- ;;;;



;;; These are general actions which could be used with other devices.
;;;

;;; First create a function which will get the finger positions from
;;; the device, or if it doesn't return any use the default motor-module
;;; position.

(defun touch-finger-position (hand finger &key (current t))
  (let ((device (device-for-hand hand :current current)))
    (if (and device (string-equal (second device) "touch"))
        (progn
          (if current
              (coerce (notify-device device (list 'finger-position hand finger)) 'vector)
            (let* ((hand-pos (hand-position hand :current nil))
                   (touch-pos (find 'touch-positions (hand-pos-other hand-pos) :key 'first))
                   (f (find finger (rest touch-pos) :key 'first)))
              (if f
                  (vector (second f) (third f))
                (coerce (notify-device device (list 'finger-position hand finger)) 'vector)))))
      (print-warning "Touch-finger-position called for ~s ~s but hand not on touch device." hand finger))))


(defun touch-finger-down (hand finger &key (current t))
  (let ((device (device-for-hand hand :current current)))
    (if (and device (string-equal (second device) "touch"))
        (progn
          (if current
              (notify-device device (list 'finger-down hand finger))
            (let* ((hand-pos (hand-position hand :current nil))
                   (touch-pos (find 'touch-positions (hand-pos-other hand-pos) :key 'first))
                   (f (find finger (rest touch-pos) :key 'first)))
              (if f
                  (zerop (fourth f))
                (notify-device device (list 'finger-down hand finger))))))
      (print-warning "Touch-finger-down called for ~s ~s but hand not on touch device." hand finger))))


(defun touch-any-finger-down (hand &key (current t))
  (let ((device (device-for-hand hand :current current)))
    (if (and device (string-equal (second device) "touch"))
        (progn
          (if current
              (notify-device device (list 'any-finger-down hand))
            (let* ((hand-pos (hand-position hand :current nil))
                   (touch-pos (find 'touch-positions (hand-pos-other hand-pos) :key 'first))
                   )
              (dolist (finger '(thumb index middle ring pinkie) nil)
                (let* ((f (find finger (rest touch-pos) :key 'first))
                       (val (if f
                                (zerop (fourth f))
                              (notify-device device (list 'finger-down hand finger)))))
                  
                  (when val
                    (return-from touch-any-finger-down t)))))))
      (print-warning "Touch-any-finger-down called for ~s but hand not on touch device." hand))))






;;; This is touch device specific

(defmethod hand-to-touch ((mtr-mod motor-module) request)
  (let ((installed-touch (find "touch" (current-devices "motor") :test 'string-equal :key 'second)))
    (cond (installed-touch
           (let* ((touch (get-module :touch-interface))
                  (hand-spec (chunk-spec-slot-spec request 'hand))
                  (hand (if (null hand-spec) 'right
                          (verify-single-explicit-value request 'hand :touch-interface "hand-to-touch"))))
             (if (and touch (valid-hand hand))
                 (let* ((touch-pos (bt:with-recursive-lock-held ((touch-module-lock touch)) (touch-module-position touch)))
                        (hand-pos (hand-position hand :current nil))
                        (hand-loc (hand-pos-loc hand-pos))
                        (polar (xy-to-polar hand-loc touch-pos))
                        (device (device-for-hand hand :current nil)))
                   
                   (if (and (= (vr polar) 0)
                            device
                            (string-equal (second device) "touch"))
                       (model-warning "HAND-TO-TOUCH requested but hand already is (or will be) at the touch position.")
                     
                     (point-hand mtr-mod :hand hand :r (vr polar) :theta (vtheta polar) :twidth 4.0 
                                 :device installed-touch :offsets 'standard :request-spec request
                                 :style 'to-touch-ply)))
               (model-warning "Hand-to-touch request received invalid hand information ~s" hand-spec))))
          (t
           (model-warning "Hand-to-touch requested but no touch device available.")))))

(defclass to-touch-ply (hand-ply)
  ())

(defmethod prepare-features ((mtr-mod motor-module) (self to-touch-ply))
  (let ((new-pos (move-a-hand mtr-mod (hand self) (r self) (theta self) :current nil)))
    (unless (eq (offsets self) 'standard)
      (model-warning "Hand-to-touch action specified non-standard finger offsets but that is not currently supported."))
    
    (setf (hand-pos-fingers new-pos) (ecase (hand self)
                                       (right (right-standard-offsets))
                                       (left (left-standard-offsets))))
    
    ;; Since the device won't set the positions
    ;; until the hand moves to it we need to set the
    ;; updated positions here as well because we
    ;; can't assume that falling through to current
    ;; will be valid if a subsequent action wants the "next" ones
    
    (let ((touch (get-module :touch-interface)))
      (when touch
        (bt:with-recursive-lock-held ((touch-module-lock touch))
          (let* ((dist (* (touch-module-ppi touch) (touch-module-finger-spacing touch)))
                 (start (if (eq (hand self) 'right) 
                            (touch-module-right-start touch) 
                          (touch-module-left-start touch)))
                 (sx (round (first start)))
                 (sy (round (second start)))
                 (finger-offsets (hand-pos-fingers new-pos))
                 (positions (list 'touch-positions))
                 (others (hand-pos-other new-pos)))
          
          
            (dolist (f finger-offsets)
              (push-last (list (first f)
                               (round (+ (* (px (second f)) dist) sx))
                               (round (+ (* (py (second f)) dist) sy))
                               1)
                         positions))
            
            (awhen (find 'touch-positions others :key 'first)
                   (setf others (remove it others)))
            
            (push positions others)
            (setf (hand-pos-other new-pos) others)))))
    
    (setf (hand-pos-device new-pos) (device self))
    (setf (updated-pos self) new-pos)))



;;; move-hand-touch
;;; Allows the model to move its hand (referenced by index finger position) to what it sees. 
;;; Also moves all of the other fingers along with it.

(defstyle touch-ply hand-ply hand finger r theta)

(defmethod prepare-features ((mtr-mod motor-module) (self touch-ply))
  (let* ((starting-point (copy-hand-pos (hand-position (hand self) :current nil)))
         (others (copy-seq (hand-pos-other starting-point))))
    (awhen (find 'touch-positions (hand-pos-other starting-point) :key 'first)
           (setf others (remove it others)))
    (let ((updated-fingers (list 'touch-positions)))
      (dolist (finger '(index middle ring pinkie thumb))
        (let ((new-pos (polar-move-xy (touch-finger-position (hand self) finger :current nil) 
                                      (vector (r self) (theta self)))))
          (push-last (list finger (px new-pos) (py new-pos) 
                           (if (touch-finger-down (hand self) finger :current nil)
                               0 1))
                     updated-fingers)))
        
      (push updated-fingers others))
    (setf (hand-pos-other starting-point) others)
    (setf (updated-pos self) starting-point)))

(defmethod queue-output-events ((mtr-mod motor-module) (self touch-ply))
  (let* ((device (device-for-hand (hand self)))
         (action-time (seconds->ms (exec-time self)))
         (finger-positions (rest (find 'touch-positions (hand-pos-other (updated-pos self)) :key 'first)))
         (hand-position (find 'index finger-positions :key 'first)))
    
    ;; Send the move-hand-touch action to the device
    
    (schedule-event-relative action-time 'notify-motor-device 
                             :time-in-ms t :module :motor :output nil
                             :params `(,device 
                                       ((model ,(current-model)) 
                                        (style move-hand-touch) (hand ,(hand self)) 
                                        (loc ,(second hand-position) ,(third hand-position)))))
    
    ;; update the hand-position
    
    (schedule-event-relative action-time 'set-hand-position :time-in-ms t :module :motor 
                             :destination :motor :params (list (hand self) (updated-pos self))
                             :output nil)
    
    ;; schedule calls to move all of the fingers
    
    (dolist (finger finger-positions)
      (let ((f finger))
        (schedule-event-relative action-time 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device 
                                           (set-finger-position ,(hand self) ,(first f) ,(second f) ,(third f))))))))



(defmethod move-hand-touch ((mtr-mod motor-module) &key loc object hand request-spec)
  (let ((real-device (device-for-hand hand :current nil)))
    (when (or (null real-device) (not (string-equal (second real-device) "touch")))
      (print-warning "Hand is not on a touch device in request to move-hand-touch.")
      (return-from move-hand-touch nil))
      
      (unless (or (check-jam mtr-mod) (check-specs 'move-hand-touch (or loc object)))
        (let ((target (or (and (chunk-p-fct object) object) (and (chunk-p-fct loc) loc)))
              (target-loc (or (and object (chunk-to-visual-position object))
                              (and loc (chunk-to-visual-position loc)))))
      
          (unless (and target target-loc)
            (print-warning "No valid location could be generated from ~s or ~s when trying to move hand on touch device." object loc)
            (return-from move-hand-touch nil))
          
          ;; need parameters from the module
          (let ((touch (get-module :touch-interface))
                drag-delay coeff noise)
            (bt:with-recursive-lock-held ((touch-module-lock touch))
              (setf drag-delay (touch-module-drag-delay touch)
                coeff (touch-module-cursor-coeff touch)
                noise (touch-module-noise touch)))
            
            ;; hand position is index finger position
            (let* ((current-loc (touch-finger-position hand 'index :current nil))
                   (r-theta (xy-to-polar current-loc (coerce target-loc 'vector))))
              (if (= 0 (vr r-theta))        ; r=0 is a no-op 
                  (model-warning "Move-hand-touch action aborted because hand is at requested target ~S" (if object object loc)))
              (let* ((w (pm-angle-to-pixels (approach-width target (vtheta r-theta) (px current-loc) (py current-loc)))) ;;approach-width returns DVA but we're working in pixels
                     (r-theta-new (if noise
                                      (noisy-loc-em? target-loc w (vtheta r-theta))
                                    r-theta)))
                
                (prepare-movement mtr-mod
                                  (make-instance 'touch-ply
                                    :request-spec request-spec
                                    :hand hand
                                    :r (vr r-theta-new)
                                    :theta (vtheta r-theta-new)
                                    :target-width w
                                    :fitts-coeff (if (and drag-delay (touch-any-finger-down hand :current nil))
                                                     ;; because control order is 0
                                                     ;; just use drag-delay directly
                                                     ;; and don't need 2^0 in normal
                                                     (* (expt-coerced 2 (if (eq drag-delay t) .8 drag-delay)) coeff)
                                                   coeff))))))))))

(defmethod move-hand-touch-request ((mtr-mod motor-module) chunk-spec)
  (let ((object (if (slot-in-chunk-spec-p chunk-spec 'object) 
                    (verify-single-explicit-value 
                     chunk-spec
                     'object
                     :motor
                     'move-hand-touch)
                  nil))
        (location (if (slot-in-chunk-spec-p chunk-spec 'loc)
                      (verify-single-explicit-value 
                       chunk-spec
                       'loc
                       :motor
                       'move-hand-touch)
                    nil))
        (hand (if (slot-in-chunk-spec-p chunk-spec 'hand)
                  (verify-single-explicit-value 
                   chunk-spec
                   'hand
                   :motor
                   'move-hand-touch)
                ;; defaults to right hand if not given
                'right)))
    
    (cond ((not (or (eq hand 'right) (eq hand 'left)))
           (print-warning "Move-hand-touch action failed because ~s is not a valid hand." hand))
          ((null (or location object))
           (print-warning "Move-hand-touch action failed because no object or location provided."))
          (t
           (schedule-event-now
            'move-hand-touch 
            :destination :motor
            :params (list :object object 
                          :loc location
                          :hand hand
                          :request-spec chunk-spec)
            :module :motor
            :output 'high
            :details (format nil "~a ~a ~a ~a ~a ~a ~a" 'move-hand-touch :object object :loc location :hand hand))))))


;;; tap movement style
;;; analogous to punch movement style, the indicated finger strikes the surface 
;;; of the display at the finger's current position and returns the finger 
;;; to a z-position where it is ready to act again
;;; Actually executed like a peck-recoil since it must move to the display and 
;;; back based on the distance parameter.

(defstyle tap () hand finger)

(defmethod tap :around ((mtr-mod motor-module) &key hand finger request-spec)
  (declare (ignore request-spec))
  (let ((real-device (device-for-hand hand :current nil)))
    (cond ((or (null real-device) (not (string-equal (second real-device) "touch")))
           (print-warning "Hand is not on a touch device in request to tap."))
          
          ((null (valid-hand hand))
           (print-warning "Tap request has invalid hand ~s." hand))
          
          ((null (valid-finger finger))
           (print-warning "Tap request has invalid finger ~s." finger))
                    
          ((touch-finger-down hand finger :current nil)
           (print-warning "Tap requested for ~a ~a but that finger is (or will be) down." hand finger))
          
          (t
           (call-next-method)))))
        
(defmethod compute-exec-time ((mtr-mod motor-module) (self tap))
  (let ((touch (get-module :touch-interface)))
    (bt:with-recursive-lock-held ((touch-module-lock touch))
      (bt:with-recursive-lock-held ((param-lock mtr-mod))
        (+ (init-time mtr-mod)  
           (max (burst-time mtr-mod)
                (randomize-time (fitts mtr-mod 
                                       ;; peck's "b" coefficient
                                       (touch-module-peck-coeff touch) 
                                       ;; Use the user defined settings for height and ppi
                                       ;; also, convert that to degrees of visual angle 
                                       ;; because since there is no target width the default
                                       ;; is 1 which would mean 1 pixel and that results in
                                       ;; a time which seems too long for the default distance
                                       ;; ~.45 seconds to tap
                                       (pixels->angle-mth (current-device-interface) (* (touch-module-ppi touch) (touch-module-height touch)))))))))))

(defmethod feat-differences ((t1 tap) (t2 tap))
  (cond ((not (eq (hand t1) (hand t2))) 2)
        ((not (eq (finger t1) (finger t2))) 1)
        (t 0)))

(defmethod queue-output-events ((mtr-mod motor-module) (self tap))
  ;; Send the tap action at execution time to whatever device is current for the hand
  (let ((device (device-for-hand (hand self))))
    (bt:with-recursive-lock-held ((param-lock mtr-mod))
      
      (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                               :time-in-ms t :module :motor :output nil
                               :params `(,device 
                                         ((model ,(current-model)) 
                                          (style tap) (hand ,(hand self)) 
                                          (finger ,(finger self)))))
      
      
      ;; also set the finger-down at execution time
      ;; and finger-up at burst time after exec time
      
      (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                               :time-in-ms t :module :motor :output nil
                               :params `(,device (set-finger-down ,(hand self) ,(finger self))))
      
      
      (schedule-event-relative (seconds->ms (+ (exec-time self) (burst-time mtr-mod))) 'notify-motor-device 
                               :time-in-ms t :module :motor :output nil
                               :params `(,device (set-finger-up ,(hand self) ,(finger self)))))))


;;; tap-hold movement style
;;; Performs a tap movement, but holds the finger to the multitouch display surface.

(defstyle tap-hold tap hand finger)

(defmethod tap-hold :around ((mtr-mod motor-module) &key hand finger request-spec)
  (declare (ignore request-spec))
  (let ((real-device (device-for-hand hand :current nil)))
    (cond ((or (null real-device) (not (string-equal (second real-device) "touch")))
           (print-warning "Hand is not on a touch device in request to tap-hold."))
          
          ((null (valid-hand hand))
           (print-warning "Tap-hold request has invalid hand ~s." hand))
          
          ((null (valid-finger finger))
           (print-warning "Tap-hold request has invalid finger ~s." finger))
                    
          ((touch-finger-down hand finger :current nil)
           (print-warning "Tap-hold requested for ~a ~a but that finger is (or will be) down." hand finger))
          
          (t
           (call-next-method)))))

(defmethod prepare-features ((mtr-mod motor-module) (self tap-hold))
  (let* ((starting-point (copy-hand-pos (hand-position (hand self) :current nil)))
         (others (copy-seq (hand-pos-other starting-point))))
    (awhen (find 'touch-positions (hand-pos-other starting-point) :key 'first)
           (setf others (remove it others)))
    (let ((updated-fingers (list 'touch-positions)))
      (dolist (finger '(index middle ring pinkie thumb))
        (let ((pos (touch-finger-position (hand self) finger :current nil)))
          (push-last (list finger (px pos) (py pos) 
                           (if (eq finger (finger self))
                               0
                             (if (touch-finger-down (hand self) finger :current nil)
                               0 1)))
                     updated-fingers)))
        
      (push updated-fingers others))
    (setf (hand-pos-other starting-point) others)
    (setf (updated-pos self) starting-point)))

(defmethod queue-output-events ((mtr-mod motor-module) (self tap-hold))
  
  ;; Send the tap-hold action at execution time to whatever device is current for the hand
      
  (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                           :time-in-ms t :module :motor :output nil
                           :params `(,(device-for-hand (hand self)) 
                                       ((model ,(current-model)) 
                                        (style tap-hold) (hand ,(hand self)) 
                                        (finger ,(finger self)))))
  
  ;; update the hand-position information
  
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                             :destination :motor :params (list (hand self) (updated-pos self))
                             :output nil)
  
  ;; also send the finger-down notice
  
  (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                           :time-in-ms t :module :motor :output nil
                           :params `(,(device-for-hand (hand self)) 
                                       (set-finger-down ,(hand self) ,(finger self)))))


;;; tap-release movement style
;;; If the finger is held against the surface of the multitouch display,
;;; as in a tap-hold movement, then tap-release moves the finger away from
;;; the multitouch display surface.

(defstyle tap-release tap hand finger)

(defmethod tap-release :around ((mtr-mod motor-module) &key hand finger request-spec)
  (declare (ignore request-spec))
  (let ((real-device (device-for-hand hand :current nil)))
    (cond ((or (null real-device) (not (string-equal (second real-device) "touch")))
           (print-warning "Hand is not on a touch device in request to tap-release."))
          
          ((null (valid-hand hand))
           (print-warning "Tap-release request has invalid hand ~s." hand))
          
          ((null (valid-finger finger))
           (print-warning "Tap-release request has invalid finger ~s." finger))
                    
          ((not (touch-finger-down hand finger :current nil))
           (print-warning "Tap-release requested for ~a ~a but that finger is (or will be) up." hand finger))
          
          (t
           (call-next-method)))))

(defmethod prepare-features ((mtr-mod motor-module) (self tap-release))
  (let* ((starting-point (copy-hand-pos (hand-position (hand self) :current nil)))
         (others (copy-seq (hand-pos-other starting-point))))
    (awhen (find 'touch-positions (hand-pos-other starting-point) :key 'first)
           (setf others (remove it others)))
    (let ((updated-fingers (list 'touch-positions)))
      (dolist (finger '(index middle ring pinkie thumb))
        (let ((pos (touch-finger-position (hand self) finger :current nil)))
          (push-last (list finger (px pos) (py pos) 
                           (if (eq finger (finger self))
                               1
                             (if (touch-finger-down (hand self) finger :current nil)
                               0 1)))
                     updated-fingers)))
        
      (push updated-fingers others))
    (setf (hand-pos-other starting-point) others)
    (setf (updated-pos self) starting-point)))

(defmethod compute-exec-time ((mtr-mod motor-module) (self tap-release))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (init-time mtr-mod)  
       ;; just costs a burst-time since it's not a targeted
       ;; motion -- just lifting a finger
       (burst-time mtr-mod))))

(defmethod queue-output-events ((mtr-mod motor-module) (self tap-release))
  
  ;; Send the tap-release action at execution time to whatever device is current for the hand
      
  (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                           :time-in-ms t :module :motor :output nil
                           :params `(,(device-for-hand (hand self)) 
                                       ((model ,(current-model)) 
                                        (style tap-release) (hand ,(hand self)) 
                                        (finger ,(finger self)))))
      
  ;; update the hand-position information
  
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                             :destination :motor :params (list (hand self) (updated-pos self))
                           :output nil)
  
  ;; also send the finger-up notice
  
  (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                           :time-in-ms t :module :motor :output nil
                           :params `(,(device-for-hand (hand self)) 
                                       (set-finger-up ,(hand self) ,(finger self)))))




;;; Index-Thumb
;;; Subclass this for pinch and rotate
;;; on its own basically a thumb swipe

;; want an extra slot so skip defstyle to avoid complications

(defclass index-thumb (hfrt-movement)
  ((move-time :accessor move-time))
  (:default-initargs
      :style-name :index-thumb))

(defmethod compute-exec-time ((mtr-mod motor-module) (self index-thumb))
  (let ((touch (get-module :touch-interface)))
    (bt:with-recursive-lock-held ((touch-module-lock touch))
      (bt:with-recursive-lock-held ((param-lock mtr-mod))
        (setf (move-time self)
          (max (burst-time mtr-mod)
               (fitts mtr-mod (peck-fitts-coeff mtr-mod) 
                      (r self)
                      ;; the width of the thumb
                      (* (touch-module-ppi touch) (touch-module-thumb-width touch)))))
        (+ (init-time mtr-mod)
           (max (burst-time mtr-mod) 
                (randomize-time (move-time self))))))))

(defmethod compute-finish-time ((mtr-mod motor-module) (self index-thumb))
  (bt:with-recursive-lock-held ((param-lock mtr-mod))
    (+ (exec-time self) 
       (burst-time mtr-mod)
       (max (burst-time mtr-mod)
            (randomize-time (move-time self))))))

(defmethod queue-output-events ((mtr-mod motor-module) (self index-thumb))
  (let* ((finger (finger self))
         (hand (hand self))
         (start-pos (touch-finger-position hand finger))
         (move-vector (vector (r self) (theta self)))
         (end-pos (polar-move-xy start-pos move-vector))
         (device (device-for-hand hand)))
        
    ;; Send the index-thumb action at execution time to whatever device is current for the hand
    ;; with the distance and direction
        
    (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                             :time-in-ms t :module :motor :output nil
                             :params `(,device 
                                       ((model ,(current-model)) 
                                        (style index-thumb) (hand ,hand) (finger ,finger) 
                                        (distance ,(vr move-vector)) (direction ,(vtheta move-vector)))))
    
    ;; schedule the finger to go down after init-time
    ;; move to the target point at exec-time
    ;; lift up after exec-time + burst-time
    ;; move back to start at finish-time
    
    (bt:with-recursive-lock-held ((param-lock mtr-mod))
      
      
      (schedule-event-relative (seconds->ms (init-time mtr-mod)) 'notify-motor-device 
                               :time-in-ms t :module :motor :output nil
                               :params `(,device 
                                         (set-finger-down ,hand ,finger)))
      
      (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                               :time-in-ms t :module :motor :output nil
                               :params `(,device 
                                         (set-finger-position ,hand ,finger ,(px end-pos) ,(py end-pos))))
      
      (schedule-event-relative (seconds->ms (+ (exec-time self) (bt:with-recursive-lock-held ((param-lock mtr-mod)) (burst-time mtr-mod))))
                               'notify-motor-device 
                               :time-in-ms t :module :motor :output nil
                               :params `(,device 
                                         (set-finger-up ,hand ,finger)))
      
      (schedule-event-relative (seconds->ms (finish-time self)) 'notify-motor-device 
                               :time-in-ms t :module :motor :output nil
                               :params `(,device 
                                         (set-finger-position ,hand ,finger ,(px start-pos) ,(py start-pos)))))))



(defmethod index-thumb ((mtr-mod motor-module) &key hand finger r theta request-spec)
  
  (let ((real-device (device-for-hand hand :current nil)))
    (cond ((or (null real-device) (not (string-equal (second real-device) "touch")))
           (print-warning "Hand is not on a touch device in request to index-thumb."))
                
          ((check-jam mtr-mod) ; prints own warning
           )
          
          ((null (valid-hand hand))
           (print-warning "Index-thumb request has invalid hand ~s." hand))
          
          ((null (valid-finger finger))
           (print-warning "Index-thumb request has invalid finger ~s." finger))
          
          ((touch-finger-down hand finger :current nil)
           (print-warning "Index-thumb requested for ~a ~a but that finger is (or will be) down." hand finger))
          
          (t
           (when (symbolp theta)
             (setf theta (symbol-value theta)))
           
           (prepare-movement mtr-mod (make-instance 'index-thumb :hand hand :finger finger :r r :theta theta :request-spec request-spec))))))



;; Pinch is finger and thumb down start-width apart then move to end-width apart lift and return to starting

(defStyle pinch index-thumb hand finger start-width end-width)

(defmethod compute-exec-time ((mtr-mod motor-module) (self pinch))
  (let ((touch (get-module :touch-interface)))
    (bt:with-recursive-lock-held ((touch-module-lock touch))
      
      (bt:with-recursive-lock-held ((param-lock mtr-mod))
        (setf (move-time self)
          (max (burst-time mtr-mod)
               (fitts mtr-mod (peck-fitts-coeff mtr-mod) 
                      (abs (- (start-width self) (end-width self)))
                      ;; the width of the thumb
                      (* (touch-module-ppi touch) (touch-module-thumb-width touch)))))
        (+ (init-time mtr-mod)
           (max (burst-time mtr-mod) 
                (randomize-time (move-time self))))))))

(defmethod queue-output-events ((mtr-mod motor-module) (self pinch))
  
  ;; Send the pinch action at execution time to whatever device is current for the hand
  ;; with the distances
  (let* ((hand (hand self))
         (finger (finger self))
         (device (device-for-hand hand)))
    
    (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                             :time-in-ms t :module :motor :output nil
                             :params `(,device 
                                       ((model ,(current-model)) 
                                        (style pinch) (hand ,hand) (finger ,finger)
                                        (range ,(start-width self) ,(end-width self)))))
  
    ;; move the finger and thumb to be start-width apart and put them
    ;; down at init-time
    ;; move to end-width distance at exec-time 
    ;; lift up a burst-time after exec-time
    ;; move back to start at finish-time
    
    (bt:with-recursive-lock-held ((param-lock mtr-mod))
      
      (let* ((thumb-start-pos (touch-finger-position hand 'thumb))
             (finger-start-pos (touch-finger-position hand finger))
             (theta (atan (- (py finger-start-pos) (py thumb-start-pos)) (- (px finger-start-pos) (px thumb-start-pos))))
             (r (round (- (dist thumb-start-pos finger-start-pos) (start-width self))))
             (thumb-init-pos (if (zerop r) thumb-start-pos
                               (polar-move-xy thumb-start-pos
                                              (vector (/ r 2)
                                                      (if (> r (start-width self))
                                                          theta
                                                        (+ pi theta))))))
             (finger-init-pos (if (zerop r) finger-start-pos
                                (polar-move-xy finger-start-pos
                                               (vector (/ r 2)
                                                       (if (> r (start-width self))
                                                           (+ pi theta)
                                                         theta)))))
             (travel-dist (round (- (start-width self) (end-width self))))
             (thumb-end-pos (if (zerop travel-dist) thumb-init-pos
                              (polar-move-xy thumb-init-pos
                                             (vector (/ travel-dist 2) theta))))
             (finger-end-pos (if (zerop travel-dist) finger-init-pos
                               (polar-move-xy finger-init-pos
                                              (vector (/ travel-dist 2) (+ theta pi)))))
             (bt (burst-time mtr-mod)))
        
        (schedule-event-relative (seconds->ms (init-time mtr-mod)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device 
                                           (set-finger-position ,hand thumb ,(px thumb-init-pos) ,(py thumb-init-pos))))
        
        (schedule-event-relative (seconds->ms (init-time mtr-mod)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device 
                                           (set-finger-position ,hand ,finger ,(px finger-init-pos) ,(py finger-init-pos))))
        
        (schedule-event-relative (seconds->ms (init-time mtr-mod)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device 
                                           (set-finger-down ,hand thumb)))
        
        (schedule-event-relative (seconds->ms (init-time mtr-mod)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device 
                                           (set-finger-down ,hand ,finger)))
        
        (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                           (set-finger-position ,hand thumb ,(px thumb-end-pos) ,(py thumb-end-pos))))
        
        (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                           (set-finger-position ,hand ,finger ,(px finger-end-pos) ,(py finger-end-pos))))
        
        (schedule-event-relative (seconds->ms (+ (exec-time self) bt)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                           (set-finger-up ,hand thumb)))
        
        (schedule-event-relative (seconds->ms (+ (exec-time self) bt)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                           (set-finger-up ,hand ,finger)))
        
        (schedule-event-relative (seconds->ms (finish-time self)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                           (set-finger-position ,hand thumb ,(px thumb-start-pos) ,(py thumb-start-pos))))
        
        (schedule-event-relative (seconds->ms (finish-time self)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device 
                                           (set-finger-position ,hand ,finger ,(px finger-start-pos) ,(py finger-start-pos))))))))


(defmethod pinch :around ((mtr-mod motor-module) &key hand finger start-width end-width request-spec)
  (declare (ignore start-width end-width request-spec))
  (let ((real-device (device-for-hand hand :current nil)))
    (cond ((or (null real-device) (not (string-equal (second real-device) "touch")))
           (print-warning "Hand is not on a touch device in request to pinch."))
      
          ((null (valid-hand hand))
           (print-warning "Pinch request has invalid hand ~s." hand))
          
          ((null (valid-finger finger))
           (print-warning "Pinch request has invalid finger ~s." finger))
          
          ((touch-finger-down hand finger :current nil)
           (print-warning "Pinch requested for ~a ~a but that finger is (or will be) down." hand finger))
          
          ((touch-finger-down hand 'thumb :current nil)
           (print-warning "Pinch requested for ~a hand but that thumb is (or will be) down." hand))
          
          (t
           (call-next-method)))))



;; Rotate is thumb and finger down and then moved along a circular path
;; the indicated number of radians (positive is clockwise negative counter-clockwise).
        
(defStyle rotate index-thumb hand finger rotation)

(defmethod compute-exec-time ((mtr-mod motor-module) (self rotate))
  (let ((touch (get-module :touch-interface))
        width)
    (bt:with-recursive-lock-held ((touch-module-lock touch))
      (setf width (touch-module-rotate-width touch)))
    
    (let ((diameter (dist (touch-finger-position (hand self) 'thumb)
                          (touch-finger-position (hand self) (finger self)))))
      
      (bt:with-recursive-lock-held ((param-lock mtr-mod))
        (setf (move-time self)
          (max (burst-time mtr-mod)
               (fitts mtr-mod (peck-fitts-coeff mtr-mod) 
                      ;; Was this but that doesn't make sense to me
                      ;;   (dist 
                      ;;     (finger-loc-m mtr-mod (hand self) 'thumb)
                      ;;     (finger-loc-m mtr-mod (hand self) 'index))
                      ;; So instead use the actual distance traveled to
                      ;; perform the indicated rotation assuming a circle
                      ;; with the thumb and indicated finger rotated around
                      ;; the midpoint of their connecting line segment.
                      ;; That's simply the angle in radians times half the
                      ;; distance between them because s = r * theta
                      (* .5 diameter (abs (rotation self)))
                      
                      ;; target size is rotate-width degrees of the circle
                      (* .5 diameter pi 1/180 width))))
        
        (+ (init-time mtr-mod)
           (max (burst-time mtr-mod) 
                (randomize-time (move-time self))))))))

(defmethod queue-output-events ((mtr-mod motor-module) (self rotate))
  (let* ((hand (hand self))
         (finger (finger self))
         (device (device-for-hand hand)))
    
    ;; Send the rotate action at execution time to whatever device is current for the hand
    ;; with the rotation angle
  
    (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                             :time-in-ms t :module :motor :output nil
                             :params `(,device
                                         ((model ,(current-model)) 
                                          (style rotate) (hand ,hand) (finger ,finger)
                                          (direction ,(rotation self)))))
  
    ;; move the finger and thumb down at init-time
    ;; move by rotation angle at exec-time (positive is clockwise)
    ;; lift up a burst-time after exec-time
    ;; move back to start at finish-time
  
    ;; just compute the final location -- not doing incremental movement along the arc
  
    (bt:with-recursive-lock-held ((param-lock mtr-mod))
      (let* ((thumb-start-pos (touch-finger-position hand 'thumb))
             (finger-start-pos (touch-finger-position hand finger))
             (center (vector (+ (px thumb-start-pos) (/ (- (px finger-start-pos) (px thumb-start-pos)) 2))
                             (+ (py thumb-start-pos) (/ (- (py finger-start-pos) (py thumb-start-pos)) 2))))
             (tx1 (- (px thumb-start-pos) (px center)))
             (ty1 (- (py thumb-start-pos) (py center)))
             (fx1 (- (px finger-start-pos) (px center)))
             (fy1 (- (py finger-start-pos) (py center)))
             (s (sin (rotation self)))
             (c (cos (rotation self)))
             (tx (round (+ (- (* tx1 c) (* ty1 s)) (px center))))
             (ty (round (+ (+ (* ty1 c) (* tx1 s)) (py center))))
             (fx (round (+ (- (* fx1 c) (* fy1 s)) (px center))))
             (fy (round (+ (+ (* fy1 c) (* fx1 s)) (py center))))
             (bt (burst-time mtr-mod)))
                
        (schedule-event-relative (seconds->ms (init-time mtr-mod)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device 
                                             (set-finger-down ,hand thumb)))
        
        (schedule-event-relative (seconds->ms (init-time mtr-mod)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                             (set-finger-down ,hand ,finger )))
        
        
        (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                             (set-finger-position ,hand thumb ,tx ,ty)))
        
        (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                             (set-finger-position ,hand ,finger ,fx ,fy)))
        
        
        (schedule-event-relative (seconds->ms (+ (exec-time self) bt)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                             (set-finger-up ,hand thumb)))
        
        (schedule-event-relative (seconds->ms (+ (exec-time self) bt)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                             (set-finger-up ,hand ,finger)))
        
        
        (schedule-event-relative (seconds->ms (finish-time self)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                             (set-finger-position ,hand thumb ,(px thumb-start-pos) ,(py thumb-start-pos))))
        
        (schedule-event-relative (seconds->ms (finish-time self)) 'notify-motor-device 
                                 :time-in-ms t :module :motor :output nil
                                 :params `(,device
                                             (set-finger-position ,hand ,finger ,(px finger-start-pos) ,(py finger-start-pos))))))))


(defmethod rotate :around ((mtr-mod motor-module) &key hand finger rotation request-spec)
  (declare (ignore rotation request-spec))
  (let ((real-device (device-for-hand hand :current nil)))
    (cond ((or (null real-device) (not (string-equal (second real-device) "touch")))
           (print-warning "Hand is not on a touch device in request to rotate."))
          ((null (valid-hand hand))
           (print-warning "Rotate request has invalid hand ~s." hand))
          ((null (valid-finger finger))
           (print-warning "Rotate request has invalid finger ~s." finger))
          ((touch-finger-down hand finger :current nil)
           (print-warning "Rotate requested for ~a ~a but that finger is (or will be) down." hand finger))
          
          ((touch-finger-down hand 'thumb :current nil)
           (print-warning "Rotate requested for ~a hand but that thumb is (or will be) down." hand))
          
          (t
           (call-next-method)))))


;; Swipe is multiple fingers down moved together for a given distance
;; and direction at a specified speed (where speed is 1-5 with 5 being fastest
;; and speed sets the Fitts law coefficient as (/ 1 (expt swipe-speed 2)) )
;; finger indicates the 'first' finger and the following (num-fingers - 1)
;; fingers are also used from the ordering: index, middle, ring, pinkie, thumb

(defStyle swipe index-thumb hand finger r theta num-fngrs swipe-speed)


(defmethod swipe :around ((mtr-mod motor-module) &key hand finger r theta num-fngrs swipe-speed request-spec)
  
  (let ((real-device (device-for-hand hand :current nil)))
    (cond ((or (null real-device) (not (string-equal (second real-device) "touch")))
           (print-warning "Hand is not on a touch device in request to swipe."))
          ((null (valid-hand hand))
           (print-warning "Swipe request has invalid hand ~s." hand))
          ((null (valid-finger finger))
           (print-warning "Swipe request has invalid finger ~s." finger))
          ((touch-any-finger-down hand :current nil)
           (print-warning "Swipe requested for ~a hand but there are (or will be) one or more fingers down." hand))
          ((< num-fngrs 1)
           (print-warning "Swipe for ~a hand requested with less than 1 finger ~s." hand num-fngrs))
          ((> (+ num-fngrs (valid-finger finger)) 5)
           (print-warning "Swipe for ~a hand requested with too many fingers (~s) starting with the ~a finger."
                          hand num-fngrs finger))
          ((not (or (numberp swipe-speed) (null swipe-speed)))
           (print-warning "Swipe for ~a hand requested with invalid swipe-speed ~s." hand swipe-speed))
          ((and swipe-speed (or (< swipe-speed 1) (> swipe-speed 5)))
           (print-warning "Swipe for ~a hand requested with invalid swipe-speed ~s." hand swipe-speed))

          (t
           
           ;; fill in the default speed if not provided
           
           (when (null swipe-speed)
             (let ((touch (get-module :touch-interface)))
               (bt:with-recursive-lock-held ((touch-module-lock touch)) 
                 (setf swipe-speed (touch-module-swipe-speed touch)))))
           
           (call-next-method mtr-mod :hand hand :finger finger :r r :theta theta 
                             :num-fngrs num-fngrs :swipe-speed swipe-speed :request-spec request-spec)))))

  
  
(defmethod compute-exec-time ((mtr-mod motor-module) (self swipe))
  (let ((touch (get-module :touch-interface)))
    (bt:with-recursive-lock-held ((touch-module-lock touch))
      (bt:with-recursive-lock-held ((param-lock mtr-mod))
        (setf (move-time self)
          (max (burst-time mtr-mod)
               (fitts mtr-mod
                      (/ 1 (expt (aif (swipe-speed self) it (touch-module-swipe-speed touch)) 2)) 
                      (r self)
                      ;; the width of a target of a swipe should be wide, ie the edge of the device
                      (touch-module-swipe-width touch)))) 
        (+ (init-time mtr-mod)
           (max (burst-time mtr-mod) 
                (randomize-time (move-time self))))))))


(defmethod queue-output-events ((mtr-mod motor-module) (self swipe))
  
  ;; Send the swipe action at execution time to whatever device is current for the hand
  ;; with the distance, direction, and number
  
  (let* ((hand (hand self))
         (device (device-for-hand hand)))
    
    (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                             :time-in-ms t :module :motor :output nil
                             :params `(,device
                                       ((model ,(current-model)) 
                                        (style swipe) (hand ,hand) (finger ,(finger self))
                                        (distance ,(r self)) (direction ,(theta self))
                                        (count ,(num-fngrs self))
                                        (speed ,(swipe-speed self)))))
    
    ;; move the fingers down at init-time
    ;; move to end-points at exec-time 
    ;; lift up a burst-time after exec-time
    ;; move back to start at finish-time
    
    (bt:with-recursive-lock-held ((param-lock mtr-mod)) 
      (let* ((move-vector (vector (r self) (theta self)))
             (bt (burst-time mtr-mod))
             (start-finger (valid-finger (finger self)))
             (count (num-fngrs self))
             (fingers '(index middle ring pinkie thumb)))
        
        (dotimes (i count)
          (let* ((finger (nth (+ i start-finger) fingers))
                 (start (touch-finger-position hand finger))
                 (end (polar-move-xy start move-vector)))
            
            (schedule-event-relative (seconds->ms (init-time mtr-mod)) 'notify-motor-device 
                                     :time-in-ms t :module :motor :output nil
                                     :params `(,device
                                               (set-finger-down ,hand ,finger)))
            
            (schedule-event-relative (seconds->ms (exec-time self)) 'notify-motor-device 
                                     :time-in-ms t :module :motor :output nil
                                     :params `(,device 
                                               (set-finger-position ,hand ,finger ,(px end) ,(py end))))
            
            (schedule-event-relative (seconds->ms (+ (exec-time self) bt)) 'notify-motor-device 
                                     :time-in-ms t :module :motor :output nil
                                     :params `(,device 
                                               (set-finger-up ,hand ,finger)))
            
            
            (schedule-event-relative (seconds->ms (finish-time self)) 'notify-motor-device 
                                     :time-in-ms t :module :motor :output nil
                                     :params `(,device 
                                               (set-finger-position ,hand ,finger ,(px start) ,(py start))))))))))



(extend-manual-requests ((hand-to-touch (:include motor-command)) (hand right)) handle-simple-command-request)

(extend-manual-requests-fct '((tap (:include motor-command)) (hand right) (finger index)) 'handle-style-request)

(extend-manual-requests-fct '((tap-hold (:include motor-command)) (hand right) (finger index)) 'handle-style-request)

(extend-manual-requests-fct '((tap-release (:include motor-command)) (hand right) (finger index)) 'handle-style-request)

(extend-manual-requests-fct '((move-hand-touch (:include motor-command)) (hand right) loc object) 'move-hand-touch-request)

(extend-manual-requests-fct '((index-thumb (:include motor-command)) (hand right) (finger thumb) r theta) 'handle-style-request)

(extend-manual-requests-fct '((swipe (:include motor-command)) (hand right) (finger index) r theta (num-fngrs 1) swipe-speed) 'handle-partial-style-request)

(extend-manual-requests-fct '((pinch (:include motor-command)) (hand right) (finger index) start-width end-width) 'handle-style-request)

(extend-manual-requests-fct '((rotate (:include motor-command)) (hand right) (finger index) rotation) 'handle-style-request)



;; Record that it's loaded so require won't load it again.

(provide "act-touch")
