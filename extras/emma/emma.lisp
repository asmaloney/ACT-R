;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Author      : Mike Byrne
;;; Copyright   : (c)2000-3 Rice U./Mike Byrne, All Rights Reserved
;;; Availability: public domain
;;; Address     : Rice University
;;;             : Psychology Department
;;;             : Houston,TX 77005-1892
;;;             : byrne@acm.org
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filename    : emma.lisp
;;; Version     : 8.1a2
;;;
;;; Description : Implementation of Dario Salvucci's EMMA system for eye
;;;             : movements based on subclassing the Vision Module and making
;;;             : eye movements based on movement styles.
;;;
;;; Bugs        : 
;;;
;;; Todo        : [X] Solve "chunks created early" problem [requires some
;;;             :     rewriting of the Vision Module] - solved w/ACT-R 6.
;;;             : [ ] Tracking.
;;;             : [X] Fix the issue with the unrequested marking. - Not an issue
;;;             :     as of 8.0a1 since it's not replacing the current vision
;;;             :     functionality.
;;;             : [X] Make sure it works properly with respect to the chunk
;;;             :     deletion code now in the main module. - Not an issue
;;;             :     as of 8.0a1 since it's not replacing the current vision
;;;             :     functionality.
;;;             : [ ] Reevaluate the timing since it doesn't look like this
;;;             :     factored in the change from RPM to ACT-R 5 with an additional
;;;             :     50ms production firing (the attention shift time was dropped
;;;             :     from 135ms to 85ms to account for that), but maybe that 
;;;             :     doesn't matter.
;;;
;;; ----- History -----
;;; 00.07.12 Mike Byrne
;;;             :  Incept date.
;;; 01.05.09 mdb [r2]
;;;             : Fixed vector bugs.  Thanks, Dario.
;;; 01.05.?? Dario Salvucci [r3]
;;;             : Noise added to both X and Y of movement.
;;; 01.05.28 mdb [r4]
;;;             : Some minor tinkering with Dario's last changes.
;;; 01.07.02 mdb [r5]
;;;             : Made eye movement relative to next landing spot.
;;; 01.09.21 mdb [r6]
;;;             : Fixed bug in COMPLETE-EYE-MOVE.
;;; 01.10.01 mdb [r7]
;;;             : Added PROC-S method that's actually correct.
;;; 01.11.10 mdb
;;;             : Changed trace log to use CONS rather than LIST to save
;;;             : a smidge of memory, and made tracing the eye path switchable.
;;; 2002.05.04 [r8]
;;;             : Removed unnecessary PROC-STUB stuff.
;;; 2002.05.07 
;;;             : Fixed problem of FIND-LOCATION being clobbered by new preps.
;;;             : Renamed FOCUS-ON to ENCODING-COMPLETE.
;;; 2003.05.07 mdb [r9]
;;;             : Fixed MCL-specific checking. 
;;; 2003.06.23 mdb [r10]
;;;             : Made it easier and cleaner to hook into eye spot updating 
;;;             : for non-MCL Lisps by making a more generic function call 
;;;             : to which specific devices can be tailored, paralleling how
;;;             : it's done for attended location. 
;;;             : Based the EYE-SPOT class on the new RPM-OVERLAY class from
;;;             : the 2.1.3 version of "mcl-device.lisp".
;;; 2003.07.17 mdb [r11]
;;;             : Added a CLEAR :after method to kill any in-progress shift
;;;             : and stripped some dead code.
;;;
;;; 2005.08.10 mjs 
;;;             : Made numerous changes to create an ACT-R 6.0 version. Changes
;;;             : are indicated by the initials mjs.deleted input-q references
;;;             : changed scheduling to  ACT-R 6 scheduling
;;;             : delete preparation-complete from event queue if one is outstanding
;;;             : when moving attention
;;;             : uses ACT-R 6.0 noise
;;;             : no pm- in top-level names
;;;             : redefines Vision Module for EMMA
;;; 2006.07.27 Dan
;;;             : * Added the code needed to draw the blue dot in ACL.
;;;             : * Took the "chunks created early" problem off of to do and
;;;             :   bugs because I'm pretty sure that's not an issue in 6.
;;;             : * Also added the package switches that are in all the other
;;;             :   module files for those that use them.
;;; 2006.09.08 Dan
;;;             : * Changed parameter checks from posnum to nonneg.
;;; 2007.03.16 Dan
;;;             : * Changed add-gaussian-noise to catch when there was 0 
;;;             :   deviation because can't generate a noise of 0.
;;; 2007.04.02 Dan
;;;             : * Minor tweak to the test in add-gaussian-noise because for
;;;             :   really small values of stdev (1*10^-23 for shorts) the old
;;;             :   fix still could have resulted in an error.
;;; 2007.06.25 Dan
;;;             : * Added a test to all the schedule-event-relative calls so that
;;;             :   they are never negative - I don't know which one was actually
;;;             :   the problem so I put a check in all of them.
;;; 2007.06.26 Dan
;;;             : * Modified the #+version> checks in the Allegro based code
;;;             :   because even though they're protected with a #+:allegro CLisp
;;;             :   still seems to break on reading them.
;;; 2007.11.05 Dan
;;;             : * Fixed the #+ checks that were modified with the last
;;;             :   update because I broke them with respect to ACL.
;;; 2007.11.15 Dan
;;;             : * Added a device-update-eye-loc method for the environment
;;;             :   visible-virtual windows.
;;;             :   To use that you need to have EMMA loaded after the environment
;;;             :   which means this file should go in other-files and not tools,
;;;             :   commands, or modules.
;;; 2007.11.20 Dan
;;;             : * Fixed a bug in object-frequency because stringp doesn't 
;;;             :   necessarily return the string itself for true.
;;; 2007.11.20 Dan [4.0a1]
;;;             : * Started to rework for use with the new vision module.
;;; 2007.11.21 Dan
;;;             : * Fixed a bug in the ACL display code that was still referring
;;;             :   to *mp*.
;;; 2007.11.21 Dan
;;;             : * Added a parameter (:vis-obj-freq) to set the default frequency value.
;;; 2007.11.28 Dan
;;;             : * Fixed a bug in move-attention so that when the target object
;;;             :   is no longer available there is still a scheduling of the
;;;             :   encoding complete to trigger the error and set the module back
;;;             :   to the free state.
;;; 2007.12.21 Dan
;;;             : * Changed the LispWorks code for the eye-spot to make it a
;;;             :   subclass of focus-ring to avoid an error for not having an
;;;             :   appropriate update-me method, but it doesn't draw and I don't
;;;             :   really know enough capi code to make the necessary changes to
;;;             :   the device file...
;;; 2008.04.24 Dan
;;;             : * Updated with the recent changes to the main vision module.
;;;             : * Added the requested keyword parameter to encoding-complete
;;;             :   so it doesn't complain when loaded, but it doesn't work
;;;             :   right with the emma module - unrequested attention shifts
;;;             :   will not be marked as such in the buffer because emma uses
;;;             :   move-attention to do so.  Will require modifying move-attention
;;;             :   both in emma and the core module.
;;;             : * Added the :test-feats parameter to the module definition.
;;;             : * Did not add :auto-attend because that doesn't seem like
;;;             :   the sort of thing that one would want when using emma and
;;;             :   it could lead to problems with the timing.
;;; 2008.07.08 Dan
;;;             : * Updated update-attended-loc to work right now that the
;;;             :   visicon chunks get deleted.  May need to fix other places
;;;             :   as well.
;;; 2008.07.15 Dan
;;;             : * Fixed a cut-and-paste error introduced with the last fix.
;;; 2008.09.17 Dan
;;;             : * Updated the LispWorks code for drawing the fixation spot
;;;             :   to match with the new device code from LispWorks.
;;; 2010.05.03 Dan
;;;             : * Changed the :output of the "No visual-object found" event
;;;             :   from 'high to 'medium since that's how it has always printed
;;;             :   anyway because of a bug in filter-test.
;;; 2011.04.28 Dan
;;;             : * Suppress warnings about extending chunks at initial load.
;;; 2011.05.16 Dan
;;;             : * Replaced pm-warning calls with model-warning.
;;; 2011.05.19 Dan
;;;             : * Changed mp-time calls to mp-time-ms calls and adjusted
;;;             :   as needed.
;;; 2011.06.17 Dan
;;;             : * Took the ":exec 'free" out of encoding-complete's state change
;;;             :   since that shouldn't happen.
;;; 2012.07.02 Dan [4.0a2]
;;;             : * Update to support one eye-spot per model in multiple window
;;;             :   and/or multiple model situations.  Like the fixation marker
;;;             :   there is a function called eye-spot-marker which can be used
;;;             :   to store the current marker instead of the *eye-spot* variable.
;;;             :   Like device-update-attended-loc the device-update-eye-loc
;;;             :   method will now occasionally get called with nil to indicate
;;;             :   that the marker should be erased.
;;;             : * Only show the eye spot when show-focus-p is true.
;;;             : * Add a modified install-device so that it can call the 
;;;             :   device-update-eye-loc method when the model switches devices
;;;             :   and always start eyes at location 0,0 when a device is installed.
;;;             : * Also updating some other code that seems to have been neglected
;;;             :   along the way (particularly the :delete-visicon-chunks,
;;;             :   :scene-change-threshold, and :viewing-distance parameters).
;;; 2012.09.07 Dan
;;;             : * Adding an eye spot for CCL since there's now a device for it.
;;;             : * Fixed a bug in the environment version of the eye-spot code.
;;; 2013.01.09 Dan
;;;             : * Replaced the #@ in the CCL code with make-point since that 
;;;             :   causes problems in all other Lisps even inside the #+CCL...
;;;             : * Removed the clearing of the eye-spot from the reset function
;;;             :   since install-device handles that and it's important that an
;;;             :   "old" spot persist to that it can be erased from the previous
;;;             :   device.
;;; 2013.09.26 Dan
;;;             : * To keep the display code separate from the model actions
;;;             :   using an after method on update-device to draw the eye-loc
;;;             :   instead of calling it from set-eye-loc.
;;; 2014.02.14 Dan [4.1]
;;;             : * Updated with the changes to the vision module to make
;;;             :   distance for visual locations be in pixels.
;;; 2014.03.17 Dan [5.0]
;;;             : * Changed the query-buffer call to be consistent with the new
;;;             :   internal code.
;;; 2014.07.07 Dan [6.0]
;;;             : * Updated for ACT-R 6.1.
;;; 2015.03.20 Dan
;;;             : * Attend failure sets the buffer failure flag using set-buffer-failure.
;;; 2015.03.23 Dan
;;;             : * The visual buffer failures are now marked as to whether it
;;;             :   was a requested encoding or an automatic one which failed.
;;; 2015.05.20 Dan
;;;             : * Updated the calls to xy-loc to include the module to allow
;;;             :   for the use of device specific coordinate slots instead of just
;;;             :   screen-x, screen-y, and distance.
;;; 2015.06.05 Dan
;;;             : * Schedule all events in ms now.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2015.07.30 Dan [6.1]
;;;             : * Added the unstuffing/overwriting params and actions.
;;; 2015.08.13 Dan
;;;             : * Changed all rand-time calls to randomize-time.
;;; 2015.09.23 Dan [7.0]
;;;             : * Completing the requests to the visual buffer for the new utility
;;;             :   learning credit assignment mechanism.  
;;;             :   One issue is that complete-eye-move gets called at multiple
;;;             :   points in the task so it can't be used to actually complete
;;;             :   the request even though it's a movement style and would do so
;;;             :   "automatically".  Instead, just use encoding-complete like 
;;;             :   the standard vision module to mark the request done since the
;;;             :   current module's request function is still called which sets
;;;             :   the last request slot of the module.
;;; 2016.03.14 Dan
;;;             : * Added the provide at the bottom for use with require-extra.
;;; 2016.07.08 Dan [7.1]
;;;             : * Added the new :tracking-clear parameter from the vision module.
;;; 2016.07.20 Dan
;;;             : * Updated things to use set-current-marker and clof where it was
;;;             :   taking (xy-loc current-marker) to deal with potentially 
;;;             :   deleted feature chunk issues.
;;;             : * Update encoding-complete and all the calls to it because it
;;;             :   takes an extra parameter for location vector.
;;;             : * Updated the calls to get-obj-at-location since it also takes
;;;             :   the vector now too.
;;; 2016.11.17 Dan
;;;             : * Fix the redefinition of open-exp-window to use current-mp.
;;; 2018.09.06 Dan [8.0a1]
;;;             : * Start of updates for 7.x.
;;;             : * Making it its own module now, and going to implement it 
;;;             :   through mechanisms that need to be added to vision module
;;;             :   (hooks, monitors, device?).
;;; 2018.09.18 Dan
;;;             : * Using the attn-module as the parent class, and then using the
;;;             :   current-marker to hold the position, last-prep instead of prep-
;;;             :   event, and the param-lock for everything.
;;; 2018.09.19 Dan
;;;             : * Add a parameter to control whether it is enabled or not.
;;;             : * Set the default values for the factor and exponent to those
;;;             :   used in the paper.
;;;             : * Set the default frequency to .1 as used for numbers and letters
;;;             :   in the paper.  That means with the other default parameters a 
;;;             :   saccade of 4.5 dva (about 85 pixels on the default AGI windows)
;;;             :   has an encoding that takes ~85ms (the default visual encoding
;;;             :   time): 
;;;             :   > (* .006 (- (log .1)) (exp (* .4 4.5)))
;;;             :   0.083578974
;;; 2018.09.21 Dan
;;;             : * Added parameters for the slot to use in the visual objects
;;;             :   to determine frequency, to set the init time of the 
;;;             :   saccades, and to set the feature prep cost of saccades.
;;;             : * Set-eye-loc notifies the devices of the position -- there's
;;;             :   always an eye position even if attention is cleared.
;;;             : * Can't use last-prep since that's part of the general pm
;;;             :   style handling code so need to put prep-event back in.
;;;             : * Generate a signal for monitoring purposes when the eye
;;;             :   moves called "current-eye-location" passed the new x and y
;;;             :   values.
;;; 2018.09.24 Dan
;;;             : * Don't need to distinguish whether there's an object there or
;;;             :   not -- absence of a target just means default frequency at
;;;             :   this point.  May want a separate parameter for that in the
;;;             :   future.
;;;             : * Need to handle state error queries since the generic method
;;;             :   doesn't.
;;; 2019.02.22 Dan  [8.1a1]
;;;             : * Don't throw out the z component in the current marker, and 
;;;             :   just compute the angles instead of going to pixels for the
;;;             :   distances.  Assuming no noise in z dimension and that there's
;;;             :   no additional cost for change in z -- the subtended angle is
;;;             :   all that matters.
;;;             : * Sending all three values to the set-eye-loc command, and
;;;             :   assuming default distance when only two values provided to
;;;             :   set-eye-location.
;;;             : * The eyeloc signal now also gets all three.
;;;             : * Switched to using tertiary reset to get ppi and viewing
;;;             :   distance after model params set.
;;;             : * Don't compute r-theta and saccade separately, just create
;;;             :   the saccade immediately and store the target point in it
;;;             :   (using a num-to-prepare to ingore the extra feature).
;;; 2020.01.10 Dan [8.1a2]
;;;             : * Removed the #' and lambdas from the module interface 
;;;             :   functions since that's not allowed in the general system
;;;             :   now.
;;; 2020.08.26 Dan
;;;             : * Removed the path for require-compiled since it's not needed
;;;             :   and results in warnings in SBCL.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "GENERAL-PM")

;;; Note:
;;; This module's operation depends upon the get-obj-at-location method from the 
;;; default vision module code which is not a documented command.


(defclass emma-vis-mod (attn-module)
  ((freq-ht :accessor freq-ht :initarg :freq-ht
            :initform (make-hash-table :test 'equalp))
   (enc-factor :accessor enc-factor :initarg :enc-factor )
   (enc-exponent :accessor enc-exponent :initarg :enc-exponent)
   (base-exe :accessor base-exe :initarg :base-exe)
   (sacc-rate :accessor sacc-rate :initarg :sacc-rate)
   (shift-start :accessor shift-start :initarg :shift-start :initform 0)
   (shift-duration :accessor shift-duration :initarg :shift-duration :initform nil)
   (shift-target :accessor shift-target :initarg :shift-target :initform nil)
   (next-loc :accessor next-loc :initform nil) ;;holds the next landing loc
   (show-focus-p :accessor show-focus-p :initform nil)
   (default-freq :accessor default-freq)
   (spot-color :accessor spot-color)
   (freq-feature :accessor freq-feature)
   (enabled :accessor enabled)
   (encoded :accessor encoded)
   (final-shift :accessor final-shift)
   (current-feat :accessor current-feat)
   (current-scale :accessor current-scale)
   (prep-event :accessor prep-event))
  (:default-initargs
      :name :emma
    :version-string "8.1a2"
    :current-marker #(0 0 1)))


(defun emma-detect-device-install (device)
  (let ((emma (get-module :emma)))
    (when emma
      (bt:with-recursive-lock-held ((param-lock emma))
        (when (and (enabled emma) (show-focus-p emma) (spot-color emma))
          (notify-device device (list "eyeloc" (px (current-marker emma)) (py (current-marker emma)) (pz (current-marker emma)) (color->name (spot-color emma) "blue"))))))))
  
(add-act-r-command "emma-detect-device-install" 'emma-detect-device-install "Monitor for installing-vision-device to send eye location from EMMA. Do not call.")

(monitor-act-r-command "installing-vision-device" "emma-detect-device-install")

(add-act-r-command "current-eye-location" nil "Signal indicating the EMMA module's eye position has changed.  Params: x y z")

(defmethod set-eye-loc ((emma emma-vis-mod) (newloc vector))
  (bt:with-recursive-lock-held ((param-lock emma))
    (if (= (length newloc) 2)
        (setf (current-marker emma)
          (vector (px newloc) (py newloc) 
                  (* (get-parameter-value :viewing-distance)
                     (get-parameter-value :pixels-per-inch))))
      (if (= (length newloc) 3)
          (setf (current-marker emma) newloc)
        (progn
          (print-warning "Invalid position ~s in call to set-eye-loc position not changed.")
          (return-from set-eye-loc))))
    (dispatch-apply "current-eye-location" (px (current-marker emma)) (py (current-marker emma)) (pz (current-marker emma)))
    (when (show-focus-p emma)
      (awhen (spot-color emma)
             ;; send window handler messages
             ;; There's always an eye position even when not an attended location
             (let ((update-list
                    (list "eyeloc" (px (current-marker emma)) (py (current-marker emma)) (pz (current-marker emma)) (color->name it "blue"))))
               (dolist (d (current-devices "vision"))
                 (notify-device d update-list)))))))


(defun emma-clear-action ()
  (let ((emma (get-module :emma)))
    (when emma ;; could check enabled, but that's probably about the same cost as the setf anyway...
      (bt:with-recursive-lock-held ((param-lock emma))
        ;; can't stop current eye movement but clear the saved features
        (setf (last-prep emma) nil)))))


(add-act-r-command "emma-clear-action" 'emma-clear-action "Monitor for visual-clear signal in EMMA. Do not call.")

(monitor-act-r-command "visual-clear" "emma-clear-action")


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Saccade movement style

(defStyle saccade () r theta trgt new-loc)

;;; Don't count the new-loc slot which is only for
;;; internal bookkeeping purposes.

(defmethod num-to-prepare ((sacc saccade))
  3)

;;; COMPUTE-EXEC-TIME      [Method]
;;; Date        : 01.05.09
;;; Description : Execution time is the sum of the base execution time and
;;;             : the distance times the rate.

(defmethod compute-exec-time ((emma emma-vis-mod) (mvmt saccade))
  (randomize-time
   (bt:with-recursive-lock-held ((param-lock emma))
     (+ (init-time emma) (* (r mvmt) (sacc-rate emma)) 
        (base-exe emma)))))


;;; COMPUTE-FINISH-TIME      [Method]
;;; Date        : 01.05.09
;;; Description : The finish time is the same as the execution time for
;;;             : saccades.

(defmethod compute-finish-time ((emma emma-vis-mod) (mvmt saccade))
  (exec-time mvmt))


;;; FEAT-DIFFERENCES      [Method]
;;; Date        : 01.05.09
;;; Description : How many features to prepare?  One for direction and one for
;;;             : distance, if they're different.

(defmethod feat-differences ((s1 saccade) (s2 saccade))
  (let ((nfeats 0))
    (unless (distance= (r s1) (r s2)) (incf nfeats))
    (unless (direction= (theta s1) (theta s2)) (incf nfeats))
    nfeats))


;;; TOTAL-TIME      [Method]
;;; Date        : 01.05.09
;;; Description : The total time for a saccade is the simple sum of the
;;;             : prep time and the execution time.

(defmethod total-time ((sacc saccade))
  (+ (fprep-time sacc) (exec-time sacc)))



;;;; ---------------------------------------------------------------------- ;;;;
;;;; Module-level stuff

(defun emma-attention-hook (current-loc new-loc feat-chunk scale)
  (declare (ignore current-loc))
  (let ((emma (get-module :emma))
        (end-loc (coerce new-loc 'vector)))
    (when emma
      (bt:with-recursive-lock-held ((param-lock emma))
        (when (enabled emma)
          
          ;; interrupt an ongoing preparation unless
          ;; it's also executing now as well.
          
          (unless (eq 'BUSY (exec-s emma))  
            (awhen (prep-event emma)
                   (delete-event it))
            (setf (prep-event emma) nil))
          
          ;; we need to know what's there to compute the encoding time
          ;; and for that we're going to rely on an undocumented function
          ;; in the vision module for getting that information.
          
          (setf (current-feat emma) feat-chunk)
          (setf (current-scale emma) scale)
          
          (let ((return-obj (get-obj-at-location (get-module :vision) feat-chunk end-loc scale)))
            
            ;; doesn't matter anymore if there's something there or not
            ;; either case works the same
            
            (let* ((start-loc (next-destination emma))
                   (sacc (compute-saccade-r-theta emma start-loc end-loc)))
              
              (change-state emma :proc 'BUSY)
              (schedule-event-now 'change-state :params (list emma :proc 'busy) :module :emma :output nil :priority :max)
              (setf (shift-duration emma) (recog-time emma return-obj sacc))
              ;; don't need the old chunk anymore
              (awhen (shift-target emma) (purge-chunk-fct it))
              
              (setf (shift-target emma) return-obj)
              (setf (final-shift emma) end-loc)
              (initiate-eye-move emma (shift-duration emma) sacc))))))
          
    t))

(defgeneric next-destination (emma)
  (:documentation  "Returns the next landing location for EMMA."))

(defmethod next-destination ((emma emma-vis-mod))
  (if (eq 'FREE (exec-s emma))
    (current-marker emma)
    (next-loc emma) ;;; mjs since no more input-q, get from eye-mod, saved by complete-eye-move
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; INITIATE-EYE-MOVE      [Method]
;;; Date        : 01.05.09
;;; Description : First, build the actual saccade movement itself.  This is a
;;;             : CLOS object whose class is generated by the DEFSTYLE call in
;;;             : this file.  Do all the state changes and queue up the
;;;             : preparation complete notice, which will then generate an
;;;             : eye movement.  Now, we only want to queue the FOCUS-ON when
;;;             : the object will be recognized before the eye movement is
;;;             : complete, so do that check and queue that event if necessary.

(defgeneric initiate-eye-move (emma recog-time sacc)
  (:documentation "Given a recognition time and a movement spec, begin an eye movement."))

(defmethod initiate-eye-move ((emma emma-vis-mod) (recog-time number) (sacc saccade))
  (bt:with-recursive-lock-held ((param-lock emma))
    (let ()
      (setf (trgt sacc) (shift-target emma))
      ;; always start the preparation of the eye move
      (setf (shift-start emma) (mp-time-ms))
      (change-state emma :prep 'BUSY)
      (schedule-event-now 'change-state :params (list emma :prep 'busy) :module :emma :output nil :priority :max)
      (setf (last-prep emma) sacc)
      (setf (prep-event emma)  ;;mjs
        (schedule-event-relative (seconds->ms (max 0 (fprep-time sacc)))  'PREPARATION-COMPLETE
                                 :time-in-ms t
                                 :destination :emma
                                 :module :emma
                                 :output 'medium
                                 :details (concatenate 'string "Preparation-complete " (write-to-string (trgt sacc)))))
      
      ;; only sometimes schedule the encoding complete
      
      (if (<= recog-time (total-time sacc))
          (progn
            (schedule-encoding-complete (max 0 recog-time))
            (setf (encoded emma) t))
        (setf (encoded emma) nil)))))


;;;mjs after method to clear prep event
(defmethod preparation-complete :after ((emma emma-vis-mod))
  (setf (prep-event emma) nil))


;;; COMPLETE-EYE-MOVE      [Method]
;;; Date        : 01.05.09
;;; Description : When an eye movement completes:
;;;             : [1] Update the module's state.
;;;             : [2] Compute the new recognition time for the target based
;;;             :     on the new distance.
;;;             : [3] Initiate an eye move.  Note that this movement should
;;;             :     die if recog time based on the eye's new location is
;;;             :     small.


(defgeneric complete-eye-move (emma marker xyloc)
  (:documentation "Called when an eye movement completes to handle all the updating."))

(defmethod complete-eye-move ((emma emma-vis-mod) (trgt symbol) (xyloc vector))
  ;; handle the module-level stuff
  (set-eye-loc emma xyloc)
  ;; when we're still moving attention to the same target, we might need
  ;; to generate another shift.
  (bt:with-recursive-lock-held ((param-lock emma))
    (when (null (encoded emma)) ;; don't care about the target anymore
                                   ;;    (eq trgt (shift-target emma)))
      
      (let* ((new-target (get-obj-at-location (get-module :vision) (current-feat emma) (final-shift emma) (current-scale emma)))
             (start-loc (current-marker emma))
             (end-loc (final-shift emma))
             (sacc (compute-saccade-r-theta emma start-loc end-loc))
             (new-duration
              (* (recog-time emma new-target sacc)
                 (- 1 (/ (ms->seconds (- (mp-time-ms) (shift-start emma)))  ;;mjs pm-time
                         (shift-duration emma))))))
        
        ;; don't need the old chunk anymore
        (awhen (shift-target emma) (purge-chunk-fct it))
        (setf (shift-target emma) new-target)
            
        (setf (shift-start emma) (mp-time-ms))
        (setf (shift-duration emma) new-duration)
        (initiate-eye-move emma new-duration sacc))))
  (finish-movement emma (last-prep emma)))


;;; PERFORM-MOVEMENT      [Method]
;;; Date        : 01.05.09
;;; Description : Performing the movement simply involves figuring out when
;;;             : to call COMPLETE-EYE-MOVE and updating the eye location.

(defmethod perform-movement ((emma emma-vis-mod) (sacc saccade))
  (change-state emma :exec 'BUSY)
  (schedule-event-now 'change-state :params (list emma :exec 'busy) :module :emma :output nil :priority :max)
  (bt:with-recursive-lock-held ((param-lock emma))
    (let ()
      (setf (next-loc emma) (new-loc sacc))
      (schedule-event-relative (seconds->ms (min (max 0 (exec-time sacc)) (init-time emma))) 'initiation-complete 
                             :time-in-ms t :destination :emma :module :emma)
      (schedule-event-relative (seconds->ms (max 0 (exec-time sacc)))  'COMPLETE-EYE-MOVE ;;;mjs
                               :time-in-ms t
                               :destination :emma
                               :module :emma
                               :params `(,(trgt sacc) ,(new-loc sacc))
                               :output 'medium
                               :details  (concatenate 'string "Complete-eye-movement " (write-to-string (trgt sacc)) " " (write-to-string (new-loc sacc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric compute-saccade-r-theta (emma start-loc end-loc)
  (:documentation "Adds gaussian noise to <end-loc> and returns r-theta vector for saccade destination."))

(defmethod compute-saccade-r-theta ((emma emma-vis-mod) (start-loc vector) (end-loc vector))
  (let* ((stddev (* 0.1 (dist start-loc end-loc)))
         (end-loc-noisy 
          (vector (round (add-gaussian-noise (px end-loc) stddev))
                  (round (add-gaussian-noise (py end-loc) stddev))
                  (pz end-loc)))
         (sacc-mvmt (make-instance 'saccade
                      :r (angle-between-vectors start-loc end-loc-noisy)
                      :theta (atan (- (py start-loc) (py end-loc)) ;; y coords are reversed since - is up
                                   (- (px end-loc) (px start-loc)))
                      :new-loc end-loc-noisy)))
    (setf (fprep-time sacc-mvmt)
      (randomize-time (compute-prep-time emma sacc-mvmt)))
    (setf (exec-time sacc-mvmt)
      (compute-exec-time emma sacc-mvmt))
    sacc-mvmt))

(defgeneric recog-time (emma obj sacc)
  (:documentation "Based on Dario's equation, compute the recognition 
                   time for an object given the displacement from the current POR."))

(defmethod recog-time ((emma emma-vis-mod) obj (sacc saccade))
  (let ((freq (object-frequency emma obj))
        )
    (bt:with-recursive-lock-held ((param-lock emma))
      (randomize-time (* (enc-factor emma)
                         (- (log freq))
                         (exp (* (r sacc) (enc-exponent emma))))))))


;;; OBJECT-FREQUENCY      [Method]
;;; Description : Use current default value if no entry.

(defgeneric object-frequency (emma obj)
  (:documentation "Compute the frequency of ocurrence of a visual 
                   object, which is used to compute the recognition time."))

(defmethod object-frequency ((emma emma-vis-mod) obj)
  (bt:with-recursive-lock-held ((param-lock emma))
    (aif (and obj 
              (gethash (fast-chunk-slot-value-fct obj (freq-feature emma)) (freq-ht emma)))
         it
         (default-freq emma))))
  

(defgeneric register-obj-freq (emma name freq)
  (:documentation "Store a frequency for a particular symbol."))

(defmethod register-obj-freq ((emma emma-vis-mod) name freq)
  (bt:with-recursive-lock-held ((param-lock emma))
    (setf (gethash name (freq-ht emma)) freq)))

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Utilities

(defun add-gaussian-noise (x stddev)
  "Adds pseudo-Gaussian noise to mean x with a given stddev."
  (let* ((v (* stddev stddev))
         (s (/ (sqrt (* 3.0 v)) pi)))
    (+ x (if (zerop s) 0 (act-r-noise s))))) ;;;mjs


;;;; ---------------------------------------------------------------------- ;;;;
;;;; toplevel stuff


(defun register-feature-frequency (feature frequency)
  "Register the frequency of an arbitrary value."
  (let ((emma (get-module :emma)))
    (when emma
      (if (and feature (numberp frequency) (<= 0 frequency 1))
          (register-obj-freq emma feature frequency)
        (print-warning "Register-feature-frequency requires a feature and a frequency between 0 and 1, but given ~s and ~s." feature frequency)))))


(defun set-eye-location (loc)
  (let ((emma (get-module :emma)))
    (when emma
      (bt:with-recursive-lock-held ((param-lock emma))
        (if (enabled emma)
            (if (and (listp loc) (>= 3 (length loc) 2))
                (progn
                  (set-eye-loc emma (coerce loc 'vector))
                  loc)
              (print-warning "Location for set-eye-location must be a list with 2 or 3 values, but given ~s" loc))
          (print-warning "EMMA module is not enabled when trying to set-eye-location."))))))

(defun current-eye-location ()
  (let ((emma (get-module :emma)))
    (when emma
      (bt:with-recursive-lock-held ((param-lock emma))
        (if (enabled emma)
            (coerce (current-marker emma) 'list)
          (print-warning "EMMA module is not enabled for current-eye-location."))))))

(add-act-r-command "register-feature-frequency" 'register-feature-frequency "Set the frequency of visual features which have the indicated value. Params: value frequency")
(add-act-r-command "set-eye-location" 'set-eye-location "Set the current position of the model's eye for EMMA. Params: location")
(add-act-r-command "current-eye-location" 'current-eye-location "Get the current position of the model's eye from EMMA. No params")


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Init stuff

;;;mjs create EMMA module

(defun create-emma-module (model-name)
  (declare (ignore model-name))
  (make-instance 'emma-vis-mod))


(defmethod reset-emma-module ((emma emma-vis-mod))
  (reset-pm-module emma)
  (bt:with-recursive-lock-held ((param-lock emma))
    (setf (shift-start emma) 0)
    (setf (shift-target emma) nil)
    (setf (last-prep emma) nil)
    (setf (prep-event emma) nil)
    (setf (next-loc emma) nil)
    (setf (encoded emma) nil)
    (setf (final-shift emma) nil)
    (setf (current-marker emma) 
      (vector 0 0 (* (get-parameter-value :viewing-distance)
                     (get-parameter-value :pixels-per-inch))))
    (clrhash (freq-ht emma))))


(defun emma-query (emma buffer slot value)
  (cond ((and (eq slot 'state) (eq value 'error))
         nil)
        (t (generic-state-query emma buffer slot value))))

(defun params-emma-module (emma param)
  (bt:with-recursive-lock-held ((param-lock emma))
    (if (consp param)
        (case (car param)
          (:emma
           (if (cdr param)
               (unless (eq 'emma-attention-hook (get-parameter-value :visual-encoding-hook))
                 (set-parameter-value :visual-encoding-hook 'emma-attention-hook))
             (when (eq 'emma-attention-hook (get-parameter-value :visual-encoding-hook))
               (set-parameter-value :visual-encoding-hook nil)))
           (setf (enabled emma) (cdr param)))
           (:saccade-feat-time
           (setf (feat-prep-time emma) (cdr param)))
          (:SACCADE-INIT-TIME
           (setf (init-time emma) (cdr param)))
          (:VISUAL-ENCODING-FACTOR
           (setf (enc-factor emma) (cdr param)))
          (:VISUAL-ENCODING-EXPONENT
           (setf (enc-exponent emma) (cdr param)))
          (:EYE-SACCADE-RATE
           (setf (sacc-rate emma) (cdr param)))
          (:SACCADE-BASE-TIME
           (setf (base-exe emma) (cdr param)))
          (:vis-obj-freq
           (setf (default-freq emma) (cdr param)))
          (:show-focus
           ;; if it was on erase the spot
           (when (and (show-focus-p emma) (null (cdr param)) (spot-color emma))
             (dolist (d (current-devices "vision"))
               (notify-device d (list "cleareyeloc"))))
           (setf (show-focus-p emma) (cdr param)))
          (:eye-spot-color
           ;; if it was on erase the spot
           (when (and (show-focus-p emma) (null (cdr param)) (spot-color emma))
             (dolist (d (current-devices "vision"))
               (notify-device d (list "cleareyeloc"))))
           (setf (spot-color emma) (cdr param)))
          (:freq-feature
           (setf (freq-feature emma) 
             (if (stringp (cdr param))
                 (string->name (cdr param))
               (cdr param)))))
      (case param
        (:emma
         (enabled emma))
        (:saccade-feat-time
         (feat-prep-time emma))
        (:SACCADE-INIT-TIME
         (init-time emma))
        (:VISUAL-ENCODING-FACTOR
         (enc-factor emma))
        (:VISUAL-ENCODING-EXPONENT
         (enc-exponent emma))
        (:EYE-SACCADE-RATE
         (sacc-rate emma))
        (:SACCADE-BASE-TIME
         (base-exe emma))
        (:vis-obj-freq
         (default-freq emma))
        (:eye-spot-color
         (spot-color emma))
        (:freq-feature
         (freq-feature emma))))))

(defun emma-buffer-status ()
  (print-module-status (get-module :emma)))

(defun eye-spot-color-value-test (x)
  (or (tornil x) (symbolp x) (stringp x)))

(defun freq-feature-value-test (x)
  (or (symbolp x) (stringp x)))

(define-module-fct :emma 
    (list (define-buffer-fct 'emma  ;; need a buffer for query and module state info
              :queries '(modality preparation execution processor last-command)
            :status-fn 'emma-buffer-status))
  
  (list 
   (define-parameter :emma :valid-test 'tornil :warning "T or nil" :default-value nil :documentation "enable the EMMA extension for the vision module")
   
   (define-parameter :show-focus :owner nil)
   (define-parameter  :VISUAL-ENCODING-FACTOR
                      :valid-test 'nonneg
                      :warning "a non-negative number" 
                      :default-value  0.006
                      :documentation "Visual encoding time factor (K)")
   (define-parameter  :VISUAL-ENCODING-EXPONENT
                      :valid-test 'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.4
                      :documentation "Visual encoding time exponent (k)")
   (define-parameter :EYE-SACCADE-RATE
                      :valid-test 'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.002
                      :documentation "Saccade rate per degree of visual angle")
   (define-parameter :SACCADE-BASE-TIME
                      :valid-test 'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.020
     :documentation "Base saccade time")
   (define-parameter :SACCADE-INIT-TIME
                      :valid-test 'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.050
     :documentation "Non-labile saccade time")
   (define-parameter :saccade-feat-time
                      :valid-test 'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.050
     :documentation "Cost to prepare each feature of a saccade")
   (define-parameter :vis-obj-freq
                      :valid-test 'nonneg
                      :warning "a non-negative number" 
                      :default-value 0.1
     :documentation "Default visual object frequecny")
   (define-parameter :eye-spot-color
     :valid-test 'eye-spot-color-value-test
     :default-value 'blue
     :warning "T, NIL, a symbol, or string"
     :documentation "Show the eye position in the GUI when vision shows the attention ring?  T, a string, or symbol will enable, and some devices will use the value to indicate a color.")
   (define-parameter :freq-feature
       :valid-test 'freq-feature-value-test
     :default-value 'value
     :warning "a symbol or string"
     :documentation "The slot of a feature's visual object which is used to determine the frequency.")
   )
  :version (version-string (make-instance 'emma-vis-mod))
  :documentation "EMMA extension for the vision module"
  :creation 'create-emma-module 
  :reset '(nil nil reset-emma-module)
  :query 'emma-query
  :params 'params-emma-module
)                 


(provide "emma")
