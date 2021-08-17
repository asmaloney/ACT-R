;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c) 2012 Cogscent, LLC
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
;;; Filename    : touch-demo-model.lisp
;;; Revision    : 3
;;; 
;;; Description : A demonstration of ACT-Touch
;;;
;;; Usage	: Load this file and call run-touch-demo to run the demonstration
;;;             : model (see below for options).  Does not require loading act-touch separately.
;;; 
;;; 
;;; Bugs        : None known
;;;
;;; To do       : Nothing
;;;
;;; ----- History -----
;;; 2012.09.29 fpt 1
;;;		: Inception: Forked from act-touch.lisp
;;; 2014.12.01 Dan Bothell
;;;             : Added the run-touch-demo function which includes the code 
;;;             : from the after method that was previously defined in the 
;;;             : virtual-multitouch-device.lisp file.
;;;             : Added a chunk-type named goal with a slot named op.
;;;             : Added chunk definitions for all of the chunks in the model.
;;; 2016.03.14 Dan Bothell
;;;             : Added the require-extra so that it automatically loads the
;;;             : act-touch extension if it's not already available.
;;; 2018.10.25 Dan Bothell
;;;             : Use the new start-hand-at-touch command to position the hands
;;;             : so the offsets are set correctly.
;;;             : Changed the productions so that it performs all of the actions
;;;             : sequentially without having to change the op slot from code.
;;;             : Added some different speed swipes.
;;;             : Changed the queries to state free so it's easier to see
;;;             : the individual actions.
;;;             : Added another rotate to see the difference in time by angle now.
;;; 2018.11.01 Dan Bothell [2]
;;;             : This version runs with ACT-R 7.6+ (7.12 when updated).
;;;             : Renamed to demo2.
;;;             : This demo installs a touch device and starts both hands at
;;;             : that device before performing some actions.
;;; 2018.11.05 Dan Bothell [3]
;;;             : Moved demo1 and 2 versions together and renamed touch-demo-model.
;;;             : Added the visible window option, some left hand actions, and
;;;             : adjusted the movements so they're on the actual window used.
;;;             : Put an image on the window for the model to tap and drag when
;;;             : it is a visible device.
;;; 2018.11.06 Dan Bothell
;;;             : Set the touch-drag-delay parameter and move the hand between
;;;             : the same places once with a finger down and once without.
;;;             : Don't start both hands at the device and instead use the hand-
;;;             : to-touch action to move one there.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(clear-all)
(require-extra "act-touch")


;;; Function to run the demo model.
;;;
;;; Has three possible modes run as follows:
;;;
;;; (run-touch-demo) 
;;;  Runs the model without a touch device installed to just demonstrate the
;;;  actions being performed.  No monitorable actions are generated, and the
;;;  finger positions are not actually changed from the motor module's 
;;;  default locations (which affects the Fitts Law timing values) because
;;;  the touch device is needed to track the finger positions in pixel space
;;;  (in the same way that the mouse cursor moves in pixel space although the 
;;;  model's hand doesn't really move from the location of the cursor itself).
;;;
;;; (run-touch-demo t)
;;;  Installs a touch device and then runs the model.  The touch device
;;;  will generate the signals which can be monitored, and some simple
;;;  monitoring functions will just print out what happens.
;;;
;;; (run-touch-demo :window)
;;;  Installs an experiment window (visible if possible) and then installs
;;;  the touch device associating it with that window.  Taps will result in
;;;  click actions which the window can respond to in addition to the touch
;;;  module generating the signals which can be monitored.  The demo adds
;;;  a button with a click action which will report the click and add two
;;;  additional monitors which will move the button between the next tap-hold
;;;  and tap-relase actions.


(defun run-touch-demo (&optional mode)
  
  (reset)
  
  (setup-action-monitors)
  
  (cond ((null mode)
         
         ;; add a visual feature that corresponds to the button in the windowed
         ;; version and automatically move it as the model would on the window
         
         (let ((feature (first (add-visicon-features '(screen-x 350 screen-y 175 width 100 height 50)))))
           (schedule-event-relative 3 'modify-visicon-features :output nil :maintenance t :params (list (list feature 'screen-x 450 'screen-y 300))))
         
         (run 20))
        
        ((eq mode t)
         
         ;; set the starting points to the same places as in the windowed version
         (sgp :left-start (100 300) :right-start (600 300))
         
         ;; Start the right hand (the default) at the touch device and automatically install the device.
         (start-hand-at-touch) 
         
         ;; Indicate there is a device for the model to move its hand to
         (mod-focus device t)
         
         ;; add a visual feature that corresponds to the button in the windowed
         ;; version and automatically move it as the model would on the window
         
         (let ((feature (first (add-visicon-features '(screen-x 350 screen-y 175 width 100 height 50)))))
           (schedule-event-relative 3 'modify-visicon-features :output nil :maintenance t :params (list (list feature 'screen-x 450 'screen-y 300))))
         
         (run 20))
        
        ((eq mode :window)
         
         ;; display the finger locations in the window when it's visible
         (sgp :show-fingers t)
         
         ;; create a window which is visible if possible
         (let ((w (open-exp-window "test" :visible t :width 500 :height 400 :x 100 :y 100)))
           
           ;; install the window device
           (install-device w)
           
           ;; add a button with a function to call when clicked
           (setf *created-button* (add-button-to-exp-window w :text "" :color "blue" :x 200 :y 50 :width 100 :height 50 :action "demo-button-clicked"))
           
           ;; install the touch device and associate it with the window above
           (install-device (list "motor" "touch" w))
           
           ;; start the right hand at the touch device
           (start-hand-at-touch)
           
           ;; indicate there is a device for the model to move its hand to
           (mod-focus device t)

           (run 20)))
        (t
         (print-warning "Invalid mode ~s given to run-touch-demo.  Possible options are nil, t, or :window." mode)))
  
  (remove-action-monitors))


;;; Variables for handling the interaction with a window.

(defvar *created-button*)
(defvar *tapped-item*)

;; Clear the interaction variables and add the monitoring functions
;; to the associated actions.

(defun setup-action-monitors ()
  (setf *created-button* nil)
  (setf *tapped-item* nil)
  
  ;; All of the signals generated by the touch device
  (monitor-act-r-command "touch-tap" "demo-tap")
  (monitor-act-r-command "touch-point" "demo-point")
  (monitor-act-r-command "touch-swipe" "demo-swipe")
  (monitor-act-r-command "touch-tap-hold" "demo-tap-hold")
  (monitor-act-r-command "touch-tap-release" "demo-tap-release")
  (monitor-act-r-command "touch-pinch" "demo-pinch")
  (monitor-act-r-command "touch-rotate" "demo-rotate"))

;; Remove the monitors to avoid any problems with other tasks
;; run after this one that also use the touch device.

(defun remove-action-monitors ()
  (remove-act-r-command-monitor "touch-tap" "demo-tap")
  (remove-act-r-command-monitor "touch-point" "demo-point")
  (remove-act-r-command-monitor "touch-swipe" "demo-swipe")
  (remove-act-r-command-monitor "touch-tap-hold" "demo-tap-hold")
  (remove-act-r-command-monitor "touch-tap-release" "demo-tap-release")
  (remove-act-r-command-monitor "touch-pinch" "demo-pinch")
  (remove-act-r-command-monitor "touch-rotate" "demo-rotate"))


;; Simple functions which are monitoring for the touch devices
;; signals and print out the values provided.

(defun demo-tap (model hand finger x y)
  (act-r-output "model ~s tapped the ~s finger on the ~s hand at point ~d , ~d" model finger hand x y))

(defun demo-point (model hand x y)
  (act-r-output "model ~s moved the ~s hand to point ~d , ~d" model hand x y))

(defun demo-swipe (model hand finger distance direction count speed)
  (act-r-output "model ~s swiped ~d fingers on the ~s hand starting with the ~s finger a distance of ~f in the ~f direction at speed ~f" model count hand finger distance direction speed))

(defun demo-tap-hold (model hand finger x y)
  (act-r-output "model ~s tapped and held the ~s finger on the ~s hand at point ~d , ~d" model finger hand x y))

(defun demo-tap-release (model hand finger x y)
  (act-r-output "model ~s released the ~s finger on the ~s hand at point ~d , ~d" model finger hand x y))

(defun demo-pinch (model hand finger start end)
  (act-r-output "model ~s pinched with the ~s finger on the ~s hand from a width of ~f to ~f" model finger hand start end))

(defun demo-rotate (model hand finger angle)
  (act-r-output "model ~s rotated with the ~s finger on the ~s hand  ~f radians" model finger hand angle))

;; Create commands for those functions so they can be used
;; used to monitor the touch device's signals.
                
(add-act-r-command "demo-tap" 'demo-tap "Touch-demo-model example tap monitoring function.")
(add-act-r-command "demo-point" 'demo-point "Touch-demo-model example point monitoring function.")
(add-act-r-command "demo-swipe" 'demo-swipe "Touch-demo-model example swipe monitoring function.")
(add-act-r-command "demo-tap-hold" 'demo-tap-hold "Touch-demo-model example tap-hold monitoring function.")
(add-act-r-command "demo-tap-release" 'demo-tap-release "Touch-demo-model example tap-release monitoring function.")
(add-act-r-command "demo-pinch" 'demo-pinch "Touch-demo-model example pinch monitoring function.")
(add-act-r-command "demo-rotate" 'demo-rotate "Touch-demo-model example rotate monitoring function.")


;; Functions for the interaction with the experiment window.

;; When the button is clicked the next hold and release are going to cause
;; the button to be moved. This is a simple demo since the model is performing
;; a specific action sequence and it's easy to deal with since there is only one 
;; item on the window, but a more general hold/drag/release would require more
;; work to determine the item when held (instead of an initial click being the
;; indication to drag a specific item).

(defun demo-button-clicked ()
  (act-r-output "model clicked the button in the experiment window")
  
  ;; set the item and add additional monitors for the tap-hold and tap-release signals
  
  (setf *tapped-item* *created-button*)
  (monitor-act-r-command "touch-tap-hold" "start-drag-button")
  (monitor-act-r-command "touch-tap-release" "stop-drag-button"))

(defun start-drag-button (model hand finger x y)
  (when *tapped-item*
    (act-r-output "model ~s starting to drag the example button with finger ~s" model finger)))

(defun stop-drag-button (model hand finger x y)
  (when *tapped-item*
    (act-r-output "model ~s stopping the drag of the example button with finger ~s" model finger)
    (modify-button-for-exp-window *tapped-item* :x (- x 100 50) :y (- y 100 25)) ;; item coords are window referenced but fingers are global and reference point is upper left, but model clicked middle
    (setf *tapped-item* nil)
    (remove-act-r-command-monitor "touch-tap-hold" "start-drag-button")
    (remove-act-r-command-monitor "touch-tap-release" "stop-drag-button")))

;; Create commands for those actions.

(add-act-r-command "demo-button-clicked" 'demo-button-clicked "Touch demo model command for detecting a click/tap on a button in an experiment window.")
(add-act-r-command "start-drag-button" 'start-drag-button "Touch demo model command for detecting tap-hold after clicking the experiment window button.")
(add-act-r-command "stop-drag-button" 'stop-drag-button "Touch demo model command for detecting tap-release after clicking the experiment window button.")

  

;;;; ---------------------------------------------------------------------- ;;;;
;;;;   A Demonstration Model
;;;; ---------------------------------------------------------------------- ;;;;
;;;
;;; Just perform a series of actions covering all the ACT-Touch extensions.
;;; Starting with the right hand:
;;;  - move the hand to the stuffed visual-location
;;;  - tap the index finger
;;;  - tap-hold the index finger
;;;  - move the hand to location 450,300
;;;  - tap-release the index finger
;;;  - swipe index and middle finger right 75 pixels at default speed
;;;  - swipe middle, ring, and pinkie fingers up 100 pixels at speed 1
;;;  - swipe index and middle finger left 75 pixels at speed 5
;;;  - pinch with the index and thumb from 60 pixels apart to 30 pixels apart
;;;  - pinch with the ring and thumb from 30 pixels apart to 60 pixels apart
;;;  - rotate with the index and thumb 45 deg (.785 radians) clockwise
;;;  - rotate with the index and thumb 90 deg (1.57 radians) counter-clockwise
;;;  - move the right hand back to where the button was originally

;;;  - move the left hand to the touch device (if one indicated as available)
;;;  - repeat the above actions for the left hand (except the first movement is to 250,200 instead of 450,300)


(define-model act-touch-demo
    
  ;; show all details, don't clear visual-loction automatically, and   
  ;; take longer to move hand when a finger is down on the device
    
  (sgp :v t :trace-detail high :unstuff-visual-location nil :touch-drag-delay t)
  
  ;; chunks used as markers in the task
  
  (define-chunks start tapped tap-holding tap-releasing 
    tapped-dragging-release tapped-dragged-releasing 
    swiped-default swiped-slow swiped pinched anti-pinched
    rotated-45 rotated-90  stop check)
  
  ;; Slots and initial goal chunk
  
  (chunk-type goal op hand device)
  (define-chunks
      (goal op start hand right))

  (goal-focus goal)
  
  ;; Normal constraint includes attended new, but
  ;; don't want that since the same item moves
  ;; without being a new object.
  
  (set-visloc-default screen-x lowest)
  
  
  (p move-hand
     =goal>
       op start
       hand =hand
     =visual-location>
     ?manual>
       state free
     ==>
     +manual>
       cmd move-hand-touch
       hand =hand
       loc =visual-location
     =goal>
       op tap
       loc =visual-location)
  
  (p do-tap
     =goal>
       op tap
       hand =hand
     ?manual>
       state free
     ==>
     +manual>
       cmd tap
       hand =hand
       finger index
     =goal>
       op tapped)
  
  (p do-tap-hold-right
     =goal>
       op tapped
       hand right
     ?manual>
       state free
     ?imaginal>
       state free
       buffer empty
     ==>
     =goal>
       op tap-holding
     +manual>
       cmd tap-hold
       hand right
       finger index
     +imaginal>
       screen-x 450
       screen-y 300
       distance 1080)
  
  (p do-tap-hold-left
     =goal>
       op tapped
       hand left
     ?manual>
       state free
     ?imaginal>
       state free
       buffer empty
     ==>
     =goal>
       op tap-holding
     +manual>
       cmd tap-hold
       hand left
       finger index
     +imaginal>
       screen-x 250
       screen-y 200
       distance 1080)
    
  (p do-tapped-dragging-release
     =goal>
       op tap-holding
       hand =hand
     ?manual>
       state free
     =imaginal>
       screen-x =x
       screen-y =y
       distance =d
     ==>
     =goal>
       op tapped-dragging-release
     +manual>
       cmd move-hand-touch
       loc =imaginal
       hand =hand)
  
  (p do-tapped-dragged-releasing
     =goal>
       op tapped-dragging-release
       hand =hand
     ?manual>
       state free
     ==>
     =goal>
       op tapped-dragged-releasing
     +manual>
       cmd tap-release
       hand =hand
       finger index)
  
  (p do-swipe-default-speed
     =goal>
       op tapped-dragged-releasing
       hand =hand
     ?manual>
       state free
     ==>
     =goal>
       op swiped-default
     +manual>
       cmd swipe
       hand =hand
       finger index
       r 75
       theta 0
       num-fngrs 2)
  
  (p do-swipe-slow-speed
     =goal>
       op swiped-default
       hand =hand
     ?manual>
       state free
     ==>
     =goal>
       op swiped-slow
     +manual>
       cmd swipe
       hand =hand
       finger middle
       r 100
       theta -1.57
       num-fngrs 3
       swipe-speed 1)
  
  
  (p do-swipe-fast-speed
     =goal>
       op swiped-slow
       hand =hand
     ?manual>
       state free
     ==>
     =goal>
       op swiped
     +manual>
       cmd swipe
       hand =hand
       finger index
       r 75
       theta 3.14
       num-fngrs 2
       swipe-speed 5)
  
  (p do-pinch
     =goal>
       op swiped
       hand =hand
     ?manual>
       state free
     ==>
     =goal>
       op pinched
     +manual>
       cmd pinch
       hand =hand
       finger index
       start-width 60
       end-width 30)
  
  (p do-anti-pinch
     =goal>
       op pinched
       hand =hand
     ?manual>
       state free
     ==>
     =goal>
       op anti-pinched
     +manual>
       cmd pinch
       hand =hand
       finger ring
       start-width 30
       end-width 60)
  
  (p do-rotate-45
     =goal>
       op anti-pinched
       hand =hand
     ?manual>
       state free
     ==>
     =goal>
       op rotated-45
     +manual>
       cmd rotate
       hand =hand
       finger index
       rotation .785)
  
  (p do-rotate-minus-90
     =goal>
       op rotated-45
       hand =hand
     ?manual>
       state free
     ==>
     =goal>
       op rotated-90
     +manual>
       cmd rotate
       hand =hand
       finger index
       rotation -1.57)
  
  
  (p move-right-hand-and-switch
     =goal>
       op rotated-90
       hand right
       loc =loc
     ?manual>
       state free
     ==>
     =goal>
       op check
       hand left
     +manual>
       cmd move-hand-touch
       hand right
       loc =loc)
  
  (p move-left-hand-and-stop
     =goal>
       op rotated-90
       hand left
       loc =loc
     ?manual>
       state free
     ==>
     =goal>
       op stop
     +manual>
       cmd move-hand-touch
       hand left
       loc =loc)
  
  (p move-left-to-device
     =goal>
       op check
       device t
     ?manual>
       state free
     ==>
     +manual>
       cmd hand-to-touch
       hand left
     =goal>
       op start
       device nil)
  
  (p skip-moving-left-to-device
     =goal>
       op check
       device nil
     ==>
     =goal>
       op start))
