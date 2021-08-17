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
;;; Filename    : act-touch-demo-model.lisp
;;; Revision    : 1
;;; 
;;; Description : A quickie demonstration of ACT-Touch
;;;
;;; Usage	: Load this file and call run-touch-demo to run the demonstration
;;;             : model.  Does not require loading act-touch separately.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ---------------------------------------------------------------------- ;;;;
;;;;   A Demonstration Model
;;;; ---------------------------------------------------------------------- ;;;;

(clear-all)
(require-extra "act-touch")

(defun run-touch-demo ()
  (reset)
  (let* ((wind (make-instance 'multitouch-display))
         (vis-locs
          (define-chunks-fct
              ;; Remember, the display is 1024 x 768 px
              '((screen-x 500 screen-y 350 kind rectangle height 200 width 200 color red))))
         (vis-objs
          (define-chunks-fct
              `((height ,(chunk-slot-value-fct (car vis-locs) 'height)
                        width  ,(chunk-slot-value-fct (car vis-locs) 'width)
                        color ,(chunk-slot-value-fct (car vis-locs) 'color)
                        screen-pos ,(car vis-locs)))))

         (wgts (list (make-instance 'test-widget :vwindow wind :vis-loc (car vis-locs) 
                       :vis-obj (car vis-objs) :nick-name :TAP))))
    
    (setf (visual-world wind) (pairlis vis-locs vis-objs)
      (widgets wind) wgts)
    
    (start-hand-at-touch) ;; default right hand 1024,300
    (start-hand-at-touch 'left 0 300)
    
    (install-device wind)
    (proc-display)
    (run 20)))


(define-model act-touch-demo

  (sgp :v t :trace-detail high)
  
  (chunk-type goal op)
  
  (define-chunks (rectangle) (start) (tapped) (tap-holding) (tap-releasing)
    (tap-drag-release) (tapping-drag-release) (tapped-dragging-release)
    (tapped-dragged-releasing) (swiped) (pinched) (rotated)(swiped-default) 
    (swiped-slow)(rotated-45) (rotated-90) (rotated-180))
  
  (add-dm
   (goal op start))

  (goal-focus goal)
  
  
(p move-hand
   =goal>
        op start
   =visual-location>
	color red
   ?manual>
	state free
==>
   +manual>
	cmd move-hand-touch
        loc =visual-location
   =goal>
	op tap)


(p do-tap
   =goal>
        op tap
   ?manual>
	state free
==>
  +manual>
	cmd tap
        hand right
        finger index
  =goal>
	op tapped)

(p do-tap-hold
   =goal>
	op tapped
   ?manual>
	state free
==>
   =goal>
	op tap-holding
   +manual>
	cmd tap-hold
        hand right
        finger index)

(p do-tap-release

   =goal>
	op tap-holding
   ?manual>
	state free
==>
   =goal>
   	op tap-releasing
   +manual>
	cmd tap-release
        hand right
        finger index)

;; Here's an example of a composed movement: the tap-hold, drag, & release
(p do-tapping-drag-release
   =goal>
	op tap-releasing
   ?manual>
	state free
   ?imaginal>
	buffer empty
==>
   =goal>
   	op tapping-drag-release
   +manual>
	cmd tap-hold
        hand right
        finger index
   +imaginal>
	screen-x 550
        screen-y 400)

(p do-tapped-dragging-release

   =goal>
	op tapping-drag-release
   ?manual>
	state free
   =imaginal>
	screen-x =x
        screen-y =y
==>
   =goal>
	op tapped-dragging-release
   +manual>
	cmd move-hand-touch
        loc =imaginal)

(p do-tapped-dragged-releasing

   =goal>
   	op tapped-dragging-release
   ?manual>
	state free
==>
   =goal>
	op tapped-dragged-releasing
   +manual>
	cmd tap-release
        hand right
        finger index)

(p do-swipe-default-speed
   =goal>
   	op tapped-dragged-releasing
   ?manual>
	state free
==>
   =goal>
	op swiped-default
   +manual>
	cmd swipe
        hand right
        finger index
        r 50
        theta 0
        num-fngrs 2)

(p do-swipe-slow-speed
   =goal>
   	op swiped-default
   ?manual>
	state free
==>
   =goal>
	op swiped-slow
   +manual>
	cmd swipe
        hand right
        finger index
        r 50
        theta 0
   num-fngrs 2
   swipe-speed 1)


(p do-swipe-fast-speed
   =goal>
   	op swiped-slow
   ?manual>
	state free
==>
   =goal>
   op swiped
   +manual>
	cmd swipe
        hand right
        finger index
        r 50
        theta 0
   num-fngrs 2
   swipe-speed 5)

(p do-pinch
   =goal>
	op swiped
   ?manual>
	state free
==>
   =goal>
	op pinched
   +manual>
	cmd pinch
        hand right
        finger index
        start-width 20
        end-width 50)

(p do-rotate-45
   =goal>
	op pinched
   ?manual>
	state free
==>
   =goal>
	op rotated-45
   +manual>
	cmd rotate
        hand right
        finger index
        rotation .785) ; it's in radians

(p do-rotate-90
   =goal>
	op rotated-45
   ?manual>
	state free
==>
   =goal>
	op rotated-90
   +manual>
	cmd rotate
        hand right
        finger index
        rotation 1.57)


(p do-rotate-180
   =goal>
	op rotated-90
   ?manual>
	state free
==>
   =goal>
	op rotated-180
   +manual>
	cmd rotate
        hand right
        finger index
        rotation 3.14)

)

