;; This file loads the demo2 model task and model from the tutorial.
;; The demo2-emma function will enable EMMA and run the task.  The
;; comments at the end show the results of running the original task
;; without enabling EMMA and then with EMMA.

(clear-all)

(require-extra "emma")

(load-act-r-code "ACT-R:tutorial;lisp;demo2.lisp")

(defun demo2-emma ()
  (let ((original (first (ssp :starting-parameters))))
    (ssp-fct (list :starting-parameters (append '(:emma t) original)))
    (demo2-experiment)
    (ssp-fct (list :starting-parameters original))))

#|
CG-USER(12): (load "ACT-R:extras;emma;example-demo2.lisp")
T
CG-USER(13): (demo2-experiment)
#|Warning: No handler available for displaying a visible window.  Using a virtual window instead. |#
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000   VISION                 PROC-DISPLAY
     0.000   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0 NIL
     0.000   VISION                 visicon-update
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED FIND-UNATTENDED-LETTER
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050   PROCEDURAL             PRODUCTION-FIRED FIND-UNATTENDED-LETTER
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050   VISION                 Find-location
     0.050   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-SELECTED ATTEND-LETTER
     0.050   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050   PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     0.100   PROCEDURAL             PRODUCTION-FIRED ATTEND-LETTER
     0.100   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100   PROCEDURAL             MODULE-REQUEST VISUAL
     0.100   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.100   VISION                 Move-attention VISUAL-LOCATION0-1 NIL
     0.100   PROCEDURAL             CONFLICT-RESOLUTION
     0.185   VISION                 Encoding-complete VISUAL-LOCATION0-1 NIL
     0.185   VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     0.185   PROCEDURAL             CONFLICT-RESOLUTION
     0.185   PROCEDURAL             PRODUCTION-SELECTED ENCODE-LETTER
     0.185   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.185   PROCEDURAL             BUFFER-READ-ACTION VISUAL
     0.185   PROCEDURAL             QUERY-BUFFER-ACTION IMAGINAL
     0.235   PROCEDURAL             PRODUCTION-FIRED ENCODE-LETTER
     0.235   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.235   PROCEDURAL             MODULE-REQUEST IMAGINAL
     0.235   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.235   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.235   PROCEDURAL             CONFLICT-RESOLUTION
     0.435   IMAGINAL               CREATE-NEW-BUFFER-CHUNK IMAGINAL
     0.435   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK0
     0.435   PROCEDURAL             CONFLICT-RESOLUTION
     0.435   PROCEDURAL             PRODUCTION-SELECTED RESPOND
     0.435   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.435   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
     0.435   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.485   PROCEDURAL             PRODUCTION-FIRED RESPOND
     0.485   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.485   PROCEDURAL             MODULE-REQUEST MANUAL
     0.485   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.485   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.485   MOTOR                  PRESS-KEY KEY V
     0.485   PROCEDURAL             CONFLICT-RESOLUTION
     0.735   MOTOR                  PREPARATION-COMPLETE
     0.735   PROCEDURAL             CONFLICT-RESOLUTION
     0.785   MOTOR                  INITIATION-COMPLETE
     0.785   PROCEDURAL             CONFLICT-RESOLUTION
     0.885   KEYBOARD               output-key DEMO2 v
     0.885   VISION                 PROC-DISPLAY
     0.885   VISION                 visicon-update
     0.885   PROCEDURAL             CONFLICT-RESOLUTION
     0.970   VISION                 Encoding-complete VISUAL-LOCATION0-1 NIL
     0.970   VISION                 No visual-object found
     0.970   PROCEDURAL             CONFLICT-RESOLUTION
     1.035   MOTOR                  FINISH-MOVEMENT
     1.035   PROCEDURAL             CONFLICT-RESOLUTION
     1.035   ------                 Stopped because no events left to process
"v"
CG-USER(14): (demo2-emma)
#|Warning: No handler available for displaying a visible window.  Using a virtual window instead. |#
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000   VISION                 PROC-DISPLAY
     0.000   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0 NIL
     0.000   VISION                 visicon-update
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED FIND-UNATTENDED-LETTER
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050   PROCEDURAL             PRODUCTION-FIRED FIND-UNATTENDED-LETTER
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050   VISION                 Find-location
     0.050   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-SELECTED ATTEND-LETTER
     0.050   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050   PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     0.100   PROCEDURAL             PRODUCTION-FIRED ATTEND-LETTER
     0.100   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100   PROCEDURAL             MODULE-REQUEST VISUAL
     0.100   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.100   VISION                 Move-attention VISUAL-LOCATION0-1 NIL
     0.100   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   EMMA                   Preparation-complete TEXT0
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   EMMA                   INITIATION-COMPLETE
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.379   EMMA                   Complete-eye-movement TEXT0 #(320 522 1080)
     0.379   PROCEDURAL             CONFLICT-RESOLUTION
     0.479   EMMA                   Preparation-complete TEXT1
     0.479   PROCEDURAL             CONFLICT-RESOLUTION
     0.529   EMMA                   INITIATION-COMPLETE
     0.529   PROCEDURAL             CONFLICT-RESOLUTION
     0.553   VISION                 Encoding-complete VISUAL-LOCATION0-1 NIL
     0.553   VISION                 SET-BUFFER-CHUNK VISUAL TEXT2
     0.553   PROCEDURAL             CONFLICT-RESOLUTION
     0.553   PROCEDURAL             PRODUCTION-SELECTED ENCODE-LETTER
     0.553   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.553   PROCEDURAL             BUFFER-READ-ACTION VISUAL
     0.553   PROCEDURAL             QUERY-BUFFER-ACTION IMAGINAL
     0.562   EMMA                   Complete-eye-movement TEXT1 #(410 443 1080)
     0.603   PROCEDURAL             PRODUCTION-FIRED ENCODE-LETTER
     0.603   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.603   PROCEDURAL             MODULE-REQUEST IMAGINAL
     0.603   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.603   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.603   PROCEDURAL             CONFLICT-RESOLUTION
     0.803   IMAGINAL               CREATE-NEW-BUFFER-CHUNK IMAGINAL
     0.803   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK0
     0.803   PROCEDURAL             CONFLICT-RESOLUTION
     0.803   PROCEDURAL             PRODUCTION-SELECTED RESPOND
     0.803   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.803   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
     0.803   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.853   PROCEDURAL             PRODUCTION-FIRED RESPOND
     0.853   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.853   PROCEDURAL             MODULE-REQUEST MANUAL
     0.853   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.853   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.853   MOTOR                  PRESS-KEY KEY V
     0.853   PROCEDURAL             CONFLICT-RESOLUTION
     1.103   MOTOR                  PREPARATION-COMPLETE
     1.103   PROCEDURAL             CONFLICT-RESOLUTION
     1.153   MOTOR                  INITIATION-COMPLETE
     1.153   PROCEDURAL             CONFLICT-RESOLUTION
     1.253   KEYBOARD               output-key DEMO2 v
     1.253   VISION                 PROC-DISPLAY
     1.253   VISION                 visicon-update
     1.253   PROCEDURAL             CONFLICT-RESOLUTION
     1.276   VISION                 Encoding-complete VISUAL-LOCATION0-1 NIL
     1.276   VISION                 No visual-object found
     1.276   PROCEDURAL             CONFLICT-RESOLUTION
     1.353   EMMA                   Preparation-complete NIL
     1.353   PROCEDURAL             CONFLICT-RESOLUTION
     1.403   MOTOR                  FINISH-MOVEMENT
     1.403   EMMA                   INITIATION-COMPLETE
     1.403   PROCEDURAL             CONFLICT-RESOLUTION
     1.426   EMMA                   Complete-eye-movement NIL #(431 454 1080)
     1.426   PROCEDURAL             CONFLICT-RESOLUTION
     1.426   ------                 Stopped because no events left to process

(NIL)


|#