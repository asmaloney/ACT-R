;;; This is the Lisp code to run the task that goes along with the 
;;; background-model. 
;;;

(load-act-r-model "ACT-R:examples;creating-image-items;background-model.lisp")

;;; It is a demonstration of creating and using an
;;; image AGI item along with custom visicon features.
;;; It assumes that the ref-brain.gif file is in the 
;;; gui/AGI-images directory if you use a visible window 
;;; to see the images.
;;;
;;; To run the task call the run-test function.  It has one
;;; optional parameter which if provided as any non-nil value 
;;; indicates the window should be visible.  The default is
;;; to use a virtual window.  

(defun run-test (&optional (visible nil))
  (reset)
  
  (let ((win (open-exp-window "background test" :visible visible :height 390 :width 390 :x 100 :y 100)))
    
    (install-device win)
    
    (start-hand-at-mouse)
    
    (add-image-to-exp-window win "background" "ref-brain.gif" :x 0 :y 0 :height 390 :width 390)
    
    (dotimes (x 3)
      (dotimes (y 3)
        (add-visicon-features (list 'screen-x (+ 164 (* x 130)) 
                                    'screen-y (+ 164 (* y 130)) 
                                    'height 128
                                    'width 128
                                    'name (list "brain" (format nil "brain-~d" (+ x (* y 3))))))))
    
    ;; run until the vision module processes the new items in the display
    (run-n-events 2) 
    
    ;; print the visicon to see how the new items are represented
    (print-visicon)
    
    ;; run for up to 10 seconds using real-time if the window is visible
    (run 10 visible)))


#|
CG-USER(12): (run-test)
     0.000   ------                 Stopped because event limit reached
Name              Att  Loc             Width  Height  Size   Name       Image  Kind   Value       
----------------  ---  --------------  -----  ------  -----  ---------  -----  -----  ------------
CHUNK0            NEW  (164 164 1080)  128    128     46.0   "brain-0"                            
CHUNK1            NEW  (164 294 1080)  128    128     46.0   "brain-3"                            
CHUNK2            NEW  (164 424 1080)  128    128     46.0   "brain-6"                            
CHUNK3            NEW  (294 164 1080)  128    128     46.0   "brain-1"                            
CHUNK4            NEW  (294 294 1080)  128    128     46.0   "brain-4"                            
CHUNK5            NEW  (294 424 1080)  128    128     46.0   "brain-7"                            
VISUAL-LOCATION0  NEW  (295 295 1080)  390    390     419.0             T      IMAGE  "background"
CHUNK6            NEW  (424 164 1080)  128    128     46.0   "brain-2"                            
CHUNK7            NEW  (424 294 1080)  128    128     46.0   "brain-5"                            
CHUNK8            NEW  (424 424 1080)  128    128     46.0   "brain-8"                            

     0.000   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
     0.000   VISION                 visicon-update
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-FIRED FIND-START
     0.050   PROCEDURAL             CLEAR-BUFFER GOAL
     0.050   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050   VISION                 Find-location
     0.050   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0
     0.050   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK9
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.100   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK0-1
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  164
   SCREEN-Y  164
   DISTANCE  1080
   SIZE  46.0

     0.100   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.100   PROCEDURAL             CONFLICT-RESOLUTION
     0.185   VISION                 Encoding-complete CHUNK0-1 NIL
     0.185   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK10
     0.185   PROCEDURAL             CONFLICT-RESOLUTION
     0.235   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK10-0
   NAME  "brain-0"
   SCREEN-POS  CHUNK0-0
   HEIGHT  128
   WIDTH  128

     0.235   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.235   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.235   MOTOR                  MOVE-CURSOR LOC CHUNK0-0
     0.235   PROCEDURAL             CONFLICT-RESOLUTION
     0.435   PROCEDURAL             CONFLICT-RESOLUTION
     0.485   PROCEDURAL             CONFLICT-RESOLUTION
     0.585   PROCEDURAL             CONFLICT-RESOLUTION
     0.635   PROCEDURAL             CONFLICT-RESOLUTION
     0.685   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     0.685   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.685   VISION                 Find-location
     0.685   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK3
     0.685   PROCEDURAL             CONFLICT-RESOLUTION
     0.735   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK3-0
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  294
   SCREEN-Y  164
   DISTANCE  1080
   SIZE  46.0

     0.735   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.735   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.735   PROCEDURAL             CONFLICT-RESOLUTION
     0.820   VISION                 Encoding-complete CHUNK3-0 NIL
     0.820   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK11
     0.820   PROCEDURAL             CONFLICT-RESOLUTION
     0.870   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK11-0
   NAME  "brain-1"
   SCREEN-POS  CHUNK3-0
   HEIGHT  128
   WIDTH  128

     0.870   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.870   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.870   MOTOR                  MOVE-CURSOR LOC CHUNK3-0
     0.870   PROCEDURAL             CONFLICT-RESOLUTION
     0.970   PROCEDURAL             CONFLICT-RESOLUTION
     1.020   PROCEDURAL             CONFLICT-RESOLUTION
     1.120   PROCEDURAL             CONFLICT-RESOLUTION
     1.170   PROCEDURAL             CONFLICT-RESOLUTION
     1.220   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     1.220   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.220   VISION                 Find-location
     1.220   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK6
     1.220   PROCEDURAL             CONFLICT-RESOLUTION
     1.270   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK6-0
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  424
   SCREEN-Y  164
   DISTANCE  1080
   SIZE  46.0

     1.270   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.270   PROCEDURAL             CLEAR-BUFFER VISUAL
     1.270   PROCEDURAL             CONFLICT-RESOLUTION
     1.355   VISION                 Encoding-complete CHUNK6-0 NIL
     1.355   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK12
     1.355   PROCEDURAL             CONFLICT-RESOLUTION
     1.405   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK12-0
   NAME  "brain-2"
   SCREEN-POS  CHUNK6-0
   HEIGHT  128
   WIDTH  128

     1.405   PROCEDURAL             CLEAR-BUFFER VISUAL
     1.405   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.405   MOTOR                  MOVE-CURSOR LOC CHUNK6-0
     1.405   PROCEDURAL             CONFLICT-RESOLUTION
     1.455   PROCEDURAL             CONFLICT-RESOLUTION
     1.555   PROCEDURAL             CONFLICT-RESOLUTION
     1.605   PROCEDURAL             CONFLICT-RESOLUTION
     1.655   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     1.655   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.655   VISION                 Find-location
     1.655   VISION                 FIND-LOC-FAILURE
     1.655   PROCEDURAL             CONFLICT-RESOLUTION
     1.705   PROCEDURAL             PRODUCTION-FIRED NEXT-ROW
     1.705   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.705   VISION                 Find-location
     1.705   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK1
     1.705   PROCEDURAL             CONFLICT-RESOLUTION
     1.755   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK1-0
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  164
   SCREEN-Y  294
   DISTANCE  1080
   SIZE  46.0

     1.755   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.755   PROCEDURAL             CLEAR-BUFFER VISUAL
     1.755   PROCEDURAL             CONFLICT-RESOLUTION
     1.840   VISION                 Encoding-complete CHUNK1-0 NIL
     1.840   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK13
     1.840   PROCEDURAL             CONFLICT-RESOLUTION
     1.890   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK13-0
   NAME  "brain-3"
   SCREEN-POS  CHUNK1-0
   HEIGHT  128
   WIDTH  128

     1.890   PROCEDURAL             CLEAR-BUFFER VISUAL
     1.890   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.890   MOTOR                  MOVE-CURSOR LOC CHUNK1-0
     1.890   PROCEDURAL             CONFLICT-RESOLUTION
     1.990   PROCEDURAL             CONFLICT-RESOLUTION
     2.040   PROCEDURAL             CONFLICT-RESOLUTION
     2.173   PROCEDURAL             CONFLICT-RESOLUTION
     2.223   PROCEDURAL             CONFLICT-RESOLUTION
     2.273   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     2.273   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.273   VISION                 Find-location
     2.273   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK4
     2.273   PROCEDURAL             CONFLICT-RESOLUTION
     2.323   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK4-0
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  294
   SCREEN-Y  294
   DISTANCE  1080
   SIZE  46.0

     2.323   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.323   PROCEDURAL             CLEAR-BUFFER VISUAL
     2.323   PROCEDURAL             CONFLICT-RESOLUTION
     2.408   VISION                 Encoding-complete CHUNK4-0 NIL
     2.408   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK14
     2.408   PROCEDURAL             CONFLICT-RESOLUTION
     2.458   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK14-0
   NAME  "brain-4"
   SCREEN-POS  CHUNK4-0
   HEIGHT  128
   WIDTH  128

     2.458   PROCEDURAL             CLEAR-BUFFER VISUAL
     2.458   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.458   MOTOR                  MOVE-CURSOR LOC CHUNK4-0
     2.458   PROCEDURAL             CONFLICT-RESOLUTION
     2.558   PROCEDURAL             CONFLICT-RESOLUTION
     2.608   PROCEDURAL             CONFLICT-RESOLUTION
     2.708   PROCEDURAL             CONFLICT-RESOLUTION
     2.758   PROCEDURAL             CONFLICT-RESOLUTION
     2.808   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     2.808   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.808   VISION                 Find-location
     2.808   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK7
     2.808   PROCEDURAL             CONFLICT-RESOLUTION
     2.858   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK7-0
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  424
   SCREEN-Y  294
   DISTANCE  1080
   SIZE  46.0

     2.858   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.858   PROCEDURAL             CLEAR-BUFFER VISUAL
     2.858   PROCEDURAL             CONFLICT-RESOLUTION
     2.943   VISION                 Encoding-complete CHUNK7-0 NIL
     2.943   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK15
     2.943   PROCEDURAL             CONFLICT-RESOLUTION
     2.993   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK15-0
   NAME  "brain-5"
   SCREEN-POS  CHUNK7-0
   HEIGHT  128
   WIDTH  128

     2.993   PROCEDURAL             CLEAR-BUFFER VISUAL
     2.993   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.993   MOTOR                  MOVE-CURSOR LOC CHUNK7-0
     2.993   PROCEDURAL             CONFLICT-RESOLUTION
     3.043   PROCEDURAL             CONFLICT-RESOLUTION
     3.143   PROCEDURAL             CONFLICT-RESOLUTION
     3.193   PROCEDURAL             CONFLICT-RESOLUTION
     3.243   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     3.243   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.243   VISION                 Find-location
     3.243   VISION                 FIND-LOC-FAILURE
     3.243   PROCEDURAL             CONFLICT-RESOLUTION
     3.293   PROCEDURAL             PRODUCTION-FIRED NEXT-ROW
     3.293   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.293   VISION                 Find-location
     3.293   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK2
     3.293   PROCEDURAL             CONFLICT-RESOLUTION
     3.343   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK2-0
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  164
   SCREEN-Y  424
   DISTANCE  1080
   SIZE  46.0

     3.343   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.343   PROCEDURAL             CLEAR-BUFFER VISUAL
     3.343   PROCEDURAL             CONFLICT-RESOLUTION
     3.428   VISION                 Encoding-complete CHUNK2-0 NIL
     3.428   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK16
     3.428   PROCEDURAL             CONFLICT-RESOLUTION
     3.478   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK16-0
   NAME  "brain-6"
   SCREEN-POS  CHUNK2-0
   HEIGHT  128
   WIDTH  128

     3.478   PROCEDURAL             CLEAR-BUFFER VISUAL
     3.478   PROCEDURAL             CLEAR-BUFFER MANUAL
     3.478   MOTOR                  MOVE-CURSOR LOC CHUNK2-0
     3.478   PROCEDURAL             CONFLICT-RESOLUTION
     3.578   PROCEDURAL             CONFLICT-RESOLUTION
     3.628   PROCEDURAL             CONFLICT-RESOLUTION
     3.761   PROCEDURAL             CONFLICT-RESOLUTION
     3.811   PROCEDURAL             CONFLICT-RESOLUTION
     3.861   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     3.861   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.861   VISION                 Find-location
     3.861   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK5
     3.861   PROCEDURAL             CONFLICT-RESOLUTION
     3.911   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK5-0
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  294
   SCREEN-Y  424
   DISTANCE  1080
   SIZE  46.0

     3.911   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.911   PROCEDURAL             CLEAR-BUFFER VISUAL
     3.911   PROCEDURAL             CONFLICT-RESOLUTION
     3.996   VISION                 Encoding-complete CHUNK5-0 NIL
     3.996   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK17
     3.996   PROCEDURAL             CONFLICT-RESOLUTION
     4.046   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK17-0
   NAME  "brain-7"
   SCREEN-POS  CHUNK5-0
   HEIGHT  128
   WIDTH  128

     4.046   PROCEDURAL             CLEAR-BUFFER VISUAL
     4.046   PROCEDURAL             CLEAR-BUFFER MANUAL
     4.046   MOTOR                  MOVE-CURSOR LOC CHUNK5-0
     4.046   PROCEDURAL             CONFLICT-RESOLUTION
     4.146   PROCEDURAL             CONFLICT-RESOLUTION
     4.196   PROCEDURAL             CONFLICT-RESOLUTION
     4.296   PROCEDURAL             CONFLICT-RESOLUTION
     4.346   PROCEDURAL             CONFLICT-RESOLUTION
     4.396   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     4.396   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     4.396   VISION                 Find-location
     4.396   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK8
     4.396   PROCEDURAL             CONFLICT-RESOLUTION
     4.446   PROCEDURAL             PRODUCTION-FIRED ATTEND
CHUNK8-0
   NAME  "brain"
   HEIGHT  128
   WIDTH  128
   SCREEN-X  424
   SCREEN-Y  424
   DISTANCE  1080
   SIZE  46.0

     4.446   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     4.446   PROCEDURAL             CLEAR-BUFFER VISUAL
     4.446   PROCEDURAL             CONFLICT-RESOLUTION
     4.531   VISION                 Encoding-complete CHUNK8-0 NIL
     4.531   VISION                 SET-BUFFER-CHUNK VISUAL CHUNK18
     4.531   PROCEDURAL             CONFLICT-RESOLUTION
     4.581   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-MOVE
CHUNK18-0
   NAME  "brain-8"
   SCREEN-POS  CHUNK8-0
   HEIGHT  128
   WIDTH  128

     4.581   PROCEDURAL             CLEAR-BUFFER VISUAL
     4.581   PROCEDURAL             CLEAR-BUFFER MANUAL
     4.581   MOTOR                  MOVE-CURSOR LOC CHUNK8-0
     4.581   PROCEDURAL             CONFLICT-RESOLUTION
     4.631   PROCEDURAL             CONFLICT-RESOLUTION
     4.731   PROCEDURAL             CONFLICT-RESOLUTION
     4.781   PROCEDURAL             CONFLICT-RESOLUTION
     4.831   PROCEDURAL             PRODUCTION-FIRED FIND-NEXT
     4.831   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     4.831   VISION                 Find-location
     4.831   VISION                 FIND-LOC-FAILURE
     4.831   PROCEDURAL             CONFLICT-RESOLUTION
     4.881   PROCEDURAL             PRODUCTION-FIRED NEXT-ROW
     4.881   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     4.881   VISION                 Find-location
     4.881   VISION                 FIND-LOC-FAILURE
     4.881   PROCEDURAL             CONFLICT-RESOLUTION
     4.931   PROCEDURAL             PRODUCTION-FIRED NO-MORE
     4.931   PROCEDURAL             CLEAR-BUFFER GOAL
     4.931   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     4.931   PROCEDURAL             CONFLICT-RESOLUTION
     4.931   ------                 Stopped because no events left to process
|#
