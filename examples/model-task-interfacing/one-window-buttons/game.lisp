;; Play the simple capture game in 1 window with buttons for interaction.
;; Buttons highlight in red or blue to indicate player turn.
;; Set player goal chunks with color, and also set their 
;; mouse cursor color to match.
;; 
;; When updating the buttons just remove them and add a replacement.

;; global variables for game info

(defvar *current-player*)  ;; name of model 
(defvar *p1*)              ;; player1 name
(defvar *p2*)              ;; player2 name
(defvar *game-over*)       ;; if true stop the game
(defvar *p1-position*)     ;; current player locations, 0-5
(defvar *p2-position*)

(defvar *window*)          ;; the interaction window

(defvar *safety-stop*)     ;; flag that modeler wants run to stop

(defvar *spaces* (make-list 6 :initial-element nil)) ;; record the button objects



(defun stop-a-run ()
  (setf *safety-stop* t))


(defun set-game-over (player) 
  (setf *game-over* player))


(defun game-over (time)
  (or *safety-stop* *game-over* (> time 1000000)))


;; The function to call when a button is pressed.
;; If the current player made the move, update the state.

(defun pick-button (index)
  
  ;; Make sure the right player made the action
  
  (when (eq (current-model) *current-player*)
    
    (if (eq *current-player* *p1*)
        ;; check if it was a valid move -- 1 or 2 spaces forward
        (when (and (> index *p1-position*) (<= (- index *p1-position*) 2))
          
          ;; update the buttons          
          ;; Remove the previous ones
          
          (remove-items-from-exp-window *window* (nth *p1-position* *spaces*) (nth index *spaces*))
          
          ;; old space is now white and blank
          
          (setf (nth *p1-position* *spaces*)
            (add-button-to-exp-window *window* :text "" :x (+ 10 (* *p1-position* 40)) :y 10 
                                      :action (list 'pick-button *p1-position*) :height 35 :width 35 :color 'white))
          
         (if (>= index *p2-position*) 
             (progn
               ;; set the game over after 3 seconds
               (schedule-event-relative 3 'set-game-over :params (list *p1*))
               
               ;; new space is green to indicate a win
               
               (setf (nth index *spaces*)
                 (add-button-to-exp-window *window* :text "1" :x (+ 10 (* index 40)) :y 10 
                                           :action (list 'pick-button index) :height 35 :width 35 :color 'green)))
          
          (progn ;; update the p1 position and make p2 the current player
            
            ;; p1 position is white
            
            (setf (nth index *spaces*)
              (add-button-to-exp-window *window* :text "1" :x (+ 10 (* index 40)) :y 10 
                                        :action (list 'pick-button index) :height 35 :width 35 :color 'white))
            
            ;; set p2 position to be blue
            
            (remove-items-from-exp-window *window* (nth *p2-position* *spaces*))
            (setf (nth *p2-position* *spaces*)
              (add-button-to-exp-window *window* :text "2" :x (+ 10 (* *p2-position* 40)) :y 10 
                                        :action (list 'pick-button *p2-position*) :height 35 :width 35 :color 'blue))
            
            ;; update position and player
            
            (setf *p1-position* index)
            (setf *current-player* *p2*))))
    
      ;; if p2 makes a valid move
      (when (and (< index *p2-position*) (<= (- *p2-position* index) 2))
          
          ;; update the buttons          
          ;; Remove the previous ones
          
          (remove-items-from-exp-window *window* (nth *p2-position* *spaces*) (nth index *spaces*))
          
          ;; old space is now white and blank
          
          (setf (nth *p2-position* *spaces*)
            (add-button-to-exp-window *window* :text "" :x (+ 10 (* *p2-position* 40)) :y 10 
                                      :action (list 'pick-button *p2-position*) :height 35 :width 35 :color 'white))
          
                   
         (if (<= index *p1-position*) 
             (progn
               ;; set the game over after 3 seconds
               (schedule-event-relative 3 'set-game-over :params (list *p2*))
               
               ;; new space is green to indicate a win
               
               (setf (nth index *spaces*)
                 (add-button-to-exp-window *window* :text "2" :x (+ 10 (* index 40)) :y 10 
                                           :action (list 'pick-button index) :height 35 :width 35 :color 'green)))
          
          (progn ;; update the p2 position and make p1 the current player
            
            ;; p2 position is white
            
            (setf (nth index *spaces*)
              (add-button-to-exp-window *window* :text "2" :x (+ 10 (* index 40)) :y 10 
                                        :action (list 'pick-button index) :height 35 :width 35 :color 'white))
            
            ;; set p1 position to be red
            
            (remove-items-from-exp-window *window* (nth *p1-position* *spaces*))
            (setf (nth *p1-position* *spaces*)
              (add-button-to-exp-window *window* :text "1" :x (+ 10 (* *p1-position* 40)) :y 10 
                                        :action (list 'pick-button *p1-position*) :height 35 :width 35 :color 'red))
            
            ;; update position and player
            
            (setf *p2-position* index)
            (setf *current-player* *p1*)))))))
  

;; play one round of the game resetting the models
;; before it starts.
        
(defun play (player1 player2)
  
  (when (every (lambda (x) (find x (mp-models))) (list player1 player2))
    
    (reset)
    
    ;; initialize some game info
    (setf *p1* player1)
    (setf *p2* player2)
    (setf *game-over* nil)
    (setf *safety-stop* nil)
    (setf *current-player* player1)
    (setf *p1-position* 0)
    (setf *p2-position* 5)
    
    (let ((game-window (open-exp-window "game" :visible t :width 300 :height 100))
          (safety-window (open-exp-window "safety" :visible t :width 100 :height 100 :x 100 :y 100))) 
      
      (add-button-to-exp-window safety-window :text "STOP" :x 0 :y 0 :action 'stop-a-run :height 80 :width 80 :color 'red)
      
      (setf *window* game-window)
      
      
      (dolist (m (mp-models))  ;; set all models to use this device
        (with-model-eval m
          (install-device game-window)))
      
      (dotimes (i 6)            ;; create the buttons
        (setf (nth i *spaces*)  ;; save them for later use
          (let ((index i))      ;; need to bind the value of i to use in the action function
            (add-button-to-exp-window game-window :text (if (zerop i) "1" (if (= i 5) "2" ""))
                                      :x (+ 10 (* i 40)) 
                                      :y 10 
                                      :action (list 'pick-button index) :height 35 :width 35 :color (if (zerop i) 'red 'white))))))
    
    
    ;; Run ACT-R until the *game-over* variable is non-nil.
    
    (run-until-condition 'game-over t))
  
  ;; return the result
  *game-over*)
          
#|
CG-USER(104): (play 'red-player 'blue-player)
     0.000  RED-PLAYER   GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  BLUE-PLAYER  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  BLUE-PLAYER  VISION                 PROC-DISPLAY
     0.000  BLUE-PLAYER  VISION                 visicon-update
     0.000  RED-PLAYER   VISION                 PROC-DISPLAY
     0.000  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0 NIL
     0.000  RED-PLAYER   VISION                 visicon-update
     0.000  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.000  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-LEFT
     0.000  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.000  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-LEFT
     0.050  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050  RED-PLAYER   VISION                 Find-location
     0.050  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION3
     0.050  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.050  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MOVE
     0.050  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.100  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MOVE
     0.100  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     0.100  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.100  RED-PLAYER   MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION3-0
     0.100  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.300  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE
     0.300  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.350  RED-PLAYER   MOTOR                  INITIATION-COMPLETE
     0.350  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.708  RED-PLAYER   MOUSE                  move-cursor RED-PLAYER mouse (408 328 1080)
     0.708  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.758  RED-PLAYER   MOTOR                  FINISH-MOVEMENT
     0.758  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.758  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED CLICK
     0.758  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.758  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.808  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED CLICK
     0.808  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.808  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     0.808  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.808  RED-PLAYER   MOTOR                  CLICK-MOUSE
     0.808  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.958  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE
     0.958  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.008  RED-PLAYER   MOTOR                  INITIATION-COMPLETE
     1.008  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.018  RED-PLAYER   MOUSE                  click-mouse RED-PLAYER (408 328 1080) INDEX
     1.018  BLUE-PLAYER  VISION                 PROC-DISPLAY
     1.018  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION11 NIL
     1.018  BLUE-PLAYER  VISION                 visicon-update
     1.018  RED-PLAYER   VISION                 PROC-DISPLAY
     1.018  RED-PLAYER   VISION                 visicon-update
     1.018  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.018  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.018  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-RIGHT
     1.018  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.018  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     1.068  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-RIGHT
     1.068  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.068  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     1.068  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.068  BLUE-PLAYER  VISION                 Find-location
     1.068  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION5
     1.068  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.068  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED MOVE
     1.068  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.068  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     1.068  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.108  RED-PLAYER   MOTOR                  FINISH-MOVEMENT
     1.108  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.118  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED MOVE
     1.118  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.118  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST MANUAL
     1.118  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.118  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.118  BLUE-PLAYER  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION5-0
     1.118  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.318  BLUE-PLAYER  MOTOR                  PREPARATION-COMPLETE
     1.318  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.368  BLUE-PLAYER  MOTOR                  INITIATION-COMPLETE
     1.368  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.750  BLUE-PLAYER  MOUSE                  move-cursor BLUE-PLAYER mouse (488 328 1080)
     1.750  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.800  BLUE-PLAYER  MOTOR                  FINISH-MOVEMENT
     1.800  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.800  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED CLICK
     1.800  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.800  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.850  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED CLICK
     1.850  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.850  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST MANUAL
     1.850  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.850  BLUE-PLAYER  MOTOR                  CLICK-MOUSE
     1.850  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.000  BLUE-PLAYER  MOTOR                  PREPARATION-COMPLETE
     2.000  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.050  BLUE-PLAYER  MOTOR                  INITIATION-COMPLETE
     2.050  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.060  BLUE-PLAYER  MOUSE                  click-mouse BLUE-PLAYER (488 328 1080) INDEX
     2.060  BLUE-PLAYER  VISION                 PROC-DISPLAY
     2.060  BLUE-PLAYER  VISION                 visicon-update
     2.060  RED-PLAYER   VISION                 PROC-DISPLAY
     2.060  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION16 NIL
     2.060  RED-PLAYER   VISION                 visicon-update
     2.060  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.060  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.060  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-LEFT
     2.060  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.060  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.110  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-LEFT
     2.110  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.110  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.110  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.110  RED-PLAYER   VISION                 Find-location
     2.110  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION14
     2.110  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.110  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MOVE
     2.110  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.110  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.110  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.150  BLUE-PLAYER  MOTOR                  FINISH-MOVEMENT
     2.150  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.160  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MOVE
     2.160  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.160  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     2.160  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.160  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.160  RED-PLAYER   MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION14-0
     2.160  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.360  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE
     2.360  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.410  RED-PLAYER   MOTOR                  INITIATION-COMPLETE
     2.410  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.558  RED-PLAYER   MOUSE                  move-cursor RED-PLAYER mouse (488 328 1080)
     2.558  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.608  RED-PLAYER   MOTOR                  FINISH-MOVEMENT
     2.608  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.608  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED CLICK
     2.608  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.608  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.658  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED CLICK
     2.658  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.658  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     2.658  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.658  RED-PLAYER   MOTOR                  CLICK-MOUSE
     2.658  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.808  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE
     2.808  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.858  RED-PLAYER   MOTOR                  INITIATION-COMPLETE
     2.858  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.868  RED-PLAYER   MOUSE                  click-mouse RED-PLAYER (488 328 1080) INDEX
     2.868  BLUE-PLAYER  VISION                 PROC-DISPLAY
     2.868  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION19 NIL
     2.868  BLUE-PLAYER  VISION                 visicon-update
     2.868  RED-PLAYER   VISION                 PROC-DISPLAY
     2.868  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION19 NIL
     2.868  RED-PLAYER   VISION                 visicon-update
     2.868  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.868  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.868  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.868  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.868  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.868  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.868  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.868  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.918  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.918  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.918  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.918  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.918  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.918  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.918  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.918  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.918  RED-PLAYER   VISION                 Find-location
     2.918  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION20
     2.918  BLUE-PLAYER  VISION                 Find-location
     2.918  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION20
     2.918  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.918  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED READ
     2.918  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.918  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.918  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.918  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.918  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED READ
     2.918  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.918  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.918  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.958  RED-PLAYER   MOTOR                  FINISH-MOVEMENT
     2.968  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED READ
     2.968  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.968  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL
     2.968  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.968  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL
     2.968  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED READ
     2.968  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.968  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL
     2.968  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.968  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.968  RED-PLAYER   VISION                 Move-attention VISUAL-LOCATION20-0 NIL
     2.968  BLUE-PLAYER  VISION                 Move-attention VISUAL-LOCATION20-0 NIL
     2.968  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.968  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.053  RED-PLAYER   VISION                 Encoding-complete VISUAL-LOCATION20-0 NIL
     3.053  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.053  BLUE-PLAYER  VISION                 Encoding-complete VISUAL-LOCATION20-0 NIL
     3.053  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.053  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     3.053  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED YEAH
     3.053  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.053  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL
     3.053  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.053  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED BOO
     3.053  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.053  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     3.103  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED YEAH
I WON 
     3.103  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.103  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL
     3.103  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED BOO
I LOST 
     3.103  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.103  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL
     3.103  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     3.103  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     5.868  RED-PLAYER   NONE                   SET-GAME-OVER RED-PLAYER
     5.868  -            ------                 Stopped because condition is true
RED-PLAYER
|#
