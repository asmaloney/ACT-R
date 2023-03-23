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
? (play 'red-player 'blue-player)
     0.000  RED-PLAYER   GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  BLUE-PLAYER  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  BLUE-PLAYER  VISION                 proc-display
     0.000  BLUE-PLAYER  VISION                 visicon-update
     0.000  RED-PLAYER   VISION                 proc-display
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
     0.050  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION2
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
     0.100  RED-PLAYER   MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION2-0
     0.100  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.300  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE 0.1
     0.300  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.350  RED-PLAYER   MOTOR                  INITIATION-COMPLETE 0.1
     0.350  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.694  RED-PLAYER   MOUSE                  move-cursor RED-PLAYER mouse (368 328 1080)
     0.694  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.744  RED-PLAYER   MOTOR                  FINISH-MOVEMENT 0.1
     0.744  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.744  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED CLICK
     0.744  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.744  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.794  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED CLICK
     0.794  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.794  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     0.794  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.794  RED-PLAYER   MOTOR                  CLICK-MOUSE
     0.794  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.944  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE 0.794
     0.944  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.994  RED-PLAYER   MOTOR                  INITIATION-COMPLETE 0.794
     0.994  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.004  RED-PLAYER   MOUSE                  click-mouse RED-PLAYER (368 328 1080) INDEX
     1.004  BLUE-PLAYER  VISION                 proc-display
     1.004  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION11 NIL
     1.004  BLUE-PLAYER  VISION                 visicon-update
     1.004  RED-PLAYER   VISION                 proc-display
     1.004  RED-PLAYER   VISION                 visicon-update
     1.004  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.004  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.004  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-RIGHT
     1.004  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.004  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     1.054  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-RIGHT
     1.054  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.054  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     1.054  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.054  BLUE-PLAYER  VISION                 Find-location
     1.054  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION4
     1.054  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.054  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED MOVE
     1.054  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.054  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     1.054  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.094  RED-PLAYER   MOTOR                  FINISH-MOVEMENT 0.794
     1.094  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.104  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED MOVE
     1.104  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.104  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST MANUAL
     1.104  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.104  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.104  BLUE-PLAYER  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION4-0
     1.104  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.304  BLUE-PLAYER  MOTOR                  PREPARATION-COMPLETE 1.104
     1.304  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.354  BLUE-PLAYER  MOTOR                  INITIATION-COMPLETE 1.104
     1.354  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.724  BLUE-PLAYER  MOUSE                  move-cursor BLUE-PLAYER mouse (448 328 1080)
     1.724  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.774  BLUE-PLAYER  MOTOR                  FINISH-MOVEMENT 1.104
     1.774  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.774  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED CLICK
     1.774  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.774  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.824  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED CLICK
     1.824  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.824  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST MANUAL
     1.824  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.824  BLUE-PLAYER  MOTOR                  CLICK-MOUSE
     1.824  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.974  BLUE-PLAYER  MOTOR                  PREPARATION-COMPLETE 1.824
     1.974  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.024  BLUE-PLAYER  MOTOR                  INITIATION-COMPLETE 1.824
     2.024  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.034  BLUE-PLAYER  MOUSE                  click-mouse BLUE-PLAYER (448 328 1080) INDEX
     2.034  BLUE-PLAYER  VISION                 proc-display
     2.034  BLUE-PLAYER  VISION                 visicon-update
     2.034  RED-PLAYER   VISION                 proc-display
     2.034  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION16 NIL
     2.034  RED-PLAYER   VISION                 visicon-update
     2.034  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.034  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.034  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-LEFT
     2.034  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.034  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.084  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-LEFT
     2.084  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.084  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.084  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.084  RED-PLAYER   VISION                 Find-location
     2.084  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION14
     2.084  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.084  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MOVE
     2.084  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.084  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.084  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.124  BLUE-PLAYER  MOTOR                  FINISH-MOVEMENT 1.824
     2.124  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.134  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MOVE
     2.134  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.134  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     2.134  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.134  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.134  RED-PLAYER   MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION14-0
     2.134  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.334  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE 2.134
     2.334  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.384  RED-PLAYER   MOTOR                  INITIATION-COMPLETE 2.134
     2.384  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.532  RED-PLAYER   MOUSE                  move-cursor RED-PLAYER mouse (448 328 1080)
     2.532  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.582  RED-PLAYER   MOTOR                  FINISH-MOVEMENT 2.134
     2.582  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.582  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED CLICK
     2.582  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.582  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.632  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED CLICK
     2.632  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.632  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     2.632  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.632  RED-PLAYER   MOTOR                  CLICK-MOUSE
     2.632  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.782  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE 2.632
     2.782  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.832  RED-PLAYER   MOTOR                  INITIATION-COMPLETE 2.632
     2.832  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.842  RED-PLAYER   MOUSE                  click-mouse RED-PLAYER (448 328 1080) INDEX
     2.842  BLUE-PLAYER  VISION                 proc-display
     2.842  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION19 NIL
     2.842  BLUE-PLAYER  VISION                 visicon-update
     2.842  RED-PLAYER   VISION                 proc-display
     2.842  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION19 NIL
     2.842  RED-PLAYER   VISION                 visicon-update
     2.842  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.842  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.842  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.842  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.842  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.842  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.842  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.842  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.892  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.892  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.892  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.892  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.892  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.892  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.892  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.892  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.892  RED-PLAYER   VISION                 Find-location
     2.892  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION20
     2.892  BLUE-PLAYER  VISION                 Find-location
     2.892  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION20
     2.892  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.892  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED READ
     2.892  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.892  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.892  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.892  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.892  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED READ
     2.892  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.892  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.892  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.932  RED-PLAYER   MOTOR                  FINISH-MOVEMENT 2.632
     2.942  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED READ
     2.942  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.942  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL
     2.942  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.942  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL
     2.942  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED READ
     2.942  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.942  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL
     2.942  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.942  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.942  RED-PLAYER   VISION                 Move-attention VISUAL-LOCATION20-0 NIL
     2.942  BLUE-PLAYER  VISION                 Move-attention VISUAL-LOCATION20-0 NIL
     2.942  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.942  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.027  RED-PLAYER   VISION                 Encoding-complete VISUAL-LOCATION20-0 NIL
     3.027  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.027  BLUE-PLAYER  VISION                 Encoding-complete VISUAL-LOCATION20-0 NIL
     3.027  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.027  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     3.027  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED YEAH
     3.027  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.027  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL
     3.027  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.027  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED BOO
     3.027  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.027  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     3.077  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED YEAH
I WON 
     3.077  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.077  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL
     3.077  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED BOO
I LOST 
     3.077  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.077  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL
     3.077  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     3.077  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     5.842  RED-PLAYER   NONE                   SET-GAME-OVER RED-PLAYER
     5.842  -            ------                 Stopped because condition is true
RED-PLAYER
|#
