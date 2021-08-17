;; Play the simple capture game with 2 models using no interface, but
;; creating custom visicon features for the model to see.

;; Each model will have two features which it can see.  There is one
;; for each player and it contains these features:
#|

While game playing

Name         Att  Loc           Kind    Position  Name    Size  Turn
-----------  ---  ------------  ------  --------  ------  ----  ----
PLAYER-LOC0  NEW  (  0 0 1080)  PLAYER  0         MODEL1  1.0   T   
PLAYER-LOC1  NEW  (100 0 1080)  PLAYER  5         MODEL2  1.0       



when game over

Name         Att  Loc           Kind    Position  Name    Size  Result
-----------  ---  ------------  ------  --------  ------  ----  ------
PLAYER-LOC0  T    (  0 0 1080)  PLAYER  4         MODEL1  1.0   WIN   
PLAYER-LOC1  NIL  (100 0 1080)  PLAYER  4         MODEL2  1.0   LOSE  

|#

;; The position value is only available in the visual object chunk.



;; global variables for game info

(defvar *current-player*) ; name of the model
(defvar *p1*)             ; player1 model name
(defvar *p2*)             ; player2 model name
(defvar *game-over*)      ; If true stop the game
(defvar *winner*)         ; which player won

(defvar *safety-stop*)    ; flag that modeler wants run to stop

(defvar *p1-position*)    ; current player spots 0-5
(defvar *p2-position*)

(defvar *p1p1*)    ; Record the visual features for each player
(defvar *p1p2*)    ; so they can be updated instead of just
(defvar *p2p1*)    ; clearing and adding new ones
(defvar *p2p2*)

;; function called for a keypress action which makes the
;; move and updates the visual info if it's a valid move
;; by the current player

(defun make-move (model key)
  (when (and (eq *current-player* model)
             (or (string-equal key "1") (string-equal key "2")))
    
    (let ((dist (read-from-string key)))
      (if (eq model *p1*) ; update the position
          (incf *p1-position* dist)
        (decf *p2-position* dist)))
    
    (cond ((<= *p2-position* *p1-position*) ; the game is over
           
           (setf *winner* *current-player*)
           (schedule-event-relative 3 'set-game-over)
         
           ;; update player 1 visual features
                      
           (with-model-eval *p1*
             (modify-visicon-features (list *p1p1* 'position *p1-position* 'turn nil 'result (if (eq *p1* *winner*) 'win 'lose)))
             (modify-visicon-features (list *p1p2* 'position *p2-position* 'turn nil 'result (if (eq *p2* *winner*) 'win 'lose))))
           
           ;; update player 2 visual features
           
           (with-model-eval *p2*
             (modify-visicon-features (list *p2p1* 'position *p1-position* 'turn nil 'result (if (eq *p1* *winner*) 'win 'lose)))
             (modify-visicon-features (list *p2p2* 'position *p2-position* 'turn nil 'result (if (eq *p2* *winner*) 'win 'lose)))))
          
          (t ;; update the game and all visual features
           
           (if (eq *current-player* *p1*)
               (setf *current-player* *p2*)
             (setf *current-player* *p1*))
           
           (with-model-eval *p1*
             (modify-visicon-features (list *p1p1* 'position *p1-position* 'turn (if (eq *current-player* *p1*) t nil)))
             (modify-visicon-features (list *p1p2* 'position *p2-position* 'turn (if (eq *current-player* *p2*) t nil))))
           
           (with-model-eval *p2*
             (modify-visicon-features (list *p2p1* 'position *p1-position* 'turn (if (eq *current-player* *p1*) t nil)))
             (modify-visicon-features (list *p2p2* 'position *p2-position* 'turn (if (eq *current-player* *p2*) t nil))))))))
  

(defun set-game-over () (setf *game-over* t))

(defun stop-a-run ()
  (setf *safety-stop* t))

(defun game-over (time)
  (or *safety-stop* *game-over* (> time 1000000)))

;; play one round of the game resetting the models
;; before it starts.  Player1 and player2 are the
;; names of the models to run in the indicated 
;; position.

(defun play (player1 player2)
  
  (when (every (lambda (x) (find x (mp-models))) (list player1 player2))
    (open-exp-window "Safety" t :width 100 :height 100 :x 100 :y 300)
    (add-button-to-exp-window "Safety" :text "STOP" :x 0 :y 0 :action 'stop-a-run :height 90 :width 90 :color 'red)
    
    (add-act-r-command "move" 'make-move "Handle player key presses")
    (monitor-act-r-command "output-key" "move")
    
    (reset)
    
    (with-model-eval player1
      (define-chunks-fct (list player2))
      
      (let ((feats (add-visicon-features `(isa (player-loc player) screen-x 0 screen-y 0 name ,player1 position (nil 0) turn (nil t))
                                         `(isa (player-loc player) screen-x 100 screen-y 0 name ,player2 position (nil 5)))))
        (setf *p1p1* (first feats))
        (setf *p1p2* (second feats))))
    
    (with-model-eval player2
      (define-chunks-fct (list player1))
      (let ((feats (add-visicon-features `(isa (player-loc player) screen-x 0 screen-y 0 name ,player1 position (nil 0) turn (nil t))
                                         `(isa (player-loc player) screen-x 100 screen-y 0 name ,player2 position (nil 5)))))
        (setf *p2p1* (first feats))
        (setf *p2p2* (second feats))))
    
    
    ;; initialize the game information
    (setf *p1* player1)
    (setf *p2* player2)
    (setf *p1-position* 0)
    (setf *p2-position* 5)
    (setf *game-over* nil)
    (setf *current-player* player1)
    (setf *safety-stop* nil)
    (setf *winner* nil)
    
    (run-until-condition 'game-over)
    
    (remove-act-r-command-monitor "output-key" "move")    
    (remove-act-r-command "move")
    
    ;; return the winner
    *winner*))

#|
CG-USER(100): (play 'model1 'model2)
     0.000  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL1  VISION                 PROC-DISPLAY
     0.000  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION PLAYER-LOC0 NIL
     0.000  MODEL1  VISION                 visicon-update
     0.000  MODEL2  VISION                 PROC-DISPLAY
     0.000  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION PLAYER-LOC0 NIL
     0.000  MODEL2  VISION                 visicon-update
     0.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED START
     0.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL2  PROCEDURAL             PRODUCTION-SELECTED START
     0.000  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED START
MY NAME IS MODEL1 
     0.050  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050  MODEL2  PROCEDURAL             PRODUCTION-FIRED START
MY NAME IS MODEL2 
     0.050  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050  MODEL1  VISION                 Find-location
     0.050  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION PLAYER-LOC0
     0.050  MODEL2  VISION                 Find-location
     0.050  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION PLAYER-LOC1
     0.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL1  PROCEDURAL             PRODUCTION-SELECTED ATTEND
     0.050  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     0.050  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL2  PROCEDURAL             PRODUCTION-SELECTED ATTEND
     0.050  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     0.100  MODEL1  PROCEDURAL             PRODUCTION-FIRED ATTEND
     0.100  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL
     0.100  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     0.100  MODEL2  PROCEDURAL             PRODUCTION-FIRED ATTEND
     0.100  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL
     0.100  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     0.100  MODEL1  VISION                 Move-attention PLAYER-LOC0-1 NIL
     0.100  MODEL2  VISION                 Move-attention PLAYER-LOC1-0 NIL
     0.100  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.100  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.185  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     0.185  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL PLAYER0
     0.185  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     0.185  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL PLAYER0
     0.185  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.185  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     0.185  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.185  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     0.185  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.185  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.235  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 0 
     0.235  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     0.235  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     0.235  MODEL1  MOTOR                  PRESS-KEY KEY 1
     0.235  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.485  MODEL1  MOTOR                  PREPARATION-COMPLETE
     0.485  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.535  MODEL1  MOTOR                  INITIATION-COMPLETE
     0.535  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.635  MODEL1  KEYBOARD               output-key MODEL1 1
     0.635  MODEL1  VISION                 PROC-DISPLAY
     0.635  MODEL1  VISION                 visicon-update
     0.635  MODEL2  VISION                 PROC-DISPLAY
     0.635  MODEL2  VISION                 visicon-update
     0.635  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.635  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.720  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     0.720  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK0 NIL
     0.720  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     0.720  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK0 NIL
     0.720  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.720  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.720  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     0.720  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.720  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     0.720  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.770  MODEL2  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 5 
     0.770  MODEL2  PROCEDURAL             MODULE-REQUEST MANUAL
     0.770  MODEL2  PROCEDURAL             CLEAR-BUFFER MANUAL
     0.770  MODEL2  MOTOR                  PRESS-KEY KEY 1
     0.770  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.785  MODEL1  MOTOR                  FINISH-MOVEMENT
     0.785  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.020  MODEL2  MOTOR                  PREPARATION-COMPLETE
     1.020  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.070  MODEL2  MOTOR                  INITIATION-COMPLETE
     1.070  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.170  MODEL2  KEYBOARD               output-key MODEL2 1
     1.170  MODEL1  VISION                 PROC-DISPLAY
     1.170  MODEL1  VISION                 visicon-update
     1.170  MODEL2  VISION                 PROC-DISPLAY
     1.170  MODEL2  VISION                 visicon-update
     1.170  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.170  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.255  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     1.255  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK1 NIL
     1.255  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     1.255  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK1 NIL
     1.255  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.255  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.255  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     1.255  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.255  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     1.255  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.305  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 1 
     1.305  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     1.305  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.305  MODEL1  MOTOR                  PRESS-KEY KEY 1
     1.305  MODEL1  MOTOR                  PREPARATION-COMPLETE
     1.305  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.320  MODEL2  MOTOR                  FINISH-MOVEMENT
     1.320  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.355  MODEL1  MOTOR                  INITIATION-COMPLETE
     1.355  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.455  MODEL1  KEYBOARD               output-key MODEL1 1
     1.455  MODEL1  VISION                 PROC-DISPLAY
     1.455  MODEL1  VISION                 visicon-update
     1.455  MODEL2  VISION                 PROC-DISPLAY
     1.455  MODEL2  VISION                 visicon-update
     1.455  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.455  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.540  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     1.540  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK2 NIL
     1.540  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     1.540  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK2 NIL
     1.540  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.540  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.540  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     1.540  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.540  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     1.540  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.590  MODEL2  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 4 
     1.590  MODEL2  PROCEDURAL             MODULE-REQUEST MANUAL
     1.590  MODEL2  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.590  MODEL2  MOTOR                  PRESS-KEY KEY 1
     1.590  MODEL2  MOTOR                  PREPARATION-COMPLETE
     1.590  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.605  MODEL1  MOTOR                  FINISH-MOVEMENT
     1.605  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.640  MODEL2  MOTOR                  INITIATION-COMPLETE
     1.640  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.740  MODEL2  KEYBOARD               output-key MODEL2 1
     1.740  MODEL1  VISION                 PROC-DISPLAY
     1.740  MODEL1  VISION                 visicon-update
     1.740  MODEL2  VISION                 PROC-DISPLAY
     1.740  MODEL2  VISION                 visicon-update
     1.740  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.740  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     1.825  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK3 NIL
     1.825  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     1.825  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK3 NIL
     1.825  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     1.825  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.825  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     1.825  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.875  MODEL1  PROCEDURAL             PRODUCTION-FIRED 2-STEP
I AM AT POSITION 2 
     1.875  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     1.875  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.875  MODEL1  MOTOR                  PRESS-KEY KEY 2
     1.875  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.890  MODEL2  MOTOR                  FINISH-MOVEMENT
     1.890  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.025  MODEL1  MOTOR                  PREPARATION-COMPLETE
     2.025  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.075  MODEL1  MOTOR                  INITIATION-COMPLETE
     2.075  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.175  MODEL1  KEYBOARD               output-key MODEL1 2
     2.175  MODEL1  VISION                 PROC-DISPLAY
     2.175  MODEL1  VISION                 visicon-update
     2.175  MODEL2  VISION                 PROC-DISPLAY
     2.175  MODEL2  VISION                 visicon-update
     2.175  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.175  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.260  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     2.260  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK4 NIL
     2.260  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     2.260  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK4 NIL
     2.260  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.260  MODEL1  PROCEDURAL             PRODUCTION-SELECTED RESULT
     2.260  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.260  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     2.260  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.260  MODEL2  PROCEDURAL             PRODUCTION-SELECTED RESULT
     2.260  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.260  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     2.310  MODEL1  PROCEDURAL             PRODUCTION-FIRED RESULT
I WIN 
     2.310  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     2.310  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.310  MODEL2  PROCEDURAL             PRODUCTION-FIRED RESULT
I LOSE 
     2.310  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     2.310  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.310  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.310  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.325  MODEL1  MOTOR                  FINISH-MOVEMENT
     2.325  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     5.175  MODEL1  NONE                   SET-GAME-OVER
     5.175  -       ------                 Stopped because condition is true
MODEL1
|#
