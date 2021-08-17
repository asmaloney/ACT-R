
;;; Play the capture game with a simple single interface that
;;; just shows position.  Have models say "one" or "two" to indicate 
;;; their move and the game says the starting player's name at 
;;; the beginning.  Show the winner's name in green when game over.


;; global variables

(defvar *current-player*) ;; model name
(defvar *p1*)             ;; player names
(defvar *p2*)
(defvar *game-over*)      ;; true means stop game
(defvar *p1-position*)    ;; player positions
(defvar *p2-position*)
(defvar *p1-text*)        ;; text items on the screen
(defvar *p2-text*)        ;; for player positions
(defvar *window*)         ;; the interface window

(defvar *safety-stop*)     ;; flag that modeler wants run to stop


(defun stop-a-run ()
  (setf *safety-stop* t))


(defun set-game-over (player) 
  (setf *game-over* player))

(defun game-over (time)
  (or *safety-stop* *game-over* (> time 1000000)))


;; the method that gets called when a model speaks
;; assume that models take turns appropriately

(defun process-moves (model string)
  
  ;; Let all other models hear this 
  (dolist (m (mp-models))
    (unless (eq model m) ; for everyone but the originator
      
      (with-model-eval m ; make the other model the current-model
        
        ;; Create the originating model's name as a simple chunk if
        ;; it isn't one already to avoid a warning of it being
        ;; created by default when the sound is generated.
        
        (unless (chunk-p-fct model)
          (define-chunks-fct (list model)))
        
        ;; Create the sound as a word with location indicating
        ;; the speaker.
        
        (new-word-sound string (mp-time) model))))
  
  (let ((move (cond ((string-equal string "one") 1)
                    ((string-equal string "two") 2)
                    (t 1))))
    
    ;; update the position
    (if (eq *current-player* *p1*)
        (incf *p1-position* move)
      (decf *p2-position* move))
    
    (clear-exp-window *window*)
    
    (if (<= *p2-position* *p1-position*)
        ;; if there's a winner
        (progn
          ;; set the winning player
          (schedule-event-relative 3 'set-game-over :params (list *current-player*))
                    
          ;; display the winner
          (add-text-to-exp-window *window* (string *current-player*) :x 60 :y 20 :color 'green :height 30 :width 80 :font-size 20))
      (progn
        
      ;; not a winner so update the display with the new position
        
        (setf *p1-text* (add-text-to-exp-window *window* (princ-to-string *p1-position*) :x 20 :y 10 :color 'red :height 30 :width 30 :font-size 20))
        (setf *p2-text* (add-text-to-exp-window *window* (princ-to-string *p2-position*) :x 140 :y 10 :color 'blue :height 30 :width 30 :font-size 20))
        
        (if (eq *current-player* *p1*)
            (setf *current-player* *p2*)
          (setf *current-player* *p1*))))))


;; Function to play one game resetting the models
;; before it starts.  Player1 and player2 are model
;; names.
      
(defun play (player1 player2)
  
  (when (every (lambda (x) (find x (mp-models))) (list player1 player2))
    (reset)

    (with-model-eval player1
      ;; create a goal chunk with the player's color and name  
      (define-chunks-fct (list (list 'goal 'isa 'play 'my-color 'red 'my-name (string player1))))
      (goal-focus goal)
      (set-parameter-value :show-focus 'red))
    
    (with-model-eval player2
      ;; create a goal chunk with the player's color and name
      (define-chunks-fct (list (list 'goal 'isa 'play 'my-color 'blue 'my-name (string player2))))
      (goal-focus goal)
      (set-parameter-value :show-focus 'blue))
  
  
    ;; initialize some game info
    
    (setf *p1* player1)
    (setf *p2* player2)
    (setf *game-over* nil)
    (setf *current-player* player1)
    (setf *safety-stop* nil)
    (setf *p1-position* 0)
    (setf *p2-position* 5)
  
    (let ((window (open-exp-window "game" :visible t :width 200 :height 100))
          (safety-window (open-exp-window "safety" :visible t :width 100 :height 100 :x 100 :y 100)))
      
      (add-button-to-exp-window safety-window :text "STOP" :x 0 :y 0 :action 'stop-a-run :x 80 :y 80 :color 'red)

    
      (setf *window* window)
    
      ;; install that as the device for all models
      (dolist (m (mp-models))
        (with-model-eval m
          (install-device window)))
    
      
      (setf *p1-text* (add-text-to-exp-window window (princ-to-string *p1-position*) :x 20 :y 10 :color 'red :height 30 :width 30 :font-size 20))
      (setf *p2-text* (add-text-to-exp-window window (princ-to-string *p2-position*) :x 140 :y 10 :color 'blue :height 30 :width 30 :font-size 20))
      
      (add-act-r-command "process-moves" 'process-moves "Handle player speak actions")
      (monitor-act-r-command "output-speech" "process-moves")
      
      ;; speak to all models telling them the name
      ;; of the first player.
      
      (dolist (m (mp-models))
        (with-model-eval m
          (new-word-sound (string *p1*) 0 'start))))
        
    (run-until-condition 'game-over t)
    
    (remove-act-r-command-monitor "output-speech" "process-moves")    
    (remove-act-r-command "process-moves"))
  
  *game-over*)

#|
CG-USER(105): (play 'model1 'model2)
     0.000  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL1  AUDIO                  new-sound
     0.000  MODEL2  AUDIO                  new-sound
     0.000  MODEL2  VISION                 PROC-DISPLAY
     0.000  MODEL2  VISION                 visicon-update
     0.000  MODEL1  VISION                 PROC-DISPLAY
     0.000  MODEL1  VISION                 visicon-update
     0.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.300  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
     0.300  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
     0.300  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.300  MODEL1  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     0.300  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300  MODEL1  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     0.300  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     0.300  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.300  MODEL2  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     0.300  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     0.300  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     0.350  MODEL1  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     0.350  MODEL1  PROCEDURAL             MODULE-REQUEST AURAL
     0.350  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     0.350  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL
     0.350  MODEL2  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     0.350  MODEL2  PROCEDURAL             MODULE-REQUEST AURAL
     0.350  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     0.350  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     0.350  MODEL1  AUDIO                  ATTEND-SOUND AUDIO-EVENT0-0
     0.350  MODEL2  AUDIO                  ATTEND-SOUND AUDIO-EVENT0-0
     0.350  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.350  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.500  MODEL1  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     0.500  MODEL2  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     0.500  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL WORD0
     0.500  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL WORD0
     0.500  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.500  MODEL1  PROCEDURAL             PRODUCTION-SELECTED I-START
     0.500  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.500  MODEL1  PROCEDURAL             BUFFER-READ-ACTION AURAL
     0.500  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.500  MODEL2  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-STARTS
     0.500  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.500  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL
     0.550  MODEL1  PROCEDURAL             PRODUCTION-FIRED I-START
     0.550  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.550  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL
     0.550  MODEL2  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-STARTS
     0.550  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     0.550  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.550  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     0.550  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.550  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     0.550  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.600  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
     0.600  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.600  MODEL1  PROCEDURAL             MODULE-REQUEST VOCAL
     0.600  MODEL1  PROCEDURAL             CLEAR-BUFFER VOCAL
     0.600  MODEL1  SPEECH                 SPEAK TEXT one
     0.600  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.750  MODEL1  SPEECH                 PREPARATION-COMPLETE
     0.750  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.800  MODEL1  SPEECH                 INITIATION-COMPLETE
     0.800  MODEL1  AUDIO                  new-sound
     0.800  MODEL1  MICROPHONE             output-speech MODEL1 one
     0.800  MODEL2  AUDIO                  new-sound
     0.800  MODEL2  VISION                 PROC-DISPLAY
     0.800  MODEL2  VISION                 visicon-update
     0.800  MODEL1  VISION                 PROC-DISPLAY
     0.800  MODEL1  VISION                 visicon-update
     0.800  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.800  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.950  MODEL1  SPEECH                 FINISH-MOVEMENT
     0.950  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.100  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT1 NIL
     1.100  MODEL1  AUDIO                  find-sound-failure
     1.100  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.100  MODEL2  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     1.100  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.100  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     1.100  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     1.100  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.150  MODEL2  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     1.150  MODEL2  PROCEDURAL             MODULE-REQUEST AURAL
     1.150  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     1.150  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     1.150  MODEL2  AUDIO                  ATTEND-SOUND AUDIO-EVENT1-0
     1.150  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.225  MODEL2  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     1.225  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL WORD1
     1.225  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.225  MODEL2  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-WENT-ONE
     1.225  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.225  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL
     1.275  MODEL2  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-WENT-ONE
     1.275  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.275  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     1.275  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.275  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     1.275  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.275  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     1.325  MODEL2  PROCEDURAL             PRODUCTION-FIRED 2-STEP
     1.325  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.325  MODEL2  PROCEDURAL             MODULE-REQUEST VOCAL
     1.325  MODEL2  PROCEDURAL             CLEAR-BUFFER VOCAL
     1.325  MODEL2  SPEECH                 SPEAK TEXT two
     1.325  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.475  MODEL2  SPEECH                 PREPARATION-COMPLETE
     1.475  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.525  MODEL2  SPEECH                 INITIATION-COMPLETE
     1.525  MODEL2  AUDIO                  new-sound
     1.525  MODEL2  MICROPHONE             output-speech MODEL2 two
     1.525  MODEL1  AUDIO                  new-sound
     1.525  MODEL2  VISION                 PROC-DISPLAY
     1.525  MODEL2  VISION                 visicon-update
     1.525  MODEL1  VISION                 PROC-DISPLAY
     1.525  MODEL1  VISION                 visicon-update
     1.525  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.525  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.675  MODEL2  SPEECH                 FINISH-MOVEMENT
     1.675  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT2 NIL
     1.825  MODEL2  AUDIO                  find-sound-failure
     1.825  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     1.825  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.825  MODEL1  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     1.825  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     1.825  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.875  MODEL1  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     1.875  MODEL1  PROCEDURAL             MODULE-REQUEST AURAL
     1.875  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     1.875  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL
     1.875  MODEL1  AUDIO                  ATTEND-SOUND AUDIO-EVENT2-0
     1.875  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.950  MODEL1  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     1.950  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL WORD2
     1.950  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.950  MODEL1  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-WENT-TWO
     1.950  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.950  MODEL1  PROCEDURAL             BUFFER-READ-ACTION AURAL
     2.000  MODEL1  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-WENT-TWO
     2.000  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.000  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL
     2.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     2.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.000  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     2.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED 2-STEP
     2.050  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.050  MODEL1  PROCEDURAL             MODULE-REQUEST VOCAL
     2.050  MODEL1  PROCEDURAL             CLEAR-BUFFER VOCAL
     2.050  MODEL1  SPEECH                 SPEAK TEXT two
     2.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.150  MODEL1  SPEECH                 PREPARATION-COMPLETE
     2.150  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.200  MODEL1  SPEECH                 INITIATION-COMPLETE
     2.200  MODEL1  AUDIO                  new-sound
     2.200  MODEL1  MICROPHONE             output-speech MODEL1 two
     2.200  MODEL2  AUDIO                  new-sound
     2.200  MODEL2  VISION                 PROC-DISPLAY
     2.200  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION6 NIL
     2.200  MODEL2  VISION                 visicon-update
     2.200  MODEL1  VISION                 PROC-DISPLAY
     2.200  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION6 NIL
     2.200  MODEL1  VISION                 visicon-update
     2.200  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.200  MODEL1  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.200  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.200  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.200  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.200  MODEL2  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.200  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.200  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.250  MODEL1  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.250  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL
     2.250  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.250  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.250  MODEL2  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.250  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL
     2.250  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.250  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.250  MODEL1  VISION                 Move-attention VISUAL-LOCATION6-0 NIL
     2.250  MODEL2  VISION                 Move-attention VISUAL-LOCATION6-0 NIL
     2.250  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.250  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.335  MODEL1  VISION                 Encoding-complete VISUAL-LOCATION6-0 NIL
     2.335  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     2.335  MODEL2  VISION                 Encoding-complete VISUAL-LOCATION6-0 NIL
     2.335  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     2.335  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.335  MODEL1  PROCEDURAL             PRODUCTION-SELECTED WINNER
     2.335  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.335  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     2.335  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.335  MODEL2  PROCEDURAL             PRODUCTION-SELECTED LOSER
     2.335  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.335  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     2.350  MODEL1  SPEECH                 FINISH-MOVEMENT
     2.385  MODEL1  PROCEDURAL             PRODUCTION-FIRED WINNER
I AM THE WINNER 
     2.385  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     2.385  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.385  MODEL2  PROCEDURAL             PRODUCTION-FIRED LOSER
"model1" WAS THE WINNER 
     2.385  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     2.385  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.385  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.385  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.500  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT3 NIL
     2.500  MODEL1  AUDIO                  find-sound-failure
     2.500  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.500  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     5.200  MODEL1  NONE                   SET-GAME-OVER MODEL1
     5.200  -       ------                 Stopped because condition is true
MODEL1
|#

