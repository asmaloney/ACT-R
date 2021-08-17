;; Play the simple capture game with 2 models using no interface.
;; Goal chunk holds all information and state indicates what to do (if set).
;; Creates new goal chunks everytime instead of checking and modifying.

;;  (chunk-type play my-pos p1 p2 state result)
;; My-pos is either p1 or p2 to indicate which player
;; p1 and p2 slots hold current position for that player
;; State slot will be move or game-over
;; Result will be win or lose
;; 
;; Assumes models interact appropriately so as to not
;; have problems with goal chunks being modified:
;; - only do something on its own turn
;; - stop doing everything once it makes the move

;; global variables for game info

(defvar *current-player*) ; name of the model
(defvar *p1*)             ; player1 model name
(defvar *p2*)             ; player2 model name
(defvar *game-over*)      ; If true stop the game

(defvar *next-move*)      ; action made by a player

(defvar *p1-position*)    ; current player spots 0-5
(defvar *p2-position*)


;; function called directly by a model to make a move of dist spaces
;; Just schedule a call to a function that actually handles that after
;; everything else the model needs to do at this time.

(defun make-move (dist)
  (when (eq (current-model) *current-player*)
    (setf *next-move* dist)
    (schedule-break-relative 0 :priority :min)))

;; Actually update the models' goal chunks as needed

(defun process-move (dist)
  (let ((forfeit nil)
        (winner nil))
    (if (numberp dist)
      (if (eq *current-player* *p1*) ; update the position
          (incf *p1-position* dist)
        (decf *p2-position* dist))
      (setf forfeit *current-player*))
    
  
    (cond ((or forfeit (<= *p2-position* *p1-position*)) ; the game is over
           
           (if forfeit
               (setf winner (if (eq *p1* *current-player*) *p2* *p1*))
             (setf winner *current-player*))
           
           (setf *game-over* winner)
         
           ;; create new chunks with the results
           
           (with-model-eval *p1*
             (goal-focus-fct (first (define-chunks-fct
                                        (list (list 'isa 'play 'my-pos 'p1 'p1 *p1-position* 'p2 *p2-position* 'state 'game-over
                                                    'result (if (eq *p1* winner) 'win 'lose)))))))
           ;; update player 2 goal
           
           (with-model-eval *p2*
             (goal-focus-fct (first (define-chunks-fct
                                        (list (list 'isa 'play 'my-pos 'p2 'p1 *p1-position* 'p2 *p2-position* 'state 'game-over
                                                    'result (if (eq *p2* winner) 'win 'lose))))))))
          
          ((eq *current-player* *p1*) ;; player 1 moved so update player 2 goal
           
           (with-model-eval *p2*
             (goal-focus-fct (car (define-chunks-fct
                                      (list (list 'isa 'play 'my-pos 'p2 'p1 *p1-position* 'p2 *p2-position* 'state 'move))))))
           (setf *current-player* *p2*))
          
          (t                          ;; player 2 moved so update player 1 goal
           (with-model-eval *p1*
             (goal-focus-fct (car (define-chunks-fct
                                      (list (list 'isa 'play 'my-pos 'p1 'p1 *p1-position* 'p2 *p2-position* 'state 'move))))))
           (setf *current-player* *p1*)))))
  
  
  
;; play one round of the game resetting the models
;; before it starts.  Player1 and player2 are the
;; names of the models to run in the indicated 
;; position.

(defun play (player1 player2)
  
  ;make sure that both names are valid model names
    
  (when (every (lambda (x) (find x (mp-models))) (list player1 player2))
    
    (add-act-r-command "make-move" 'make-move "Model action function in simple capture game.")
    
    (reset)
    
    ; For the players create the goal chunks
    
    (with-model-eval player1
      
      (define-chunks 
          (goal isa play my-pos p1 p1 0 p2 5 state move))
      
      (goal-focus goal))
    
    (with-model-eval player2
      
      (define-chunks 
          (goal isa play my-pos p2 p1 0 p2 5))
      
      (goal-focus goal))

    
    ;; initialize the game information
    (setf *p1* player1)
    (setf *p2* player2)
    (setf *p1-position* 0)
    (setf *p2-position* 5)
    (setf *game-over* nil)
    (setf *current-player* player1)
    (setf *next-move* nil)
    
    (while (not *game-over*)
      ;; until a move is made which breaks the run
      ;; or 1000 simulated seconds pass which is a forfeit
      (run 1000) 
      (process-move *next-move*))
    
    ;; Give them a chance to process results
    (run-full-time 3)
    
    (remove-act-r-command "make-move")
    
    ;; return the winner
    *game-over*))

#|
? (play 'model1 'model2)
     0.000  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     0.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED 2-STEP
I AM AT POSITION 0 
     0.050  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     0.050  ------  ------                 BREAK-EVENT 
     0.050  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL PLAY0 NIL
     0.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     0.050  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.100  MODEL2  PROCEDURAL             PRODUCTION-FIRED 2-STEP
I AM AT POSITION 5 
     0.100  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     0.100  ------  ------                 BREAK-EVENT 
     0.100  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL PLAY0 NIL
     0.100  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.100  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.100  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     0.100  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.150  MODEL1  PROCEDURAL             PRODUCTION-FIRED 2-STEP
I AM AT POSITION 2 
     0.150  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     0.150  ------  ------                 BREAK-EVENT 
     0.150  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL PLAY1 NIL
     0.150  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL PLAY1 NIL
     0.150  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.150  MODEL1  PROCEDURAL             PRODUCTION-SELECTED RESULT
     0.150  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.150  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.150  MODEL2  PROCEDURAL             PRODUCTION-SELECTED RESULT
     0.150  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.200  MODEL1  PROCEDURAL             PRODUCTION-FIRED RESULT
I WIN 
     0.200  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     0.200  MODEL2  PROCEDURAL             PRODUCTION-FIRED RESULT
I LOSE 
     0.200  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     0.200  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.200  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     3.150  -       ------                 Stopped because time limit reached
MODEL1
|#
