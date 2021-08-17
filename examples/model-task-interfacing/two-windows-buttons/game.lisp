;; Play the simple capture game with each model having its own
;; window and each player plays left->right as red in its own window.


;; global variables 

(defvar *current-player*)  ;; player name
(defvar *p1*)              ;; player 1 name
(defvar *p2*)              ;; player 2 name
(defvar *game-over*)       ;; stop game when true
(defvar *p1-position*)     ;; current player positions
(defvar *p2-position*)
(defvar *p1-win*)          ;; windows for each
(defvar *p2-win*)
(defvar *spaces-1* (make-list 6 :initial-element nil)) ;; buttons for each player
(defvar *spaces-2* (make-list 6 :initial-element nil))



(defun stop-a-run ()
  (schedule-break-relative 0))


;; The function to call when a button is pressed.
;; If the current player made the move, update the state.

(defun pick-button (model index)
  
  ;; if the current player makes the action 
  (when (eq model *current-player*)
    
    (if (eq *current-player* *p1*) 
        ;; update the boards when player1 makes a valid move
        
        (when (and (> index *p1-position*) (<= (- index *p1-position*) 2))
        
          (if (>= index *p2-position*)
            
              (progn ;; player 1 wins
              
                ;; give the models some time to process things before it stops
                
                (schedule-break-relative 3)
                (setf *game-over* *p1*)
                  
                (remove-items-from-exp-window *p1-win* (nth *p1-position* *spaces-1*) (nth index *spaces-1*))
                (remove-items-from-exp-window *p2-win* (nth (- 5 *p1-position*) *spaces-2*) (nth (- 5 index) *spaces-2*))
                 
                (add-button-to-exp-window *p1-win* :text "" :x (+ 10 (* *p1-position* 40)) :y 10 :height 35 :width 35 :color 'white)
                (add-button-to-exp-window *p1-win* :text "1" :x (+ 10 (* index 40)) :y 10 :height 35 :width 35 :color 'green)
                
                
                (add-button-to-exp-window *p2-win* :text "" :x (+ 10 (* (- 5 *p1-position*) 40)) :y 10 :height 35 :width 35 :color 'white)
                (add-button-to-exp-window *p2-win* :text "2" :x (+ 10 (* (- 5 index) 40)) :y 10 :height 35 :width 35 :color 'green))
            
            (progn  ;; non-winning player 1 move
              
              (remove-items-from-exp-window *p1-win* (nth *p1-position* *spaces-1*) (nth index *spaces-1*))
              
              (remove-items-from-exp-window *p2-win* (nth (- 5 *p2-position*) *spaces-2*)
                                            (nth (- 5 *p1-position*) *spaces-2*) (nth (- 5 index) *spaces-2*))
              
              (setf (nth *p1-position* *spaces-1*) 
                (add-button-to-exp-window *p1-win* :text "" :x (+ 10 (* *p1-position* 40)) :y 10 
                                          :height 35 :width 35 :color 'white))
              
              (setf (nth index *spaces-1*)
                (add-button-to-exp-window *p1-win* :text "1" :x (+ 10 (* index 40)) :y 10 
                                          :height 35 :width 35 :color 'white))
              
              
              (setf (nth (- 5 *p2-position*) *spaces-2*) 
                (add-button-to-exp-window *p2-win* :text "1" :x (+ 10 (* (- 5 *p2-position*) 40)) :y 10 
                                          :height 35 :width 35 :color 'red))
              
              (setf (nth (- 5 *p1-position*) *spaces-2*)
                (add-button-to-exp-window *p2-win* :text "" :x (+ 10 (* (- 5 *p1-position*) 40)) :y 10 
                                          :action (list 'pick-button *p2* *p1-position*) :height 35 :width 35 :color 'white))
              
              (setf (nth (- 5 index) *spaces-2*)
                (add-button-to-exp-window *p2-win* :text "2" :x (+ 10 (* (- 5 index) 40)) :y 10 
                                          :action (list 'pick-button *p2* index) :height 35 :width 35 :color 'white))
            
              (setf *p1-position* index)
              
              (setf *current-player* *p2*))))
      
      ;; player 2 makes a valid move
      (when (and (< index *p2-position*) (<= (- *p2-position* index) 2))
        
        (if (<= index *p1-position*)
            (progn ;; player 2 wins
              
              ;; give the models some time to process things before it stops
              
              (schedule-break-relative 3)
              (setf *game-over* *p2*)
              
              (remove-items-from-exp-window *p2-win* (nth (- 5 *p2-position*) *spaces-2*) (nth (- 5 index) *spaces-2*))
              (remove-items-from-exp-window *p1-win* (nth *p2-position* *spaces-1*) (nth index *spaces-1*))
              
              (add-button-to-exp-window *p2-win* :text "" :x (+ 10 (* (- 5 *p2-position*) 40)) :y 10 :height 35 :width 35 :color 'white)
              (add-button-to-exp-window *p2-win* :text "1" :x (+ 10 (* (- 5 index) 40)) :y 10 :height 35 :width 35 :color 'green)
              
              
              (add-button-to-exp-window *p1-win* :text "" :x (+ 10 (* *p2-position* 40)) :y 10 :height 35 :width 35 :color 'white)
              (add-button-to-exp-window *p1-win* :text "2" :x (+ 10 (* index 40)) :y 10 :height 35 :width 35 :color 'green))
          
          
          (progn  ;; non-winning player 2 move
            
            (remove-items-from-exp-window *p2-win* (nth (- 5 *p2-position*) *spaces-2*) (nth (- 5 index) *spaces-2*))
            (remove-items-from-exp-window *p1-win* (nth *p2-position* *spaces-1*)
                                          (nth *p1-position* *spaces-1*) (nth index *spaces-1*))
            
            (setf (nth (- 5 *p2-position*) *spaces-2*) 
              (add-button-to-exp-window *p2-win* :text "" :x (+ 10 (* (- 5 *p2-position*) 40)) :y 10 
                                        :height 35 :width 35 :color 'white))
            
            (setf (nth (- 5 index) *spaces-2*)
              (add-button-to-exp-window *p2-win* :text "1" :x (+ 10 (* (- 5 index) 40)) :y 10 
                                        :height 35 :width 35 :color 'white))
            
            
            (setf (nth *p2-position* *spaces-1*)
              (add-button-to-exp-window *p1-win* :text "" :x (+ 10 (* *p2-position* 40)) :y 10 
                                        :action (list 'pick-button *p1* *p2-position*) :height 35 :width 35 :color 'white))
            
            
            (setf (nth *p1-position* *spaces-1*) 
              (add-button-to-exp-window *p1-win* :text "1" :x (+ 10 (* *p1-position* 40)) :y 10 
                                        :height 35 :width 35 :color 'red))
                        
            (setf (nth index *spaces-1*)
              (add-button-to-exp-window *p1-win* :text "2" :x (+ 10 (* index 40)) :y 10 
                                        :action (list 'pick-button *p1* index) :height 35 :width 35 :color 'white))
            
            (setf *p2-position* index)
            
            (setf *current-player* *p1*)))))))
  
  

;; play one round of the game resetting models
;; before it starts.  Player1 and player2 are the
;; names of models to run.

(defun play (player1 player2)
  
  (when (every (lambda (x) (find x (mp-models))) (list player1 player2))
    (reset)
  
    (setf *p1* player1)
    (setf *p2* player2)
    (setf *game-over* nil)
    (setf *current-player* player1)
    (setf *p1-position* 0)
    (setf *p2-position* 5)
    
    (let ((p1-window (open-exp-window "player1" :visible t :width 300 :height 100 :x 100 :y 100))
          (p2-window (open-exp-window "player2" :visible t :width 300 :height 100 :x 450 :y 100))
          (safety-window (open-exp-window "safety" :visible t :width 100 :height 100 :x 100 :y 300)))
      
      (setf *p1-win* p1-window)
      (setf *p2-win* p2-window)
      
      (add-button-to-exp-window safety-window :text "STOP" :x 0 :y 0 :action 'stop-a-run :height 80 :width 80 :color 'red)
      
      (dotimes (i 6)            ;; create the buttons
        
        (setf (nth i *spaces-1*)  ;; save them for later use
          (let ((index i))      ;; need to bind the value of i to use in the action function
            (add-button-to-exp-window p1-window :text (if (zerop i) "1" (if (= i 5) "2" ""))
                                      :x (+ 10 (* i 40)) 
                                      :y 10 
                                      :action (list 'pick-button player1 index) :height 35 :width 35 :color (if (zerop i) 'red 'white))))

        
        (setf (nth i *spaces-2*)  ;; save them for later use
          (let ((index i))      ;; need to bind the value of i to use in the action function
            (add-button-to-exp-window p2-window :text (if (zerop i) "1" (if (= i 5) "2" ""))
                                      :x (+ 10 (* i 40)) 
                                      :y 10 
                                      :action (list 'pick-button player2 (- 5 index)) :height 35 :width 35 :color 'white))))
      
    
      (with-model-eval player1  ;; for first player
        (install-device p1-window))
        
      
      (with-model-eval player2 ;; same for player 2 but reverse the button positions
        (install-device p2-window))
    
      (run 10000 t)))
  
  *game-over*)
 
#|
CG-USER(109): (play 'model1 'model2)
     0.000  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL1  VISION                 PROC-DISPLAY
     0.000  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0 NIL
     0.000  MODEL1  VISION                 visicon-update
     0.000  MODEL2  VISION                 PROC-DISPLAY
     0.000  MODEL2  VISION                 visicon-update
     0.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED MY-TURN
     0.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED MY-TURN
     0.050  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050  MODEL1  VISION                 Find-location
     0.050  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION2
     0.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL1  PROCEDURAL             PRODUCTION-SELECTED MOVE
     0.050  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.100  MODEL1  PROCEDURAL             PRODUCTION-FIRED MOVE
     0.100  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     0.100  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     0.100  MODEL1  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION2-0
     0.100  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.300  MODEL1  MOTOR                  PREPARATION-COMPLETE
     0.300  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.350  MODEL1  MOTOR                  INITIATION-COMPLETE
     0.350  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.590  MODEL1  MOUSE                  move-cursor MODEL1 mouse (168 128 1080)
     0.590  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.640  MODEL1  MOTOR                  FINISH-MOVEMENT
     0.640  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.640  MODEL1  PROCEDURAL             PRODUCTION-SELECTED CLICK
     0.640  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.640  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.690  MODEL1  PROCEDURAL             PRODUCTION-FIRED CLICK
     0.690  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.690  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     0.690  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     0.690  MODEL1  MOTOR                  CLICK-MOUSE
     0.690  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.840  MODEL1  MOTOR                  PREPARATION-COMPLETE
     0.840  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.890  MODEL1  MOTOR                  INITIATION-COMPLETE
     0.890  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.900  MODEL1  MOUSE                  click-mouse MODEL1 (168 128 1080) INDEX
     0.900  MODEL1  VISION                 PROC-DISPLAY
     0.900  MODEL1  VISION                 visicon-update
     0.900  MODEL2  VISION                 PROC-DISPLAY
     0.900  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION8 NIL
     0.900  MODEL2  VISION                 visicon-update
     0.900  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.900  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.900  MODEL2  PROCEDURAL             PRODUCTION-SELECTED MY-TURN
     0.900  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.900  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.950  MODEL2  PROCEDURAL             PRODUCTION-FIRED MY-TURN
     0.950  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.950  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.950  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.950  MODEL2  VISION                 Find-location
     0.950  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION3
     0.950  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.950  MODEL2  PROCEDURAL             PRODUCTION-SELECTED MOVE
     0.950  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.950  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.950  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.990  MODEL1  MOTOR                  FINISH-MOVEMENT
     0.990  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.000  MODEL2  PROCEDURAL             PRODUCTION-FIRED MOVE
     1.000  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.000  MODEL2  PROCEDURAL             MODULE-REQUEST MANUAL
     1.000  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.000  MODEL2  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.000  MODEL2  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION3-0
     1.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.200  MODEL2  MOTOR                  PREPARATION-COMPLETE
     1.200  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.250  MODEL2  MOTOR                  INITIATION-COMPLETE
     1.250  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.651  MODEL2  MOUSE                  move-cursor MODEL2 mouse (558 128 1080)
     1.651  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.701  MODEL2  MOTOR                  FINISH-MOVEMENT
     1.701  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.701  MODEL2  PROCEDURAL             PRODUCTION-SELECTED CLICK
     1.701  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.701  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.751  MODEL2  PROCEDURAL             PRODUCTION-FIRED CLICK
     1.751  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.751  MODEL2  PROCEDURAL             MODULE-REQUEST MANUAL
     1.751  MODEL2  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.751  MODEL2  MOTOR                  CLICK-MOUSE
     1.751  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.901  MODEL2  MOTOR                  PREPARATION-COMPLETE
     1.901  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.951  MODEL2  MOTOR                  INITIATION-COMPLETE
     1.951  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.961  MODEL2  MOUSE                  click-mouse MODEL2 (558 128 1080) INDEX
     1.961  MODEL2  VISION                 PROC-DISPLAY
     1.961  MODEL2  VISION                 visicon-update
     1.961  MODEL1  VISION                 PROC-DISPLAY
     1.961  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION12 NIL
     1.961  MODEL1  VISION                 visicon-update
     1.961  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.961  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.961  MODEL1  PROCEDURAL             PRODUCTION-SELECTED MY-TURN
     1.961  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.961  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.011  MODEL1  PROCEDURAL             PRODUCTION-FIRED MY-TURN
     2.011  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.011  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.011  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.011  MODEL1  VISION                 Find-location
     2.011  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION14
     2.011  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.011  MODEL1  PROCEDURAL             PRODUCTION-SELECTED MOVE
     2.011  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.011  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.011  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.051  MODEL2  MOTOR                  FINISH-MOVEMENT
     2.051  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.061  MODEL1  PROCEDURAL             PRODUCTION-FIRED MOVE
     2.061  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.061  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     2.061  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.061  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     2.061  MODEL1  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION14-0
     2.061  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.261  MODEL1  MOTOR                  PREPARATION-COMPLETE
     2.261  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.311  MODEL1  MOTOR                  INITIATION-COMPLETE
     2.311  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.459  MODEL1  MOUSE                  move-cursor MODEL1 mouse (248 128 1080)
     2.459  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.509  MODEL1  MOTOR                  FINISH-MOVEMENT
     2.509  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.509  MODEL1  PROCEDURAL             PRODUCTION-SELECTED CLICK
     2.509  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.509  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.559  MODEL1  PROCEDURAL             PRODUCTION-FIRED CLICK
     2.559  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.559  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     2.559  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     2.559  MODEL1  MOTOR                  CLICK-MOUSE
     2.559  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.709  MODEL1  MOTOR                  PREPARATION-COMPLETE
     2.709  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.759  MODEL1  MOTOR                  INITIATION-COMPLETE
     2.759  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.769  MODEL1  MOUSE                  click-mouse MODEL1 (248 128 1080) INDEX
     2.769  MODEL1  VISION                 PROC-DISPLAY
     2.769  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION17 NIL
     2.769  MODEL1  VISION                 visicon-update
     2.769  MODEL2  VISION                 PROC-DISPLAY
     2.769  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION17 NIL
     2.769  MODEL2  VISION                 visicon-update
     2.769  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.769  MODEL1  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.769  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.769  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.769  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.769  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.769  MODEL2  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.769  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.769  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.769  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.819  MODEL1  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.819  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL
     2.819  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     2.819  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.819  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.819  MODEL2  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.819  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL
     2.819  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     2.819  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.819  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.819  MODEL1  VISION                 Move-attention VISUAL-LOCATION17-0 NIL
     2.819  MODEL2  VISION                 Move-attention VISUAL-LOCATION17-0 NIL
     2.819  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.819  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.859  MODEL1  MOTOR                  FINISH-MOVEMENT
     2.859  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.904  MODEL1  VISION                 Encoding-complete VISUAL-LOCATION17-0 NIL
     2.904  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL OVAL0
     2.904  MODEL2  VISION                 Encoding-complete VISUAL-LOCATION17-0 NIL
     2.904  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL OVAL0
     2.904  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.904  MODEL2  PROCEDURAL             PRODUCTION-SELECTED REPORT-LOSE
     2.904  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     2.904  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.904  MODEL1  PROCEDURAL             PRODUCTION-SELECTED REPORT-WIN
     2.904  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     2.954  MODEL2  PROCEDURAL             PRODUCTION-FIRED REPORT-LOSE
I LOSE 
     2.954  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.954  MODEL1  PROCEDURAL             PRODUCTION-FIRED REPORT-WIN
I WIN 
     2.954  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.954  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.954  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     5.769  ------  ------                 BREAK-EVENT 
MODEL1
|#
