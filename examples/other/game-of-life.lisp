;;; This example shows the ACT-R software being used as a general 
;;; programming tool instead of as a model of human cognition by
;;; creating a model that encapsulates the rules for a single cell
;;; in Conway's Game of Life and then creating many cells each 
;;; using that model to play the game.
;;;
;;; While this isn't the intended purpose of the software, it 
;;; does demonstrate some of the more advanced operations of the
;;; software which one may need when creating tasks for cognitive 
;;; models.  The operations used to run this task include working
;;; with more than one model at a time, creating arbitrary percepts,
;;; modifying AGI elements as the system runs, and using an arbitrary
;;; real-time clock to provide the current time for a model (an 
;;; absolute clock instead of an interval clock).
;;;
;;; The default display uses an AGI window to display a grid of buttons
;;; with a red button indicating a cell which is on and a gray button
;;; one which is off.  In that display there is also a button labeled
;;; "stop" which if pressed while ACT-R is running will schedule a
;;; break event to stop the run.
;;;
;;;
;;; To run the game call run-game and it requires two parameters which
;;; are the width and height of the board in cells:
;;;
;;; (run-game 10 10)
;;; 
;;; That will run the game with a randomly generated starting state 
;;; for 10 seconds with one cycle per second displayed (after the 
;;; initial initialization of the display and all of the models 
;;; which takes a little time).  
;;;
;;; On my machine up to about 200 cells can run at a rate of
;;; one update per second with the graphic display through the
;;; ACT-R Environment, but performance will vary for many reasons.
;;;
;;; To continue running it you can just call run-full-time again 
;;; with the number of cycles to display (it always uses one cycle
;;; per second of model time) and must include the real-time flag
;;; since the game's real-time clock controls the updating:
;;;
;;; (run-full-time 10 t)
;;;
;;; It's also possible to specify a longer or shorter initial number
;;; of cycles to show using the :cycles keyword parameter:
;;;
;;; (run-game 10 10 :cycles 15)
;;;
;;; Would run the game for 15 cycles before stopping.
;;;
;;; Similarly, the display rate can be changed. The default is to
;;; wait one second per cycle drawn, but specifying the :spc keyword
;;; parameter allows you to change that.  This would draw a new
;;; cycle every 2 seconds:
;;;
;;; (run-game 10 10 :spc 2)
;;;
;;; and this would draw 4 new cycles every second (assuming that
;;; the system can run that fast):
;;;
;;; (run-game 10 10 :spc .25)
;;;
;;; 
;;; A text only option can also be run by providing the :text
;;; keyword parameter with a true value.
;;;
;;; (run-game 10 10 :text t)
;;; 
;;; That will draw the board to the command-trace assuming a fixed
;;; width font with a * for on and space for off:

#|
____________
|**   **   |
| *      * |
|    *** **|
|*      * *|
|    *  * *|
|    *     |
|     **   |
|   ** *   |
|  *     * |
|   * ** **|
------------
|#

;;; The default game starts with a random distribution
;;; of on cells where the probability of a cell being on
;;; can be specified by the :p parameter to run-game.  The
;;; default p is .25:
;;; 
;;; (run-game 10 10 :p .5)
;;;
;;; Finally, it is possible to manually configure the starting
;;; board when using the graphic representation by specifying 
;;; the :setup parameter as t:
;;;
;;; (run-game 10 10 :setup t)
;;;
;;; When that happens a random board will be generated, but
;;; the user can click on the cells to toggle them.  When 
;;; you are ready to run the model with the cell configuration
;;; created press the "done" button on the display.
;;; If one wants to setup initial states it can be useful to
;;; also specify :p as 0 or 1 to start with a board that is
;;; either all off or all on respectively.
;;;
;;;
;;; The models which control the cells are provided with
;;; a visual-location chunk that contains their current
;;; state and how many neighbors they have, and they use
;;; buffer stuffing to always get the new chunk when the
;;; game updates.  Each must respond vocally saying either
;;; "on" to turn on its cell, "off" to turn its cell off,
;;; or anything else to remain in the same state.  Once 
;;; all models have spoken their new status the game will
;;; advance the clock to the next cycle once the requested
;;; cycle time has passed (or immediately if it passed 
;;; before all the models responced).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The code which implements the models that control the cells.
;;; Specified in a list so that multiple instances of the same
;;; model can easily be generated.

(defparameter *model-code* 
  '((sgp :v nil)
    
    ;; Install a microphone so their speech output can
    ;; be detected.
    
    (install-device '("speech" "microphone"))
    
    ;; Set the constraints for stuffing the visual-location
    ;; buffer to be any chunk in the visicon since the default
    ;; is :attended new which means it'd eventually stop stuffing
    ;; the chunk once it was no longer "new".
   
    (set-visloc-default isa visual-location)
    
    ;; Create a new chunk-type for the cell information
    ;; which will use the x and y slots for position instead
    ;; of screen-x and screen-y (it will still have a distance
    ;; and size created by default).
        
    (chunk-type cell state neighbors x y)

    
    ;; The productions to encode the rules of the game.
    
    (p lonely
       "On with less than 2 neighbors turns off"
       =visual-location>
         state t
       < neighbors 2
       ?vocal> 
         state free
     ==>
       +vocal> 
         isa speak 
         string "off")
    
    (p overcrowded
       "On with more than 3 neighbors turns off"
       =visual-location>
         state t
       > neighbors 3
       ?vocal> 
         state free
     ==>
       +vocal> 
         isa speak 
         string "off")
    
    (p born
       "Off with 3 neighbors turns on"
       =visual-location>
         state nil
         neighbors 3
       ?vocal> 
         state free
     ==>
       +vocal> 
         isa speak 
         string "on")
    
    (p good
       "On with 2 or 3 neighbors stays on (does nothing)"
       =visual-location>
         state t
       >= neighbors 2
       <= neighbors 3
       ?vocal> 
         state free
     ==>
       +vocal> 
         isa speak 
         string "same")

    (p remain-dorment
       "Off without 3 neighbors stays off"
       =visual-location>
         state nil
       - neighbors 3
       ?vocal> 
         state free
     ==>
       +vocal> 
         isa speak 
         string "same")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code to implement the task.

;;;
;;; Structures to hold the details of the board and each cell.


;;; The board holds the overall game info and has a lock for
;;; protecting the contents.  The lock is not technically necessary
;;; at this point since all of the code is only called from scheduler
;;; activity after things start running and at this point the 
;;; scheduler is a serial system, but it is still the "safe" thing
;;; to do and at some point the scheduler will allow parallel
;;; operations and that could be used to improve the performance
;;; of something like this -- if it is safe to do so.
;;; 

(defstruct gol-board height width cells model->cell responses total cycle (lock (bt:make-lock "gol-lock")))


;;; The cell structure holds the current state of the cell, the model's
;;; update for the current cycle, and all of the related items necessay:
;;; the model, visual feature, and button.

(defstruct gol-cell state change button feature model)


;;; Store the board globally for convenience.

(defvar *board*)


;;; Record the model's vocal response as the action to take this step
;;; in the cell along with an indication that the model has made its choice.

(defun process-speech (model string)
  (bt:with-lock-held ((gol-board-lock *board*))
    (let ((cell (gethash model (gol-board-model->cell *board*))))
      (when (or (string-equal string "on") (string-equal string "off"))
        (setf (gol-cell-change cell) string))
      (pushnew model (gol-board-responses *board*)))))



;;; The main function to run the game.
;;; w and h are width and height of the board in cells.
;;; cycles is how many update cycles to run.
;;; p is the probability that a cell will be on in the initial random state.
;;; setup allows the user to adjust the starting graphic based board.
;;; text indicates that only text output should be displayed.
;;; spc is the seconds per cycle setting -- how long in "real time" should
;;;     be spent between cycles (each of which is always 1 second of model
;;;     time).

(defun run-game (w h &key (cycles 10) (p .25) (setup nil) (text nil) (spc 1.0))
  
  ;; If there're any open windows close them, and delete all models for a clean start.
  
  (close-all-exp-windows)
  (clear-all)
  
  ;; Create a model for use in creating the game's events, to use for the
  ;; random state, and to output the text based version of the board.
  
  (define-model game-state (sgp :v nil :cmdt t))
    
  ;; setup the initial global game info
  
  (setf *board* (make-gol-board :width w :height h :responses nil :total (* w h) :cycle 1 :cells (make-array (list w h)) :model->cell (make-hash-table)))
  
  ;; If it's not a text only display open the window and add the
  ;; stop button which will schedule an immediate break event to
  ;; stop a run.  Since it's the only open window we don't need
  ;; to record the reference because all the other GUI calls can
  ;; specify nil as the reference to use the default window.
  
  (unless text
    (open-exp-window "Game of Life" :visible t :width (1+ (* w 21)) :height (+ 55 (* h 21)))
    (add-button-to-exp-window nil :text "Stop" :x 0 :y (+ 5 (* h 21)) 
                              :action (lambda () 
                                (schedule-break-relative 0 :details "Game stop button pressed"))
                              :height 20 :width 75))
  
  ;; For each cell instantiate a new model whose name
  ;; indicates which cell it is, set the initial state of
  ;; the cell randomly, and create a button for the display
  ;; if it's not text only.  Then record those items so that
  ;; it's easy to get back to them when needed.
  
  (dotimes (i w)
    (dotimes (j h)
      (let* ((model (intern (format nil "M-~d-~d" i j)))
             (state (< (with-model game-state (act-r-random 1.0)) p))
             (button (unless text (add-button-to-exp-window nil :text "" :x (* i 21) :y (* j 21) 
                                                            :action (when setup (list 'click-button i j))
                                                            :height 20 :width 20 
                                                            :color (if state 'red 'gray))))
             (cell (make-gol-cell 
                    :state state
                    :change nil
                    :button button
                    :model model)))
        (define-model-fct (intern (format nil "M-~d-~d" i j)) *model-code*)
        (setf (gethash model (gol-board-model->cell *board*)) cell)
        (setf (aref (gol-board-cells *board*) i j) cell))))
  
  ;; Remove any preexisting commands which have the
  ;; same names as those that we're using here and also
  ;; remove the monitoring of output-speech (assuming that
  ;; it's being monitored if our monitoring command is 
  ;; already defined).
  
  (when (check-act-r-command "update-board")
    (remove-act-r-command "update-board"))
  (when (check-act-r-command "gol-clock")
    (remove-act-r-command "gol-clock"))
  (when (check-act-r-command "process-speech")
    (remove-act-r-command-monitor "output-speech" "process-speech")
    (remove-act-r-command "process-speech"))

  ;; Add commands to update the board, handle the models' speech
  ;; output, and provide the custom clock, and then monitor the
  ;; microphone's signal to actually detect the speech output.
  
  (add-act-r-command "update-board" 'update-board "Game of Life cycle update command.")
  (add-act-r-command "process-speech" 'process-speech "Game of Life monitor for speech output.")

  (add-act-r-command "gol-clock" 
                     (let ((start-time (get-internal-real-time)))
                       (lambda () 
                         (bt:with-lock-held ((gol-board-lock *board*))
                           
                           ;; if all the models have responded and the 
                           ;; appropriate amount of real time per cycle
                           ;; has passed then advance the clock and record
                           ;; the current real-time.
                           
                           (when (and (= (gol-board-total *board*) (length (gol-board-responses *board*)))
                                      (>= (get-internal-real-time) (+ start-time (* spc internal-time-units-per-second))))
                             (incf (gol-board-cycle *board*))
                             (setf start-time (get-internal-real-time))
                             (setf (gol-board-responses *board*) nil))
                           
                           ;; Always return the current time as 1ms before the
                           ;; next update so that is as far as the models will
                           ;; run before waiting.
                           
                           (- (* (gol-board-cycle *board*) 1000) 1))))
                     
                     "Game of Life custom clock command")
  
  (monitor-act-r-command "output-speech" "process-speech")

  ;; Install this clock command as an absolute clock i.e. it
  ;; returns the current time instead of an arbitrary interval
  ;; which ACT-R then tests, and indicate that its units are
  ;; milliseconds (1000 units per second).
  
  (mp-real-time-management :mode 'absolute :time-function "gol-clock" :units-per-second 1000)
  
  
  ;; Let the user create a custom starting state for the gui when requeted.
  ;; Create a button to press when done and just loop until the user
  ;; presses it, and then remove it and disable all the buttons -- set
  ;; their action to the AGI default function.
  
  (when (and setup (not text))
    
    (let* ((done nil)
           (b (add-button-to-exp-window nil :text "Done" :x 0 :y (+ 30 (* h 21)) 
                                        :action (lambda () (setf done t))
                                        :height 20 :width 75)))
      (while (not done)
        (process-events))
      
      (remove-items-from-exp-window nil b))
    
    (dotimes (y (gol-board-height *board*))
      (dotimes (x (gol-board-width *board*))
        (let ((cell (aref (gol-board-cells *board*) x y)))
          (modify-button-for-exp-window (gol-cell-button cell) :action "default-button-action")))))
  
  ;; When it's a text display draw the initial board.
  
  (when text
    (with-model game-state (draw-board)))
  
  ;; Give the models their initial input, and schedule a 
  ;; periodic event for the updates starting at the end 
  ;; of this cycle (1 second).
  
  (generate-model-percepts)
  (with-model game-state (schedule-periodic-event 1.0 "update-board" :params (list text) :maintenance t :initial-delay 1.0))
  
  ;; Run the game for the requested number of cycles (simulated seconds)
  
  (run-full-time cycles t))


(defun click-button (x y)
  (bt:with-lock-held ((gol-board-lock *board*))
    (let ((cell (aref (gol-board-cells *board*) x y)))
      (cond ((gol-cell-state cell)
             (setf (gol-cell-state cell) nil)
             (modify-button-for-exp-window (gol-cell-button cell) :color 'gray))
            (t
             (setf (gol-cell-state cell) t)
             (modify-button-for-exp-window (gol-cell-button cell) :color 'red))))))

             

;;; Function to draw the board to the command-trace using *'s and spaces
;;; with lines around it.  Always called from 

(defun draw-board ()
  (bt:with-lock-held ((gol-board-lock *board*))
    (command-output (make-string (+ 2 (gol-board-width *board*)) :initial-element #\_))
    
    (dotimes (y (gol-board-height *board*))
      (let ((line (make-string (+ 2 (gol-board-width *board*)) :initial-element #\|)))
        (dotimes (x (gol-board-width *board*))
          (setf (char line (1+ x)) (if (gol-cell-state (aref (gol-board-cells *board*) x y)) #\* #\space)))
        (command-output line)))
    
    (command-output (make-string (+ 2 (gol-board-width *board*)) :initial-element #\-))))


;;; Function to count the number of neighbors of a cell
;;; using a simple check for each adjacent space which exists.
;;; Only called from generate-model-percepts which holds the
;;; lock already.

(defun count-neighbors (x y)
  (let ((mx (1- (gol-board-width *board*)))
        (my (1- (gol-board-height *board*)))
        (count 0))
    (when (and (> x 0) (> y 0) (gol-cell-state (aref (gol-board-cells *board*) (1- x) (1- y))))
      (incf count))
    (when (and (> y 0) (gol-cell-state (aref (gol-board-cells *board*) x (1- y))))
      (incf count))
    (when (and (< x mx) (> y 0) (gol-cell-state (aref (gol-board-cells *board*) (1+ x) (1- y))))
      (incf count))
    (when (and (> x 0) (gol-cell-state (aref (gol-board-cells *board*) (1- x) y)))
      (incf count))
    (when (and (< x mx) (gol-cell-state (aref (gol-board-cells *board*) (1+ x) y)))
      (incf count))
    (when (and (> x 0) (< y my) (gol-cell-state (aref (gol-board-cells *board*) (1- x) (1+ y))))
      (incf count))
    (when (and  (< y my) (gol-cell-state (aref (gol-board-cells *board*) x (1+ y))))
      (incf count))
    (when (and (< x mx) (< y my) (gol-cell-state (aref (gol-board-cells *board*) (1+ x) (1+ y))))
      (incf count))
    count))

;;; The periodic event scheduled to generate the updates.
;;; It updates the board with all of the models' moves
;;; then redraws it and has each model to reprocess the
;;; visual display.

(defun update-board (text)
  
  (bt:with-lock-held ((gol-board-lock *board*))
    
    (dotimes (y (gol-board-height *board*))
      (dotimes (x (gol-board-width *board*))
        (let ((cell (aref (gol-board-cells *board*) x y)))
          (when (gol-cell-change cell)
            (if (string-equal "on" (gol-cell-change cell))
                (progn
                  (setf (gol-cell-state cell) t)
                  (unless text
                    (modify-button-for-exp-window (gol-cell-button cell) :color 'red)))
              (progn
                (setf (gol-cell-state cell) nil)
                (unless text
                  (modify-button-for-exp-window (gol-cell-button cell) :color 'gray))))
            (setf (gol-cell-change cell) nil))))))
  
  (when text
    (draw-board))
  
  (generate-model-percepts))


;;; Function to create a visicon feature for the cell
;;; for each model or modify that feature if one has
;;; already been created.  The chunk holds the cell
;;; coordinates in the x and y slots (which are set to
;;; be the position slots).  If it is on then the state
;;; slot holds t, otherwise there is no state slot.
;;; The neighbors slot contains the count of neighboring
;;; on cells.

(defun generate-model-percepts ()
  (bt:with-lock-held ((gol-board-lock *board*))
    (dotimes (y (gol-board-height *board*))
      (dotimes (x (gol-board-width *board*))
        (let ((cell (aref (gol-board-cells *board*) x y)))
          (with-model-eval (gol-cell-model cell)
            (if (gol-cell-feature cell)
                (modify-visicon-features (list (gol-cell-feature cell) 'state (gol-cell-state cell) 'neighbors (count-neighbors x y)))
              (setf (gol-cell-feature cell)
                (first (add-visicon-features (list 'isa 'cell 'x x 'y y 'state (gol-cell-state cell) 'neighbors (count-neighbors x y) :x-slot 'x :y-slot 'y)))))))))))
  

