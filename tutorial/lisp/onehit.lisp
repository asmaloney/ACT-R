; ACT-R tutorial unit 5 one-hit blackjack task
;
; This file implements the one-hit blackjack game
; that is described in the unit text and allows
; one to run a model against a human opponent or
; an opponent controlled by functions created
; to play the game.  It also allows one to control
; the decks of cards that are used to provide 
; different situations for the model to learn.


; Before loading the model define the function
; that will be used to compute the similarities
; between numbers and add a command for it because
; that command name is used in the model's parameter
; settings.

(defun 1hit-bj-number-sims (a b)
  (when (and (numberp a) (numberp b))
    (- (/ (abs (- a b)) (max a b)))))

(add-act-r-command "1hit-bj-number-sims" '1hit-bj-number-sims 
                   "Similarity between numbers for 1-hit blackjack task.")

(load-act-r-model "ACT-R:tutorial;unit5;1hit-blackjack-model.lisp")


; Define a lot of global variables to control the
; details of the game, a code-based opponent, 
; record responses, and keep track of whether the
; output-key action is being monitored.

(defvar *deck1*)
(defvar *deck2*)
(defvar *opponent-rule*)
(defvar *opponent-feedback*)
(defvar *model-action*)
(defvar *human-action*)
(defvar *opponent-threshold*)
(defvar *key-monitor-installed* nil)


; respond-to-keypress will be monitoring the
; output-key command and will be called when a 
; key is pressed by the model or a human playing
; the game.  It records the key in the corresponding
; variable based on who made it.

(defun respond-to-keypress (model key)
  (if model
      (setf *model-action* key)
    (setf *human-action* key)))

; These functions are used to create the command and monitor
; output-key and correspondingly remove the monitor and command
; when needed because it is more efficient to do so once instead
; of on each trial (as has been done for most prior tasks) since
; this will require running many trials to collect the data.

(defun add-key-monitor ()
  (unless *key-monitor-installed*
    (add-act-r-command "1hit-bj-key-press" 'respond-to-keypress 
                       "1-hit blackjack task key output monitor")
    (monitor-act-r-command "output-key" "1hit-bj-key-press")
    (setf *key-monitor-installed* t)))

(defun remove-key-monitor ()
  (remove-act-r-command-monitor "output-key" "1hit-bj-key-press")
  (remove-act-r-command "1hit-bj-key-press")
  (setf *key-monitor-installed* nil))


; onehit-hands takes one required parameter which is a number of
; hands to play and an optional parameter which if specified as
; non-nil will print out the details of the hands.
; It plays the game based on the settings of the global varaibles
; for the decks of cards and opponent functions and returns the
; list of results.

(defun onehit-hands (hands &optional (print-game nil))
  (let ((scores (list 0 0 0 0))
        (need-to-remove (add-key-monitor)))
        
    (dotimes (i hands)
      (let* ((mcards (deal *deck1*))
             (ocards (deal *deck2*))
             (mchoice (show-model-cards (butlast mcards) (first ocards)))
             (ochoice (show-opponent-cards (butlast ocards) (first mcards))))
        
        (unless (string-equal "h" mchoice) 
          (setf mcards (butlast mcards)))
        (unless (string-equal "h" ochoice) 
          (setf ocards (butlast ocards)))
        
        (let* ((mtot (score-cards mcards))
               (otot (score-cards ocards))
               (mres (compute-outcome mcards ocards))
               (ores (compute-outcome ocards mcards)))
          
          (show-model-results mcards ocards mres ores)
          (show-opponent-results ocards mcards ores mres)
          
          (when print-game
            (format t "Model: ~{~2d ~} -> ~2d (~4s)   Opponent: ~{~2d ~}-> ~2d (~4s)~%"
              mcards mtot mres ocards otot ores))
          
          (setf scores (mapcar '+ scores
                         (list (if (eq mres 'win) 1 0)
                               (if (eq ores 'win) 1 0)
                               (if (and (eq mres 'bust) (eq ores 'bust)) 1 0)
                               (if (and (= mtot otot) 
                                        (not (eq mres 'bust)) 
                                        (not (eq ores 'bust))) 1 0)))))))
    
    (when need-to-remove
      (remove-key-monitor))
    
    scores))

; onehit-blocks takes two required parameters which are how many 
; blocks of hands to play and how many hands are in a block.
; It plays the specified number of blocks and returns the list
; of results by block.

(defun onehit-blocks (blocks block-size) 
  (let (res
        (need-to-remove (add-key-monitor)))    
    (dotimes (i blocks)
      (push (onehit-hands block-size) res))
    (when need-to-remove
      (remove-key-monitor))
    (reverse res)))

; game0 function sets the global variables to configure
; the default game -- the regular distribution of cards
; in a deck and an opponent which has a fixed threshold
; of 15 for deciding whether to hit or stay and which does
; not process the feedback.

(defun game0 ()
  (setf *deck1* 'regular-deck)
  (setf *deck2* 'regular-deck)
  (setf *opponent-rule* 'fixed-threshold)
  (setf *opponent-threshold* 15)
  (setf *opponent-feedback* nil))


; onehit-learning requires one paramter which is how many
; 100 hand games to play.  There are two optional paramters
; which indicate whether a graph of the results should be 
; drawn in an experiment window (default is t which draws it)
; and to specify a function to use to set the variables that
; configure the game play (the default is game0).
; It returns a list with the average win percentages from
; the n games in both blocks of 20 and blocks of 5 hands.

(defun onehit-learning (n &optional (graph t) (game 'game0))
  (let ((data nil)
        (need-to-remove (add-key-monitor)))
    (dotimes (i n)
      (reset)
      (funcall game)
      (if (null data)
          (setf data (onehit-blocks 20 5))
        (setf data (mapcar (lambda (x y) 
                             (mapcar '+ x y)) 
                     data (onehit-blocks 20 5)))))
    
    (when need-to-remove
      (remove-key-monitor))
    
    (let ((percentages (mapcar (lambda (x) (/ (car x) n 5.0)) data)))
      (when graph
        (draw-graph percentages))
      
      (list (list (/ (apply '+ (subseq percentages 0 5)) 5)
                  (/ (apply '+ (subseq percentages 5 10)) 5)
                  (/ (apply '+ (subseq percentages 10 15)) 5)
                  (/ (apply '+ (subseq percentages 15 20)) 5))
                  percentages))))

; draw-graph takes a list of percentages and displays them in
; a graph using an experiment window for output.

(defun draw-graph (points)
  (let ((w (open-exp-window "Data" :visible t :width 550 :height 460)))
    (add-line-to-exp-window w '(50 0) '(50 420) 'white)
    (dotimes (i 11)
      (add-text-to-exp-window w (format nil "~3,1f" (- 1 (* i .1))) 
                              :x 5 :y (+ 5 (* i 40)) :width 35)
      (add-line-to-exp-window w (list 45 (+ 10 (* i 40))) 
                              (list 550 (+ 10 (* i 40))) 'white))
    
    (let ((x 50))
      (mapcar (lambda (a b) 
                (add-line-to-exp-window w (list x (floor (- 410 (* a 400))))
                                        (list (incf x 25) (floor (- 410 (* b 400))))
                                        'blue))
        (butlast points) (cdr points)))))

; deal takes a deck function and returns a list of
; the next three cards that it returns when called.

(defun deal (deck)
  (list (funcall deck)
        (funcall deck)
        (funcall deck)))


; score-cards takes a list of cards and an optional value
; indicating the number over which a hand busts (defaults to 21).
; It returns the total value of those cards treating 1s as 11 
; if possible without busting.

(defun score-cards (cards &optional (bust 21))
  (let ((total (apply '+ cards)))
    (dotimes (i (count 1 cards))
      (when (<= (+ total 10) bust)
        (incf total 10)))
    total))


; compute-outcome takes a list of cards for each player and an
; optional value indicating the number over which a hand busts.
; It computes the total for each hand of cards and returns the
; result (win, lose, or bust) for the first list of cards.

(defun compute-outcome (p1cards p2cards &optional (bust 21))
  (let ((p1tot (score-cards p1cards bust))
        (p2tot (score-cards p2cards bust)))
    (if (> p1tot bust) 
        'bust 
      (if (or (> p2tot bust) (> p1tot p2tot)) 
          'win 
        'lose))))

; show-model-cards takes two parameters. The first is a list of
; the model's starting cards and the second is the opponent's face
; up card.  If there is a chunk in the model's goal buffer it is
; modified to the initial state of the game.  If there is not a
; chunk in the goal buffer then a new chunk is created and placed
; into the buffer.  Then the model is run for exactly 10 seconds
; and any response it made is returned.

(defun show-model-cards (mcards ocard)
  (if (buffer-read 'goal)
      (mod-focus-fct `(mc1 ,(first mcards) mc2 ,(second mcards) mc3 nil 
                       mtot nil mstart ,(score-cards mcards) mresult nil
                       oc1 ,ocard oc2 nil oc3 nil otot nil 
                       ostart ,(score-cards (list ocard)) oresult nil 
                       state start))
    (goal-focus-fct (car (define-chunks-fct 
                             `((isa game-state mc1 ,(first mcards) 
                                mc2 ,(second mcards) 
                                mstart ,(score-cards mcards)
                                oc1 ,ocard ostart ,(score-cards (list ocard)) 
                                state start))))))
  (setf *model-action* nil)
  (run-full-time 10)
  *model-action*)

; show-model-results takes four parameters. The first is a list of
; the model's final cards and the second is the list of the opponent's
; final cards.  The third is the model's end result and the fourth is
; the opponents end result. 
; If there is a chunk in the model's goal buffer it is modified to
; the results state of the game with all the information.  If there
; is not a chunk in the goal buffer then a new chunk is created with
; the results information and placed into the buffer.  Then the model
; is run for exactly 10 seconds.

(defun show-model-results (mcards ocards mres ores)
  (if (buffer-read 'goal)
      (mod-focus-fct `(mc1 ,(first mcards)  mc2 ,(second mcards) 
                       mc3 ,(third mcards) mtot ,(score-cards mcards)
                       mstart ,(score-cards (subseq mcards 0 2)) 
                       mresult ,mres oc1 ,(first ocards) 
                       oc2 ,(second ocards) oc3 ,(third ocards) 
                       otot ,(score-cards ocards) 
                       ostart ,(score-cards (list (first ocards))) 
                       oresult ,ores state results))
    (goal-focus-fct (car (define-chunks-fct 
                             `((isa game-state mc1 ,(first mcards)  
                                mc2 ,(second mcards) mc3 ,(third mcards) 
                                mtot ,(score-cards mcards) 
                                mstart ,(score-cards (subseq mcards 0 2)) 
                                mresult ,mres oc1 ,(first ocards) 
                                oc2 ,(second ocards) oc3 ,(third ocards) 
                                otot ,(score-cards ocards) 
                                ostart ,(score-cards (list (first ocards))) 
                                oresult ,ores state results))))))
  (run-full-time 10))


; play-human takes two parameters. The first is the list of
; the player's cards and the other is the model's face up card.
; It opens an experiment window to display that information to
; a person and waits exactly 10 seconds before continuing the
; game. It returns the key press the player made, or "s" (stay)
; if no key was pressed.

(defun play-human (cards oc1)
  (let ((win (open-exp-window "Human")))
    (add-text-to-exp-window win "You" :x 50 :y 20)
    (add-text-to-exp-window win "Model" :x 200 :y 20)
    (dotimes (i 2)
      (dotimes (j 3)
        (add-text-to-exp-window win (format nil "C~d" (1+ j)) 
                                :x (+ 25 (* j 30) (* i 150)) :y 40 :width 20)
        (cond ((and (zerop i) (< j 2))
               (add-text-to-exp-window win (princ-to-string (nth j cards)) 
                                       :x (+ 25 (* j 30) (* i 150)) :y 60 :width 20))
              ((and (= i 1) (zerop j))
               (add-text-to-exp-window win (princ-to-string oc1) 
                                       :x (+ 25 (* j 30) (* i 150)) :y 60 :width 20)))))
    (setf *human-action* nil)
    
    (let ((start-time (get-time nil)))
      (while (< (- (get-time nil) start-time) 10000)
        (process-events)))
    
    (if *human-action*
        *human-action*
      "s")))

; show-human-results takes four parameters. The first is a list of
; the player's final cards and the second is the list of the model's
; final cards.  The third is the player's end result and the fourth is
; the model's end result. 
; All of the cards and outcomes are displayed in an experiment
; window and it waits 10 seconds before continuing.

(defun show-human-results (own-cards others-cards own-result others-result)
  (let ((win (open-exp-window "Human")))
    (add-text-to-exp-window win "You" :x 50 :y 20)
    (add-text-to-exp-window win "Model" :x 200 :y 20)
    (dotimes (i 2)
      (dotimes (j 3)
        (add-text-to-exp-window win (format nil "C~d" (1+ j)) 
                                :x (+ 25 (* j 30) (* i 150)) :y 40 :width 20)
        (if (zerop i)
            (when (nth j own-cards)
              (add-text-to-exp-window win (princ-to-string (nth j own-cards)) 
                                      :x (+ 25 (* j 30) (* i 150)) :y 60 :width 20))
            (when (nth j others-cards)
              (add-text-to-exp-window win (princ-to-string (nth j others-cards)) 
                                      :x (+ 25 (* j 30) (* i 150)) :y 60 :width 20)))))
    (add-text-to-exp-window win (princ-to-string own-result) :x 50 :y 85)
    (add-text-to-exp-window win (princ-to-string others-result) :x 200 :y 85)
    (let ((start-time (get-time nil)))
      (while (< (- (get-time nil) start-time) 10000)
        (process-events)))))

; play-against-model requries one parameter which is how many
; hands to play and an optional parameter indicating whether 
; the hand information should be printed.  It sets the global
; variables to those needed to have a person play against the
; model and then runs for the indicated number of hands.  After
; that, it sets the variables back to the values they had
; before.

(defun play-against-model (count &optional (print-game nil))
  (if (visible-virtuals-available?)
      (let* ((old-rule *opponent-rule*)
             (old-feedback *opponent-feedback*)
             (*opponent-rule* 'play-human)
             (*opponent-feedback* 'show-human-results))
        (unwind-protect
            (onehit-hands count print-game)
          (progn
            (setf *opponent-rule* old-rule)
            (setf *opponent-feedback* old-feedback))))
    (print-warning "Cannot play against the model without a visible window available.")))

; show-opponent-cards and show-opponent-results are used
; by the game code to call the appropriate function for
; the non-model player to receive the game information.

(defun show-opponent-cards (cards mc1)
  (funcall *opponent-rule* cards mc1))

(defun show-opponent-results (ocards mcards ores mres)
  (when *opponent-feedback* 
    (funcall *opponent-feedback* ocards mcards ores mres)))

; The functions below are used to create the game0 and game1
; situations.


; regular-deck takes no parameters and returns a number 
; between 1 and 10 with 10s being 4 times as likely as 
; other numbers.  This is used as the deck function for
; both players in game0.

(defun regular-deck ()
  (min 10 (1+ (act-r-random 13))))

; fixed-threshold implements a rule for an opponent 
; that will always hit below a fixed threshold. It
; is used in game0 for the opponent.

(defun fixed-threshold (cards mc1)
  (if (< (score-cards cards) *opponent-threshold*) 
      "h"
    "s"))

; always-hit implements a rule for an opponent
; that will always hit. It is used for the opponent
; in game1.

(defun always-hit (cards mc1) 
  "h")

; Create a variable for a list of cards, and
; a function that will place the 6 cards onto 
; that list for the player decks (the first 3
; cards are the model's and the next 3 are for
; the opponent). That deck function is used for
; both players and represents the situation in
; game1 where the opponent's face up card is
; a perfect predictor for the action the model
; needs to take to win.

(defvar *card-list* nil)

(defun load-stacked-deck ()
  (let* ((c1 (+ 5 (act-r-random 6)))
         (c2 (+ 7 (act-r-random 4)))
         (c4 (if (> (act-r-random 1.0) .5) 2 8))
         (c3 (if (= c4 2) 10 (- 21 (+ c1 c2))))
         (c5 10)
         (c6 (if (= c4 2) 10 2)))
    (list c1 c2 c3 c4 c5 c6)))

(defun stacked-deck ()
  (cond (*card-list* (pop *card-list*))
        (t (setf *card-list* (load-stacked-deck)) 
           (pop *card-list*))))

; function to set variables to the values needed to
; implement game1

(defun game1 ()
  (setf *card-list* nil)
  (setf *deck1* 'stacked-deck)
  (setf *deck2* 'stacked-deck)
  (setf *opponent-rule* 'always-hit)
  (setf *opponent-feedback* nil))

; call game0 to set the initial game variable values.

(game0)
