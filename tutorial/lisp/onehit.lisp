
(defun 1hit-bj-number-sims (a b)
  (when (and (numberp a) (numberp b))
    (- (/ (abs (- a b)) (max a b)))))

(add-act-r-command "1hit-bj-number-sims" '1hit-bj-number-sims 
                   "Similarity between numbers for 1-hit blackjack task.")

(load-act-r-model "ACT-R:tutorial;unit5;1hit-blackjack-model.lisp")

(defvar *deck1*)
(defvar *deck2*)
(defvar *opponent-rule*)
(defvar *opponent-feedback*)
(defvar *model-action*)
(defvar *human-action*)
(defvar *opponent-threshold*)
(defvar *key-monitor-installed* nil)

(defun respond-to-keypress (model key)
  (if model
      (setf *model-action* key)
    (setf *human-action* key)))


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

(defun onehit-blocks (blocks block-size) 
  (let (res
        (need-to-remove (add-key-monitor)))    
    (dotimes (i blocks)
      (push (onehit-hands block-size) res))
    (when need-to-remove
      (remove-key-monitor))
    (reverse res)))


(defun game0 ()
  (setf *deck1* 'regular-deck)
  (setf *deck2* 'regular-deck)
  (setf *opponent-rule* 'fixed-threshold)
  (setf *opponent-threshold* 15)
  (setf *opponent-feedback* nil))


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

(defun deal (deck)
  (list (funcall deck)
        (funcall deck)
        (funcall deck)))

(defun score-cards (cards &optional (bust 21))
  (let ((total (apply '+ cards)))
    (dotimes (i (count 1 cards))
      (when (<= (+ total 10) bust)
        (incf total 10)))
    total))
  
(defun compute-outcome (p1cards p2cards &optional (bust 21))
  (let ((p1tot (score-cards p1cards bust))
        (p2tot (score-cards p2cards bust)))
    (if (> p1tot bust) 
        'bust 
      (if (or (> p2tot bust) (> p1tot p2tot)) 
          'win 
        'lose))))
  
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


(defun show-opponent-cards (cards mc1)
  (funcall *opponent-rule* cards mc1))

(defun show-opponent-results (ocards mcards ores mres)
  (when *opponent-feedback* 
    (funcall *opponent-feedback* ocards mcards ores mres)))

(defun regular-deck ()
  (min 10 (1+ (act-r-random 13))))

(defun fixed-threshold (cards mc1)
  (if (< (score-cards cards) *opponent-threshold*) 
      "h"
    "s"))

(defvar *card-list* nil)

(defun always-hit (cards mc1) 
  "h")

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

(defun game1 ()
  (setf *card-list* nil)
  (setf *deck1* 'stacked-deck)
  (setf *deck2* 'stacked-deck)
  (setf *opponent-rule* 'always-hit)
  (setf *opponent-feedback* nil))


(game0)
