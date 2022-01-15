; ACT-R tutorial unit 5 fan task.
; This experiment presents a model with a person-location pair
; of items and the model must respond whether that pair of items
; was part of the study set that it has recorded in memory.
; The task and data to which the model is fit are in the paper:
;
; Anderson, J. R. (1974). Retrieval of propositional information from
; long-term memory. Cognitive Psychology, 5, 451 - 474.
;
; The results are reported are the time to respond to the probe
; based on the 'fan' of the items presented (how many places a person
; is in or how many people are in the place) and whether the probe
; is or isn't in the test set.
;
; This version of the task does not use the perceptual or motor
; modules of ACT-R and instead places the probes directly into
; slots of the goal buffer and reads the model's response from
; a slot of the goal buffer when done.

; Load the corresponding model for the task.

(load-act-r-model "ACT-R:tutorial;unit5;fan-no-pm-model.lisp")

; Create a variable with the original experiment data.

(defvar *person-location-data* '(1.11 1.17 1.22
                                 1.17 1.20 1.22
                                 1.15 1.23 1.36
                                 1.20 1.22 1.26
                                 1.25 1.36 1.29
                                 1.26 1.47 1.47))

; The fan-sentence function takes 4 parameters.
; The first two are the strings of the person and location
; to present.  The third is t or nil to indicate whether
; this was or wasn't in the study set, and the last is
; either the symbol person or location to indicate which
; of the productions the model should use for retrieval.
;
; It presents the probe items given directly to the model
; through the goal buffer, runs the model, and
; returns a list indicating how many seconds the model
; ran and t or nil to indicate if the response was correct.

(defun fan-sentence (person location target term)
  (reset)
  
  ; disable the production that isn't being used for retrieval
  
  (case term 
    (person (pdisable retrieve-from-location))
    (location (pdisable retrieve-from-person)))
  
  ; modify the chunk named goal (which will be set in the goal buffer
  ; when the model runs) to set the arg1 and arg2 slots to the probe
  ; items and state slot to test
  
  (mod-chunk-fct 'goal (list 'arg1 person 'arg2 location 'state 'test))
  
  ; run the model recording the time spent running
  ; and get the value from the state slot of the goal buffer representing the
  ; model's response to the task
  
  (let ((response-time (run 30.0))
        (response (buffer-slot-value 'goal 'state)))
      
    (list response-time
          (or (and target (string-equal response "k"))
              (and (null target) (string-equal response "d"))))))

; do-person-location requires one parameter which is either
; the symbol person or location to indicate which of the 
; productions the model should use for retrieval.
; It runs one trial of each fan condition and returns a list
; of the results.

(defun do-person-location (term) 
  (let ((test-set '(("lawyer" "store" t)("captain" "cave" t)("hippie" "church" t)
                    ("debutante" "bank" t)("earl" "castle" t)("hippie" "bank" t)
                    ("fireman" "park" t)("captain" "park" t)("hippie" "park" t)
                    ("fireman" "store" nil)("captain" "store" nil)
                    ("giant" "store" nil)("fireman" "bank" nil)
                    ("captain" "bank" nil)("giant" "bank" nil)
                    ("lawyer" "park" nil)("earl" "park" nil)
                    ("giant" "park" nil)))
        (results nil))
    
    (dolist (sentence test-set)
      (push (apply 'fan-sentence (append sentence (list term))) results))
    
    (reverse results)))

; fan-experiment runs the model through one trial of 
; each condition using each of the retrieval productions
; and averages the results then displays the results.

(defun fan-experiment ()
  (output-person-location (mapcar (lambda (x y) 
                                    (list (/ (+ (car x) (car y)) 2.0) 
                                          (and (cadr x) (cadr y))))
                            (do-person-location 'person) 
                            (do-person-location 'location))))

(defun output-person-location (data)
  (let ((rts (mapcar 'first data)))
    (correlation rts *person-location-data*)
    (mean-deviation rts *person-location-data*)
    (format t "~%TARGETS:~%                         Person fan~%")
    (format t  "  Location      1             2             3~%")
    (format t "    fan")
    
    (dotimes (i 3)
      (format t "~%     ~d    " (1+ i))
      (dotimes (j 3)
        (format t "~{~8,3F (~3s)~}" (nth (+ j (* i 3)) data))))
    
    (format t "~%~%FOILS:")
    (dotimes (i 3)
      (format t "~%     ~d    " (1+ i))
      (dotimes (j 3)
        (format t "~{~8,3F (~3s)~}" (nth (+ j (* (+ i 3) 3)) data))))))
