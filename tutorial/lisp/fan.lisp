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
; This version of the task presents the probe items in a window
; which the model must read to complete the task.

; Load the corresponding model for the task.

(load-act-r-model "ACT-R:tutorial;unit5;fan-model.lisp")

; Create a variable with the original experiment data.

(defvar *person-location-data* '(1.11 1.17 1.22
                                 1.17 1.20 1.22
                                 1.15 1.23 1.36
                                 1.20 1.22 1.26
                                 1.25 1.36 1.29
                                 1.26 1.47 1.47))

; create variables to hold the model's response and the time of
; that response.

(defvar *response*)
(defvar *response-time*)

; The fan-sentence function takes 4 parameters.
; The first two are the strings of the person and location
; to present.  The third is t or nil to indicate whether
; this was or wasn't in the study set, and the last is
; either the symbol person or location to indicate which
; of the productions the model should use for retrieval.
;
; It presents the probe items given in a window, runs the
; model, and returns a list indicating how many seconds it 
; took to respond (or 30 if no response was made) and t or nil 
; to indicate if the response was correct.

(defun fan-sentence (person location target term)
      
  (reset)
  
  (let ((window (open-exp-window "Sentence Experiment" 
                                 :visible nil
                                 :width 600 
                                 :height 300))
        (x 25))
        
    (install-device window)
        
    (add-act-r-command "fan-response" 'respond-to-key-press "Fan experiment model response")
    (monitor-act-r-command "output-key" "fan-response")
    
    ; disable the production that isn't being used for retrieval
    
    (case term 
      (person (pdisable retrieve-from-location))
      (location (pdisable retrieve-from-person)))
    
    (add-text-to-exp-window window person :x 50 :y 150 :width 75)
    (add-text-to-exp-window window location :x 250 :y 150 :width 75)
    
    (setf *response* nil)
    (setf *response-time* nil)
    
    (run 30)
    
    (remove-act-r-command-monitor "output-key" "fan-response")
    (remove-act-r-command "fan-response")
    
        (if (null *response*)
            (list 30.0 nil)
          (list (/ *response-time* 1000.0)
                (or (and target (string-equal *response* "k"))
                    (and (null target) (string-equal *response* "d")))))))


; respond-to-key-press is set to monitor the output-key command
; and records the time and key that was pressed by the model.

(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (setf *response-time* (get-time))
  (setf *response* key))

; do-person-location requires one parameter which is either
; the symbol person or location to indicate which of the 
; productions the model should use for retrieval.
; It runs one trial of each fan condition and returns a list
; of the results.

(defun do-person-location (term) 
  (let ((results nil))
    
    (dolist (sentence '(("lawyer" "store" t)
                        ("captain" "cave" t)
                        ("hippie" "church" t)
                        ("debutante" "bank" t)
                        ("earl" "castle" t)
                        ("hippie" "bank" t)
                        ("fireman" "park" t)
                        ("captain" "park" t)
                        ("hippie" "park" t)
                        ("fireman" "store" nil)
                        ("captain" "store" nil)
                        ("giant" "store" nil)
                        ("fireman" "bank" nil)
                        ("captain" "bank" nil)
                        ("giant" "bank" nil)
                        ("lawyer" "park" nil)
                        ("earl" "park" nil)
                        ("giant" "park" nil)))
      
      (push (apply 'fan-sentence (append sentence (list term))) results))
    
    (reverse results)))

; fan-experiment runs the model through one trial of 
; each condition using each of the retrieval productions
; and averages the results then displays the results.

(defun fan-experiment ()
  (output-person-location (mapcar (lambda (x y) 
                                    (list (/ (+ (first x) (first y)) 2.0) 
                                          (and (second x) (second y))))
                            (do-person-location 'person) 
                            (do-person-location 'location))))

(defun output-person-location (data)
  (let ((rts (mapcar 'first data)))
    (correlation rts *person-location-data*)
    (mean-deviation rts *person-location-data*)
    
    (format t "TARGETS:~%                         Person fan~%")
    (format t "  Location      1             2             3~%")
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

