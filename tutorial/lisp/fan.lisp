
(load-act-r-model "ACT-R:tutorial;unit5;fan-model.lisp")

(defvar *person-location-data* '(1.11 1.17 1.22
                                 1.17 1.20 1.22
                                 1.15 1.23 1.36
                                 1.20 1.22 1.26
                                 1.25 1.36 1.29
                                 1.26 1.47 1.47))

(defvar *response*)
(defvar *response-time*)

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


(defun respond-to-key-press (model key)
  (declare (ignore model))
  
  (setf *response-time* (get-time))
  (setf *response* key))

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

