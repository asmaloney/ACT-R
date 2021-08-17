
(load-act-r-model "ACT-R:tutorial;unit5;fan-no-pm-model.lisp")

(defvar *person-location-data* '(1.11 1.17 1.22
                                 1.17 1.20 1.22
                                 1.15 1.23 1.36
                                 1.20 1.22 1.26
                                 1.25 1.36 1.29
                                 1.26 1.47 1.47))

(defun fan-sentence (person location target term)
  (reset)
  
  (case term 
    (person (pdisable retrieve-from-location))
    (location (pdisable retrieve-from-person)))
  
  (mod-chunk-fct 'goal (list 'arg1 person 'arg2 location 'state 'test))
  
  (let ((response-time (run 30.0))
        (response (chunk-slot-value-fct (buffer-read 'goal) 'state)))
      
    (list response-time
          (or (and target (string-equal response "k"))
              (and (null target) (string-equal response "d"))))))



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
