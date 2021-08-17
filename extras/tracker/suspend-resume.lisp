#|
This file contains the same experiment and basic model as the simple.lisp example.

However, in this model it is also going to suspend and resume the tracker, create
additional trackers, and also query the tracker information to demonstrate those
capabilities.

|#


(defvar *target* nil)
(defvar *feature* nil)

(defun response (model key)
  (declare (ignore model))
  (if (equal key *target*)
      (modify-visicon-features (list *feature* 'prompt 'correct))
    (modify-visicon-features (list *feature* 'prompt 'wrong)))
  (schedule-event-relative 5 'choose :output nil :maintenance t))

(defun choose ()
  (modify-visicon-features (list *feature* 'prompt 'choose)))
       
(defun run-test (time &optional (value 2))
  
  (if (< 0 value 4)
      (setf *target* (princ-to-string value))
    (setf *target* "2"))
  
  (reset)
  (install-device (list "motor" "keyboard"))
  (add-act-r-command "response" 'response "Process key response")
  (monitor-act-r-command "output-key" "response")
  
  (setf *feature* (car (add-visicon-features '(screen-x 100 screen-y 100 prompt choose))))
  
  (run time)
  
  (remove-act-r-command-monitor "output-key" "response")
  (remove-act-r-command "response"))
  

(clear-all)

(require-extra "tracker")

(define-model simple-tracker-test
    
  (sgp :v t :trace-tracker t :trace-detail high)
  
  ; Seed which generates the test
   (sgp :seed (103 0))
  
  ;; create chunk-types for the goal, imaginal, and visual-location chunks
  
  (chunk-type guess value state test name)
  (chunk-type result good bad)
  (chunk-type prompt screen-x screen-y prompt)
  
  ;; avoid warnings for the slots in productions that are modified and untested or vice-versa
  
  (declare-buffer-usage goal guess value)
  (declare-buffer-usage imaginal result :all)
  
  ;; Create the chunks that are used to name the slots and 
  ;; provide the visual prompts.
  
  (define-chunks good bad value choose wrong correct)
  
  ;; Have any unattended visual chunk stuffed into the buffer.
  ;; This model doesn't need to actually attend to do this task.
  
  (set-visloc-default :attended nil)
  
  ;; Create the tracker and set the initial chunks in the
  ;; goal and imagial buffers.  The tracker will set the
  ;; value slot of the goal, and watch the good and bad
  ;; slots of the imaginal buffer, to determine the best
  ;; value among 1, 2, and 3.
  ;; Also start a temporal timer to determine when to
  ;; interrupt and perform the additional features for this
  ;; demonstration.
  
  (p start
     ?goal>
       buffer empty
     ?imaginal>
       state free
       buffer empty
     ?tracker>
       state free
     ==>
     +tracker>
       control-slot value
       good-slot good
       bad-slot bad
       min 1
       max 3
       control-count 3
     +imaginal>
     +goal>
     +temporal> isa time)
  
  ;; When the choose prompt is provided press the key
  ;; for the value that the tracker has currently set.
  
  (p guess
     =goal>
       value =val
       state nil
     =visual-location>
       prompt choose 
     ?manual>
       state free
    ==>
     +manual>
       isa press-key
       key =val)
  
  ;; If the feedback indicates a wrong answer modify the 
  ;; bad slot of imaginal for the tracker to record.
  
  (p wrong
     =goal>
     =imaginal>
     =visual-location>
       prompt wrong 
     ==>
     =imaginal>
       bad 1
     =goal>
       state t)
  
  ;; If the feedback indicates a correct answer modify the 
  ;; good slot of imaginal for the tracker to record.

  (p correct
     =goal>
     =imaginal>
     =visual-location>
       prompt correct 
     ==>
     =imaginal>
       good 1
     =goal>
       state t)
  
  (p continue
     =goal>
       state t
     =temporal>
     < ticks 60
     ==>
     =goal>
     state nil)
  
  (p suspend-only-tracker
     =goal>
       state t
       test nil
     ?tracker>
       exists value
       active value
     =temporal>
     >= ticks 60
     =tracker>
       control-slot value
     ==>
     =goal>
       state nil
       test 1
     +temporal> isa time
     +tracker>
       isa suspend-tracker
       control-slot value    
     )
  
  (p doesnt-match-1
     =goal>
     state t
     test nil
     ?tracker>
     - exists value
     =temporal>
     >= ticks 60
     ==>
     )
  
  (p doesnt-match-2
     =goal>
     state t
     test nil
     ?tracker>
     - active value
     =temporal>
     >= ticks 60
     ==>
     )
  
  (spp (doesnt-match-1 doesnt-match-2) :u 3)
  
  (p resume-only-tracker
     =goal>
     state t
     test 1
     ?tracker>
     exists value
     - active value
     
     =temporal>
     >= ticks 60
     ==>
     =goal>
     state nil
     test 2
     +temporal> isa time
     +tracker>
       isa resume-tracker
       control-slot value    
     )
  (p doesnt-match-3
     =goal>
     state t
     test 1
     ?tracker>
     - exists value
     =temporal>
     >= ticks 60
     ==>
     )
  
  (p doesnt-match-4
     =goal>
     state t
     test 1
     ?tracker>
     active value
     =temporal>
     >= ticks 60
     ==>
     )
  
  (spp (doesnt-match-3 doesnt-match-4) :u 3)
  
  (p implicit-suspend-at-creation
     =goal>
     state t
     test 2
     
     ?tracker>
     exists value
     active value
     =tracker>
     control-slot value
     name =name
     =temporal>
     >= ticks 60
     ==>
     =goal>
     state nil
     test 3
     name =name
     +temporal> isa time
     +tracker>
     control-slot value
      good-slot good
      bad-slot bad
      min 0
       max 5
     control-count 6
     name new-tracker
     )
  

  
    (p implicit-suspend-at-resume
     =goal>
     state t
     test 3
     name =name
     =temporal>
     >= ticks 60
     ==>
     =goal>
     state nil
     test 4
     +temporal> isa time
     +tracker>
       isa resume-tracker
       name =name
     )
  
  
      (p explicit-suspend-by-name
     =goal>
     state t
     test 4
     name =name
     =temporal>
     >= ticks 60
     ==>
     =goal>
     state nil
     test 5
     +temporal> isa time
     +tracker>
       isa suspend-tracker
       name =name
     )
  
  
      (p failed-suspend-by-name
     =goal>
     state t
     test 5
     name =name
     =temporal>
     >= ticks 20
     ==>
     =goal>
     state nil
     test 6
     +temporal> isa time
     +tracker>
       isa suspend-tracker
       name =name
     )
  
  
  
      (p failed-suspend-by-slot
     =goal>
     state t
     test 6
     name =name
     =temporal>
     >= ticks 20
     ==>
     =goal>
     state nil
     test 7
     +temporal> isa time
     +tracker>
       isa suspend-tracker
       control-slot value
     )
  
  

  
     (p failed-resume-1
     =goal>
     state t
     test 7
     name =name
     =temporal>
     >= ticks 20
     ==>
     =goal>
     state nil
     test 9
     +temporal> isa time
     +tracker>
        isa resume-tracker
        )
  
       (p failed-resume-2
     =goal>
     state t
     test 9
     name =name
     =temporal>
     >= ticks 20
     ==>
     =goal>
     state nil
     test 10
     +temporal> isa time
     +tracker>
          isa resume-tracker
          name foo
     )
  
       (p failed-resume-3
     =goal>
     state t
     test 10
     name =name
     =temporal>
     >= ticks 20
     ==>
     =goal>
     state nil
     test 11
     +temporal> isa time
     +tracker>
          isa resume-tracker
          control-slot color
     )
  
         (p failed-resume-4
     =goal>
     state t
     test 11
     name =name
     =temporal>
     >= ticks 20
     ==>
     =goal>
     state nil
     test 12
     +temporal> isa time
     +tracker>
          isa resume-tracker
          control-slot value
     )
  
  
  
  )

