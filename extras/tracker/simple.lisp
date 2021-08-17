#|
This file contains a simple experiment and a model which uses a
tracker to learn the correct response to the experiment.

In the experiment the model is to press one of three keys, 1, 2, or 3
when prompted to choose.  It will then be provided feedback of whether
that answer was correct or not and given 5 seconds to process that 
before the next choose prompt.  One of the choices will always be the
correct answer and the other two will always be wrong, and the model
is trying to learn the correct choice.

This is not the type of task for which the tracker is intended because
the existing declarative or procedural learning approaches could easily 
be applied to learn this simple choice response since the responses
are discrete and there is no ambiguity in credit assignment from the
feedback, but by using such a simple task the operation of the tracker
should be easy to follow for example purposes.


To run the task call run-test and provide the length of time (in seconds)
to have the model perform the task, and optionally provide the correct
response (which defaults to 2).

There is an example run provided at the end of this file.

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
  
  ; Seed which generated the test run below
  ; (sgp :seed (57531033409 0))
  
  ;; create chunk-types for the goal, imaginal, and visual-location chunks
  
  (chunk-type guess value)
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
     +goal>)
  
  ;; When the choose prompt is provided press the key
  ;; for the value that the tracker has currently set.
  
  (p guess
     =goal>
       value =val
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
     =imaginal>
     =visual-location>
       prompt wrong 
     ==>
     =imaginal>
       bad 1)
  
  ;; If the feedback indicates a correct answer modify the 
  ;; good slot of imaginal for the tracker to record.

  (p correct
     =imaginal>
     =visual-location>
       prompt correct 
     ==>
     =imaginal>
       good 1)
  )



#|

When a tracker is created it will create an initial guess 
for the value.  That guess will be used until an update to
the result is made, at which time a new guess will be chosen
and used until the next update.  Those updates are not
associated with the actions or feedback, and just occur
after a randomly chosen update period passes (by default
that is from an exponential distribution with a mean of 10s
bounded by 1s and 60s).

> (run-test 100)
     0.000   VISION                 PROC-DISPLAY
     0.000   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
     0.000   VISION                 visicon-update
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED START
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION IMAGINAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION TRACKER
     0.050   PROCEDURAL             PRODUCTION-FIRED START
     0.050   PROCEDURAL             MODULE-REQUEST TRACKER
Tracker TRACKER0 generating value for buffer GOAL slot VALUE with temperature 1.0:
  Choices: 1.000 2.000 3.000
  Probs:   0.333 0.333 0.333
 Chosen result is 1.0
     0.050   PROCEDURAL             MODULE-REQUEST IMAGINAL
     0.050   PROCEDURAL             MODULE-REQUEST GOAL
     0.050   PROCEDURAL             CLEAR-BUFFER TRACKER
     0.050   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.050   PROCEDURAL             CLEAR-BUFFER GOAL
     0.050   GOAL                   CREATE-NEW-BUFFER-CHUNK GOAL
     0.050   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK2
     0.050   TRACKER                MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-SELECTED GUESS
     0.050   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.100   PROCEDURAL             PRODUCTION-FIRED GUESS
     0.100   PROCEDURAL             MODULE-REQUEST MANUAL
     0.100   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.100   MOTOR                  PRESS-KEY KEY 1
     0.100   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   IMAGINAL               CREATE-NEW-BUFFER-CHUNK IMAGINAL
     0.250   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK3
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  PREPARATION-COMPLETE
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   MOTOR                  INITIATION-COMPLETE
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   KEYBOARD               output-key SIMPLE-TRACKER-TEST 1
     0.500   VISION                 PROC-DISPLAY
     0.500   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
     0.500   VISION                 visicon-update
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   PROCEDURAL             PRODUCTION-SELECTED WRONG
     0.500   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
     0.500   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.550   PROCEDURAL             PRODUCTION-FIRED WRONG
     0.550   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
     0.550   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  FINISH-MOVEMENT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     5.500   VISION                 PROC-DISPLAY
     5.500   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
     5.500   VISION                 visicon-update
     5.500   PROCEDURAL             CONFLICT-RESOLUTION
     5.500   PROCEDURAL             PRODUCTION-SELECTED GUESS
     5.500   PROCEDURAL             BUFFER-READ-ACTION GOAL
     5.500   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     5.500   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     5.550   PROCEDURAL             PRODUCTION-FIRED GUESS
     5.550   PROCEDURAL             MODULE-REQUEST MANUAL
     5.550   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     5.550   PROCEDURAL             CLEAR-BUFFER MANUAL
     5.550   MOTOR                  PRESS-KEY KEY 1
     5.550   MOTOR                  PREPARATION-COMPLETE
     5.550   PROCEDURAL             CONFLICT-RESOLUTION
     5.600   MOTOR                  INITIATION-COMPLETE
     5.600   PROCEDURAL             CONFLICT-RESOLUTION
     5.700   KEYBOARD               output-key SIMPLE-TRACKER-TEST 1
     5.700   VISION                 PROC-DISPLAY
     5.700   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
     5.700   VISION                 visicon-update
     5.700   PROCEDURAL             CONFLICT-RESOLUTION
     5.700   PROCEDURAL             PRODUCTION-SELECTED WRONG
     5.700   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
     5.700   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     5.750   PROCEDURAL             PRODUCTION-FIRED WRONG
     5.750   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
     5.750   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     5.750   PROCEDURAL             CONFLICT-RESOLUTION
     5.850   MOTOR                  FINISH-MOVEMENT
     5.850   PROCEDURAL             CONFLICT-RESOLUTION
    10.700   VISION                 PROC-DISPLAY
    10.700   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    10.700   VISION                 visicon-update
    10.700   PROCEDURAL             CONFLICT-RESOLUTION
    10.700   PROCEDURAL             PRODUCTION-SELECTED GUESS
    10.700   PROCEDURAL             BUFFER-READ-ACTION GOAL
    10.700   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    10.700   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    10.750   PROCEDURAL             PRODUCTION-FIRED GUESS
    10.750   PROCEDURAL             MODULE-REQUEST MANUAL
    10.750   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    10.750   PROCEDURAL             CLEAR-BUFFER MANUAL
    10.750   MOTOR                  PRESS-KEY KEY 1
    10.750   MOTOR                  PREPARATION-COMPLETE
    10.750   PROCEDURAL             CONFLICT-RESOLUTION
    10.800   MOTOR                  INITIATION-COMPLETE
    10.800   PROCEDURAL             CONFLICT-RESOLUTION
    10.900   KEYBOARD               output-key SIMPLE-TRACKER-TEST 1
    10.900   VISION                 PROC-DISPLAY
    10.900   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    10.900   VISION                 visicon-update
    10.900   PROCEDURAL             CONFLICT-RESOLUTION
    10.900   PROCEDURAL             PRODUCTION-SELECTED WRONG
    10.900   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    10.900   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    10.950   PROCEDURAL             PRODUCTION-FIRED WRONG
    10.950   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
    10.950   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    10.950   PROCEDURAL             CONFLICT-RESOLUTION
    11.050   MOTOR                  FINISH-MOVEMENT
    11.050   PROCEDURAL             CONFLICT-RESOLUTION
    15.900   VISION                 PROC-DISPLAY
    15.900   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    15.900   VISION                 visicon-update
    15.900   PROCEDURAL             CONFLICT-RESOLUTION
    15.900   PROCEDURAL             PRODUCTION-SELECTED GUESS
    15.900   PROCEDURAL             BUFFER-READ-ACTION GOAL
    15.900   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    15.900   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    15.950   PROCEDURAL             PRODUCTION-FIRED GUESS
    15.950   PROCEDURAL             MODULE-REQUEST MANUAL
    15.950   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    15.950   PROCEDURAL             CLEAR-BUFFER MANUAL
    15.950   MOTOR                  PRESS-KEY KEY 1
    15.950   MOTOR                  PREPARATION-COMPLETE
    15.950   PROCEDURAL             CONFLICT-RESOLUTION
    16.000   MOTOR                  INITIATION-COMPLETE
    16.000   PROCEDURAL             CONFLICT-RESOLUTION
    16.100   KEYBOARD               output-key SIMPLE-TRACKER-TEST 1
    16.100   VISION                 PROC-DISPLAY
    16.100   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    16.100   VISION                 visicon-update
    16.100   PROCEDURAL             CONFLICT-RESOLUTION
    16.100   PROCEDURAL             PRODUCTION-SELECTED WRONG
    16.100   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    16.100   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    16.150   PROCEDURAL             PRODUCTION-FIRED WRONG
    16.150   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
    16.150   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    16.150   PROCEDURAL             CONFLICT-RESOLUTION
    16.250   MOTOR                  FINISH-MOVEMENT
    16.250   PROCEDURAL             CONFLICT-RESOLUTION
    21.100   VISION                 PROC-DISPLAY
    21.100   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    21.100   VISION                 visicon-update
    21.100   PROCEDURAL             CONFLICT-RESOLUTION
    21.100   PROCEDURAL             PRODUCTION-SELECTED GUESS
    21.100   PROCEDURAL             BUFFER-READ-ACTION GOAL
    21.100   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    21.100   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    21.150   PROCEDURAL             PRODUCTION-FIRED GUESS
    21.150   PROCEDURAL             MODULE-REQUEST MANUAL
    21.150   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    21.150   PROCEDURAL             CLEAR-BUFFER MANUAL
    21.150   MOTOR                  PRESS-KEY KEY 1
    21.150   MOTOR                  PREPARATION-COMPLETE
    21.150   PROCEDURAL             CONFLICT-RESOLUTION
    21.200   MOTOR                  INITIATION-COMPLETE
    21.200   PROCEDURAL             CONFLICT-RESOLUTION
    21.300   KEYBOARD               output-key SIMPLE-TRACKER-TEST 1
    21.300   VISION                 PROC-DISPLAY
    21.300   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    21.300   VISION                 visicon-update
    21.300   PROCEDURAL             CONFLICT-RESOLUTION
    21.300   PROCEDURAL             PRODUCTION-SELECTED WRONG
    21.300   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    21.300   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    21.350   PROCEDURAL             PRODUCTION-FIRED WRONG
    21.350   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
    21.350   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    21.350   PROCEDURAL             CONFLICT-RESOLUTION
    21.450   MOTOR                  FINISH-MOVEMENT
    21.450   PROCEDURAL             CONFLICT-RESOLUTION
    26.300   VISION                 PROC-DISPLAY
    26.300   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    26.300   VISION                 visicon-update
    26.300   PROCEDURAL             CONFLICT-RESOLUTION
    26.300   PROCEDURAL             PRODUCTION-SELECTED GUESS
    26.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
    26.300   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    26.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    26.350   PROCEDURAL             PRODUCTION-FIRED GUESS
    26.350   PROCEDURAL             MODULE-REQUEST MANUAL
    26.350   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    26.350   PROCEDURAL             CLEAR-BUFFER MANUAL
    26.350   MOTOR                  PRESS-KEY KEY 1
    26.350   MOTOR                  PREPARATION-COMPLETE
    26.350   PROCEDURAL             CONFLICT-RESOLUTION
    26.400   MOTOR                  INITIATION-COMPLETE
    26.400   PROCEDURAL             CONFLICT-RESOLUTION
    26.500   KEYBOARD               output-key SIMPLE-TRACKER-TEST 1
    26.500   VISION                 PROC-DISPLAY
    26.500   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    26.500   VISION                 visicon-update
    26.500   PROCEDURAL             CONFLICT-RESOLUTION
    26.500   PROCEDURAL             PRODUCTION-SELECTED WRONG
    26.500   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    26.500   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    26.550   PROCEDURAL             PRODUCTION-FIRED WRONG
    26.550   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
    26.550   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    26.550   PROCEDURAL             CONFLICT-RESOLUTION
    26.650   MOTOR                  FINISH-MOVEMENT
    26.650   PROCEDURAL             CONFLICT-RESOLUTION
    31.500   VISION                 PROC-DISPLAY
    31.500   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    31.500   VISION                 visicon-update
    31.500   PROCEDURAL             CONFLICT-RESOLUTION
    31.500   PROCEDURAL             PRODUCTION-SELECTED GUESS
    31.500   PROCEDURAL             BUFFER-READ-ACTION GOAL
    31.500   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    31.500   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    31.550   PROCEDURAL             PRODUCTION-FIRED GUESS
    31.550   PROCEDURAL             MODULE-REQUEST MANUAL
    31.550   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    31.550   PROCEDURAL             CLEAR-BUFFER MANUAL
    31.550   MOTOR                  PRESS-KEY KEY 1
    31.550   MOTOR                  PREPARATION-COMPLETE
    31.550   PROCEDURAL             CONFLICT-RESOLUTION
    31.600   MOTOR                  INITIATION-COMPLETE
    31.600   PROCEDURAL             CONFLICT-RESOLUTION
    31.700   KEYBOARD               output-key SIMPLE-TRACKER-TEST 1
    31.700   VISION                 PROC-DISPLAY
    31.700   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    31.700   VISION                 visicon-update
    31.700   PROCEDURAL             CONFLICT-RESOLUTION
    31.700   PROCEDURAL             PRODUCTION-SELECTED WRONG
    31.700   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    31.700   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    31.750   PROCEDURAL             PRODUCTION-FIRED WRONG
    31.750   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
    31.750   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    31.750   PROCEDURAL             CONFLICT-RESOLUTION
    31.850   MOTOR                  FINISH-MOVEMENT
    31.850   PROCEDURAL             CONFLICT-RESOLUTION
    31.946   TRACKER                update-tracker TRACKER0 VALUE
Tracker TRACKER0 for VALUE slot updating the equation
  new equation is -0.6383563 + 0.5319536x + -0.10638899x^2
Tracker TRACKER0 generating value for buffer GOAL slot VALUE with temperature 0.8494733:
  Choices: 1.000 2.000 3.000
  Probs:   0.280 0.360 0.360
 Chosen result is 2.0
    31.946   TRACKER                MOD-BUFFER-CHUNK GOAL
    31.946   PROCEDURAL             CONFLICT-RESOLUTION
    36.700   VISION                 PROC-DISPLAY
    36.700   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    36.700   VISION                 visicon-update
    36.700   PROCEDURAL             CONFLICT-RESOLUTION
    36.700   PROCEDURAL             PRODUCTION-SELECTED GUESS
    36.700   PROCEDURAL             BUFFER-READ-ACTION GOAL
    36.700   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    36.700   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    36.750   PROCEDURAL             PRODUCTION-FIRED GUESS
    36.750   PROCEDURAL             MODULE-REQUEST MANUAL
    36.750   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    36.750   PROCEDURAL             CLEAR-BUFFER MANUAL
    36.750   MOTOR                  PRESS-KEY KEY 2
    36.750   PROCEDURAL             CONFLICT-RESOLUTION
    36.900   MOTOR                  PREPARATION-COMPLETE
    36.900   PROCEDURAL             CONFLICT-RESOLUTION
    36.950   MOTOR                  INITIATION-COMPLETE
    36.950   PROCEDURAL             CONFLICT-RESOLUTION
    37.050   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    37.050   VISION                 PROC-DISPLAY
    37.050   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    37.050   VISION                 visicon-update
    37.050   PROCEDURAL             CONFLICT-RESOLUTION
    37.050   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    37.050   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    37.050   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    37.100   PROCEDURAL             PRODUCTION-FIRED CORRECT
    37.100   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    37.100   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    37.100   PROCEDURAL             CONFLICT-RESOLUTION
    37.200   MOTOR                  FINISH-MOVEMENT
    37.200   PROCEDURAL             CONFLICT-RESOLUTION
    38.955   TRACKER                update-tracker TRACKER0 VALUE
Tracker TRACKER0 for VALUE slot updating the equation
  new equation is -1.0129573 + 1.0314226x + -0.2312571x^2
Tracker TRACKER0 generating value for buffer GOAL slot VALUE with temperature 0.8222745:
  Choices: 1.000 2.000 3.000
  Probs:   0.263 0.396 0.341
 Chosen result is 2.0
    38.955   TRACKER                MOD-BUFFER-CHUNK GOAL
    38.955   PROCEDURAL             CONFLICT-RESOLUTION
    39.955   TRACKER                update-tracker TRACKER0 VALUE
Tracker TRACKER0 for VALUE slot updating the equation
  new equation is -0.97136295 + 0.97596157x + -0.21739036x^2
Tracker TRACKER0 generating value for buffer GOAL slot VALUE with temperature 0.81853527:
  Choices: 1.000 2.000 3.000
  Probs:   0.264 0.393 0.343
 Chosen result is 1.0
    39.955   TRACKER                MOD-BUFFER-CHUNK GOAL
    39.955   PROCEDURAL             CONFLICT-RESOLUTION
    42.050   VISION                 PROC-DISPLAY
    42.050   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    42.050   VISION                 visicon-update
    42.050   PROCEDURAL             CONFLICT-RESOLUTION
    42.050   PROCEDURAL             PRODUCTION-SELECTED GUESS
    42.050   PROCEDURAL             BUFFER-READ-ACTION GOAL
    42.050   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    42.050   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    42.100   PROCEDURAL             PRODUCTION-FIRED GUESS
    42.100   PROCEDURAL             MODULE-REQUEST MANUAL
    42.100   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    42.100   PROCEDURAL             CLEAR-BUFFER MANUAL
    42.100   MOTOR                  PRESS-KEY KEY 1
    42.100   PROCEDURAL             CONFLICT-RESOLUTION
    42.250   MOTOR                  PREPARATION-COMPLETE
    42.250   PROCEDURAL             CONFLICT-RESOLUTION
    42.300   MOTOR                  INITIATION-COMPLETE
    42.300   PROCEDURAL             CONFLICT-RESOLUTION
    42.400   KEYBOARD               output-key SIMPLE-TRACKER-TEST 1
    42.400   VISION                 PROC-DISPLAY
    42.400   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    42.400   VISION                 visicon-update
    42.400   PROCEDURAL             CONFLICT-RESOLUTION
    42.400   PROCEDURAL             PRODUCTION-SELECTED WRONG
    42.400   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    42.400   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    42.450   PROCEDURAL             PRODUCTION-FIRED WRONG
    42.450   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
    42.450   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    42.450   PROCEDURAL             CONFLICT-RESOLUTION
    42.550   MOTOR                  FINISH-MOVEMENT
    42.550   PROCEDURAL             CONFLICT-RESOLUTION
    43.291   TRACKER                update-tracker TRACKER0 VALUE
Tracker TRACKER0 for VALUE slot updating the equation
  new equation is -0.99539316 + 0.99599135x + -0.22139755x^2
Tracker TRACKER0 generating value for buffer GOAL slot VALUE with temperature 0.8063035:
  Choices: 1.000 2.000 3.000
  Probs:   0.261 0.395 0.344
 Chosen result is 2.0
    43.291   TRACKER                MOD-BUFFER-CHUNK GOAL
    43.291   PROCEDURAL             CONFLICT-RESOLUTION
    47.400   VISION                 PROC-DISPLAY
    47.400   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    47.400   VISION                 visicon-update
    47.400   PROCEDURAL             CONFLICT-RESOLUTION
    47.400   PROCEDURAL             PRODUCTION-SELECTED GUESS
    47.400   PROCEDURAL             BUFFER-READ-ACTION GOAL
    47.400   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    47.400   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    47.450   PROCEDURAL             PRODUCTION-FIRED GUESS
    47.450   PROCEDURAL             MODULE-REQUEST MANUAL
    47.450   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    47.450   PROCEDURAL             CLEAR-BUFFER MANUAL
    47.450   MOTOR                  PRESS-KEY KEY 2
    47.450   PROCEDURAL             CONFLICT-RESOLUTION
    47.600   MOTOR                  PREPARATION-COMPLETE
    47.600   PROCEDURAL             CONFLICT-RESOLUTION
    47.650   MOTOR                  INITIATION-COMPLETE
    47.650   PROCEDURAL             CONFLICT-RESOLUTION
    47.750   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    47.750   VISION                 PROC-DISPLAY
    47.750   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    47.750   VISION                 visicon-update
    47.750   PROCEDURAL             CONFLICT-RESOLUTION
    47.750   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    47.750   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    47.750   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    47.800   PROCEDURAL             PRODUCTION-FIRED CORRECT
    47.800   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    47.800   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    47.800   PROCEDURAL             CONFLICT-RESOLUTION
    47.900   MOTOR                  FINISH-MOVEMENT
    47.900   PROCEDURAL             CONFLICT-RESOLUTION
    52.750   VISION                 PROC-DISPLAY
    52.750   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    52.750   VISION                 visicon-update
    52.750   PROCEDURAL             CONFLICT-RESOLUTION
    52.750   PROCEDURAL             PRODUCTION-SELECTED GUESS
    52.750   PROCEDURAL             BUFFER-READ-ACTION GOAL
    52.750   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    52.750   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    52.800   PROCEDURAL             PRODUCTION-FIRED GUESS
    52.800   PROCEDURAL             MODULE-REQUEST MANUAL
    52.800   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    52.800   PROCEDURAL             CLEAR-BUFFER MANUAL
    52.800   MOTOR                  PRESS-KEY KEY 2
    52.800   MOTOR                  PREPARATION-COMPLETE
    52.800   PROCEDURAL             CONFLICT-RESOLUTION
    52.850   MOTOR                  INITIATION-COMPLETE
    52.850   PROCEDURAL             CONFLICT-RESOLUTION
    52.950   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    52.950   VISION                 PROC-DISPLAY
    52.950   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    52.950   VISION                 visicon-update
    52.950   PROCEDURAL             CONFLICT-RESOLUTION
    52.950   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    52.950   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    52.950   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    53.000   PROCEDURAL             PRODUCTION-FIRED CORRECT
    53.000   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    53.000   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    53.000   PROCEDURAL             CONFLICT-RESOLUTION
    53.100   MOTOR                  FINISH-MOVEMENT
    53.100   PROCEDURAL             CONFLICT-RESOLUTION
    57.950   VISION                 PROC-DISPLAY
    57.950   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    57.950   VISION                 visicon-update
    57.950   PROCEDURAL             CONFLICT-RESOLUTION
    57.950   PROCEDURAL             PRODUCTION-SELECTED GUESS
    57.950   PROCEDURAL             BUFFER-READ-ACTION GOAL
    57.950   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    57.950   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    58.000   PROCEDURAL             PRODUCTION-FIRED GUESS
    58.000   PROCEDURAL             MODULE-REQUEST MANUAL
    58.000   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    58.000   PROCEDURAL             CLEAR-BUFFER MANUAL
    58.000   MOTOR                  PRESS-KEY KEY 2
    58.000   MOTOR                  PREPARATION-COMPLETE
    58.000   PROCEDURAL             CONFLICT-RESOLUTION
    58.050   MOTOR                  INITIATION-COMPLETE
    58.050   PROCEDURAL             CONFLICT-RESOLUTION
    58.150   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    58.150   VISION                 PROC-DISPLAY
    58.150   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    58.150   VISION                 visicon-update
    58.150   PROCEDURAL             CONFLICT-RESOLUTION
    58.150   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    58.150   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    58.150   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    58.200   PROCEDURAL             PRODUCTION-FIRED CORRECT
    58.200   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    58.200   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    58.200   PROCEDURAL             CONFLICT-RESOLUTION
    58.300   MOTOR                  FINISH-MOVEMENT
    58.300   PROCEDURAL             CONFLICT-RESOLUTION
    63.150   VISION                 PROC-DISPLAY
    63.150   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    63.150   VISION                 visicon-update
    63.150   PROCEDURAL             CONFLICT-RESOLUTION
    63.150   PROCEDURAL             PRODUCTION-SELECTED GUESS
    63.150   PROCEDURAL             BUFFER-READ-ACTION GOAL
    63.150   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    63.150   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    63.200   PROCEDURAL             PRODUCTION-FIRED GUESS
    63.200   PROCEDURAL             MODULE-REQUEST MANUAL
    63.200   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    63.200   PROCEDURAL             CLEAR-BUFFER MANUAL
    63.200   MOTOR                  PRESS-KEY KEY 2
    63.200   MOTOR                  PREPARATION-COMPLETE
    63.200   PROCEDURAL             CONFLICT-RESOLUTION
    63.250   MOTOR                  INITIATION-COMPLETE
    63.250   PROCEDURAL             CONFLICT-RESOLUTION
    63.350   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    63.350   VISION                 PROC-DISPLAY
    63.350   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    63.350   VISION                 visicon-update
    63.350   PROCEDURAL             CONFLICT-RESOLUTION
    63.350   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    63.350   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    63.350   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    63.400   PROCEDURAL             PRODUCTION-FIRED CORRECT
    63.400   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    63.400   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    63.400   PROCEDURAL             CONFLICT-RESOLUTION
    63.500   MOTOR                  FINISH-MOVEMENT
    63.500   PROCEDURAL             CONFLICT-RESOLUTION
    68.350   VISION                 PROC-DISPLAY
    68.350   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    68.350   VISION                 visicon-update
    68.350   PROCEDURAL             CONFLICT-RESOLUTION
    68.350   PROCEDURAL             PRODUCTION-SELECTED GUESS
    68.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
    68.350   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    68.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    68.400   PROCEDURAL             PRODUCTION-FIRED GUESS
    68.400   PROCEDURAL             MODULE-REQUEST MANUAL
    68.400   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    68.400   PROCEDURAL             CLEAR-BUFFER MANUAL
    68.400   MOTOR                  PRESS-KEY KEY 2
    68.400   MOTOR                  PREPARATION-COMPLETE
    68.400   PROCEDURAL             CONFLICT-RESOLUTION
    68.450   MOTOR                  INITIATION-COMPLETE
    68.450   PROCEDURAL             CONFLICT-RESOLUTION
    68.550   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    68.550   VISION                 PROC-DISPLAY
    68.550   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    68.550   VISION                 visicon-update
    68.550   PROCEDURAL             CONFLICT-RESOLUTION
    68.550   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    68.550   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    68.550   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    68.600   PROCEDURAL             PRODUCTION-FIRED CORRECT
    68.600   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    68.600   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    68.600   PROCEDURAL             CONFLICT-RESOLUTION
    68.700   MOTOR                  FINISH-MOVEMENT
    68.700   PROCEDURAL             CONFLICT-RESOLUTION
    73.550   VISION                 PROC-DISPLAY
    73.550   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    73.550   VISION                 visicon-update
    73.550   PROCEDURAL             CONFLICT-RESOLUTION
    73.550   PROCEDURAL             PRODUCTION-SELECTED GUESS
    73.550   PROCEDURAL             BUFFER-READ-ACTION GOAL
    73.550   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    73.550   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    73.600   PROCEDURAL             PRODUCTION-FIRED GUESS
    73.600   PROCEDURAL             MODULE-REQUEST MANUAL
    73.600   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    73.600   PROCEDURAL             CLEAR-BUFFER MANUAL
    73.600   MOTOR                  PRESS-KEY KEY 2
    73.600   MOTOR                  PREPARATION-COMPLETE
    73.600   PROCEDURAL             CONFLICT-RESOLUTION
    73.650   MOTOR                  INITIATION-COMPLETE
    73.650   PROCEDURAL             CONFLICT-RESOLUTION
    73.750   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    73.750   VISION                 PROC-DISPLAY
    73.750   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    73.750   VISION                 visicon-update
    73.750   PROCEDURAL             CONFLICT-RESOLUTION
    73.750   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    73.750   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    73.750   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    73.800   PROCEDURAL             PRODUCTION-FIRED CORRECT
    73.800   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    73.800   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    73.800   PROCEDURAL             CONFLICT-RESOLUTION
    73.900   MOTOR                  FINISH-MOVEMENT
    73.900   PROCEDURAL             CONFLICT-RESOLUTION
    78.750   VISION                 PROC-DISPLAY
    78.750   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    78.750   VISION                 visicon-update
    78.750   PROCEDURAL             CONFLICT-RESOLUTION
    78.750   PROCEDURAL             PRODUCTION-SELECTED GUESS
    78.750   PROCEDURAL             BUFFER-READ-ACTION GOAL
    78.750   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    78.750   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    78.800   PROCEDURAL             PRODUCTION-FIRED GUESS
    78.800   PROCEDURAL             MODULE-REQUEST MANUAL
    78.800   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    78.800   PROCEDURAL             CLEAR-BUFFER MANUAL
    78.800   MOTOR                  PRESS-KEY KEY 2
    78.800   MOTOR                  PREPARATION-COMPLETE
    78.800   PROCEDURAL             CONFLICT-RESOLUTION
    78.850   MOTOR                  INITIATION-COMPLETE
    78.850   PROCEDURAL             CONFLICT-RESOLUTION
    78.950   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    78.950   VISION                 PROC-DISPLAY
    78.950   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    78.950   VISION                 visicon-update
    78.950   PROCEDURAL             CONFLICT-RESOLUTION
    78.950   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    78.950   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    78.950   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    79.000   PROCEDURAL             PRODUCTION-FIRED CORRECT
    79.000   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    79.000   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    79.000   PROCEDURAL             CONFLICT-RESOLUTION
    79.100   MOTOR                  FINISH-MOVEMENT
    79.100   PROCEDURAL             CONFLICT-RESOLUTION
    83.950   VISION                 PROC-DISPLAY
    83.950   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    83.950   VISION                 visicon-update
    83.950   PROCEDURAL             CONFLICT-RESOLUTION
    83.950   PROCEDURAL             PRODUCTION-SELECTED GUESS
    83.950   PROCEDURAL             BUFFER-READ-ACTION GOAL
    83.950   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    83.950   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    84.000   PROCEDURAL             PRODUCTION-FIRED GUESS
    84.000   PROCEDURAL             MODULE-REQUEST MANUAL
    84.000   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    84.000   PROCEDURAL             CLEAR-BUFFER MANUAL
    84.000   MOTOR                  PRESS-KEY KEY 2
    84.000   MOTOR                  PREPARATION-COMPLETE
    84.000   PROCEDURAL             CONFLICT-RESOLUTION
    84.050   MOTOR                  INITIATION-COMPLETE
    84.050   PROCEDURAL             CONFLICT-RESOLUTION
    84.150   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    84.150   VISION                 PROC-DISPLAY
    84.150   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    84.150   VISION                 visicon-update
    84.150   PROCEDURAL             CONFLICT-RESOLUTION
    84.150   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    84.150   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    84.150   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    84.200   PROCEDURAL             PRODUCTION-FIRED CORRECT
    84.200   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    84.200   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    84.200   PROCEDURAL             CONFLICT-RESOLUTION
    84.300   MOTOR                  FINISH-MOVEMENT
    84.300   PROCEDURAL             CONFLICT-RESOLUTION
    85.155   TRACKER                update-tracker TRACKER0 VALUE
Tracker TRACKER0 for VALUE slot updating the equation
  new equation is -1.193099 + 1.2595936x + -0.28729433x^2
Tracker TRACKER0 generating value for buffer GOAL slot VALUE with temperature 0.67897624:
  Choices: 1.000 2.000 3.000
  Probs:   0.239 0.430 0.331
 Chosen result is 2.0
    85.155   TRACKER                MOD-BUFFER-CHUNK GOAL
    85.155   PROCEDURAL             CONFLICT-RESOLUTION
    89.150   VISION                 PROC-DISPLAY
    89.150   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    89.150   VISION                 visicon-update
    89.150   PROCEDURAL             CONFLICT-RESOLUTION
    89.150   PROCEDURAL             PRODUCTION-SELECTED GUESS
    89.150   PROCEDURAL             BUFFER-READ-ACTION GOAL
    89.150   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    89.150   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    89.200   PROCEDURAL             PRODUCTION-FIRED GUESS
    89.200   PROCEDURAL             MODULE-REQUEST MANUAL
    89.200   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    89.200   PROCEDURAL             CLEAR-BUFFER MANUAL
    89.200   MOTOR                  PRESS-KEY KEY 2
    89.200   MOTOR                  PREPARATION-COMPLETE
    89.200   PROCEDURAL             CONFLICT-RESOLUTION
    89.250   MOTOR                  INITIATION-COMPLETE
    89.250   PROCEDURAL             CONFLICT-RESOLUTION
    89.350   KEYBOARD               output-key SIMPLE-TRACKER-TEST 2
    89.350   VISION                 PROC-DISPLAY
    89.350   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    89.350   VISION                 visicon-update
    89.350   PROCEDURAL             CONFLICT-RESOLUTION
    89.350   PROCEDURAL             PRODUCTION-SELECTED CORRECT
    89.350   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    89.350   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    89.400   PROCEDURAL             PRODUCTION-FIRED CORRECT
    89.400   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current GOOD value
 Updating event occurred in the GOOD slot of the IMAGINAL buffer
  updating with value 1.0
    89.400   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    89.400   PROCEDURAL             CONFLICT-RESOLUTION
    89.500   MOTOR                  FINISH-MOVEMENT
    89.500   PROCEDURAL             CONFLICT-RESOLUTION
    90.279   TRACKER                update-tracker TRACKER0 VALUE
Tracker TRACKER0 for VALUE slot updating the equation
  new equation is -1.1981206 + 1.2662959x + -0.28897288x^2
Tracker TRACKER0 generating value for buffer GOAL slot VALUE with temperature 0.6661017:
  Choices: 1.000 2.000 3.000
  Probs:   0.237 0.432 0.331
 Chosen result is 3.0
    90.279   TRACKER                MOD-BUFFER-CHUNK GOAL
    90.279   PROCEDURAL             CONFLICT-RESOLUTION
    94.350   VISION                 PROC-DISPLAY
    94.350   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    94.350   VISION                 visicon-update
    94.350   PROCEDURAL             CONFLICT-RESOLUTION
    94.350   PROCEDURAL             PRODUCTION-SELECTED GUESS
    94.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
    94.350   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    94.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    94.400   PROCEDURAL             PRODUCTION-FIRED GUESS
    94.400   PROCEDURAL             MODULE-REQUEST MANUAL
    94.400   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    94.400   PROCEDURAL             CLEAR-BUFFER MANUAL
    94.400   MOTOR                  PRESS-KEY KEY 3
    94.400   PROCEDURAL             CONFLICT-RESOLUTION
    94.550   MOTOR                  PREPARATION-COMPLETE
    94.550   PROCEDURAL             CONFLICT-RESOLUTION
    94.600   MOTOR                  INITIATION-COMPLETE
    94.600   PROCEDURAL             CONFLICT-RESOLUTION
    94.700   KEYBOARD               output-key SIMPLE-TRACKER-TEST 3
    94.700   VISION                 PROC-DISPLAY
    94.700   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    94.700   VISION                 visicon-update
    94.700   PROCEDURAL             CONFLICT-RESOLUTION
    94.700   PROCEDURAL             PRODUCTION-SELECTED WRONG
    94.700   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    94.700   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    94.750   PROCEDURAL             PRODUCTION-FIRED WRONG
    94.750   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
    94.750   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    94.750   PROCEDURAL             CONFLICT-RESOLUTION
    94.850   MOTOR                  FINISH-MOVEMENT
    94.850   PROCEDURAL             CONFLICT-RESOLUTION
    99.700   VISION                 PROC-DISPLAY
    99.700   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    99.700   VISION                 visicon-update
    99.700   PROCEDURAL             CONFLICT-RESOLUTION
    99.700   PROCEDURAL             PRODUCTION-SELECTED GUESS
    99.700   PROCEDURAL             BUFFER-READ-ACTION GOAL
    99.700   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    99.700   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
    99.750   PROCEDURAL             PRODUCTION-FIRED GUESS
    99.750   PROCEDURAL             MODULE-REQUEST MANUAL
    99.750   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    99.750   PROCEDURAL             CLEAR-BUFFER MANUAL
    99.750   MOTOR                  PRESS-KEY KEY 3
    99.750   MOTOR                  PREPARATION-COMPLETE
    99.750   PROCEDURAL             CONFLICT-RESOLUTION
    99.800   MOTOR                  INITIATION-COMPLETE
    99.800   PROCEDURAL             CONFLICT-RESOLUTION
    99.900   KEYBOARD               output-key SIMPLE-TRACKER-TEST 3
    99.900   VISION                 PROC-DISPLAY
    99.900   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION CHUNK0 NIL
    99.900   VISION                 visicon-update
    99.900   PROCEDURAL             CONFLICT-RESOLUTION
    99.900   PROCEDURAL             PRODUCTION-SELECTED WRONG
    99.900   PROCEDURAL             BUFFER-READ-ACTION IMAGINAL
    99.900   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
    99.950   PROCEDURAL             PRODUCTION-FIRED WRONG
    99.950   PROCEDURAL             MOD-BUFFER-CHUNK IMAGINAL
Tracker TRACKER0 for VALUE updating the current BAD value
 Updating event occurred in the BAD slot of the IMAGINAL buffer
  updating with value 1.0
    99.950   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
    99.950   PROCEDURAL             CONFLICT-RESOLUTION
   100.000   ------                 Stopped because time limit reached
|#