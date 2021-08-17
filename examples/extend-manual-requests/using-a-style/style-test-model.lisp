;;; This file defines a model which just repeatedly
;;; performs the new-action request defined in
;;; the new-style.lisp file.  A trace of the run
;;; is included below.

(clear-all)

(define-model foo 
  (sgp :trace-detail high)
  
  (p p1
     ?manual>
     state free
     ==> 
     +manual> 
     isa new-action 
     arg1 1 
     arg2 2))

#|
CG-USER(114): (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED P1
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED P1
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  NEW-ACTION ARG1 1 ARG2 2
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   MOTOR                  PREPARATION-COMPLETE 0.05
     0.200   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   MOTOR                  INITIATION-COMPLETE 0.05
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   NONE                   DEMO-PRINT-STYLE-INFO 1 2 0.15 0.3
Features are 1 and 2
0.15 seconds spent in preparation
finishing after 0.3 seconds
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   MOTOR                  FINISH-MOVEMENT 0.05
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   PROCEDURAL             PRODUCTION-SELECTED P1
     0.500   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.550   PROCEDURAL             PRODUCTION-FIRED P1
     0.550   PROCEDURAL             MODULE-REQUEST MANUAL
     0.550   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.550   MOTOR                  NEW-ACTION ARG1 1 ARG2 2
     0.550   MOTOR                  PREPARATION-COMPLETE 0.55
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  INITIATION-COMPLETE 0.55
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   NONE                   DEMO-PRINT-STYLE-INFO 1 2 0.0 0.3
Features are 1 and 2
0.0 seconds spent in preparation
finishing after 0.3 seconds
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  FINISH-MOVEMENT 0.55
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   PROCEDURAL             PRODUCTION-SELECTED P1
     0.850   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.900   PROCEDURAL             PRODUCTION-FIRED P1
     0.900   PROCEDURAL             MODULE-REQUEST MANUAL
     0.900   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.900   MOTOR                  NEW-ACTION ARG1 1 ARG2 2
     0.900   MOTOR                  PREPARATION-COMPLETE 0.9
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   MOTOR                  INITIATION-COMPLETE 0.9
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     1.000   ------                 Stopped because time limit reached

|#
