;;; This model simply tests the extended motor module actions by performing
;;; all the old and new actions at least once each with some instances of
;;; automatically releasing things.
;;;
;;; It is by no means a good model, nor does it completely test everything,
;;; but does verify that all the commands and new parameters work.
;;;

;;; Variables for changing the new parameters.  Set initially to the
;;; corresponding default values.
(defparameter *kct* .01)
(defparameter *krt* .04)
(defparameter *dp* nil)
(defparameter *de* t)
(defparameter *psd* nil)
(defparameter *cdd* nil)
(defparameter *dpd* .075)
(defparameter *fpd* .05)
(defparameter *spd* .1)
                       
;;; Variables for modifying the productions in the test model
(defparameter *query* 'state)

;;; variable for recording 
(defvar *data* nil)

(defun key-down-recorder (model key)
  (declare (ignore model))
  (push-last (list :pressed (mp-time) key) *data*))

(defmethod key-up-recorder (model key)
  (declare (ignore model))
  (push-last (list :released (mp-time) key) *data*))

(defmethod mouse-down-recorder (model position finger)
  (declare (ignore model finger))
  (push-last (list :clicked (mp-time) position) *data*))

(defmethod mouse-up-recorder (model position finger)
  (declare (ignore model finger))
  (push-last (list :unclicked (mp-time) position) *data*))


(defun setup-monitors ()
  (add-act-r-command "extended-motor-key-down" 'key-down-recorder "Monitor for output-key in testing extended-motor-actions extra.")
  (monitor-act-r-command "output-key" "extended-motor-key-down")
  (add-act-r-command "extended-motor-key-up" 'key-up-recorder "Monitor for release-key in testing extended-motor-actions extra.")
  (monitor-act-r-command "release-key" "extended-motor-key-up")
  (add-act-r-command "extended-motor-mouse-down" 'mouse-down-recorder "Monitor for click-mouse in testing extended-motor-actions extra.")
  (monitor-act-r-command "click-mouse" "extended-motor-mouse-down")
  (add-act-r-command "extended-motor-mouse-up" 'mouse-up-recorder "Monitor for release-mouse in testing extended-motor-actions extra.")
  (monitor-act-r-command "release-mouse" "extended-motor-mouse-up"))

(defun remove-monitors ()
  (remove-act-r-command-monitor "output-key" "extended-motor-key-down")
  (remove-act-r-command-monitor "release-key" "extended-motor-key-up")
  (remove-act-r-command-monitor "click-mouse" "extended-motor-mouse-down")
  (remove-act-r-command-monitor "release-mouse" "extended-motor-mouse-up")
  (remove-act-r-command  "extended-motor-key-down")
  (remove-act-r-command  "extended-motor-key-up")
  (remove-act-r-command  "extended-motor-mouse-down")
  (remove-act-r-command  "extended-motor-mouse-up"))


(defun run-test (name)
  ;; set all to testing defaults
  (setf *kct* .01)
  (setf *krt* .04)
  (setf *dp* nil)
  (setf *de* t)
  (setf *psd* nil)
  (setf *cdd* nil)
  (setf *dpd* .075)
  (setf *fpd* .05)
  (setf *spd* .1)
  (setf *query* 'state)

  (setup-monitors)
  (setf *data* nil)
  (funcall name)
  (remove-monitors)
  (summarize-tests *data*))

(defun summarize-tests (data)
  (do* ((start 0 end)
        (end (position :test data :key 'car :start (if start (1+ start) 0))
             (position :test data :key 'car :start (if start (1+ start) 0))))
       ((null start) data)
    (display-data (subseq data start end))))

(defun display-data (data)
  (format t "~&#####~%~S~%#####~%" (cdar data))
  (dolist (action '(:pressed :released :clicked :unclicked))
    (format t " ~a:~{~^~%~{   ~7,3f ~10S ~@[(~5,3f)~]~}~}~%" 
      action
      (let ((last nil))
        (mapcan (lambda (x) 
                  (when (eq (car x) action)
                    (list (prog1
                              (if last
                                  (list (second x) (third x) (- (second x) last))
                                (list (second x) (third x) nil))
                              (setf last (second x))))))
          data))))
  (format t " Held:~%")
  (dolist (pressed (remove-if-not (lambda (x)
                                    (eq (car x) :pressed))
                                  data))
    (format t "  ~10s ~@[~5,3f~]~%" (third pressed)
      (let ((release (find-if (lambda (x)
                                (and (eq (car x) :released) (eql (third x) (third pressed))))
                              data :start (position pressed data))))
        (when release
          (- (second release) (second pressed))))))
  (format t " Clicked:~%")
  (dolist (clicked (remove-if-not (lambda (x)
                                    (eq (car x) :clicked))
                                  data))
    (format t "  ~10s ~@[~{~10s ~5,3f~}~]~%" (third clicked)
      (let ((release (find-if (lambda (x) (eq (car x) :unclicked))
                              data :start (position clicked data))))
        (when release
          (list (third release) (- (second release) (second clicked))))))))

  
                                    


(defun test1 ()
  (dolist (hand '(same diff))
    (dolist (query '(state processor preparation))
      (dolist (processors '(t nil))
        (dolist (executions '(t nil))
          (let ((details (format nil "~s hand ~s processors ~s executions query ~s"
                           hand (if processors 2 1) (if executions 2 1) query)))
            
            (setf *dp* processors)
            (setf *de* executions)
            (setf *query* query)
            (reset)
            (format t "########################~%~S~%########################~%" details)
            (push-last (cons :test details) *data*)
            (goal-focus-fct (car (define-chunks-fct `((isa goal test test1 param ,hand)))))
            (run 10)))))))


(defun test2 ()
  (dolist (x '(nil t .5 1.0))
    (push-last (cons :test (format nil "click and hold mouse *cdd* ~s" x)) *data*)
    (setf *cdd* x)
    (reset)
    (start-hand-at-mouse)
    (goal-focus-fct (car (define-chunks (isa goal test test2))))
    (run 10)))

(defun test3 ()
  (push-last (cons :test "Auto release key & mouse when moving hand") *data*)
  (setf *krt* .025)
  (setf *kct* .025)
  (reset)
  
  (goal-focus-fct (car (define-chunks (isa goal test test3))))
  (run .5)
  (buffer-status manual-left manual-right)
  (run .75)
  (buffer-status manual-left manual-right)
  (run 5))


(defun test4 ()
  (push-last (cons :test "Some various key pressing") *data*)
  (setf *psd* .2)
  (reset)
  (goal-focus-fct (car (define-chunks (isa goal test test4))))
  (pbreak test4-7)
  (run 10)
  (buffer-status manual-left manual-right)
  (dolist (hand '(left right))
    (dolist (finger '(index middle ring pinkie thumb))
      (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger))))
  (run 10)
  (buffer-status manual-left manual-right)
  (dolist (hand '(left right))
    (dolist (finger '(index middle ring pinkie thumb))
      (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger))))
  )
             
(defun test5 ()
  (push-last (cons :test "Move the fingers and hands around") *data*)
  (reset)
  (goal-focus-fct (car (define-chunks (isa goal test test5))))
  
  (pbreak test5-5 test5-9 test5-11 test5-12)
  
  (do ((time (run 10) (run 10)))
      ((= time 0.0))
  (buffer-status manual-left manual-right)
  (dolist (hand '(left right))
    (dolist (finger '(index middle ring pinkie thumb))
      (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger))))))

(defun test6 ()
  (push-last (cons :test "Some type-key actions from non-home position.") *data*)
  (reset)
  (goal-focus-fct (car (define-chunks (isa goal test test6))))
  (run 10)
  (buffer-status manual-left manual-right)
  (dolist (hand '(left right))
    (dolist (finger '(index middle ring pinkie thumb))
      (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger)))))

(defun test7 ()
  (push-last (cons :test "Default timed delayed-punch actions") *data*)
  (reset)
  (goal-focus-fct (car (define-chunks (test test7))))
  (run 10)
  (push-last (cons :test "Faster timed delayed-punch actions") *data*)
  (setf *dpd* .05)
  (setf *fpd* .025)
  (setf *spd* .07)
  (reset)
  (goal-focus-fct (car (define-chunks (test test7))))
  (run 10))
  
(defun test8 ()
  (push-last (cons :test "All home with multiple fingers down") *data*)
  (reset)
  (goal-focus-fct (car (define-chunks (test test8))))
  (run 10)
  (buffer-status manual-left manual-right)
  (dolist (hand '(left right))
    (dolist (finger '(index middle ring pinkie thumb))
      (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger)))))


(defun test9 ()
  (push-last (cons :test "Release-all-fingers with multiple fingers down") *data*)
  (reset)
  (goal-focus-fct (car (define-chunks (test test9))))
  (run 10)
  (buffer-status manual-left manual-right)
  (dolist (hand '(left right))
    (dolist (finger '(index middle ring pinkie thumb))
      (format t "  ~S ~S: ~S~%" hand finger (finger-loc-m (get-module :motor) hand finger)))))
                  
(clear-all)
(require-extra "extended-motor-actions")

(define-model test-motor
    (sgp-fct (list :v t :trace-detail 'high :key-closure-time *kct* 
                   :needs-mouse t :cursor-drag-delay *cdd*
                   :key-release-time *krt* :dual-processor-stages *dp*
                   :dual-execution-stages *de* :peck-strike-distance *psd*
                   :default-punch-delay *dpd* :fast-punch-delay *fpd*
                   :slow-punch-delay *spd*))
  
  (chunk-type goal test param state)
  
  (define-chunks 
      (loc1 isa visual-location screen-x 10 screen-y 20 distance 1080) 
      (loc2 isa visual-location screen-x 100 screen-y 40 distance 1080))
  
  (install-device (open-exp-window "" :visible nil :x 0 :y 0))
  
  (define-chunks test1 test2 test3 press2 same diff click hold move 
    hold-next to-mouse to-keys test4 test5 test6 test7 test8 test9)
  
  
  (declare-buffer-usage goal goal :all)
  
  (p test1-first-press
     =goal>
     isa goal
     test test1
     state nil
     ?manual>
     state free
     ==>
     =goal>
     state press2
     +manual>
     isa press-key
     key "r")
  
  (p-fct `(test1-second-press-same
           =goal>
           isa goal
           test test1
           state press2
           param same
           ?manual>
           ,*query* free
           ==>
           -goal>
           +manual>
           isa press-key
           key "e"))
  
  (p-fct `(test1-second-press-diff
           =goal>
           isa goal
           test test1
           state press2
           param diff
           ?manual>
           ,*query* free
           ==>
           -goal>
           +manual>
           isa press-key
           key "i"))
  
  (p test2-start
     =goal>
     isa goal
     test test2
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa move-cursor
     loc loc1
     =goal>
     state click)
  (p test2-click
     =goal>
     isa goal
     test test2
     state click
     ?manual>
     state free
     ==>
     =goal>
     state hold
     +manual>
     isa click-mouse)
  (p test2-hold
     =goal>
     isa goal
     test test2
     state hold
     ?manual>
     state free
     ==>
     +manual>
     isa hold-mouse
     =goal>
     state move
     )
  (p test2-move
     =goal>
     isa goal
     state move
     test test2
     ?manual>
     state free
     ==>
     +manual>
     isa move-cursor
     loc loc2
     =goal>
     state release)
  (p test2-release
     =goal>
     isa goal
     test test2
     state release
     ?manual>
     state free
     ==>
     +manual>
     isa release-mouse
     -goal>)
  
  
  (p test3-start
     =goal>
     isa goal
     test test3
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa hold-key
     key shift
     =goal>
     state hold-next)
  
  (p test3-hold-k
     =goal>
     isa goal
     test test3
     state hold-next
     ?manual>
     preparation free
     ==>
     +manual>
     isa hold-key
     key "k"
     =goal>
     state to-mouse)
  

  
  (p test3-to-mouse
     =goal>
     isa goal
     test test3
     state to-mouse
     ?manual>
     state free
     ==>
     =goal>
     state hold-mouse
     +manual>
     isa hand-to-mouse)
  
  (p test3-hold
     =goal>
     isa goal
     test test3
     state hold-mouse
     ?manual>
     state free
     ==>
     +manual>
     isa hold-mouse
     =goal>
     state to-keys
     )
  (p test3-keys
     =goal>
     isa goal
     test test3
     state to-keys
     ?manual>
     state free
     ==>
     +manual>
     isa hand-to-home
     -goal>)
  
  (p test4
     =goal>
     isa goal
     test test4
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa hit-key
     key "t"
     =goal>
     state 1)
  (p test4-1
     =goal>
     isa goal
     test test4
     state 1
     ?manual>
     preparation free
     ==>
     +manual>
     isa hit-key
     key "r"
     =goal>
     state 2)
  (p test4-2
     =goal>
     isa goal
     test test4
     state 2
     ?manual>
     preparation free
     ==>
     +manual>
     isa hit-key
     key "i"
     =goal>
     state 3)
    (p test4-3
     =goal>
     isa goal
     test test4
     state 3
     ?manual>
     preparation free
     ==>
     +manual>
     isa hit-key
     key "f"
     =goal>
       state 4)
    (p test4-4
     =goal>
     isa goal
     test test4
     state 4
     ?manual>
     preparation free
     ==>
     +manual>
     isa hit-key
     key "n"
     =goal>
     state 5)
  (p test4-5
     =goal>
     isa goal
     test test4
     state 5
     ?manual>
     preparation free
     ==>
     +manual>
     isa hold-key
     key "x"
     =goal>
     state 6)
  (p test4-6
     =goal>
     isa goal
     test test4
     state 6
     ?manual>
     preparation free
     ==>
     +manual>
      isa move-finger-to-home
      hand right
      finger index
     =goal>
     state 7)
  (p test4-7
     =goal>
     isa goal
     test test4
     state 7
     ?manual>
     state free
     ==>
     +manual>
     isa all-fingers-to-home
     -goal>)
  
  (p test5
     =goal>
     isa goal
     test test5
     state nil
     ?manual>
     state free
     ==>
     +manual>
      isa move-to-key
      key "w"
     =goal>
     state 1)
  (p test5-1
     =goal>
     isa goal
     test test5
     state 1
     ?manual>
     state free
     ==>
     +manual>
      isa hold-punch
     hand left
     finger ring
     =goal>
     state 2)
  
  (p test5-2
     =goal>
     isa goal
     test test5
     state 2
     ?manual>
     state free
     ==>
     +manual>
      isa release-key
     key "w"
     =goal>
     state 3)
  
    (p test5-3
     =goal>
     isa goal
     test test5
     state 3
     ?manual>
     state free
     ==>
     +manual>
      isa hit-key
     key "w"
     =goal>
       state 4)
  (p test5-4
     =goal>
     isa goal
     test test5
     state 4
     ?manual>
     state free
     ==>
     +manual>
      isa hold-key
     key "w"
     =goal>
       state 5)
  (p test5-5
     =goal>
     isa goal
     test test5
     state 5
     ?manual>
     state free
     ==>
     +manual>
      isa release-key-to-home
     key "w"
     =goal>
       state 6)
  (p test5-6
     =goal>
     isa goal
     test test5
     state 6
     ?manual>
     state free
     ==>
     +manual>
      isa move-to-key
     key "g"
     =goal>
       state 7)
  (p test5-7
     =goal>
     isa goal
     test test5
     state 7
     ?manual>
     state free
     ==>
     +manual>
      isa hold-key
     key "g"
     =goal>
     state 8)
  (p test5-8
     =goal>
     isa goal
     test test5
     state 8
     ?manual>
     state free
     ==>
     +manual>
     isa prepare
     style hand-ply
     hand right
     r 1
     theta 3.14
     =goal>
     state 9)
    (p test5-9
     =goal>
     isa goal
     test test5
     state 9
     ?manual>
     state free
     ==>
     +manual>
     isa execute
     =goal>
       state 10)
  (p test5-10
     =goal>
     isa goal
     test test5
     state 10
     ?manual>
     state free
     ==>
     +manual>
     isa point-hand-at-key
     hand right
     to-key "y"
     =goal>
       state 11)
  (p test5-11
     =goal>
     isa goal
     test test5
     state 11
     ?manual>
     state free
     ==>
     +manual>
     isa hold-key
     key "k"
     =goal>
     state 12)
  (p test5-12
     =goal>
     isa goal
     test test5
     state 12
     ?manual>
     state free
     ==>
     +manual>
     isa all-fingers-to-home
     hand right
     -goal>
     )
  
   (p test6
     =goal>
     isa goal
     test test6
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa move-to-key
     key "t"
     =goal>
     state 1)
  (p test6-1
     =goal>
     isa goal
     test test6
     state 1
     ?manual>
     preparation free
     ==>
     +manual>
     isa type-key
     key "r"
     =goal>
     state 2)
  (p test6-2
     =goal>
     isa goal
     test test6
     state 2
     ?manual>
     preparation free
     ==>
     +manual>
     isa type-key
     key "t"
     =goal>
     state 3)
    (p test6-3
     =goal>
     isa goal
     test test6
     state 3
     ?manual-left>
       index free
       ?manual>
       buffer empty
     ==>
     +manual>
     isa move-finger-to-home
       hand left
       finger index
     -goal>)
  
  (p test7
     =goal>
     isa goal
     test test7
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa delayed-punch
     hand left
     finger index
     =goal>
     state 1)
  (p test7-1
     =goal>
     isa goal
     test test7
     state 1
     ?manual>
     preparation free
     ==>
     +manual>
     isa delayed-punch
     hand right
     finger index
     delay fast
     =goal>
     state 2)
    (p test7-2
     =goal>
     isa goal
     test test7
     state 2
     ?manual>
     preparation free
     ==>
     +manual>
     isa delayed-punch
     hand right
     finger middle
     delay slow
     =goal>
     state 3)
    (p test7-3
     =goal>
     isa goal
     test test7
     state 3
     ?manual>
     preparation free
     ==>
     +manual>
     isa delayed-punch
     hand left
     finger ring
     delay .22
       -goal>)
  
    (p test8
     =goal>
     isa goal
     test test8
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa hold-punch
     hand left
     finger index
     =goal>
     state 1)
  (p test8-1
     =goal>
     isa goal
     test test8
     state 1
     ?manual>
     preparation free
     ==>
     +manual>
     isa hold-punch
     hand right
     finger index
     =goal>
     state 2)
  
    (p test8-2
     =goal>
     isa goal
     test test8
     state 2
     ?manual>
     preparation free
     ==>
     +manual>
       isa hold-key
       key z
       
     =goal>
     state 3)
  
    (p test8-3
     =goal>
     isa goal
     test test8
     state 3
     ?manual>
     preparation free
     ==>
     +manual>
       isa hold-key
       key o
       
     =goal>
     state 4)
  
     (p test8-4
     =goal>
     isa goal
     test test8
     state 4
     ?manual>
     state free
     ==>
     +manual>
       isa all-fingers-to-home
       
     -goal>)
  
  
  (p test9
     =goal>
     isa goal
     test test9
     state nil
     ?manual>
     state free
     ==>
     +manual>
     isa hold-punch
     hand left
     finger index
     =goal>
     state 1)
  (p test9-1
     =goal>
     isa goal
     test test9
     state 1
     ?manual>
     preparation free
     ==>
     +manual>
     isa hold-punch
     hand right
     finger index
     =goal>
     state 2)
  
    (p test9-2
     =goal>
     isa goal
     test test9
     state 2
     ?manual>
     preparation free
     ==>
     +manual>
       isa hold-key
       key z
       
     =goal>
     state 3)
  
    (p test9-3
     =goal>
     isa goal
     test test9
     state 3
     ?manual>
     preparation free
     ==>
     +manual>
       isa hold-key
       key o
       
     =goal>
     state 4)
  
     (p test9-4
     =goal>
     isa goal
     test test9
     state 4
     ?manual>
     state free
     ==>
     +manual>
       isa release-all-fingers
       
        =goal>
        state 5)
     (p test9-5
     =goal>
     isa goal
     test test9
     state 5
     ?manual>
     state free
     ==>
     +manual>
       isa release-all-fingers
       
        -goal>)
  
  )
  

#|  Runs of all the test models for reference

CG-USER(3): (DOLIST (X '(TEST1 TEST2 TEST3 TEST4 TEST5 TEST6 TEST7 TEST8 TEST9)) (RUN-TEST X))
########################
"SAME hand 2 processors 2 executions query STATE"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER GOAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  PRESS-KEY KEY e
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  PREPARATION-COMPLETE 0.65 style PECK-RECOIL hand LEFT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  INITIATION-COMPLETE 0.65 style PECK-RECOIL hand LEFT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   KEYBOARD               output-key TEST-MOTOR e
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     0.990   KEYBOARD               release-key TEST-MOTOR e
     0.990   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   MOTOR                  FINISH-MOVEMENT 0.65 style PECK-RECOIL hand LEFT
     1.100   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   ------                 Stopped because no events left to process
########################
"SAME hand 2 processors 1 executions query STATE"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER GOAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  PRESS-KEY KEY e
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  PREPARATION-COMPLETE 0.65 style PECK-RECOIL hand LEFT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  INITIATION-COMPLETE 0.65 style PECK-RECOIL hand LEFT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   KEYBOARD               output-key TEST-MOTOR e
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     0.990   KEYBOARD               release-key TEST-MOTOR e
     0.990   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   MOTOR                  FINISH-MOVEMENT 0.65 style PECK-RECOIL hand LEFT
     1.100   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   ------                 Stopped because no events left to process
########################
"SAME hand 1 processors 2 executions query STATE"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER GOAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  PRESS-KEY KEY e
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  PREPARATION-COMPLETE 0.65 style PECK-RECOIL hand LEFT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  INITIATION-COMPLETE 0.65 style PECK-RECOIL hand LEFT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   KEYBOARD               output-key TEST-MOTOR e
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     0.990   KEYBOARD               release-key TEST-MOTOR e
     0.990   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   MOTOR                  FINISH-MOVEMENT 0.65 style PECK-RECOIL hand LEFT
     1.100   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   ------                 Stopped because no events left to process
########################
"SAME hand 1 processors 1 executions query STATE"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER GOAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  PRESS-KEY KEY e
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  PREPARATION-COMPLETE 0.65 style PECK-RECOIL hand LEFT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  INITIATION-COMPLETE 0.65 style PECK-RECOIL hand LEFT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   KEYBOARD               output-key TEST-MOTOR e
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     0.990   KEYBOARD               release-key TEST-MOTOR e
     0.990   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   MOTOR                  FINISH-MOVEMENT 0.65 style PECK-RECOIL hand LEFT
     1.100   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   ------                 Stopped because no events left to process
########################
"SAME hand 2 processors 2 executions query PROCESSOR"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER GOAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  PRESS-KEY KEY e
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  PREPARATION-COMPLETE 0.4 style PECK-RECOIL hand LEFT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.4 style PECK-RECOIL hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR e
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR e
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.4 style PECK-RECOIL hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"SAME hand 2 processors 1 executions query PROCESSOR"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER GOAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  PRESS-KEY KEY e
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  PREPARATION-COMPLETE 0.4 style PECK-RECOIL hand LEFT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.4 style PECK-RECOIL hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR e
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR e
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.4 style PECK-RECOIL hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"SAME hand 1 processors 2 executions query PROCESSOR"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER GOAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  PRESS-KEY KEY e
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  PREPARATION-COMPLETE 0.4 style PECK-RECOIL hand LEFT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.4 style PECK-RECOIL hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR e
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR e
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.4 style PECK-RECOIL hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"SAME hand 1 processors 1 executions query PROCESSOR"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER GOAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  PRESS-KEY KEY e
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  PREPARATION-COMPLETE 0.4 style PECK-RECOIL hand LEFT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.4 style PECK-RECOIL hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR e
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR e
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.4 style PECK-RECOIL hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"SAME hand 2 processors 2 executions query PREPARATION"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   MOTOR                  PRESS-KEY KEY e
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR e
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR e
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"SAME hand 2 processors 1 executions query PREPARATION"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   MOTOR                  PRESS-KEY KEY e
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR e
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR e
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"SAME hand 1 processors 2 executions query PREPARATION"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   MOTOR                  PRESS-KEY KEY e
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR e
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR e
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"SAME hand 1 processors 1 executions query PREPARATION"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-SAME
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-SAME
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   MOTOR                  PRESS-KEY KEY e
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR e
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR e
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"DIFF hand 2 processors 2 executions query STATE"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER GOAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  PRESS-KEY KEY i
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  PREPARATION-COMPLETE 0.65 style PECK-RECOIL hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.65 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     1.000   KEYBOARD               output-key TEST-MOTOR i
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.040   KEYBOARD               release-key TEST-MOTOR i
     1.040   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   MOTOR                  FINISH-MOVEMENT 0.65 style PECK-RECOIL hand RIGHT
     1.150   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   ------                 Stopped because no events left to process
########################
"DIFF hand 2 processors 1 executions query STATE"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER GOAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  PRESS-KEY KEY i
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  PREPARATION-COMPLETE 0.65 style PECK-RECOIL hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.65 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     1.000   KEYBOARD               output-key TEST-MOTOR i
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.040   KEYBOARD               release-key TEST-MOTOR i
     1.040   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   MOTOR                  FINISH-MOVEMENT 0.65 style PECK-RECOIL hand RIGHT
     1.150   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   ------                 Stopped because no events left to process
########################
"DIFF hand 1 processors 2 executions query STATE"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER GOAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  PRESS-KEY KEY i
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  PREPARATION-COMPLETE 0.65 style PECK-RECOIL hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.65 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     1.000   KEYBOARD               output-key TEST-MOTOR i
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.040   KEYBOARD               release-key TEST-MOTOR i
     1.040   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   MOTOR                  FINISH-MOVEMENT 0.65 style PECK-RECOIL hand RIGHT
     1.150   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   ------                 Stopped because no events left to process
########################
"DIFF hand 1 processors 1 executions query STATE"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER GOAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  PRESS-KEY KEY i
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  PREPARATION-COMPLETE 0.65 style PECK-RECOIL hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.65 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     1.000   KEYBOARD               output-key TEST-MOTOR i
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.040   KEYBOARD               release-key TEST-MOTOR i
     1.040   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   MOTOR                  FINISH-MOVEMENT 0.65 style PECK-RECOIL hand RIGHT
     1.150   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   ------                 Stopped because no events left to process
########################
"DIFF hand 2 processors 2 executions query PROCESSOR"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER GOAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  PRESS-KEY KEY i
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   MOTOR                  PREPARATION-COMPLETE 0.4 style PECK-RECOIL hand RIGHT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.4 style PECK-RECOIL hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR i
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR i
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.4 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"DIFF hand 2 processors 1 executions query PROCESSOR"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER GOAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  PRESS-KEY KEY i
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   MOTOR                  PREPARATION-COMPLETE 0.4 style PECK-RECOIL hand RIGHT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.4 style PECK-RECOIL hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR i
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR i
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.4 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"DIFF hand 1 processors 2 executions query PROCESSOR"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER GOAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  PRESS-KEY KEY i
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   MOTOR                  PREPARATION-COMPLETE 0.4 style PECK-RECOIL hand RIGHT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.4 style PECK-RECOIL hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR i
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR i
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.4 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"DIFF hand 1 processors 1 executions query PROCESSOR"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER GOAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  PRESS-KEY KEY i
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   MOTOR                  PREPARATION-COMPLETE 0.4 style PECK-RECOIL hand RIGHT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.4 style PECK-RECOIL hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR i
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR i
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.4 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"DIFF hand 2 processors 2 executions query PREPARATION"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   MOTOR                  PRESS-KEY KEY i
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand RIGHT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand RIGHT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.700   KEYBOARD               output-key TEST-MOTOR i
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.740   KEYBOARD               release-key TEST-MOTOR i
     0.740   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   ------                 Stopped because no events left to process
########################
"DIFF hand 2 processors 1 executions query PREPARATION"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   MOTOR                  PRESS-KEY KEY i
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand RIGHT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR i
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR i
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
########################
"DIFF hand 1 processors 2 executions query PREPARATION"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   MOTOR                  PRESS-KEY KEY i
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand RIGHT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand RIGHT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.700   KEYBOARD               output-key TEST-MOTOR i
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.740   KEYBOARD               release-key TEST-MOTOR i
     0.740   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   ------                 Stopped because no events left to process
########################
"DIFF hand 1 processors 1 executions query PREPARATION"
########################
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST1-FIRST-PRESS
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST1-FIRST-PRESS
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  PRESS-KEY KEY r
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST1-SECOND-PRESS-DIFF
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK-RECOIL hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST1-SECOND-PRESS-DIFF
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   MOTOR                  PRESS-KEY KEY i
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   KEYBOARD               output-key TEST-MOTOR r
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR r
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand RIGHT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.05 style PECK-RECOIL hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   KEYBOARD               output-key TEST-MOTOR i
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.790   KEYBOARD               release-key TEST-MOTOR i
     0.790   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   ------                 Stopped because no events left to process
#####
"SAME hand 2 processors 2 executions query STATE"
#####
 PRESSED:
     0.450 "r"        
     0.950 "e"        (0.500)
 RELEASED:
     0.490 "r"        
     0.990 "e"        (0.500)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 2 processors 1 executions query STATE"
#####
 PRESSED:
     0.450 "r"        
     0.950 "e"        (0.500)
 RELEASED:
     0.490 "r"        
     0.990 "e"        (0.500)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 1 processors 2 executions query STATE"
#####
 PRESSED:
     0.450 "r"        
     0.950 "e"        (0.500)
 RELEASED:
     0.490 "r"        
     0.990 "e"        (0.500)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 1 processors 1 executions query STATE"
#####
 PRESSED:
     0.450 "r"        
     0.950 "e"        (0.500)
 RELEASED:
     0.490 "r"        
     0.990 "e"        (0.500)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 2 processors 2 executions query PROCESSOR"
#####
 PRESSED:
     0.450 "r"        
     0.750 "e"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "e"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 2 processors 1 executions query PROCESSOR"
#####
 PRESSED:
     0.450 "r"        
     0.750 "e"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "e"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 1 processors 2 executions query PROCESSOR"
#####
 PRESSED:
     0.450 "r"        
     0.750 "e"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "e"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 1 processors 1 executions query PROCESSOR"
#####
 PRESSED:
     0.450 "r"        
     0.750 "e"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "e"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 2 processors 2 executions query PREPARATION"
#####
 PRESSED:
     0.450 "r"        
     0.750 "e"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "e"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 2 processors 1 executions query PREPARATION"
#####
 PRESSED:
     0.450 "r"        
     0.750 "e"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "e"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 1 processors 2 executions query PREPARATION"
#####
 PRESSED:
     0.450 "r"        
     0.750 "e"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "e"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"SAME hand 1 processors 1 executions query PREPARATION"
#####
 PRESSED:
     0.450 "r"        
     0.750 "e"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "e"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "e"        0.040
 Clicked:
#####
"DIFF hand 2 processors 2 executions query STATE"
#####
 PRESSED:
     0.450 "r"        
     1.000 "i"        (0.550)
 RELEASED:
     0.490 "r"        
     1.040 "i"        (0.550)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 2 processors 1 executions query STATE"
#####
 PRESSED:
     0.450 "r"        
     1.000 "i"        (0.550)
 RELEASED:
     0.490 "r"        
     1.040 "i"        (0.550)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 1 processors 2 executions query STATE"
#####
 PRESSED:
     0.450 "r"        
     1.000 "i"        (0.550)
 RELEASED:
     0.490 "r"        
     1.040 "i"        (0.550)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 1 processors 1 executions query STATE"
#####
 PRESSED:
     0.450 "r"        
     1.000 "i"        (0.550)
 RELEASED:
     0.490 "r"        
     1.040 "i"        (0.550)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 2 processors 2 executions query PROCESSOR"
#####
 PRESSED:
     0.450 "r"        
     0.750 "i"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "i"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 2 processors 1 executions query PROCESSOR"
#####
 PRESSED:
     0.450 "r"        
     0.750 "i"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "i"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 1 processors 2 executions query PROCESSOR"
#####
 PRESSED:
     0.450 "r"        
     0.750 "i"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "i"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 1 processors 1 executions query PROCESSOR"
#####
 PRESSED:
     0.450 "r"        
     0.750 "i"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "i"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 2 processors 2 executions query PREPARATION"
#####
 PRESSED:
     0.450 "r"        
     0.700 "i"        (0.250)
 RELEASED:
     0.490 "r"        
     0.740 "i"        (0.250)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 2 processors 1 executions query PREPARATION"
#####
 PRESSED:
     0.450 "r"        
     0.750 "i"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "i"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 1 processors 2 executions query PREPARATION"
#####
 PRESSED:
     0.450 "r"        
     0.700 "i"        (0.250)
 RELEASED:
     0.490 "r"        
     0.740 "i"        (0.250)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
#####
"DIFF hand 1 processors 1 executions query PREPARATION"
#####
 PRESSED:
     0.450 "r"        
     0.750 "i"        (0.300)
 RELEASED:
     0.490 "r"        
     0.790 "i"        (0.300)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "i"        0.040
 Clicked:
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST2-START
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST2-START
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  MOVE-CURSOR LOC LOC1
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   MOTOR                  PREPARATION-COMPLETE 0.05 style CURSOR-PLY hand RIGHT
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  INITIATION-COMPLETE 0.05 style CURSOR-PLY hand RIGHT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   MOUSE                  move-cursor TEST-MOTOR mouse (10 20 1080)
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  FINISH-MOVEMENT 0.05 style CURSOR-PLY hand RIGHT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   PROCEDURAL             PRODUCTION-SELECTED TEST2-CLICK
     0.450   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.450   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.500   PROCEDURAL             PRODUCTION-FIRED TEST2-CLICK
     0.500   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.500   PROCEDURAL             MODULE-REQUEST MANUAL
     0.500   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.500   MOTOR                  CLICK-MOUSE
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  PREPARATION-COMPLETE 0.5 style PUNCH hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.700   MOTOR                  INITIATION-COMPLETE 0.5 style PUNCH hand RIGHT
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.710   MOUSE                  click-mouse TEST-MOTOR (10 20 1080) INDEX
     0.710   PROCEDURAL             CONFLICT-RESOLUTION
     0.740   MOUSE                  release-mouse TEST-MOTOR (10 20 1080) INDEX
     0.740   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  FINISH-MOVEMENT 0.5 style PUNCH hand RIGHT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   PROCEDURAL             PRODUCTION-SELECTED TEST2-HOLD
     0.800   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.800   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.850   PROCEDURAL             PRODUCTION-FIRED TEST2-HOLD
     0.850   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.850   PROCEDURAL             MODULE-REQUEST MANUAL
     0.850   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.850   MOTOR                  PREPARATION-COMPLETE 0.85 style HOLD-PUNCH hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.85 style HOLD-PUNCH hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.910   MOUSE                  click-mouse TEST-MOTOR (10 20 1080) INDEX
     0.910   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   MOTOR                  FINISH-MOVEMENT 0.85 style HOLD-PUNCH hand RIGHT
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   PROCEDURAL             PRODUCTION-SELECTED TEST2-MOVE
     0.950   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.950   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.000   PROCEDURAL             PRODUCTION-FIRED TEST2-MOVE
     1.000   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.000   PROCEDURAL             MODULE-REQUEST MANUAL
     1.000   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.000   MOTOR                  MOVE-CURSOR LOC LOC2
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.200   MOTOR                  PREPARATION-COMPLETE 1.0 style CURSOR-PLY hand RIGHT
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.250   MOTOR                  INITIATION-COMPLETE 1.0 style CURSOR-PLY hand RIGHT
     1.250   PROCEDURAL             CONFLICT-RESOLUTION
     1.493   MOUSE                  move-cursor TEST-MOTOR mouse (100 40 1080)
     1.493   PROCEDURAL             CONFLICT-RESOLUTION
     1.543   MOTOR                  FINISH-MOVEMENT 1.0 style CURSOR-PLY hand RIGHT
     1.543   PROCEDURAL             CONFLICT-RESOLUTION
     1.543   PROCEDURAL             PRODUCTION-SELECTED TEST2-RELEASE
     1.543   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.543   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.593   PROCEDURAL             PRODUCTION-FIRED TEST2-RELEASE
     1.593   PROCEDURAL             MODULE-REQUEST MANUAL
     1.593   PROCEDURAL             CLEAR-BUFFER GOAL
     1.593   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.593   PROCEDURAL             CONFLICT-RESOLUTION
     1.743   MOTOR                  PREPARATION-COMPLETE 1.593 style RELEASE hand RIGHT
     1.743   PROCEDURAL             CONFLICT-RESOLUTION
     1.793   MOTOR                  INITIATION-COMPLETE 1.593 style RELEASE hand RIGHT
     1.793   PROCEDURAL             CONFLICT-RESOLUTION
     1.833   MOUSE                  release-mouse TEST-MOTOR (100 40 1080) INDEX
     1.833   PROCEDURAL             CONFLICT-RESOLUTION
     1.843   MOTOR                  FINISH-MOVEMENT 1.593 style RELEASE hand RIGHT
     1.843   PROCEDURAL             CONFLICT-RESOLUTION
     1.843   ------                 Stopped because no events left to process
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST2-START
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST2-START
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  MOVE-CURSOR LOC LOC1
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   MOTOR                  PREPARATION-COMPLETE 0.05 style CURSOR-PLY hand RIGHT
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  INITIATION-COMPLETE 0.05 style CURSOR-PLY hand RIGHT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   MOUSE                  move-cursor TEST-MOTOR mouse (10 20 1080)
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  FINISH-MOVEMENT 0.05 style CURSOR-PLY hand RIGHT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   PROCEDURAL             PRODUCTION-SELECTED TEST2-CLICK
     0.450   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.450   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.500   PROCEDURAL             PRODUCTION-FIRED TEST2-CLICK
     0.500   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.500   PROCEDURAL             MODULE-REQUEST MANUAL
     0.500   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.500   MOTOR                  CLICK-MOUSE
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  PREPARATION-COMPLETE 0.5 style PUNCH hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.700   MOTOR                  INITIATION-COMPLETE 0.5 style PUNCH hand RIGHT
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.710   MOUSE                  click-mouse TEST-MOTOR (10 20 1080) INDEX
     0.710   PROCEDURAL             CONFLICT-RESOLUTION
     0.740   MOUSE                  release-mouse TEST-MOTOR (10 20 1080) INDEX
     0.740   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  FINISH-MOVEMENT 0.5 style PUNCH hand RIGHT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   PROCEDURAL             PRODUCTION-SELECTED TEST2-HOLD
     0.800   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.800   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.850   PROCEDURAL             PRODUCTION-FIRED TEST2-HOLD
     0.850   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.850   PROCEDURAL             MODULE-REQUEST MANUAL
     0.850   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.850   MOTOR                  PREPARATION-COMPLETE 0.85 style HOLD-PUNCH hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.85 style HOLD-PUNCH hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.910   MOUSE                  click-mouse TEST-MOTOR (10 20 1080) INDEX
     0.910   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   MOTOR                  FINISH-MOVEMENT 0.85 style HOLD-PUNCH hand RIGHT
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   PROCEDURAL             PRODUCTION-SELECTED TEST2-MOVE
     0.950   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.950   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.000   PROCEDURAL             PRODUCTION-FIRED TEST2-MOVE
     1.000   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.000   PROCEDURAL             MODULE-REQUEST MANUAL
     1.000   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.000   MOTOR                  MOVE-CURSOR LOC LOC2
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.200   MOTOR                  PREPARATION-COMPLETE 1.0 style CURSOR-PLY hand RIGHT
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.250   MOTOR                  INITIATION-COMPLETE 1.0 style CURSOR-PLY hand RIGHT
     1.250   PROCEDURAL             CONFLICT-RESOLUTION
     1.673   MOUSE                  move-cursor TEST-MOTOR mouse (100 40 1080)
     1.673   PROCEDURAL             CONFLICT-RESOLUTION
     1.723   MOTOR                  FINISH-MOVEMENT 1.0 style CURSOR-PLY hand RIGHT
     1.723   PROCEDURAL             CONFLICT-RESOLUTION
     1.723   PROCEDURAL             PRODUCTION-SELECTED TEST2-RELEASE
     1.723   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.723   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.773   PROCEDURAL             PRODUCTION-FIRED TEST2-RELEASE
     1.773   PROCEDURAL             MODULE-REQUEST MANUAL
     1.773   PROCEDURAL             CLEAR-BUFFER GOAL
     1.773   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.773   PROCEDURAL             CONFLICT-RESOLUTION
     1.923   MOTOR                  PREPARATION-COMPLETE 1.773 style RELEASE hand RIGHT
     1.923   PROCEDURAL             CONFLICT-RESOLUTION
     1.973   MOTOR                  INITIATION-COMPLETE 1.773 style RELEASE hand RIGHT
     1.973   PROCEDURAL             CONFLICT-RESOLUTION
     2.013   MOUSE                  release-mouse TEST-MOTOR (100 40 1080) INDEX
     2.013   PROCEDURAL             CONFLICT-RESOLUTION
     2.023   MOTOR                  FINISH-MOVEMENT 1.773 style RELEASE hand RIGHT
     2.023   PROCEDURAL             CONFLICT-RESOLUTION
     2.023   ------                 Stopped because no events left to process
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST2-START
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST2-START
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  MOVE-CURSOR LOC LOC1
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   MOTOR                  PREPARATION-COMPLETE 0.05 style CURSOR-PLY hand RIGHT
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  INITIATION-COMPLETE 0.05 style CURSOR-PLY hand RIGHT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   MOUSE                  move-cursor TEST-MOTOR mouse (10 20 1080)
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  FINISH-MOVEMENT 0.05 style CURSOR-PLY hand RIGHT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   PROCEDURAL             PRODUCTION-SELECTED TEST2-CLICK
     0.450   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.450   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.500   PROCEDURAL             PRODUCTION-FIRED TEST2-CLICK
     0.500   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.500   PROCEDURAL             MODULE-REQUEST MANUAL
     0.500   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.500   MOTOR                  CLICK-MOUSE
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  PREPARATION-COMPLETE 0.5 style PUNCH hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.700   MOTOR                  INITIATION-COMPLETE 0.5 style PUNCH hand RIGHT
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.710   MOUSE                  click-mouse TEST-MOTOR (10 20 1080) INDEX
     0.710   PROCEDURAL             CONFLICT-RESOLUTION
     0.740   MOUSE                  release-mouse TEST-MOTOR (10 20 1080) INDEX
     0.740   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  FINISH-MOVEMENT 0.5 style PUNCH hand RIGHT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   PROCEDURAL             PRODUCTION-SELECTED TEST2-HOLD
     0.800   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.800   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.850   PROCEDURAL             PRODUCTION-FIRED TEST2-HOLD
     0.850   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.850   PROCEDURAL             MODULE-REQUEST MANUAL
     0.850   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.850   MOTOR                  PREPARATION-COMPLETE 0.85 style HOLD-PUNCH hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.85 style HOLD-PUNCH hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.910   MOUSE                  click-mouse TEST-MOTOR (10 20 1080) INDEX
     0.910   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   MOTOR                  FINISH-MOVEMENT 0.85 style HOLD-PUNCH hand RIGHT
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   PROCEDURAL             PRODUCTION-SELECTED TEST2-MOVE
     0.950   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.950   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.000   PROCEDURAL             PRODUCTION-FIRED TEST2-MOVE
     1.000   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.000   PROCEDURAL             MODULE-REQUEST MANUAL
     1.000   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.000   MOTOR                  MOVE-CURSOR LOC LOC2
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.200   MOTOR                  PREPARATION-COMPLETE 1.0 style CURSOR-PLY hand RIGHT
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.250   MOTOR                  INITIATION-COMPLETE 1.0 style CURSOR-PLY hand RIGHT
     1.250   PROCEDURAL             CONFLICT-RESOLUTION
     1.594   MOUSE                  move-cursor TEST-MOTOR mouse (100 40 1080)
     1.594   PROCEDURAL             CONFLICT-RESOLUTION
     1.644   MOTOR                  FINISH-MOVEMENT 1.0 style CURSOR-PLY hand RIGHT
     1.644   PROCEDURAL             CONFLICT-RESOLUTION
     1.644   PROCEDURAL             PRODUCTION-SELECTED TEST2-RELEASE
     1.644   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.644   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.694   PROCEDURAL             PRODUCTION-FIRED TEST2-RELEASE
     1.694   PROCEDURAL             MODULE-REQUEST MANUAL
     1.694   PROCEDURAL             CLEAR-BUFFER GOAL
     1.694   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.694   PROCEDURAL             CONFLICT-RESOLUTION
     1.844   MOTOR                  PREPARATION-COMPLETE 1.694 style RELEASE hand RIGHT
     1.844   PROCEDURAL             CONFLICT-RESOLUTION
     1.894   MOTOR                  INITIATION-COMPLETE 1.694 style RELEASE hand RIGHT
     1.894   PROCEDURAL             CONFLICT-RESOLUTION
     1.934   MOUSE                  release-mouse TEST-MOTOR (100 40 1080) INDEX
     1.934   PROCEDURAL             CONFLICT-RESOLUTION
     1.944   MOTOR                  FINISH-MOVEMENT 1.694 style RELEASE hand RIGHT
     1.944   PROCEDURAL             CONFLICT-RESOLUTION
     1.944   ------                 Stopped because no events left to process
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST2-START
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST2-START
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  MOVE-CURSOR LOC LOC1
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   MOTOR                  PREPARATION-COMPLETE 0.05 style CURSOR-PLY hand RIGHT
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  INITIATION-COMPLETE 0.05 style CURSOR-PLY hand RIGHT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   MOUSE                  move-cursor TEST-MOTOR mouse (10 20 1080)
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  FINISH-MOVEMENT 0.05 style CURSOR-PLY hand RIGHT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   PROCEDURAL             PRODUCTION-SELECTED TEST2-CLICK
     0.450   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.450   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.500   PROCEDURAL             PRODUCTION-FIRED TEST2-CLICK
     0.500   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.500   PROCEDURAL             MODULE-REQUEST MANUAL
     0.500   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.500   MOTOR                  CLICK-MOUSE
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  PREPARATION-COMPLETE 0.5 style PUNCH hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.700   MOTOR                  INITIATION-COMPLETE 0.5 style PUNCH hand RIGHT
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.710   MOUSE                  click-mouse TEST-MOTOR (10 20 1080) INDEX
     0.710   PROCEDURAL             CONFLICT-RESOLUTION
     0.740   MOUSE                  release-mouse TEST-MOTOR (10 20 1080) INDEX
     0.740   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  FINISH-MOVEMENT 0.5 style PUNCH hand RIGHT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   PROCEDURAL             PRODUCTION-SELECTED TEST2-HOLD
     0.800   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.800   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.850   PROCEDURAL             PRODUCTION-FIRED TEST2-HOLD
     0.850   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.850   PROCEDURAL             MODULE-REQUEST MANUAL
     0.850   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.850   MOTOR                  PREPARATION-COMPLETE 0.85 style HOLD-PUNCH hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.85 style HOLD-PUNCH hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.910   MOUSE                  click-mouse TEST-MOTOR (10 20 1080) INDEX
     0.910   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   MOTOR                  FINISH-MOVEMENT 0.85 style HOLD-PUNCH hand RIGHT
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   PROCEDURAL             PRODUCTION-SELECTED TEST2-MOVE
     0.950   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.950   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.000   PROCEDURAL             PRODUCTION-FIRED TEST2-MOVE
     1.000   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.000   PROCEDURAL             MODULE-REQUEST MANUAL
     1.000   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.000   MOTOR                  MOVE-CURSOR LOC LOC2
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.200   MOTOR                  PREPARATION-COMPLETE 1.0 style CURSOR-PLY hand RIGHT
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.250   MOTOR                  INITIATION-COMPLETE 1.0 style CURSOR-PLY hand RIGHT
     1.250   PROCEDURAL             CONFLICT-RESOLUTION
     1.736   MOUSE                  move-cursor TEST-MOTOR mouse (100 40 1080)
     1.736   PROCEDURAL             CONFLICT-RESOLUTION
     1.786   MOTOR                  FINISH-MOVEMENT 1.0 style CURSOR-PLY hand RIGHT
     1.786   PROCEDURAL             CONFLICT-RESOLUTION
     1.786   PROCEDURAL             PRODUCTION-SELECTED TEST2-RELEASE
     1.786   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.786   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.836   PROCEDURAL             PRODUCTION-FIRED TEST2-RELEASE
     1.836   PROCEDURAL             MODULE-REQUEST MANUAL
     1.836   PROCEDURAL             CLEAR-BUFFER GOAL
     1.836   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.836   PROCEDURAL             CONFLICT-RESOLUTION
     1.986   MOTOR                  PREPARATION-COMPLETE 1.836 style RELEASE hand RIGHT
     1.986   PROCEDURAL             CONFLICT-RESOLUTION
     2.036   MOTOR                  INITIATION-COMPLETE 1.836 style RELEASE hand RIGHT
     2.036   PROCEDURAL             CONFLICT-RESOLUTION
     2.076   MOUSE                  release-mouse TEST-MOTOR (100 40 1080) INDEX
     2.076   PROCEDURAL             CONFLICT-RESOLUTION
     2.086   MOTOR                  FINISH-MOVEMENT 1.836 style RELEASE hand RIGHT
     2.086   PROCEDURAL             CONFLICT-RESOLUTION
     2.086   ------                 Stopped because no events left to process
#####
"click and hold mouse *cdd* NIL"
#####
 PRESSED:
 RELEASED:
 CLICKED:
     0.710 (10 20 1080) 
     0.910 (10 20 1080) (0.200)
 UNCLICKED:
     0.740 (10 20 1080) 
     1.833 (100 40 1080) (1.093)
 Held:
 Clicked:
  (10 20 1080) (10 20 1080) 0.030
  (10 20 1080) (100 40 1080) 0.923
#####
"click and hold mouse *cdd* T"
#####
 PRESSED:
 RELEASED:
 CLICKED:
     0.710 (10 20 1080) 
     0.910 (10 20 1080) (0.200)
 UNCLICKED:
     0.740 (10 20 1080) 
     2.013 (100 40 1080) (1.273)
 Held:
 Clicked:
  (10 20 1080) (10 20 1080) 0.030
  (10 20 1080) (100 40 1080) 1.103
#####
"click and hold mouse *cdd* 0.5"
#####
 PRESSED:
 RELEASED:
 CLICKED:
     0.710 (10 20 1080) 
     0.910 (10 20 1080) (0.200)
 UNCLICKED:
     0.740 (10 20 1080) 
     1.934 (100 40 1080) (1.194)
 Held:
 Clicked:
  (10 20 1080) (10 20 1080) 0.030
  (10 20 1080) (100 40 1080) 1.024
#####
"click and hold mouse *cdd* 1.0"
#####
 PRESSED:
 RELEASED:
 CLICKED:
     0.710 (10 20 1080) 
     0.910 (10 20 1080) (0.200)
 UNCLICKED:
     0.740 (10 20 1080) 
     2.076 (100 40 1080) (1.336)
 Held:
 Clicked:
  (10 20 1080) (10 20 1080) 0.030
  (10 20 1080) (100 40 1080) 1.166
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST3-START
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST3-START
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style HOLD-PECK hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST3-HOLD-K
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style HOLD-PECK hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST3-HOLD-K
     0.350   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  FINISH-MOVEMENT 0.05 style HOLD-PECK hand LEFT
     0.450   KEYBOARD               output-key TEST-MOTOR shift
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   MOTOR                  PREPARATION-COMPLETE 0.35 style HOLD-PUNCH hand RIGHT
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   ------                 Stopped because time limit reached
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : NIL
  state busy            : T
  state error           : NIL
  processor free        : NIL
  processor busy        : T
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PECK
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : T
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : NIL
  state busy            : T
  state error           : NIL
  processor free        : NIL
  processor busy        : T
  execution free        : NIL
  execution busy        : T
  last-command          : HOLD-PUNCH
  index  free           : T
  index  down           : NIL
  middle free           : NIL
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

     0.550   MOTOR                  INITIATION-COMPLETE 0.35 style HOLD-PUNCH hand RIGHT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.575   KEYBOARD               output-key TEST-MOTOR k
     0.575   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  FINISH-MOVEMENT 0.35 style HOLD-PUNCH hand RIGHT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   PROCEDURAL             PRODUCTION-SELECTED TEST3-TO-MOUSE
     0.600   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.600   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.650   PROCEDURAL             PRODUCTION-FIRED TEST3-TO-MOUSE
     0.650   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.650   PROCEDURAL             MODULE-REQUEST MANUAL
     0.650   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.650   MOTOR                  HAND-TO-MOUSE
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  PREPARATION-COMPLETE 0.65 style HAND-PLY hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  INITIATION-COMPLETE 0.65 style HAND-PLY hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.925   KEYBOARD               release-key TEST-MOTOR k
     0.925   PROCEDURAL             CONFLICT-RESOLUTION
     1.153   MOTOR                  MOVE-A-HAND RIGHT 21.095022 -0.094951704
     1.153   PROCEDURAL             CONFLICT-RESOLUTION
     1.203   MOTOR                  FINISH-MOVEMENT 0.65 style HAND-PLY hand RIGHT
     1.203   PROCEDURAL             CONFLICT-RESOLUTION
     1.203   PROCEDURAL             PRODUCTION-SELECTED TEST3-HOLD
     1.203   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.203   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.250   ------                 Stopped because time limit reached
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PECK
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : T
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HAND-PLY
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

     1.253   PROCEDURAL             PRODUCTION-FIRED TEST3-HOLD
     1.253   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.253   PROCEDURAL             MODULE-REQUEST MANUAL
     1.253   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.253   PROCEDURAL             CONFLICT-RESOLUTION
     1.403   MOTOR                  PREPARATION-COMPLETE 1.253 style HOLD-PUNCH hand RIGHT
     1.403   PROCEDURAL             CONFLICT-RESOLUTION
     1.453   MOTOR                  INITIATION-COMPLETE 1.253 style HOLD-PUNCH hand RIGHT
     1.453   PROCEDURAL             CONFLICT-RESOLUTION
     1.478   MOUSE                  click-mouse TEST-MOTOR (0 0 1080) INDEX
     1.478   PROCEDURAL             CONFLICT-RESOLUTION
     1.503   MOTOR                  FINISH-MOVEMENT 1.253 style HOLD-PUNCH hand RIGHT
     1.503   PROCEDURAL             CONFLICT-RESOLUTION
     1.503   PROCEDURAL             PRODUCTION-SELECTED TEST3-KEYS
     1.503   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.503   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.553   PROCEDURAL             PRODUCTION-FIRED TEST3-KEYS
     1.553   PROCEDURAL             MODULE-REQUEST MANUAL
     1.553   PROCEDURAL             CLEAR-BUFFER GOAL
     1.553   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.553   MOTOR                  HAND-TO-HOME 
     1.553   PROCEDURAL             CONFLICT-RESOLUTION
     1.753   MOTOR                  PREPARATION-COMPLETE 1.553 style HAND-PLY hand RIGHT
     1.753   PROCEDURAL             CONFLICT-RESOLUTION
     1.803   MOTOR                  INITIATION-COMPLETE 1.553 style HAND-PLY hand RIGHT
     1.803   PROCEDURAL             CONFLICT-RESOLUTION
     1.828   MOUSE                  release-mouse TEST-MOTOR (0 0 1080) INDEX
     1.828   PROCEDURAL             CONFLICT-RESOLUTION
     2.056   MOTOR                  MOVE-A-HAND RIGHT 21.095022 3.0466409
     2.056   PROCEDURAL             CONFLICT-RESOLUTION
     2.106   MOTOR                  FINISH-MOVEMENT 1.553 style HAND-PLY hand RIGHT
     2.106   PROCEDURAL             CONFLICT-RESOLUTION
     2.106   ------                 Stopped because no events left to process
#####
"Auto release key & mouse when moving hand"
#####
 PRESSED:
     0.450 "shift"    
     0.575 "k"        (0.125)
 RELEASED:
     0.925 "k"        
 CLICKED:
     1.478 (0 0 1080) 
 UNCLICKED:
     1.828 (0 0 1080) 
 Held:
  "shift"    
  "k"        0.350
 Clicked:
  (0 0 1080) (0 0 1080) 0.350
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST4
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST4
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style PECK hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST4-1
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style PECK hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST4-1
     0.350   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK hand LEFT
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   PROCEDURAL             PRODUCTION-SELECTED TEST4-2
     0.400   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.400   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.432   KEYBOARD               output-key TEST-MOTOR t
     0.450   PROCEDURAL             PRODUCTION-FIRED TEST4-2
     0.450   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.450   PROCEDURAL             MODULE-REQUEST MANUAL
     0.450   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.490   KEYBOARD               release-key TEST-MOTOR t
     0.490   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   MOTOR                  FINISH-MOVEMENT 0.05 style PECK hand LEFT
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.550   MOTOR                  INITIATION-COMPLETE 0.35 style PECK hand LEFT
     0.550   PROCEDURAL             CONFLICT-RESOLUTION
     0.627   KEYBOARD               output-key TEST-MOTOR r
     0.627   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  PREPARATION-COMPLETE 0.45 style PECK hand RIGHT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   PROCEDURAL             PRODUCTION-SELECTED TEST4-3
     0.650   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.650   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.690   KEYBOARD               release-key TEST-MOTOR r
     0.700   MOTOR                  FINISH-MOVEMENT 0.35 style PECK hand LEFT
     0.700   MOTOR                  INITIATION-COMPLETE 0.45 style PECK hand RIGHT
     0.700   PROCEDURAL             PRODUCTION-FIRED TEST4-3
     0.700   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.700   PROCEDURAL             MODULE-REQUEST MANUAL
     0.700   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.777   KEYBOARD               output-key TEST-MOTOR i
     0.777   PROCEDURAL             CONFLICT-RESOLUTION
     0.840   KEYBOARD               release-key TEST-MOTOR i
     0.840   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  FINISH-MOVEMENT 0.45 style PECK hand RIGHT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  PREPARATION-COMPLETE 0.7 style PECK hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   PROCEDURAL             PRODUCTION-SELECTED TEST4-4
     0.900   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.900   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.950   MOTOR                  INITIATION-COMPLETE 0.7 style PECK hand LEFT
     0.950   PROCEDURAL             PRODUCTION-FIRED TEST4-4
     0.950   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.950   PROCEDURAL             MODULE-REQUEST MANUAL
     0.950   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     1.027   KEYBOARD               output-key TEST-MOTOR f
     1.027   PROCEDURAL             CONFLICT-RESOLUTION
     1.090   KEYBOARD               release-key TEST-MOTOR f
     1.090   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   MOTOR                  FINISH-MOVEMENT 0.7 style PECK hand LEFT
     1.100   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   MOTOR                  PREPARATION-COMPLETE 0.95 style PECK hand RIGHT
     1.150   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   PROCEDURAL             PRODUCTION-SELECTED TEST4-5
     1.150   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.150   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.200   MOTOR                  INITIATION-COMPLETE 0.95 style PECK hand RIGHT
     1.200   PROCEDURAL             PRODUCTION-FIRED TEST4-5
     1.200   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.200   PROCEDURAL             MODULE-REQUEST MANUAL
     1.200   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.282   KEYBOARD               output-key TEST-MOTOR n
     1.282   PROCEDURAL             CONFLICT-RESOLUTION
     1.340   KEYBOARD               release-key TEST-MOTOR n
     1.340   PROCEDURAL             CONFLICT-RESOLUTION
     1.350   MOTOR                  FINISH-MOVEMENT 0.95 style PECK hand RIGHT
     1.350   PROCEDURAL             CONFLICT-RESOLUTION
     1.400   MOTOR                  PREPARATION-COMPLETE 1.2 style HOLD-PECK hand LEFT
     1.400   PROCEDURAL             CONFLICT-RESOLUTION
     1.400   PROCEDURAL             PRODUCTION-SELECTED TEST4-6
     1.400   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.400   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.450   MOTOR                  INITIATION-COMPLETE 1.2 style HOLD-PECK hand LEFT
     1.450   PROCEDURAL             PRODUCTION-FIRED TEST4-6
     1.450   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.450   PROCEDURAL             MODULE-REQUEST MANUAL
     1.450   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.450   PROCEDURAL             CONFLICT-RESOLUTION
     1.527   KEYBOARD               output-key TEST-MOTOR x
     1.527   PROCEDURAL             CONFLICT-RESOLUTION
     1.550   MOTOR                  FINISH-MOVEMENT 1.2 style HOLD-PECK hand LEFT
     1.550   PROCEDURAL             CONFLICT-RESOLUTION
     1.650   MOTOR                  PREPARATION-COMPLETE 1.45 style POINT-FINGER hand RIGHT
     1.650   PROCEDURAL             CONFLICT-RESOLUTION
     1.700   MOTOR                  INITIATION-COMPLETE 1.45 style POINT-FINGER hand RIGHT
     1.700   PROCEDURAL             CONFLICT-RESOLUTION
     1.750   PROCEDURAL             CONFLICT-RESOLUTION
     1.800   MOTOR                  FINISH-MOVEMENT 1.45 style POINT-FINGER hand RIGHT
     1.800   PROCEDURAL             CONFLICT-RESOLUTION
     1.800   PROCEDURAL             PRODUCTION-SELECTED TEST4-7
(P TEST4-7
   =GOAL>
       TEST TEST4
       STATE 7
   ?MANUAL>
       STATE FREE
 ==>
   +MANUAL>
       CMD ALL-FINGERS-TO-HOME
   -GOAL>
)
     1.800   ------                 BREAK-EVENT PRODUCTION TEST4-7
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PECK
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : T
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : POINT-FINGER
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(4 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 5)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(7 4)
  RIGHT MIDDLE: #(8 3)
  RIGHT RING: #(9 4)
  RIGHT PINKIE: #(10 4)
  RIGHT THUMB: #(6 6)
     1.800   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.800   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.850   PROCEDURAL             PRODUCTION-FIRED TEST4-7
     1.850   PROCEDURAL             MODULE-REQUEST MANUAL
     1.850   PROCEDURAL             CLEAR-BUFFER GOAL
     1.850   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.850   PROCEDURAL             CONFLICT-RESOLUTION
     1.950   MOTOR                  PREPARATION-COMPLETE 1.85 style ALL-FINGERS-TO-HOME hand BOTH
     1.950   PROCEDURAL             CONFLICT-RESOLUTION
     2.000   MOTOR                  INITIATION-COMPLETE 1.85 style ALL-FINGERS-TO-HOME hand BOTH
     2.000   PROCEDURAL             CONFLICT-RESOLUTION
     2.040   KEYBOARD               release-key TEST-MOTOR x
     2.040   PROCEDURAL             CONFLICT-RESOLUTION
     2.150   MOTOR                  ALL-FINGERS-TO-HOME LEFT
     2.150   MOTOR                  ALL-FINGERS-TO-HOME RIGHT
     2.150   PROCEDURAL             CONFLICT-RESOLUTION
     2.200   MOTOR                  FINISH-MOVEMENT 1.85 style ALL-FINGERS-TO-HOME hand BOTH
     2.200   PROCEDURAL             CONFLICT-RESOLUTION
     2.200   ------                 Stopped because no events left to process
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : ALL-FINGERS-TO-HOME
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : ALL-FINGERS-TO-HOME
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(4 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 4)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(7 4)
  RIGHT MIDDLE: #(8 4)
  RIGHT RING: #(9 4)
  RIGHT PINKIE: #(10 4)
  RIGHT THUMB: #(6 6)
#####
"Some various key pressing"
#####
 PRESSED:
     0.432 "t"        
     0.627 "r"        (0.195)
     0.777 "i"        (0.150)
     1.027 "f"        (0.250)
     1.282 "n"        (0.255)
     1.527 "x"        (0.245)
 RELEASED:
     0.490 "t"        
     0.690 "r"        (0.200)
     0.840 "i"        (0.150)
     1.090 "f"        (0.250)
     1.340 "n"        (0.250)
     2.040 "x"        (0.700)
 CLICKED:
 UNCLICKED:
 Held:
  "t"        0.058
  "r"        0.063
  "i"        0.063
  "f"        0.063
  "n"        0.058
  "x"        0.513
 Clicked:
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST5
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST5
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style POINT-FINGER hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style POINT-FINGER hand LEFT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  FINISH-MOVEMENT 0.05 style POINT-FINGER hand LEFT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   PROCEDURAL             PRODUCTION-SELECTED TEST5-1
     0.450   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.450   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.500   PROCEDURAL             PRODUCTION-FIRED TEST5-1
     0.500   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.500   PROCEDURAL             MODULE-REQUEST MANUAL
     0.500   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.500   MOTOR                  HOLD-PUNCH HAND LEFT FINGER RING
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  PREPARATION-COMPLETE 0.5 style HOLD-PUNCH hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.700   MOTOR                  INITIATION-COMPLETE 0.5 style HOLD-PUNCH hand LEFT
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.710   KEYBOARD               output-key TEST-MOTOR w
     0.710   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   MOTOR                  FINISH-MOVEMENT 0.5 style HOLD-PUNCH hand LEFT
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   PROCEDURAL             PRODUCTION-SELECTED TEST5-2
     0.750   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.750   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.800   PROCEDURAL             PRODUCTION-FIRED TEST5-2
     0.800   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.800   PROCEDURAL             MODULE-REQUEST MANUAL
     0.800   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.800   MOTOR                  PREPARATION-COMPLETE 0.8 style RELEASE hand LEFT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.850   MOTOR                  INITIATION-COMPLETE 0.8 style RELEASE hand LEFT
     0.850   PROCEDURAL             CONFLICT-RESOLUTION
     0.890   KEYBOARD               release-key TEST-MOTOR w
     0.890   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.8 style RELEASE hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   PROCEDURAL             PRODUCTION-SELECTED TEST5-3
     0.900   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.900   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.950   PROCEDURAL             PRODUCTION-FIRED TEST5-3
     0.950   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.950   PROCEDURAL             MODULE-REQUEST MANUAL
     0.950   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.950   MOTOR                  PREPARATION-COMPLETE 0.95 style PUNCH hand LEFT
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     1.000   MOTOR                  INITIATION-COMPLETE 0.95 style PUNCH hand LEFT
     1.000   PROCEDURAL             CONFLICT-RESOLUTION
     1.010   KEYBOARD               output-key TEST-MOTOR w
     1.010   PROCEDURAL             CONFLICT-RESOLUTION
     1.040   KEYBOARD               release-key TEST-MOTOR w
     1.040   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   MOTOR                  FINISH-MOVEMENT 0.95 style PUNCH hand LEFT
     1.100   PROCEDURAL             CONFLICT-RESOLUTION
     1.100   PROCEDURAL             PRODUCTION-SELECTED TEST5-4
     1.100   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.100   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.150   PROCEDURAL             PRODUCTION-FIRED TEST5-4
     1.150   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.150   PROCEDURAL             MODULE-REQUEST MANUAL
     1.150   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.150   MOTOR                  PREPARATION-COMPLETE 1.15 style HOLD-PUNCH hand LEFT
     1.150   PROCEDURAL             CONFLICT-RESOLUTION
     1.200   MOTOR                  INITIATION-COMPLETE 1.15 style HOLD-PUNCH hand LEFT
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.210   KEYBOARD               output-key TEST-MOTOR w
     1.210   PROCEDURAL             CONFLICT-RESOLUTION
     1.250   MOTOR                  FINISH-MOVEMENT 1.15 style HOLD-PUNCH hand LEFT
     1.250   PROCEDURAL             CONFLICT-RESOLUTION
     1.250   PROCEDURAL             PRODUCTION-SELECTED TEST5-5
(P TEST5-5
   =GOAL>
       TEST TEST5
       STATE 5
   ?MANUAL>
       STATE FREE
 ==>
   +MANUAL>
       KEY "w"
       CMD RELEASE-KEY-TO-HOME
   =GOAL>
       STATE 6
)
     1.250   ------                 BREAK-EVENT PRODUCTION TEST5-5
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PUNCH
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : T
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : NONE
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(4 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 3)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(7 4)
  RIGHT MIDDLE: #(8 4)
  RIGHT RING: #(9 4)
  RIGHT PINKIE: #(10 4)
  RIGHT THUMB: #(6 6)
     1.250   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.250   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.300   PROCEDURAL             PRODUCTION-FIRED TEST5-5
     1.300   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.300   PROCEDURAL             MODULE-REQUEST MANUAL
     1.300   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.300   PROCEDURAL             CONFLICT-RESOLUTION
     1.550   MOTOR                  PREPARATION-COMPLETE 1.3 style RELEASE-RECOIL hand LEFT
     1.550   PROCEDURAL             CONFLICT-RESOLUTION
     1.600   MOTOR                  INITIATION-COMPLETE 1.3 style RELEASE-RECOIL hand LEFT
     1.600   PROCEDURAL             CONFLICT-RESOLUTION
     1.640   KEYBOARD               release-key TEST-MOTOR w
     1.640   PROCEDURAL             CONFLICT-RESOLUTION
     1.750   MOTOR                  FINISH-MOVEMENT 1.3 style RELEASE-RECOIL hand LEFT
     1.750   PROCEDURAL             CONFLICT-RESOLUTION
     1.750   PROCEDURAL             PRODUCTION-SELECTED TEST5-6
     1.750   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.750   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.800   PROCEDURAL             PRODUCTION-FIRED TEST5-6
     1.800   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.800   PROCEDURAL             MODULE-REQUEST MANUAL
     1.800   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.800   PROCEDURAL             CONFLICT-RESOLUTION
     1.950   MOTOR                  PREPARATION-COMPLETE 1.8 style POINT-FINGER hand LEFT
     1.950   PROCEDURAL             CONFLICT-RESOLUTION
     2.000   MOTOR                  INITIATION-COMPLETE 1.8 style POINT-FINGER hand LEFT
     2.000   PROCEDURAL             CONFLICT-RESOLUTION
     2.050   PROCEDURAL             CONFLICT-RESOLUTION
     2.100   MOTOR                  FINISH-MOVEMENT 1.8 style POINT-FINGER hand LEFT
     2.100   PROCEDURAL             CONFLICT-RESOLUTION
     2.100   PROCEDURAL             PRODUCTION-SELECTED TEST5-7
     2.100   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.100   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.150   PROCEDURAL             PRODUCTION-FIRED TEST5-7
     2.150   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.150   PROCEDURAL             MODULE-REQUEST MANUAL
     2.150   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.150   PROCEDURAL             CONFLICT-RESOLUTION
     2.300   MOTOR                  PREPARATION-COMPLETE 2.15 style HOLD-PUNCH hand LEFT
     2.300   PROCEDURAL             CONFLICT-RESOLUTION
     2.350   MOTOR                  INITIATION-COMPLETE 2.15 style HOLD-PUNCH hand LEFT
     2.350   PROCEDURAL             CONFLICT-RESOLUTION
     2.360   KEYBOARD               output-key TEST-MOTOR g
     2.360   PROCEDURAL             CONFLICT-RESOLUTION
     2.400   MOTOR                  FINISH-MOVEMENT 2.15 style HOLD-PUNCH hand LEFT
     2.400   PROCEDURAL             CONFLICT-RESOLUTION
     2.400   PROCEDURAL             PRODUCTION-SELECTED TEST5-8
     2.400   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.400   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.450   PROCEDURAL             PRODUCTION-FIRED TEST5-8
     2.450   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.450   PROCEDURAL             MODULE-REQUEST MANUAL
     2.450   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.450   MOTOR                  PREPARE HAND-PLY HAND RIGHT R 1 THETA 3.14
     2.450   PROCEDURAL             CONFLICT-RESOLUTION
     2.650   MOTOR                  PREPARATION-COMPLETE 2.45 style HAND-PLY hand RIGHT
     2.650   PROCEDURAL             CONFLICT-RESOLUTION
     2.650   PROCEDURAL             PRODUCTION-SELECTED TEST5-9
(P TEST5-9
   =GOAL>
       TEST TEST5
       STATE 9
   ?MANUAL>
       STATE FREE
 ==>
   +MANUAL>
       CMD EXECUTE
   =GOAL>
       STATE 10
)
     2.650   ------                 BREAK-EVENT PRODUCTION TEST5-9
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PUNCH
  index  free           : T
  index  down           : T
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HAND-PLY
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(5 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 4)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(7 4)
  RIGHT MIDDLE: #(8 4)
  RIGHT RING: #(9 4)
  RIGHT PINKIE: #(10 4)
  RIGHT THUMB: #(6 6)
     2.650   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.650   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.700   PROCEDURAL             PRODUCTION-FIRED TEST5-9
     2.700   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.700   PROCEDURAL             MODULE-REQUEST MANUAL
     2.700   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.700   MOTOR                  EXECUTE
     2.700   PROCEDURAL             CONFLICT-RESOLUTION
     2.750   MOTOR                  INITIATION-COMPLETE 2.45 style HAND-PLY hand RIGHT
     2.750   PROCEDURAL             CONFLICT-RESOLUTION
     2.850   MOTOR                  MOVE-A-HAND RIGHT 1 3.14
     2.850   PROCEDURAL             CONFLICT-RESOLUTION
     2.900   MOTOR                  FINISH-MOVEMENT 2.45 style HAND-PLY hand RIGHT
     2.900   PROCEDURAL             CONFLICT-RESOLUTION
     2.900   PROCEDURAL             PRODUCTION-SELECTED TEST5-10
     2.900   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.900   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.950   PROCEDURAL             PRODUCTION-FIRED TEST5-10
     2.950   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.950   PROCEDURAL             MODULE-REQUEST MANUAL
     2.950   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.950   MOTOR                  POINT-HAND-AT-KEY HAND RIGHT TO-KEY y
     2.950   PROCEDURAL             CONFLICT-RESOLUTION
     3.000   MOTOR                  PREPARATION-COMPLETE 2.95 style HAND-PLY hand RIGHT
     3.000   PROCEDURAL             CONFLICT-RESOLUTION
     3.050   MOTOR                  INITIATION-COMPLETE 2.95 style HAND-PLY hand RIGHT
     3.050   PROCEDURAL             CONFLICT-RESOLUTION
     3.150   MOTOR                  MOVE-A-HAND RIGHT 1.0 -1.5707964
     3.150   PROCEDURAL             CONFLICT-RESOLUTION
     3.200   MOTOR                  FINISH-MOVEMENT 2.95 style HAND-PLY hand RIGHT
     3.200   PROCEDURAL             CONFLICT-RESOLUTION
     3.200   PROCEDURAL             PRODUCTION-SELECTED TEST5-11
(P TEST5-11
   =GOAL>
       TEST TEST5
       STATE 11
   ?MANUAL>
       STATE FREE
 ==>
   +MANUAL>
       KEY "k"
       CMD HOLD-KEY
   =GOAL>
       STATE 12
)
     3.200   ------                 BREAK-EVENT PRODUCTION TEST5-11
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PUNCH
  index  free           : T
  index  down           : T
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HAND-PLY
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(5 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 4)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(6 3)
  RIGHT MIDDLE: #(7 3)
  RIGHT RING: #(8 3)
  RIGHT PINKIE: #(9 3)
  RIGHT THUMB: #(5 5)
     3.200   PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.200   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     3.250   PROCEDURAL             PRODUCTION-FIRED TEST5-11
     3.250   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.250   PROCEDURAL             MODULE-REQUEST MANUAL
     3.250   PROCEDURAL             CLEAR-BUFFER MANUAL
     3.250   PROCEDURAL             CONFLICT-RESOLUTION
     3.500   MOTOR                  PREPARATION-COMPLETE 3.25 style HOLD-PECK hand RIGHT
     3.500   PROCEDURAL             CONFLICT-RESOLUTION
     3.550   MOTOR                  INITIATION-COMPLETE 3.25 style HOLD-PECK hand RIGHT
     3.550   PROCEDURAL             CONFLICT-RESOLUTION
     3.650   MOTOR                  FINISH-MOVEMENT 3.25 style HOLD-PECK hand RIGHT
     3.650   KEYBOARD               output-key TEST-MOTOR k
     3.650   PROCEDURAL             CONFLICT-RESOLUTION
     3.650   PROCEDURAL             PRODUCTION-SELECTED TEST5-12
(P TEST5-12
   =GOAL>
       TEST TEST5
       STATE 12
   ?MANUAL>
       STATE FREE
 ==>
   +MANUAL>
       HAND RIGHT
       CMD ALL-FINGERS-TO-HOME
   -GOAL>
)
     3.650   ------                 BREAK-EVENT PRODUCTION TEST5-12
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PUNCH
  index  free           : T
  index  down           : T
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PECK
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : T
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(5 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 4)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(6 3)
  RIGHT MIDDLE: #(8 4)
  RIGHT RING: #(8 3)
  RIGHT PINKIE: #(9 3)
  RIGHT THUMB: #(5 5)
     3.650   PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.650   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     3.700   PROCEDURAL             PRODUCTION-FIRED TEST5-12
     3.700   PROCEDURAL             MODULE-REQUEST MANUAL
     3.700   PROCEDURAL             CLEAR-BUFFER GOAL
     3.700   PROCEDURAL             CLEAR-BUFFER MANUAL
     3.700   PROCEDURAL             CONFLICT-RESOLUTION
     3.800   MOTOR                  PREPARATION-COMPLETE 3.7 style ALL-FINGERS-TO-HOME hand RIGHT
     3.800   PROCEDURAL             CONFLICT-RESOLUTION
     3.850   MOTOR                  INITIATION-COMPLETE 3.7 style ALL-FINGERS-TO-HOME hand RIGHT
     3.850   PROCEDURAL             CONFLICT-RESOLUTION
     3.890   KEYBOARD               release-key TEST-MOTOR k
     3.890   PROCEDURAL             CONFLICT-RESOLUTION
     4.000   MOTOR                  ALL-FINGERS-TO-HOME RIGHT
     4.000   PROCEDURAL             CONFLICT-RESOLUTION
     4.050   MOTOR                  FINISH-MOVEMENT 3.7 style ALL-FINGERS-TO-HOME hand RIGHT
     4.050   PROCEDURAL             CONFLICT-RESOLUTION
     4.050   ------                 Stopped because no events left to process
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : HOLD-PUNCH
  index  free           : T
  index  down           : T
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : ALL-FINGERS-TO-HOME
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(5 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 4)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(7 4)
  RIGHT MIDDLE: #(8 4)
  RIGHT RING: #(9 4)
  RIGHT PINKIE: #(10 4)
  RIGHT THUMB: #(6 6)
     4.050   ------                 Stopped because no events left to process
#####
"Move the fingers and hands around"
#####
 PRESSED:
     0.710 "w"        
     1.010 "w"        (0.300)
     1.210 "w"        (0.200)
     2.360 "g"        (1.150)
     3.650 "k"        (1.290)
 RELEASED:
     0.890 "w"        
     1.040 "w"        (0.150)
     1.640 "w"        (0.600)
     3.890 "k"        (2.250)
 CLICKED:
 UNCLICKED:
 Held:
  "w"        0.180
  "w"        0.030
  "w"        0.430
  "g"        
  "k"        0.240
 Clicked:
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL GOAL0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST6
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST6
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  PREPARATION-COMPLETE 0.05 style POINT-FINGER hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   PROCEDURAL             PRODUCTION-SELECTED TEST6-1
     0.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.350   MOTOR                  INITIATION-COMPLETE 0.05 style POINT-FINGER hand LEFT
     0.350   PROCEDURAL             PRODUCTION-FIRED TEST6-1
     0.350   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.350   PROCEDURAL             MODULE-REQUEST MANUAL
     0.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   MOTOR                  PREPARATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.400   PROCEDURAL             PRODUCTION-SELECTED TEST6-2
     0.400   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.400   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.450   MOTOR                  FINISH-MOVEMENT 0.05 style POINT-FINGER hand LEFT
     0.450   PROCEDURAL             PRODUCTION-FIRED TEST6-2
     0.450   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.450   PROCEDURAL             MODULE-REQUEST MANUAL
     0.450   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.500   MOTOR                  INITIATION-COMPLETE 0.35 style PECK-RECOIL hand LEFT
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  PREPARATION-COMPLETE 0.45 style PUNCH hand LEFT
     0.600   KEYBOARD               output-key TEST-MOTOR r
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.640   KEYBOARD               release-key TEST-MOTOR r
     0.640   PROCEDURAL             CONFLICT-RESOLUTION
     0.750   MOTOR                  FINISH-MOVEMENT 0.35 style PECK-RECOIL hand LEFT
     0.750   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  INITIATION-COMPLETE 0.45 style PUNCH hand LEFT
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.810   KEYBOARD               output-key TEST-MOTOR t
     0.810   PROCEDURAL             CONFLICT-RESOLUTION
     0.840   KEYBOARD               release-key TEST-MOTOR t
     0.840   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  FINISH-MOVEMENT 0.45 style PUNCH hand LEFT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   PROCEDURAL             PRODUCTION-SELECTED TEST6-3
     0.900   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.900   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL-LEFT
     0.900   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.950   PROCEDURAL             PRODUCTION-FIRED TEST6-3
     0.950   PROCEDURAL             MODULE-REQUEST MANUAL
     0.950   PROCEDURAL             CLEAR-BUFFER GOAL
     0.950   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     1.200   MOTOR                  PREPARATION-COMPLETE 0.95 style POINT-FINGER hand LEFT
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.250   MOTOR                  INITIATION-COMPLETE 0.95 style POINT-FINGER hand LEFT
     1.250   PROCEDURAL             CONFLICT-RESOLUTION
     1.300   PROCEDURAL             CONFLICT-RESOLUTION
     1.350   MOTOR                  FINISH-MOVEMENT 0.95 style POINT-FINGER hand LEFT
     1.350   PROCEDURAL             CONFLICT-RESOLUTION
     1.350   ------                 Stopped because no events left to process
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : POINT-FINGER
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : NONE
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(4 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 4)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(7 4)
  RIGHT MIDDLE: #(8 4)
  RIGHT RING: #(9 4)
  RIGHT PINKIE: #(10 4)
  RIGHT THUMB: #(6 6)
#####
"Some type-key actions from non-home position."
#####
 PRESSED:
     0.600 "r"        
     0.810 "t"        (0.210)
 RELEASED:
     0.640 "r"        
     0.840 "t"        (0.200)
 CLICKED:
 UNCLICKED:
 Held:
  "r"        0.040
  "t"        0.030
 Clicked:
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST7
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST7
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  DELAYED-PUNCH HAND LEFT FINGER INDEX DELAY 0.075
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   MOTOR                  PREPARATION-COMPLETE 0.05 style DELAYED-PUNCH hand LEFT
     0.200   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   PROCEDURAL             PRODUCTION-SELECTED TEST7-1
     0.200   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.200   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.250   MOTOR                  INITIATION-COMPLETE 0.05 style DELAYED-PUNCH hand LEFT
     0.250   PROCEDURAL             PRODUCTION-FIRED TEST7-1
     0.250   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.250   PROCEDURAL             MODULE-REQUEST MANUAL
     0.250   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.250   MOTOR                  DELAYED-PUNCH HAND RIGHT FINGER INDEX DELAY 0.05
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.260   KEYBOARD               output-key TEST-MOTOR f
     0.260   PROCEDURAL             CONFLICT-RESOLUTION
     0.335   KEYBOARD               release-key TEST-MOTOR f
     0.335   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  PREPARATION-COMPLETE 0.25 style DELAYED-PUNCH hand RIGHT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST7-2
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.385   MOTOR                  FINISH-MOVEMENT 0.05 style DELAYED-PUNCH hand LEFT
     0.400   MOTOR                  INITIATION-COMPLETE 0.25 style DELAYED-PUNCH hand RIGHT
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST7-2
     0.400   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  DELAYED-PUNCH HAND RIGHT FINGER MIDDLE DELAY 0.1
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.410   KEYBOARD               output-key TEST-MOTOR j
     0.410   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  PREPARATION-COMPLETE 0.4 style DELAYED-PUNCH hand RIGHT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   PROCEDURAL             PRODUCTION-SELECTED TEST7-3
     0.450   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.450   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.460   KEYBOARD               release-key TEST-MOTOR j
     0.500   PROCEDURAL             PRODUCTION-FIRED TEST7-3
     0.500   PROCEDURAL             MODULE-REQUEST MANUAL
     0.500   PROCEDURAL             CLEAR-BUFFER GOAL
     0.500   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.500   MOTOR                  DELAYED-PUNCH HAND LEFT FINGER RING DELAY 0.22
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.510   MOTOR                  FINISH-MOVEMENT 0.25 style DELAYED-PUNCH hand RIGHT
     0.510   PROCEDURAL             CONFLICT-RESOLUTION
     0.560   MOTOR                  INITIATION-COMPLETE 0.4 style DELAYED-PUNCH hand RIGHT
     0.560   PROCEDURAL             CONFLICT-RESOLUTION
     0.570   KEYBOARD               output-key TEST-MOTOR k
     0.570   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  PREPARATION-COMPLETE 0.5 style DELAYED-PUNCH hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.5 style DELAYED-PUNCH hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.660   KEYBOARD               output-key TEST-MOTOR s
     0.660   PROCEDURAL             CONFLICT-RESOLUTION
     0.670   KEYBOARD               release-key TEST-MOTOR k
     0.670   PROCEDURAL             CONFLICT-RESOLUTION
     0.720   MOTOR                  FINISH-MOVEMENT 0.4 style DELAYED-PUNCH hand RIGHT
     0.720   PROCEDURAL             CONFLICT-RESOLUTION
     0.880   KEYBOARD               release-key TEST-MOTOR s
     0.880   PROCEDURAL             CONFLICT-RESOLUTION
     0.930   MOTOR                  FINISH-MOVEMENT 0.5 style DELAYED-PUNCH hand LEFT
     0.930   PROCEDURAL             CONFLICT-RESOLUTION
     0.930   ------                 Stopped because no events left to process
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST7
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST7
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  DELAYED-PUNCH HAND LEFT FINGER INDEX DELAY 0.05
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   MOTOR                  PREPARATION-COMPLETE 0.05 style DELAYED-PUNCH hand LEFT
     0.200   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   PROCEDURAL             PRODUCTION-SELECTED TEST7-1
     0.200   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.200   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.250   MOTOR                  INITIATION-COMPLETE 0.05 style DELAYED-PUNCH hand LEFT
     0.250   PROCEDURAL             PRODUCTION-FIRED TEST7-1
     0.250   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.250   PROCEDURAL             MODULE-REQUEST MANUAL
     0.250   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.250   MOTOR                  DELAYED-PUNCH HAND RIGHT FINGER INDEX DELAY 0.025
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.260   KEYBOARD               output-key TEST-MOTOR f
     0.260   PROCEDURAL             CONFLICT-RESOLUTION
     0.310   KEYBOARD               release-key TEST-MOTOR f
     0.310   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  PREPARATION-COMPLETE 0.25 style DELAYED-PUNCH hand RIGHT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST7-2
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.360   MOTOR                  FINISH-MOVEMENT 0.05 style DELAYED-PUNCH hand LEFT
     0.400   MOTOR                  INITIATION-COMPLETE 0.25 style DELAYED-PUNCH hand RIGHT
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST7-2
     0.400   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   MOTOR                  DELAYED-PUNCH HAND RIGHT FINGER MIDDLE DELAY 0.07
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.410   KEYBOARD               output-key TEST-MOTOR j
     0.410   PROCEDURAL             CONFLICT-RESOLUTION
     0.435   KEYBOARD               release-key TEST-MOTOR j
     0.435   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  PREPARATION-COMPLETE 0.4 style DELAYED-PUNCH hand RIGHT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   PROCEDURAL             PRODUCTION-SELECTED TEST7-3
     0.450   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.450   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.485   MOTOR                  FINISH-MOVEMENT 0.25 style DELAYED-PUNCH hand RIGHT
     0.500   PROCEDURAL             PRODUCTION-FIRED TEST7-3
     0.500   PROCEDURAL             MODULE-REQUEST MANUAL
     0.500   PROCEDURAL             CLEAR-BUFFER GOAL
     0.500   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.500   MOTOR                  DELAYED-PUNCH HAND LEFT FINGER RING DELAY 0.22
     0.500   PROCEDURAL             CONFLICT-RESOLUTION
     0.535   MOTOR                  INITIATION-COMPLETE 0.4 style DELAYED-PUNCH hand RIGHT
     0.535   PROCEDURAL             CONFLICT-RESOLUTION
     0.545   KEYBOARD               output-key TEST-MOTOR k
     0.545   PROCEDURAL             CONFLICT-RESOLUTION
     0.600   MOTOR                  PREPARATION-COMPLETE 0.5 style DELAYED-PUNCH hand LEFT
     0.600   PROCEDURAL             CONFLICT-RESOLUTION
     0.615   KEYBOARD               release-key TEST-MOTOR k
     0.615   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  INITIATION-COMPLETE 0.5 style DELAYED-PUNCH hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.660   KEYBOARD               output-key TEST-MOTOR s
     0.660   PROCEDURAL             CONFLICT-RESOLUTION
     0.665   MOTOR                  FINISH-MOVEMENT 0.4 style DELAYED-PUNCH hand RIGHT
     0.665   PROCEDURAL             CONFLICT-RESOLUTION
     0.880   KEYBOARD               release-key TEST-MOTOR s
     0.880   PROCEDURAL             CONFLICT-RESOLUTION
     0.930   MOTOR                  FINISH-MOVEMENT 0.5 style DELAYED-PUNCH hand LEFT
     0.930   PROCEDURAL             CONFLICT-RESOLUTION
     0.930   ------                 Stopped because no events left to process
#####
"Default timed delayed-punch actions"
#####
 PRESSED:
     0.260 "f"        
     0.410 "j"        (0.150)
     0.570 "k"        (0.160)
     0.660 "s"        (0.090)
 RELEASED:
     0.335 "f"        
     0.460 "j"        (0.125)
     0.670 "k"        (0.210)
     0.880 "s"        (0.210)
 CLICKED:
 UNCLICKED:
 Held:
  "f"        0.075
  "j"        0.050
  "k"        0.100
  "s"        0.220
 Clicked:
#####
"Faster timed delayed-punch actions"
#####
 PRESSED:
     0.260 "f"        
     0.410 "j"        (0.150)
     0.545 "k"        (0.135)
     0.660 "s"        (0.115)
 RELEASED:
     0.310 "f"        
     0.435 "j"        (0.125)
     0.615 "k"        (0.180)
     0.880 "s"        (0.265)
 CLICKED:
 UNCLICKED:
 Held:
  "f"        0.050
  "j"        0.025
  "k"        0.070
  "s"        0.220
 Clicked:
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST8
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST8
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  HOLD-PUNCH HAND LEFT FINGER INDEX
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   MOTOR                  PREPARATION-COMPLETE 0.05 style HOLD-PUNCH hand LEFT
     0.200   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   PROCEDURAL             PRODUCTION-SELECTED TEST8-1
     0.200   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.200   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.250   MOTOR                  INITIATION-COMPLETE 0.05 style HOLD-PUNCH hand LEFT
     0.250   PROCEDURAL             PRODUCTION-FIRED TEST8-1
     0.250   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.250   PROCEDURAL             MODULE-REQUEST MANUAL
     0.250   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.250   MOTOR                  HOLD-PUNCH HAND RIGHT FINGER INDEX
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.260   KEYBOARD               output-key TEST-MOTOR f
     0.260   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  FINISH-MOVEMENT 0.05 style HOLD-PUNCH hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  PREPARATION-COMPLETE 0.25 style HOLD-PUNCH hand RIGHT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST8-2
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   MOTOR                  INITIATION-COMPLETE 0.25 style HOLD-PUNCH hand RIGHT
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST8-2
     0.400   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.410   KEYBOARD               output-key TEST-MOTOR j
     0.410   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  FINISH-MOVEMENT 0.25 style HOLD-PUNCH hand RIGHT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  PREPARATION-COMPLETE 0.4 style HOLD-PECK hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   PROCEDURAL             PRODUCTION-SELECTED TEST8-3
     0.650   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.650   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.700   MOTOR                  INITIATION-COMPLETE 0.4 style HOLD-PECK hand LEFT
     0.700   PROCEDURAL             PRODUCTION-FIRED TEST8-3
     0.700   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.700   PROCEDURAL             MODULE-REQUEST MANUAL
     0.700   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  FINISH-MOVEMENT 0.4 style HOLD-PECK hand LEFT
     0.800   KEYBOARD               output-key TEST-MOTOR z
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  PREPARATION-COMPLETE 0.7 style HOLD-PECK hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   MOTOR                  INITIATION-COMPLETE 0.7 style HOLD-PECK hand RIGHT
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     1.050   MOTOR                  FINISH-MOVEMENT 0.7 style HOLD-PECK hand RIGHT
     1.050   KEYBOARD               output-key TEST-MOTOR o
     1.050   PROCEDURAL             CONFLICT-RESOLUTION
     1.050   PROCEDURAL             PRODUCTION-SELECTED TEST8-4
     1.050   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.050   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.100   PROCEDURAL             PRODUCTION-FIRED TEST8-4
     1.100   PROCEDURAL             MODULE-REQUEST MANUAL
     1.100   PROCEDURAL             CLEAR-BUFFER GOAL
     1.100   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.100   PROCEDURAL             CONFLICT-RESOLUTION
     1.200   MOTOR                  PREPARATION-COMPLETE 1.1 style ALL-FINGERS-TO-HOME hand BOTH
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.250   MOTOR                  INITIATION-COMPLETE 1.1 style ALL-FINGERS-TO-HOME hand BOTH
     1.250   PROCEDURAL             CONFLICT-RESOLUTION
     1.290   KEYBOARD               release-key TEST-MOTOR f
     1.290   KEYBOARD               release-key TEST-MOTOR z
     1.290   KEYBOARD               release-key TEST-MOTOR j
     1.290   KEYBOARD               release-key TEST-MOTOR o
     1.290   PROCEDURAL             CONFLICT-RESOLUTION
     1.400   MOTOR                  ALL-FINGERS-TO-HOME LEFT
     1.400   MOTOR                  ALL-FINGERS-TO-HOME RIGHT
     1.400   PROCEDURAL             CONFLICT-RESOLUTION
     1.450   MOTOR                  FINISH-MOVEMENT 1.1 style ALL-FINGERS-TO-HOME hand BOTH
     1.450   PROCEDURAL             CONFLICT-RESOLUTION
     1.450   ------                 Stopped because no events left to process
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : ALL-FINGERS-TO-HOME
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : ALL-FINGERS-TO-HOME
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(4 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 4)
  LEFT PINKIE: #(1 4)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(7 4)
  RIGHT MIDDLE: #(8 4)
  RIGHT RING: #(9 4)
  RIGHT PINKIE: #(10 4)
  RIGHT THUMB: #(6 6)
#####
"All home with multiple fingers down"
#####
 PRESSED:
     0.260 "f"        
     0.410 "j"        (0.150)
     0.800 "z"        (0.390)
     1.050 "o"        (0.250)
 RELEASED:
     1.290 "f"        
     1.290 "z"        (0.000)
     1.290 "j"        (0.000)
     1.290 "o"        (0.000)
 CLICKED:
 UNCLICKED:
 Held:
  "f"        1.030
  "j"        0.880
  "z"        0.490
  "o"        0.240
 Clicked:
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK0 NIL
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.000   PROCEDURAL             PRODUCTION-SELECTED TEST9
     0.000   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.050   PROCEDURAL             PRODUCTION-FIRED TEST9
     0.050   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050   PROCEDURAL             MODULE-REQUEST MANUAL
     0.050   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.050   MOTOR                  HOLD-PUNCH HAND LEFT FINGER INDEX
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   MOTOR                  PREPARATION-COMPLETE 0.05 style HOLD-PUNCH hand LEFT
     0.200   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   PROCEDURAL             PRODUCTION-SELECTED TEST9-1
     0.200   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.200   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.250   MOTOR                  INITIATION-COMPLETE 0.05 style HOLD-PUNCH hand LEFT
     0.250   PROCEDURAL             PRODUCTION-FIRED TEST9-1
     0.250   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.250   PROCEDURAL             MODULE-REQUEST MANUAL
     0.250   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.250   MOTOR                  HOLD-PUNCH HAND RIGHT FINGER INDEX
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.260   KEYBOARD               output-key TEST-MOTOR f
     0.260   PROCEDURAL             CONFLICT-RESOLUTION
     0.300   MOTOR                  FINISH-MOVEMENT 0.05 style HOLD-PUNCH hand LEFT
     0.300   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   MOTOR                  PREPARATION-COMPLETE 0.25 style HOLD-PUNCH hand RIGHT
     0.350   PROCEDURAL             CONFLICT-RESOLUTION
     0.350   PROCEDURAL             PRODUCTION-SELECTED TEST9-2
     0.350   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.350   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.400   MOTOR                  INITIATION-COMPLETE 0.25 style HOLD-PUNCH hand RIGHT
     0.400   PROCEDURAL             PRODUCTION-FIRED TEST9-2
     0.400   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.400   PROCEDURAL             MODULE-REQUEST MANUAL
     0.400   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.400   PROCEDURAL             CONFLICT-RESOLUTION
     0.410   KEYBOARD               output-key TEST-MOTOR j
     0.410   PROCEDURAL             CONFLICT-RESOLUTION
     0.450   MOTOR                  FINISH-MOVEMENT 0.25 style HOLD-PUNCH hand RIGHT
     0.450   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   MOTOR                  PREPARATION-COMPLETE 0.4 style HOLD-PECK hand LEFT
     0.650   PROCEDURAL             CONFLICT-RESOLUTION
     0.650   PROCEDURAL             PRODUCTION-SELECTED TEST9-3
     0.650   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.650   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.700   MOTOR                  INITIATION-COMPLETE 0.4 style HOLD-PECK hand LEFT
     0.700   PROCEDURAL             PRODUCTION-FIRED TEST9-3
     0.700   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.700   PROCEDURAL             MODULE-REQUEST MANUAL
     0.700   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.700   PROCEDURAL             CONFLICT-RESOLUTION
     0.800   MOTOR                  FINISH-MOVEMENT 0.4 style HOLD-PECK hand LEFT
     0.800   KEYBOARD               output-key TEST-MOTOR z
     0.800   PROCEDURAL             CONFLICT-RESOLUTION
     0.900   MOTOR                  PREPARATION-COMPLETE 0.7 style HOLD-PECK hand RIGHT
     0.900   PROCEDURAL             CONFLICT-RESOLUTION
     0.950   MOTOR                  INITIATION-COMPLETE 0.7 style HOLD-PECK hand RIGHT
     0.950   PROCEDURAL             CONFLICT-RESOLUTION
     1.050   MOTOR                  FINISH-MOVEMENT 0.7 style HOLD-PECK hand RIGHT
     1.050   KEYBOARD               output-key TEST-MOTOR o
     1.050   PROCEDURAL             CONFLICT-RESOLUTION
     1.050   PROCEDURAL             PRODUCTION-SELECTED TEST9-4
     1.050   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.050   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.100   PROCEDURAL             PRODUCTION-FIRED TEST9-4
     1.100   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.100   PROCEDURAL             MODULE-REQUEST MANUAL
     1.100   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.100   MOTOR                  release-all-fingers
     1.100   PROCEDURAL             CONFLICT-RESOLUTION
     1.150   MOTOR                  PREPARATION-COMPLETE 1.1 style RELEASE-ALL-FINGERS hand BOTH
     1.150   PROCEDURAL             CONFLICT-RESOLUTION
     1.200   MOTOR                  INITIATION-COMPLETE 1.1 style RELEASE-ALL-FINGERS hand BOTH
     1.200   PROCEDURAL             CONFLICT-RESOLUTION
     1.240   MOTOR                  RELEASING-ALL-FINGERS
     1.240   KEYBOARD               release-key TEST-MOTOR f
     1.240   KEYBOARD               release-key TEST-MOTOR z
     1.240   KEYBOARD               release-key TEST-MOTOR j
     1.240   KEYBOARD               release-key TEST-MOTOR o
     1.240   PROCEDURAL             CONFLICT-RESOLUTION
     1.300   MOTOR                  FINISH-MOVEMENT 1.1 style RELEASE-ALL-FINGERS hand BOTH
     1.300   PROCEDURAL             CONFLICT-RESOLUTION
     1.300   PROCEDURAL             PRODUCTION-SELECTED TEST9-5
     1.300   PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.300   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.350   PROCEDURAL             PRODUCTION-FIRED TEST9-5
     1.350   PROCEDURAL             MODULE-REQUEST MANUAL
#|Warning: Release-all-fingers action called when no fingers need to be released. |#
     1.350   PROCEDURAL             CLEAR-BUFFER GOAL
     1.350   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.350   PROCEDURAL             CONFLICT-RESOLUTION
     1.350   ------                 Stopped because no events left to process
MANUAL-LEFT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : RELEASE-ALL-FINGERS
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

MANUAL-RIGHT:
  buffer empty          : T
  buffer full           : NIL
  buffer failure        : NIL
  buffer requested      : NIL
  buffer unrequested    : NIL
  state free            : T
  state busy            : NIL
  state error           : NIL
  processor free        : T
  processor busy        : NIL
  execution free        : T
  execution busy        : NIL
  last-command          : RELEASE-ALL-FINGERS
  index  free           : T
  index  down           : NIL
  middle free           : T
  middle down           : NIL
  ring   free           : T
  ring   down           : NIL
  pinkie free           : T
  pinkie down           : NIL
  thumb  free           : T
  thumb  down           : NIL

  LEFT INDEX: #(4 4)
  LEFT MIDDLE: #(3 4)
  LEFT RING: #(2 4)
  LEFT PINKIE: #(1 5)
  LEFT THUMB: #(5 6)
  RIGHT INDEX: #(7 4)
  RIGHT MIDDLE: #(8 4)
  RIGHT RING: #(9 3)
  RIGHT PINKIE: #(10 4)
  RIGHT THUMB: #(6 6)
#####
"Release-all-fingers with multiple fingers down"
#####
 PRESSED:
     0.260 "f"        
     0.410 "j"        (0.150)
     0.800 "z"        (0.390)
     1.050 "o"        (0.250)
 RELEASED:
     1.240 "f"        
     1.240 "z"        (0.000)
     1.240 "j"        (0.000)
     1.240 "o"        (0.000)
 CLICKED:
 UNCLICKED:
 Held:
  "f"        0.980
  "j"        0.830
  "z"        0.440
  "o"        0.190
 Clicked:
NIL
|#
