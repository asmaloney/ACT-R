;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2013 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : parallel-computation.lisp
;;; Version     : 2.0a1
;;; 
;;; Description : Provides "mapcar like" functionality which can perform 
;;;               calculations in parallel using multiple threads using the
;;;               ACT-R uni-files functions.  The functions being used for
;;;               mapping must be arity 1.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] More testing, both for performance and reliability.
;;; 
;;; ----- History -----
;;; 2013.10.14 Dan
;;;             : * Initial creation.
;;; 2013.10.29 Dan
;;;             : * Update some of the docs after running some performance tests.
;;;             : * Replaced the separate running functions with a single one that
;;;             :   takes parameters to setup the different situations.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2018.08.29 Dan [2.0a1]
;;;             : * Updated to work with ACT-R 7.x using bordeaux-threads and
;;;             :   require-extra.
;;; 2018.08.30 Dan
;;;             : * Eliminated the "out of order" options since the current 
;;;             :   implementation always collects things in order.
;;; 2018.09.04 Dan
;;;             : * Add the parallel mapcan back in since it's useful.
;;; 2018.09.06 Dan
;;;             : * Instead of doing the subseq calls at the top level, just
;;;             :   set the indices and have the workers do it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This code only depends upon bordeaux-threads which means it could be use 
;;; independently of ACT-R, but the assumption here is that it will be used with
;;; ACT-R 7.x which is also using bordeaux-threads.
;;;
;;; To use this the best option is to use require-extra:
;;;
;;; (require-extra "parallel-computation")
;;;
;;; in any file which uses the functions defined here.
;;;
;;; The basic procedure for using this is to create a "team" of workers.  Then, that
;;; team, a function, and list of parameters can be passed to one of the parallel 
;;; running functions.  The parallel function will create a list of results based
;;; on the function and parameters provided by breaking the parameters into sublists
;;; and having the workers of the team process those sublists in parallel.  The return 
;;; values from the parallel function will then be a list of results and an indication 
;;; of whether there was any error.  When the team is no longer needed it should be 
;;; released to end the worker processes that were created.
;;;
;;; An important thing to keep in mind is that the function passed to a parallel
;;; running command must be "thread safe".  The most important aspect of that is
;;; that it can't modify non-local data without appropriate protection.  It also
;;; shouldn't call any of the parallel running code, especially with the same team.
;;; Any input or output operations are also likely going to need to be locked if
;;; the same stream is being used (for example printing to *standard-output*).  
;;;
;;; With respect to ACT-R commands, those which are published in the reference manual
;;; are thread-safe, but if you use non-documented ACT-R commands they may or may
;;; not be thread-safe and should be thoroughly investigated before using them in
;;; parallel operations.  Note however that since the ACT-R commands are thread safe
;;; if you're using the same ACT-R command in parallel with itself it's possible that 
;;; its locking to protect things could effectively force those parallel instance to
;;; work serially anyway which might eliminate any benefit from the parallelism.
;;;
;;; It is possible to use the same team for different tasks, and that is the easiest
;;; way to use this (similar to the parallel-models extra).  However, if different
;;; tasks have different performance issues then different sized teams may be more
;;; efficient for handling those different tasks in parallel.  Also, while it is
;;; technically possible to have a team of workers each of which uses a separate team
;;; of workers to further subdivide a task, using the system in that fashion was not
;;; how it was intended and recursively using the parallel commands has not been tested 
;;; and is not recommended (recursively trying to use the same team of workers is not
;;; possible for what should be obvious reasons).
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;; create-parallel-team
;;; 
;;; defun create-parallel-team (name size)
;;; 
;;;  name must be a string which is used to create the names for the worker processes
;;;   and associated control items.
;;;  size must be a positive integer indicating the number of workers to create.
;;;  
;;;  If the parameters provided are appropiate it creates and returns a team of workers
;;;  which can be used for running the parallel-* functions.  Size processes for the 
;;;  workers will be created and waiting until there is something for them to perform.
;;;  Those processes will remain alive until the team is released or killed.  
;;;  If the parameters provided are not appropriate then it prints a warning and returns 
;;;  nil without creating any processes.
;;;  
;;;  When creating a team one should probably create fewer workers than there are
;;;  processors/cores in the machine since there will be the main process and likely 
;;;  other Lisp processes (gc, GUI control, repl, etc) also running.  That way
;;;  workers aren't fighting for processor time, but there may be situations where
;;;  more workers than processors may show an improvement (possibilities would be 
;;;  if memory accessing is a limiting factor or the function runs so fast that
;;;  workers can finish while others are still being started or waiting to be
;;;  collected).
;;;
;;; release-parallel-team
;;; kill-parallel-team
;;;
;;; defun release-parallel-team (team)
;;; defun kill-parallel-team (team)
;;;
;;; team must be a team that was returned from create-parallel-team.
;;;
;;; These functions end the worker threads which were created with the team
;;; provided and mark the team as unusable.  Release-parallel-team is the 
;;; recommended way to do so and should be called while the team is not 
;;; actively being used first to have the team cleanly end the processes.
;;; However, if the worker processes for a team are not terminating because
;;; of an error in a parallel function which was not handled or for any other
;;; reason do not seem to exit cleanly, then kill-parallel-team can be used
;;; to attempt to explicitly kill the processes directly.
;;;
;;; If release-parallel-team is passed a team which has not yet been released
;;; then it will return t after releasing the worker processes, otherwise it 
;;; will print a warning and return nil.  
;;; If kill-parallel-team is passed a team it will try to kill the worker 
;;; processes and return t, otherwise it will print a warning and return nil.
;;; 
;;; parallel-mapcar
;;; parallel-mapcan
;;; parallel-funcall
;;;
;;; defun parallel-mapcar (team function params &rest more-params)
;;; defun parallel-mapcan (team function params &rest more-params)
;;; defun parallel-funcall (team function params &rest more-params)
;;;
;;; team must be a team that was returned from create-parallel-team.
;;; function must be a function or function name.
;;; params must be a list of parameters which will be passed to the function.
;;; more-params can be additional lists of parameters to pass to the function.  
;;;
;;; The parallel-* functions are used to carry out the execution of the function 
;;; provided with the params given, distributed across the workers of the team.  
;;; How the params are passed to the function and what is returned depends on which 
;;; of the parallel-* functions is called (the differences will be described below).
;;; All of the parallel-* functions return two values.
;;;
;;; If team is a team returned from create-parallel-team which has not yet been
;;; released or killed, function is a function or symbol which names a function, 
;;; and params and more-params are lists of items all of the same length (a 
;;; simplification requirement for now) then those lists of items will be split 
;;; into m sublists, where m=min(workers in team, length(params)), of equal length
;;; (or as close to equal as possible).  Each of those lists of sublists will be 
;;; given to a worker to process with function.  The results of the workers will be
;;; appended together and returned as the first value from a parallel-* function.
;;;
;;; The second value returned from a parallel-* function indicates whether or not
;;; there was an error either in the parameters provided or the execution of the
;;; workers.  If there is an error then a warning will be printed and the second
;;; value will be t.  If there are no errors then the second value will be nil.
;;; If the second value is t then the first value returned is likely not valid and
;;; should probably be discarded.
;;;
;;; Here are how the different parallel-* functions use the provided function
;;; and collect the results.
;;;
;;; Parallel-mapcar
;;;
;;; Each worker creates a list of results by using (mapcar function params(i) more-params[0][i] ...)
;;; where i is an index of the sublists by worker and all of the lists provided in more-params
;;; are included.  Those resulting lists are appended together such that the overall order of
;;; the resulting list matches the order of the original params list, and that list is returned
;;; as the first value.  Thus the result is the same as calling (mapcar function params more-params[0] ...)
;;; (assuming that function operating sequentially is the same as operating in parallel).
;;;
;;; Parallel-mapcan
;;;
;;; Each worker creates a list of results by using (mapcan function params(i) more-params[0][i] ...)
;;; where i is an index of the sublists by worker and all of the lists provided in more-params
;;; are included.  Those resulting lists are appended together such that the overall order of
;;; the resulting list matches the order of the original params list, and that list is returned
;;; as the first value.  Thus the result is the same as calling (mapcan function params more-params[0] ...)
;;; (assuming that function operating sequentially is the same as operating in parallel).
;;;
;;; Parallel-funcall
;;;
;;; Each worker creates a result using (funcall function params(i) more-params[0][i] ...)
;;; i.e. the function will be passed the entire sublists as its parameters.  The
;;; results of those calls are appened together (in the order of the sublists) to
;;; produce the first result returned.  Because the results are appended the
;;; function provided should always return a list.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Considered some existing libraries which do this sort of thing, but had problems
;;; with them in some Lisps and don't want to deal with issues like that so produced
;;; a simple mechanism using bordeaux-threads directly.
;;;
;;; It splits the list upfront so that it's divided across all of the 
;;; workers initially instead of passing off one item at a time to the workers 
;;; to handle since that seems to be more efficient in most situations because the
;;; locking/unlock for passing and returning values from a worker is costly.
;;;
;;; It uses ignore-errors to prevent errors in the functions from breaking
;;; the workers.  If there are any errors in a worker it stops processing the
;;; remainder of its parameter list(s) and just signals an error result.  That 
;;; doesn't stop any other workers which are processing other parts of the
;;; parameter list(s).
;;;
;;; The workers have an unwind-protect which should signal the team controller
;;; when a worker thread has termiated unexpectedly, but if a worker thread is
;;; killed in a way that doesn't respect unwind-protect the whole team could be
;;; stuck if that happens during the processing of items.
;;; 
;;; There's no protection in the workers to ensure the code they're using is
;;; itself thread safe.  That's up to the user to ensure, and things like writing 
;;; output to the REPL can certainly be problematic.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct p_team "A group of workers for parallel tasks"
  name workers state target current lock cv size)

(defstruct p_worker "The parallel worker structure"
  name process state function result input cv lock mechanism start end)


(defun p_worker-thread-function (worker team)
  
  (setf (p_worker-state worker) :started)
  
  (bt:with-lock-held ((p_team-lock team))
    (pushnew worker (p_team-current team))
    (bt:condition-notify (p_team-cv team)))
  
  (unwind-protect
      (progn
        (bt:acquire-lock (p_worker-lock worker))
        (loop
          (bt:condition-wait (p_worker-cv worker) (p_worker-lock worker))
          (setf (p_worker-state worker) :working)
          
          (case (p_worker-mechanism worker)
            (:exit
             (setf (p_worker-state worker) :done)
             (return))
            (t
             (multiple-value-bind (value condition)
                 ; simply apply the indicated mechanism to the function and values provided
                 (ignore-errors (apply (p_worker-mechanism  worker) (p_worker-function worker) 
                                       
                                       (mapcar 
                                           (lambda (x)
                                             (subseq x (p_worker-start worker) (p_worker-end worker)))
                                         (p_worker-input worker))
                                       
                                       ))
               
               ; if there's a problem send the condition back
               ; otherwise return the resulting list
               
               (if (subtypep (type-of condition) 'condition)
                   (setf (p_worker-state worker) :fail
                     (p_worker-result worker) condition)
                 (setf (p_worker-state worker) :success
                   (p_worker-result worker) value)))
             
             (bt:with-lock-held ((p_team-lock team))
               (pushnew worker (p_team-current team))
               (bt:condition-notify (p_team-cv team)))))))
    
    ;; just notify the team when exiting...
    ;; expected for a clean exit
    (bt:with-lock-held ((p_team-lock team))
      (pushnew worker (p_team-current team))
      (bt:condition-notify (p_team-cv team)))))

  

(defun create-parallel-team (name size)
  (if (and (stringp name) (integerp size) (plusp size))
      (let ((team (make-p_team :name name :workers nil :state :creation :target size :current nil 
                               :size size :lock (bt:make-lock name) :cv (bt:make-condition-variable :name name))))
        
        (bt:acquire-lock (p_team-lock team))
        
        (dotimes (i size)
          (let* ((w-name (format nil "~a-~d" name i))
                 (worker (make-p_worker :name w-name :state :creating :cv (bt:make-condition-variable :name w-name)
                                        :lock (bt:make-lock w-name)
                                        )))
            (setf (p_worker-process worker)
              (bt:make-thread (lambda () (p_worker-thread-function worker team)) :name w-name))
            (push-last worker (p_team-workers team))))
        
        (loop
          (bt:condition-wait (p_team-cv team) (p_team-lock team))
          (when (= (length (p_team-current team)) (p_team-target team))
            (return)))
        (setf (p_team-state team) :ready)
        (bt:release-lock (p_team-lock team))
        team)
    (if (stringp name)
        (print-warning "Create-parallel-team requires a positive integer for the size, but given ~s" size)
      (print-warning "Create-parallel-team requires a string for the name, but given ~s" name))))

        
  
(defun run-parallel-team (team function params mechanism)
  (cond ((not (p_team-p team))
         (values (print-warning "Must provide a team to run instead of ~s" team) t))
        ((not (or (functionp function) (fboundp function)))
         (values (print-warning "Must provide a valid function instead of ~s" function) t))
        (t
         (let ((state (bt:with-lock-held ((p_team-lock team)) (p_team-state team))))
           (cond ((not (eq state :ready))
                  (values (print-warning "Team ~s is not currently ready to run." (p_team-name team)) t))
                 ((not (every (lambda (x)
                                (eq :started (p_worker-state x)))
                              (p_team-workers team)))
                  (values (print-warning "Some worker on team ~s is in an unexpected state and team cannot be used." (p_team-name team)) t))
                 ((not (every (lambda (x)
                                (= (length x) (length (first params))))
                              params))
                  (values (print-warning "All parameter lists must be the same length but given ~s." params) t))
                 (t
                  (bt:with-lock-held ((p_team-lock team)) 
                    (setf (p_team-state team) :busy))
                  
                  (unwind-protect
                      (let* ((n (length (first params)))
                             (m (min n (p_team-size team)))
                             (size (floor n m))
                             (sizes (make-list m :initial-element size)))
                        
                        (unless (= n (* m size))
                          (dotimes (i (- n (* m size)))
                            (incf (nth i sizes))))
                        
                        
                        
                        (bt:with-lock-held ((p_team-lock team)) 
                          (setf (p_team-current team) nil
                            (p_team-target team) m))
                        
                        (let ((start 0))
                          (dotimes (i m)
                            
                            (let ((worker (nth i (p_team-workers team))))
                              (bt:with-lock-held ((p_worker-lock worker))
                                (setf (p_worker-function worker) function)
                                (setf (p_worker-mechanism worker) mechanism)
                                
                                (setf (p_worker-input worker) params)
                                (setf (p_worker-start worker) start)
                                (setf (p_worker-end worker) (+ start (nth i sizes)))
                                                                   
                                (incf start (nth i sizes))
                                (bt:condition-notify (p_worker-cv worker))))))
                        
                        (bt:acquire-lock (p_team-lock team))
                        (while (< (length (p_team-current team)) m)
                          (bt:condition-wait (p_team-cv team) (p_team-lock team)))
                        (bt:release-lock (p_team-lock team))
                        
                        (let ((used-workers (subseq (p_team-workers team) 0 m)))
                          (prog1
                              
                              (cond ((not (every (lambda (x) (eq (p_worker-state x) :success)) used-workers))
                                     (dolist (x used-workers)
                                       (when (eq (p_worker-state x) :fail)
                                         (print-warning "Worker ~s reports failure: ~/print-error-message/" (p_worker-name x) (p_worker-result x))))
                                     (values nil t))
                                    ((not (every (lambda (x) (listp (p_worker-result x))) used-workers))
                                     (print-warning "One or more workers returned a value which is not a list.  Cannot append results.")
                                     (values (mapcar 'p_worker-result used-workers) t))
                                    (t (values (reduce 'append (mapcar 'p_worker-result (subseq (p_team-workers team) 0 m))) nil)))
                            
                            (dolist (x used-workers)
                              (bt:with-lock-held ((p_worker-lock x))
                                (setf (p_worker-state x) :started))))))
                    (bt:with-lock-held ((p_team-lock team)) 
                    (setf (p_team-state team) :ready)))))))))
                  

(defun parallel-mapcar (team function params &rest more-params)
  (run-parallel-team team function (cons params more-params) 'mapcar))

(defun parallel-funcall (team function params &rest more-params)
  (run-parallel-team team function (cons params more-params) 'funcall))

(defun parallel-mapcan (team function params &rest more-params)
  (run-parallel-team team function (cons params more-params) 'mapcan))


(defun release-parallel-team (team)
  ;; Set the exit flag and then unlock workers so they terminate by
  ;; returning from the function in the process instead of killing them.
  (if (p_team-p team)
      (let ((state (bt:with-lock-held ((p_team-lock team))
                     (p_team-state team))))
        (if (not (eq state :ready))
            (print-warning "Team ~s is not currently in the ready state and cannot be released." (p_team-name team))
          (progn
            
            (dolist (x (p_team-workers team))
              (when (bt:thread-alive-p (p_worker-process x))
                (bt:with-lock-held ((p_worker-lock x))
                  (setf (p_worker-mechanism x) :exit))
                (bt:condition-notify (p_worker-cv x))))
            
            ;; don't bother waiting or checking, just 
            ;; consider it done
            
            (bt:with-lock-held ((p_team-lock team))
              (setf (p_team-state team) :done))
            t)))
    (print-warning "Release-parallel-team requires a team created by create-parallel-team, but was given ~s" team)))


;; If something goes wrong may need to kill the processes.

(defun kill-parallel-team (team)
  (if (p_team-p team)
      (progn
        (dolist (x (p_team-workers team))
          (when (bt:thread-alive-p (p_worker-process x))
            (ignore-errors (bt:destroy-thread (p_worker-process x)))))
        (bt:with-lock-held ((p_team-lock team))
          (setf (p_team-state team) :done)
          (setf (p_team-workers team) nil))
        t)
    (print-warning "Kill-parallel-team requires a team created by create-parallel-team, but was given ~s" team)))


#|

Here's a dummy example of computing fibonacci numbers in parallel 
on a Windows machine with an i7-2600 (4 cores plus hyperthreads for
8 virtual cores) running ACL with a few different team sizes.

These results may not reflect the performance of any other task
split into parallel components with these tools.


CG-USER(16): (require-extra "parallel-computation")
;;; Compiling file C:\Users\db30\Desktop\actr7.x\extras\parallel-computation\parallel-computation.lisp
;;; Writing fasl file C:\Users\db30\Desktop\actr7.x\extras\parallel-computation\parallel-computation.fasl
;;; Fasl write complete
; Fast loading C:\Users\db30\Desktop\actr7.x\extras\parallel-computation\parallel-computation.fasl
T
CG-USER(17): (defun test-fib (x) (if (<= x 1) x (+ (test-fib (1- x)) (test-fib (- x 2)))))
TEST-FIB
CG-USER(18): (compile 'test-fib)
TEST-FIB
NIL
NIL
CG-USER(19): (time (mapcar 'test-fib (make-list 30 :initial-element 40)))
; cpu time (non-gc) 54.625000 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  54.625000 sec user, 0.000000 sec system
; cpu time (thread) 54.625000 sec user, 0.000000 sec system
; real time  54.614000 sec (100.0%)
; space allocation:
;  0 cons cells, 56 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
(102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 ...)
CG-USER(20): (defvar *team* (create-parallel-team "size-4" 4))
*TEAM*
CG-USER(21): (time (parallel-mapcar *team* 'test-fib (make-list 30 :initial-element 40)))
; cpu time (non-gc) 73.609375 sec (00:01:13.609375) user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  73.609375 sec (00:01:13.609375) user, 0.000000 sec system
; cpu time (thread) 0.000000 sec user, 0.000000 sec system
; real time  19.878000 sec (370.3%)
; space allocation:
;  0 cons cells, 56 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
(102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 ...)
CG-USER(22): (release-parallel-team *team*)
T
CG-USER(23): (setf *team* (create-parallel-team "size-8" 8))
#S(P_TEAM  ...)
CG-USER(24): (time (parallel-mapcar *team* 'test-fib (make-list 30 :initial-element 40)))
; cpu time (non-gc) 96.671875 sec (00:01:36.671875) user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  96.671875 sec (00:01:36.671875) user, 0.000000 sec system
; cpu time (thread) 0.000000 sec user, 0.000000 sec system
; real time  13.067000 sec (739.8%)
; space allocation:
;  0 cons cells, 56 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
(102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 ...)
CG-USER(25): (release-parallel-team *team*)
T
CG-USER(26): (setf *team* (create-parallel-team "size-16" 16))
#S(P_TEAM ...)
CG-USER(27): (time (parallel-mapcar *team* 'test-fib (make-list 30 :initial-element 40)))
; cpu time (non-gc) 95.390625 sec (00:01:35.390625) user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  95.390625 sec (00:01:35.390625) user, 0.000000 sec system
; cpu time (thread) 0.000000 sec user, 0.000000 sec system
; real time  14.360000 sec (664.3%)
; space allocation:
;  0 cons cells, 56 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
(102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 102334155 ...)
CG-USER(28): (release-parallel-team *team*)
T


|#       

(provide "parallel-computation")

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
