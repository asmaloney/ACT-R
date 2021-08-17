;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2015 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : parallel-models.lisp
;;; Version     : 2.0a1
;;; 
;;; Description : Provides the ability to run independent models in separate
;;;             : processes within a single Lisp instance.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] More testing, both for performance and reliability.
;;; 
;;; ----- History -----
;;; 2015.06.15 Dan
;;;             : * Initial creation.
;;; 2015.06.17 Dan
;;;             : * Slight tweak to run with non-ide ACL.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;;             : * Changed the examples to use the ACT-R logical.
;;; 2015.08.05 Dan
;;;             : * The main ACT-R load file is now called load-act-r.lisp.
;;; 2018.08.29 Dan [2.0a1]
;;;             : * Renamed parallel-models and did the same for the directory
;;;             :   so require-extra can be used to load it.
;;;             : * Updated to use bordeaux-threads for ACT-R 7.x since uni-files
;;;             :   doesn't exist now.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; This file provides a means of running multiple independent models along with
;;; their supporting Lisp code simultaneously.  It creates multiple instances of
;;; the full ACT-R system within a single Lisp and uses those to run the models.
;;; Those separate instances do not open a remote connection for the dispatcher
;;; therefore everything must be done in Lisp to use this extension.
;;; 
;;; This code depends on components of ACT-R therefore ACT-R must be loaded
;;; before loading this file.
;;; 
;;; The recommended way to load this is by using the require-extra command:
;;; 
;;; (require-extra "parallel-models")
;;;
;;;
;;; The basic procedure for using this is to create a set of independent ACT-R
;;; instances, instantiate the model to be run in each, and then use those to 
;;; run multiple copies of a model in parallel and get the results of each returned
;;; in a list.
;;;
;;; Each of the ACT-R instances is created in its own package and loads the full
;;; ACT-R sources into that package.  Instantiating the model involves compiling
;;; and then loading the model file(s) into each of those separate packages.  There
;;; are some restrictions because of that:
;;;  - The model code should not call reload since there will only be a single
;;;    compiled file which will be specific to one of the packages.
;;;  - The model code should not include any package specific code, or if it 
;;;    does, then it must use the full package specifier for all references to 
;;;    that code and that code must be thread-safe.
;;;  - The model file(s) must include all the code necessary to run it.
;;;  - Any code which writes out data should be sure to use unique file names
;;;    and/or separate output streams i.e. if everything is writing to the
;;;    default *standard-output* it could be very difficult to read, or worse
;;;    could lead to errors if the Lisp's output functions aren't actually
;;;    thread safe on the default streams (surprising but seems to be true
;;;    in some cases!).
;;;
;;; Since the values returned from the model runs will each be generated in a
;;; different package it is probably best to avoid returning symbols as results
;;; since they will be symbols from that package.
;;;
;;; Because this recompiles the original ACT-R sources you will need to force
;;; them to be recompiled the next time you start a Lisp and load ACT-R since
;;; the existing "fasls" will be compiled for a package which doesn't exist.
;;;
;;; How many instances to create for best performance is going to depend on
;;; a lot of factors.  Perhaps the biggest is memory availability and usage
;;; within the Lisp being used since each instance is a full copy of the ACT-R
;;; system code.  That's likely going to set the upper limit of how many you
;;; can create and actually run.  I can get about a half dozen in ACL and
;;; CCL on a Windows 7 machine with 8GB of RAM before things start to become
;;; unstable.
;;;
;;; There's an example at the bottom of this file showing it being used to run
;;; the zbrodoff model from unit 4 of the tutorial.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; create-parallel-instances (number)
;;;
;;; Number is how many independent ACT-R instances to create and must be a positive
;;; integer.
;;; 
;;; How many to create for best performance is going to require testing by the 
;;; user because it's going to depend on the Lisp and machine involved since
;;; there will be a lot of memory required to create the separate instances and
;;; each will be run in its own process/thread.  
;;;
;;; This function should only be called once to create the instances.
;;;
;;; If it is successful it returns the number specified, otherwise it will
;;; print warnings and return nil.
;;;
;;; instantiate-parallel-models (&rest files)
;;;
;;; Files should be one or more pathname designators with a type of "lisp".
;;;
;;; For each of the parallel instances created with create-parallel-instances
;;; this compiles and then loads each of the files specified in the order
;;; given into that instance.
;;;
;;; If this successfully loads the files into each instance it will return
;;; the number of instances.  If there are any problems warnings will be printed
;;; and nil returned.
;;;
;;; 
;;; destroy-parallel-instances ()
;;;
;;; This function will delete the packages created by create-parallel-instances.
;;; If the packages are deleted then create-parallel-instances would have to be
;;; called again before being able to run parallel models.
;;; 
;;; If all the instances are successfully deleted then the number of instances that
;;; were destroyed is returned.  If there are any problems then warnings will be
;;; printed and nil returned.
;;;
;;;
;;; 
;;; run-parallel-models (function &optional (time-out 60) parameters &rest more-parameters)
;;;
;;; function should be a string which contains the name of a function
;;; to call in each of the instances to run the model.  It does not
;;; have to name a function in the default/current package.
;;;
;;; time-out is a positive number in seconds which indicates how long
;;; the instances should be allowed to run before being terminated 
;;; automatically.
;;;
;;; parameters should be a list of parameters which will be passed to 
;;; the function specified.  The elements of that list will be written
;;; to a string which will then be evaluated in one of the instances.
;;;
;;; more-parameters is zero or more lists like parameters.
;;;
;;; If more-parameters are not specified then each instance which
;;; was created will effectively execute: 
;;;
;;;   (apply (read-from-string function) parameters)
;;; 
;;; with all the symbols in parameters being local to the package in which
;;; it is being run.
;;;
;;; Two values are returned.  The first is a list containing the result 
;;; of that call for each instance which was run.  A result is the return 
;;; value from the call if it completed before the time-out expired, the 
;;; keyword :time-out if the function did not complete before the time-out 
;;; period, or the keyword :error if the instance encountered an error 
;;; while running the function provided.  The second parameter is a list
;;; of conses for the error conditions which occurred with one for each 
;;; occurrence of :error in the results list.  The car of the cons is the
;;; error condition encountered and the cdr is a string with the printing
;;; of that error message in the package which generated it.
;;;
;;; If more-parameters contains ome or more items and fewer than the 
;;; number of instances which were created then each of the lists
;;; of parameters specified in parameters and more-parameters will
;;; be passed to a separate instance to be run and it will return
;;; two values as described above.
;;; The first return value will be effectively as if this were called:
;;;
;;; (mapcar (lambda (x) (apply function x)) (append (list parameters) more-parameters))
;;; 
;;; with each of the elements being generated in parallel and also
;;; subject to the time-out and error detection as indicated above.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Decided that since modern CPUs have multiple cores in them and most Lisps
;;; support threading that utilizes multiple cores I should try to take
;;; advantage of that for model running as well as general code execution as
;;; is done with the parallel-computation code (I don't recommend trying to
;;; use the two together).
;;;
;;; This is similar to the parallel-computation code, except this instantiates 
;;; multiple ACT-Rs in separate packages to run things.
;;;
;;; Right now it's really bare-bones -- create the instance, load the code into
;;; them, run as needed, and kill when done. 
;;;
;;; The processor allocation is up to the Lisp to handle.  So how well it spreads
;;; things across processors/cores is going to require testing with the Lisp and
;;; machine involved, and memory use is probably also a big factor since there are
;;; multiple full instances of ACT-R loaded.
;;;
;;; Just record stuff in globals since I'm only allowing one "set" of instances
;;; to be used at a time.
;;;
;;; Thread safety is up to the user -- it assumes what is being run is safe.
;;;
;;; A "better" system would be to spawn a fresh lisp process for each and then
;;; have that run things and send the results back.  I have a system which can
;;; do that, but it's very Lisp and OS specific since it needs to be able to
;;; run system commands to start the other jobs and that doesn't seem reasonable
;;; or general enough to make available with ACT-R.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defvar *parallel-actr-package-count* 0)
(defvar *existing-parallel-packages* nil)
(defvar *parallel-packages-running* nil)
(defvar *parallel-packages-done* 0)
(defvar *parallel-packages-lock* nil)

(defun make-and-load-actr-package ()
  (handler-case
    (let ((name (format nil "ACT-R-~d" (incf *parallel-actr-package-count*))))
      (if (find :allegro-ide *features*)
          (make-package name :use '(:cl-user :cg-user :cg :cl :excl))
        (if (find :allegro *features*)
            (make-package name :use '(:cl-user :cl :excl))
          (if (find :ccl *features*)
              (make-package name :use '(:cl-user :cl :ccl))
            (make-package name :use '(:cl-user :cl)))))
      (let ((cp (package-name *package*))
            (cm *act-r-modules*))
        (unwind-protect
            (progn
              (eval `(in-package ,name))
              (let ((*features* (remove :act-r *features*))
                    (*modules* (set-difference *modules* cm :test 'string-equal)))
                
                (pushnew :actr-recompile *features*)
                (pushnew :actr-fast *features*)
                (pushnew :standalone *features*)
                
                (load "ACT-R:load-act-r.lisp")))
          (eval `(in-package ,cp))))
      name)
    (error (x)
      (print-warning "Encountered error ~/print-error-message/ while trying to create a new package with ACT-R." x))))

(defun create-parallel-instances (number)
  (if *existing-parallel-packages*
      (print-warning "There are already ~d parallel instances available.  None created." (length *existing-parallel-packages*))
    (progn
      (setf *parallel-actr-package-count* 0)
      (dotimes (i number number)
        (aif (make-and-load-actr-package) 
             (push-last it *existing-parallel-packages*)
             (return-from create-parallel-instances nil))))))

(defun instantiate-parallel-models (&rest files)
  (if (not (every 'probe-file files))
      (print-warning "These files were not found when trying to instantiate parallel models: ~{~s~^, ~}"
                     (mapcan (lambda (x) (unless (probe-file x) (list x))) files))
    (let ((cp (package-name *package*))
          (cm *act-r-modules*)
          (success (length *existing-parallel-packages*)))
      (dolist (package *existing-parallel-packages*)
        (eval `(in-package ,package))
        ;; Want to make sure each package loads any
        ;; 'requires' even though they may have already been
        ;; loaded -- should fix that at some point.
        
        (let ((*modules* (set-difference *modules* cm :test 'string-equal)))
          
          (pushnew :actr-recompile *features*)
          (pushnew :actr-fast *features*)
          
          (dolist (x files)
            (handler-case (compile-and-load x)
              (error (e)
                (eval `(in-package ,cp))
                (print-warning (format nil "Error ~/print-error-message/ in compile and load of file ~s in package ~s~%" e x package))
                (return-from instantiate-parallel-models nil)))))
          (eval `(in-package ,cp)))
      success)))
  
(defun destroy-parallel-instances ()
  (if *existing-parallel-packages*
      (let ((count (length *existing-parallel-packages*)))
        (dolist (x *existing-parallel-packages*)
          (let ((cp (package-name *package*)))

            (eval `(in-package ,x))
            (eval (read-from-string "(stop-des)"))
            (eval `(in-package ,cp))
            (delete-package x)))
        (setf *existing-parallel-packages* nil)
        count)
    (print-warning "There are no parallel instances to destroy.")))


(defun run-parallel-models (function &optional (time-out 60) parameters &rest more-parameters)
  (cond (*parallel-packages-running*
         (print-warning "Run-parallel-models cannot be run multiple times.  You must wait for the previous run to finish."))
        ((null *existing-parallel-packages*)
         (print-warning "There are no parallel instances available to run models.  First create and instantiate some."))
        ((not (stringp function))
         (print-warning "Function parameter to run-parallel-models must be a string not ~s" function))
        ((not (and (numberp time-out) (plusp time-out)))
         (print-warning "Time-out parameter to run-parallel-models must be a positive number not ~s" time-out))
        ((and more-parameters (>= (length more-parameters) (length *existing-parallel-packages*)))
         (print-warning "Too many sets of parameters passed to run-parallel-models because there are only ~d instances available."
                        (length *existing-parallel-packages*)))
        (t
         (let* ((count (if more-parameters 
                           (1+ (length more-parameters))
                         (length *existing-parallel-packages*)))
                (status (make-list count :initial-element :time-out))
                (result (make-list count :initial-element nil))
                (jobs (make-list count :initial-element nil)))
             
           (unwind-protect
               (let ((params (if more-parameters
                                 (append (list parameters) more-parameters)
                               (make-list count :initial-element parameters))))
                 (setf *parallel-packages-running* t)
                 (setf *parallel-packages-done* 0)
                 (setf *parallel-packages-lock* (bt:make-lock "Parallel-models"))
                 
                 (dotimes (i count)
                   (let ((pack (nth i *existing-parallel-packages*))
                         (index i)
                         (p (format nil "~s" (nth i params))))
                     (setf (nth i jobs)
                       (bt:make-thread  
                        (lambda ()
                          (eval `(in-package ,pack))
                          (multiple-value-bind (r e)
                              (ignore-errors (apply (read-from-string function) (read-from-string p)))
                            
                            (bt:with-lock-held (*parallel-packages-lock*)
                            
                              (if (subtypep (type-of e) 'condition)
                                  (setf (nth index status) :error
                                    (nth index result) (cons e (format nil "Error: ~/print-error-message/~%" e)))
                                (setf (nth index status) :success
                                  (nth index result) r))
                            
                              (setf (nth index jobs) nil)
                              (incf *parallel-packages-done*))))
                        :name pack))))
                 
                 (let ((start (get-internal-real-time)))
                   (while (and (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) time-out)
                                 (< (bt:with-lock-held (*parallel-packages-lock*) *parallel-packages-done*) count))
                     (process-events))))
               
             (unless (= count (bt:with-lock-held (*parallel-packages-lock*) *parallel-packages-done*))
               (dolist (x jobs)
                 (when x
                   (bt:destroy-thread x))))
             (setf *parallel-packages-running* nil))
           
           (let ((res (make-list count))
                 (errors nil))
             (dotimes (i count)
               (setf (nth i res) (case (nth i status)
                                   (:time-out :time-out)
                                   (:error (push-last (nth i result) errors) :error)
                                   (:success (nth i result)))))
             (values res errors))))))


(provide "parallel-models")

#|

Example of using it to run the zbrodoff model from unit 4 of the tutorial.
This was run using this version of ACL:
 
International Allegro CL Enterprise Edition
9.0 [Windows *SMP*] (Jun 15, 2016 12:41)
Copyright (C) 1985-2012, Franz Inc., Oakland, CA, USA.  All Rights Reserved.


Running the experiment 4 times sequentially took 26.443 seconds
and running it in 4 parallel instance took 9.734 seconds (although
that didn't include averaging the results of the 4 runs since the
zbrodoff-compare function doesn't return the results but that
would have only added a very small amount of time to the end).

######### Loading of ACT-R 7 is complete #########
CG-USER(2): (compile-and-load "ACT-R:tutorial;lisp;zbrodoff.lisp")
;;; Compiling file ACT-R:tutorial;lisp;zbrodoff.lisp (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.lisp)
;;; Writing fasl file ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Fasl write complete
; Fast loading ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
#|Warning: Non-ACT-R messages during load of "ACT-R:tutorial;unit4;zbrodoff-model.lisp":
; Loading
;    C:\Users\db30\Desktop\actr7.x\tutorial\unit4\zbrodoff-model.lisp

 |#
T
CG-USER(3): (time (zbrodoff-compare 4))
CORRELATION:  0.286
MEAN DEVIATION:  1.312

              2 (64)      3 (64)      4 (64)
Block  1  2.297 (64)  2.796 (64)  3.301 (64)
Block  2  2.301 (64)  2.798 (64)  3.299 (64)
Block  3  2.297 (64)  2.798 (64)  3.300 (64)
; cpu time (non-gc) 23.171875 sec user, 1.390625 sec system
; cpu time (gc)     1.937500 sec user, 0.000000 sec system
; cpu time (total)  25.109375 sec user, 1.390625 sec system
; cpu time (thread) 20.640625 sec user, 0.500000 sec system
; real time  26.443000 sec (100.2%)
; space allocation:
;  22,587,443 cons cells, -3,614,648,864 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(4): (clear-all)
NIL
CG-USER(5): (require-extra "parallel-models")
;;; Compiling file C:\Users\db30\Desktop\actr7.x\extras\parallel-models\parallel-models.lisp
;;; Writing fasl file C:\Users\db30\Desktop\actr7.x\extras\parallel-models\parallel-models.fasl
;;; Fasl write complete
; Fast loading C:\Users\db30\Desktop\actr7.x\extras\parallel-models\parallel-models.fasl
T
CG-USER(6): (create-parallel-instances 4)

< deleted 4 instances of compiling and loading the ACT-R code >

CG-USER(7): (instantiate-parallel-models "ACT-R:tutorial;lisp;zbrodoff.lisp")
;;; Compiling file ACT-R:tutorial;lisp;zbrodoff.lisp (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.lisp)
;;; Writing fasl file ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Fasl write complete
; Fast loading ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Compiling file ACT-R:tutorial;lisp;zbrodoff.lisp (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.lisp)
;;; Writing fasl file ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Fasl write complete
; Fast loading ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Compiling file ACT-R:tutorial;lisp;zbrodoff.lisp (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.lisp)
;;; Writing fasl file ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Fasl write complete
; Fast loading ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Compiling file ACT-R:tutorial;lisp;zbrodoff.lisp (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.lisp)
;;; Writing fasl file ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Fasl write complete
; Fast loading ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
4
CG-USER(8): (time (run-parallel-models "zbrodoff-compare"  60 '(1)))
; cpu time (non-gc) 36.312500 sec user, 1.875000 sec system
; cpu time (gc)     2.171875 sec user, 0.000000 sec system
; cpu time (total)  38.484375 sec user, 1.875000 sec system
; cpu time (thread) 7.531250 sec user, 0.046875 sec system
; real time  9.734000 sec (414.6%)
; space allocation:
;  21,063,595 cons cells, 1,126,146,176 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
(NIL NIL NIL NIL)
NIL

<the result print-out is in the ACL Console window instead of the Debug (listener) window because it's from a separate thread
 but looks like this:


              2 (64)      3 (64)      4 (64)
Block  1  2.298 (64)  2.800 (64)  3.295 (64)
Block  2  2.300 (64)  2.790 (64)  3.297 (64)
Block  3  2.303 (64)  2.795 (64)  3.289 (64)

              2 (64)      3 (64)      4 (64)
Block  1  2.306 (64)  2.798 (64)  3.306 (64)
Block  2  2.300 (64)  2.795 (64)  3.306 (64)
Block  3  2.295 (64)  2.800 (64)  3.295 (64)

              2 (64)      3 (64)      4 (64)
Block  1  2.304 (64)  2.789 (64)  3.298 (64)
Block  2  2.297 (64)  2.784 (64)  3.300 (64)
Block  3  2.295 (64)  2.787 (64)  3.293 (64)

              2 (64)      3 (64)      4 (64)
Block  1  2.300 (64)  2.804 (64)  3.290 (64)
Block  2  2.297 (64)  2.801 (64)  3.309 (64)
Block  3  2.301 (64)  2.804 (64)  3.295 (64)
>

 
CG-USER(9): (destroy-parallel-instances)
4
|#


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
