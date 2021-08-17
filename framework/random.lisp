;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : random.lisp
;;; Version     : 1.1
;;; 
;;; Description : Module that contains a pseudo-random number generator.
;;; 
;;; Bugs        : 
;;;
;;; To do       : Roll rand-time into the scheduling functions
;;;             : Fill in the API details
;;; 
;;; ----- History -----
;;;
;;; 2004.09.17 Dan
;;;             : Creation
;;; 2005.01.04 Dan
;;;             : Actually put a version of the Mersenne Twister in here.
;;; 2005.01.07 Dan
;;;             : Restored rand-time and :randomize-time so it works the
;;;             : way it used to in PM (for now).
;;; 2005.01.13 Dan
;;;             : * Fixed an issue with how :seed set and reported values.
;;;             :   If one was set explicitly, it didn't always report that
;;;             :   same value back, but did properly reset the seed.
;;;             : * Changed act-r-noise so it doesn't call random when s is
;;;             :   0.
;;; 2005.02.10 Dan
;;;             : * Changed log to log-coerced in act-r-noise.
;;; 2005.02.11 Dan
;;;             : * Changed the declare in genrand_int32.
;;; 2005.10.19 Dan
;;;             : * Slight change to genrand_int32 to give y an initial value
;;;             :   since the declare is a problem since y gets an initial
;;;             :   value of nil and CMUCL doesn't like that at all...
;;; 2006.01.17 Dan
;;;             : * Updated the version to 1.0 since there haven't been any
;;;             :   problems it's time to drop the "a".
;;; 2006.08.11 Dan
;;;             : * Adjusted randomize-time function and corresponding parameter
;;;             :   so that the param forces the value to be an integer thus
;;;             :   the check isn't needed in the function.  Also doesn't
;;;             :   bother to do the rounding now since the scheduler does that
;;;             :   automatically, and simplified the number of math operation
;;;             :   necessary to get the time.
;;; 2006.09.11 Dan
;;;             : * Take the unnecessary zerop test out of act-r-noise because
;;;             :   the plusp already rejects that case.
;;; 2009.09.10 Dan
;;;             : * Moved permute-list here from the act-gui-interface.lisp file.
;;; 2011.09.02 Dan
;;;             : * Wrapped an abs around the initial seed setting since some
;;;             :   Lisps (ACL 6.2 in the standalone in particular) were returning
;;;             :   a negative from get-internal-real-time for some reason.
;;; 2012.09.27 Dan
;;;             : * Added a genrand_real2_L for internal use which specifies
;;;             :   the float as long for the division to avoid the situation
;;;             :   where the result is 1.0 due to the floating point size.
;;;             :   That's only used when the request is for an integer however
;;;             :   to avoid returning a different float type than asked for.
;;;             :   Thus, this only avoids (act-r-random INT) = INT errors not
;;;             :   (act-r-random FLOAT) = FLOAT if the default float size is
;;;             :   too small to represent 1 - 4294967295/4294967296.0.  Of 
;;;             :   course if the largest float size is too small to represent
;;;             :   that then errors are also still possible for integers too.
;;; 2015.08.13 Dan
;;;             : * Basically eliminate rand-time and have it just print a warning
;;;             :   and return the time it was passed for now.
;;; 2015.08.17 Dan
;;;             : * Added the randomize-time-ms function for randomizing times
;;;             :   that are in ms because they need to return integer results.
;;;             :   It's not quite the same as converting to seconds, randomizing 
;;;             :   that, and then rounding back to milliseconds because that 
;;;             :   is inefficient, but it's pretty close (the probability of
;;;             :   the extreme values in the range are uniform with this whereas
;;;             :   with the other method they aren't since they would be rounded
;;;             :   to milliseconds after the fact).
;;; 2017.01.11 Dan
;;;             : * Adding dispatcher functions for act-r-random and permute-list.
;;; 2017.01.18 Dan
;;;             : * Removed the echo-act-r-output from commands.  The assumption
;;;             :   now is that echoing is handled by the user.
;;;             : * Use handle-evaluate-results.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.08 Dan
;;;             : * Reworked the local versions of the remote commands to not
;;;             :   go out through the dispatcher.
;;; 2017.06.01 Dan
;;;             : * Changed how act-r-random checks for a current model and gets
;;;             :   the module to avoid the warnings when it has to default to 
;;;             :   the default state.
;;; 2017.06.28 Dan
;;;             : * Adding a lock on the module, make the global actually a
;;;             :   module and not just a mersenne-twister, and then add a
;;;             :   new global to serve as the default state.
;;; 2017.12.14 Dan
;;;             : * Fix a typo in the doc string for permute-list.
;;; 2018.02.28 Dan
;;;             : * Add remote commands for act-r-noise, randomize-time, and
;;;             :   randomize-time-ms.
;;; 2018.06.15 Dan
;;;             : * Updated doc strings for consistency.
;;; 2018.08.27 Dan
;;;             : * The incf in the constructor really should be protected by
;;;             :   a lock for safety so adding that.
;;; 2020.01.10 Dan [1.1]
;;;             : * Removed the lambdas from the module interface functions
;;;             :   since that's not allowed in the general system now.
;;; 2020.08.25 Dan
;;;             : * Replaced constants that had *...* with +...+ to avoid the
;;;             :   warnings in SBCL.
;;;             : * Switch from defconstant to the define-constant macro to 
;;;             :   avoid issues with SBCL (instead of redefining defconstant
;;;             :   for SBCL as was done previously).
;;;             : * Specify an initial value and change the type for the array
;;;             :   to make SBCL happy...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; act-r-random
;;;
;;; act-r-noise
;;;
;;; randomize-time
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; I chose to use the Mersenne Twister generator (as implemented in the C file
;;; mt19937ar.c) for the pseudorandom number generator.  It is considered to
;;; be the best one available for Monte Carlo simulation and is currently the
;;; same one used already in ACL and CMUCL as well as being an option for 
;;; LispWorks.  So, putting it into the code for ACT-R in many cases doesn't
;;; even really change how the models will run since the same generator would
;;; have been used with random anyway.
;;;
;;; The big benefit is in being able to record the seed in a consistent cross-
;;; platform manner that is easy to work with instead of the Lisp specific
;;; *random-states*.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;(eval-when (:compile-toplevel :Load-toplevel :execute)
;  (proclaim '(optimize (speed 3) (space 0) (saftey 0))))

;;; Start off with code for the pseudorandom number generator

(defvar *random-module-counter* -1)
(defvar *random-lock* (bt:make-lock "random-lock-count"))

(defstruct (mersenne-twister (:conc-name mt-))
  (n 624 :type fixnum)
  (m 397 :type fixnum)
  (mti 625 :type fixnum)
  (mt (make-array 624 :element-type '(or bignum integer) :initial-element 0) :type vector)
  (count 0 :type integer)
  (start (bt:with-lock-held (*random-lock*) (incf *random-module-counter*)) :type integer)
  (initial-seed 5489 :type integer))

;; Some constants from the algorithm

(define-constant +matrix_a+ #x9908b0df)
(define-constant +upper_mask+ #x80000000)
(define-constant +lower_mask+ #x7fffffff)


;; Truncating numbers down to 32 bits with ldb so make this a constant
(define-constant +byte-32-0+ (byte 32 0))
;(defconstant *b1* (byte 1 31))

(defvar *default-random-state* (make-mersenne-twister))

;;; functions translated from the C code of mt19937ar.c and 
;;; modified as necessary to take a state parameter.

(defun init_genrand (s &optional (state *default-random-state*))
  (setf (svref (mt-mt state) 0) (ldb +byte-32-0+ s))
  
  (setf (mt-mti state) 1)
  (do ()
      ((= (mt-mti state) (mt-n state)))
    (setf (svref (mt-mt state) (mt-mti state))
      (ldb +byte-32-0+
           (+ (* 1812433253
                 (logxor (svref (mt-mt state) (1- (mt-mti state)))
                         (ash (svref (mt-mt state) (1- (mt-mti state))) -30)))
              (mt-mti state)))
      )
    (incf (mt-mti state))))


(defun init_by_array (init_key &optional (state *default-random-state*))
  (init_genrand 19650218 state)
  (let ((key_length (length init_key))
        (i 1 )
        (j 0 ))
    (declare (fixnum i j))
    (do ((k (if (> (mt-n state) key_length)
                (mt-n state)
              key_length)
            (1- k)))
        ((zerop k))
      (setf (svref (mt-mt state) i)
        (ldb +byte-32-0+
             (+ (logxor (svref (mt-mt state) i)
                        (* 1664525
                           (logxor (svref (mt-mt state) (1- i))
                                   (ash (svref (mt-mt state) (1- i)) -30))))
                (svref init_key j) j)))
      (incf i)
      (incf j)
      (when (= j key_length)
        (setf j 0))
      (when (= i (mt-n state))
        (setf (svref (mt-mt state) 0) (svref (mt-mt state) (1- (mt-n state))))
        (setf i 1)))
    
    (do ((k (1- (mt-n state))
           (1- k)))
        ((zerop k))
      (setf (svref (mt-mt state) i)
        (ldb +byte-32-0+
             (- (logxor (svref (mt-mt state) i)
                      (* 1566083941
                         (logxor (svref (mt-mt state) (1- i))
                                 (ash (svref (mt-mt state) (1- i)) -30))))
                i)))
      (incf i)
      (when (= i (mt-n state))
        (setf (svref (mt-mt state) 0)
          (svref (mt-mt state) (1- (mt-n state))))
        (setf i 1)))
    
    (setf (svref (mt-mt state) 0) #x80000000)))

;; Some different versions of the computation are included here
;; because I want to test them in different lisps/oss

(defun genrand_int32(&optional (state *default-random-state*))
  (let ((y 0))
    (declare (type integer y))
    
    (when (>= (mt-mti state) (mt-n state))
      (let ((kk 0))
        (when (> (mt-mti state) (mt-n state))
          (init_genrand 5489 state))
        (do ()
            ((>= kk (- (mt-n state) (mt-m state))))
          (setf y 
            ;(logior (logand (svref mt kk) +upper_mask+)
            ;        (ldb (byte 31 0) (svref mt (1+ kk)))) 
            
            ;(dpb (ldb *b1* (svref mt kk))
            ;     *b1* 
            ;     (ldb (byte 31 0) (svref mt (1+ kk))))
            
            (logior (logand (svref (mt-mt state) kk) +upper_mask+)
                    (logand (svref (mt-mt state) (1+ kk)) +lower_mask+))
            
            )
          (setf (svref (mt-mt state) kk)
            (logxor (svref (mt-mt state) (+ kk (mt-m state)))
                    (ash y -1)
                    (if (zerop (logand y 1))
                        0
                      +matrix_a+)))
          (incf kk))
        
        (do ()
            ((>= kk (1- (mt-n state))))
          (setf y 
            ;(logior (logand (svref mt kk) +upper_mask+)
            ;        (ldb (byte 31 0) (svref mt (1+ kk))))
            
            ;(dpb (ldb *b1* (svref mt kk))
            ;     *b1* 
            ;     (ldb (byte 31 0) (svref mt (1+ kk))))
            
            (logior (logand (svref (mt-mt state) kk) +upper_mask+)
                    (logand (svref (mt-mt state) (1+ kk)) +lower_mask+))
            )
          (setf (svref (mt-mt state) kk)
            (logxor (svref (mt-mt state) (+ kk (- (mt-m state) (mt-n state))))
                    (ash y -1)
                    (if (zerop (logand y 1))
                        0
                      +matrix_a+)))
          (incf kk))
        
        (setf y 
          ;(logior (logand (svref mt (1- *n*)) +upper_mask+)
          ;          (ldb (byte 31 0) (svref mt 0)))
          
          ;(dpb (ldb *b1* (svref mt (1- *n*)))
          ;       *b1* 
          ;     (ldb (byte 31 0) (svref mt 0)))
          
          (logior (logand (svref (mt-mt state) (1- (mt-n state))) +upper_mask+)
                    (logand (svref (mt-mt state) 0) +lower_mask+))          
          )
        (setf (svref (mt-mt state) (1- (mt-n state)))
          (logxor (svref (mt-mt state) (1- (mt-m state)))
                  (ash y -1)
                  (if (zerop (logand y 1))
                      0
                    +matrix_a+)))
        
        (setf (mt-mti state) 0)))
    
    (setf y (svref (mt-mt state) (mt-mti state)))
    (incf (mt-mti state))
    (incf (mt-count state))
    
    
    (setf y (logxor y (ash y -11)))
    (setf y (logxor y (logand (ash y 7) #x9d2c5680)))
    (setf y (logxor y (logand (ash y 15) #xefc60000)))
    (setf y (logxor y (ash y -18)))
    
    y))

(defun genrand_real2_L (&optional (state *default-random-state*))
    (/  (genrand_int32 state) 4294967296.0L0))

(defun genrand_real2 (&optional (state *default-random-state*))
    (/  (genrand_int32 state) 4294967296.0))

#|
This function should produce the same numbers as the 2000 
in the test output file provided with mt19937ar.c -> mt19937ar.out.txt 

(defun test-mt-generator ()
  (let ((res1 nil)
        (res2 nil)
        (init (make-array 4 :initial-contents 
                          '(#x123 #x234 #x345 #x456))))
    (init_by_array init)
    (dotimes (i 1000)
      (push (genrand_int32) res1))
    (dotimes (i 1000)
      (push (genrand_real2) res2))
    (list (reverse res1) (reverse res2))))
|#

;; Here's the actual module definition code

(defstruct act-r-random-module 
  state randomize-time (lock (bt:make-lock "random-lock")))


(defun create-random-module (ignore)
  (declare (ignore ignore))
  (let ((state (make-mersenne-twister)))
    
    ;; Here's where we initialize the thing - not in reset
    ;; since that defeats the purpose of having a good generator.
    
    ;; Use get-internal-real-time to randomly seed things.
    ;; However, get-internal-real-time may be > 32 bits and we also don't
    ;; want two modules having the same initial seed because they happened
    ;; to be created at the "same" time.  So also add a count of modules
    ;; created so far to the time returned and if that is > 32 bits
    ;; build an array of numbers to do the initializing.
    
    (setf (mt-initial-seed state)
      (abs (+ (mt-start state)
              (get-internal-real-time))))
    
    (init-random-state-from-seed state (mt-initial-seed state))
    
    (make-act-r-random-module :state state)))


(defun init-random-state-from-seed (state seed)
  
  (if (<= (ceiling (log seed 2)) 32)
      (init_genrand seed state)
    (init_by_array (make-array 
                    2 :initial-contents 
                    (list (ldb (byte 32 0) seed)
                          (ldb (byte 32 32) seed)))
                   state)))

(defvar *default-random-module* (create-random-module nil))



(defun random-module-params (module param)
  (bt:with-lock-held ((act-r-random-module-lock module))
    (cond ((consp param)
           
           (case (car param)
             (:seed (unless (symbolp (cdr param))
                      ;; ignore the case when it sends the "default"
                      (let ((seed (first (cdr param)))
                            (count (second (cdr param)))
                            (state (act-r-random-module-state module)))
                        
                        (init-random-state-from-seed state seed)
                        (setf (mt-count state) 0)
                        (setf (mt-initial-seed state) seed)
                        (dotimes (i count)
                          (genrand_int32 state))
                        (list seed count))))
             
             (:randomize-time (setf (act-r-random-module-randomize-time module)
                                (cdr param)))))
          (t 
           (case param
             (:seed (list (mt-initial-seed (act-r-random-module-state module))
                          (mt-count (act-r-random-module-state module))))
             (:randomize-time (act-r-random-module-randomize-time module)))))))


(defun valid-random-module-seed (x)
  (or (and (symbolp x) (eq x 'no-default))
                         (and (listp x)
                              (= (length x) 2)
                              (integerp (first x))
                              (not (minusp (first x)))
                              (integerp (second x))
                              (not (minusp (second x))))))

(defun valid-randomize-time-value (x)
  (or (eq x t)
      (null x)
      (integerp x)))

(define-module-fct 'random-module nil 
  (list 
   (define-parameter :seed 
     :documentation "Current seed of the random number generator"
     :valid-test 'valid-random-module-seed
     :default-value 'no-default
     :warning "a list of two non-negative integers"
     )
   (define-parameter :randomize-time 
       :valid-test 'valid-randomize-time-value
     :default-value nil
     :warning "T, nil, or an integer"
     :documentation 
     "Allows the timing of certain actions to vary")
   )
  :version "1.1"
  :documentation 
  "Provide a good and consistent source of pseudorandom numbers for all systems"
  :creation 'create-random-module
  ; Don't need to do anything special on reset :reset ...
  :params 'random-module-params)


(defun act-r-random (limit)
  (if (and (numberp limit) (plusp limit))
      (let ((module (if (current-model-struct)
                        (get-module random-module)
                      *default-random-module*)))
        (bt:with-lock-held ((act-r-random-module-lock module))
          (cond ((integerp limit)
                 (if (< limit #xffffffff)
                     (values (floor (* limit (genrand_real2_L (act-r-random-module-state module)))))
                   (let ((accum 0)
                         (nums (ceiling (/ (1+ (log limit 2)) 32))))
                     (dotimes (i nums)
                       (setf accum (+ (ash accum 32) (genrand_int32 (act-r-random-module-state module)))))
                     (mod accum limit))))
                (t
                 (* limit (genrand_real2 (act-r-random-module-state module)))))))
    (print-warning "Act-r-random called with an invalid value ~s" limit)))

(add-act-r-command "act-r-random" 'act-r-random "Return a random number up to limit using the current model's random stream. Params: limit." nil)


(defun act-r-noise (s)
  "Approximates a sample from a normal distribution with mean zero and
   the given s-value (/ (sqrt (* 3.0 variance)) 3.1416)."
  ;; Need to test bound because of short-float lack of precision
  (if (and (numberp s) (plusp s))
      (let ((p (max 0.0001 (min (act-r-random 1.0) 0.9999))))
        (* s (log-coerced (/ (- 1.0 p) p))))
    (print-warning "Act-r-noise called with an invalid s ~S" s)))
  
(add-act-r-command "act-r-noise" 'act-r-noise "Return a random sample from a logistic distribution with mean 0 and s value provided using the current model's random stream. Params: s-value." nil)

;;; Do it this way so that later it is easier
;;; to just disable rand-time and let the scheduler
;;; use randomize-time so that the modules don't have
;;; to be immediately cleaned up.

(defun randomize-time (time)
  (if (numberp time)
      (let ((rand-module (get-module random-module)))
        (if rand-module
            (let ((rand (bt:with-lock-held ((act-r-random-module-lock rand-module)) (act-r-random-module-randomize-time rand-module))))
              (if (or (not rand) (zerop time))
                  time
                (let* ((tscale (if (numberp rand) rand 3)))  ;; default when t is 3
                  ;; Old way
                  ;(min (float (* time (/ (1- tscale) tscale))))
                  ;(max (float (* time (/ (1+ tscale) tscale)))))
                  ;(+ min (act-r-random (- max min))))
                  
                  ;; simplified
                  (* (/ time tscale) (+ tscale -1 (act-r-random 2.0))))))
          time))
    (progn
      (print-warning "Invalid value passed to randomize-time: ~S" time)
      time)))

(add-act-r-command "randomize-time" 'randomize-time "If :randomize-time is enabled return a random result from the uniform distribution from time - time/n to time + time/n where n is the :randomize-time value. Params: time." nil)

(defun randomize-time-ms (time)
  (if (integerp time)
      (let ((rand-module (get-module random-module)))
        (if rand-module
            (let ((rand (bt:with-lock-held ((act-r-random-module-lock rand-module)) (act-r-random-module-randomize-time rand-module))))
              (if (or (not rand) (zerop time))
                  time
                (let* ((tscale (if (numberp rand) rand 3))  ;; default w
                       (step (round time tscale)))
                  (+ time (- step) (act-r-random (+ 1 step step))))))
          time))
    (progn
      (print-warning "Invalid value passed to randomize-time-ms: ~S. Value must be an integer." time)
      time)))

(add-act-r-command "randomize-time-ms" 'randomize-time-ms "If :randomize-time is enabled return a random integer result from the uniform distribution from time - time/n to time + time/n where n is the :randomize-time value. Params: time." nil)

;;; PERMUTE-LIST  [Function]
;;; Description : This function returns a randomly ordered copy of the passed
;;;             : in list.

(defun permute-list (lis)
  "Return a random permutation of the list"
  (if (and (listp lis) lis)
      (do* ((item (nth (act-r-random (length lis)) lis) (nth (act-r-random (length temp)) temp))
        (temp (remove item lis :count 1) (remove item temp :count 1))
        (result (list item) (cons item result)))
           ((null temp) result))
    nil))

(add-act-r-command "permute-list" 'permute-list "Return a randomly ordered copy of the list provided using the current model's random stream. Params: list." nil)


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
