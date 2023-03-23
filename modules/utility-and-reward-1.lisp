;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell & John Anderson
;;; Copyright   : (c) 2006 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : utility-and-reward-1.lisp
;;; Version     : 6.0
;;; 
;;; Description : The procedural utility computation functions and a module
;;;             : for handling the "reward" given to a production.
;;;             :
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Consider a new command or switch which allows one to
;;;             :     not clear the history after propigating a reward.
;;; 
;;; ----- History -----
;;; 2006.05.22 Dan
;;;             : * Initial creation.
;;; 2006.08.24 Dan
;;;             : * Added the "null" learning event marker (the way setting
;;;             :   both success and failure worked in the old system) - if
;;;             :   trigger-reward is passed nil it just clears the history.
;;; 2006.09.08 Dan
;;;             : * Changed the :at parameter check from posnum to nonneg.
;;; 2006.10.06 Dan
;;;             : * Changed some print-warning calls to model-warning so that
;;;             :   they turn off with the model-warning parameter.
;;; 2006.11.15 Dan
;;;             : * Changed trigger-reward so that it tests its parameter
;;;             :   before generating the event instead of waiting for the 
;;;             :   propigate-reward function to catch problems.
;;;             : * Changed the name of the module from utility-2 to utility
;;;             :   in preparation for making it the default module.
;;;             : * Changed the doc string for the module and updated the
;;;             :   version to 2.0.
;;; 2007.10.26 Dan [2.1]
;;;             : * Added the Utility threshold parameter back into the system.
;;; 2008.02.26 Dan
;;;             : * Fixed a bug in propigate-reward which allowed utility 
;;;             :   learning to go on even when :esc was set to nil.
;;; 2008.07.22 Dan
;;;             : * Changed compute-utility so that it has an optional parameter
;;;             :   which indicates whether or not to store the computed value.
;;; 2008.07.23 Dan
;;;             : * Added the require of "CENTRAL-PARAMETERS" since it does use
;;;             :   it - don't rely on procedural or declarative to load it first.
;;;             : * Added call to the new register-subsymbolic-parameters to
;;;             :   note which parameters should trigger the warning.
;;; 2008.08.01 Dan
;;;             : * Procedural now owns :dat.
;;; 2009.08.12 Dan
;;;             : * Added a utility-offset hook which is called after the normal
;;;             :   utility for a production is computed and set (without noise).  
;;;             :   If it returns a number that's added into the utility value.
;;; 2011.02.16 Dan [2.2]
;;;             : * Added a utility learning trace parameter which prints out
;;;             :   the utility changes when a reward occurs.
;;;             : * Also changed linear-update-utility to take the utility module
;;;             :   as a parameter.
;;; 2011.04.26 Dan
;;;             : * Updated to use millisecond timer for production times.
;;; 2011.04.28 Dan
;;;             : * Suppress warnings about extending productions at initial load.
;;; 2011.05.16 Dan
;;;             : * Changed the formatting of the utility trace slightly.
;;; 2011.06.02 Dan
;;;             : * Added a reward-hook parameter to allow one to override the
;;;             :   effective reward provided to a production.
;;; 2011.08.03 Dan
;;;             : * Added a reward-notify-hook which can hold multiple functions
;;;             :   and gets called during propagate-reward with the reward value.
;;; 2012.03.14 Dan
;;;             : * Added the ability to set :reward to t using spp to be the
;;;             :   equivalent of calling trigger-reward with nil (can't set the
;;;             :   :reward parameter to nil since that's the default and seems
;;;             :   like a bad idea to change that).  
;;;             : * That change requires making a change to how production 
;;;             :   compilation determines whether or not there is a :reward 
;;;             :   setting for the new production.  The old rule still holds --
;;;             :   if they're both numbers then it's the max, and if only one is
;;;             :   a number then the new one gets that number (now that means
;;;             :   the other can be either t or nil).  The new change is that now 
;;;             :   if neither is a number but at least one is t then the new
;;;             :   production will get a value of t.
;;; 2012.06.18 Dan
;;;             : * Fixed a bug with the test for warning about parameter changes 
;;;             :   so that it warns if there are any productions defined.
;;; 2014.09.30 Dan
;;;             : * If reward and utility aren't numbers don't try to print them
;;;             :   with ~f in spp.
;;; 2015.06.04 Dan
;;;             : * Store production action time internally in ms.
;;; 2015.06.05 Dan
;;;             : * Use schedule-event-now for propagate-reward.
;;;             : * Get-default-action-time and get-default-u functions now take
;;;             :   values from the utility module instead of indirectly through
;;;             :   sgp, and the module records dat in ms.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2015.09.01 Dan
;;;             : * Add an optional parameter to trigger-reward to allow the
;;;             :   propagate-reward event to be marked as maintenance.
;;; 2015.09.10 Dan [3.0]
;;;             : * Start of the process of adding a new reward assignment 
;;;             :   mechanism which only rewards productions that have "completed"
;;;             :   all of their actions instead of just having been selected.
;;; 2015.09.11 Dan
;;;             : * Added the new option for :ul as complete to initiate the 
;;;             :   new mechanism.
;;;             : * Propagate-reward tests the switch and updates appropriately.
;;; 2015.09.14 Dan
;;;             : * When a production specifies a reward now that leads to a
;;;             :   trigger-reward being scheduled instead of happening directly
;;;             :   during the "fired" action because when the complete setting
;;;             :   is in effect the actions of the production need to happen
;;;             :   before determining whether or not to reward this production
;;;             :   itself.
;;;             : * The learn-parameters function now updates the requests
;;;             :   stored in the history since the production has executed its
;;;             :   actions at that point.
;;; 2015.10.14 Dan [3.1]
;;;             : * Added a new production parameter available via spp :fixed-utility.
;;;             :   It defaults to nil, but if set to t then utility learning is
;;;             :   disabled for that production and its :u value will remain at
;;;             :   whatever value is set with spp (it can be changed by spp even
;;;             :   while marked as fixed).
;;; 2016.03.09 Dan
;;;             : * Allow a production to have its reward status turned off via
;;;             :   spp, but track if that changes during a run and warn about
;;;             :   it during compilation updating since reward for a compiled
;;;             :   production is only set on inital creation.
;;; 2017.03.13 Dan
;;;             : * Added a remote spp command and just have spp-fct always
;;;             :   decode the parameters passed to it.
;;; 2017.03.14 Dan
;;;             : * Have the remote spp command 'collect' the parameters with a
;;;             :   &rest before passing them to spp-fct.
;;; 2017.08.15 Dan
;;;             : * Added a lock to protect all the parameters.
;;;             : * Changed linear-update-utility to take alpha directly instead
;;;             :   of getting it from the module.
;;; 2017.11.14 Dan
;;;             : * Changed the decoding of the parameters to spp from the main
;;;             :   function to only do it through the remote one -- can't use
;;;             :   strings from the Lisp calls.
;;; 2017.11.16 Dan [4.0]
;;;             : * Changed how trigger-reward/propagate-reward are signaled with
;;;             :   a dummy to clear the history.  Instead of nil now any value
;;;             :   that isn't a number or nil will result in clearing the history.
;;;             :   This makes the reward setting for a production be consistent
;;;             :   with the underlying functions and allows for one to speicfy
;;;             :   meaningful 'dummy' rewards for the trace or other purposes.
;;;             : * Added a remote version of trigger-reward.
;;; 2017.11.20 Dan
;;;             : * Make trigger-reward go out through the dispatcher so it can
;;;             :   be monitored.
;;; 2018.04.16 Dan
;;;             : * Allow :reward-hook, :reward-notify-hook, :utility-hook, and
;;;             :   :utility-offsets to be set to remote commands.
;;;             : * Mark the reward-notify-hook as depricated.
;;; 2018.06.22 Dan
;;;             : * Encode the spp results on the way out through remote.
;;; 2018.07.26 Dan [5.0]
;;;             : * Don't need to track requested actions anymore.
;;; 2020.01.09 Dan [5.1]
;;;             : * To make things safer for external access can't use the 
;;;             :   function object for things like parameter tests or interface
;;;             :   actions.
;;;             : * Add the ability to remove hook functions from the lists.
;;; 2020.08.26 Dan
;;;             : * Removed the path for require-compiled since it's not needed
;;;             :   and results in warnings in SBCL.
;;; 2021.07.09 Dan
;;;             : * Fixed a bug with spp setting :at because it was returning
;;;             :   the time in ms instead of seconds.
;;; 2021.10.19 Dan [6.0]
;;;             : * Set the :required flag on the module to procedural.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description:
;;;
;;; See the new-utility.doc file for all the details, but the general idea is
;;; that instead of tracking P and C and effectively boolean successes/failures
;;; we collapse things into one value called utility and have the successes and
;;; failures represented numerically as a single reward value.
;;; [Note the variable noise value discussed in the doc is not implemented at this
;;; time.]
;;;
;;; Thus, a production only has a utility value, U, which is adjusted over time based
;;; on the rewards received.  The equation for the learning of production i is now:
;;;
;;; Ui(n) = Ui(n-1) + A[Ri(n) - Ui(n-1)]
;;; 
;;; A := the learning rate set by a parameter (defaults to .2).
;;; Ri(n) := the effective reward value of production i at time n.  Like the old 
;;; mechanim, the learning only occurs when directly triggered and all productions 
;;; that have been selected since the last "learning event" are updated (unless the
;;; special complete mode is in effect).  The effective reward is the reward value 
;;; at time n minus the time since the selection of production i.
;;; Ui(0) := set by a parameter (defaults to 0).
;;;
;;; The new meechanism only needs the following general parameters:
;;;
;;; dat := default action time, same as before - the default time to fire a 
;;;        production.
;;; egs := the expected gain s value, same as before.
;;;
;;; utility-hook := a function that can bypass the utility computation for
;;;                 productions, same as before.
;;;
;;; ul := utility learning, t or nil to indicate whether or not to 
;;;       apply the equation above.  There is no decaying version as there is with
;;;       the old parameter pl at this time.
;;; alpha := the learning rate for the equation above.  Note this differs from
;;;          the alpha parameter in the old system because that only applied to
;;;          the production compilation mechanism.
;;; iu := the default U(0) value for an initial (user defined) production.
;;; nu := the default U(0) value for a newly learned (production compilation) production.
;;; reward-hook := a function to bypass the R(n) calculation.
;;;
;;; In addition, this parameter from the old system is now available again
;;; 
;;; ut := the utility threshold.  Productions with a utility less than this
;;;       value will not be chosen during conflict resolution.
;;;
;;; ult := The utility learing trace parameter.  When a reward occurs
;;;        display the utility changes which occur.
;;;
;;; For a production, only the following parameters are needed for spp:
;;;
;;; at := The action time of the production, how long between the production's
;;;       selection and when it fires.  Can be set directly, defaults to :dat.
;;; name := returns the name of the production.  Cannot be set directly.
;;; u := the current U(n) value for the production.  Can be set directly.
;;; utility := the last computed utility value of the production during 
;;;            conflict resolution.  Cannot be set directly.
;;; reward := a reward value to apply when this production fires if set to a
;;;           non-nil value (the default is nil).  Can be set directly.
;;;
;;; Note: in the old system there was a parameter called value which
;;; was used to order productions if :esc was nil, but now the u value is used
;;; regardless of the setting of esc.  However learning cannot take palce if esc
;;; is nil.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; since this module uses :esc make sure that module is available

(require-compiled "CENTRAL-PARAMETERS")

;;; similarly, require productions even though it will most certainly
;;; have been loaded by something else

(require-compiled "PRODUCTIONS")

;;; The structure that is the utility module

(defstruct utility
  egs esc dat ul ut
  iu nu alpha
  utility-hook 
  reward-hook
  history
  offsets
  trace
  reward-notify-hooks
  (param-lock (bt:make-lock "utility-module")))

;;; Functions necessary to support the production parameters


(defun get-default-action-time (p)
  (declare (ignore p))
  (verify-current-mp 
   "No meta-process available when trying to set production action time"
   (verify-current-model 
    "No current model when trying to set production action time"
    (let ((u (get-module utility)))
      (if u
          (bt:with-lock-held ((utility-param-lock u)) (utility-dat u))
        (print-warning "No utility module available when trying to set production action time"))))))

(defun get-default-u (p)
  (declare (ignore p))
  (verify-current-mp 
   "No meta-process available when trying to set production u value"
   (verify-current-model 
    "No current model when trying to set production u value"
    (let ((u (get-module utility)))
      (if u
          (bt:with-lock-held ((utility-param-lock u)) (utility-iu u))
        (print-warning "No utility module available when trying to set production u value"))))))

;;; The parameters added to productions for utility purposes

(suppress-extension-warnings)

(extend-productions utility)
(extend-productions u :default-function get-default-u)
(extend-productions at :default-function get-default-action-time)
(extend-productions reward)
(extend-productions fixed-utility)
(extend-productions reward-changed)

(unsuppress-extension-warnings)

;;; Instead of a simple cons for the history actually store
;;; structures now for ease of reading

(defstruct utility-history name time)



;;; The functions that are required by the procedural system

(defun note-production-selection (production)
  (let ((u (get-module utility)))
    (when u 
      (bt:with-lock-held ((utility-param-lock u))
        (when (utility-ul u)
          (push-last 
           (make-utility-history
            :name production
            :time (mp-time-ms))
           (utility-history u)))))))


(defun productions-action-time (production)
  (production-at production))

(defun compute-utility (production &optional (save nil))
  (let ((u (get-module utility)))
    (when u
      (let (utility-hook esc egs offsets)
        
        (bt:with-lock-held ((utility-param-lock u))
          (setf utility-hook (utility-utility-hook u)
            esc (utility-esc u)
            egs (utility-egs u)
            offsets (utility-offsets u)))
        
        (let* ((over-ride (awhen utility-hook
                                 (dispatch-apply it production)))
               (utility (if (numberp over-ride)
                            over-ride
                          (+ (if (and esc egs (not (zerop egs)))
                               (act-r-noise egs)
                             0)
                           (production-u production)
                           (if offsets
                               (reduce #'+ (mapcar  (lambda (x) 
                                             (let ((val (dispatch-apply x production)))
                                               (if (numberp val)
                                                   val
                                                 0)))
                                             offsets))
                             0)))))
        (if save
            (setf (production-utility production) utility)
          utility))))))


(defun minimum-utility ()
  (let ((u (get-module utility)))
    (when u
      (bt:with-lock-held ((utility-param-lock u))
        (utility-ut u)))))

(defun linear-update-utility (alpha production reward)
  (let ((old (production-u production)))
    (setf (production-u production) (+ old (* alpha (- reward old))))))

(defun trigger-reward (value &optional (maintenance nil))
  (evaluate-act-r-command "trigger-reward" value maintenance))

(defun trigger-reward-internal (value &optional (maintenance nil))
  (cond ((null (current-model))
         (print-warning "No current model.  Trigger-reward has no effect."))
        ((numberp value)
         (schedule-event-now 'propagate-reward :module 'utility
                             :priority :max :params (list value)
                             :output 'medium :maintenance maintenance)
         t)
        (value ;; want to differentiate the param to propagate in some way for this?
         (schedule-event-now 'propagate-reward :module 'utility
                             :priority :max :params (list value)
                             :output 'medium :maintenance maintenance)
         t)
        (t
         (print-warning "Trigger-reward must be called with a non-null value -- nil does not indicate a null reward anymore."))))


(add-act-r-command "trigger-reward" 'trigger-reward-internal "Provide a reward signal for utility learning. Params: reward-value {maintenance?}")

(defun propagate-reward (value)  
  (let* ((u (get-module utility))
         ul reward-notify-hooks ult esc alpha reward-hook history)
    
    (bt:with-lock-held ((utility-param-lock u))
      (setf ul (utility-ul u)
        reward-notify-hooks (utility-reward-notify-hooks u)
        ult (utility-trace u)
        esc (utility-esc u)
        alpha (utility-alpha u)
        reward-hook (utility-reward-hook u)
        history (utility-history u)))
    
    (dolist (hook reward-notify-hooks)
      (dispatch-apply hook value))
    
    (if (numberp value)
        (if (and esc ul)
            (let ((keep nil))
              (when ult
                (model-output " Utility updates with Reward = ~f   alpha = ~f" value  alpha))
              
              (dolist (item history)
                (let ((name (utility-history-name item)))
                  (if (production-fixed-utility name)
                      (when ult
                        (model-output "  Updating of production ~S skipped because it has a fixed utility" name))
                    
                    (let ((delta-t (ms->seconds (- (mp-time-ms) (utility-history-time item)))))
                      
                      (let* ((override (and reward-hook 
                                                (dispatch-apply reward-hook name value delta-t)))
                                 (r (if (numberp override) override (- value delta-t))))
                            
                            (when ult
                              (model-output "  Updating utility of production ~S" name)
                              (model-output "   U(n-1) = ~f   R(n) = ~f [~:[~f - ~f seconds since selection~;from reward-hook function~]]" 
                                            (production-u (utility-history-name item)) r override value delta-t))
                            (linear-update-utility alpha name r)
                            (when ult
                              (model-output "   U(n) = ~f" (production-u name))))))))
              
              (bt:with-lock-held ((utility-param-lock u)) (setf (utility-history u) keep)))
          (print-warning "Trigger-reward can only be used if utility learning is enabled."))
      (if value
          (progn
            (when ult
              (model-output "  Non-numeric reward clears utility learning history."))
            (bt:with-lock-held ((utility-param-lock u)) (setf (utility-history u) nil)))
        (print-warning "Trigger-reward called with nil has no effect at all now.")))))


(defun learn-parameters (production)
  (awhen (production-reward production)
         (schedule-event-now 'trigger-reward :module 'procedural :params (list it) :priority :min :output 'high)))

(defun initialize-utility-for-compiled-production (new-p p1 p2)
  (let* ((at1 (production-at p1))
         (at2 (production-at p2))
         (at (max at1 at2))
         (r1 (production-reward p1))
         (r2 (production-reward p2))
         (reward (or (and (numberp r1)
                          (numberp r2)
                          (max r1 r2))
                     (and (numberp r1) r1)
                     (and (numberp r2) r2)
                     r1 r2)))
    (setf (production-at new-p) at)
    (setf (production-reward new-p) reward)
    (setf (production-u new-p) (car (no-output (sgp :nu))))))


(defun update-utility-for-compiled-production (p3 p1 p2)
  (let ((u (get-module utility))
        ult ul alpha)
    (when (or (production-reward-changed p1) (production-reward-changed p2))
      (model-warning "Parent productions for ~s may now provide different rewards than they did when it was created." p3))
    
    (when u 
      (bt:with-lock-held ((utility-param-lock u))
        (setf ult (utility-trace u)
          ul (utility-ul u)
          alpha (utility-alpha u)))
      (when ul
        (when ult
          (model-output "  Updating utility of production ~S from production compilation" p3)
          (model-output "    U(n-1) = ~f   R(n) = ~f [U(n) for first parent] alpha = ~f" (production-u p3)
                      (production-u p1) alpha))
      
        (linear-update-utility alpha p3 (production-u p1))
      
        (when ult
          (model-output "    U(n) = ~f" (production-u p3)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here's the actual module and associated support code

(defun utility-module-params (u param)
  (bt:with-lock-held ((utility-param-lock u))
    (cond ((consp param)
           
           ;; Changing utility parameters may lead to
           ;; to possibly trigger conflict-resolution if it were waiting
           
           (un-delay-conflict-resolution)
           
           (when (and (> (length (all-productions)) 0)
                      (member (car param) '(:esc :dat :ul :iu)))
             (model-warning "Changing procedural parameters when productions exist unsupported.")
             (model-warning "Results may not be what one expects."))
           
           (case (car param)
             (:esc (setf (utility-esc u) (cdr param)))
             
             (:egs (setf (utility-egs u) (cdr param)))
             (:ul (setf (utility-ul u) (cdr param)))
             (:dat (setf (utility-dat u) (seconds->ms (cdr param))))
             (:iu (setf (utility-iu u) (cdr param)))
             (:nu (setf (utility-nu u) (cdr param)))
             
             (:alpha (setf (utility-alpha u) (cdr param)))
             (:ult (setf (utility-trace u) (cdr param)))
             
             (:ut (setf (utility-ut u) (cdr param)))
             
             (:utility-hook 
              (when (and (cdr param) (utility-utility-hook u))
                (print-warning 
                 "Utility-hook was set to ~S and is being overwritten"
                 (utility-utility-hook u)))
              (setf (utility-utility-hook u) (cdr param)))
             (:reward-hook 
              (when (and (cdr param) (utility-reward-hook u))
                (print-warning 
                 "Reward-hook was set to ~S and is being overwritten"
                 (utility-reward-hook u)))
              (setf (utility-reward-hook u) (cdr param)))
             
             (:utility-offsets
              (setf (utility-offsets u) 
                (set-or-remove-hook-parameter :utility-offsets (utility-offsets u) (cdr param))))
             
             (:reward-notify-hook
              (setf (utility-reward-notify-hooks u) 
                (set-or-remove-hook-parameter :reward-notify-hook (utility-reward-notify-hooks u) (cdr param))))))
          (t 
           (case param
             (:egs  (utility-egs u))
             (:ul (utility-ul u))
             
             (:iu (utility-iu u))
             (:nu (utility-nu u))
             
             (:ut (utility-ut u))
             
             (:alpha (utility-alpha u))
             (:ult (utility-trace u))
             (:utility-hook (utility-utility-hook u))
             (:reward-hook (utility-reward-hook u))
             (:utility-offsets (utility-offsets u))
             (:reward-notify-hook (utility-reward-notify-hooks u)))))))


(defun reset-utility-module (u)
  (bt:with-lock-held ((utility-param-lock u))
    (setf (utility-history u) nil)))

(defun create-utility-module (x)
  (declare (ignore x))
  (make-utility))

(define-module-fct 'utility nil
  (list (define-parameter :esc :owner nil)
        
        (define-parameter :egs :valid-test 'numberp :default-value 0.0
          :warning "a number" :documentation "Expected Gain S")
        (define-parameter :ul :valid-test 'tornil :default-value nil
          :warning "T or nil" :documentation "Utility learning switch")
        (define-parameter :dat :owner nil)
        (define-parameter :iu :valid-test 'numberp :default-value 0
          :warning "a number" :documentation "default U(0) value for an initial (user defined) production")
        (define-parameter :nu :valid-test 'numberp :default-value 0
          :warning "a number" :documentation "default U(0) value for a newly learned production")
        
        (define-parameter :ut :valid-test 'numornil :default-value nil
          :warning "a number or nil" :documentation "Utility Threshold")

        (define-parameter :alpha :default-value .2
          :valid-test 'numberp :warning "a number"
          :documentation "Production learning rate")
        
        (define-parameter :ult :valid-test 'tornil :default-value nil
          :warning "T or nil" :documentation "Utility learning trace")
        
        (define-parameter :utility-hook :valid-test 'local-or-remote-function-or-nil 
          :default-value nil
          :warning "a function, string naming a command, or nil" 
          :documentation "Utility computation hook")
        (define-parameter :reward-hook :valid-test 'local-or-remote-function-or-nil  
          :default-value nil
          :warning "a function, string naming a command, or nil" 
          :documentation "Reward value hook")
        (define-parameter :utility-offsets :valid-test 'local-or-remote-function-or-remove 
          :default-value nil
          :warning "a function, string naming a command, nil, or (:remove <item>)" 
          :documentation "Add additional utility equation components")
        (define-parameter :reward-notify-hook :valid-test 'local-or-remote-function-or-remove 
          :default-value nil
          :warning "a function, string naming a command, nil, or (:remove <item>)" 
          :documentation "Depricated - monitor trigger-reward instead. Functions to call when there is a reward provided"))
  
  :version "6.0" 
  :documentation  "A module that computes production utilities"
    
  :creation 'create-utility-module
  :reset 'reset-utility-module
  :params 'utility-module-params
  :required 'procedural)

(register-subsymbolic-parameters :egs :ul :iu :nu :alpha)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section is all support for spp 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Taken and modified from ACT-R 5 code

(defun production-parameter-fct (production-name &optional parameters)
  "Returns the value of the production parameter(s), or print all if none given."
  (let ((esc (car (no-output (sgp :esc))))
        (ul (car (no-output (sgp :ul))))
        (value nil)
        (values nil))
    (cond (production-name
           (command-output "Parameters for production ~S:" production-name)
           
           (cond (parameters
                  (dolist (parameter parameters)
                    (setf value 
                      (case parameter
                        (:name production-name)
                        (:utility (production-utility production-name))
                        (:u (production-u production-name))
                        (:at (ms->seconds (production-at production-name)))
                        (:reward (production-reward production-name))
                        (:fixed-utility (production-fixed-utility production-name))
                        
                        (t (print-warning "NO PARAMETER ~A DEFINED FOR PRODUCTIONS." parameter)
                           :error)))
                    (push-last value values)
                    (if (numberp value)
                        (command-output " ~S ~6,3F" parameter value)
                      (command-output " ~S ~6@s" parameter value)))
                  values)
                 (t
                  (when esc 
                    (if (numberp (production-utility production-name))
                        (command-output " :utility ~6,3F" (production-utility production-name))
                      (command-output " :utility ~6@s" (production-utility production-name))))
                  
                  (command-output " :u  ~6,3F" (production-u production-name))
                  (command-output " :at ~6,3F" (ms->seconds (production-at production-name)))
                  
                    
                  (when (and esc ul)
                    (if (numberp (production-reward production-name))
                        (command-output " :reward ~6,3F" (production-reward production-name))
                      (command-output " :reward ~6@s" (production-reward production-name)))
                    (command-output " :fixed-utility ~6@s" (production-fixed-utility production-name)))
                  production-name)))
          (t :error))))

;;; This name is dangerously close to what I use in the sgp code, but
;;; for now I'll leave it as is.

(defmacro set-parameter (slot parameter test warning &rest housekeeping)
  "Sets parameter of production p in slot if value passes test, otherwise issue warning."
  `(cond (,test
          (setf (,slot p) value)
          ,@housekeeping
          value)
         (t
          (print-warning
           "PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~:@(~A~)."
           ,parameter value ,warning)
          :error)))


(defun parameters-fct (p parameters)
  "Sets the parameters of the production (internal - user should use spp)."
  
  ;; Changing procedural parameters reschedules conflict resolution
  ;; if it's waiting to happen
         
  (un-delay-conflict-resolution)
         
  ;; Having the name of the production be p and the
  ;; value to be set called value are critical to
  ;; the functioning of the set-parameter macro...
  
  (let ((values nil)
        (ul (car (no-output (sgp :ul)))))
    (if p
        (loop
          
          (unless parameters 
            (return values))
        
        (let* ((parameter (pop parameters))
               (value (pop parameters)))
          
          ;; not sure about this, but I'll leave it in for now
          
          (when (and (listp value) 
                     (eq (first value) 'quote))
            (setf value (second value)))  
          
          (push-last
           (case parameter
             (:name
              (print-warning "PARAMETER NAME CANNOT BE SET.")
              :error)
             (:utility
              (print-warning "PARAMETER UTILITY CANNOT BE SET.")
              :error)
             
             
             (:u
              (set-parameter production-u :u
                             (numberp value) "a number"))
             
             (:at
              (setf value (safe-seconds->ms value 'spp))
              (let ((res (set-parameter production-at :at
                                        (nonneg value)
                                        "a positive number")))
                (if (numberp res)
                    (ms->seconds res)
                  res)))
             
             (:reward ;; can't use set-parameter because I need to test
                      ;; the original value before setting it but the
                      ;; housekeeping only occurs after
              (if ul
                  (cond ((or (null value) (eq value t) (numberp value))
                         (unless (zerop (mp-time-ms))
                           (let ((old (production-reward p)))
                             (unless (eq value old)
                               (setf (production-reward-changed p) t))))
                         
                         (setf (production-reward p) value)
                         value)
                        (t
                         (print-warning
                          "PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~:@(~A~)."
                          :reward value "a number, t, or nil")
                         :error))
                (print-warning "PARAMETER REWARD CAN ONLY BE SET WHEN UL IS T.")))
             
             (:fixed-utility
              (set-parameter production-fixed-utility :fixed-utility
                             (or (eq value t) (null value))
                             "T or nil"))
             (t
              (print-warning
               "NO PARAMETER ~A DEFINED FOR PRODUCTIONS." parameter)
              :error))
           values)))
      :error)))


(defmacro spp (&rest production-parameters)
  "Inspects and sets production parameters."
  `(spp-fct ',production-parameters))


(defun spp-fct (parameters)
  "Inspects and sets production parameters."
  (let ((results nil))  
    (if (null parameters) ; print all parameters for all productions
        (dolist (production (all-productions))
          (push-last (production-parameter-fct production) results))
      (dolist (description (if (or (keywordp (first parameters))
                                   (keywordp (second parameters))
                                   (and (listp (first parameters))
                                        (null (second parameters))
                                        (not (keywordp 
                                              (second 
                                               (first parameters))))))
                               (list parameters) parameters))
        (when (atom description) (setf description (list description)))
        (if (keywordp (first description))
            (dolist (production (all-productions))
              (push-last
               (if (and (cdr description) 
                        (not (keywordp (second description))))
                   (parameters-fct production description)
                 (production-parameter-fct production description))
               results))
          
          (dolist (production (if (atom (first description))
                                  (list (first description))
                                (first description)))
            (if (get-production production)
                (push-last
                 (if (and (cddr description) 
                          (not (keywordp (third description))))
                     (parameters-fct production (rest description))
                   (production-parameter-fct production (rest description)))
                 results)
              (progn
                (model-warning "Spp cannot adjust parameters because production ~S does not exist" production)
                (push-last :error results)))))))
    results))

(defun spp-remote (&rest params)
  (encode-string-names (spp-fct (decode-string-names params))))

(add-act-r-command "spp" 'spp-remote "Set or get production parameters. Params: see ACT-R manual." t)

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