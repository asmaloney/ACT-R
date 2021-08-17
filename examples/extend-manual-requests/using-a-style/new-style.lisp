;;; This example will create a new manual request called new-action
;;; which has two features named arg1 and arg2.  All it is going
;;; to do is print out the arguments provided and timing information
;;; when a request is made .2 seconds after the movement is prepared
;;; and it will spend a total of .3 seconds in the execution stage.

;;; This file creates the new action for the motor module using
;;; extend-manual-requests based on the style mechanism built
;;; into the motor module.  It's not necessary for one to use
;;; a style with extend-manual-requests, but doing so eliminates
;;; the need to deal directly with the internal state management
;;; and scheduling of the typical motor module action stages
;;; (preparation, initiation, and execution).

;;; The adding of a new motor module action only needs to be
;;; performed once (not with each model), and a file like this
;;; could be placed into the user-loads directory to have it
;;; loaded automatically with the system.

;;; A style requires a name and a set of features.  With the
;;; normal style handling mechanisms each request for an action
;;; that is implemented as a style will require all of the features
;;; to be set to perform the action.

;;; Internally, a style is a CLOS class and there are several 
;;; methods which can be defined for a style's class which 
;;; control how it operates.  Many of the default style
;;; class (movement-style) methods should not be overridden,
;;; but a few are necessary to provide the details for the
;;; new style.  The example below will show those that are
;;; necessary for configuring the new style and also some of 
;;; the accessors for the movement-style slots which are set
;;; by the default methods which may be useful.

;;; To create a style one calls defstyle.  It has two required
;;; parameters: a name for the new style and a list of any
;;; other styles which this style will inherit from.  It can take
;;; any number of additional parameters to name the features
;;; of the style.  This results in creating a new CLOS class
;;; which is a subclass of the movement-style class and any
;;; other classes provided.  The features provided are slots
;;; in that new class and have accessor functions which are the
;;; names of the features.  

;;; This creates the new-action style with features arg1 and arg2.

(defstyle new-action () arg1 arg2)

;;; Now, we need to define some methods on the motor-module and
;;; new-action class to control how our new action will perform.
;;; For this example we will not be accessing the motor module
;;; itself because it's implementation is not part of the ACT-R 
;;; API.  Doing so could be useful to access the parameters in 
;;; the motor module, but since that's not part of the API one 
;;; will have to look at the implementation of the motor module 
;;; to find the details and there are no guarantees that the
;;; motor module's internal details will not change in the future.

;;; If the style will be moving the hand, finger(s), or depends 
;;; upon the current position of the hand to generate the features,
;;; then the prepare-features method should be written for the style.
;;; That method should compute the necessary features based on
;;; the expected hand position (not the current position since
;;; it may be in the process of moving), and set the updated-pos
;;; slot of the movement-style to the position where the hand
;;; will be when it finishes execution if it will change.

;;; This example does not require that, but as an example here
;;; is the method for the peck style movement which moves a finger.
;;; The move-a-finger method of the motor module returns the new
;;; position after a finger is moved, and that is being computed 
;;; based on the expected position since the :current nil flag is
;;; provided.  If that was not provided, or provided as non-nil, 
;;; then the current hand position would have been used.

#|
(defmethod prepare-features ((mtr-mod motor-module) (self peck))
  (setf (updated-pos self)
    (move-a-finger mtr-mod (hand self) (finger self) (r self) (theta self) :current nil)))
|#


;;; The preparation cost for a style action will require the
;;; time to prepare n+1 features where n is the number of features
;;; specified for the style, and the +1 is for the style itself.

;;; When the style action is repeated the feat-differences 
;;; method is called to determine how many features can be
;;; shared between the two.  It must return a number indicating
;;; how many features to prepare. 

;;; After calling feat-differences the fprep-time slot of the 
;;; style will be set to the cost of the preparation in seconds.

(defmethod feat-differences ((f1 new-action) (f2 new-action))
  (let ((count 0))
    (unless (equal (arg1 f1) (arg1 f2))
      (incf count))
    (unless (equal (arg2 f1) (arg2 f2))
      (incf count))
    count))

;;; The compute-exec-time method will be called to determine
;;; how long after preparation of the style's action it will take
;;; until the action should be 'executed'.  It is called every
;;; time there is a request for the action and thus can change
;;; dynamically based on the features.  It must return a number
;;; which is the execution time delay in seconds.

;;; After calling compute-exec-time the style will set the 
;;; exec-time slot of the style to the result.


(defmethod compute-exec-time ((m motor-module) (self new-action))
  ;; We're just setting a fixed cost of .2 seconds
  .2)

;;; The compute-finish-time method is called to determine how long
;;; after the preparation of the style's action it will take until
;;; the execution stage should complete and become free again.
;;; It must return a number which is the total execution stage
;;; time in seconds.  This is called after the exec-time is set.

;;; After calling compute-finish-time the style will set the 
;;; finish-time slot of the style to the result.

(defmethod compute-finish-time ((m motor-module) (self new-action))
  ;; .1 second longer than the execution time
  (+ .1 (exec-time self)))

;;; Once preparation completes for the action the queue-output-events
;;; method is called which should schedule the actions to perform.  
;;; The expected time for the actions is the already computed exec-time,
;;; but that is not required.

(defmethod queue-output-events ((m motor-module) (self new-action))
  
  ;; Here we are just scheduling an event to print out the details
  ;; of the request at the execution time.
  
  (schedule-event-relative (exec-time self) 
                           'demo-print-style-info
                           :params (list (arg1 self) (arg2 self) (fprep-time self) (finish-time self)))
  
  ;; If the action were moving the hand then it should schedule that
  ;; here using the set-hand-position action with the value that was
  ;; stored in updated-pos by the prepare-features method.  Again, this
  ;; example does not need to do so, but this example is from the peck 
  ;; style movement:
  
  #|
  (schedule-event-relative (seconds->ms (exec-time self)) 'set-hand-position :time-in-ms t :module :motor 
                           :output nil :destination :motor 
                           :params (list (hand self) (updated-pos self)))
  |#
  )

;;; The function that will be called:

(defun demo-print-style-info (w x y z) 
  (format t "Features are ~S and ~S~%~s seconds spent in preparation~%finishing after ~f seconds~%" w x y z))

;;; To add the new style use extend-manual-requests and provide the 
;;; style name and features as the description of the action and
;;; use the built-in function handle-style-request to process the
;;; requests.

(extend-manual-requests (new-action arg1 arg2) handle-style-request)

