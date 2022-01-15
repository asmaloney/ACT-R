;;; This module creates 2 buffers called create and output which
;;; take simple requests to create a new chunk or print some
;;; information in the trace.  For demonstration of how and
;;; when the module interface commands are called they are all
;;; traced before running the simple demo model as shown in
;;; the comments at the bottom.

;;; This code is not thread safe, but if this module is to be used when there
;;; are any remote systems connected (for example the ACT-R Environment or
;;; a remote connection like the one provided for Python with the ACT-R tutorial)
;;; then it is not safe to use this module if those systems may access its 
;;; state or make requests concurrently with a model running e.g. using the
;;; buffer status tool in the Environment or the get-parameter-value remote
;;; command.  To protect that, all the code which sets or reads the slot 
;;; contents of the module structure would need to be protected appropriately,
;;; and a simple lock around the code which accesses the demo-module's 
;;; slots should be sufficient.

(defstruct demo-module delay esc busy)

(defun create-demo-module (model-name)
  (declare (ignore model-name))
  (make-demo-module))

(defun reset-demo-module (demo)
  (declare (ignore demo))
  (chunk-type demo-output value (demo-output t)))

(defun delete-demo-module (demo)
  (declare (ignore demo)))

(defun demo-module-params (demo param)
  (if (consp param)
    (case (car param)
      (:create-delay
       (setf (demo-module-delay demo) (cdr param)))  
      (:esc
       (setf (demo-module-esc demo) (cdr param))))
    (case param
      (:create-delay
       (demo-module-delay demo)))))

(defun demo-module-requests (demo buffer spec)
  (if (eq buffer 'create)
     (demo-create-chunk demo spec)
    (demo-handle-print-out spec)))


(defun demo-handle-print-out (spec)
  (let ((output-slot? (chunk-spec-slot-spec spec 'demo-output))
        (v1 (chunk-spec-slot-spec spec 'value)))
     (if output-slot?
         (if v1
             (if (= (length v1) 1)
                 (if (eq (caar v1) '=)
                     (model-output "Value: ~s" (caddar v1))
                   (model-warning "Invalid slot modifier ~s in output buffer request" (caar v1)))
               (model-warning "Value slot specified multiple times in output buffer request"))
           (model-warning "Value slot missing in output buffer request"))
       (model-warning "demo-output slot missing in request to output buffer"))))

(defun demo-create-chunk (demo spec)
   (if (demo-module-busy demo)
      (model-warning "Cannot handle request when busy")
     (let ((chunk-def (chunk-spec-to-chunk-def spec)))
        (when chunk-def
          (let ((delay (if (demo-module-esc demo) 
                           (demo-module-delay demo)
                         0)))
            (setf (demo-module-busy demo) t)
            (schedule-set-buffer-chunk 'create spec delay :module 'demo)
            (schedule-event-relative delay 'free-demo-module :params (list demo) :module 'demo))))))

(defun free-demo-module (demo)
  (setf (demo-module-busy demo) nil))


(defun demo-module-queries (demo buffer query value)
  (declare (ignore buffer))
  (case query
    (state
     (case value
       (busy (demo-module-busy demo))
       (free (not (demo-module-busy demo)))
       (error nil)
       (t (print-warning "Bad state query to the ~s buffer" buffer))))
    (t (print-warning "Invalid query ~s to the ~s buffer" query buffer))))





(define-module-fct 'demo
                   '(create output) 
  (list (define-parameter :create-delay 
               :documentation 
                  "time to create the chunk for the demo module"
               :default-value .1
               :valid-test 'nonneg
               :warning "Non-negative number"
               :owner t)
        (define-parameter :esc :owner nil))

   :request 'demo-module-requests
   :query 'demo-module-queries

   :version "1.0a1"
   :documentation "Demo module for ICCM tutorial"
   :creation 'create-demo-module
   :reset 'reset-demo-module 
   :delete 'delete-demo-module
   :params 'demo-module-params
)


#|

> (trace demo-module-requests demo-module-queries create-demo-module reset-demo-module delete-demo-module demo-module-params)
(DEMO-MODULE-PARAMS DELETE-DEMO-MODULE RESET-DEMO-MODULE CREATE-DEMO-MODULE DEMO-MODULE-QUERIES
                    DEMO-MODULE-REQUESTS)
> (reset)
 0[5]: (RESET-DEMO-MODULE #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL))
 0[5]: returned DEMO-OUTPUT
 0[5]: (DEMO-MODULE-PARAMS #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) (:CREATE-DELAY . 0.1))
 0[5]: returned 0.1
 0[5]: (DEMO-MODULE-PARAMS #S(DEMO-MODULE :DELAY 0.1 :ESC T :BUSY NIL) (:ESC))
 0[5]: returned NIL
 0[5]: (DEMO-MODULE-PARAMS #S(DEMO-MODULE :DELAY 0.1 :ESC NIL :BUSY NIL) (:ESC . T))
 0[5]: returned T
 0[5]: (DEMO-MODULE-PARAMS #S(DEMO-MODULE :DELAY 0.1 :ESC T :BUSY NIL) (:CREATE-DELAY . 0.15))
 0[5]: returned 0.15
#|Warning: Production P2 has a condition for buffer CREATE with an isa that provides no tests. |#
T
> (run 1)
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
 0[5]: (DEMO-MODULE-QUERIES #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) CREATE STATE FREE)
 0[5]: returned T
     0.000   PROCEDURAL             PRODUCTION-SELECTED P1
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION GOAL
     0.000   PROCEDURAL             QUERY-BUFFER-ACTION CREATE
     0.050   PROCEDURAL             PRODUCTION-FIRED P1
     0.050   PROCEDURAL             MODULE-REQUEST GOAL
     0.050   PROCEDURAL             MODULE-REQUEST CREATE
 0[5]: (DEMO-MODULE-REQUESTS #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) CREATE #S(ACT-R-CHUNK-SPEC :FILLED-SLOTS 422212465065984 :EMPTY-SLOTS 0 :REQUEST-PARAM-SLOTS 0 :DUPLICATE-SLOTS 0 :EQUAL-SLOTS 422212465065984 :NEGATED-SLOTS 0 :RELATIVE-SLOTS 0 :VARIABLES NIL :SLOT-VARS NIL :DEPENDENCIES NIL :SLOTS (#2=#S(ACT-R-SLOT-SPEC :MODIFIER = :NAME SCREEN-X :VALUE 10 :VARIABLE NIL) #1=#S(ACT-R-SLOT-SPEC :MODIFIER = :NAME SCREEN-Y :VALUE 20 :VARIABLE NIL)) :TESTABLE-SLOTS (#1# #2#) ...))
 0[5]: returned 15
     0.050   PROCEDURAL             CLEAR-BUFFER GOAL
     0.050   PROCEDURAL             CLEAR-BUFFER CREATE
     0.050   GOAL                   SET-BUFFER-CHUNK-FROM-SPEC GOAL 
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.200   DEMO                   SET-BUFFER-CHUNK-FROM-SPEC CREATE 
     0.200   DEMO                   FREE-DEMO-MODULE #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY T)
     0.200   PROCEDURAL             CONFLICT-RESOLUTION
 0[5]: (DEMO-MODULE-QUERIES #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) OUTPUT STATE FREE)
 0[5]: returned T
     0.200   PROCEDURAL             PRODUCTION-SELECTED P2
     0.200   PROCEDURAL             BUFFER-READ-ACTION CREATE
     0.200   PROCEDURAL             QUERY-BUFFER-ACTION OUTPUT
     0.250   PROCEDURAL             PRODUCTION-FIRED P2
     0.250   PROCEDURAL             MODULE-REQUEST OUTPUT
 0[5]: (DEMO-MODULE-REQUESTS #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL) OUTPUT #S(ACT-R-CHUNK-SPEC :FILLED-SLOTS 604462909807348947091456 :EMPTY-SLOTS 0 :REQUEST-PARAM-SLOTS 0 :DUPLICATE-SLOTS 0 :EQUAL-SLOTS 604462909807348947091456 :NEGATED-SLOTS 0 :RELATIVE-SLOTS 0 :VARIABLES NIL :SLOT-VARS NIL :DEPENDENCIES NIL :SLOTS (#2=#S(ACT-R-SLOT-SPEC :MODIFIER = :NAME VALUE :VALUE CHUNK0 :VARIABLE NIL) #1=#S(ACT-R-SLOT-SPEC :MODIFIER = :NAME DEMO-OUTPUT :VALUE T :VARIABLE NIL)) :TESTABLE-SLOTS (#1# #2#) ...))
Value: CHUNK0
 0[5]: returned NIL
     0.250   PROCEDURAL             CLEAR-BUFFER CREATE
     0.250   PROCEDURAL             CLEAR-BUFFER OUTPUT
     0.250   PROCEDURAL             CONFLICT-RESOLUTION
     0.250   ------                 Stopped because no events left to process
0.25
25
NIL
> (clear-all)
 0[5]: (DELETE-DEMO-MODULE #S(DEMO-MODULE :DELAY 0.15 :ESC T :BUSY NIL))
 0[5]: returned NIL
NIL

|#
