;;; This example shows how one can specify a command to dynamically 
;;; create the visual object chunks for features that are added to 
;;; the visicon instead of specifying them when they are added.
;;;
;;; There is a device called object-creator which can be installed
;;; for the vision interface with the details of the device being
;;; the command to use.  When the model shifts attention to an
;;; item in the visicon if an object-creator device is installed
;;; then the command associated with that device will be passed
;;; the visicon entry id for the item being attended (the value
;;; which was returned when the item was added to the visicon).
;;; If the command returns a chunk it will be the one placed into
;;; the visual buffer as a result of the attention shift.  If
;;; it returns any other value then the default mechanism will be
;;; used to create the visual-object chunk, and if that other value
;;; is not nil then a warning will be printed to indicate that an
;;; invalid value was returned.

(defvar *ids* nil)

(defun custom-object-chunk (id)
  (when (equalp id (first *ids*))
    (first (define-chunks-fct (list (list 'isa 'custom-object 'color 'red 'time (mp-time-ms)))))))

(defun run-example ()
  (reset)
  
  ;; Here we install our custom function using an object-creator device
  (install-device '("vision" "object-creator" custom-object-chunk))
  
  ;; Now we add two features to the display, the first of which will
  ;; have the object created by the custom function and the other by
  ;; the default mechanism.
  
  (setf *ids* (add-visicon-features '(screen-x 10 screen-y 20)
                                    '(screen-x 100 screen-y 20)))
  
  ;; run to give vision module a chance to
  ;; process those features and then print the
  ;; visicon.
  
  (run-n-events 3)
  (print-visicon)
  
  ;; run to have model complete task
  (run 1))

;; The model just attends to items and prints out the location and
;; object chunks which it gets.

(load-act-r-model "ACT-R:examples;vision-module;dynamic-object-creation-model.lisp")
