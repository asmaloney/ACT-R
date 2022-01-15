; ACT-R tutorial unit 5 grouped task.
; This is a simple example task to show partial matching.
; It simply runs the model and records values that the
; model provides and returns the list of provided values
; in the order provided after the run.

; Load the corresponding model for the task.

(load-act-r-model "ACT-R:tutorial;unit5;grouped-model.lisp")

; Create a varaiable to hold the values from the model

(defvar *response* nil)

; The recall function creates a command for
; the record-response function, clears the response
; list, runs the model, and returns the response list
; in the order provided by the model.

(defun grouped-recall ()
  (add-act-r-command "grouped-response" 'record-response "Response recording function for the tutorial grouped model.")
  (setf *response* nil)
  (reset)
  (run 20)
  (remove-act-r-command "grouped-response")
  (reverse *response*))

; Store a value provided by the model on the response list

(defun record-response (value)
  (push value *response*))
