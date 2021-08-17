
(load-act-r-model "ACT-R:tutorial;unit5;grouped-model.lisp")

(defvar *response* nil)

(defun grouped-recall ()
  (add-act-r-command "grouped-response" 'record-response "Response recording function for the tutorial grouped model.")
  (setf *response* nil)
  (reset)
  (run 20)
  (remove-act-r-command "grouped-response")
  (reverse *response*))

(defun record-response (value)
  (push value *response*))
