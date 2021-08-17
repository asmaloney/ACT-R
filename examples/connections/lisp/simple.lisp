;; Load quicklisp and the socket and JSON libraries used.

#-:QUICKLISP (eval-when (:compile-toplevel :load-toplevel :execute)
               (when (probe-file "~/quicklisp/setup.lisp")
                 (load "~/quicklisp/setup.lisp"))
               (unless (find :quicklisp *features*)
                 (error "This example requires Quicklisp to load the components necessary for the remote connection.")))

(ql:quickload :usocket)
(ql:quickload :cl-json)

;; a function to send a string to ACT-R followed by the 
;; end of message character, and make sure the stream 
;; sends the output.

(defun send-actr-message (stream string)
  (format stream "~a~c" string (code-char 4))
  (finish-output stream))


;; Read from the stream until the end of message character
;; is received and then return the decoded JSON result.

(defun read-actr-message (stream)
  (let ((s (make-array 40
                       :element-type 'character
                       :adjustable T
                       :fill-pointer 0)))
    (loop
      (let ((char (read-char stream nil :done)))
        (cond ((eq char :done)
               (format *error-output* "External connection closed.")
               (return-from read-actr-message nil))
              ((eq char (code-char 4))
               (return-from read-actr-message (json:decode-json-from-string s)))
              (t (vector-push-extend char s)))))))


;; Read the ACT-R configuration files for the connection information

(defvar *address* (with-open-file (f (translate-logical-pathname "~/act-r-address.txt") :direction :input)
                    (read-line f)))
(defvar *port* (with-open-file (f (translate-logical-pathname "~/act-r-port-num.txt") :direction :input)
                    (read f)))

;; Open the socket

(defvar *socket* (usocket:socket-connect *address* *port* :nodelay :if-supported))

;; Create a variable with the stream which can be read/written for that socket.

(defvar *stream* (usocket:socket-stream *socket*))


;; Set the name for this ACT-R connection

(send-actr-message *stream* "{\"method\":\"set-name\",\"params\":[\"Remote Lisp Example\"],\"id\":null}")

;; Evaluate the "act-r-version" command 

(send-actr-message *stream* "{\"method\":\"evaluate\",\"params\":[\"act-r-version\"],\"id\":1}")

;; Read the response and check it for validity.
;; Note, cl-json converts a JSON object into an alist with
;; the car of an item being a keyword that matches the 
;; name of an item and the cdr is the value of that
;; name.

(let ((message (read-actr-message *stream*)))
  (cond ((and (= (length message) 3)
              (assoc :result message)
              (assoc :error message)
              (assoc :id message))
         (let ((id (cdr (assoc :id message)))
               (result (cdr (assoc :result message)))
               (error (cdr (assoc :error message))))
           (cond ((= id 1)
                  (if result
                      (format t "ACT-R version is ~s~%" (first result))
                    (format t "Error: ~s~%" (cdr (assoc :message error)))))
                 (t 
                  (format t "Unexpected id on return result: ~s~%" id)))))
        (t
         (format t "Unexpected message from ACT-R: ~s~%" message))))


;; Add the "remote-count" command to ACT-R using "count" as the local name.

(send-actr-message *stream* "{\"method\":\"add\",\"params\":[\"remote-count\",\"count\",\"Count the number of items in a list. Params: list\"],\"id\":null}")

;; Loop forever reading a message from ACT-R and returning
;; the count of items in the provided list or an error message
;; if things are not correct.

(loop
  (let ((message (read-actr-message *stream*)))
    
    (cond ((and (= (length message) 3)
                (assoc :method message)
                (assoc :params message)
                (assoc :id message)
                (string-equal (cdr (assoc :method message)) "evaluate"))
           
           (let ((params (cdr (assoc :params message)))
                 (id (cdr (assoc :id message)))
                 (result nil)
                 (error nil))
             (cond ((and (= (length params) 3)
                         (string= "count" (first params)))
                    (if (listp (third params))
                        (handler-case
                            (setf result (length (third params)))
                          ((or error condition) (x)
                           (setf error (format nil "Error: ~s" x))))
                      (setf error (format nil "Requires a list of parameters but given ~s" (third params)))))
                   
                   ((not (string= "count" (first params)))
                    (setf error "Only evaluates count command."))
                   
                   (t
                    (setf error (format nil "Invalid parameters to count: ~s" params))))
             
             (when id
               (if error
                   (send-actr-message *stream* (format nil "{\"result\":null,\"error\":{\"message\": ~s},\"id\":~a}" error (json:encode-json-to-string id)))
                 (send-actr-message *stream* (format nil "{\"result\":[~d],\"error\":null,\"id\":~a}" result (json:encode-json-to-string id)))))))
          
          (t (format t "Unexpected message from ACT-R: ~s~%" message)))))