;;; This example shows a combination of the save-model and
;;; parallel-models extras.  It runs a model to generate a
;;; few chunks in declarative memory and then save the current
;;; state to a file.  That file can then either be loaded and
;;; run sequentially or multiple times in parallel returning the
;;; value of the next retrieved chunk using the functions 
;;; run-m-guesses-total or run-m-guesses-total-n-at-a-time
;;; respectively.


(clear-all)

(require-extra "save-model")
(require-extra "parallel-models")

(define-model create-and-guess
    (sgp :esc t :er t :bll .5 :ans .5 :v nil :rt -10)
  
  (chunk-type goal state)
 
  (p start
     ?goal>
       buffer empty
     ?imaginal>
       state free
       buffer empty
     ==>
     +imaginal>
       value 1)
  
  (p generate-next
     =imaginal>
       value =v
      < value 5
     ?imaginal>
       state free
     ==>
     !bind! =n (1+ =v)
     +imaginal>
       value =n)
  
  (p stop
     =imaginal>
       value 5
     ==>
     +goal>
       state nil)
  
  (p guess
     =goal>
       state guess
     ?retrieval>
       state free
       buffer empty
     ==>
     +retrieval>))


(defun initial-run-and-save ()
  
  (load-act-r-model "ACT-R:extras;parallel-models;example.lisp")
  (run 10)
  
  ;; Save the model to a file along with a function to run it further.
  
  (with-open-file (f "ACT-R:extras;parallel-models;guess-model.lisp" :direction :output :if-exists :supersede :if-does-not-exist :create)
    
    (save-chunks-and-productions f)
    
    ;; Write out the function that'll run the model multiple times and return the result of
    ;; the value slot from the retrieval buffer on each run.
    
    (format f "(defun make-guesses (i)~%")
    (format f "  (let (result)~%")
    (format f "    (dotimes (n i)~%")
    (format f "      (reset)~%")
    (format f "      (sgp :v nil)~%")
    (format f "      (mod-focus state guess)~%")
    (format f "      (run-until-condition (lambda (x)~%")
    (format f "                             (declare (ignore x))~%")
    (format f "                             (or (buffer-read 'retrieval)~%")
    (format f "                                 (query-buffer 'retrieval '(buffer failure)))))~%")
    (format f "      (push (chunk-slot-value-fct (buffer-read 'retrieval) 'value) result))~%")
    (format f "    result))")))

(defun run-m-guesses-total (m)
  
  (initial-run-and-save)
  
  ;; load the saved model file
  
  (load-act-r-model "ACT-R:extras;parallel-models;guess-model.lisp" t)
  
  (make-guesses 1000))  

(defun run-m-guesses-total-n-at-a-time (m n)

  (initial-run-and-save)
  
  ;; create the parallel ACT-R threads
  
  (create-parallel-instances n)
  
  ;; load the model file into each instance
  
  (instantiate-parallel-models "ACT-R:extras;parallel-models;guess-model.lisp")
  
  ;; Have each instance run for no more than 10 seconds and
  ;; run one nth of the total guesses combining all the 
  ;; returned lists into a single list.
  
  (flatten (run-parallel-models "make-guesses" 10 (list (round m n)))))
