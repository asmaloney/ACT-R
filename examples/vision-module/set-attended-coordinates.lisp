;;; This example demonstrates how one can set the initial position
;;; of visual attention.


(defun create-features ()
  ;; just put two locations out there
  ;; one close to 0,0 the default initial
  ;; position and one close to 50,50 which
  ;; is set as the starting point below.
  
  (add-visicon-features '(screen-x 0 screen-y 5 distance 1080)
                        '(screen-x 60 screen-y 55 distance 1080))
  (run-n-events 3)
  (print-visicon))


(defun run-example ()
  
  ;; with the default initial position
  (reset)
  (create-features)
  (run 10)
  
  ;; set the position
  (reset)
  (create-features)
  (attend-visual-coordinates 50 50)
  (run 10))

  
  

;;; The model is very simple in that it just finds the
;;; location closest to current and prints out the result.
;;;

(load-act-r-model "ACT-R:examples;vision-module;set-attended-coordinates-model.lisp")

