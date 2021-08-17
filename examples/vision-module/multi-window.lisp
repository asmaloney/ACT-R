;;; This example shows that multiple devices can be installed to
;;; the vision interface at the same time.  It also shows that the
;;; visicon features created by each exp-window installed are 
;;; available simultaneously as well as those from the mouse
;;; cursor.  Finally, it shows that the coordinates for all of
;;; the features of the exp-window device and the mouse cursor
;;; are represented in a global coordinate system and not in
;;; the local coordinates of the windows themselves.

(defun run-example ()
  (reset)
  
  ;; The first window is located at x=0 and y=0 which
  ;; is fine for virtual windows, but if it were a real
  ;; window that window would be behind the menu bar
  ;; at the top of the display under OS X which makes
  ;; it difficult to interact with.  The second window
  ;; is located at x=200, y=200.  The default mouse 
  ;; position is x=0, y=0.
  
  (let ((w1 (open-exp-window "W1" :visible nil :height 100 :width 100 :x 0 :y 0))
        (w2 (open-exp-window "W2" :visible nil :height 100 :width 100 :x 200 :y 200)))
    
    ;; add text to the same local position in each window.
    
    (add-text-to-exp-window w1 "a" :x 10 :y 10 :color 'red)
    (add-text-to-exp-window w2 "a" :x 10 :y 10 :color 'blue)
    
    ;; Install both windows and the mouse cursor 
    
    (install-device w1)
    (install-device w2)
    (install-device '("vision" "cursor" "mouse"))
    
    ;; Just run the model to have vision module process
    ;; things and print the visicon.
    ;; The model doesn't do anything.
    
    (run 1)
    (print-visicon)))

;; Load a model which doesn't do anything.

(load "ACT-R:examples;vision-module;multi-window-model.lisp")