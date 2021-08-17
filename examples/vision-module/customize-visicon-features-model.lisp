
(clear-all)

;;; The model is very simple in that it just repeatedly finds
;;; a location and then attends to the item there printing
;;; out the chunks in the visual-location and visual buffers
;;; after the corresponding requet completes.
;;;
;;; The visual-location requests are specified such that 
;;; there will only be one visual-location which matches
;;; the request.  
;;;
;;; The productions which make the visual-location requests
;;; describe the new features being used to perform the 
;;; request.

(define-model custom-visual-features
    
    (sgp :v t :trace-detail high :needs-mouse t)
  
  (install-device '("motor" "cursor" "mouse"))
  
  (start-hand-at-mouse)
  
  (chunk-type custom-location x y z height width size)
  
  (chunk-type goal step)
  
  ;; starting location
  
  (define-chunks (start-pos screen-x 100 screen-y 0))
  
  
  (p start
     ?goal>
       buffer empty
     ?manual>
       state free
     ==>
     +goal>
     +visual-location>
       < screen-x 100
     +manual>
       isa move-cursor
       loc start-pos)
  
  (p move-to-first-loc
     =goal>
       step nil
     =visual-location>
     ?manual>
       state free
     ==>
     !output! (first location chunk)
     !eval! (buffer-chunk visual-location)
     
     =goal>
       step 1
     +manual>
       isa move-cursor
       loc =visual-location)
  
  (p return-to-start
     =goal>
       step 1
     ?manual>
       state free
     ==>
     +visual-location>
       > x 100
     +manual>
       isa move-cursor
       loc start-pos
     =goal>
       step 2)
  
  (p move-to-second-loc
     =goal>
       step 2
     =visual-location>
     ?manual>
       state free
     ==>
     !output! (second location chunk)
     !eval! (buffer-chunk visual-location)
     
     =goal>
       step 3
     +manual>
       isa move-cursor
       loc =visual-location))