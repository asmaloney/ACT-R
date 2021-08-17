
(clear-all)

;;; This model relies on buffer stuffing to detect when
;;; the visual scene changes (the only constraint on the
;;; stuffed chunk is screen-x lowest) and then attends to
;;; an items and prints the location and object chunks.
;;;
;;; The items it attends are prespecified to be the ones
;;; that are changed by the task code for demonstration.

(define-model adjust-visual-features
    
    (sgp :v t)
  
  ;; chunk-types for the visual features 
  (chunk-type (polygon-feature (:include visual-location)) regular)
  (chunk-type (polygon (:include visual-object)) sides)
  
  ;; simple goal buffer chunk to step through the example
  (chunk-type goal step)
  
  ;; Do this to avoid the warnings when the chunks are created
  (define-chunks true false polygon)
  
  ;; stuff the leftmost item
  (set-visloc-default screen-x lowest)
  
  (p start
     ?goal>
       buffer empty
     =visual-location>
       screen-x =x
     ?visual>
       state free
     ==>
     +visual-location>
       > screen-x =x
     +visual>
       isa move-attention
       screen-pos =visual-location
     !output! (First location chunk)
     !eval! (buffer-chunk visual-location))

  (p attend-second
     ?goal>
       buffer empty
     =visual-location>
     =visual>
     ?visual>
       state free
     ==>
     +visual>
     isa move-attention
     screen-pos =visual-location
     !output! (First object chunk)
     !eval! (buffer-chunk visual)
     !output! (Second location chunk)
     !eval! (buffer-chunk visual-location))
  
  (p show-second
     ?goal>
     buffer empty
     
     =visual>
     ?visual-location>
     buffer empty
     ==>
     !output! (Second object chunk)
     !eval! (buffer-chunk visual)
     +goal>)
  

  (p first-change
     =goal>
       step nil
     ?visual-location>
       buffer unrequested
     ==>
     +visual-location>
       color green
     =goal>
       step 1)
  
  (p second-item
     =goal>
       step 1
     ?visual-location>
       buffer unrequested
     ==>
     +visual-location>
       color red
     =goal>
       step 2)
  
  (p third-item
     =goal>
       step 2
     ?visual-location>
       buffer unrequested
     ==>
     +visual-location>
       color green
     =goal>
       step 3)
  
  (p fourth-item
     =goal>
       step 3
     ?visual-location>
       buffer unrequested
     ==>
     +visual-location>
       color green
     =goal>
       step 4)
  
  
  (p attend
     =goal>
     =visual-location>
     ?visual-location>
       buffer requested
     ?visual>
       state free
     ==>
     +visual>
     isa move-attention
     screen-pos =visual-location
     !output! (Current location chunk)
     !eval! (buffer-chunk visual-location))
  
  (p report-attended
     =goal>
     =visual>
     ==>
     !output! (Current object chunk)
     !eval! (buffer-chunk visual))
  
  )

  
  
