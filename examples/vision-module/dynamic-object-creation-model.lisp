
(clear-all)

;;; This find visual items and attends to them from left to right
;;; printing out the location and object chunks.

(define-model dynamic-visual-objects
    
    (sgp :v t)
  
  ;; chunk-types for the special object chunk
  
  (chunk-type (custom-object (:include visual-object)) time)
  
  
  
  (p attend
     =visual-location>
     ?visual>
     state free
     ==>
     +visual>
       isa move-attention
     screen-pos =visual-location
          !output! (location chunk)
     !eval! (buffer-chunk visual-location))

  (p harvest-and-find-next
     =visual>
     ==>
     +visual-location>
     >= screen-x current
     :attended nil
     !output! (object chunk)
     !eval! (buffer-chunk visual)))

  
  
