
(clear-all)

;;; The model is very simple in that it just repeatedly finds
;;; a location and then attends to the item there printing
;;; out the chunks in the visual-location and visual buffers
;;; after the corresponding requet completes relying on finsts
;;; markers to stop after attending all of them.
;;;
;;;

(define-model new-visual-feature-slots
    
  (sgp :v t :trace-detail low)
  
  (chunk-type real-space x y z size)
  (chunk-type other-space pos-1 pos-2 pos-3 size value)
  
  (p start
     ?goal>
      buffer empty
     ==>
     +goal>
     +visual-location>
       :attended nil)
  
  (p attend 
     =visual-location>
     ?visual-location>
       buffer requested
     ?visual>
       state free
   ==>
     +visual>
       isa move-attention
       screen-pos =visual-location
     !eval! (pprint-chunks-fct (list =visual-location)))
  
  (p harvest
     =visual>
   ==>
     +visual-location>
       :attended nil
     !eval! (pprint-chunks-fct (list =visual))))