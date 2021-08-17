
(clear-all)

;;; The model is very simple in that it just makes a
;;; request to find the feature nearest to current and
;;; then prints the result.

(define-model set-attended-coordinates-model
    
  (sgp :v t :trace-detail low)
  
  
  (p start
     ?goal>
      buffer empty
     ==>
     +goal>
     +visual-location>
       :nearest current)
  
  (p found
     =visual-location>
     ?visual-location>
       buffer requested
   ==>
     !eval! (pprint-chunks-fct (list =visual-location))))