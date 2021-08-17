;;; This model simply uses the buffers create and output
;;; from the demo module to show their operation.


(clear-all)
(define-model test-demo-module
    
    (sgp :esc t :create-delay .15 :trace-detail high)
    
    (p p1
       ?goal>
       buffer empty
       ?create>
       state free
       buffer empty
       ==>
       +goal>
       +create>
       isa visual-location
       screen-x 10
       screen-y 20)
  
  (p p2
     =create>
     isa visual-location
     ==>
     +output>
     isa demo-output
     value =create
     ))
