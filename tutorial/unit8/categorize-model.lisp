(clear-all)

(define-model categorize
  (sgp :v t :act t)
  (sgp :esc t :lf .01 :blc 10 :rt -100 :do-not-harvest imaginal)
  (sgp :sim-hook "size-similarities" :cache-sim-hook-results t)
  (sgp :mp 1 :ppm 1 :egs .25 :ans .25)
  
  (chunk-type goal state name value)
  (chunk-type example category)
  
  (add-dm (small isa chunk)
          (medium isa chunk)
          (large isa chunk)
          (unknown isa chunk))
  
  (define-chunks (categorize) (add-attribute))
  
  (call-act-r-command "create-example-memories")
  
  (set-similarities (small medium -.4)
                    (medium large -.4)
                    (small large -.8))
  
    
  (set-buffer-chunk 'imaginal '(category unknown))
  
  ;; Declare the goal buffer chunk-type usage which is going to be set from
  ;; outside of the model.
  
  (declare-buffer-usage goal goal :all)
  
  
 )
