(clear-all)

(define-model unit2
    
(sgp :v t :show-focus t)


(chunk-type read-letters step)
(chunk-type array letter1 letter2 letter3)

(add-dm 
 (start isa chunk)
 (goal isa read-letters step start))

 
(goal-focus goal)
)
