(clear-all)

(define-model past-tense
   (sgp
     :V t
     :esc T
     :ul t 
     :OL 6   
     :BLL 0.5  
     :ANs 0.1   
     :EGs 0.2   
     :mas  3.5 
     :LF 0.5
     :RT 0.5
     :epl t
     :alpha 0.1 
     :pct t
     :iu 5    
     :ncnar nil
     :do-not-harvest imaginal)
  
  (chunk-type past-tense verb stem suffix)
  (chunk-type goal state)
  
  (define-chunks 
      (starting-goal isa goal state start)
      (start isa chunk) (blank isa chunk) (done isa chunk))


  (declare-buffer-usage goal goal state)
  (declare-buffer-usage imaginal past-tense :all)

;;; When there is a stem and no suffix we have an irregular

(p past-tense-irregular
   =goal>
     isa    goal
     state  done
   =imaginal>
     isa    past-tense
     verb   =word
     stem   =past
     suffix blank
   ==>
   =goal> 
     state  nil)

(spp past-tense-irregular :reward 5)

;;; When the stem matches the verb and the suffix is not "blank" consider it regular

(p past-tense-regular
   =goal>
    isa     goal
    state   done
   =imaginal>
     isa    past-tense
     verb   =stem
     stem   =stem
     suffix =suffix
   !safe-eval! (not (eq =suffix 'blank)) 
  ==>
   =goal> 
     state  nil)

(spp past-tense-regular :reward 4.2)

;;; when there's no stem and no suffix that's a "give up" result

(p no-past-tense-formed
   =goal>
     isa    goal
     state  done
   =imaginal>
     isa    past-tense
     stem   nil
     suffix nil
  ==>
   =goal> 
     state  nil)

(spp no-past-tense-formed :reward 3.9) 
)
