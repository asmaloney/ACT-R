;;; This file allows models that are running together to hear each 
;;; other talking by monitoring the output-speech command from the
;;; microphone device of the speech module.
;;;
;;; To run models using this code just call the test-it function.
;;; That will install a microphone device for all the models,
;;; create a new command, set that command to monitor for speech 
;;; output, and run the models.
;;;
;;; There are example models in the multi-model-talking-model file,
;;; and running those models is shown in the comments below.
;;;


;;; This is the key to them hearing each other.
;;; When any model speaks into a microphone device all other models
;;; will hear that with the location being the name of the model 
;;; which spoke.

;;; The audio & speech modules take care of a model hearing
;;; itself automatically so don't want to double that up.

(defun relay-speech-output (model string)
  
  ;; Let all other models hear this 
  (dolist (m (mp-models))
    (unless (eq model m) ; for everyone but the originator
      
      (with-model-eval m ; make the other model the current-model
        
        ;; Create the originating model's name as a simple chunk if
        ;; it isn't one already to avoid a warning of it being
        ;; created by default when the sound is generated.
        
        (unless (chunk-p-fct model)
          (define-chunks-fct (list model)))
        
        ;; Create the sound as a word with location indicating
        ;; the speaker.
        
        (new-word-sound string (mp-time) model)))))

(defun test-it ()
  
  (reset)
  
  (add-act-r-command "relay-speech-output" 'relay-speech-output "Handle player speak actions")
  (monitor-act-r-command "output-speech" "relay-speech-output")
      
  (dolist (m (mp-models))
    (with-model-eval m
      (install-device '("speech" "microphone"))))
    
  (run 10)
    
  (remove-act-r-command-monitor "output-speech" "relay-speech-output")    
  (remove-act-r-command "relay-speech-output"))

#|
> (test-it)
     0.000  M1  PROCEDURAL              CONFLICT-RESOLUTION
     0.000  M2  PROCEDURAL              CONFLICT-RESOLUTION
     0.000  M3  PROCEDURAL              CONFLICT-RESOLUTION
     0.050  M2  PROCEDURAL              PRODUCTION-FIRED SPEAK
     0.050  M2  PROCEDURAL              CLEAR-BUFFER GOAL
     0.050  M2  PROCEDURAL              CLEAR-BUFFER VOCAL
     0.050  M2  SPEECH                  SPEAK TEXT Hello
     0.050  M2  GOAL                    SET-BUFFER-CHUNK GOAL CHUNK0
     0.050  M2  PROCEDURAL              CONFLICT-RESOLUTION
     0.200  M2  PROCEDURAL              CONFLICT-RESOLUTION
     0.250  M2  AUDIO                   new-sound
     0.250  M1  AUDIO                   new-sound
     0.250  M3  AUDIO                   new-sound
     0.250  M2  PROCEDURAL              CONFLICT-RESOLUTION
     0.250  M1  PROCEDURAL              CONFLICT-RESOLUTION
     0.250  M3  PROCEDURAL              CONFLICT-RESOLUTION
     0.500  M2  PROCEDURAL              CONFLICT-RESOLUTION
     0.550  M1  AUDIO                   SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
     0.550  M3  AUDIO                   SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
     0.550  M2  AUDIO                   find-sound-failure
     0.550  M1  PROCEDURAL              CONFLICT-RESOLUTION
     0.550  M3  PROCEDURAL              CONFLICT-RESOLUTION
     0.550  M2  PROCEDURAL              CONFLICT-RESOLUTION
     0.600  M1  PROCEDURAL              PRODUCTION-FIRED DETECTED-SOUND
     0.600  M1  PROCEDURAL              CLEAR-BUFFER AURAL-LOCATION
     0.600  M1  PROCEDURAL              CLEAR-BUFFER GOAL
     0.600  M1  PROCEDURAL              CLEAR-BUFFER AURAL
     0.600  M3  PROCEDURAL              PRODUCTION-FIRED DETECTED-SOUND
     0.600  M3  PROCEDURAL              CLEAR-BUFFER AURAL-LOCATION
     0.600  M3  PROCEDURAL              CLEAR-BUFFER GOAL
     0.600  M3  PROCEDURAL              CLEAR-BUFFER AURAL
     0.600  M1  AUDIO                   ATTEND-SOUND AUDIO-EVENT0-0
     0.600  M3  AUDIO                   ATTEND-SOUND AUDIO-EVENT0-0
     0.600  M1  GOAL                    SET-BUFFER-CHUNK GOAL CHUNK0
     0.600  M3  GOAL                    SET-BUFFER-CHUNK GOAL CHUNK0
     0.600  M1  PROCEDURAL              CONFLICT-RESOLUTION
     0.600  M3  PROCEDURAL              CONFLICT-RESOLUTION
     0.725  M1  AUDIO                   SET-BUFFER-CHUNK AURAL WORD0
     0.725  M3  AUDIO                   SET-BUFFER-CHUNK AURAL WORD0
     0.725  M1  PROCEDURAL              CONFLICT-RESOLUTION
     0.725  M3  PROCEDURAL              CONFLICT-RESOLUTION
     0.775  M1  PROCEDURAL              PRODUCTION-FIRED HEAR
I HEARD M2 SAY "Hello" 
     0.775  M1  PROCEDURAL              CLEAR-BUFFER AURAL
     0.775  M1  PROCEDURAL              CLEAR-BUFFER VOCAL
     0.775  M3  PROCEDURAL              PRODUCTION-FIRED HEAR
I HEARD M2 SAY "Hello" 
     0.775  M3  PROCEDURAL              CLEAR-BUFFER AURAL
     0.775  M1  SPEECH                  SPEAK TEXT Back at you
     0.775  M1  PROCEDURAL              CONFLICT-RESOLUTION
     0.775  M3  PROCEDURAL              CONFLICT-RESOLUTION
     0.925  M1  PROCEDURAL              CONFLICT-RESOLUTION
     0.975  M1  AUDIO                   new-sound
     0.975  M2  AUDIO                   new-sound
     0.975  M3  AUDIO                   new-sound
     0.975  M1  PROCEDURAL              CONFLICT-RESOLUTION
     0.975  M2  PROCEDURAL              CONFLICT-RESOLUTION
     0.975  M3  PROCEDURAL              CONFLICT-RESOLUTION
     1.275  M2  AUDIO                   SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT1 NIL
     1.275  M3  AUDIO                   SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT1 NIL
     1.275  M1  AUDIO                   find-sound-failure
     1.275  M1  PROCEDURAL              CONFLICT-RESOLUTION
     1.275  M2  PROCEDURAL              CONFLICT-RESOLUTION
     1.275  M3  PROCEDURAL              CONFLICT-RESOLUTION
     1.325  M2  PROCEDURAL              PRODUCTION-FIRED DETECTED-SOUND
     1.325  M2  PROCEDURAL              CLEAR-BUFFER AURAL-LOCATION
     1.325  M2  PROCEDURAL              CLEAR-BUFFER GOAL
     1.325  M2  PROCEDURAL              CLEAR-BUFFER AURAL
     1.325  M3  PROCEDURAL              PRODUCTION-FIRED DETECTED-SOUND
     1.325  M3  PROCEDURAL              CLEAR-BUFFER AURAL-LOCATION
     1.325  M3  PROCEDURAL              CLEAR-BUFFER GOAL
     1.325  M3  PROCEDURAL              CLEAR-BUFFER AURAL
     1.325  M2  AUDIO                   ATTEND-SOUND AUDIO-EVENT1-0
     1.325  M3  AUDIO                   ATTEND-SOUND AUDIO-EVENT1-0
     1.325  M2  GOAL                    SET-BUFFER-CHUNK GOAL CHUNK1
     1.325  M3  GOAL                    SET-BUFFER-CHUNK GOAL CHUNK1
     1.325  M2  PROCEDURAL              CONFLICT-RESOLUTION
     1.325  M3  PROCEDURAL              CONFLICT-RESOLUTION
     1.525  M1  PROCEDURAL              CONFLICT-RESOLUTION
     1.725  M2  AUDIO                   SET-BUFFER-CHUNK AURAL WORD1
     1.725  M3  AUDIO                   SET-BUFFER-CHUNK AURAL WORD1
     1.725  M2  PROCEDURAL              CONFLICT-RESOLUTION
     1.725  M3  PROCEDURAL              CONFLICT-RESOLUTION
     1.775  M2  PROCEDURAL              PRODUCTION-FIRED HEAR
I HEARD M1 SAY "Back at you" 
     1.775  M2  PROCEDURAL              CLEAR-BUFFER AURAL
     1.775  M3  PROCEDURAL              PRODUCTION-FIRED HEAR
I HEARD M1 SAY "Back at you" 
     1.775  M3  PROCEDURAL              CLEAR-BUFFER AURAL
     1.775  M2  PROCEDURAL              CONFLICT-RESOLUTION
     1.775  M3  PROCEDURAL              CONFLICT-RESOLUTION
     1.775  -   ------                  Stopped because no events left to process
|#
