(clear-all)

(defparameter *model-code*
    '((sgp :v t :trace-detail high :needs-mouse nil :er t)
      
      (chunk-type play my-color my-name state)
      
      (set-audloc-default - location self :attended nil)
      (set-visloc-default color green)
      
      (define-chunks move say-done start)
      
      (declare-buffer-usage goal play :all)
      
      (p game-over
         =visual-location>
         ?visual>
           state free
       ==>
         +visual>
           cmd move-attention
           screen-pos =visual-location)
      
      (p winner
         =goal>
         my-name =name
         =visual>
         value =name
         ==>
         -goal>
         !output! (I am the winner))
      
      (p loser
         =goal>
         my-name =name
         =visual>
         - value =name
         value =winner
         ==>
         -goal>
         !output! (=winner was the winner))
      
      
      (p hear-something
         =goal>
         =aural-location>
         ?aural>
           state free
       ==>
         +aural>
           event =aural-location)
      
      (p I-start
         =goal>
           my-name =name
         =aural>
           content =name
       ==>
         =goal>
           state move)
      
      (p other-player-starts
         =goal>
           my-name =name
         =aural>
          - content =name
          - content "one"
          - content "two"
       ==>
         )
      
      (p other-player-went-one
         =goal>
         =aural>
           content "one"
       ==>
         =goal>
           state move)
      
      (p other-player-went-two
         =goal>
         =aural>
           content "two"
       ==>
         =goal>
           state move)

      (p 1-step
         =goal>
           state move
         ?vocal>
           state free
       ==>
         +vocal>
           cmd speak
           string "one"
         =goal>
           state nil)
      
      (p 2-step
         =goal>
           state move
         ?vocal>
           state free
       ==>
         +vocal>
           cmd speak
           string "two"
         =goal>
           state nil)      
      ))
      
(define-model-fct 'model1 *model-code*)
(define-model-fct 'model2 *model-code*)

#|
CG-USER(63): (PLAY 'MODEL1 'MODEL2)
     0.000  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL1  AUDIO                  new-sound
     0.000  MODEL2  AUDIO                  new-sound
     0.000  MODEL2  VISION                 PROC-DISPLAY
     0.000  MODEL2  VISION                 visicon-update
     0.000  MODEL1  VISION                 PROC-DISPLAY
     0.000  MODEL1  VISION                 visicon-update
     0.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
#|Warning (in model MODEL1): Creating chunk START with no slots |#
     0.300  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
#|Warning (in model MODEL2): Creating chunk START with no slots |#
     0.300  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
     0.300  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.300  MODEL1  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     0.300  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300  MODEL1  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     0.300  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     0.300  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.300  MODEL2  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     0.300  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.300  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     0.300  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     0.350  MODEL1  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     0.350  MODEL1  PROCEDURAL             MODULE-REQUEST AURAL
     0.350  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     0.350  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL
     0.350  MODEL2  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     0.350  MODEL2  PROCEDURAL             MODULE-REQUEST AURAL
     0.350  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     0.350  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     0.350  MODEL1  AUDIO                  ATTEND-SOUND AUDIO-EVENT0-0
     0.350  MODEL2  AUDIO                  ATTEND-SOUND AUDIO-EVENT0-0
     0.350  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.350  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.500  MODEL1  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     0.500  MODEL2  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     0.500  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL WORD0
     0.500  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL WORD0
     0.500  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.500  MODEL1  PROCEDURAL             PRODUCTION-SELECTED I-START
     0.500  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.500  MODEL1  PROCEDURAL             BUFFER-READ-ACTION AURAL
     0.500  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.500  MODEL2  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-STARTS
     0.500  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.500  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL
     0.550  MODEL1  PROCEDURAL             PRODUCTION-FIRED I-START
     0.550  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.550  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL
     0.550  MODEL2  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-STARTS
     0.550  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     0.550  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.550  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     0.550  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.550  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     0.550  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.600  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
     0.600  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.600  MODEL1  PROCEDURAL             MODULE-REQUEST VOCAL
     0.600  MODEL1  PROCEDURAL             CLEAR-BUFFER VOCAL
     0.600  MODEL1  SPEECH                 SPEAK TEXT one
     0.600  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.750  MODEL1  SPEECH                 PREPARATION-COMPLETE
     0.750  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.800  MODEL1  SPEECH                 INITIATION-COMPLETE
     0.800  MODEL1  AUDIO                  new-sound
     0.800  MODEL1  MICROPHONE             output-speech MODEL1 one
     0.800  MODEL2  AUDIO                  new-sound
     0.800  MODEL2  VISION                 PROC-DISPLAY
     0.800  MODEL2  VISION                 visicon-update
     0.800  MODEL1  VISION                 PROC-DISPLAY
     0.800  MODEL1  VISION                 visicon-update
     0.800  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.800  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.950  MODEL1  SPEECH                 FINISH-MOVEMENT
     0.950  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.100  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT1 NIL
     1.100  MODEL1  AUDIO                  find-sound-failure
     1.100  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.100  MODEL2  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     1.100  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.100  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     1.100  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     1.100  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.150  MODEL2  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     1.150  MODEL2  PROCEDURAL             MODULE-REQUEST AURAL
     1.150  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     1.150  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     1.150  MODEL2  AUDIO                  ATTEND-SOUND AUDIO-EVENT1-0
     1.150  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.225  MODEL2  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     1.225  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL WORD1
     1.225  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.225  MODEL2  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-WENT-ONE
     1.225  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.225  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL
     1.275  MODEL2  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-WENT-ONE
     1.275  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.275  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     1.275  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.275  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     1.275  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.275  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     1.325  MODEL2  PROCEDURAL             PRODUCTION-FIRED 1-STEP
     1.325  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.325  MODEL2  PROCEDURAL             MODULE-REQUEST VOCAL
     1.325  MODEL2  PROCEDURAL             CLEAR-BUFFER VOCAL
     1.325  MODEL2  SPEECH                 SPEAK TEXT one
     1.325  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.475  MODEL2  SPEECH                 PREPARATION-COMPLETE
     1.475  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.525  MODEL2  SPEECH                 INITIATION-COMPLETE
     1.525  MODEL2  AUDIO                  new-sound
     1.525  MODEL2  MICROPHONE             output-speech MODEL2 one
     1.525  MODEL1  AUDIO                  new-sound
     1.525  MODEL2  VISION                 PROC-DISPLAY
     1.525  MODEL2  VISION                 visicon-update
     1.525  MODEL1  VISION                 PROC-DISPLAY
     1.525  MODEL1  VISION                 visicon-update
     1.525  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.525  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.675  MODEL2  SPEECH                 FINISH-MOVEMENT
     1.675  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT2 NIL
     1.825  MODEL2  AUDIO                  find-sound-failure
     1.825  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     1.825  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.825  MODEL1  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     1.825  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     1.825  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.875  MODEL1  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     1.875  MODEL1  PROCEDURAL             MODULE-REQUEST AURAL
     1.875  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     1.875  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL
     1.875  MODEL1  AUDIO                  ATTEND-SOUND AUDIO-EVENT2-0
     1.875  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.950  MODEL1  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     1.950  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL WORD2
     1.950  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.950  MODEL1  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-WENT-ONE
     1.950  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.950  MODEL1  PROCEDURAL             BUFFER-READ-ACTION AURAL
     2.000  MODEL1  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-WENT-ONE
     2.000  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.000  MODEL1  PROCEDURAL             CLEAR-BUFFER AURAL
     2.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     2.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.000  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     2.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED 2-STEP
     2.050  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.050  MODEL1  PROCEDURAL             MODULE-REQUEST VOCAL
     2.050  MODEL1  PROCEDURAL             CLEAR-BUFFER VOCAL
     2.050  MODEL1  SPEECH                 SPEAK TEXT two
     2.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.150  MODEL1  SPEECH                 PREPARATION-COMPLETE
     2.150  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.200  MODEL1  SPEECH                 INITIATION-COMPLETE
     2.200  MODEL1  AUDIO                  new-sound
     2.200  MODEL1  MICROPHONE             output-speech MODEL1 two
     2.200  MODEL2  AUDIO                  new-sound
     2.200  MODEL2  VISION                 PROC-DISPLAY
     2.200  MODEL2  VISION                 visicon-update
     2.200  MODEL1  VISION                 PROC-DISPLAY
     2.200  MODEL1  VISION                 visicon-update
     2.200  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.200  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.350  MODEL1  SPEECH                 FINISH-MOVEMENT
     2.350  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.500  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT3 NIL
     2.500  MODEL1  AUDIO                  find-sound-failure
     2.500  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.500  MODEL2  PROCEDURAL             PRODUCTION-SELECTED HEAR-SOMETHING
     2.500  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.500  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL-LOCATION
     2.500  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION AURAL
     2.500  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.550  MODEL2  PROCEDURAL             PRODUCTION-FIRED HEAR-SOMETHING
     2.550  MODEL2  PROCEDURAL             MODULE-REQUEST AURAL
     2.550  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL-LOCATION
     2.550  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     2.550  MODEL2  AUDIO                  ATTEND-SOUND AUDIO-EVENT3-0
     2.550  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.625  MODEL2  AUDIO                  AUDIO-ENCODING-COMPLETE #<WORD-SOUND-EVT >
     2.625  MODEL2  AUDIO                  SET-BUFFER-CHUNK AURAL WORD3
     2.625  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.625  MODEL2  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-WENT-TWO
     2.625  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.625  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL
     2.675  MODEL2  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-WENT-TWO
     2.675  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.675  MODEL2  PROCEDURAL             CLEAR-BUFFER AURAL
     2.675  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.675  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     2.675  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.675  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     2.725  MODEL2  PROCEDURAL             PRODUCTION-FIRED 2-STEP
     2.725  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.725  MODEL2  PROCEDURAL             MODULE-REQUEST VOCAL
     2.725  MODEL2  PROCEDURAL             CLEAR-BUFFER VOCAL
     2.725  MODEL2  SPEECH                 SPEAK TEXT two
     2.725  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.825  MODEL2  SPEECH                 PREPARATION-COMPLETE
     2.825  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.875  MODEL2  SPEECH                 INITIATION-COMPLETE
     2.875  MODEL2  AUDIO                  new-sound
     2.875  MODEL2  MICROPHONE             output-speech MODEL2 two
     2.875  MODEL1  AUDIO                  new-sound
     2.875  MODEL2  VISION                 PROC-DISPLAY
     2.875  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION8-0 NIL
     2.875  MODEL2  VISION                 visicon-update
     2.875  MODEL1  VISION                 PROC-DISPLAY
     2.875  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION8-0 NIL
     2.875  MODEL1  VISION                 visicon-update
     2.875  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.875  MODEL2  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.875  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.875  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.875  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.875  MODEL1  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.875  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.875  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.925  MODEL2  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.925  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL
     2.925  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.925  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.925  MODEL1  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     2.925  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL
     2.925  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.925  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.925  MODEL2  VISION                 Move-attention VISUAL-LOCATION8-0-0 NIL
     2.925  MODEL1  VISION                 Move-attention VISUAL-LOCATION8-0-0 NIL
     2.925  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.925  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     3.010  MODEL2  VISION                 Encoding-complete VISUAL-LOCATION8-0-0 NIL
     3.010  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.010  MODEL1  VISION                 Encoding-complete VISUAL-LOCATION8-0-0 NIL
     3.010  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.010  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     3.010  MODEL2  PROCEDURAL             PRODUCTION-SELECTED WINNER
     3.010  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.010  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     3.010  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     3.010  MODEL1  PROCEDURAL             PRODUCTION-SELECTED LOSER
     3.010  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.010  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     3.025  MODEL2  SPEECH                 FINISH-MOVEMENT
     3.060  MODEL2  PROCEDURAL             PRODUCTION-FIRED WINNER
I AM THE WINNER 
     3.060  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     3.060  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     3.060  MODEL1  PROCEDURAL             PRODUCTION-FIRED LOSER
"model2" WAS THE WINNER 
     3.060  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     3.060  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     3.060  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     3.060  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     3.175  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT4 NIL
     3.175  MODEL2  AUDIO                  find-sound-failure
     3.175  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     3.175  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     5.875  MODEL2  NONE                   SET-GAME-OVER MODEL2
     5.875  -       ------                 Stopped because condition is true
MODEL2
|#