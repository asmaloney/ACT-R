# This file allows models that are running together to hear each 
# other talking by monitoring the output-speech command from the
# microphone device of the speech module.
#
# To run models using this code just call the test_it function.
# That will install a microphone device for all the models,
# create a new command, set that command to monitor for speech 
# output, and run the models.
#
# There are example models in the multi-model-talking-model file,
# and running those models is shown in the comments below.
#

import actr

# This is the key to them hearing each other.
# When any model speaks into a microphone device all other models
# will hear that with the location being the name of the model 
# which spoke.

# The audio & speech modules take care of a model hearing
# itself automatically so don't want to double that up.

def relay_speech_output (model, string):

    for m in actr.mp_models():

        if not (m.lower() == model.lower()):  # all other models will hear this
        
            actr.set_current_model(m)
  
            # Create the originating model's name as a simple chunk if
            # it isn't one already to avoid a warning of it being
            # created by default when the sound is generated.

            if not(actr.chunk_p(model)):
                actr.define_chunks(model)
        
            # Create the sound as a word with location indicating
            # the speaker.
        
            actr.new_word_sound(string, actr.mp_time(), model)

def test_it ():

    actr.reset()
  
    actr.add_command("relay-speech-output", relay_speech_output, "Handle player speak actions")
    actr.monitor_command("output-speech","relay-speech-output")

    for m in actr.mp_models():
        actr.set_current_model(m)
        actr.install_device(["speech","microphone"])

    actr.run(10)
    
    actr.remove_command_monitor("output-speech", "relay-speech-output")    
    actr.remove_command("relay-speech-output")

"""
>>> import multi_model_talking
>>> actr.load_act_r_model("ACT-R:examples;multiple-models;multi-model-talking-models.lisp")
True
>>> multi_model_talking.test_it()
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
"""