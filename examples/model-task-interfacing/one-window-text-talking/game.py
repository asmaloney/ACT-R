
# Play the capture game with a simple single interface that
# just shows position.  Have models say "one" or "two" to indicate 
# their move and the game says the starting player's name at 
# the beginning.  Show the winner's name in green when game over.



import actr

# global variables for game info

current_player = None  # name of model
p1 = None              # player1 name
p2 = None              # player2 name
game_over = None       # if true stop the game
p1_position = -1       # current player locations, 0-5
p2_position = -1

window = None

safety_stop = False # flag that modeler wants run to stop

p1_text = None
p2_text = None


def set_game_over(player):
    global game_over

    game_over = player

def stop_a_run():
    global safety_stop
    
    safety_stop = True


def is_game_over(time):
    return (safety_stop or game_over or (time > 1000000))



# the function that gets called when a model speaks
# assume that models take turns appropriately

def process_moves(model, string):
    global p1_position, p2_position,current_player,p1_text,p2_text

    # Let all other models hear this 
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
  
    if (string.lower() == "one"):
        move = 1
    elif (string.lower() == "two"):
        move = 2
    else:
        print ("Wrong move assumed to be 1: %s"%string)
        move = 1
    
    if current_player == p1:
        p1_position += move
    else:
        p2_position -= move
    
    actr.clear_exp_window(window)
    
    if (p2_position <= p1_position):
        # if there's a winner
        actr.schedule_event_relative(3,'set_game_over',params=[current_player])
                    
        actr.add_text_to_exp_window(window, current_player, x=60, y=20, color='green', height=30, width=80, font_size=20)
    else:        
        # not a winner so update the display with the new position
        
        p1_text = actr.add_text_to_exp_window(window, str(p1_position), x=20, y=10, color='red', height=30, width=30, font_size=20)
        p2_text =  actr.add_text_to_exp_window(window, str(p2_position), x=140, y=10, color='blue', height=30, width=30, font_size=20)
        
        if current_player == p1:
            current_player = p2
        else:
            current_player = p1


# Function to play one game resetting the models
# before it starts.  Player1 and player2 are model
# names.
      
def play (player1,player2):
    global window, p1, p2, p1_position, p2_position, current_player, game_over, safety_stop, p1_text, p2_text

    if (player1.lower() in (x.lower() for x in actr.mp_models())) and (player2.lower() in (x.lower() for x in actr.mp_models())):
    
        actr.reset()

        actr.set_current_model(player1)

        # create a goal chunk with the player's color and name  
        actr.define_chunks(['goal', 'isa', 'play', 'my-color', 'red', 'my-name', "'%s'"%player1])
        actr.goal_focus('goal')
        actr.set_parameter_value(':show-focus','red')
    
        actr.set_current_model(player2)
 
        # create a goal chunk with the player's color and name  
        actr.define_chunks(['goal', 'isa', 'play', 'my-color', 'blue', 'my-name', "'%s'"%player2])
        actr.goal_focus('goal')
        actr.set_parameter_value(':show-focus','blue')


        window = actr.open_exp_window("game",visible=True,width=200,height=100)
        safety_window = actr.open_exp_window('Safety',visible=True,height=100,width=100,x=100,y=100)
      
        actr.add_command('stop-a-run',stop_a_run,'Set the flag to terminate the game.')
 
        actr.add_button_to_exp_window(safety_window, text="STOP", x=0, y=0, action='stop-a-run', height=80, width=80, color='red')
      
        p1 = player1
        p2 = player2
        game_over = False
        safety_stop = False
        current_player = player1
        p1_position = 0
        p2_position = 5

 
        for m in actr.mp_models():
            actr.set_current_model(m)
            actr.install_device(window)

        p1_text = actr.add_text_to_exp_window (window, str(p1_position), x=20, y=10, color='red', height=30, width=30, font_size=20)
        p2_text = actr.add_text_to_exp_window (window, str(p2_position), x=140, y=10, color='blue', height=30, width=30, font_size=20)
      
        actr.add_command("process-moves", process_moves, "Handle player speak actions")
        actr.monitor_command("output-speech","process-moves")
      
        # speak to all models telling them the name
        # of the first player.
      
        for m in actr.mp_models():
            actr.set_current_model(m)
            actr.new_word_sound(p1,0,'start')

        actr.add_command('is-game-over',is_game_over,'Test whether game should stop running.')
        actr.add_command('set_game_over',set_game_over,'Set the flag to stop the game.')

        actr.run_until_condition('is-game-over',True)
    
        actr.remove_command_monitor("output-speech", "process-moves")    
        actr.remove_command("process-moves")
        actr.remove_command('stop-a-run')
        actr.remove_command('is-game-over')
        actr.remove_command('set_game_over')
  
  
    return game_over
  
"""
>>> game.play('model1','model2')
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
     0.300  MODEL1  AUDIO                  SET-BUFFER-CHUNK AURAL-LOCATION AUDIO-EVENT0 NIL
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
     0.550  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     0.550  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.550  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     0.550  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.600  MODEL1  PROCEDURAL             PRODUCTION-FIRED 2-STEP
     0.600  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.600  MODEL1  PROCEDURAL             MODULE-REQUEST VOCAL
     0.600  MODEL1  PROCEDURAL             CLEAR-BUFFER VOCAL
     0.600  MODEL1  SPEECH                 SPEAK TEXT two
     0.600  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.750  MODEL1  SPEECH                 PREPARATION-COMPLETE
     0.750  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.800  MODEL1  SPEECH                 INITIATION-COMPLETE
     0.800  MODEL1  AUDIO                  new-sound
     0.800  MODEL1  MICROPHONE             output-speech MODEL1 two
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
     1.225  MODEL2  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-WENT-TWO
     1.225  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.225  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL
     1.275  MODEL2  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-WENT-TWO
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
     2.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     2.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.000  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VOCAL
     2.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
     2.050  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.050  MODEL1  PROCEDURAL             MODULE-REQUEST VOCAL
     2.050  MODEL1  PROCEDURAL             CLEAR-BUFFER VOCAL
     2.050  MODEL1  SPEECH                 SPEAK TEXT one
     2.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.150  MODEL1  SPEECH                 PREPARATION-COMPLETE
     2.150  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.200  MODEL1  SPEECH                 INITIATION-COMPLETE
     2.200  MODEL1  AUDIO                  new-sound
     2.200  MODEL1  MICROPHONE             output-speech MODEL1 one
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
     2.625  MODEL2  PROCEDURAL             PRODUCTION-SELECTED OTHER-PLAYER-WENT-ONE
     2.625  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.625  MODEL2  PROCEDURAL             BUFFER-READ-ACTION AURAL
     2.675  MODEL2  PROCEDURAL             PRODUCTION-FIRED OTHER-PLAYER-WENT-ONE
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
     2.875  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION8 NIL
     2.875  MODEL2  VISION                 visicon-update
     2.875  MODEL1  VISION                 PROC-DISPLAY
     2.875  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION8 NIL
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
     2.925  MODEL2  VISION                 Move-attention VISUAL-LOCATION8-0 NIL
     2.925  MODEL1  VISION                 Move-attention VISUAL-LOCATION8-0 NIL
     2.925  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.925  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     3.010  MODEL2  VISION                 Encoding-complete VISUAL-LOCATION8-0 NIL
     3.010  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.010  MODEL1  VISION                 Encoding-complete VISUAL-LOCATION8-0 NIL
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
     5.875  MODEL1  NONE                   set_game_over model2
     5.875  -       ------                 Stopped because condition is true
'model2'
"""
