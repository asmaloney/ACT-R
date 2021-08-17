# Play the simple capture game with 2 models using no interface, but
# creating custom visicon features for the model to see.

import actr

# Each model will have two features which it can see.  There is one
# for each player and it contains these features:

# While game playing

# Name         Att  Loc           Kind    Position  Name    Size  Turn
# -----------  ---  ------------  ------  --------  ------  ----  ----
# PLAYER-LOC0  NEW  (  0 0 1080)  PLAYER  0         MODEL1  1.0   T   
# PLAYER-LOC1  NEW  (100 0 1080)  PLAYER  5         MODEL2  1.0       


# when game over

# Name         Att  Loc           Kind    Position  Name    Size  Result
# -----------  ---  ------------  ------  --------  ------  ----  ------
# PLAYER-LOC0  T    (  0 0 1080)  PLAYER  4         MODEL1  1.0   WIN   
# PLAYER-LOC1  NIL  (100 0 1080)  PLAYER  4         MODEL2  1.0   LOSE  
 


# The position and turn values are only available in the visual object chunk.



# global variables for game info

current_player = None # name of the model
p1 = None             # player1 model name
p2 = None             # player2 model name
game_over = False     # If true stop the game
winner = None         # which player won

safety_stop = False    # flag that modeler wants run to stop

p1_position = -1   # player locations 0-5
p2_position = -1

p1p1 = None   # Record the visual features for each player
p1p2 = None   # so they can be updated instead of just
p2p1 = None   # clearing and adding new ones
p2p2 = None


# function called for a keypress action which makes the
# move and updates the visual info if it's a valid move
# by the current player

def make_move(model,key):
    global p1_position, p2_position, winner, current_player

    if (model.lower() == current_player.lower()) and (key == '1' or key == '2'):
        if key == '1':
            dist = 1
        else:
            dist = 2

        if model.lower() == p1.lower():
            p1_position += dist
        else:
            p2_position -= dist
    
    if p2_position <= p1_position: # the game is over
           
        winner = current_player
        actr.schedule_event_relative(3,'set_game_over')
         
        if winner == p1:
            p1_result = 'win'
            p2_result = 'lose'
        else:
            p1_result = 'lose'
            p2_result = 'win'

        # update player 1 visual features
                      
        actr.set_current_model(p1)

        actr.modify_visicon_features([p1p1,'position',p1_position,'turn',False,'result',p1_result])
        actr.modify_visicon_features([p1p2,'position',p2_position,'turn',False,'result',p2_result])
           
           
        # update player 2 visual features
                      
        actr.set_current_model(p2)

        actr.modify_visicon_features([p2p1,'position',p1_position,'turn',False,'result',p1_result])
        actr.modify_visicon_features([p2p2,'position',p2_position,'turn',False,'result',p2_result])
          
    else:  # update the game and all visual features
           
        if current_player == p1:
            current_player = p2
            p1_turn = False
            p2_turn = True
        else:
            current_player = p1
            p1_turn = True
            p2_turn = False

        # update player 1 visual features
                      
        actr.set_current_model(p1)

        actr.modify_visicon_features([p1p1,'position',p1_position,'turn',p1_turn])
        actr.modify_visicon_features([p1p2,'position',p2_position,'turn',p2_turn])
           
           
        # update player 2 visual features
                      
        actr.set_current_model(p2)

        actr.modify_visicon_features([p2p1,'position',p1_position,'turn',p1_turn])
        actr.modify_visicon_features([p2p2,'position',p2_position,'turn',p2_turn])
 

def set_game_over():
    global game_over

    game_over = True

def stop_a_run():
    global safety_stop
    
    safety_stop = True


def is_game_over(time):
    return (safety_stop or game_over or (time > 1000000))

# play one round of the game resetting the models
# before it starts.  Player1 and player2 are the
# names of the models to run in the indicated 
# position.

def play(player1,player2):
    global  p1, p2, p1_position, p2_position, game_over, current_player, safety_stop, winner, p1p1, p1p2, p2p1, p2p2

    if (player1.lower() in (x.lower() for x in actr.mp_models())) and (player2.lower() in (x.lower() for x in actr.mp_models())):
      
        actr.add_command('stop-a-run',stop_a_run,'Set the flag to terminate the game.')

        win = actr.open_exp_window('Safety',visible=True,height=100,width=100,x=100,y=300)
        actr.add_button_to_exp_window('Safety', text='STOP', x=0, y=0, action='stop-a-run', height=90, width=90, color='red')
  
        actr.add_command('move',make_move,'Handle player key presses')
        actr.monitor_command('output-key','move')

        actr.reset()


    
        # initialize the game information
        p1 = player1
        p2 = player2
        p1_position = 0
        p2_position = 5
        game_over = False
        current_player = player1
        safety_stop = False
        winner = None
 
        actr.set_current_model(player1)

        actr.define_chunks(player2)
        
        feats = actr.add_visicon_features(['isa',['player-loc','player'],
                                           'screen-x',0,'screen-y',0,'name',player1,
                                           'position',[False,p1_position],
                                           'turn',[False,True]],
                                          ['isa',['player-loc','player'],
                                           'screen-x',100,'screen-y',0,'name',player2,
                                           'position',[False,p2_position]])

        p1p1 = feats[0]
        p1p2 = feats[1]

        actr.set_current_model(player2)

        actr.define_chunks(player1)
        
        feats = actr.add_visicon_features(['isa',['player-loc','player'],
                                           'screen-x',0,'screen-y',0,'name',player1,
                                           'position',[False,p1_position],
                                           'turn',[False,True]],
                                          ['isa',['player-loc','player'],
                                           'screen-x',100,'screen-y',0,'name',player2,
                                           'position',[False,p2_position]])

        p2p1 = feats[0]
        p2p2 = feats[1]

        actr.add_command('is-game-over',is_game_over,'Test whether game should stop running.')
        actr.add_command('set_game_over',set_game_over,'Set the flag to stop the game.')

        actr.run_until_condition('is-game-over')
    
        actr.clear_exp_window(win)
        actr.remove_command('stop-a-run')
        actr.remove_command_monitor('output-key','move')    
        actr.remove_command('move')
        actr.remove_command('is-game-over')
        actr.remove_command('set_game_over')

    return winner


"""
>>> game.play('model1','model2')
     0.000  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL1  VISION                 PROC-DISPLAY
     0.000  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION PLAYER-LOC0 NIL
     0.000  MODEL1  VISION                 visicon-update
     0.000  MODEL2  VISION                 PROC-DISPLAY
     0.000  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION PLAYER-LOC0 NIL
     0.000  MODEL2  VISION                 visicon-update
     0.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED START
     0.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL2  PROCEDURAL             PRODUCTION-SELECTED START
     0.000  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED START
MY NAME IS MODEL1
     0.050  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050  MODEL2  PROCEDURAL             PRODUCTION-FIRED START
MY NAME IS MODEL2
     0.050  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050  MODEL1  VISION                 Find-location
     0.050  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION PLAYER-LOC0
     0.050  MODEL2  VISION                 Find-location
     0.050  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION PLAYER-LOC1
     0.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL1  PROCEDURAL             PRODUCTION-SELECTED ATTEND
     0.050  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     0.050  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL2  PROCEDURAL             PRODUCTION-SELECTED ATTEND
     0.050  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     0.100  MODEL1  PROCEDURAL             PRODUCTION-FIRED ATTEND
     0.100  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL
     0.100  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     0.100  MODEL2  PROCEDURAL             PRODUCTION-FIRED ATTEND
     0.100  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL
     0.100  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     0.100  MODEL1  VISION                 Move-attention PLAYER-LOC0-1 NIL
     0.100  MODEL2  VISION                 Move-attention PLAYER-LOC1-0 NIL
     0.100  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.100  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.185  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     0.185  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL PLAYER0
     0.185  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     0.185  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL PLAYER0
     0.185  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.185  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     0.185  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.185  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     0.185  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.185  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.235  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 0
     0.235  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     0.235  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     0.235  MODEL1  MOTOR                  PRESS-KEY KEY 1
     0.235  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.485  MODEL1  MOTOR                  PREPARATION-COMPLETE
     0.485  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.535  MODEL1  MOTOR                  INITIATION-COMPLETE
     0.535  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.635  MODEL1  KEYBOARD               output-key MODEL1 1
     0.635  MODEL1  VISION                 PROC-DISPLAY
     0.635  MODEL1  VISION                 visicon-update
     0.635  MODEL2  VISION                 PROC-DISPLAY
     0.635  MODEL2  VISION                 visicon-update
     0.635  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.635  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.720  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     0.720  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK0 NIL
     0.720  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     0.720  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK0 NIL
     0.720  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.720  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.720  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     0.720  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.720  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     0.720  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.770  MODEL2  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 5
     0.770  MODEL2  PROCEDURAL             MODULE-REQUEST MANUAL
     0.770  MODEL2  PROCEDURAL             CLEAR-BUFFER MANUAL
     0.770  MODEL2  MOTOR                  PRESS-KEY KEY 1
     0.770  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.785  MODEL1  MOTOR                  FINISH-MOVEMENT
     0.785  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.020  MODEL2  MOTOR                  PREPARATION-COMPLETE
     1.020  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.070  MODEL2  MOTOR                  INITIATION-COMPLETE
     1.070  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.170  MODEL2  KEYBOARD               output-key MODEL2 1
     1.170  MODEL1  VISION                 PROC-DISPLAY
     1.170  MODEL1  VISION                 visicon-update
     1.170  MODEL2  VISION                 PROC-DISPLAY
     1.170  MODEL2  VISION                 visicon-update
     1.170  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.170  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.255  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     1.255  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK1 NIL
     1.255  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     1.255  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK1 NIL
     1.255  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.255  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.255  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     1.255  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.255  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     1.255  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.305  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 1
     1.305  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     1.305  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.305  MODEL1  MOTOR                  PRESS-KEY KEY 1
     1.305  MODEL1  MOTOR                  PREPARATION-COMPLETE
     1.305  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.320  MODEL2  MOTOR                  FINISH-MOVEMENT
     1.320  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.355  MODEL1  MOTOR                  INITIATION-COMPLETE
     1.355  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.455  MODEL1  KEYBOARD               output-key MODEL1 1
     1.455  MODEL1  VISION                 PROC-DISPLAY
     1.455  MODEL1  VISION                 visicon-update
     1.455  MODEL2  VISION                 PROC-DISPLAY
     1.455  MODEL2  VISION                 visicon-update
     1.455  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.455  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.540  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     1.540  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK2 NIL
     1.540  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     1.540  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK2 NIL
     1.540  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.540  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.540  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     1.540  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.540  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     1.540  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.590  MODEL2  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 4
     1.590  MODEL2  PROCEDURAL             MODULE-REQUEST MANUAL
     1.590  MODEL2  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.590  MODEL2  MOTOR                  PRESS-KEY KEY 1
     1.590  MODEL2  MOTOR                  PREPARATION-COMPLETE
     1.590  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.605  MODEL1  MOTOR                  FINISH-MOVEMENT
     1.605  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.640  MODEL2  MOTOR                  INITIATION-COMPLETE
     1.640  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.740  MODEL2  KEYBOARD               output-key MODEL2 1
     1.740  MODEL1  VISION                 PROC-DISPLAY
     1.740  MODEL1  VISION                 visicon-update
     1.740  MODEL2  VISION                 PROC-DISPLAY
     1.740  MODEL2  VISION                 visicon-update
     1.740  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.740  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     1.825  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK3 NIL
     1.825  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     1.825  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK3 NIL
     1.825  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.825  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     1.825  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.825  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     1.825  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.875  MODEL1  PROCEDURAL             PRODUCTION-FIRED 2-STEP
I AM AT POSITION 2
     1.875  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     1.875  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.875  MODEL1  MOTOR                  PRESS-KEY KEY 2
     1.875  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.890  MODEL2  MOTOR                  FINISH-MOVEMENT
     1.890  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.025  MODEL1  MOTOR                  PREPARATION-COMPLETE
     2.025  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.075  MODEL1  MOTOR                  INITIATION-COMPLETE
     2.075  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.175  MODEL1  KEYBOARD               output-key MODEL1 2
     2.175  MODEL1  VISION                 PROC-DISPLAY
     2.175  MODEL1  VISION                 visicon-update
     2.175  MODEL2  VISION                 PROC-DISPLAY
     2.175  MODEL2  VISION                 visicon-update
     2.175  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.175  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.260  MODEL1  VISION                 Encoding-complete PLAYER-LOC0-1 NIL
     2.260  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK4 NIL
     2.260  MODEL2  VISION                 Encoding-complete PLAYER-LOC1-0 NIL
     2.260  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL CHUNK4 NIL
     2.260  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.260  MODEL1  PROCEDURAL             PRODUCTION-SELECTED RESULT
     2.260  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.260  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     2.260  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.260  MODEL2  PROCEDURAL             PRODUCTION-SELECTED RESULT
     2.260  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.260  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     2.310  MODEL1  PROCEDURAL             PRODUCTION-FIRED RESULT
I WIN
     2.310  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     2.310  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.310  MODEL2  PROCEDURAL             PRODUCTION-FIRED RESULT
I LOSE
     2.310  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     2.310  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL
     2.310  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.310  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.325  MODEL1  MOTOR                  FINISH-MOVEMENT
     2.325  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     5.175  MODEL1  NONE                   set_game_over
     5.175  -       ------                 Stopped because condition is true
'model1'
"""