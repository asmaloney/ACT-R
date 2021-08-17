# Play the simple capture game with 2 models using no interface.
# Goal chunk holds all information and state indicates what to do (if set).
# Creates new goal chunks everytime instead of checking and modifying.

import actr
import numbers

#  (chunk-type play my-pos p1 p2 state result)
# My-pos is either p1 or p2 to indicate which player
# p1 and p2 slots hold current position for that player
# State slot will be move or game-over
# Result will be win or lose
 
# Assumes models interact appropriately so as to not
# have problems with goal chunks being modified:
# - only do something on its own turn
# - stop doing everything once it makes the move

# global variables for game info

current_player = None   # model name
p1 = None               # player1 model name
p2 = None               # player2 model name
game_over = False       # If true stop the game

next_move = 0        # action made by a player = number of steps

p1_position = -1  # current player spots 0-5
p2_position = -1   


# function called directly by a model to make a move of dist spaces
# Just schedule a call to a function that actually handles that after
# everything else the model needs to do at this time.

def make_move(dist):
    global next_move

    if (actr.current_model().lower() == current_player.lower()):
        next_move = dist
        actr.schedule_break_relative(0,priority=":min")


# Actually update the models' goal chunks as needed

def process_move(dist):
    global p1_position,p2_position,game_over,current_player

    forfeit = False
    winner = None
    if isinstance(dist,numbers.Number):
        if (current_player == p1):
            p1_position += dist
        else:
            p2_position -= dist
    else:
        forfeit = current_player
    
  
    if (forfeit or (p2_position <= p1_position)):  # game over
        if forfeit:
            if (current_player == p1):
                winner = p2
            else:
                winner = p1
        else:
            winner = current_player
           
        game_over = winner
         
        # create new chunks with the results
           
        actr.set_current_model(p1)
        
        if p1 == winner:
            result = 'win'
        else:
            result = 'lose'

        actr.goal_focus(actr.define_chunks(['isa','play',
                                            'my-pos', 'p1',
                                            'p1', p1_position,
                                            'p2', p2_position,
                                            'state','game-over',
                                            'result', result])[0])
        # update player 2 goal
           
        actr.set_current_model(p2)

        if p2 == winner:
            result = 'win'
        else:
            result = 'lose'

        actr.goal_focus(actr.define_chunks(['isa','play',
                                            'my-pos', 'p2',
                                            'p1', p1_position,
                                            'p2', p2_position,
                                            'state','game-over',
                                            'result', result])[0])
    elif current_player == p1:
        
        # player 1 moved so update player 2 goal
           

        actr.set_current_model(p2)

        actr.goal_focus(actr.define_chunks(['isa','play',
                                            'my-pos', 'p2',
                                            'p1', p1_position,
                                            'p2', p2_position,
                                            'state','move'])[0])
        current_player = p2

    else:
        
        # player 2 moved so update player 1
        
        actr.set_current_model(p1)

        actr.goal_focus(actr.define_chunks(['isa','play',
                                            'my-pos', 'p1',
                                            'p1', p1_position,
                                            'p2', p2_position,
                                            'state','move'])[0])
        current_player = p1
  
  
  
# play one round of the game resetting the models
# before it starts.  Player1 and player2 are the
# names of the models to run in the indicated 
# position.

def play(player1,player2):
    global p1,p2,p1_position,p2_position,game_over,current_player,next_move
  
    # make sure that both names are valid model names
  
    if (player1.lower() in (x.lower() for x in actr.mp_models())) and (player2.lower() in (x.lower() for x in actr.mp_models())):
        actr.reset()

        actr.add_command('make-move',make_move,"Model action function in simple capture game.")
    
        # create the goal chunks
    
        actr.set_current_model(player1)

        actr.define_chunks(['goal','isa','play','my-pos','p1','p1',0,'p2',5,'state','move'])
      
        actr.goal_focus('goal')
    
        actr.set_current_model(player2)

        actr.define_chunks(['goal','isa','play','my-pos','p1','p2',0,'p2',5,'state','move'])
      
        actr.goal_focus('goal')

    
        # initialize the game information

        p1 = player1
        p2 = player2
        p1_position = 0
        p2_position = 5
        game_over = False
        current_player = player1
        next_move = None
    
        while not(game_over):
            # until a move is made which breaks the run
            # or 1000 simulated seconds pass which is a forfeit
            actr.run(1000) 
            process_move(next_move)
    
        # Give them a chance to process results

        actr.run_full_time(3)
    
        actr.remove_command('make-move')

    return game_over

"""
>>> game.play('model1','model2')
     0.000  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     0.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 0
     0.050  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     0.050  ------  ------                 BREAK-EVENT
     0.050  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL PLAY0 NIL
     0.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     0.050  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.100  MODEL2  PROCEDURAL             PRODUCTION-FIRED 2-STEP
I AM AT POSITION 5
     0.100  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     0.100  ------  ------                 BREAK-EVENT
     0.100  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL PLAY0 NIL
     0.100  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.100  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.100  MODEL1  PROCEDURAL             PRODUCTION-SELECTED 1-STEP
     0.100  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.150  MODEL1  PROCEDURAL             PRODUCTION-FIRED 1-STEP
I AM AT POSITION 1
     0.150  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     0.150  ------  ------                 BREAK-EVENT
     0.150  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL PLAY1 NIL
     0.150  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.150  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.150  MODEL2  PROCEDURAL             PRODUCTION-SELECTED 2-STEP
     0.150  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.200  MODEL2  PROCEDURAL             PRODUCTION-FIRED 2-STEP
I AM AT POSITION 3
     0.200  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     0.200  ------  ------                 BREAK-EVENT
     0.200  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL PLAY1 NIL
     0.200  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL PLAY2 NIL
     0.200  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.200  MODEL2  PROCEDURAL             PRODUCTION-SELECTED RESULT
     0.200  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.200  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.200  MODEL1  PROCEDURAL             PRODUCTION-SELECTED RESULT
     0.200  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.250  MODEL2  PROCEDURAL             PRODUCTION-FIRED RESULT
I WIN
     0.250  MODEL2  PROCEDURAL             CLEAR-BUFFER GOAL
     0.250  MODEL1  PROCEDURAL             PRODUCTION-FIRED RESULT
I LOSE
     0.250  MODEL1  PROCEDURAL             CLEAR-BUFFER GOAL
     0.250  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.250  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     3.200  -       ------                 Stopped because time limit reached
'model2'
"""