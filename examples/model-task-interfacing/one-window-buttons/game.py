# Play the simple capture game in 1 window with buttons for interaction.
# Buttons highlight in red or blue to indicate player turn.
# Set player goal chunks with color, and also set their 
# mouse cursor color to match.
# When updating the buttons just remove them and add a replacement.

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

spaces =  [None]*6 # record the button objects


def set_game_over(player):
    global game_over

    game_over = player

def stop_a_run():
    global safety_stop
    
    safety_stop = True


def is_game_over(time):
    return (safety_stop or game_over or (time > 1000000))


# The function to call when a button is pressed.
# If the current player made the move, update the state.

def pick_button(index):
    global spaces, p1_position, p2_position, current_player

    # Make sure the right player made the action
  
    if actr.current_model().lower() == current_player.lower():
    
        if (current_player == p1) and (index > p1_position) and ((index - p1_position) <= 2):

            actr.remove_items_from_exp_window(window, spaces[p1_position], spaces[index])
          
            # old space is now white and blank
           
            spaces[p1_position] = actr.add_button_to_exp_window(window, x=(10 + (p1_position * 40)), y=10, action=['pick-button',p1_position], height=35, width=35, color='white')
          
            if index >= p2_position:

                # set the game over after 3 seconds
                actr.schedule_event_relative(3,'set_game_over',params=[p1])
               
                # new space is green to indicate a win
               
                spaces[index]= actr.add_button_to_exp_window(window, text="1",  x=(10 + (index * 40)), y=10, action=['pick-button',index], height=35, width=35, color='green')
          
            else: # update the p1 position and make p2 the current player
            
                # p1 position is white
            
                spaces[index]= actr.add_button_to_exp_window(window, text="1",  x=(10 + (index * 40)), y=10, action=['pick-button',index], height=35, width=35, color='white')
                 
                # set p2 position to be blue
            
                actr.remove_items_from_exp_window(window, spaces[p2_position])
 
                spaces[p2_position]= actr.add_button_to_exp_window(window, text="2",  x=(10 + (p2_position * 40)), y=10, action=['pick-button',p2_position], height=35, width=35, color='blue')
            
            # update position and player
            
            p1_position = index
            current_player = p2
    
        elif (current_player == p2) and (index < p2_position) and ((p2_position - index) <= 2): # if p2 makes a valid move
          
            actr.remove_items_from_exp_window(window, spaces[p2_position], spaces[index])
          
            # old space is now white and blank
           
            spaces[p2_position] = actr.add_button_to_exp_window(window,  x=(10 + (p2_position * 40)), y=10, action=['pick-button',p2_position], height=35, width=35, color='white')
          
            if index <= p1_position:

                # set the game over after 3 seconds
                actr.schedule_event_relative(3,'set_game_over',params=[p2])
               
                # new space is green to indicate a win
               
                spaces[index]= actr.add_button_to_exp_window(window, text="2",  x=(10 + (index * 40)), y=10, action=['pick-button',index], height=35, width=35, color='green')
          
            else: # update the p2 position and make p2 the current player
            
                # p2 position is white
            
                spaces[index]= actr.add_button_to_exp_window(window, text="2",  x=(10 + (index * 40)), y=10, action=['pick-button',index], height=35, width=35, color='white')
                 
                # set p1 position to be red
            
                actr.remove_items_from_exp_window(window, spaces[p1_position])
 
                spaces[p1_position]= actr.add_button_to_exp_window(window, text="1",  x=(10 + (p1_position * 40)), y=10, action=['pick-button',p1_position], height=35, width=35, color='red')
            
            # update position and player
            
            p2_position = index
            current_player = p1


# play one round of the game resetting the models before it starts.
        
def play(player1,player2):
    global p1, p2, game_over, safety_stop, current_player, p1_position, p2_position, window  

    if (player1.lower() in (x.lower() for x in actr.mp_models())) and (player2.lower() in (x.lower() for x in actr.mp_models())):
    
        actr.reset()
    
        # initialize game info

        p1 = player1
        p2 = player2
        game_over = False
        safety_stop = False
        current_player = player1
        p1_position = 0
        p2_position = 5
    
        window = actr.open_exp_window("game",visible=True,width=300,height=100)
        safety_window = actr.open_exp_window('Safety',visible=True,height=100,width=100,x=100,y=100)
      
        actr.add_command('stop-a-run',stop_a_run,'Set the flag to terminate the game.')
        actr.add_command('pick-button',pick_button,'Button action function.')

        actr.add_button_to_exp_window(safety_window, text="STOP",  x=0, y=0, action='stop-a-run',  height=80, width=80, color='red')
      
        for m in actr.mp_models():
            actr.set_current_model(m)
            actr.install_device(window)
      
        for i in range(6):
            if i == 0:
                b = "1"
                c = 'red'
            elif i == 5:
                b = "2"
                c = 'white'
            else:
                b = ""
                c = 'white'

            spaces[i] = actr.add_button_to_exp_window(window, text=b, x=10 + (i * 40), y=10, action=['pick-button',i], height=35, width=35, color=c)
    
    
        actr.add_command('is-game-over',is_game_over,'Test whether game should stop running.')
        actr.add_command('set_game_over',set_game_over,'Set the flag to stop the game.')

        # Run ACT-R until the game is over
    
        actr.run_until_condition('is-game-over')
    
        actr.remove_command('stop-a-run')
        actr.remove_command('pick-button')
        actr.remove_command('is-game-over')
        actr.remove_command('set_game_over')

    return game_over


"""
>>> game.play ('red-player','blue-player')
     0.000  RED-PLAYER   GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  BLUE-PLAYER  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  BLUE-PLAYER  VISION                 proc-display
     0.000  BLUE-PLAYER  VISION                 visicon-update
     0.000  RED-PLAYER   VISION                 proc-display
     0.000  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0 NIL
     0.000  RED-PLAYER   VISION                 visicon-update
     0.000  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.000  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-LEFT
     0.000  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.000  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-LEFT
     0.050  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050  RED-PLAYER   VISION                 Find-location
     0.050  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION2
     0.050  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.050  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MOVE
     0.050  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.100  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MOVE
     0.100  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     0.100  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.100  RED-PLAYER   MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION2-0
     0.100  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.300  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE 0.1
     0.300  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.350  RED-PLAYER   MOTOR                  INITIATION-COMPLETE 0.1
     0.350  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.694  RED-PLAYER   MOUSE                  move-cursor RED-PLAYER mouse (368 328 1080)
     0.694  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.744  RED-PLAYER   MOTOR                  FINISH-MOVEMENT 0.1
     0.744  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.744  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED CLICK
     0.744  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.744  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.794  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED CLICK
     0.794  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.794  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     0.794  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.794  RED-PLAYER   MOTOR                  CLICK-MOUSE
     0.794  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.944  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE 0.794
     0.944  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     0.994  RED-PLAYER   MOTOR                  INITIATION-COMPLETE 0.794
     0.994  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.004  RED-PLAYER   MOUSE                  click-mouse RED-PLAYER (368 328 1080) INDEX
     1.004  BLUE-PLAYER  VISION                 proc-display
     1.004  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION11 NIL
     1.004  BLUE-PLAYER  VISION                 visicon-update
     1.004  RED-PLAYER   VISION                 proc-display
     1.004  RED-PLAYER   VISION                 visicon-update
     1.004  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.004  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.004  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-RIGHT
     1.004  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.004  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     1.054  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-RIGHT
     1.054  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.054  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     1.054  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.054  BLUE-PLAYER  VISION                 Find-location
     1.054  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION5
     1.054  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.054  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED MOVE
     1.054  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.054  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     1.054  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.094  RED-PLAYER   MOTOR                  FINISH-MOVEMENT 0.794
     1.094  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     1.104  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED MOVE
     1.104  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.104  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST MANUAL
     1.104  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.104  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.104  BLUE-PLAYER  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION5-0
     1.104  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.304  BLUE-PLAYER  MOTOR                  PREPARATION-COMPLETE 1.104
     1.304  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.354  BLUE-PLAYER  MOTOR                  INITIATION-COMPLETE 1.104
     1.354  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.736  BLUE-PLAYER  MOUSE                  move-cursor BLUE-PLAYER mouse (488 328 1080)
     1.736  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.786  BLUE-PLAYER  MOTOR                  FINISH-MOVEMENT 1.104
     1.786  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.786  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED CLICK
     1.786  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.786  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.836  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED CLICK
     1.836  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.836  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST MANUAL
     1.836  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.836  BLUE-PLAYER  MOTOR                  CLICK-MOUSE
     1.836  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     1.986  BLUE-PLAYER  MOTOR                  PREPARATION-COMPLETE 1.836
     1.986  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.036  BLUE-PLAYER  MOTOR                  INITIATION-COMPLETE 1.836
     2.036  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.046  BLUE-PLAYER  MOUSE                  click-mouse BLUE-PLAYER (488 328 1080) INDEX
     2.046  BLUE-PLAYER  VISION                 proc-display
     2.046  BLUE-PLAYER  VISION                 visicon-update
     2.046  RED-PLAYER   VISION                 proc-display
     2.046  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION16 NIL
     2.046  RED-PLAYER   VISION                 visicon-update
     2.046  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.046  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.046  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-LEFT
     2.046  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.046  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.096  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-LEFT
     2.096  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.096  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.096  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.096  RED-PLAYER   VISION                 Find-location
     2.096  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION4
     2.096  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.096  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED MOVE
     2.096  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.096  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.096  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.136  BLUE-PLAYER  MOTOR                  FINISH-MOVEMENT 1.836
     2.136  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.146  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED MOVE
     2.146  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.146  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     2.146  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.146  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.146  RED-PLAYER   MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION4-0
     2.146  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.346  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE 2.146
     2.346  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.396  RED-PLAYER   MOTOR                  INITIATION-COMPLETE 2.146
     2.396  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.544  RED-PLAYER   MOUSE                  move-cursor RED-PLAYER mouse (448 328 1080)
     2.544  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.594  RED-PLAYER   MOTOR                  FINISH-MOVEMENT 2.146
     2.594  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.594  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED CLICK
     2.594  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.594  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.644  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED CLICK
     2.644  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.644  RED-PLAYER   PROCEDURAL             MODULE-REQUEST MANUAL
     2.644  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.644  RED-PLAYER   MOTOR                  CLICK-MOUSE
     2.644  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.794  RED-PLAYER   MOTOR                  PREPARATION-COMPLETE 2.644
     2.794  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.844  RED-PLAYER   MOTOR                  INITIATION-COMPLETE 2.644
     2.844  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.854  RED-PLAYER   MOUSE                  click-mouse RED-PLAYER (448 328 1080) INDEX
     2.854  BLUE-PLAYER  VISION                 proc-display
     2.854  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION21 NIL
     2.854  BLUE-PLAYER  VISION                 visicon-update
     2.854  RED-PLAYER   VISION                 proc-display
     2.854  RED-PLAYER   VISION                 visicon-update
     2.854  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.854  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.854  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED MY-TURN-FROM-RIGHT
     2.854  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.854  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.904  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED MY-TURN-FROM-RIGHT
     2.904  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.904  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.904  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.904  BLUE-PLAYER  VISION                 Find-location
     2.904  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION19
     2.904  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     2.904  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED MOVE
     2.904  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.904  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.904  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.944  RED-PLAYER   MOTOR                  FINISH-MOVEMENT 2.644
     2.944  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     2.954  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED MOVE
     2.954  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.954  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST MANUAL
     2.954  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.954  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER MANUAL
     2.954  BLUE-PLAYER  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION19-0
     2.954  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.154  BLUE-PLAYER  MOTOR                  PREPARATION-COMPLETE 2.954
     3.154  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.204  BLUE-PLAYER  MOTOR                  INITIATION-COMPLETE 2.954
     3.204  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.304  BLUE-PLAYER  MOUSE                  move-cursor BLUE-PLAYER mouse (448 328 1080)
     3.304  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.354  BLUE-PLAYER  MOTOR                  FINISH-MOVEMENT 2.954
     3.354  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.354  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED CLICK
     3.354  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.354  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     3.404  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED CLICK
     3.404  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.404  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST MANUAL
     3.404  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER MANUAL
     3.404  BLUE-PLAYER  MOTOR                  CLICK-MOUSE
     3.404  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.554  BLUE-PLAYER  MOTOR                  PREPARATION-COMPLETE 3.404
     3.554  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.604  BLUE-PLAYER  MOTOR                  INITIATION-COMPLETE 3.404
     3.604  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.614  BLUE-PLAYER  MOUSE                  click-mouse BLUE-PLAYER (448 328 1080) INDEX
     3.614  BLUE-PLAYER  VISION                 proc-display
     3.614  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION24 NIL
     3.614  BLUE-PLAYER  VISION                 visicon-update
     3.614  RED-PLAYER   VISION                 proc-display
     3.614  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION24 NIL
     3.614  RED-PLAYER   VISION                 visicon-update
     3.614  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.614  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     3.614  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.614  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     3.614  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     3.614  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     3.614  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.614  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     3.664  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     3.664  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.664  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     3.664  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.664  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED GAME-OVER
     3.664  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.664  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     3.664  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.664  BLUE-PLAYER  VISION                 Find-location
     3.664  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION25
     3.664  RED-PLAYER   VISION                 Find-location
     3.664  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION25
     3.664  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.664  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED READ
     3.664  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.664  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     3.664  BLUE-PLAYER  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     3.664  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     3.664  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED READ
     3.664  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.664  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     3.664  RED-PLAYER   PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     3.704  BLUE-PLAYER  MOTOR                  FINISH-MOVEMENT 3.404
     3.714  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED READ
     3.714  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.714  BLUE-PLAYER  PROCEDURAL             MODULE-REQUEST VISUAL
     3.714  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.714  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL
     3.714  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED READ
     3.714  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.714  RED-PLAYER   PROCEDURAL             MODULE-REQUEST VISUAL
     3.714  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     3.714  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL
     3.714  BLUE-PLAYER  VISION                 Move-attention VISUAL-LOCATION25-0 NIL
     3.714  RED-PLAYER   VISION                 Move-attention VISUAL-LOCATION25-0 NIL
     3.714  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.714  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     3.799  BLUE-PLAYER  VISION                 Encoding-complete VISUAL-LOCATION25-0 NIL
     3.799  BLUE-PLAYER  VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.799  RED-PLAYER   VISION                 Encoding-complete VISUAL-LOCATION25-0 NIL
     3.799  RED-PLAYER   VISION                 SET-BUFFER-CHUNK VISUAL TEXT0
     3.799  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.799  BLUE-PLAYER  PROCEDURAL             PRODUCTION-SELECTED YEAH
     3.799  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.799  BLUE-PLAYER  PROCEDURAL             BUFFER-READ-ACTION VISUAL
     3.799  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     3.799  RED-PLAYER   PROCEDURAL             PRODUCTION-SELECTED BOO
     3.799  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION GOAL
     3.799  RED-PLAYER   PROCEDURAL             BUFFER-READ-ACTION VISUAL
     3.849  BLUE-PLAYER  PROCEDURAL             PRODUCTION-FIRED YEAH
I WON
     3.849  BLUE-PLAYER  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.849  BLUE-PLAYER  PROCEDURAL             CLEAR-BUFFER VISUAL
     3.849  RED-PLAYER   PROCEDURAL             PRODUCTION-FIRED BOO
I LOST
     3.849  RED-PLAYER   PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     3.849  RED-PLAYER   PROCEDURAL             CLEAR-BUFFER VISUAL
     3.849  BLUE-PLAYER  PROCEDURAL             CONFLICT-RESOLUTION
     3.849  RED-PLAYER   PROCEDURAL             CONFLICT-RESOLUTION
     6.614  BLUE-PLAYER  NONE                   set_game_over blue-player
     6.614  -            ------                 Stopped because condition is true
'blue-player'
"""