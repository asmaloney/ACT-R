# Play the simple capture game with each model having its own
# window and each player plays left->right as red in its own window.

import actr

# global variables for game info

current_player = None  # name of model
p1 = None              # player1 name
p2 = None              # player2 name
game_over = None       # if true stop the game
p1_position = -1       # current player locations, 0-5
p2_position = -1
p1_window = None       # window for each
p2_window = None

p1_spaces =  [None]*6  # record the button objects for the windows
p2_spaces =  [None]*6 


def stop_a_run():
    actr.schedule_break_relative(0,priority=":min")


# The function to call when a button is pressed.
# If the current player made the move, update the state.

def pick_button(model,index):
    global spaces, p1_position, p2_position, current_player, p1_spaces, p2_spaces, game_over

    # Make sure the right player made the action
  
    if actr.current_model().lower() == current_player.lower():

        if (current_player == p1) and (index > p1_position) and ((index - p1_position) <= 2): # valid move
     
            if index >= p2_position:

                # end the run after 3 seconds

                actr.schedule_break_relative(3)
                game_over = p1

                # remove the current position

                actr.remove_items_from_exp_window(p1_window, p1_spaces[p1_position], p1_spaces[index])
                actr.remove_items_from_exp_window(p2_window, p2_spaces[5 - p1_position], p2_spaces[5 - index])

                # add the blank old one and the new one marked green to indicate a win
                # don't need to store them or provide the action function because game is over
 
                actr.add_button_to_exp_window(p1_window, x=(10 + (p1_position * 40)), y=10, height=35, width=35, color='white')
                actr.add_button_to_exp_window(p1_window, text="1", x=(10 + (index * 40)), y=10, height=35, width=35, color='green')

                # mirror image for p2 window
                actr.add_button_to_exp_window(p2_window,  x=(10 + ((5 - p1_position) * 40)), y=10, height=35, width=35, color='white')
                actr.add_button_to_exp_window(p2_window, text="2", x=(10 + ((5 - index) * 40)), y=10, height=35, width=35, color='green')
          
            else: # update the p1 position and make p2 the current player
            
              
                actr.remove_items_from_exp_window(p1_window, p1_spaces[p1_position], p1_spaces[index])
                actr.remove_items_from_exp_window(p2_window, p2_spaces[5 - p1_position], p2_spaces[5 - index],p2_spaces[5 - p2_position])

              
                # No action needed for spaces that can't be used again

                p1_spaces[p1_position] = actr.add_button_to_exp_window(p1_window, x=(10 + (p1_position * 40)), y=10, height=35, width=35, color='white')
                p1_spaces[index] = actr.add_button_to_exp_window(p1_window, text="1", x=(10 + (index * 40)), y=10, height=35, width=35, color='white')


                p2_spaces[5 - p2_position] = actr.add_button_to_exp_window(p2_window, text="1", x=(10 + ((5 - p2_position) * 40)), y=10, height=35, width=35, color='red')
                p2_spaces[5 - p1_position] = actr.add_button_to_exp_window(p2_window,  x=(10 + ((5 - p1_position) * 40)), y=10, action=['pick-button',p2,p1_position], height=35, width=35, color='white')
                p2_spaces[5 - index] = actr.add_button_to_exp_window(p2_window, text="2", x=(10 + ((5 - index) * 40)), y=10, action=['pick-button',p2,index], height=35, width=35, color='white')
              
                p1_position = index
                current_player = p2 


        elif (current_player == p2) and (index < p2_position) and ((p2_position - index) <= 2): # valid move

            if index <= p1_position:

                # end the run after 3 seconds

                actr.schedule_break_relative(3)
                game_over = p2

                # remove the current position

                actr.remove_items_from_exp_window(p2_window, p2_spaces[5 - p2_position], p2_spaces[5 - index])
                actr.remove_items_from_exp_window(p1_window, p1_spaces[p2_position], p1_spaces[index])

                # add the blank old one and the new one marked green to indicate a win
                # don't need to store them or provide the action function because game is over
 
                actr.add_button_to_exp_window(p2_window, x=(10 + ((5 - p2_position) * 40)), y=10, height=35, width=35, color='white')
                actr.add_button_to_exp_window(p2_window, text="1", x=(10 + ((5 - index) * 40)), y=10, height=35, width=35, color='green')

                actr.add_button_to_exp_window(p1_window, x=(10 + (p2_position * 40)), y=10, height=35, width=35, color='white')
                actr.add_button_to_exp_window(p1_window, text="2", x=(10 + (index * 40)), y=10, height=35, width=35, color='green')

          
            else: # update the p2 position and make p1 the current player
              
                actr.remove_items_from_exp_window(p2_window, p2_spaces[5 - p2_position], p2_spaces[5 - index])
                actr.remove_items_from_exp_window(p1_window, p1_spaces[p2_position], p1_spaces[index],p1_spaces[p1_position])

              
                # No action needed for spaces that can't be used again

                p2_spaces[5 - p2_position] = actr.add_button_to_exp_window(p2_window, x=(10 + ((5 - p2_position) * 40)), y=10, height=35, width=35, color='white')
                p2_spaces[5 - index] = actr.add_button_to_exp_window(p2_window, text="1", x=(10 + ((5 - index) * 40)), y=10, height=35, width=35, color='white')


                p1_spaces[p2_position] = actr.add_button_to_exp_window(p1_window, x=(10 + (p2_position * 40)), y=10, action=['pick-button',p1,p2_position], height=35, width=35, color='white')
                p1_spaces[p1_position] = actr.add_button_to_exp_window(p1_window, text="1", x=(10 + (p1_position * 40)), y=10, action=['pick-button',p1,p1_position], height=35, width=35, color='red')
                p1_spaces[index] = actr.add_button_to_exp_window(p1_window, text="2", x=(10 + (index * 40)), y=10, action=['pick-button',p1,index], height=35, width=35, color='white')
              
                p2_position = index
                current_player = p1

# play one round of the game resetting models
# before it starts.  Player1 and player2 are the
# names of models to run.

def play(player1,player2):
    global p1, p2, game_over, safety_stop, current_player, p1_position, p2_position, p1_window , p2_window, p1_spaces, p2_spaces

    if (player1.lower() in (x.lower() for x in actr.mp_models())) and (player2.lower() in (x.lower() for x in actr.mp_models())):
    
        actr.reset()
    
        # initialize game info

        p1 = player1
        p2 = player2
        game_over = False
        current_player = player1
        p1_position = 0
        p2_position = 5
    
        # open a window for each model and the safety stop button

        p1_window = actr.open_exp_window("player1",visible=True,width=300,height=100,x=100,y=100)
        p2_window = actr.open_exp_window("player2",visible=True,width=300,height=100,x=450,y=100)
        safety_window = actr.open_exp_window('Safety',visible=True,height=100,width=100,x=100,y=300)
      
        actr.add_command('stop-a-run',stop_a_run,'Set the flag to terminate the game.')
        actr.add_command('pick-button',pick_button,'Button action function.')

        actr.add_button_to_exp_window(safety_window, text="STOP", x=0, y=0, action='stop-a-run', height=80, width=80, color='red')

        for i in range(6):

            if i == 0:
                b = "1"
                p1_c = 'red'
            elif i == 5:
                b = "2"
                p1_c = 'white'
            else:
                b = ""
                p1_c = 'white'

            p1_spaces[i] = actr.add_button_to_exp_window(p1_window, text=b, x=(10 + (i * 40)), y=10, action=['pick-button',p1,i], height=35, width=35, color=p1_c)
            p2_spaces[i] = actr.add_button_to_exp_window(p2_window, text=b, x=(10 + (i * 40)), y=10, action=['pick-button',p2,(5 - i)], height=35, width=35, color='white')

        actr.set_current_model(p1)
        actr.install_device(p1_window)

        actr.set_current_model(p2)
        actr.install_device(p2_window)

        actr.run(10000,True)

        actr.remove_command('stop-a-run')
        actr.remove_command('pick-button')

    return game_over  
 
"""
>>> game.play('model1','model2')
     0.000  MODEL1  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL2  GOAL                   SET-BUFFER-CHUNK GOAL GOAL NIL
     0.000  MODEL1  VISION                 PROC-DISPLAY
     0.000  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0 NIL
     0.000  MODEL1  VISION                 visicon-update
     0.000  MODEL2  VISION                 PROC-DISPLAY
     0.000  MODEL2  VISION                 visicon-update
     0.000  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.000  MODEL1  PROCEDURAL             PRODUCTION-SELECTED MY-TURN
     0.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.000  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL1  PROCEDURAL             PRODUCTION-FIRED MY-TURN
     0.050  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.050  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.050  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050  MODEL1  VISION                 Find-location
     0.050  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION2
     0.050  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.050  MODEL1  PROCEDURAL             PRODUCTION-SELECTED MOVE
     0.050  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.050  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.050  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.100  MODEL1  PROCEDURAL             PRODUCTION-FIRED MOVE
     0.100  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.100  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     0.100  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.100  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     0.100  MODEL1  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION2-0
     0.100  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.300  MODEL1  MOTOR                  PREPARATION-COMPLETE
     0.300  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.350  MODEL1  MOTOR                  INITIATION-COMPLETE
     0.350  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.590  MODEL1  MOUSE                  move-cursor MODEL1 mouse (168 128 1080)
     0.590  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.640  MODEL1  MOTOR                  FINISH-MOVEMENT
     0.640  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.640  MODEL1  PROCEDURAL             PRODUCTION-SELECTED CLICK
     0.640  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.640  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.690  MODEL1  PROCEDURAL             PRODUCTION-FIRED CLICK
     0.690  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.690  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     0.690  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     0.690  MODEL1  MOTOR                  CLICK-MOUSE
     0.690  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.840  MODEL1  MOTOR                  PREPARATION-COMPLETE
     0.840  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.890  MODEL1  MOTOR                  INITIATION-COMPLETE
     0.890  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.900  MODEL1  MOUSE                  click-mouse MODEL1 (168 128 1080) INDEX
     0.900  MODEL1  VISION                 PROC-DISPLAY
     0.900  MODEL1  VISION                 visicon-update
     0.900  MODEL2  VISION                 PROC-DISPLAY
     0.900  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION8 NIL
     0.900  MODEL2  VISION                 visicon-update
     0.900  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     0.900  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.900  MODEL2  PROCEDURAL             PRODUCTION-SELECTED MY-TURN
     0.900  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.900  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.950  MODEL2  PROCEDURAL             PRODUCTION-FIRED MY-TURN
     0.950  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     0.950  MODEL2  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     0.950  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.950  MODEL2  VISION                 Find-location
     0.950  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION3
     0.950  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     0.950  MODEL2  PROCEDURAL             PRODUCTION-SELECTED MOVE
     0.950  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     0.950  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     0.950  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     0.990  MODEL1  MOTOR                  FINISH-MOVEMENT
     0.990  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.000  MODEL2  PROCEDURAL             PRODUCTION-FIRED MOVE
     1.000  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.000  MODEL2  PROCEDURAL             MODULE-REQUEST MANUAL
     1.000  MODEL2  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.000  MODEL2  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.000  MODEL2  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION3-0
     1.000  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.200  MODEL2  MOTOR                  PREPARATION-COMPLETE
     1.200  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.250  MODEL2  MOTOR                  INITIATION-COMPLETE
     1.250  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.651  MODEL2  MOUSE                  move-cursor MODEL2 mouse (558 128 1080)
     1.651  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.701  MODEL2  MOTOR                  FINISH-MOVEMENT
     1.701  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.701  MODEL2  PROCEDURAL             PRODUCTION-SELECTED CLICK
     1.701  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.701  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     1.751  MODEL2  PROCEDURAL             PRODUCTION-FIRED CLICK
     1.751  MODEL2  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     1.751  MODEL2  PROCEDURAL             MODULE-REQUEST MANUAL
     1.751  MODEL2  PROCEDURAL             CLEAR-BUFFER MANUAL
     1.751  MODEL2  MOTOR                  CLICK-MOUSE
     1.751  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.901  MODEL2  MOTOR                  PREPARATION-COMPLETE
     1.901  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.951  MODEL2  MOTOR                  INITIATION-COMPLETE
     1.951  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.961  MODEL2  MOUSE                  click-mouse MODEL2 (558 128 1080) INDEX
     1.961  MODEL2  VISION                 PROC-DISPLAY
     1.961  MODEL2  VISION                 visicon-update
     1.961  MODEL1  VISION                 PROC-DISPLAY
     1.961  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION12 NIL
     1.961  MODEL1  VISION                 visicon-update
     1.961  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     1.961  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     1.961  MODEL1  PROCEDURAL             PRODUCTION-SELECTED MY-TURN
     1.961  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     1.961  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.011  MODEL1  PROCEDURAL             PRODUCTION-FIRED MY-TURN
     2.011  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.011  MODEL1  PROCEDURAL             MODULE-REQUEST VISUAL-LOCATION
     2.011  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.011  MODEL1  VISION                 Find-location
     2.011  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION14
     2.011  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.011  MODEL1  PROCEDURAL             PRODUCTION-SELECTED MOVE
     2.011  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.011  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.011  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.051  MODEL2  MOTOR                  FINISH-MOVEMENT
     2.051  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.061  MODEL1  PROCEDURAL             PRODUCTION-FIRED MOVE
     2.061  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.061  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     2.061  MODEL1  PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.061  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     2.061  MODEL1  MOTOR                  MOVE-CURSOR LOC VISUAL-LOCATION14-0
     2.061  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.261  MODEL1  MOTOR                  PREPARATION-COMPLETE
     2.261  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.311  MODEL1  MOTOR                  INITIATION-COMPLETE
     2.311  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.459  MODEL1  MOUSE                  move-cursor MODEL1 mouse (248 128 1080)
     2.459  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.509  MODEL1  MOTOR                  FINISH-MOVEMENT
     2.509  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.509  MODEL1  PROCEDURAL             PRODUCTION-SELECTED CLICK
     2.509  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.509  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION MANUAL
     2.559  MODEL1  PROCEDURAL             PRODUCTION-FIRED CLICK
     2.559  MODEL1  PROCEDURAL             MOD-BUFFER-CHUNK GOAL
     2.559  MODEL1  PROCEDURAL             MODULE-REQUEST MANUAL
     2.559  MODEL1  PROCEDURAL             CLEAR-BUFFER MANUAL
     2.559  MODEL1  MOTOR                  CLICK-MOUSE
     2.559  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.709  MODEL1  MOTOR                  PREPARATION-COMPLETE
     2.709  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.759  MODEL1  MOTOR                  INITIATION-COMPLETE
     2.759  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.769  MODEL1  MOUSE                  click-mouse MODEL1 (248 128 1080) INDEX
     2.769  MODEL1  VISION                 PROC-DISPLAY
     2.769  MODEL1  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION17 NIL
     2.769  MODEL1  VISION                 visicon-update
     2.769  MODEL2  VISION                 PROC-DISPLAY
     2.769  MODEL2  VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION17 NIL
     2.769  MODEL2  VISION                 visicon-update
     2.769  MODEL1  PROCEDURAL             CONFLICT-RESOLUTION
     2.769  MODEL1  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.769  MODEL1  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.769  MODEL1  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.769  MODEL1  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.769  MODEL2  PROCEDURAL             CONFLICT-RESOLUTION
     2.769  MODEL2  PROCEDURAL             PRODUCTION-SELECTED GAME-OVER
     2.769  MODEL2  PROCEDURAL             BUFFER-READ-ACTION GOAL
     2.769  MODEL2  PROCEDURAL             BUFFER-READ-ACTION VISUAL-LOCATION
     2.769  MODEL2  PROCEDURAL             QUERY-BUFFER-ACTION VISUAL
     2.772  ------  ------                 BREAK-EVENT
'model1'
"""
