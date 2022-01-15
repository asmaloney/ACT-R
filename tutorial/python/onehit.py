# ACT-R tutorial unit 5 one-hit blackjack task
#
# This file implements the one-hit blackjack game
# that is described in the unit text and allows
# one to run a model against a human opponent or
# an opponent controlled by functions created
# to play the game.  It also allows one to control
# the decks of cards that are used to provide 
# different situations for the model to learn.

# Import the actr module for tutorial tasks, math
# for the floor function, and numbers for the Number class.

import actr
import math
import numbers


# Before loading the model define the function
# that will be used to compute the similarities
# between numbers and add a command for it because
# that command name is used in the model's parameter
# settings.

def onehit_bj_number_sims(a,b):

    if isinstance(b,numbers.Number) and isinstance(a,numbers.Number):
        return (- ( abs(a - b) / max(a,b)))
    else:
        return False    

actr.add_command("1hit-bj-number-sims",onehit_bj_number_sims,
                 "Similarity between numbers for 1-hit blackjack task.")

actr.load_act_r_model("ACT-R:tutorial;unit5;1hit-blackjack-model.lisp")


# Define a lot of global variables to control the
# details of the game, a code-based opponent, 
# record responses, and keep track of whether the
# output-key action is being monitored.

deck1 = None
deck2 = None
opponent_rule = None
opponent_feedback = None
model_action = None
human_action = None
opponent_threshold = None
key_monitor_installed = False


# respond_to_keypress will be monitoring the
# output-key command and will be called when a 
# key is pressed by the model or a human playing
# the game.  It records the key in the corresponding
# variable based on who made it.

def respond_to_keypress(model,key):
    global model_action,human_action

    if model:
        model_action = key
    else:
        human_action = key

# These functions are used to create the command and monitor
# output-key and correspondingly remove the monitor and command
# when needed because it is more efficient to do so once instead
# of on each trial (as has been done for most prior tasks) since
# this will require running many trials to collect the data.

def add_key_monitor():
    global key_monitor_installed

    if key_monitor_installed == False:
        actr.add_command("1hit-bj-key-press",respond_to_keypress,
                         "1-hit blackjack task key output monitor")
        actr.monitor_command("output-key","1hit-bj-key-press")
        key_monitor_installed = True
        return True
    else:
        return False

def remove_key_monitor():

    actr.remove_command_monitor("output-key","1hit-bj-key-press")
    actr.remove_command("1hit-bj-key-press")

    global key_monitor_installed
    key_monitor_installed = False

# hands takes one required parameter which is a number of
# hands to play and an optional parameter which if specified as
# True will print out the details of the hands.
# It plays the game based on the settings of the global varaibles
# for the decks of cards and opponent functions and returns the
# list of results.

def hands(hands,print_game=False):

    scores = [0,0,0,0]
    need_to_remove = add_key_monitor()

    for i in range(hands):
        mcards = deal(deck1)
        ocards = deal(deck2)
        mchoice = show_model_cards(mcards[0:2],ocards[0])
        ochoice = show_opponent_cards(ocards[0:2],mcards[0])

        if not(mchoice.lower() == 'h'):
            mcards = mcards[0:2]

        if not(ochoice.lower() == 'h'):
            ocards = ocards[0:2]

        mtot = score_cards(mcards)
        otot = score_cards(ocards)
        mres = compute_outcome(mcards,ocards)
        ores = compute_outcome(ocards,mcards)

        show_model_results(mcards,ocards,mres,ores)
        show_opponent_results(ocards,mcards,ores,mres)

        if print_game:
            print("Model: ",end="")
            for c in mcards:
                print("%2d "%c,end="")
            print(" -> %2d (%-4s)   Opponent: "%(mtot,mres),end="")
            for c in ocards:
                print("%2d "%c,end="")
            print("-> %2d (%-4s)"%(otot,ores))

        if mres == 'win':
            scores[0] += 1
        if ores == 'win':
            scores[1] += 1
        if mres == 'bust' and ores == 'bust':
            scores[2] += 1
        if mtot == otot and not(mres == 'bust') and not(ores == 'bust'):
            scores[3] += 1

    if need_to_remove:
        remove_key_monitor()

    return scores

# blocks takes two required parameters which are how many 
# blocks of hands to play and how many hands are in a block.
# It plays the specified number of blocks and returns the list
# of results by block.

def blocks(blocks,block_size):
    
    res = []
    need_to_remove = add_key_monitor()

    for i in range(blocks):
        res.append(hands(block_size))

    if need_to_remove:
        remove_key_monitor()

    return res

# game0 function sets the global variables to configure
# the default game -- the regular distribution of cards
# in a deck and an opponent which has a fixed threshold
# of 15 for deciding whether to hit or stay and which does
# not process the feedback.

def game0():
    global deck1,deck2,opponent_threshold,opponent_rule,opponent_feedback

    deck1 = regular_deck
    deck2 = regular_deck
    opponent_rule = fixed_threshold
    opponent_threshold = 15
    opponent_feedback = None

# sum_lists is used to add results lists
# together for the data collection below

def sum_lists(x,y):
    return list(map(lambda v,w: v + w,x,y))


# learning requires one paramter which is how many 100 hand
# games to play.  There are two optional paramters which
# indicate whether a graph of the results should be drawn
# in an experiment window (default is t which draws it)
# and to specify a function to use to set the variables that
# configure the game play (the default is game0).
# It returns a list with the average win percentages from
# the n games in both blocks of 20 and blocks of 5 hands.

def learning(n,graph=True,game=game0):

    data = [[0,0,0,0]]*20
    need_to_remove = add_key_monitor()

    for i in range(n):
        actr.reset()
        game()
        data = list(map(lambda x,y: sum_lists(x,y),data,blocks(20,5)))

    if need_to_remove:
        remove_key_monitor()

    percentages = list(map(lambda x: x[0] / n / 5,data))

    if graph:
        draw_graph(percentages)

    return [[sum(percentages[0:5])/5,
             sum(percentages[5:10])/5,
             sum(percentages[10:15])/5,
             sum(percentages[15:20])/5],
             percentages]
    
# draw_graph takes a list of percentages and displays them in
# a graph using an experiment window for output.

def draw_graph(points):

    w = actr.open_exp_window('Data',visible=True,width=550,height=460)
    actr.add_line_to_exp_window(w,[50,0],[50,420],'white')

    for i in range(11):
        actr.add_text_to_exp_window(w, "%3.1f"%(1.0 - (i * .1)),
                                    x=5, y=(5 + (i * 40)), width=35)
        actr.add_line_to_exp_window(w,[45,10 + (i * 40)],
                                    [550,10 + (i * 40)],'white')

    x = 50

    for (a,b) in zip(points[0:-1],points[1:]):
        actr.add_line_to_exp_window(w,[x,math.floor(410 - (a * 400))],
                                    [x+25,math.floor(410 - (b * 400))],
                                    'blue')
        x += 25


# deal takes a deck function and returns a list of
# the next three cards that it returns when called.

def deal(deck):
    return [deck(),deck(),deck()]


# score_cards takes a list of cards and an optional value
# indicating the number over which a hand busts (defaults to 21).
# It returns the total value of those cards treating 1s as 11 
# if possible without busting.

def score_cards(cards,bust=21):

    total = sum(cards)
    
    for i in range(cards.count(1)):
        if (total + 10) <= bust:
            total += 10

    return total

# compute_outcome takes a list of cards for each player and an
# optional value indicating the number over which a hand busts.
# It computes the total for each hand of cards and returns the
# result (win, lose, or bust) for the first list of cards.

def compute_outcome(p1cards,p2cards,bust=21):
    p1tot = score_cards(p1cards,bust)
    p2tot = score_cards(p2cards,bust)
    if p1tot > bust:
        return 'bust'
    elif p2tot > bust:
        return 'win'
    elif p1tot > p2tot:
        return 'win'
    else:
        return 'lose'

# show_model_cards takes two parameters. The first is a list of
# the model's starting cards and the second is the opponent's face
# up card.  If there is a chunk in the model's goal buffer it is
# modified to the initial state of the game.  If there is not a
# chunk in the goal buffer then a new chunk is created and placed
# into the buffer.  Then the model is run for exactly 10 seconds
# and any response it made is returned.

def show_model_cards(mcards,ocard):

    if actr.buffer_read('goal'):
        actr.mod_focus('mc1',mcards[0],'mc2', mcards[1],'mc3',None,'mtot',None,
                       'mstart',score_cards(mcards),'mresult',None,'oc1',ocard,
                       'oc2',None,'oc3',None,'otot',None,'ostart',score_cards([ocard]),
                       'oresult',None,'state','start')
    else:
        actr.goal_focus(actr.define_chunks(['isa','game-state','mc1',mcards[0],
                                            'mc2', mcards[1],'mstart',score_cards(mcards),
                                            'oc1', ocard,'ostart',score_cards([ocard]),
                                            'state','start'])[0])

    global model_action
    model_action = 's'
    actr.run_full_time(10)
    return model_action

# show_model_results takes four parameters. The first is a list of
# the model's final cards and the second is the list of the opponent's
# final cards.  The third is the model's end result and the fourth is
# the opponents end result. 
# If there is a chunk in the model's goal buffer it is modified to
# the results state of the game with all the information.  If there
# is not a chunk in the goal buffer then a new chunk is created with
# the results information and placed into the buffer.  Then the model
# is run for exactly 10 seconds.

def show_model_results(mcards,ocards,mres,ores):

    if len(mcards) ==3:
        mhit = mcards[2]
    else:
        mhit = 'nil'

    if len(ocards) ==3:
        ohit = ocards[2]
    else:
        ohit = 'nil'

    if actr.buffer_read('goal'):
        actr.mod_focus('mc1',mcards[0],'mc2', mcards[1],'mc3',mhit,'mtot',score_cards(mcards),
                       'mstart',score_cards(mcards[0:2]),'mresult',mres,'oc1',ocards[0],
                       'oc2',ocards[1],'oc3',ohit,'otot',score_cards(ocards),
                       'ostart',score_cards(ocards[0:1]),'oresult',ores,'state','results')
    else:
        actr.goal_focus(actr.define_chunks(['isa','game-state','mc1',mcards[0],'mc2',mcards[1],
                                            'mc3',mhit,'mtot',score_cards(mcards),
                                            'mstart',score_cards(mcards[0:2]),'mresult',mres,
                                            'oc1',ocards[0],'oc2',ocards[1],'oc3',ohit,
                                            'otot',score_cards(ocards),
                                            'ostart',score_cards(ocards[0:1]),'oresult',ores,
                                            'state','results'])[0])

    actr.run_full_time(10)


# play_human takes two parameters. The first is the list of
# the player's cards and the other is the model's face up card.
# It opens an experiment window to display that information to
# a person and waits exactly 10 seconds before continuing the
# game. It returns the key press the player made, or 's' (stay)
# if no key was pressed.

def play_human(cards,oc1):

    win = actr.open_exp_window('Human')
    actr.add_text_to_exp_window(win, 'You', x=50, y=20)
    actr.add_text_to_exp_window(win, 'Model', x=200, y=20)
    
    for i in range(2):
        for j in range(3):
            actr.add_text_to_exp_window(win, "C%d"%(j+1), x=(25 + (j * 30) + (i * 150)), y=40, width=20)
            if i == 0 and j < 2:
                actr.add_text_to_exp_window(win, str(cards[j]), x=(25 + (j * 30) + (i * 150)), y=60, width=20)
            if i == 1 and j == 0:
                actr.add_text_to_exp_window(win, str(oc1), x=(25 + (j * 30) + (i * 150)), y=60, width=20)
             
    global human_action
    human_action = None

    start_time = actr.get_time(False)

    while (actr.get_time(False) - start_time) < 10000:
        actr.process_events()

    if human_action:
        return human_action
    else:
        return 's'

# show_human_results takes four parameters. The first is a list of
# the player's final cards and the second is the list of the model's
# final cards.  The third is the player's end result and the fourth is
# the model's end result. 
# All of the cards and outcomes are displayed in an experiment
# window and it waits 10 seconds before continuing.

def show_human_results(own_cards,others_cards,own_result,others_result):

    win = actr.open_exp_window('Human')
    actr.add_text_to_exp_window(win, 'You', x=50, y=20)
    actr.add_text_to_exp_window(win, 'Model', x=200, y=20)
    
    for i in range(2):
        for j in range(3):
            actr.add_text_to_exp_window(win, "C%d"%(j+1), x=(25 + (j * 30) + (i * 150)), y=40, width=20)
            if i == 0:
                if j < len(own_cards):
                    actr.add_text_to_exp_window(win, str(own_cards[j]),
                                                x=(25 + (j * 30) + (i * 150)), y=60, width=20)
            else:
                if j < len(others_cards):
                    actr.add_text_to_exp_window(win, str(others_cards[j]),
                                                x=(25 + (j * 30) + (i * 150)), y=60, width=20)
             
    actr.add_text_to_exp_window(win, own_result, x=50, y=85)
    actr.add_text_to_exp_window(win, others_result, x=200, y=85)

    start_time = actr.get_time(False)

    while (actr.get_time(False) - start_time) < 10000:
        actr.process_events()

# play_against_model requries one parameter which is how many
# hands to play and an optional parameter indicating whether 
# the hand information should be printed.  It sets the global
# variables to those needed to have a person play against the
# model and then runs for the indicated number of hands.  After
# that, it sets the variables back to the values they had
# before.

def play_against_model(count,print_games=False):
    global opponent_rule,opponent_feedback

    if actr.visible_virtuals_available():
        old_rule = opponent_rule
        old_feedback = opponent_feedback

        opponent_rule = play_human
        opponent_feedback = show_human_results
    
        result = ()
        try: 
            result = hands(count,print_games)
        finally:
            opponent_rule = old_rule
            opponent_feedback = old_feedback

        return(result)
    else:
        actr.print_warning("Cannot play against the model without a visible window available.")

# show_opponent_cards and show_opponent_results are used
# by the game code to call the appropriate function for
# the non-model player to receive the game information.

def show_opponent_cards(ocards,mc1):
    return opponent_rule(ocards,mc1)

def show_opponent_results(ocards,mcards,ores,mres):
    if opponent_feedback:
        opponent_feedback(ocards,mcards,ores,mres)


# The functions below are used to create the game0 and game1
# situations.


# regular_deck takes no parameters and returns a number 
# between 1 and 10 with 10s being 4 times as likely as 
# other numbers.  This is used as the deck function for
# both players in game0.

def regular_deck():
    return min(10,actr.random(13)+1)


# fixed_threshold implements a rule for an opponent 
# that will always hit below a fixed threshold. It
# is used in game0 for the opponent.

def fixed_threshold(cards,mc1):

    if score_cards(cards) < opponent_threshold:
        return 'h'
    else:
        return 's'

# always_hit implements a rule for an opponent
# that will always hit. It is used for the opponent
# in game1. 

def always_hit(cards,mc1):
    return 'h'

# Create a variable for a list of cards, and
# a function that will place the 6 cards onto 
# that list for the player decks (the first 3
# cards are the model's and the next 3 are for
# the opponent). That deck function is used for
# both players and represents the situation in
# game1 where the opponent's face up card is
# a perfect predictor for the action the model
# needs to take to win.

card_list = []

def load_stacked_deck():
    c1 = 5 + actr.random(6)
    c2 = 7 + actr.random(4)
    if actr.random(1.0) > .5:
        c4 = 2
    else:
        c4 = 8
    if c4 == 2:
        c3 = 10
        c6 = 10
    else:
        c3 = 21 - (c1 + c2)
        c6 = 2
    c5 = 10

    return [c1,c2,c3,c4,c5,c6]

def stacked_deck():
    global card_list

    if len(card_list) == 0:
        card_list = load_stacked_deck()
       
    c = card_list[0]
    card_list = card_list[1:]

    return c
  
# function to set variables to the values needed to
# implement game1
  
def game1():
    global deck1,deck2,opponent_threshold,opponent_rule,opponent_feedback,card_list

    card_list = []
    deck1 = stacked_deck
    deck2 = stacked_deck
    opponent_rule = always_hit
    opponent_feedback = None

# call game0 to set the initial game variable values.

game0()
