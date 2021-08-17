import actr
import math
import numbers

def onehit_bj_number_sims(a,b):

    if isinstance(b,numbers.Number) and isinstance(a,numbers.Number):
        return (- ( abs(a - b) / max(a,b)))
    else:
        return False    

actr.add_command("1hit-bj-number-sims",onehit_bj_number_sims,
                 "Similarity between numbers for 1-hit blackjack task.")

actr.load_act_r_model("ACT-R:tutorial;unit5;1hit-blackjack-model.lisp")

deck1 = None
deck2 = None
opponent_rule = None
opponent_feedback = None
model_action = None
human_action = None
opponent_threshold = None
key_monitor_installed = False

def respond_to_keypress(model,key):
    global model_action,human_action

    if model:
        model_action = key
    else:
        human_action = key



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

def blocks(blocks,block_size):
    
    res = []
    need_to_remove = add_key_monitor()

    for i in range(blocks):
        res.append(hands(block_size))

    if need_to_remove:
        remove_key_monitor()

    return res

def game0():
    global deck1,deck2,opponent_threshold,opponent_rule,opponent_feedback

    deck1 = regular_deck
    deck2 = regular_deck
    opponent_rule = fixed_threshold
    opponent_threshold = 15
    opponent_feedback = None


def sum_lists(x,y):
    return list(map(lambda v,w: v + w,x,y))

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

def deal(deck):
    return [deck(),deck(),deck()]

def score_cards(cards,bust=21):

    total = sum(cards)
    
    for i in range(cards.count(1)):
        if (total + 10) <= bust:
            total += 10

    return total

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

def show_opponent_cards(ocards,mc1):
    return opponent_rule(ocards,mc1)

def show_opponent_results(ocards,mcards,ores,mres):
    if opponent_feedback:
        opponent_feedback(ocards,mcards,ores,mres)


def regular_deck():
    return min(10,actr.random(13)+1)

def fixed_threshold(cards,mc1):

    if score_cards(cards) < opponent_threshold:
        return 'h'
    else:
        return 's'



card_list = []
   
def always_hit(cards,mc1):
    return 'h'

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
    
def game1():
    global deck1,deck2,opponent_threshold,opponent_rule,opponent_feedback,card_list

    card_list = []
    deck1 = stacked_deck
    deck2 = stacked_deck
    opponent_rule = always_hit
    opponent_feedback = None


game0()