# ACT-R tutorial unit7 task for investigating production
# compilation modeling issues.

import actr

actr.load_act_r_model("ACT-R:tutorial;unit7;production-compilation-issues-model.lisp")

val1 = ''
val2 = ''
responses = []
start_time = 0
times = []
exp_length = 0
task_over = True
task_state = None
window = None

result_matrix = [[["win","lose","draw","lose"],["win","lose","draw","lose"],["win","lose","draw","lose"],["lose","draw","win","lose"]],
                 [["win","lose","draw","lose"],["win","lose","draw","lose"],["lose","draw","win","lose"],["lose","draw","win","lose"]],
                 [["win","lose","draw","lose"],["win","lose","draw","lose"],["lose","draw","win","lose"],["lose","draw","win","lose"]],
                 [["win","lose","draw","lose"],["lose","draw","win","lose"],["lose","draw","win","lose"],["lose","draw","win","lose"]]]

def convert_key_to_index(key):
    if key.lower() == 's':
        return 0
    if key.lower() == 'd':
        return 1
    if key.lower() == 'f':
        return 2
    else:
        return 3


def respond_to_key_press (model,key):
    global responses,task_over,times

    if not(task_over):
        if task_state == 'trial':
            ri = convert_key_to_index(key)
            r = result_matrix[val1][val2][ri]
            responses.append(r)
            times.append(actr.get_time() - start_time)
            present_feedback(r)
        elif key.lower() == 'space':
            if len(responses) == exp_length:
                task_over = True
            else:
                present_next_trial()

            
def present_feedback(result):
    global task_state

    task_state = 'feedback'
    actr.clear_exp_window(window)
    actr.add_text_to_exp_window(window, result, x=50, y=100)

def present_next_trial():
    global task_state,start_time,val1,val2

    task_state = 'trial'
    actr.clear_exp_window(window)
    val1 = actr.random(4)
    val2 = actr.random(4)
    actr.add_text_to_exp_window(window, str(val1), x=10, y=50)
    actr.add_text_to_exp_window(window, str(val2), x=110, y=50)
    start_time = actr.get_time()

def game_over():
    return task_over

def trials(n=150,reset=True,output=True):
    global responses,task_over,exp_length,window

    if reset:
        actr.reset()

    window = actr.open_exp_window("Compilation task",visible=False)
    times = []
    responses = []
    task_over = False
    exp_length = n
    present_next_trial()
    actr.install_device(window)

    actr.add_command('compilation-issues-response',respond_to_key_press,"Compilation issues key press response monitor")
    actr.monitor_command('output-key','compilation-issues-response')

    # Just run a long time to have the model perform the task

    actr.run(20000)

    actr.remove_command_monitor('output-key','compilation-issues-response')
    actr.remove_command ('compilation-issues-response')

    return analyze_results(output)

def game(n,show_games=False):
    scores = [0]*15
    times = [0]*15
    for i in range(n):
        r = trials(150,True,show_games)
        scores = list(map(lambda x,y: x + y,scores,r[0]))
        times = list(map(lambda x,y: x + y,times,r[1]))
    print("Average Score of %d trials"%n)
    for x in scores:
        print("%4.2f "%(x / n),end="")
    print()
    print("Average Response times")
    for x in times:
        print("%4.2f "%(x / n / 1000.0),end="")
    print()

def analyze_results(output):
    global responses,times

    r=[]
    t=[]
   
    if output:
        print("Score")

    while len(responses) > 0:
        s = 0
        c = 0
        for i in range(min(10,len(responses))):
            res = responses.pop(0)
            if res == 'win':
                s += 1
            if res == 'lose':
                s -= 1
            c += 1
        if output:
            print("  %2d"%s,end="")
        r.append(s)
    
    if output:
        print()
        print("Average response times")

    while len(times) > 0:
        s = 0
        c = 0
        for i in range(min(10,len(times))):
            s += times.pop(0)
            c += 1
        if output:
            print("%4.2f "%(s / c / 1000.0),end="")
        t.append(s/c)

    if output:
        print()

    return[r,t]

            
    