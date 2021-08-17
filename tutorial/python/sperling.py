import actr

actr.load_act_r_model("ACT-R:tutorial;unit3;sperling-model.lisp")

responses = []
show_responses = True

exp_data = [3.03,2.4,2.03,1.5]

def trial(onset_time):
   
    actr.reset()

    letters = actr.permute_list(["B","C","D","F","G","H","J",
                                 "K","L","M","N","P","Q","R",
                                 "S","T","V","W","X","Y","Z"])
    answers = []
    row = actr.random(3)
    window = actr.open_exp_window("Sperling Experiment", visible=True)

    for i in range(3):
        for j in range(4):
            txt = letters[j + (i * 4)]
            if i == row:
                answers.append(txt)
            actr.add_text_to_exp_window(window, txt, x=(75 + (j * 50)), y=(100 + (i * 50)))

    actr.install_device(window)

    if row == 0:
        freq = 2000
    elif row == 1:
        freq = 1000
    else:
        freq = 500

    actr.new_tone_sound(freq,.5,onset_time)
    actr.schedule_event_relative(900 + actr.random(200),
                                        "clear-exp-window",
                                        params=[window],time_in_ms=True)

    global responses
    responses = []

    actr.add_command("sperling-response",respond_to_key_press,
                     "Sperling task key press response monitor")
    actr.monitor_command("output-key","sperling-response")

    actr.run(30,True)    

    actr.remove_command_monitor("output-key","sperling-response")
    actr.remove_command("sperling-response")

    if show_responses:
        print("answers: %s"%answers)
        print("responses: %s"%responses)

    return(compute_score(answers))


def compute_score(answers):

    score = 0
 
    for s in responses:
        if s.upper() in answers:
            score += 1

    return(score)

def respond_to_key_press (model,key):
    global responses

    if not(key.lower() == "space"):
        responses.append(key)

def report_data(data):

    actr.correlation(data,exp_data)
    actr.mean_deviation(data,exp_data)
    print_results (data)

def print_results(data):

    print("Condition    Current Participant   Original Experiment")
    for (c,d,o) in zip([0.0,0.15,0.3,1.0],data,exp_data):
        print(" %4.2f sec.          %6.2f                %6.2f"%(c,d,o))


def one_block():

    result = []

    for t in actr.permute_list([0.0,.15,.3,1.0]):
        result.append((t,trial(t)))

    result.sort()
    return (list(map(lambda x: x[1],result)))

def experiment(n):
    results=[0,0,0,0]

    for i in range(n):
        results=list(map(lambda x,y: x + y,results,one_block()))

    report_data(list(map(lambda x: x/n,results)))
    
