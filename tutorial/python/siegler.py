import actr

actr.load_act_r_model("ACT-R:tutorial;unit5;siegler-model.lisp")

response = False
monitor_installed = False

siegler_data = [[0, .05, .86,  0,  .02,  0, .02, 0, 0, .06],
                [0, .04, .07, .75, .04,  0, .02, 0, 0, .09],
                [0, .02, 0, .10, .75, .05, .01, .03, 0, .06],
                [.02, 0, .04, .05, .80, .04, 0, .05, 0, 0],
                [0, 0, .07, .09, .25, .45, .08, .01, .01, .06],
                [.04, 0, 0, .05, .21, .09, .48, 0, .02, .11]]


def record_model_speech (model,string):
    global response
    response = string.lower()


def add_speech_monitor():
    global monitor_installed

    if monitor_installed == False:
        actr.add_command("siegler-response",record_model_speech,"Siegler task model response")
        actr.monitor_command("output-speech","siegler-response")
        monitor_installed = True
        return True
    else:
        return False

def remove_speech_monitor():

    actr.remove_command_monitor("output-speech","siegler-response")
    actr.remove_command("siegler-response")

    global monitor_installed
    monitor_installed = False


def trial(arg1,arg2):

    actr.reset()
    actr.install_device(["speech","microphone"])
    need_to_remove = add_speech_monitor()
    actr.new_digit_sound(arg1)
    actr.new_digit_sound(arg2,.75)
    global response
    response = False
    actr.run(30)
    if need_to_remove:
        remove_speech_monitor()

    return response


def set ():

    need_to_remove = add_speech_monitor()
    
    data = [trial(1,1),trial(1,2),trial(1,3),
            trial(2,2),trial(2,3),trial(3,3)]

    if need_to_remove:
        remove_speech_monitor()

    return data

def experiment(n):
    
    add_speech_monitor()

    data = []

    for i in range(n):
        data.append(set())

    remove_speech_monitor()
    analyze(data)


def analyze(responses):

    results = [[0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0]]

    positions = {'zero':0,'one':1,'two':2,'three':3,'four':4,'five':5,
                 'six':6,'seven':7,'eight':8}


    for r in responses:
        for i in range(6):
            if r[i] in positions:
                results[i][positions[r[i]]] += 1
            else:
                results[i][9] += 1

    n = len(responses)

    for i in range(6):
        for j in range(10):
            results[i][j] /= n

    display_results(results)

def display_results(results):

    questions = ["1+1","1+2","1+3","2+2","2+3","3+3"]

    actr.correlation(results,siegler_data)
    actr.mean_deviation(results,siegler_data)


    print("       0     1     2     3     4     5     6     7     8   Other")
    for i in range(6):
        print(questions[i],end="")
        
        for j in range(10):
            print("%6.2f" % results[i][j],end="")
        print()