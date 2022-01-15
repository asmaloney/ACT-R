# ACT-R tutorial unit 5 siegler task.
# This experiment presents a model with a pair of numbers aurally
# and the model must respond vocally with the sum of those numbers.
# The task and data to which the model is fit are in the paper:
#
# Siegler, R. S., & Shrager, J. (1984). Strategy choices in addition 
# and subtraction: How do children know what to do? In C. Sophian (Ed.), 
# Origins of cognitive skills (pp. 229-293). Hillsdale, NJ: Erlbaum.
#
# The original experiment was performed with 4 year-olds who made
# many errors in their responses.

# Import the actr module for tutorial tasks

import actr

# Load the corresponding model for the task.

actr.load_act_r_model("ACT-R:tutorial;unit5;siegler-model.lisp")

# Create variables for the response, to record whether the 
# monitoring function is currently available, and a subset of
# the original data for comparison.

response = False
monitor_installed = False

siegler_data = [[0, .05, .86,  0,  .02,  0, .02, 0, 0, .06],
                [0, .04, .07, .75, .04,  0, .02, 0, 0, .09],
                [0, .02, 0, .10, .75, .05, .01, .03, 0, .06],
                [.02, 0, .04, .05, .80, .04, 0, .05, 0, 0],
                [0, 0, .07, .09, .25, .45, .08, .01, .01, .06],
                [.04, 0, 0, .05, .21, .09, .48, 0, .02, .11]]


# record_model_speech will be monitoring the output-speech
# command called by the microphone device so that it can
# record the model's speech output.

def record_model_speech (model,string):
    global response
    response = string.lower()


# Because the task can be run as a single trial, or over
# larger blocks it's more efficient to only install and
# remove the monitor once for the run instead of on each
# trial as has been done in other tasks.  These functions
# are used to do that when necessary.

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

# trial takes two parameters which must be numbers.
# It resets the model and adds a microphone device to record
# the models speech output.  Then, the numbers are presented
# aurally to the model using new-digit-sound, and after 
# running the model it returns any vocal response that it made.

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

# set runs one trial for each of the addition problems
# in the data set and returns the results of those trials.

def set ():

    need_to_remove = add_speech_monitor()
    
    data = [trial(1,1),trial(1,2),trial(1,3),
            trial(2,2),trial(2,3),trial(3,3)]

    if need_to_remove:
        remove_speech_monitor()

    return data

# experiment requires one parameter which is how many sets
# of trials to run.  It runs that many trials collecting the
# responses and then passes those to analyze to compute the 
# response percentages, compare the data to the experimental 
# data, and display the results.

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