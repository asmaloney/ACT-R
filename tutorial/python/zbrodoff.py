# ACT-R tutorial unit 4 zbrodoff task.
# This experiment presents participants with alpha-arithmetic
# problems like "A + 2 = C" which they must respond to by
# pressing k if the problem is correct or d if it is not.
# The code runs the control condition from the paper:
#
# Zbrodoff, N. J. (1995).  Why is 9 + 7 harder than 2 + 3? 
# Strength and interference as explanations of the problem-size 
# effect.  Memory & Cognition, 23 (6), 689-700.
#
# That condition presents problems with addends of 2, 3, and 4 
# with equal frequency in blocks of 192 trials where half of the
# trials in a block are correct and half are false.  The 
# data for comparison is the average response time by block
# and addend for correct answers (including both true and
# false problems).

# Import the actr module for tutorial tasks

import actr

# Load the corresponding model for the task

actr.load_act_r_model("ACT-R:tutorial;unit4;zbrodoff-model.lisp")

# Global variables to hold the trials to present, the results that have
# been collected, and the original data for comparison.

trials = []
results = []

control_data = [1.84, 2.46, 2.82, 1.21, 1.45, 1.42, 1.14, 1.21, 1.17]

# Also create a variable to indicate whether it will be a model or person.
# This is done to keep the number of parameters needed to run the functions
# smaller since one may want a visible window for either a person or model
# and this avoids having to specify both who is running and whether the window
# should be shown.

run_model = True

# Because the data collection for this task is a little more involved
# we're going to record the trials in a class to keep everything
# together and organized instead of just a list of items as has been
# done for other experiments.  This will also allow us to store all of
# the information needed to present a trial together so that we can 
# create them all in advance which can be useful when running in an
# event-driven style.  A trial will hold the block number, the addend
# value, the text of the problem to display, the correct answer, whether
# the window should be visible or not, whether the response from the
# participant was correct, the time the trial started, and the response
# time.

class trial():
    def __init__(self,block,addend1,addend2,sum,answer,visible=None):
        self.block = block
        self.addend2 = addend2
        self.text = addend1 + " + " + addend2 + " = " + sum
        self.answer = answer.lower()
        if visible == None:
            self.visible = not(run_model)
        else:
            self.visible = visible
        self.correct = False

# The present_trial function takes one parameter which is a trial
# structure and an optional parameter which indicates whether or not
# to open a new window for this trial (since this task is running 
# continuously it will run faster if it uses the same window repeatedly,
# but because the same code is used to run it for a variety of
# different situations it needs to know when to start over with a
# new display). 

def present_trial(trial, new_window = True):

    if new_window:
        # If a new window is requested it opens one using
        # the visible status indicated in the trial and
        # if the model is performing the task it installs
        # that window device for the model.

        w = actr.open_exp_window("Alpha-arithmetic Experiment", visible=trial.visible)
        if run_model:
          actr.install_device(w)
    else:

        # otherwise it just clears the current window

        actr.clear_exp_window()

    # add the text from the trial to the window and set the
    # start time in the trial structure.

    actr.add_text_to_exp_window(None, trial.text, x=100, y=150)

    trial.start = actr.get_time(run_model)


# The respond_to_key_press function will be set up to monitor
# the output-key actions, and thus will be called with two parameters
# when a key is pressed: the name of the model that pressed the key 
# (or None if it is a person) and the string naming the key that was
# pressed. 
# Unlike the previous tasks, since this one is event-driven we will
# actually do more than just record the key and time in this function.
# It will also present the next trial if there is one so that the
# model can continue to run in the task until it is complete.

def respond_to_key_press (model,key):
    global trials,results

    # Set the response time and correctness in the trial
    # and add it to the results list.

    trials[0].time = (actr.get_time(run_model) - trials[0].start) / 1000.0

    if key.lower() == trials[0].answer :
        trials[0].correct = True

    results.append(trials[0])

    # Remove the current trial, and if there are any trials left to 
    # present then present the first of them now.

    trials = trials[1:]

    if len(trials) > 0 :
        present_trial(trials[0],False)


# The collect_responses function takes no parameters and runs all of
# the trials available.

def collect_responses():

    # record how many trials need to be run

    total = len(trials)

    # Create a command for respond_to_key_press and monitor output-key.

    actr.add_command("zbrodoff-response", respond_to_key_press,
                     "Zbrodoff task key press response monitor")
    actr.monitor_command("output-key","zbrodoff-response")

    # present the first trial

    present_trial(trials[0])

    # If it's a model doing the task run for 10s per trial,
    # and if it's a person loop until there are as many results
    # as there were trials to run.

    if run_model :
        actr.run(10 * total)
    else:
        if actr.visible_virtuals_available():
            while len(results) < total:
                actr.process_events()

    # stop monitoring and remove the command

    actr.remove_command_monitor("output-key","zbrodoff-response")
    actr.remove_command("zbrodoff-response")


# The problem function takes 4 required parameters.
# The first three are the strings of the elements of the problem
# to present e.g. "A","2","C" to preset "A + 2 = C".  The fourth
# is a string with the key press that will be a correct response
# which is "k" if the problem is correct and "d" if the problem
# is not correct.  The optional parameter can be specified as True
# to have the window displayed, but if not provided defaults to
# not showing the window.
# It clears the current results, creates a list with the single
# trial specified and runs the task for that trial and displays
# the results.

def problem(addend1,addend2,sum,answer,visible=None):

    global results
    results = []

    global trials
    trials = [trial(1,addend1,addend2,sum,answer,visible)]

    collect_responses()
    return analyze_results()

# set and block are similar to problem except that instead 
# of presenting a single trial they present a full set (24 
# trials) or block (192 trials) of items.

def set(visible=None):

    global results
    results = []

    global trials
    trials = create_set(1,visible)

    collect_responses()
    return analyze_results()

def block(visible=None):

    global results
    results = []

    global trials
    trials = []

    for i in range(8):
        trials = trials + create_set(1,visible)

    collect_responses()
    return analyze_results()


# experiment has two optioal parameters.  The first is
# whether or not to show the window which defaults to not shown,
# and the second is whether or not to display the results after
# the experiment is run (the default is to show them).
# It resets the model, generates three blocks of trials, runs
# those trials, and reports the results.

def experiment(visible=None,show=True):

    actr.reset()

    global trials
    trials = []

    for j in range(3):
        for i in range(8):
            trials = trials + create_set(j+1,visible)

    global results
    results = []

    collect_responses()
    return analyze_results(show)


# compare takes one required parameter which is the number
# of times to run a model through the full experiment.  It runs
# the model that many times and averages the results of those
# runs which it compares to the original data for the task and
# then displays the results.

def compare(n):

    rts = [0,0,0,0,0,0,0,0,0]
    counts = [0,0,0,0,0,0,0,0,0]

    for i in range(n):
        r,c = experiment(False,False)
        rts = list(map(lambda x,y: x + y,rts,r))
        counts = list(map(lambda x,y: x + y,counts,c))
    
    rts = list(map(lambda x: x/n,rts))
    counts = list(map(lambda x: x/n,counts))

    actr.correlation(rts,control_data)
    actr.mean_deviation(rts,control_data)

    print_analysis(rts,counts,[1,2,3],['2','3','4'], [192,192,192])

# analyze_results takes one optional parameter which 
# indicates whether or not to print the results in addition
# to averaging the times by addend and block and returning
# the averaged results and counts of correct items in a list.

def analyze_results(show=True):

    blocks = []
    addends = []
    data = dict()    
    totals = dict()

    for i in results:
        if i.addend2 in totals:
            totals[i.addend2] += 1
        else:
            totals[i.addend2] = 1
 
        if i.correct:
            if (i.block,i.addend2) in data:
                data[(i.block,i.addend2)].append(i.time)
            else:
                data[(i.block,i.addend2)]=[i.time]
                if i.block not in blocks:
                    blocks.append(i.block)
                if i.addend2 not in addends:
                    addends.append(i.addend2)


    blocks.sort()
    addends.sort()

    rts =[]
    counts =[]
    for b in blocks:
        for a in addends:
            rts.append(sum(data[(b,a)]) / len(data[(b,a)]))
            counts.append(len(data[(b,a)]))

    if show:
        print_analysis(rts,counts,blocks,addends,[totals[i] for i in addends])

    return (rts, counts)
    
# print_analysis displays a table with the data items provided.

def print_analysis(rts,counts,blocks,addends,totals):

    print()
    print("         ", end="")
    for a,t in zip(addends,map(lambda x: x/len(blocks), totals)):
        print("%6s (%2d) " % (a, t),end="")
    print()
    for b in range(len(blocks)):
        print("Block  %d" % blocks[b],end="")
        for a in range(len(addends)):
            print(" %6.3f (%2d)" % (rts[a+b*len(addends)],counts[a+b*len(addends)]),end="")
        print()


# This varaible holds the problems to be presented
# in one set of the task -- 4 problems with each addend
# in a correct equation and 4 problems with each addend
# in an incorrect equation.

data_set = [["a","2","c","k"],["d","2","f","k"],
            ["b","3","e","k"],["e","3","h","k"],
            ["c","4","g","k"],["f","4","j","k"],
            ["a","2","d","d"],["d","2","g","d"],
            ["b","3","f","d"],["e","3","i","d"],
            ["c","4","h","d"],["f","4","k","d"],
            ["a","2","c","k"],["d","2","f","k"],
            ["b","3","e","k"],["e","3","h","k"],
            ["c","4","g","k"],["f","4","j","k"],
            ["a","2","d","d"],["d","2","g","d"],
            ["b","3","f","d"],["e","3","i","d"],
            ["c","4","h","d"],["f","4","k","d"]]


# create_set takes a block number and whether the items
# should be visible and returns a randomized list of
# trial structures representing one set of data with
# those conditions.
 
def create_set(block,visible):

    return list(map(lambda x: trial(block,*x,visible=visible), actr.permute_list(data_set)))
    

