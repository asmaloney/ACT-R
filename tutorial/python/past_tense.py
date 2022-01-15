# ACT-R tutorial unit 7 past-tense task.
# This task presents the model with an English
# verb (taken from a very small sample of verbs)
# randomly chosen based on the frequency of that
# verb's usage relative to the other verbs in the
# set.  The model is then supposed to generate 
# the past-tense for that verb.  For every verb
# that the model must generate, it receives two
# randomly chosen correctly formed past-tenses
# which are merged into declarative memory.  The
# code reports the percentages of correctly and
# incorrectly formed past-tenses by the model and
# will display a graph of the correctness for the
# irregular verbs which tend to show a U-shape in
# their learning.

# Import the actr module for tutorial tasks and
# math for the floor and ceil functions.

import actr
import math

# Load the corresponding model for the task

actr.load_act_r_model("ACT-R:tutorial;unit7;past-tense-model.lisp")

# Global variables to hold the collected data and the verbs
# along with their frequencies.

report = []
total_count = 0
word_list = []

verbs = [['have','i',12458,'had'],
         ['do','i',4367,'did'],
         ['make','i',2312,'made'],
         ['get','i',1486,'got'],
         ['use','r',1016,'use'],
         ['look','r',910,'look'],
         ['seem','r',831,'seem'],
         ['tell','i',759,'told'],
         ['show','r',640,'show'],
         ['want','r',631,'want'],
         ['call','r',627,'call'],
         ['ask','r',612,'ask'],
         ['turn','r',566,'turn'],
         ['follow','r',540,'follow'],
         ['work','r',496,'work'],
         ['live','r',472,'live'],
         ['try','r',472,'try'],
         ['stand','i',468,'stood'],
         ['move','r',447,'move'],
         ['need','r',413,'need'],
         ['start','r',386,'start'],
         ['lose','i',274,'lost']]


# make_word_freq_list takes one parameter which is a list
# of verb description lists (as created above for the verbs
# variable) that contains the verb, whether it has a regular
# or irregular past-tense, it's relative frequency, and the
# stem for its past-tense (either the verb itself for a regular
# or the correct irregular inflection).  It returns a list
# with a list for each verb that has a cumulative frequency
# value and the three components for specifying the chunk
# of the verb's past-tense (the verb, stem, and suffix slot
# values).

def make_word_freq_list (l):

    data = []
    count = 0

    for verb in l:
        count += verb[2]
        if verb[1] == 'i':
            suffix = 'blank'
        else:
            suffix = 'ed'

        data.append([count,verb[0],verb[3],suffix])

    global total_count
    total_count = count
    return(data)


# random_word returns a list with the slot values for
# a randomly choosen word from those in the word_list list
# based on frequency.

def random_word():

    num=actr.random(total_count)

    for i in word_list:
        if i[0] > num:
            return(i[1:])


# make_one_goal randomly chooses a verb to present to the
# model in the imaginal buffer and sets the goal to be a
# copy of the starting-goal chunk.  It returns the verb list
# for the chosen verb.

def make_one_goal():

    word = random_word()
  
    actr.set_buffer_chunk('imaginal',['verb',word[0]])
    
    actr.goal_focus('starting-goal')
    
    return(word)

# add_past_tense_to_memory randomly chooses a verb to
# merge into the model's declartive memory.  It does
# the merging by setting the imaginal buffer to a
# chunk which has the appropriate slots set and then
# clearing the buffer.

def add_past_tense_to_memory ():

    word = random_word()

    actr.set_buffer_chunk('imaginal',['verb',word[0],'stem',word[1],'suffix',word[2]])
    actr.clear_buffer('imaginal')


# print_header prints the column labels for the data displayed.

def print_header():
    print ()
    print ( "trials      Irregular       Regular    No inflection  Inflected correctly")

# results prints out the performance of the model
# averaged over blocks of 1000 verbs, and optionally draws a
# graph of the correctness of inflected irregular verbs (by 
# default it draws the graph but providing a value of False or None 
# will suppress the graph)

def results(graph=True):

    print_header()
    data = rep_f_i(0,len(report),1000)
    if graph and len(data) > 1:
        graph_it(data)
    return(data)

# graph_it requires one parameter which is a list of
# data which it draws in an experiment window scaling
# increments on the x and y axis to fit the data to
# the size of the graph

def graph_it(data):

    win = actr.open_exp_window("Irregular Verbs correct",visible=True,width=500,height=475)
    low = min(data)
    zoom = min([.9,math.floor(10 * low)/10])

    actr.clear_exp_window(win)
    actr.add_text_to_exp_window(win, "1.0", x=5, y=5, width=22)
    actr.add_text_to_exp_window(win, "%0.2f" % zoom, x=5, y=400, width=22)
    actr.add_text_to_exp_window(win, "%0.2f" % (zoom + ((1.0 - zoom) / 2)), x=5, y=200, width=22)

    actr.add_text_to_exp_window(win, "Trials", x=200, y=420, width=100)
    actr.add_line_to_exp_window(win,[30,10],[30,410],'black')
    actr.add_line_to_exp_window(win,[450,410],[25,410],'black')

    for i in range(10):
        actr.add_line_to_exp_window(win,[25,10 + i*40],[35,10 + i*40],'black')

    start = data[0]
    increment = max([1.0,math.floor( 450 / len(data))])
    r = math.floor (400/ (1.0 - zoom))
    intercept = r + 10
    lastx =30
    lasty = intercept - math.floor(r * start)
    
    for p in data[1:]:
        x = lastx + 30
        y = intercept - math.floor(r * p)

        actr.add_line_to_exp_window(win,[lastx,lasty],[x,y],'red')
        lastx = x
        lasty = y
    

# safe_div takes two parameters, n and d and returns
# n/d unless d is 0 in which case it returns 0.

def safe_div(n, d):
    if d == 0:
        return(0)
    else:
        return (n / d)

# rep_f_i computes the average performance of the model
# given a range of elements in the data in blocks of size
# count and prints those values.

def rep_f_i(start,end,count):

    data = []

    for i in range(math.ceil( (end - start) / count)):
        irreg = 0
        reg = 0
        none = 0
        
        if end > (start + count + (i * count)):
            e = start + count + (i * count)
        else:
            e = end

        for x in report[(start + (i * count)):e]:
            if x[0]:
                if x[1] == 'reg':
                    reg += 1
                elif x[1] == 'irreg':
                    irreg += 1
                elif x[1] == 'none':
                    none += 1

        total = irreg + reg + none
        correct = safe_div(irreg, (irreg + reg))

        print("%6d %13.3f %13.3f %13.3f %13.3f"%
              (e,safe_div(irreg,total),safe_div(reg,total),safe_div(none,total),correct))

        data.append(correct)

    return data

# add_to_report takes two parameters which are the verb
# that was presented to the model and the resulting chunk
# that it had in the imaginal buffer.  It records the
# result based on the type of verb and correctness of
# the response.  It also reports warnings for verbs
# that are formed incorrectly by the model.

def add_to_report(target, chunk):
    global report

    stem = actr.chunk_slot_value(chunk,"stem")
    word = actr.chunk_slot_value(chunk,"verb")
    suffix = actr.chunk_slot_value(chunk,"suffix")
    irreg = (target[2] == 'blank')

    if target[0].lower() == word.lower():
        if stem == word and suffix.lower() == 'ed':
            report.append([irreg,'reg'])
        elif stem == None and suffix == None:
            report.append([irreg,'none'])
        elif stem.lower() == target[1].lower() and suffix.lower() == 'blank':
            report.append([irreg,'irreg'])
        else:
            actr.print_warning(
                "Incorrectly formed verb. Presented %s and produced verb %s,stem %s,suffix %s."%
                (target[0],word,stem,suffix))
    else:
        actr.print_warning(
            "Incorrectly formed verb. Presented %s and produced verb %s,stem %s,suffix %s."%
            (target[0],word,stem,suffix))
        report.append([irreg,'error'])


# create a global variable and a function that will be
# used to monitor the trigger-reward command so that 
# the code can verify whether or not the model receives
# a reward on each trial.

reward_check = False

def verify_reward(*params):
    global reward_check
    reward_check = True

# trials is used to run the model through
# the task.  It takes one required parameter which
# is the number of trials to present.  It has two
# optional parameters.  The firt of those indicates
# whether the model should continue doing the task
# or be reset and start over.  The default is to
# start over, but providing a true value will
# cause it to continue.  The second optional parameter
# controls whether the trace is shown or not.  The
# default is to not show the trace, but providing 
# a value of True will show the trace.

def trials(n,cont=False,v=False):

    global report,word_list,reward_check

    # add a command to monitor trigger-reward 

    actr.add_command("reward-check",verify_reward,
                     "Past tense code check for a reward each trial.")
    actr.monitor_command("trigger-reward","reward-check")

    # if there isn't a word list created yet or the
    # model is supposed to start over then
    # reset and create chunks for the verbs on the
    # list.
  
    if not(cont) or not(word_list):
        actr.reset()
        word_list = make_word_freq_list(verbs)
        new = []
        for x in word_list:
            for y in x[1:]:
                if y not in new:
                    new.append(y)
        for x in new:
            if not(actr.chunk_p(x)):
                actr.define_chunks([x])

        print_header()
        report = []

    # set the :v value as provided

    actr.set_parameter_value(":v",v)

    # present the n trials and report the data in
    # blocks of 100 as it goes.
    
    start = 100 * math.floor(len(report) / 100)
    count = len(report) % 100

    for i in range(n):
        # Add the two random past-tenses to memory
        add_past_tense_to_memory()
        add_past_tense_to_memory()

        # run the model up to 100 seconds to
        # process a randomly chosen past-tense
        # and record the data outputting it
        # if there're 100 items to output
      
        reward_check = False
        target = make_one_goal()
        duration = actr.run(100)[0]
        add_to_report(target,actr.buffer_read('imaginal'))
        actr.clear_buffer('imaginal')
        count += 1
        
        if count == 100:
            rep_f_i(start, start + 100, 100)
            count = 0
            start += 100

        # If the model didn't get a reward or 
        # spent all 100s running warn about that.

        if not(reward_check):
            actr.print_warning("Model did not receive a reward when given %s."% target[0])

        if duration == 100:
            actr.print_warning("Model spent 100 seconds generating a past tense for %s."%
                               target[0])

        # run the model until 200 seconds have
        # passed since the start of the trial

        actr.run_full_time(200 - duration)

    # if there are any remaining data items 
    # report them (< 100 after the last block).

    rep_f_i(start,start+count,100)

    # remove the monitor for trigger-reward 

    actr.remove_command_monitor("trigger-reward","reward-check")
    actr.remove_command("reward-check")

