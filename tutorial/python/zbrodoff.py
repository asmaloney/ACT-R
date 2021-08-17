import actr

actr.load_act_r_model("ACT-R:tutorial;unit4;zbrodoff-model.lisp")

trials = []
results = []

control_data = [1.84, 2.46, 2.82, 1.21, 1.45, 1.42, 1.14, 1.21, 1.17]

run_model = True

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

def present_trial(trial, new_window = True):

    if new_window:
        w = actr.open_exp_window("Alpha-arithmetic Experiment", visible=trial.visible)
        if run_model:
          actr.install_device(w)
    else:
        actr.clear_exp_window()

    actr.add_text_to_exp_window(None, trial.text, x=100, y=150)

    trial.start = actr.get_time(run_model)


def respond_to_key_press (model,key):
    global trials,results

    trials[0].time = (actr.get_time(run_model) - trials[0].start) / 1000.0

    if key.lower() == trials[0].answer :
        trials[0].correct = True

    results.append(trials[0])

    trials = trials[1:]

    if len(trials) > 0 :
        present_trial(trials[0],False)


def collect_responses(count):
    global results

    results = []

    actr.add_command("zbrodoff-response", respond_to_key_press,
                     "Zbrodoff task key press response monitor")
    actr.monitor_command("output-key","zbrodoff-response")

    present_trial(trials[0])

    if run_model :
        actr.run(10 * count)
    else:
        if actr.visible_virtuals_available():
            while len(results) < count:
                actr.process_events()

    actr.remove_command_monitor("output-key","zbrodoff-response")
    actr.remove_command("zbrodoff-response")


def problem(addend1,addend2,sum,answer,visible=None):

    global trials
    trials = [trial(1,addend1,addend2,sum,answer,visible)]

    collect_responses(1)
    return analyze_results()

def set(visible=None):

    global trials
    trials = create_set(1,visible)

    collect_responses(24)
    return analyze_results()

def block(visible=None):
    global trials
    trials = []

    for i in range(8):
        trials = trials + create_set(1,visible)

    collect_responses(192)
    return analyze_results()

def experiment(visible=None,show=True):

    actr.reset()

    global trials
    trials = []

    for j in range(3):
        for i in range(8):
            trials = trials + create_set(j+1,visible)

    collect_responses(576)
    return analyze_results(show)


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


def create_set(block,visible):

    return list(map(lambda x: trial(block,*x,visible=visible), actr.permute_list(data_set)))
    

