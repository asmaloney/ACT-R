# ACT-R tutorial unit 5 fan task.
# This experiment presents a model with a person-location pair
# of items and the model must respond whether that pair of items
# was part of the study set that it has recorded in memory.
# The task and data to which the model is fit are in the paper:
#
# Anderson, J. R. (1974). Retrieval of propositional information from
# long-term memory. Cognitive Psychology, 5, 451 - 474.
#
# The results are reported are the time to respond to the probe
# based on the 'fan' of the items presented (how many places a person
# is in or how many people are in the place) and whether the probe
# is or isn't in the test set.
#
# This version of the task presents the probe items in a window
# which the model must read to complete the task.

# Import the actr module for tutorial tasks

import actr

# Load the corresponding model for the task.

actr.load_act_r_model("ACT-R:tutorial;unit5;fan-model.lisp")

# Create a variable with the original experiment data.

person_location_data = [1.11, 1.17, 1.22,
                        1.17, 1.20, 1.22,
                        1.15, 1.23, 1.36,
                        1.20, 1.22, 1.26,
                        1.25, 1.36, 1.29,
                        1.26, 1.47, 1.47]

# create variables to hold the model's response and the time of
# that response.

response = False
response_time = False


# The sentence function takes 4 parameters.
# The first two are the strings of the person and location
# to present.  The third is True or False to indicate whether
# this was or wasn't in the study set, and the last is
# either the string 'person' or 'location' to indicate which
# of the productions the model should use for retrieval.
#
# It presents the probe items given in a window, runs the 
# model, and returns a tuple indicating how many seconds it 
# took to respond (or 30 if no response was made) and True or False 
# to indicate if the response was correct.

def sentence (person, location, target, term):

    actr.reset()

    window = actr.open_exp_window("Sentence Experiment",visible=False,width=600,height=300)
    x = 25

    actr.install_device(window)

    actr.add_command("fan-response",respond_to_key_press,"Fan experiment model response")
    actr.monitor_command("output-key","fan-response")

    # disable the production that isn't being used for retrieval

    if term == 'person':
        actr.pdisable("retrieve-from-location")
    else:
        actr.pdisable("retrieve-from-person")


    actr.add_text_to_exp_window (window, person, x=50, y=150, width=75)
    actr.add_text_to_exp_window (window, location, x=250, y=150, width=75)

    global response,response_time

    response = ''
    response_time = 0

    actr.run(30)

    actr.remove_command_monitor("output-key","fan-response")
    actr.remove_command("fan-response")

    if response == '':
        return (30,False)
    elif target:
        if response.lower() == 'k'.lower():
            return (response_time / 1000,True)
        else:
            return (response_time / 1000,False)
    else:
        if response.lower() == 'd'.lower():
            return (response_time / 1000,True)
        else:
            return (response_time / 1000,False)
   

# respond_to_key_press is set to monitor the output-key command
# and records the time and key that was pressed by the model.

def respond_to_key_press (model,key):
    global response,response_time

    response_time = actr.get_time()
    response = key

# do_person_location requires one parameter which is either
# the string 'person' or 'location' to indicate which of the 
# productions the model should use for retrieval.
# It runs one trial of each fan condition and returns a list
# of the results.
    
def do_person_location(term):

    results = []

    for person,location,target in [("lawyer", "store", True),
                                   ("captain", "cave", True),
                                   ("hippie", "church", True),
                                   ("debutante", "bank", True),
                                   ("earl", "castle", True),
                                   ("hippie", "bank", True),
                                   ("fireman", "park", True),
                                   ("captain", "park", True),
                                   ("hippie", "park", True),
                                   ("fireman", "store", False),
                                   ("captain", "store", False),
                                   ("giant", "store", False),
                                   ("fireman", "bank", False),
                                   ("captain", "bank", False),
                                   ("giant", "bank", False),
                                   ("lawyer", "park", False),
                                   ("earl", "park", False),
                                   ("giant", "park", False)]:

        results.append(sentence(person,location,target,term))

    return results

# experiment runs the model through one trial of 
# each condition using each of the retrieval productions
# and averages the results then displays the results.

def experiment():

    output_person_location(list(map(lambda x,y:((x[0]+y[0])/2,(x[1] and y[1])),
                                    do_person_location('person'),
                                    do_person_location('location'))))


def output_person_location(data):

    rts = list(map(lambda x: x[0],data))

    actr.correlation(rts,person_location_data)
    actr.mean_deviation(rts,person_location_data)

    print("TARGETS:\n                         Person fan")
    print("  Location      1             2             3")
    print("    fan")
    
    for i in range(3):
        print("     %d      " % (i+1),end="")
        for j in range(3):
            print("%6.3f (%-5s)" % (data[j + (i * 3)]),end="")
        print()

    print()
    print("FOILS:")
    for i in range(3):
        print("     %d      " % (i+1),end="")
        for j in range(3):
            print("%6.3f (%-5s)" % (data[j + ((i + 3) * 3)]),end="")
        print()
