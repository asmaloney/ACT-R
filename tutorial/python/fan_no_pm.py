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
# This version of the task does not use the perceptual or motor
# modules of ACT-R and instead places the probes directly into
# slots of the goal buffer and reads the model's response from
# a slot of the goal buffer when done.

# Import the actr module for tutorial tasks

import actr

# Load the corresponding model for the task.

actr.load_act_r_model("ACT-R:tutorial;unit5;fan-no-pm-model.lisp")

# Create a variable with the original experiment data.

person_location_data = [1.11, 1.17, 1.22,
                        1.17, 1.20, 1.22,
                        1.15, 1.23, 1.36,
                        1.20, 1.22, 1.26,
                        1.25, 1.36, 1.29,
                        1.26, 1.47, 1.47]

# The sentence function takes 4 parameters.
# The first two are the strings with a string of the person and location
# to present e.g. "'hippie'".  The third is True or False to indicate whether
# this was or wasn't in the study set, and the last is
# either the string 'person' or 'location' to indicate which
# of the productions the model should use for retrieval.
#
# It presents the probe items given directly to the model
# through the goal buffer, runs the model, and
# returns a tuple indicating how many seconds the model
# ran and True or False to indicate if the response was correct.

def sentence (person, location, target, term):

    actr.reset()

    # disable the production that isn't being used for retrieval

    if term == 'person':
        actr.pdisable("retrieve-from-location")
    else:
        actr.pdisable("retrieve-from-person")

    # modify the chunk named goal (which will be placed into the goal buffer
    # when the model runs) to set the arg1 and arg2 slots to the probe
    # items and state slot to test

    actr.mod_chunk("goal","arg1",person,"arg2",location,"state","test")

    # run the model recording the time spent running
    # and get the value from the state slot of the goal buffer representing the
    # model's response to the task

    response_time = actr.run(30)[0]
    response = actr.buffer_slot_value("goal","state")

    if target:
        if response.lower() == "'k'".lower():
            return (response_time ,True)
        else:
            return (response_time ,False)
    else:
        if response.lower() == "'d'".lower():
            return (response_time ,True)
        else:
            return (response_time ,False)
   
# do_person_location requires one parameter which is either
# the string 'person' or 'location' to indicate which of the 
# productions the model should use for retrieval.
# It runs one trial of each fan condition and returns a list
# of the results.
    
def do_person_location(term):

    data = []

    for person,location,target in [("'lawyer'", "'store'", True),
                                   ("'captain'", "'cave'", True),
                                   ("'hippie'", "'church'", True),
                                   ("'debutante'", "'bank'", True),
                                   ("'earl'", "'castle'", True),
                                   ("'hippie'", "'bank'", True),
                                   ("'fireman'", "'park'", True),
                                   ("'captain'", "'park'", True),
                                   ("'hippie'", "'park'", True),
                                   ("'fireman'", "'store'", False),
                                   ("'captain'", "'store'", False),
                                   ("'giant'", "'store'", False),
                                   ("'fireman'", "'bank'", False),
                                   ("'captain'", "'bank'", False),
                                   ("'giant'", "'bank'", False),
                                   ("'lawyer'", "'park'", False),
                                   ("'earl'", "'park'", False),
                                   ("'giant'", "'park'", False)]:

        data.append(sentence(person,location,target,term))

    return data

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

    print("\nTARGETS:\n                         Person fan")
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
