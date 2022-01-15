# ACT-R tutorial unit 5 grouped task.
# This is a simple example task to show partial matching.
# It simply runs the model and records values that the
# model provides and returns the list of provided values
# in the order provided after the run.

# Import the actr module for tutorial tasks

import actr

# Load the corresponding model for the task.

actr.load_act_r_model("ACT-R:tutorial;unit5;grouped-model.lisp")

# Create a variable to hold the responses

response = []

# The recall function creates a command for
# the record_response function, clears the response
# list, runs the model, and returns the response list.

def recall ():

    actr.add_command("grouped-response",record_response,"Response recording function for the tutorial grouped model.")
    global response
    response = []
    actr.reset()
    actr.run(20)
    actr.remove_command("grouped-response")
    return response


# Store a value provided by the model on the response list

def record_response (item):

    global response
    response.append(item)


