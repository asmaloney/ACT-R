
import actr

actr.load_act_r_model("ACT-R:tutorial;unit5;grouped-model.lisp")

response = []

def recall ():

    actr.add_command("grouped-response",record_response,"Response recording function for the tutorial grouped model.")
    global response
    response = []
    actr.reset()
    actr.run(20)
    actr.remove_command("grouped-response")
    return response

def record_response (item):

    global response
    response.append(item)


