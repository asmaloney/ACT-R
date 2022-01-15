# ACT-R tutorial unit7 task for investigating utility
# learning modeling issues.

import actr

def present_choose():

    actr.goal_focus('initial-goal')

    if actr.random(1.0) < .6:
        answer = 'a' 
    elif actr.random(1.0) > .5:
        answer = 'b'
    else:
        answer = None

    actr.schedule_event_relative(5,'utility-learning-issues-show-result',params=[answer],output='medium')


def show_result (choice):
    actr.mod_chunk('response','answer',choice)
    actr.set_buffer_chunk('imaginal','response')
    actr.schedule_event_relative(2,'utility-learning-issues-choose',output='medium')


actr.add_command('utility-learning-issues-choose',present_choose,"Function to change model's goal for utility learning issues model")
actr.add_command('utility-learning-issues-show-result',show_result,"Function to set the model's imaginal buffer for utility learning issues model")


def finished ():
    actr.remove_command('utility-learning-issues-choose')
    actr.remove_command('utility-learning-issues-show-result')


actr.load_act_r_model("ACT-R:tutorial;unit7;utility-learning-issues-model.lisp")
