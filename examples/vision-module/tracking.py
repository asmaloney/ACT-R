"""
There are two example functions here: one which uses the AGI to 
display a moving X and one which uses add- & modify- visicon-features
to move an arbitrary feature.

To run them call either agi or arbitrary respectively to do the task.

The same model found in tracking-model.lisp performs both tasks.

This code depends upon the actr module found with the tutorial to
connect and interact with ACT-R.

"""

import actr

actr.load_act_r_model("ACT-R:examples;vision-module;tracking-model.lisp")

def move_text(text,y):
    y += 10
    actr.current_connection.evaluate_single("modify-text-for-exp-window",text,None,None,y)
    actr.schedule_event_relative (.500, "agi-move-it",params=[text,y],maintenance=True)
    

def agi ():
  
    actr.reset()
  
    # open a window and add the text
  
    window = actr.open_exp_window("Moving X", visible=True)
    text = actr.add_text_to_exp_window(window, "x", x=10, y=10)
    y = 10
    
    actr.install_device(window)
    
    # schedule an event to move it but don't have the 
    # periodic-event command available at this point so
    # just use a relative event which schedules itself
    
    actr.add_command("agi-move-it",move_text)

    actr.schedule_event_relative (1, "agi-move-it",params=[text,y],maintenance=True)

    # run the model in real time since it's a visible window
    
    actr.run(3,True)

    actr.remove_command("agi-move-it")


def move_feat(feature, x):

    x += 10

    actr.modify_visicon_features([feature,'screen-x',x])
    actr.schedule_event_relative (.5, "arbitrary-move-it",params=[feature,x],maintenance=True)



def arbitrary ():
  
    actr.reset()
  
    feature = actr.add_visicon_features(['screen-x',15,'screen-y',20,'value',"'x'"])[0]
    x = 15

    # schedule an event to move it but don't have the 
    # periodic-event command available at this point so
    # just use a relative event which schedules itself
    
    actr.add_command("arbitrary-move-it",move_feat)

    actr.schedule_event_relative (1, "arbitrary-move-it",params=[feature,x],maintenance=True)

    # run the model in real time since it's a visible window
    
    actr.run(3,True)

    actr.remove_command("arbitrary-move-it")

