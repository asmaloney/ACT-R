# This is the Python code to run the task that goes along with the 
# creating-an-image-model. 
#
# It is a simple demonstration of creating and using the
# image AGI item, and it assumes that the smalllogo.gif
# and ref-brain.gif files are in the gui/AGI-images directory
# if you use a visible window to see the images.
#
# To run the task call the run_test function.  It has one
# optional parameter which if provided as True indicates 
# the window should be visible.  The default is to use a virtual
# window.  

import actr

actr.load_act_r_model("ACT-R:examples;creating-image-items;creating-an-image-model.lisp")

def click_brain (text,pos):
    actr.model_output('Clicked image %s at %d %d'%(text,pos[0],pos[1]))

actr.add_command("click-brain-py",click_brain,"Example function for image click action. Do not call.")

def click_brain_2 (value):
    actr.model_output('Clicked the second brain image given value %s'%value)

actr.add_command ("click-brain-2-py",click_brain_2,"Example function for image click action with parameters. Do not call.")


def run_test (visible=False):

    actr.reset()
  
    win = actr.open_exp_window("image test",visible=visible, width=310, height=420)
    
    actr.install_device(win)
    
    actr.start_hand_at_mouse()
    
    actr.add_image_to_exp_window(win, "logo", "smalllogo.gif", x=10, y=10, width=288, height=142)
    
    actr.add_items_to_exp_window(win, actr.create_image_for_exp_window(win, "brain", "ref-brain.gif", x=10, y=160, width=128, height=128, action="click-brain-py"))
    
    actr.add_image_to_exp_window(win, "brain-2", "ref-brain.gif", x=10, y=290, width=128, height=128, action=["click-brain-2-py","this string"])
    
    # run for the vision module to process the scene
    actr.run_n_events(2) 
    
    actr.print_visicon()
    
    # run for up to 5 seconds using real-time if the window is visible
    actr.run(5, visible)

"""
>>> creating_an_image.run_test()
     0.000   ------                 Stopped because event limit reached
Name              Att  Loc             Image  Kind   Height  Width  Value      Size
----------------  ---  --------------  -----  -----  ------  -----  ---------  ----------
VISUAL-LOCATION1  NEW  (374 524 1080)  T      IMAGE  128     128    "brain"    46.0
VISUAL-LOCATION2  NEW  (374 654 1080)  T      IMAGE  128     128    "brain-2"  46.0
VISUAL-LOCATION0  NEW  (454 381 1080)  T      IMAGE  142     288    "logo"     114.259995
     0.000   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION0 NIL
     0.000   VISION                 visicon-update
     0.000   PROCEDURAL             CONFLICT-RESOLUTION
     0.050   PROCEDURAL             PRODUCTION-FIRED ATTEND
VISUAL-LOCATION0-0
   KIND  IMAGE
   VALUE  IMAGE
   HEIGHT  142
   WIDTH  288
   SCREEN-X  454
   SCREEN-Y  381
   DISTANCE  1080
   SIZE  114.259995
     0.050   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.050   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.050   PROCEDURAL             CONFLICT-RESOLUTION
     0.135   VISION                 Encoding-complete VISUAL-LOCATION0-0 NIL
     0.135   VISION                 SET-BUFFER-CHUNK VISUAL IMAGE0
     0.135   PROCEDURAL             CONFLICT-RESOLUTION
     0.185   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-INSTAN-LOC
IMAGE0-0
   SCREEN-POS  VISUAL-LOCATION0-0
   VALUE  "logo"
   HEIGHT  142
   WIDTH  288
   IMAGE  T
     0.185   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.185   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.185   PROCEDURAL             CONFLICT-RESOLUTION
     0.385   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK0
     0.385   PROCEDURAL             CONFLICT-RESOLUTION
     0.435   PROCEDURAL             PRODUCTION-FIRED MOVE
     0.435   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.435   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.435   PROCEDURAL             CLEAR-BUFFER GOAL
     0.435   MOTOR                  MOVE-CURSOR LOC CHUNK0-0
     0.435   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK1
     0.435   PROCEDURAL             CONFLICT-RESOLUTION
     0.635   PROCEDURAL             CONFLICT-RESOLUTION
     0.685   PROCEDURAL             CONFLICT-RESOLUTION
     0.785   PROCEDURAL             CONFLICT-RESOLUTION
     0.835   PROCEDURAL             CONFLICT-RESOLUTION
     0.885   PROCEDURAL             PRODUCTION-FIRED CLICK
     0.885   PROCEDURAL             CLEAR-BUFFER GOAL
     0.885   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     0.885   PROCEDURAL             CLEAR-BUFFER MANUAL
     0.885   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.885   MOTOR                  CLICK-MOUSE
     0.885   VISION                 Find-location
     0.885   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION1
     0.885   PROCEDURAL             CONFLICT-RESOLUTION
     0.935   PROCEDURAL             PRODUCTION-FIRED ATTEND
VISUAL-LOCATION1-0
   KIND  IMAGE
   VALUE  IMAGE
   HEIGHT  128
   WIDTH  128
   SCREEN-X  374
   SCREEN-Y  524
   DISTANCE  1080
   SIZE  46.0
     0.935   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     0.935   PROCEDURAL             CLEAR-BUFFER VISUAL
     0.935   PROCEDURAL             CONFLICT-RESOLUTION
     1.020   VISION                 Encoding-complete VISUAL-LOCATION1-0 NIL
     1.020   VISION                 SET-BUFFER-CHUNK VISUAL IMAGE1
     1.020   PROCEDURAL             CONFLICT-RESOLUTION
     1.070   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-INSTAN-LOC
IMAGE1-0
   SCREEN-POS  VISUAL-LOCATION1-0
   VALUE  "brain"
   HEIGHT  128
   WIDTH  128
   IMAGE  T
     1.070   PROCEDURAL             CLEAR-BUFFER VISUAL
     1.070   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     1.070   PROCEDURAL             CONFLICT-RESOLUTION
     1.085   PROCEDURAL             CONFLICT-RESOLUTION
Image with id "logo" clicked at relative position 144 71
     1.095   PROCEDURAL             CONFLICT-RESOLUTION
     1.185   PROCEDURAL             CONFLICT-RESOLUTION
     1.270   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK2
     1.270   PROCEDURAL             CONFLICT-RESOLUTION
     1.320   PROCEDURAL             PRODUCTION-FIRED MOVE
     1.320   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     1.320   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.320   PROCEDURAL             CLEAR-BUFFER GOAL
     1.320   MOTOR                  MOVE-CURSOR LOC CHUNK2-0
     1.320   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK3
     1.320   PROCEDURAL             CONFLICT-RESOLUTION
     1.520   PROCEDURAL             CONFLICT-RESOLUTION
     1.570   PROCEDURAL             CONFLICT-RESOLUTION
     1.670   PROCEDURAL             CONFLICT-RESOLUTION
     1.720   PROCEDURAL             CONFLICT-RESOLUTION
     1.770   PROCEDURAL             PRODUCTION-FIRED CLICK
     1.770   PROCEDURAL             CLEAR-BUFFER GOAL
     1.770   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     1.770   PROCEDURAL             CLEAR-BUFFER MANUAL
     1.770   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.770   MOTOR                  CLICK-MOUSE
     1.770   VISION                 Find-location
     1.770   VISION                 SET-BUFFER-CHUNK VISUAL-LOCATION VISUAL-LOCATION2
     1.770   PROCEDURAL             CONFLICT-RESOLUTION
     1.820   PROCEDURAL             PRODUCTION-FIRED ATTEND
VISUAL-LOCATION2-0
   KIND  IMAGE
   VALUE  IMAGE
   HEIGHT  128
   WIDTH  128
   SCREEN-X  374
   SCREEN-Y  654
   DISTANCE  1080
   SIZE  46.0
     1.820   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     1.820   PROCEDURAL             CLEAR-BUFFER VISUAL
     1.820   PROCEDURAL             CONFLICT-RESOLUTION
     1.905   VISION                 Encoding-complete VISUAL-LOCATION2-0 NIL
     1.905   VISION                 SET-BUFFER-CHUNK VISUAL IMAGE2
     1.905   PROCEDURAL             CONFLICT-RESOLUTION
     1.955   PROCEDURAL             PRODUCTION-FIRED ATTEND-AND-INSTAN-LOC
IMAGE2-0
   SCREEN-POS  VISUAL-LOCATION2-0
   VALUE  "brain-2"
   HEIGHT  128
   WIDTH  128
   IMAGE  T
     1.955   PROCEDURAL             CLEAR-BUFFER VISUAL
     1.955   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     1.955   PROCEDURAL             CONFLICT-RESOLUTION
     1.970   PROCEDURAL             CONFLICT-RESOLUTION
Clicked image brain at 64 64
     1.980   PROCEDURAL             CONFLICT-RESOLUTION
     2.070   PROCEDURAL             CONFLICT-RESOLUTION
     2.155   IMAGINAL               SET-BUFFER-CHUNK IMAGINAL CHUNK4
     2.155   PROCEDURAL             CONFLICT-RESOLUTION
     2.205   PROCEDURAL             PRODUCTION-FIRED MOVE
     2.205   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     2.205   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.205   PROCEDURAL             CLEAR-BUFFER GOAL
     2.205   MOTOR                  MOVE-CURSOR LOC CHUNK4-0
     2.205   GOAL                   SET-BUFFER-CHUNK GOAL CHUNK5
     2.205   PROCEDURAL             CONFLICT-RESOLUTION
     2.405   PROCEDURAL             CONFLICT-RESOLUTION
     2.455   PROCEDURAL             CONFLICT-RESOLUTION
     2.555   PROCEDURAL             CONFLICT-RESOLUTION
     2.605   PROCEDURAL             CONFLICT-RESOLUTION
     2.655   PROCEDURAL             PRODUCTION-FIRED CLICK
     2.655   PROCEDURAL             CLEAR-BUFFER GOAL
     2.655   PROCEDURAL             CLEAR-BUFFER IMAGINAL
     2.655   PROCEDURAL             CLEAR-BUFFER MANUAL
     2.655   PROCEDURAL             CLEAR-BUFFER VISUAL-LOCATION
     2.655   MOTOR                  CLICK-MOUSE
     2.655   VISION                 Find-location
     2.655   VISION                 FIND-LOC-FAILURE
     2.655   PROCEDURAL             CONFLICT-RESOLUTION
     2.805   PROCEDURAL             CONFLICT-RESOLUTION
     2.855   PROCEDURAL             CONFLICT-RESOLUTION
Clicked the second brain image given value this string
     2.865   PROCEDURAL             CONFLICT-RESOLUTION
     2.955   PROCEDURAL             CONFLICT-RESOLUTION
     2.955   ------                 Stopped because no events left to process
"""