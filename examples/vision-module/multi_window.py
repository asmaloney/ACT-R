# This example shows that multiple devices can be installed to
# the vision interface at the same time.  It also shows that the
# visicon features created by each exp-window installed are 
# available simultaneously as well as those from the mouse
# cursor.  Finally, it shows that the coordinates for all of
# the features of the exp-window device and the mouse cursor
# are represented in a global coordinate system and not in
# the local coordinates of the windows themselves.

# It depends upon the actr module from the ACT-R tutorial to provide
# the connection to ACT-R and functions to access the ACT-R commands.

import actr

def example ():
    actr.reset()
  
    # The first window is located at x=0 and y=0 which
    # is fine for virtual windows, but if it were a real
    # window that window would be behind the menu bar
    # at the top of the display under OS X which makes
    # it difficult to interact with.  The second window
    # is located at x=200, y=200.  The default mouse 
    # position is x=0, y=0.
  
    w1 = actr.open_exp_window("W1", visible=False, width=100, height=100, x=0, y=0)
    w2 = actr.open_exp_window("W2", visible=False, width=100, height=100, x=200, y=200)
    
    # add text to the same local position in each window.
    
    actr.add_text_to_exp_window(w1, "a", x=10, y=10, color='red')
    actr.add_text_to_exp_window(w2, "a", x=10, y=10, color='blue')
    
    # Install both windows and the mouse cursor 
    
    actr.install_device(w1)
    actr.install_device(w2)
    actr.install_device(["vision", "cursor", "mouse"])
    
    # Just run the model to have vision module process
    # things and print the visicon.
    # The model doesn't do anything.
    
    actr.run(1)
    actr.print_visicon()

# Load a model which doesn't do anything.

actr.load_act_r_model("ACT-R:examples;vision-module;multi-window-model.lisp")