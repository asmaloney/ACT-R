"""
This example shows how one can specify a command to dynamically 
create the visual object chunks for features that are added to 
the visicon instead of specifying them when they are added.

There is a device called object-creator which can be installed
for the vision interface with the details of the device being
the command to use.  When the model shifts attention to an
item in the visicon if an object-creator device is installed
then the command associated with that device will be passed
the visicon entry id for the item being attended (the value
which was returned when the item was added to the visicon).
If the command returns a chunk it will be the one placed into
the visual buffer as a result of the attention shift.  If
it returns any other value then the default mechanism will be
used to create the visual-object chunk, and if that other value
is not nil then a warning will be printed to indicate that an
invalid value was returned.

It depends upon the actr module from the ACT-R tutorial to provide
the connection to ACT-R and functions to access the ACT-R commands.
"""

import actr

ids = None

def custom_object_chunk (id):
    if ids[0] == id:
        return actr.define_chunks(["isa", "custom-object", "color", "red", "time", actr.mp_time_ms()])[0]

def example ():

    global ids

    actr.reset()
  
    actr.add_command("custom-object-chunk",custom_object_chunk,"Command to dynamically create visual object")

    # Here we install our custom function using an object-creator device
    actr.install_device(["vision", "object-creator", "custom-object-chunk"])
  
    # Now we add two features to the display, the first of which will
    # have the object created by the custom function and the other by
    # the default mechanism.
  
    ids = actr.add_visicon_features(["screen-x", 10, "screen-y", 20],
                                    ["screen-x", 100, "screen-y", 20])
  
    # run to give vision module a chance to
    # process those features and then print the
    # visicon.
  
    actr.run_n_events(3)
    actr.print_visicon()
  
    # run to have model complete task
    actr.run(1)

    actr.remove_command("custom-object-chunk")

# The model just attends to items and prints out the location and
# object chunks which it gets.

actr.load_act_r_model("ACT-R:examples;vision-module;dynamic-object-creation-model.lisp")
