"""
 This example demonstrates the customization options available
 when adding visual features.  There are curretnly two customizations
 available: the names of the location chunk's position slots can
 be changed from the defaults of screen-x, screen-y, and distance
 and changing the effective width of the item as needed for computing
 the movement times based on Fitts' Law from the default of assuming
 it is a rectangle based on the given height and width.  Those items
 can be changed independently for each feature.

 It depends upon the actr module from the ACT-R tutorial to provide
 the connection to ACT-R and functions to access the ACT-R commands.
"""

import actr
import numbers

def custom_width (feature, angle, x, y):
    actr.model_output("Computing a custom width for feature: %s being approached at angle %s from position %s,%s."%(feature, angle, x, y))
    h = actr.chunk_slot_value(feature, 'height')

    if isinstance(h,numbers.Number):
        return(2 * h)

def example ():
  
    actr.reset()
  
    """
    We will create two items in the display and have the model
    find them and move the mouse to them to see the difference
    in timing based on the width function setting.

    One feature will use the normal visual-location and object
    chunk-types and the other will use a custom visual-location
    type defined like this which replaces screen-x, screen-y,
    and distance with slots named x, y, and z instead.

    (chunk-type custom-location x y z height width size)

    The feature with using the standard visual-location slots
    will include a custom width function to make it act like
    a circle with twice its given height.
    """

    actr.add_command("custom-width", custom_width, "Return twice the height of a visual feature.")
  
  
    actr.add_visicon_features(['screen-x', 50, 'screen-y', 500, 'height', 20, 'width', 20, ':width-fn', "'custom-width'"],
                              ['isa', 'custom-location', 'x', 150, 'y', 500, 'height', 20, 'width', 20, ':x-slot', 'x', ':y-slot', 'y', ':z-slot', 'z'])
  
    # Give the vision module a chance to process the display
    # before printing the visicon.
  
    actr.run_n_events(3)
    actr.print_visicon()
  
    # run the model to move the cursor to 100,0 as a starting point.
    # and from there move to the left location, back to the start, and then
    # the right location.
  
    actr.run(10)
  
    actr.remove_command('custom-width')


# The model is very simple and it just finds the two locations,
# prints them out, and moves the mouse there from a fixed starting 
# point to see the difference the width function makes.

actr.load_act_r_model("ACT-R:examples;vision-module;customize-visicon-features-model.lisp")

