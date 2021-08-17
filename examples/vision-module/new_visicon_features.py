"""
This example demonstrates how one can generate new visual features
for a model using the add-visicon-features command which allows one
to specify visual features which have a 1<->1 mapping between locations
and objects.

It depends upon the actr module from the ACT-R tutorial to provide
the connection to ACT-R and functions to access the ACT-R commands.
"""

import actr

def example():
  
    actr.reset()
  
    """ 
    Add-visicon-features allows the modeler to specify arbirary
    visual features for the visicon of a model.  Any number of
    features can be provided at once, and each feature represents
    an object and its location.  The description of those items
    is provided in much the same way that one specifies a chunk
    i.e. as slot-value pairs, and internally the vision module
    uses three chunks to represent that item (the location, the
    object, and a feature which contains the information that 
    can be 'found' through a visual-location request).

    All features must have an x and y position which default
    to being in the screen-x and screen-y slots (see the
    new-location-slots example for how to change that).  The
    distance (z position) can be specified, but if not the
    default distance will be included.  Similarly, a size
    (which is measured in degrees of visual angle squared)
    can be provided, but if not it will be computed from the
    height and width if specified or set to a default size if
    those are not provided.  
  
    When specifying the slot-value pairs to describe a feature
    the slot must be a valid slot for chunks (or the name isa
    to declare a chunk-type).  The value can be provided as
    either an explict value or a list of two values.  If
    an explicit value is provided then that value represents
    the value of that slot for both the location and object
    except when it is one of the x, y, or z coordinates which
    are only provided in the location.  If a list of values
    is provided then the first one represents the value of
    that slot for the visual-location chunk and the second
    one represents the value of the slot for the object.
    Either of the items can be specified as None to indicate
    that the corresponding chunk should not have that slot.
    The value for the feature which can be found through the
    visual-location request is the value for the object if
    one is provided otherwise it will be the value for the
    location.
  
    The isa specification can also take a list of two values which
    indicate the chunk-types to specify when creating the chunks
    for the location and object, which can include default slot
    values.  In addition to that, if a chunk-type is declared for
    the object and no slot named kind is specified for the feature
    then the location chunk will include a kind slot with a value
    that matches the chunk-type declared for the object.
  
    For this example these chunk-types are defined in the model
    which subclass the standard visual-location and visual-object
    chunk-types.
  
    (chunk-type (polygon-feature (:include visual-location)) regular)
    (chunk-type (polygon (:include visual-object)) sides)
    (chunk-type (square (:include polygon)) (sides 4) (square t))

    and the oval type is predefined by the vision module like this:

    (chunk-type (oval (:include visual-object)) (oval t))

    One final thing to note is that if one wants to specify a
    string as a value for a slot it must be specified using
    a set of single quotes inside the double quotes of a
    string because when sent to ACT-R strings are interpreted as 
    names/symbols by default for the visicon features.
    """  

    actr.add_visicon_features(['screen-x', 0, 'screen-y', 5, 'regular', 'true'],
                              ['isa', ['visual-location', 'oval'], 'screen-x', 12, 'screen-y', 20, 
                               'value', ['oval', "'the oval'"], 'height', 10, 'width', 40, 
                               'color', 'blue'],
                              ['isa', ['polygon-feature', 'polygon'], 'screen-x', 50, 'screen-y', 50,
                               'value', ('polygon', "'poly 1'"), 'height', 20, 'width', 40, 
                               'color', 'blue', 'regular', ['false', None], 
                               'sides', [None, 7]],
                              ['isa', ['polygon-feature', 'square'], 'screen-x', 10, 'screen-y', 50, 
                               'value', ['square', "'the square'"], 'height', 30, 'width', 30, 
                               'color', 'red', 'regular', ['true', None], 
                               'sides', [None, 4]],
                              ['isa', ['polygon-feature', 'polygon'], 'screen-x', 5, 'screen-y', 70, 
                               'value', ['polygon', "'poly 2'"], 'height', 50, 'width', 45, 
                               'color', 'green', 'regular', ['false', None], 
                               'sides', [None, 5]])
                                     
    
    # Give the vision module a chance to process the display
    # before printing the visicon.
    
    actr.run_n_events(3)
    actr.print_visicon()
    
    # Then just run the model
    actr.run(10)
  

"""
 The model is very simple in that it just repeatedly finds
 a location and then attends to the item there printing
 out the chunks in the visual-location and visual buffers
 after the corresponding requet completes.
"""

actr.load_act_r_model("ACT-R:examples;vision-module;new-visicon-features-model.lisp")

