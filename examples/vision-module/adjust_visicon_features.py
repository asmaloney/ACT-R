"""
 This example demonstrates how visual features created using
 the add-visicon-features command can be modified and removed from
 the visicon, and that those changes automatically trigger an update
 by the vision module.

 It depends upon the actr module from the ACT-R tutorial to provide
 the connection to ACT-R and functions to access the ACT-R commands.
"""

import actr

def example():
  
    actr.reset()
  
    """
    Add-visicon-features returns a list of names which can be
    used to update the features which were added to the visicon.
    The modify-visicon-features command can be used to adjust
    the location and object representation of an item that was
    added to the visicon.  The delete-visicon-features and
    delete-all-visicon-features commands can be used to remove
    items which were added to the visicon.

    For this example these chunk-types are defined in the model
    which subclass the standard visual-location and visual-object
    chunk-types as were used for the new-visicon-features example.
  
    (chunk-type (polygon-feature (:include visual-location)) regular)
    (chunk-type (polygon (:include visual-object)) sides)
    """

    features = actr.add_visicon_features(['isa', ['polygon-feature', 'polygon'], 'screen-x', 0, 'screen-y', 50,
                                          'value', ('polygon', "'poly1'"), 'height', 20, 'width', 40, 
                                          'color', 'blue', 'regular', ['false', None], 
                                          'sides', [None, 7]],
                                         ['isa', ['polygon-feature', 'polygon'], 'screen-x', 50, 'screen-y', 70, 
                                          'value', ['polygon', "'square'"], 'height', 30, 'width', 30, 
                                          'color', 'red', 'regular', ['true', None], 
                                          'sides', [None, 4]])

    
    # Give the vision module a chance to process the display
    # before printing the visicon.
    
    actr.run_n_events(3)
    actr.print_visicon()
    
    # run the model to show the current chunks
    actr.run(10)
    
    """
    Modify the first of those features to change the color to green,
    adjust the x position, and change the number of sides to 8.
    The parameters for modify-visicon-features are very similar to
    add except that the list of features must start with the name
    of the feature to update which was returned from add-visicon-features.
    One thing to note is that because nil is used in the list of values to
    indicate that a slot should not be applied to the location or object 
    means that you can't use nil to 'remove' a slot from a feature through
    a modification.
    """

    actr.modify_visicon_features([features[0],'color', 'green', 'screen-x', 5, 'sides', [None, 8]])
    
    # Give the vision module a chance to process the display
    # before printing the visicon.
    
    actr.run_n_events(3)
    actr.print_visicon()
    
    # run the model to show the update
    actr.run(10)
    
    # Modify the second of those features to change the regular value to false and the
    # height to 25
    
    actr.modify_visicon_features([features[1],'height', 25, 'regular', ['false', None]])
    
    # Give the vision module a chance to process the display
    # before printing the visicon.
    
    actr.run_n_events(3)
    actr.print_visicon()
    
    # run the model to show the update
    actr.run(10)
    
    # delete the second feature.  delete-visicon-features takes any number
    # of parameters each of which should be the name of a feature that was
    # returned by add-visicon-feature and those features are removed from
    # the visicon.
    
    actr.delete_visicon_features(features[1])
    
    # Give the vision module a chance to process the display
    # before printing the visicon.
    
    actr.run_n_events(3)
    actr.print_visicon()
    
    # run the model to show the update
    actr.run(10)
    
    # delete all of the visicon features.  delete-all-visicon-features
    # removes all of the features from the visicon.
    
    actr.delete_all_visicon_features()
    
    # Give the vision module a chance to process the display
    # before printing the visicon.
    
    actr.run_n_events(3)
    actr.print_visicon()
    
    # run the model to show the update
    actr.run(10)
  
  

"""
 The model is very simple in that it just repeatedly finds
 a location and then attends to the item there printing
 out the chunks in the visual-location and visual buffers
 after the corresponding requet completes.
 The ordering of the modifications are such that it will
 not automatically updated the attended object chunks 
 since the currently attended item is unchanged in each
 case.
"""

actr.load_act_r_model("ACT-R:examples;vision-module;adjust-visicon-features-model.lisp")

