;;; This example demonstrates how visual features created using
;;; the add-visicon-features command can be modified and removed from
;;; the visicon, and that those changes automatically trigger an update
;;; by the vision module.

(defun run-example ()
  
  (reset)
  
  ;; Add-visicon-features returns a list of names which can be
  ;; used to update the features which were added to the visicon.
  ;; The modify-visicon-features command can be used to adjust
  ;; the location and object representation of an item that was
  ;; added to the visicon.  The delete-visicon-features and
  ;; delete-all-visicon-features commands can be used to remove
  ;; items which were added to the visicon.
  
  ;; For this example these chunk-types are defined in the model
  ;; which subclass the standard visual-location and visual-object
  ;; chunk-types as were used for the new-visicon-features example.
  
  ;; (chunk-type (polygon-feature (:include visual-location)) regular)
  ;; (chunk-type (polygon (:include visual-object)) sides)
  
  (let ((features (add-visicon-features 
                        '(isa (polygon-feature polygon) screen-x 0 screen-y 50
                          value (polygon "poly1") height 20 width 40 
                          color blue regular (false nil) 
                          sides (nil 7))
                        '(isa (polygon-feature polygon) screen-x 50 screen-y 70 
                          value (polygon "square") height 30 width 30 
                          color red regular (true nil) 
                          sides (nil 4)))))
                                     
    
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    
    (run-n-events 3)
    (print-visicon)
    
    ;; run the model to show the current chunks
    (run 10)
    
    ;; Modify the first of those features to change the color to green,
    ;; adjust the x position, and change the number of sides to 8.
    ;; The parameters for modify-visicon-features are very similar to
    ;; add except that the list of features must start with the name
    ;; of the feature to update which was returned from add-visicon-features.
    ;; One thing to note is that because nil is used in the list of values to
    ;; indicate that a slot should not be applied to the location or object 
    ;; means that you can't use nil to 'remove' a slot from a feature through
    ;; a modification.
    
    (modify-visicon-features (list (first features) 'color 'green 'screen-x 5 'sides '(nil 8)))
    
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    
    (run-n-events 3)
    (print-visicon)
    
    ;; run the model to show the updated
    (run 10)
    
    
    ;; Modify the second of those features to change the regular value to false and the
    ;; height to 25
    
    (modify-visicon-features (list (second features) 'height 25 'regular '(false nil)))
    
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    (run-n-events 3)
    (print-visicon)
    
    ;; run the model to show the updated
    (run 10)
    
    ;; delete the second feature.  delete-visicon-features takes any number
    ;; of parameters each of which should be the name of a feature that was
    ;; returned by add-visicon-feature and those features are removed from
    ;; the visicon.
    
    (delete-visicon-features (second features))
    
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    (run-n-events 3)
    (print-visicon)
    
    ;; run the model to show the updated
    (run 10)
    
    ;; delete all of the visicon features.  delete-all-visicon-features
    ;; removes all of the features from the visicon.
    
    (delete-all-visicon-features)
    
    ;; Give the vision module a chance to process the display
    ;; before printing the visicon.
    (run-n-events 3)
    (print-visicon)
    
    ;; run the model to show the updated
    (run 10)))
  
  

;;; The model is very simple in that it just repeatedly finds
;;; a location and then attends to the item there printing
;;; out the chunks in the visual-location and visual buffers
;;; after the corresponding requet completes.
;;; The ordering of the modifications are such that it will
;;; not automatically updated the attended object chunks 
;;; since the currently attended item is unchanged in each
;;; case.

(load-act-r-model "ACT-R:examples;vision-module;adjust-visicon-features-model.lisp")

