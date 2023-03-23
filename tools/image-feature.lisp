;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2018 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : image-feature.lisp
;;; Version     : 1.1
;;; 
;;; Description : Provides an image item for the virtual windows of the AGI which
;;;               can display a .gif through the visible virtual windows.
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 18.01.31 Dan Bothell
;;;             : * This was an example previously, but since it has too much
;;;             :   "internal" code now it's not suitable as an example and I'm
;;;             :   just rolling it into the sources.  It does however get tied
;;;             :   in through some extensions I've added which makes it an 
;;;             :   example for me of using those extensions.
;;; 18.03.14 Dan
;;;             : * Update the build-vis-locs-for method since that doesn't 
;;;             :   get the vision module any more.
;;; 18.03.19 Dan
;;;             : * Add the window position to make visual-location in gloabl
;;;             :   coordinates not local.
;;; 18.04.27 Dan
;;;             : * Copied the code for modifying buttons in here as a reference,
;;;             :   but not yet adding a modify-image... because it doesn't have
;;;             :   the subclass for a visible virtual window yet.
;;; 18.05.02 Dan
;;;             : * Updated because of new add-visicon-features spec.
;;; 18.06.14 Dan
;;;             : * Don't need to double-quote strings for the visicon features.
;;; 18.06.22 Dan
;;;             : * Use keyword params instead of optional and options list for
;;;             :   remote versions.
;;; 18.07.17 Dan
;;;             : * Fix a bug with the call to create-image-... in add-image-...
;;; 2018.08.03 Dan
;;;             : * Updated the click handler to test things like the button
;;;             :   handler does.
;;; 2019.08.27 Dan
;;;             : * Use new-symbol-fct instead of new-name-fct because it can be
;;;             :   used when there isn't a current model.
;;; 2021.05.11 Dan
;;;             : * Changed reference to GUI directory to gui to avoid issues
;;;             :   with logical pathnames (particularlly in SBCL).
;;; 2022.03.02 Dan [1.1]
;;;             : * Added the :clickable keyword parameter to create-image-...
;;;             :   and add-image-... so that one can turn off the response to
;;;             :   clicks if needed, for example when placing other items that
;;;             :   are clickable (like buttons) overlapping an image.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Create a new virtual dialog item called an image which can be used to draw
;;; .gif files in the visible virtual windows if those images are located in the
;;; gui/AGI-images directory of the Environment.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Two functions are provided for using them like the other AGI elements:
;;; 
;;; create-image-for-exp-window (win text file &key (x 0) (y 0) (width 50) (height 50) (action nil) (clickable t))
;;;
;;; and 
;;; 
;;; add-image-to-exp-window (win text file &key (x 0) (y 0) (width 50) (height 50) (action nil) (clickable t))
;;;
;;; Win is the window reference (as with other AGI items), text is a string which
;;; will be the value the model sees for attending the item.  File should be the
;;; name of a file in the gui/AGI-images directory of the Environment to display
;;; in the visible virtual window.  X and y are the coordinates for the upper left
;;; corner of the image in the window.  Width and height specify the size of the
;;; image for display purposes and the .gif file will be clipped to fit if 
;;; needed (it does not scale, center, or otherwise attempt to fit that space).
;;; If clickable is specified as non-nil (the default) then an action function
;;; can be set, otherwise the image will ignore mouse clicks. Action specifies a 
;;; user function to call when the mouse is clicked within the boundaries of the
;;; image, and it can be specified the same way as button actions:
;;; a valid command string, a Lisp function, a symbol naming a Lisp function, or
;;; a list where the first item is a command/function and the remaining items
;;; are values which will be passed to that function.  If it is not a list, then
;;; the command/function will be passed two items.  The text for the image object
;;; and the relative position within the image where it was clicked as a 2
;;; element list.
;;;
;;;
;;; These calls (assuming there is only one open experiment window):
;;;
;;;    (add-image-to-exp-window nil "logo" "smalllogo.gif" :x 10 :y 10 :width 288 :height 142)
;;;   
;;;    (add-items-to-exp-window win (create-image-for-exp-window nil "brain" "ref-brain.gif" :x 10 :y 160 :width 128 :height 128))
;;;
;;; will result in these visicon features:
;;;
;;; Name              Att  Loc             Height  Width  Kind   Value    Size      
;;; ----------------  ---  --------------  ------  -----  -----  -------  ----------
;;; VISUAL-LOCATION1  NEW  ( 74 224 1080)  128     128    IMAGE  "brain"  46.0      
;;; VISUAL-LOCATION0  NEW  (154  81 1080)  142     288    IMAGE  "logo"   114.259995
;;;
;;; and these visual chunks for the second item:
;;; 
;;; VISUAL-LOCATION0-0-0
;;;    KIND  IMAGE
;;;    VALUE  IMAGE
;;;    HEIGHT  142
;;;    WIDTH  288
;;;    DISTANCE  1080
;;;    SCREEN-X  154
;;;    SCREEN-Y  81
;;;    SIZE  114.259995
;;; 
;;; 
;;; IMAGE0-0
;;;    SCREEN-POS  VISUAL-LOCATION0-0-0
;;;    VALUE  "logo"
;;;    HEIGHT  142
;;;    WIDTH  288
;;;    DISTANCE  1080
;;;    IMAGE  T
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Implemented using the extension mechanisms that I added to the AGI and
;;; Environment visible-virtual window code to make it possible to add new items
;;; without having to redefine things as was needed previously.
;;; This goes along with the 999-image-item.tcl file to provide the corresponding
;;; display support.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; The first step for creating a new virtual feature is to create a subclass
;;; of the virtual-dialog-item and rpm-dialog-item classes.  Any new slots 
;;; necessary for that class can be added, and it should automatically create
;;; a unique id.

(defclass image-vdi ( virtual-dialog-item)
 
  ((file :accessor file :initform nil :initarg :file))
  
  (:default-initargs 
    :id (string-downcase (symbol-name (new-symbol-fct "IMAGE-VDI"))) ;; make sure it gets a unique name
    :handles-click-p t             ;; indicate that this item can be clicked on by a model
    :action nil)) 

  
(defun default-image-action (text position)
  (print-warning "Image ~s with no valid action clicked at position ~d,~d at time ~S." text (first position) (second position) (mp-time)))


;;; The function for creating one which handles all the AGI setup necessary for
;;; recording and accessing the underlying object cleanly.

(defun create-image-for-exp-window (win text file &key (x 0) (y 0) (width 50) (height 50) (action nil) (clickable t))
  "Creates an image item for the experiment window"
  
  ;(when (and win (symbolp win))
  ;  (setf win (symbol-name win)))
  
  ; should be more error checking...
  
  (let ((agi (agi-component)))
    (multiple-value-bind (window key) (determine-exp-window agi win)
      (if window
          (let* ((item (make-instance 'image-vdi
                         :x-pos x
                         :y-pos y
                         :dialog-item-text text
                         :action (when clickable 
                                   (if (valid-button-action action) 
                                       action
                                     'default-image-action))
                         :handles-click-p clickable
                         :height height
                         :width width 
                         :file file
                         :lock (window-lock window)))
                 (item-key (list key (string (id item)))))
              
            (if (and agi item)
                (bt:with-recursive-lock-held ((agi-lock agi))
                  (setf (gethash item-key (global-agi-table agi)) item)
                  (push item (gethash key (window-objects-table agi)))
                  
                  item-key)
              (print-warning "Problem creating image item.  No image created.")))
        (print-warning "No window available for creating an image.")))))

(defun external-create-image-for-exp-window (win text file &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'create-image-for-exp-window '(:x :y :height :width :action :clickable))
    (when valid 
      (apply 'create-image-for-exp-window win text file ol))))

(add-act-r-command "create-image-for-exp-window" 'external-create-image-for-exp-window "Create an image item for the provided experiment window with the features specified. Params: window text file  {< x, y, width, height, action, clickable >}.")

(defun add-image-to-exp-window (win text file &key (x 0) (y 0) (width 50) (height 50) (action nil) (clickable t))
  "Create and display an image item in the experiment window"
  
  (when (and win (symbolp win))
    (setf win (symbol-name win)))
  
  (let ((item-key (create-image-for-exp-window win text file :x x :y y :width width :height height :action action :clickable clickable)))
    (if item-key
        (if (add-items-to-exp-window win item-key)
            item-key
          nil)
      nil)))

(defun external-add-image-to-exp-window (win text file &optional params)
  (multiple-value-bind (valid ol) 
      (process-options-list params 'add-image-to-exp-window '(:x :y :height :width :action :clickable))
    (when valid 
      (apply 'add-image-to-exp-window win text file ol))))
  
(add-act-r-command "add-image-to-exp-window" 'external-add-image-to-exp-window "Create an image item for the provided experiment window with the features specified and place it in the window. Params: window text file {<x, y, width, height, action, clickable >}.")

;;; The vv-click-event-handler is the method which must be written
;;; to actually process the click which the model produces.  This is
;;; the internal virtual window method that gets called and we use
;;; that to call the action function for the image-vdi item.


(defmethod vv-click-event-handler ((self image-vdi) position)
  (if (listp (action-function self))
      (cond ((stringp (first (action-function self)))
             (multiple-value-bind (success result)
                 (apply 'evaluate-act-r-command (first (action-function self)) (rest (action-function self)))
               (unless success
                 (print-warning "Image click action ~s reported error ~s." (action-function self) result))))
            ((or (functionp (first (action-function self)))
                 (and (symbolp (first (action-function self))) 
                      (fboundp (first (action-function self)))
                      (not (macro-function (first (action-function self))))))
             (apply (first (action-function self)) (rest (action-function self))))
            (t
             (default-image-action (dialog-item-text self) 
                 (list (- (px position) (x-pos self))
                       (- (py position) (y-pos self))))))
    (cond ((stringp (action-function self))
           (multiple-value-bind (success result)
               (apply 'evaluate-act-r-command (action-function self) (list (dialog-item-text self) 
                                                                           (list (- (px position) (x-pos self))
                                                                                 (- (py position) (y-pos self)))))
               (unless success
                 (print-warning "Image click action ~s reported error ~s." (action-function self) result))))
          ((or (functionp (action-function self))
               (and (symbolp (action-function self)) 
                    (fboundp (action-function self))
                    (not (macro-function (action-function self)))))
           (apply (action-function self) (list (dialog-item-text self) 
                                               (list (- (px position) (x-pos self))
                                                     (- (py position) (y-pos self))))))
          (t
           (default-image-action (dialog-item-text self) 
               (list (- (px position) (x-pos self))
                     (- (py position) (y-pos self))))))))
    


;;; The build-vis-locs-for method is typically written for the
;;; whole device, but the virtual windows in ACT-R extend that so
;;; that the device actually calls that method for each of the
;;; items in the window to create the features.  It needs to 
;;; return the feature list for the visual information desired.

(defmethod build-vis-locs-for ((self image-vdi))
  (let* ((top (find-top-level-window self))
         (top-xy (bt:with-recursive-lock-held ((window-lock top)) (vector (x-pos top) (y-pos top)))))
           
    
    (unless (chunk-p image)
      (define-chunks (image name image))) 
    
    (unless (chunk-type-p image)
      (chunk-type (image (:include visual-object)) (image t)))
    
    `((isa (visual-location image)
       value (image ,(format nil "~a" (dialog-item-text self)))
       screen-x ,(+ (px top-xy) (x-pos self) (round (width self) 2))
       screen-y ,(+ (py top-xy) (y-pos self) (round (height self) 2))
       width ,(width self)
       height ,(height self)))))
  

;;; This method is used by the extension to the visible-virtual handler
;;; to create the feature list that is sent to the Environment.  It
;;; needs an appropriate handler defined to process it (which is in the
;;; 999-image-item.tcl file).

(defmethod env-window-features ((item image-vdi))
  (list "image" (list (id item) (x-pos item) (y-pos item)
                      (file item) (width item) (height item))))



#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#

