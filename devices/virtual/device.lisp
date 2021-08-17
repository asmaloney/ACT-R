;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne & Dan Bothell
;;; Address     : Rice University, MS-25
;;;             : Psychology Department
;;;             : Houston,TX 77251-1892
;;;             : byrne@acm.org
;;; 
;;; Copyright   : (c)2000-2004 Mike Byrne
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : virtual-view.lisp
;;; Version     : 1.0
;;; 
;;; Description : Instantiates "virtual views" so that RPM can act on things
;;;             : other than MCL windows.
;;; 
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;; 00.06.08 Mike Byrne
;;;             :  Date for new header.
;;; 00.09.13 mdb
;;;             : Updated for vectors.
;;; 00.11.15 mdb
;;;             : Fixed minor compiler warnings, cleaned up some lingering
;;;             : vector bugs.
;;; 01.07.03 mdb
;;;             : Added defgenerics and doc strings.
;;; 02.01.15 Dan
;;;             : Changed the declaration of ignore to
;;;             : ignore-if-unused for the subviews method
;;;             : to eliminate an ugly warning in ACL.  
;;; 02.02.28 Dan
;;;             : changed vv-click-event-handler because
;;;             : functionp doesn't gurantee that the function is
;;;             : returned as the true value.
;;; 02.06.21 Dan [b7]
;;;             : Added the rpm-window class as part of the reorganization
;;;             : of internal window classes.
;;;             : Changed the #+:mcl to better work with openmcl.
;;; 02.06.30 Dan
;;;             : Moved the view based line support in to here.
;;;             : Moved the populate-loc-to-key-array from generic-interface
;;;             : to here.
;;;             : Took the UWI code out of here.
;;; 02.12.19 Dan 
;;;             : Added an around method for text items to handle color.
;;; 04.04.13 Dan [2.2]  (the previous update is "new" as of 2.2 as well)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;;             : Changed vw-output to show in the trace and not in the
;;;             :  a window specific stream
;;;             :
;;;             : depends on the vision module and dmi, but that seems
;;;             : reasonable for now.
;;; 2005.05.11 Dan
;;;             : * Added a check of :vwt to vw-output so that it can be
;;;             :   easily shut off (which is actually the default for now).
;;; 2006.01.16 Dan
;;;             : * Discovered that the hash-table implementation of the
;;;             :   virtual-view subviews can lead to non-repeatable performance
;;;             :   of tutorial models (both between different Lisps or even
;;;             :   within a single Lisp!).  So, for now at least, the 
;;;             :   build-features-for method for a virtual-window will sort
;;;             :   the features based on xy coordinates.  That way the tutorial
;;;             :   model results will remain consistent for all systems that
;;;             :   use the virtuals (any hooked up to the environment).
;;; 2006.09.07 Dan
;;;             : * Modified the build-features-for method on the virtual-
;;;             :   windows so that it calls fill-default-dimensions.
;;; 2007.06.21 Dan
;;;             : * Converted things over to use the new chunk based device
;;;             :   specification.
;;; 2007.07.05 Dan
;;;             : * No need to sort the features being returned because the
;;;             :   hash-table inside vision will mess it up anyway...
;;; 2007.08.23 Dan
;;;             : * Fixed build-vis-locs-for of buttons so that the text 
;;;             :   is located at the same point as the oval (bug carried
;;;             :   over from the old stuff).
;;; 2008.01.08 Dan
;;;             : * Put the sorting code back in build-vis-locs-for because 
;;;             :   without it there's no consistency in the names of the 
;;;             :   visual-location chunks for testing and verification purposes.
;;; 2008.01.09 Dan
;;;             : * Adding a little more to the sorting method so that co-located
;;;             :   items of different types also get a specific ordering.
;;; 2008.04.11 Dan [1.0]
;;;             : * Using the new parameter :stable-loc-names in the build-vis-
;;;             :   locs-for method on virtual-windows to check whether the 
;;;             :   subview items should be sorted.
;;;             : * Also took the package setting out of the mode line at the
;;;             :   top since there isn't a specific package for any of the
;;;             :   source files.
;;; 2008.04.14 Dan
;;;             : * Modifying the objects and build-vis-locs-for methods to
;;;             :   only create 1 chunk per item across proc-display calls.
;;;             :   That makes it compatible with the :test-feats nil setting.
;;;             :   Doesn't hold when :optimize-visual is nil or text items
;;;             :   contain multiple "words" i.e. only compatible when each text
;;;             :   item produces only one feature.
;;; 2008.04.15 Dan
;;;             : * Fixed a bug with the line views.  Without a specific 
;;;             :   point-in-vv-p the default misinterprets their size values
;;;             :   and could consider clicks which didn't overlap the line
;;;             :   to be over the line and not be passed on to the "real" item
;;;             :   which should be clicked.  Now, lines are not clickable at
;;;             :   all.
;;;             : * Took all the "the fixnum" declarations out of point-in-vv-p
;;;             :   because nothing forces the mouse position to be a fixnum and
;;;             :   when visual items have an odd dimension the mouse position
;;;             :   can get set to a rational.  In LispWorks point-in-vv-p does 
;;;             :   not work if the values are rationals declared as fixnums.
;;; 2008.07.01 Dan
;;;             : * Added code to the build-vis-locs-for methods to purge any
;;;             :   old chunks when possible.
;;;             : * Also purge them in remove-subviews so that when the item is
;;;             :   cleared from a window they're also removed.
;;; 2010.08.11 Dan
;;;             : * Changed view-loc to round the divisions to of height and width
;;;             :   to make sure things stay fixnums since an odd width button
;;;             :   causes problems in build-string-feats for Lisps that care
;;;             :   about the fixnum delcaration.
;;; 2011.04.28 Dan
;;;             : * Changed an ignore-if-unused declaration to ignorable.
;;; 2012.06.20 Dan
;;;             : * Changed loc-chunks to be a hash-table referenced by a cons
;;;             :   of meta-process and model names since multiple models may
;;;             :   be viewing the same item and they need to keep their chunks
;;;             :   separate.  Now the chunks are stored as the values of the
;;;             :   table for the appropriate mp and model and always in a list.
;;; 2014.01.24 Dan
;;;             : * Updated the build-vis-locs-for methods for text and button
;;;             :   items so that they use the updated build-string-feats to
;;;             :   deal with newlines in the text.
;;;             : * The spacing for lines in virtual items is the max of 
;;;             :   (* (text-height item) 1.25) (1+ (text-height item)).
;;; 2014.02.07 Dan
;;;             : * Why have virtual static text items always assumed a centered
;;;             :   y justification when all the other devices assume top?  It
;;;             :   now assumes top justification like everything else for 
;;;             :   consistency which will probably break lots of models...
;;; 2014.02.11 Dan
;;;             : * Removed the lines variable from the text build-vis-locs-for
;;;             :   to avoid a warning since it's not needed.
;;; 2014.05.19 Dan
;;;             : * Fixed the use of chunk-type-slot-names by using chunk-filled-
;;;             :   slots-list instead.
;;; 2014.08.29 Dan
;;;             : * Changed vv-click-event-handler for buttons so that the action 
;;;             :   can be either a function or a symbol naming a function.
;;; 2015.05.20 Dan
;;;             : * The approach-width call in the buttons build-vis-locs-for
;;;             :   method needs to pass the vision module in too.
;;; 2015.12.17 Dan
;;;             : * Changed the name of the key at 19,2 from esc to clear to
;;;             :   actually match the reference image since the model now has
;;;             :   keypad actions.
;;; 2016.06.16 Dan
;;;             : * When a button or line changes the size of the location now
;;;             :   gets updated along with the other features.
;;; 2016.11.17 Dan
;;;             : * The code for storing and removing the loc chunks was using
;;;             :   the meta-process name, but that's not needed (or available)
;;;             :   anymore.
;;; 2017.01.05 Dan
;;;             : * Removed the loc-chunks slot altogether and removed some of
;;;             :   the old device methods.
;;;             : * Also removed methods that weren't used anywhere else like
;;;             :   set-view-size and set-view-position.
;;; 2017.01.06 Dan
;;;             : * Stripped some other unnecessary code like close and select.
;;;             : * The build-vis-locs-for methods now just return the feature
;;;             :   lists as needed for the add-visicon-features command and it's
;;;             :   up to the agi to handle the actual adding and recording of
;;;             :   the chunk returned by vision.
;;;             : * Add-subviews now warns if an item already has a container
;;;             :   and doesn't put it into the new window.
;;; 2017.01.30 Dan
;;;             : * Add-act-r-command call parameters reordered.
;;; 2017.02.09 Dan
;;;             : * Updated the build-vis-locs-for for lines to specify isa 
;;;             :   line-location since it needs the end* slots, and added that
;;;             :   chunk-type to vision module.
;;; 2017.03.02 Dan
;;;             : * Changed device-handle-click to vv-handle-click to avoid
;;;             :   issues since it now gets passed the click point.
;;;             : * Fixed a bug in the vv-click-event-handler.
;;; 2017.03.03 Dan
;;;             : * Changed the way the button's action function is specified.
;;;             :   Now it can be:
;;;             :     - a function or dispatch command string
;;;             :        That function or command will be called with no parameters
;;;             :     - a list where the first is as above
;;;             :        That function or command will be called with the rest
;;;             :        of the list as its parameters
;;;             :   Also, the validity of that item is only tested when the
;;;             :   button is created and not before each execution, but if an error
;;;             :   is reported by the dispatch call it will report that.
;;; 2017.03.08 Dan
;;;             : * Adding back the device-move-cursor-to method because I need
;;;             :   something to call to send the updates to the environment and
;;;             :   that's where it was before.
;;; 2017.05.31 Dan
;;;             : * Use new-symbol instead of new-name so there doesn't need
;;;             :   to be a current model to create GUI elements.
;;; 2017.06.19 Dan
;;;             : * Remove the unused optional parameter from subviews.
;;;             : * Removed the build-vis-locs-for methods.
;;; 2018.03.14 Dan
;;;             : * Just set :handles-click-p to nil for lines instead of having
;;;             :   a dummy point-in-vv-p method for them.
;;; 2020.01.10 Dan 
;;;             : * Removed #' from lambdas since not using it other places either.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;;; ---------------------------------------------------------------------- ;;;;
;;;; Base Virtual View class.

(defclass virtual-view ()
  ((view-subviews :accessor view-subviews :initarg :subviews
                  :initform (make-hash-table))
   (x-pos :accessor x-pos :initform nil :initarg :x-pos)
   (y-pos :accessor y-pos :initform nil :initarg :y-pos)
   (width :accessor width :initform nil :initarg :width)
   (height :accessor height :initform nil :initarg :height)
   (id :accessor id :initarg :id :initform (new-symbol-fct "VV"))
   (handles-click-p :accessor handles-click-p :initform t 
                    :initarg :handles-click-p)
   (view-container :accessor view-container :initform nil)
   (color :accessor color :initarg :color :initform 'black)
   (visual-features :accessor visual-features :initform nil)))

#+(or (not :mcl) :openmcl)
(defgeneric subviews (view)
  (:documentation "Returns a list of subviews of <view>."))

(defmethod subviews ((vv virtual-view))
  (let (accum)
    (maphash (lambda (x y) (declare (ignore x)) 
                 (push y accum)) 
             (view-subviews vv))
    accum))

(defmethod view-loc ((vv virtual-view))
  (vector (+ (x-pos vv) (round (width vv) 2))
          (+ (y-pos vv) (round (height vv) 2))))


#+(or (not :mcl) :openmcl)
(defgeneric add-subviews (view &rest subviews)
  (:documentation  "Add subviews to <view>."))

(defmethod add-subviews ((vv virtual-view) &rest subviews)
  (dolist (sub subviews)
    (if (view-container sub)
        (print-warning "Item ~s is already in window ~s so it can't be added to window ~s" sub (view-container sub) vv)
      (progn
        (setf (gethash (id sub) (view-subviews vv)) sub)
        (setf (view-container sub) vv)))))


#+(or (not :mcl) :openmcl)
(defgeneric remove-subviews (view &rest subviews)
  (:documentation  "Remove subviews from <view>."))

(defmethod remove-subviews ((vv virtual-view) &rest subviews)
  (dolist (sub subviews)
    (if (gethash (id sub) (view-subviews vv))
        (progn
          (remhash (id sub) (view-subviews vv))
          (setf (view-container sub) nil)
          
          )
      (print-warning "Item ~s is not in window ~s so it can't be removed from it" sub vv))))

(defgeneric point-in-vv-p (vview point)
  (:documentation  "Determine if the supplied point is inside the supplied view."))

(defmethod point-in-vv-p ((vv virtual-view) point)
  (let ((x (px point))
        (y (py point)))
    (and (>= x (x-pos vv))
         (>= y (y-pos vv))
         (<= x (+ (x-pos vv) (width vv)))
         (<= y (+ (y-pos vv) (height vv))))))


(defgeneric vv-click-event-handler (vview point)
  (:documentation  "Handle a click in <vview> at point <point>."))

(defmethod vv-click-event-handler ((vv virtual-view) point)
  (declare (ignore point))
  nil)


;;;; ---------------------------------------------------------------------- ;;;;
;;;; Virtual Window class and methods.

(defclass virtual-window (virtual-view)
  ((window-title :accessor window-title :initarg :window-title
                 :initform "Virtual Window")
   )
  (:default-initargs
    :x-pos 0
    :y-pos 0
    :id (new-symbol-fct "VW")
    ))

(defmethod device-move-cursor-to ((vw virtual-window) (loc vector))
  )

(defun subview-sort (i1 i2)
  (and (numberp (x-pos i1))
       (numberp (x-pos i2))
       (numberp (y-pos i1))
       (numberp (y-pos i2))
       (or
        (< (x-pos i1) (x-pos i2))
        (and (= (x-pos i1) (x-pos i2))
             (< (y-pos i1) (y-pos i2)))
        (and (= (x-pos i1) (x-pos i2))
             (= (y-pos i1) (y-pos i2))
             (string< (symbol-name (type-of i1)) (symbol-name (type-of i2)))))))


(defmethod vv-handle-click ((vw virtual-window) (pos vector))
  (dolist (sub (subviews vw))
    (when (and (handles-click-p sub)
               (point-in-vv-p sub pos))
      (vv-click-event-handler sub pos)
      (return-from vv-handle-click t))))


#+(or (not :mcl) :openmcl)
(defgeneric view-window (view)
  (:documentation  "Returns the window associated with <view> (if any), or <view> if <view> is a window."))

(defmethod view-window ((vw virtual-window))
  vw)

;;;; ---------------------------------------------------------------------- ;;;;
;;;; View classes:  static-text-vv, button-vv, 


(defclass virtual-dialog-item (virtual-view)
  ((text :accessor dialog-item-text :initform "Untitled" 
         :initarg :dialog-item-text)
   (action-function :accessor action-function :initarg :action :initform nil)
   (text-height :accessor text-height :initarg :text-height :initform 10)
   (str-width-fct :accessor str-width-fct :initarg :str-width-fct
                  :initform (lambda (str)
                                (* 7 (length str))))
   (lock :initform nil :accessor dialog-item-lock :initarg :lock))
  (:default-initargs
       :height 18
       :width 60
       :subviews nil))


(defmethod subviews ((vdi virtual-dialog-item))
  (declare (ignorable vdi))
  nil)


(defmethod view-window ((vdi virtual-dialog-item))
  (let ((vc (view-container vdi)))
    (if (null vc)
      nil
      (if (typep vc 'virtual-window)
        vc
        (view-window vc)))))


(defclass static-text-vdi (virtual-dialog-item)
  ()
  (:default-initargs
      :id (new-symbol-fct "TEXT-VDI")
      :handles-click-p nil))




(defclass button-vdi (virtual-dialog-item)
  ()
  (:default-initargs
       :id (new-symbol-fct "BUTTON-VDI")
       :action "default-button-action"))


(defmethod vv-click-event-handler ((btn button-vdi) where)
  (declare (ignore where))
  (if (listp (action-function btn))
      (cond ((stringp (first (action-function btn)))
             (multiple-value-bind (success result)
                 (apply 'evaluate-act-r-command (first (action-function btn)) (rest (action-function btn)))
               (unless success
                 (print-warning "Button click action ~s reported error ~s." (action-function btn) result))))
            ((or (functionp (first (action-function btn)))
                 (and (symbolp (first (action-function btn))) 
                      (fboundp (first (action-function btn)))
                      (not (macro-function (first (action-function btn))))))
             (apply (first (action-function btn)) (rest (action-function btn))))
            (t
             (default-button-action)))
    (cond ((stringp (action-function btn))
           (multiple-value-bind (success result)
                 (evaluate-act-r-command (action-function btn))
               (unless success
                 (print-warning "Button click action ~s reported error ~s." (action-function btn) result))))
          ((or (functionp (action-function btn))
               (and (symbolp (action-function btn)) 
                    (fboundp (action-function btn))
                    (not (macro-function (action-function btn)))))
           (funcall (action-function btn)))
          (t
           (default-button-action)))))



(defun default-button-action ()
  (print-warning "Button with no valid action clicked at time ~S." (mp-time)))

(add-act-r-command "default-button-action" 'default-button-action "Function called for a button with no action. Do not call directly." nil)


;;; The base class for the view based lines. 

(defclass v-liner (virtual-dialog-item)
  ()
  (:default-initargs
      :handles-click-p nil))


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
