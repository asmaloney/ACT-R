;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : uniform-interface-virtual.lisp
;;; Version     : 2.0
;;; 
;;; Description : wirtual view functions to implement the UWI.
;;;             : NOTE: The UWI is only still around to support the 
;;;             :       ACT-R GUI interface. I don't advocate using it directly.      
;;; 
;;; Bugs        : 
;;; --- History ---
;;; 2002.06.30 Dan
;;;             : Added this header.
;;;             : Moved all of the UWI code from virtual-view 
;;;             : to this file where it belongs.
;;;             : Added the visible-virtuals-avaialable? function.
;;;             : Actually documented the code and added defgenerics with
;;;             : documentation for the uwi.
;;; 2002.12.19 Dan
;;;             : Modified make-static-text-for-rpm-window to take a
;;;             : color parameter.
;;; 04.04.13 Dan [2.2] (previous change is "new" as of 2.2 as well)
;;;             : Changed the copyright notice and added the LGPL stuff.
;;;
;;; 04.10.19 Dan [Moved into ACT-R 6]
;;;             : Reset the version to 1.0a1
;;;             : added the packaging switches
;;; 2007.07.13 Dan
;;;             : * Adding color as an option for button items in make-
;;;                 button-for
;;; 2008.07.01 Dan
;;;             : * Added code so that closing a virtual window will force all
;;;             :   the associated chunks to be purged:
;;;             :    -  closing a virtual window will first remove all of its
;;;             :       objects.
;;;             :    -  if it has a loc-chunk (a cursor) it is purged.
;;; 2012.06.20 Dan
;;;             : * The loc-chunk slot now holds a hash-table since multiple
;;;             :   models could be sharing the window and each will have its
;;;             :   own cursor chunk.
;;; 2012.10.02 Dan
;;;             : * Make-rpm-window now uses print warning to report when there
;;;             :   isn't a visible window available when one is requested and
;;;             :   returns a virtual on instead.
;;; 2013.01.10 Dan
;;;             : * Visible-virtuals-available? now just calls check-with-environment-for-visible-virtuals
;;;             :   if it's defined to avoid having to redefine visible-virtuals-available?
;;;             :   it later.
;;; 2015.05.26 Dan
;;;             : * Added a font-size parameter to make-static-text-for-rpm-window.
;;;             :   Should be a point size for the font, defaults to 12 and the
;;;             :   height and width are based on the ratios for the previous
;;;             :   defaults which were 10 high and 7 wide for 12 point.
;;; 2016.06.08 Dan
;;;             : * Allow the class in make-rpm-window to override visible-
;;;             :   virtual-windows if it's a subtype of that class.
;;;             : * Add the modify-*-for-rpm-window methods which just directly
;;;             :   change the slots of the object since that's all a virtual
;;;             :   item needs (the device code modifies the underlying feature
;;;             :   chunks as needed).
;;; 2016.06.09 Dan
;;;             : * Properly change the action of a button in the modify.
;;; 2017.01.05 Dan
;;;             : * Add the installed slot to rpm-window class.
;;;             : * Removed device methods.
;;; 2017.01.06 Dan
;;;             : * More adjustments with the assumption that virtual windows
;;;             :   are the only interface for the AGI and nobody will be trying
;;;             :   to treat them like MCL windows.
;;; 2017.06.01 Dan
;;;             : * Remove the installed slot because that needs to be tracked by
;;;             :   the AGI since a window could be installed for more than 1 model.
;;; 2017.06.07 Dan
;;;             : * Adding a lock to the window object and using that in the add,
;;;             :   remove, and modify actions (technically modify should probably
;;;             :   be a lock on the item itself, but just using the window lock
;;;             :   for now).
;;; 2017.06.08 Dan
;;;             : * Removed the lock use from the items since the modify actions
;;;             :   don't change the window and are single-instance.
;;;             : * Fixed the with-act-r-lock usage since it uses a simpler 
;;;             :   syntax than the underlying with-lock.
;;;             : * Have each window create a lock with a unique name.
;;; 2017.06.14 Dan
;;;             : * Put the lock checks back into the modify actions since the
;;;             :   AGI commands can be called directly from Lisp which bypasses
;;;             :   the single instance tests. 
;;;             : * Added a lock test to close-rpm-window around the window-open?
;;;             :   setting.
;;; 2017.06.16 Dan
;;;             : * Use a raw lock instead of an act-r lock for the window and
;;;             :   redefine some of the virtual-window methods on the rpm-
;;;             :   class so that the lock can be tested.  Also make it recursive
;;;             :   because add-subviews should test it but that happens in the
;;;             :   context of other things too.
;;;             : * Create new classes for the dialog items so that they can
;;;             :   have a lock slot which gets set when they are created to
;;;             :   be the parent window's lock.
;;;             : * Moved the build-vis-locs-for methods here since they're
;;;             :   defined on the subclasses to be thread safe.
;;; 2017.06.23 Dan
;;;             : * Added a name for the window's lock.
;;; 2017.08.16 Dan
;;;             : * Need to release the window's lock in the vv-handle-click
;;;             :   method before calling vv-click-event-handler because that
;;;             :   might end up calling a dispatched function to modify the
;;;             :   window.
;;; 2017.09.08 Dan
;;;             : * Updated the warning about visible virtuals not available.
;;; 2018.03.13 Dan [2.0]
;;;             : * Build-vis-locs-for methods don't need vision module now!
;;;             : * Vv-handle-click now needs to check the window and then 
;;;             :   convert from global to local for testing subviews.
;;; 2018.03.15 Dan
;;;             : * Simplifying the class hierarcy because the UWI doesn't
;;;             :   really need the rpm-dialog-item and rpm-window classes
;;;             :   at this point or the rpm-*-item classes.
;;; 2018.03.27 Dan
;;;             : * Moved find-top-level-window here from the agi file.
;;; 2018.05.02 Dan
;;;             : * Update the feature creation to match the new add-visicon-features
;;;             :   specification.
;;; 2018.05.04 Dan
;;;             : * Updated the modify calls as well.
;;; 2018.06.13 Dan
;;;             : * Button text doesn't need to double quote the value.
;;; 2018.08.06 Dan
;;;             : * Line locations can just use visual-location instead of line-
;;;             :   location now.
;;; 2018.09.13 Dan
;;;             : * Add the send-update-to-handlers method which does nothing
;;;             :   for virtual windows.
;;; 2019.10.01 Dan
;;;             : * Fixed the typo in the monitor-move-cursor slot name.
;;; 2020.05.01 Dan (from Mike)
;;;             : * Changed the line features to include the orientation in the
;;;             :   value slot of both chunks.
;;; 2020.12.10 Dan
;;;             : * Added an external command for visible-virtuals-available?.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)



;;; RPM-VIRTUAL-WINDOW  [Class]
;;; Description : This is the UWI's window class to produce a virtual window.

(defclass rpm-virtual-window (virtual-window)
  ((monitor-attended-loc :accessor monitor-attended-loc :initform nil)
   (monitor-click-mouse :accessor monitor-click-mouse :initform nil)
   (monitor-move-cursor :accessor monitor-move-cursor :initform nil)
   (monitor-delete-cursor :accessor monitor-delete-cursor :initform nil)
   (cursor-drawn :accessor cursor-drawn :initform nil)
   (open :initform t :accessor window-open?)
   (lock :initform (bt:make-recursive-lock "virtual-window-lock") :accessor window-lock :initarg :lock)))


;;;; ---------------------------------------------------------------------- ;;;;
;;;; These are the UWI Methods.
;;;; ---------------------------------------------------------------------- ;;;;

(defun find-top-level-window (item)
  (do ((w item (view-container w)))
      ((null (view-container w)) w)))


;;; OPEN-RPM-WINDOW?  [Method]
;;; Description : Returns t if the window is open and nil if not.

(defgeneric open-rpm-window? (window)
  (:documentation  "Returns t if the window is currently open"))

(defmethod open-rpm-window? ((win rpm-virtual-window))
  (bt:with-recursive-lock-held ((window-lock win))
    (window-open? win)))

(defmethod open-rpm-window? ((win null))
  nil)


(defgeneric send-update-to-handlers (window params)
  (:documentation "Returns t if the window is visible"))

(defmethod send-update-to-handlers ((win rpm-virtual-window) params)
  (declare (ignorable win params)))


;;; CLOSE-RPM-WINDOW  [Method]
;;; Description : Closes the window.

(defgeneric close-rpm-window (window)
  (:documentation  "Close an rpm-window"))

(defmethod close-rpm-window ((win rpm-virtual-window))
  (remove-all-items-from-rpm-window win)
  (bt:with-recursive-lock-held ((window-lock win))
    (setf (window-open? win) nil)))

;;; SELECT-RPM-WINDOW  [Method]
;;; Description : Brings the specified window to the foreground.

(defgeneric select-rpm-window (window)
  (:documentation "Bring an rpm-window to the front"))

(defmethod select-rpm-window ((win rpm-virtual-window))
  nil)

;;; ADD-VISUAL-ITEMS-TO-RPM-WINDOW  [Method]
;;; Description : Makes the specified items subviews of the window and
;;;             : calls view-draw-contents and event-dispatch to make sure
;;;             : that they show up.

(defgeneric add-visual-items-to-rpm-window (window &rest x)
  (:documentation "Add items to the rpm-window for display"))

(defmethod add-visual-items-to-rpm-window ((win rpm-virtual-window) &rest items)
  (bt:with-recursive-lock-held ((window-lock win))
    (dolist (item items)
      (add-subviews win item))))


;;; REMOVE-VISUAL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Take the specified items out of the subviews of the
;;;             : window and make it redraw.

(defgeneric remove-visual-items-from-rpm-window (window &rest x)
  (:documentation "Remove items from the display in the window"))

(defmethod remove-visual-items-from-rpm-window ((win rpm-virtual-window) &rest items)
  (bt:with-recursive-lock-held ((window-lock win))
    (dolist (item items)
      (remove-subviews win item))))


;;; REMOVE-ALL-ITEMS-FROM-RPM-WINDOW  [Method]
;;; Description : Remove all the subvies of the window and redisplay it.

(defgeneric remove-all-items-from-rpm-window (window)
  (:documentation "Remove all items from the window"))

(defmethod remove-all-items-from-rpm-window ((win rpm-virtual-window))
  (bt:with-recursive-lock-held ((window-lock win))
    (apply 'remove-subviews win (subviews win))))


;;;;; redefine some virtual-view methods to be able to test the lock

(defmethod add-subviews ((vv rpm-virtual-window) &rest subviews)
  (bt:with-recursive-lock-held ((window-lock vv))
    (dolist (sub subviews)
      (if (view-container sub)
          (print-warning "Item ~s is already in window ~s so it can't be added to window ~s" sub (view-container sub) vv)
        (progn
          (setf (gethash (id sub) (view-subviews vv)) sub)
          (setf (view-container sub) vv))))))

(defmethod remove-subviews ((vv rpm-virtual-window) &rest subviews)
  (bt:with-recursive-lock-held ((window-lock vv))
    (dolist (sub subviews)
      (if (gethash (id sub) (view-subviews vv))
          (progn
            (remhash (id sub) (view-subviews vv))
            (setf (view-container sub) nil)
            
            )
        (print-warning "Item ~s is not in window ~s so it can't be removed from it" sub vv)))))


(defmethod vv-handle-click ((vw rpm-virtual-window) (pos vector))
  (bt:acquire-recursive-lock (window-lock vw))
  (when (point-in-vv-p vw pos)
    (let ((local-pos (map 'vector '- pos (vector (x-pos vw) (y-pos vw)))))
      (dolist (sub (subviews vw))
        (when (and (handles-click-p sub)
                   (point-in-vv-p sub local-pos))
          (bt:release-recursive-lock (window-lock vw))
          (vv-click-event-handler sub local-pos)
          (return-from vv-handle-click t)))))
  (bt:release-recursive-lock (window-lock vw))
  nil)



;;; MAKE-RPM-WINDOW  [Function]
;;; Description : Make and return a window based on the parameters supplied.
;;;             : Visible determines wheter or not it should be a real or
;;;             : virtual and if the environment is connected it will use a 
;;;             : visible-virtual for the real window unless the user explicitly
;;;             : specifies the class to use.

(defun make-rpm-window (&key (visible nil) (title "RPM Window") (width 100) (height 100) (x 0 ) (y 0))
  (if visible
      (aif (visible-virtuals-available?) 
          (make-instance 'visible-virtual-window :window-title title :width width :height height :x-pos x :y-pos y :display-handlers it)
        (progn
          (print-warning "No handler available for displaying a visible window.  Using a virtual window instead.")
          (make-instance 'rpm-virtual-window :window-title title :width width :height height :x-pos x :y-pos y)))
    (make-instance 'rpm-virtual-window :window-title title :width width :height height :x-pos x :y-pos y)))

;;; MAKE-BUTTON-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a button-dialog-item based on the
;;;             : parameters supplied.

(defgeneric make-button-for-rpm-window (window &key x y text action height width color)
  (:documentation "Returns a button built with the parameters supplied"))

(defmethod make-button-for-rpm-window ((win rpm-virtual-window) &key (x 0) (y 0) (text "Ok")  (action nil) (height 18)  (width 60) (color 'gray))
  (make-instance 'button-vdi
    :x-pos x :y-pos y
    :dialog-item-text text
    :action action
    :height height
    :width width
    :color color
    :lock (window-lock win)))

(defmethod modify-button-for-rpm-window ((button-item button-vdi) &key (x nil xp) (y nil yp) (text nil textp) (height nil heightp) (width nil widthp) (color nil colorp) (action nil actionp))
  "Modify a button item for the rpm window"
  (bt:with-recursive-lock-held ((dialog-item-lock button-item))
    (when xp
      (setf (x-pos button-item) x))
    (when yp
      (setf (y-pos button-item) y))
    (when textp
      (setf (dialog-item-text button-item) text))
    (when heightp
      (setf (height button-item) height))
    (when widthp
      (setf (width button-item) width))
    (when colorp 
      (setf (color button-item) color))
    (when actionp
      (setf (action-function button-item) action))
    button-item))

(defmethod build-vis-locs-for ((self button-vdi))
  (bt:with-recursive-lock-held ((dialog-item-lock self))
    
    (let* ((line-height (max (round (text-height self) .8) (+ (text-height self) 1)))
           (lines (1+ (count #\newline (dialog-item-text self))))
           (view-loc (view-loc self))
           (top (find-top-level-window self))
           (top-xy (bt:with-recursive-lock-held ((window-lock top)) (vector (x-pos top) (y-pos top))))
           (text-feats (build-string-feats :text (dialog-item-text self)
                                           :start-x (px view-loc)
                                           :x-fct (lambda (string startx obj)
                                                    (- startx
                                                       (round (dispatch-apply (str-width-fct obj) string) 2)))
                                           :y-pos (round (- (py view-loc) (* 1/2 line-height (1- lines))))
                                           :width-fct (str-width-fct self)
                                           :height (text-height self)
                                           :obj self
                                           :line-height line-height
                                           :x-off (px top-xy)
                                           :y-off (py top-xy))))
      
      ;; set the text to be black
      (dolist (f text-feats)
        (push-last 'color f)
        (push-last 'black f))
      
      
      ;; previously it set the approach width of the text chunks,
      ;; but can't do that since don't have a chunk here now.
      ;; Also, that was only actually correct if there was a single
      ;; text item co-located with the oval.  Otherwise the angle was
      ;; off since the cursor was moving to the center of the text item.
      
      #| (car feats was the oval chunk)
    (let ((fun (lambda (x y) (declare (ignore x)) (approach-width (car feats) y vis-mod))))
      (mapcar #'(lambda (f)
                  (setf (chunk-visual-approach-width-fn f) fun))
        text-feats))
    |#    
      
      ;; return the oval which now has the text as its value if there is any
      ;; along with the text features
      
      (append `((isa (visual-location oval)
                 screen-x ,(+ (px top-xy) (px view-loc))
                 screen-y ,(+ (py top-xy) (py view-loc))
                 width ,(width self)
                 height ,(height self)
                 value ,(if text-feats
                            `(oval ,(dialog-item-text self))
                          `(oval nil))
                 color ,(color self)))
              text-feats))))


;;; MAKE-STATIC-TEXT-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return a static-text-dialog-item based on the
;;;             : parameters supplied.

(defgeneric make-static-text-for-rpm-window (window &key x y text height width color)
  (:documentation "Returns a text item built with the parameters supplied"))

(defmethod make-static-text-for-rpm-window ((win rpm-virtual-window) &key (x 0) (y 0) (text "") 
                                            (height 20) (width 80) (color 'black) font-size)
  (unless (numberp font-size)
    (setf font-size 12))
  (make-instance 'static-text-vdi
    :x-pos x :y-pos y
    :dialog-item-text text
    :height height
    :width width
    :color color
    :text-height (round font-size 12/10)
    :str-width-fct (let ((w (round font-size 12/7))) (lambda (str) (* (length str) w)))
    :lock (window-lock win)))

(defmethod modify-text-for-rpm-window ((text-item static-text-vdi) &key (x nil xp) (y nil yp) (text nil textp) (height nil heightp) (width nil widthp) (color nil colorp) (font-size nil font-sizep))
  "Modify a text item for the rpm window"
  (bt:with-recursive-lock-held ((dialog-item-lock text-item))
    (when xp
      (setf (x-pos text-item) x))
    (when yp
      (setf (y-pos text-item) y))
    (when textp
      (setf (dialog-item-text text-item) text))
    (when heightp
      (setf (height text-item) height))
    (when widthp
      (setf (width text-item) width))
    (when font-sizep
      (setf (text-height text-item) (round font-size 12/10))
      (setf (str-width-fct text-item) (let ((w (round font-size 12/7))) (lambda (str) (* (length str) w)))))
    (when colorp 
      (setf (color text-item) color))
    text-item))

(defmethod build-vis-locs-for ((self static-text-vdi))
  (bt:with-recursive-lock-held ((dialog-item-lock self))
    (let* ((line-height (max (round (text-height self) .8) (+ (text-height self) 1)))
           (top (find-top-level-window self))
           (top-xy (bt:with-recursive-lock-held ((window-lock top)) (vector (x-pos top) (y-pos top))))
           (feats (build-string-feats :text (dialog-item-text self)
                                      :start-x (1+ (x-pos self))
                                      :y-pos (+ (y-pos self) (round line-height 2)) ;;(round (- (py (view-loc self)) (* 1/2 line-height (1- lines))))
                                      :width-fct (str-width-fct self)
                                      :height (text-height self)
                                      :obj self
                                      :line-height line-height
                                      :x-off (px top-xy)
                                      :y-off (py top-xy))))
      (dolist (f feats)
        (push-last 'color f)
        (push-last (color self) f))
  
      feats)))

;;; MAKE-LINE-FOR-RPM-WINDOW  [Method]
;;; Description : Build and return the appropriate liner object for the
;;;             : window based on the parameters supplied.

(defgeneric make-line-for-rpm-window (window start-pt end-pt &optional color)
  (:documentation "returns a view which will draw a line with the parameters supplied"))

(defmethod make-line-for-rpm-window ((win rpm-virtual-window) start-pt end-pt &optional (color 'black))
  (make-instance 'v-liner
    :color color
    :x-pos (first start-pt)
    :y-pos (second start-pt)
    :width (first end-pt)
    :height (second end-pt)
    :lock (window-lock win)))

(defmethod modify-line-for-rpm-window ((line v-liner) start-pt end-pt &key (color nil colorp))
  (bt:with-recursive-lock-held ((dialog-item-lock line))
    (when start-pt
      (setf (x-pos line) (first start-pt))
      (setf (y-pos line) (second start-pt)))
    (when end-pt
      (setf (width line) (first end-pt))
      (setf (height line) (second end-pt)))
    (when colorp
      (setf (color line) color))
    line))


(defun pts-to-orientation (pt1 pt2)
  "Return the orientation of the line connecting two points, in degrees from -89 to +90. 0 is vertical."   
  (let ((angle (- (round (rad->deg (vtheta (xy-to-polar pt1 pt2)))) 90)))
    (cond ((< angle -90) (+ angle 180))
          ((> angle 90) (- angle 180))
          ((= angle -90) 90)
          (t angle))))

(defmethod build-vis-locs-for ((lnr v-liner))
  "Convert the view to a feature to be placed into the visual icon"
  (let* ((top (find-top-level-window lnr))
         (top-xy (bt:with-recursive-lock-held ((window-lock top)) (vector (x-pos top) (y-pos top)))))
    (bt:with-recursive-lock-held ((dialog-item-lock lnr))
      `((isa (visual-location line)
         color ,(color lnr)
         value ,(pts-to-orientation
                 (vector (+ (px top-xy) (x-pos lnr))
                         (+ (py top-xy) (y-pos lnr)))
                 (vector (+ (px top-xy) (width lnr))
                         (+ (py top-xy) (height lnr))))
         screen-x ,(+ (px top-xy) (floor (/ (+ (x-pos lnr) (width lnr)) 2)))
         screen-y ,(+ (px top-xy) (floor (/ (+ (y-pos lnr) (height lnr)) 2)))
         width ,(abs (- (x-pos lnr) (width lnr)))
         height ,(abs (- (y-pos lnr) (height lnr)))
         end1-x (nil ,(+ (px top-xy) (x-pos lnr)))
         end1-y (nil ,(+ (py top-xy) (y-pos lnr)))
         end2-x (nil ,(+ (px top-xy) (width lnr)))
         end2-y (nil ,(+ (py top-xy) (height lnr))))))))



;;; VISIBLE-VIRTUALS-AVAILABLE?  [Function]
;;; Description : Check with environment to determine if they're available.

(defun visible-virtuals-available? () 
  "Return whether or not the visible-virtuals are available"
  (and (fboundp 'check-with-environment-for-visible-virtuals)
       (funcall 'check-with-environment-for-visible-virtuals)))

(add-act-r-command "visible-virtuals-available?" 'visible-virtuals-available? "Check whether there is a handler active for displaying visible virtual windows.")

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
