;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2007 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : env-graphic-trace.lisp
;;; Version     : 2.0
;;; 
;;; Description : Support for a quick and dirty graphic display of the 
;;;               buffer trace info through the environment connection.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Check that all module state changes get flagged correctly
;;;             :     for "unusual" modules and situations.
;;; 
;;; ----- History -----
;;;
;;; 2007.04.30 Dan
;;;             : Initially added it to the archive.
;;; 2007.05.21 Dan
;;;             : * Modified the vertical trace to be much faster because
;;;             :   it sends all the data in one update which the environment
;;;             :   parses as a list for displaying.  Will change the horizontal 
;;;             :   once testing verifies that the vertical is stable like this).
;;; 2007.05.22 Dan
;;;             : * Reference the parameters that were added to the buffer trace
;;;             :   module for display purposes.
;;;             : * Made the horizontal trace use the fast transmit mechanism.
;;;             : * Fixed the default color list referencing so that it wraps
;;;             :   around if needed.
;;; 2007.06.06 Dan
;;;             : * Added the use of error->clear to the buffer summary parsing
;;;             :   to better handle requests that occur immediately following
;;;             :   an error (retrieval failures had some oddities in the trace).
;;; 2007.07.30 Dan
;;;             : * Adding a new production parameter called color and then
;;;             :   using that to set the color in the graphic traces if set to
;;;             :   a valid color string.
;;; 2011.04.28 Dan
;;;             : * Suppress warnings about extending productions at initial load.
;;; 2011.05.31 Dan
;;;             : * Moved the color code to a file in support which is required
;;;             :   now since the BOLD tools use it too.
;;; 2011.06.01 Dan
;;;             : * Removed some global vars that are no longer needed.
;;; 2014.05.06 Dan
;;;             : * Fixed a bug with parse-trace-list which could happen when
;;;             :   production breaks cause 'bad' production events to be recorded.
;;; 2015.07.28 Dan
;;;             : * Changed the logical to ACT-R-support in the require-compiled.
;;; 2016.04.29 Dan [2.0]
;;;             : * Updating for use with the saved data approach and doing some
;;;             :   slight reorganizing of the data for the display.
;;; 2016.05.03 Dan
;;;             : * Parse-trace-list now returns the trace from the buffers in
;;;             :   the trace not those set with :save-buffer-trace since it could
;;;             :   be set to ones that weren't traced.
;;;             : * Similarly, it's got to pull the time from the data as well.
;;;             : * Also needs color info from productions, but not sure how to
;;;             :   deal with that yet.
;;; 2016.05.04 Dan
;;;             : * Saving production and width info with the buffer trace for
;;;             :   the environment by simply making a list of the three for
;;;             :   the new get-environment-buffer-trace.
;;; 2016.05.05 Dan
;;;             : * Added the end time to the get-environment-buffer-trace data
;;;             :   so that it doesn't call mp-time-ms when drawing an unfinished
;;;             :   rectangle.
;;;             : * Adding the buffer name to the rectangle data passed over to
;;;             :   the environment side.
;;; 2016.05.27 Dan
;;;             : * Added an extra 100ms to the end time passed for the graphic
;;;             :   traces so that what happens at the end should be visible.
;;; 2017.09.29 Dan [3.0]
;;;             : * Convert to a history data processor and ignore the colors
;;;             :   for now -- that gets done on the 'display' side, but should
;;;             :   have some suggestions in the data passed over since the 
;;;             :   production coloring can result in some nice data visibility.
;;;             :   Still going to scale the x/y sizes based on how many, but 
;;;             :   could also be scaled on the other side as needed.
;;; 2017.10.02 Dan
;;;             : * Changed the size value sent to not include the unnecessary 0.
;;;             : * Added the buffer-index in place of a color value. 
;;;             : * Changed the vertical to use same width for all.
;;;             : * Test for the case when there are no buffers to avoid a divide
;;;             :   by zero error.
;;;             : * Sort the buffer list so things are consistent.
;;; 2017.10.03 Dan
;;;             : * Moved the buffer-trace data list accessors to the buffer-trace
;;;             :   file.
;;; 2017.10.04 Dan
;;;             : * Use mp-time-ms for end of ongoing actions in parse-trace-list
;;;             :   since it doesn't get start & end times now.
;;; 2018.02.28 Dan
;;;             : * Updated the calls to define-history-processor to name the
;;;             :   data stream first.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; In the graphic trace the box is drawn when the buffer reports that the 
;;; module is busy, whenever a request is sent through the buffer, or whenver a
;;; chunk is placed into the buffer.  The request is drawn at the top of the 
;;; box and the chunk placed into the buffer is drawn at the bottom (if there is
;;; such a chunk).  The one exception to that is 0-time events (visual-location
;;; and goal requests for example).  For 0-time events only the chunk name is 
;;; shown after the line representing the event in the vertical trace.
;;;
;;; If you place the mouse cursor over a box (or the chunk name if it is outside
;;; of the box like a 0-time event will be) in the trace it will show the start
;;; and stop times for that box in the lower left corner of the display window.
;;;
;;; The "+" and "-" buttons in the control can be used to rescale the image along
;;; the time dimension.
;;; 
;;; The "Remove Text" button will erase all of the request and chunk names from
;;; the image.  The only way to restore them after that is to redisplay the
;;; whole thing.
;;;
;;; The save button will write the image out to a Postscript file because that's the
;;; easy default available in Tk.  There are lots of ps->pdf converters out there
;;; so hopefully that's not an issue for people, but eventually it could be made
;;; to generate something "nicer" if people demand it.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; None (accessed through the environment side viewer).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Quick and dirty - the rectangle drawing info is actually created on the Lisp
;;; side and pushed to Tk for drawing.  A much nicer mechanism would be to just
;;; send the descriptive info so that more display control would be available on
;;; the environment side (like stretching columns or reordering things). 
;;; If there's a signifigant demand (and time) at some point this could be made
;;; very nice, but that'll require modifications to the environment server control
;;; code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;; Give the option to color the productions
(suppress-extension-warnings)
(extend-productions color)
(unsuppress-extension-warnings)

(defun vert-graphic-trace-return (h)
  (let* ((d (json:decode-json-from-string h))
         (data (parse-trace-list d :vertical)))
    (cons (list "size" (+ 100 (br-time (first (last d))))) data)))

(defun hor-graphic-trace-return (h)
  (let* ((d (json:decode-json-from-string h))
         (data (parse-trace-list d :horizontal)))
    (cons (list "size" (+ 100 (br-time (first (last d))))) data)))
  

(define-history-processor-fct "buffer-trace" "horizontal-graphic-trace" 'hor-graphic-trace-return)
(define-history-processor-fct "buffer-trace" "vertical-graphic-trace" 'vert-graphic-trace-return)


(defstruct gt-rect start end (request "") (chunk "") (notes ""))


(defun parse-trace-list (trace dir)
  (let* ((buffers (mapcar 'bs-name (br-buffers (first trace))))
         (y 0)
         (y-inc (when buffers (min 50 (floor (/ 380 (length buffers))))))
         (x-coord 0)
         (x-inc (when buffers (min 190 (floor (/ 960 (length buffers))))))
         (all-data nil)
         (buffer-index 0))
    
    (dolist (x (sort buffers 'string<))
      (let ((rects nil)
            (current-rect nil))
        (dolist (z trace)
          (let ((record (find x (br-buffers z) :key 'bs-name :test 'string-equal)))
            (cond (current-rect
                   (awhen (check-bs-chunk-name record)
                          (setf (gt-rect-chunk current-rect) it))
                   
                   (awhen (check-bs-notes record)
                          (setf (gt-rect-notes current-rect) it))
                   
                   (when (or (null (bs-busy record))
                             (bs-busy->free record)
                             (check-bs-request record))
                     
                     (setf (gt-rect-end current-rect) (br-time z))
                     (push current-rect rects)
                     (if (check-bs-request record)
                         (setf current-rect (make-gt-rect :start (br-time z)
                                                          :request (value-bs-request record)))
                       (setf current-rect nil))))
              
                  ((bs-busy record)
                   (if (and (check-bs-request record) 
                            (or (check-bs-chunk-name record)
                                (and (bs-error record)
                                     (not (bs-error->clear record)))
                                (bs-busy->free record)))
                       (push (make-gt-rect :start (br-time z)
                                           :end (br-time z)
                                           :request (value-bs-request record)
                                           :chunk (value-bs-chunk-name record)
                                           :notes (value-bs-notes record))
                             rects)
                     (setf current-rect (make-gt-rect :start (br-time z)
                                                      :request (value-bs-request record)
                                                      :chunk (value-bs-chunk-name record)
                                                     :notes (value-bs-notes record)))))
                  ((check-bs-request record)
                   (push (make-gt-rect :start (br-time z) :end (br-time z)
                                       :request (value-bs-request record)
                                       :chunk (value-bs-chunk-name record)
                                       :notes (value-bs-notes record))
                         rects))
                  ((check-bs-chunk-name record)
                   (push (make-gt-rect :start (br-time z) :end (br-time z)
                                       :request (value-bs-request record)
                                       :chunk (value-bs-chunk-name record)
                                       :notes (value-bs-notes record))
                         rects)))))
        
        (dolist (z rects)
          (if (eq dir :horizontal)
              (push (list "rectangle" (gt-rect-start z) y (aif (gt-rect-end z) it (mp-time-ms)) (+ y y-inc) 
                          
                        buffer-index #| (aif (and (eq x 'production)
                                    (gt-rect-request z)
                                    (assoc (gt-rect-request z) prod-colors :test 'string-equal))
                               (if (stringp (cdr it))
                                   (cdr it)
                                 (nth buffer-index color-list))
                               (nth buffer-index color-list)) |#
                          
                          (gt-rect-request z) 
                          (gt-rect-chunk z) 
                          (if (gt-rect-notes z)
                              (format nil "~a" (gt-rect-notes z))
                            "")
                          (if (string-equal x "production") (gt-rect-request z) (gt-rect-chunk z))
                          x)
                    all-data)
            
            (push (list "rectangle" x-coord (gt-rect-start z) 
                        (+ x-coord x-inc) (aif (gt-rect-end z) it (mp-time-ms))  
                        
                        buffer-index #|(aif (and (eq x 'production)
                                  (gt-rect-request z)
                                  (assoc (gt-rect-request z) prod-colors :test 'string-equal))
                             (if (stringp (cdr it))
                                 (cdr it)
                               (nth buffer-index color-list))
                             (nth buffer-index color-list)) |#
                        
                        (gt-rect-request z) (gt-rect-chunk z) 
                        (if (gt-rect-notes z)
                            (format nil "~a" (gt-rect-notes z))
                          "")
                        (if (string-equal x "production") (gt-rect-request z) (gt-rect-chunk z))
                        x)
                  all-data)))
        
        (if (eq dir :horizontal)
            (push (list "label" x (+ y (floor (/ y-inc 2))) buffer-index #|(nth buffer-index color-list)|#) all-data)
          (push (list "label" x (+ x-coord (floor (/ x-inc #|(nth buffer-index widths-list)|# 2))) buffer-index #|(nth buffer-index color-list)|# x-inc #|(nth buffer-index widths-list)|#) all-data)))
      
      
      (incf y y-inc)
      (incf x-coord x-inc)
      (incf buffer-index))
  
  all-data))


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
