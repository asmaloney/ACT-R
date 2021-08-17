;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : production-history.lisp
;;; Version     : 2.1
;;; 
;;; Description : Code to support the production trace tool in the environment.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [X] Consider making a module for this to allow configuration
;;;             :     of the display (colors, widths, restricted productions, 
;;;             :     etc) and provide a simple parameter switch to enable.
;;;             : [ ] Draw the links "better" with respect to crossing and
;;;             :     overlapping.
;;; 
;;; ----- History -----
;;; 2008.08.06 Dan
;;;             : * Initial creation.
;;; 2008.08.06 Dan
;;;             : * Making it a module and doing some optimizing on how it
;;;             :   stores/creates the data to send because it can't handle 
;;;             :   long runs (zbrodoff tutorial model often results in an
;;;             :   allocation error).
;;; 2008.08.07 Dan
;;;             : * Another stability improvement.  It now breaks the grid
;;;             :   up into smaller updates to send over to the environment
;;;             :   to avoid having to create one very large string.  Slows
;;;             :   down the displaying, but shouldn't result in any allocation
;;;             :   issues.
;;; 2008.08.12 Dan
;;;             : * Added the :p-history-colors parameter to allow one to
;;;             :   change the colors used in the grid.
;;; 2011.10.28 Dan [1.1]
;;;             : * Adding a different display option for the data - a directed
;;;             :   graph with the productions as nodes and where a link from A->B 
;;;             :   indicates that B was in the conflict set after A fired.
;;;             : * Changed the name of the function used by the previous display
;;;             :   to production-history-chart-data since the new tool is actually
;;;             :   a graph.
;;;             : * Added parameters for some minor control over the graph display.
;;; 2011.11.17 Dan
;;;             : * Added support for another display option in the history graph:
;;;             :   "Frequencies" which draws the links with widths based on thier
;;;             :   relative frequencies.  The most frequent will be drawn 1/4 of
;;;             :   the x spacing wide and the rest scaled to that (unless all are
;;;             :   the same in which case they're all drawn width 1).
;;; 2011.12.06 Dan
;;;             : * Adding the extra data to the info sent for the graph so that
;;;             :   the tcl side can write out a .dot file for the graph.
;;; 2011.12.07 Dan
;;;             : * Added the command production-transition-graph which prints
;;;             :   a DOT representation of the saved production history data
;;;             :   to the current ACT-R command output stream for the specified
;;;             :   graph type which can be one of :all, :run, :cycle, :unique-run,
;;;             :   or :unique-cycle.  The graphs correspond to those that are 
;;;             :   shown in the environment tool using the corresponding buttons, 
;;;             :   and when there are multiple subgraphs each has its own cluster 
;;;             :   in the output.  This allows one to create the graphs without
;;;             :   without having the environment connected if an external DOT
;;;             :   graph viewer is used instead.  
;;; 2011.12.22 Dan
;;;             : * Added another type of display - utilities.
;;; 2012.02.01 Dan
;;;             : * Added a declare and removed unused variables to clean up 
;;;             :   some load time warnings.
;;; 2012.03.12 Dan
;;;             : * Save both the current utility and the U(n) value for display
;;;             :   in the environment production history viewer info.
;;; 2012.03.22 Dan
;;;             : * Changed the graph display for runs and utilities so that it
;;;             :   doesn't show the unchosen links from the end of one session
;;;             :   to the beginning of the next on the following display (that
;;;             :   is the dashed links from the previous displays red box to the
;;;             :   unchosen competitors for the current display's green box).
;;; 2012.04.27 Dan
;;;             : * Fixed a bug with displaying the utility graph when there are
;;;             :   no utility differences.
;;;             : * Added an option for hiding the unused productions in the graph
;;;             :   display.
;;; 2013.01.10 Dan
;;;             : * Changed the environment data cache to an equalp hashtable
;;;             :   since the keys are now a cons of the handler name and the
;;;             :   window to prevent issues with multi-environment settings.
;;; 2015.06.10 Dan
;;;             : * Changed time to use ms instead.
;;; 2016.04.22 Dan
;;;             : * Started work to be able to load saved history info.  First
;;;             :   step is adding a function to get the current data because
;;;             :   that will be used to save current results and since this is
;;;             :   used for two different purposes it basically needs to save
;;;             :   the whole module.
;;; 2016.04.28 Dan
;;;             : * Updating the interface functions to use the stored history
;;;             :   info instead of pulling it from the module directly.
;;;             : * Depricated the :draw-blank-columns parameter.  Only the 
;;;             :   box on the viewer controls that now.
;;;             : * Took the cache table out of the module and made it global
;;;             :   because of the possibility of loading saved data without a
;;;             :   model present.
;;; 2016.05.16 Dan
;;;             : * Send the production text over for the graph display so that
;;;             :   it can be shown in a window since can't rely on opening a 
;;;             :   procedural viewer since saved data may not match current 
;;;             :   assuming there even is a current model.
;;; 2016.05.26 Dan
;;;             : * Send the production name over in the grid column info for
;;;             :   a more detailed whynot output.
;;; 2017.08.09 Dan
;;;             : * Use printed-production-text instead of capturing the pp output.
;;; 2017.09.22 Dan [2.0]
;;;             : * Using a history data stream and removing the save-p-history
;;;             :   parameter.
;;; 2017.09.26 Dan
;;;             : * Fixed a bug in the get-p-history-data since the tag is :stop
;;;             :   not :end.
;;;             : * Removed the unneeded parameters.
;;;             : * Starting to convert the graph creation code into something
;;;             :   general that can be returned from a data processor.
;;; 2017.09.27 Dan
;;;             : * Added all the processors and just have them return data that's
;;;             :   still specific to drawing the graph, but in a slightly more
;;;             :   general list format.
;;;             : * Tags all the 'unused' items so they can be hidden when wanted.
;;;             : * All the processors work now.
;;; 2017.10.11 Dan
;;;             : * Removed the parameters for spacing since it's hard-coded now.
;;;             : * Eliminate the unused slots in the module and the global
;;;             :   variable for the data cache.
;;; 2018.02.28 Dan
;;;             : * Updated the calls to define-history-processor to name the
;;;             :   data stream first.
;;; 2020.01.13 Dan [2.1]
;;;             : * Removed the lambdas from the module interface.
;;;             : * Use the (:remove <>) option for hook functions.
;;;             : * Create functions for the processors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Open either of the production history tools, "Production Grid" or "Production
;;; Graph" before running the model or record the history "production-history" or 
;;; "production-grid".
;;; 
;;; For the "Grid" tool, once the model has run click the "Get history" button 
;;; at the bottom left corner of the window.  It will draw a grid with the rows 
;;; being the productions in the model and the columns being the times at which
;;; a conflict-resolution event occurred.
;;;
;;; The cells in the grid are color coded based on what happened during the 
;;; conflict resolution event at the time indicated for the column.
;;; 
;;; If the production was the one selected the cell will be green.
;;; If the production was in the conflict set, but not selected then the cell
;;; will be orange.
;;; If the production was not in the conflict set then the cell will be red.
;;; If the production did not exist at that time the cell will be white.
;;;
;;; The colors for selected, matched, and mismatched can be changed by clicking
;;; on the color in the tool.
;;;
;;; Placing the cursor over a cell will cause some details for that production
;;; during that conflict resolution event to be printed at the bottom of the
;;; window.
;;;
;;; For the selected and matched cells it will print the utility value for the
;;; production at that time.  For the red cells it will print the whynot 
;;; information indicating the condition that caused the production to not be
;;; in the conflict set.  There is no extra information to print for a white
;;; cell.
;;;
;;; Clicking on the name of a production in the grid will open a new window
;;; showing the production text.
;;;
;;; Clicking a grid in the cell will open a window to display the whynot info or
;;; utility values if it matched.
;;;
;;; For the "Graph" tool, after the model has run click the "get history" button
;;; or select a different radio button to see a different view of the data.
;;; All of the displays are drawn the same way and the common features will be 
;;; described before indicating what differs among the choices.
;;; 
;;; The display will show all of the productions in the model in boxes.  If the
;;; box has a black border then it was selected and fired at some point in the 
;;; model's run.  If it has a gray border then it was not selected and fired.
;;; If there is a green border around a production then it is the first production
;;; which was selected for the currently displayed information, and if it has
;;; a red border then it was the last production selected for the currently displayed
;;; information.
;;;
;;; An arrow from a production A to production B means that production B was in
;;; the conflict set after production A fired.  If the arrow has a solid black line then 
;;; production B was selected and fired after A, but if the arrow has a dashed gray
;;; line then it was not selected and fired.
;;;
;;; Clicking on the name of a production in the graph will open a new window showing
;;; the text of the production.
;;; 
;;; The "All transitions" button shows the data for all production firings over
;;; the entire run of the model.  The "Frequencies" button shows the same data
;;; except that the thickness of the links reflects their relative frequencies.
;;; The most frequent will be 1/4 of :p-history-graph-x wide and the others
;;; scaled appropriately.
;;;
;;; The "Cycles" and "Unique Cycles" buttons show the data broken down into 
;;; cycles which occur in the graph (when a loop is formed by a production
;;; eventually firing again after itself).  The display will show only one
;;; cycle at a time.  The number of cycles which occurred is shown below
;;; the graph and the "-" and "+" buttons can be used to change which cycle
;;; is shown.  For "Cycles" all production firings are shown and the cycles
;;; are displayed in temporal order with the model time for the start and end
;;; displayed at the bottom of the window.  For the "Unique Cycles" it only
;;; shows one instance of each cycle that occurs and no specific time information
;;; is shown.
;;; 
;;; The "Runs" and "Unique Runs" buttons show the data broken down into sections
;;; based on when one of the ACT-R running commands were called.  The display will 
;;; only one "run" at a time.  The number of non-empty runs which occurred is 
;;; shown below the graph and the "-" and "+" buttons can be used to change which run
;;; is shown.  For "Runs" all production firings are shown and the runs
;;; are displayed in temporal order with the model time for the start and end
;;; of the run displayed at the bottom of the window.  For the "Unique Runs" it only
;;; shows one instance of each production graph that occurs in a run and no specific 
;;; time information is shown.
;;;
;;; The "Utilities" button breaks the data down into sections based on when the
;;; model receives a reward.  Each of the production boxes will be the same width
;;; in this display and may have two additional bars displayed within the box.
;;; A blue bar at the top represents the relative utility of that production before 
;;; the reward which ended this section has been applied and a blue bar at the 
;;; bottom represents the relative utility of that production after the reward
;;; has been applied.  The utility values are scaled so that the range between the
;;; minimum utility across all sections and the maximum utility across all sections
;;; can be displayed.  If there is no bottom bar then it means that there was no
;;; reward provided (the last chart may end because the model stopped instead of
;;; because there was a reward provided).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct p-history-module
  (lock (bt:make-lock "p-history-module"))
  (alive t)
  enabled
  history
  why-not-list
  current-data)
  

(defstruct p-history
  time
  selected
  matched
  mismatched
  info
  tag
  reward)


(defun production-history-reward-markers (reward)
  (let ((history (get-module production-history)))
    (bt:with-lock-held ((p-history-module-lock history))
      (push (make-p-history :tag :reward :reward reward :time (mp-time-ms) :info (no-output (spp :name :u))) (p-history-module-history history)))))

(defun production-history-start-markers (ph)
  (bt:with-lock-held ((p-history-module-lock ph))
    (when (p-history-module-enabled ph)
      (push (make-p-history :tag :start :time (mp-time-ms)) (p-history-module-history ph)))))

(defun production-history-stop-markers (ph)
  (bt:with-lock-held ((p-history-module-lock ph))
    (when (p-history-module-enabled ph)
      (push (make-p-history :tag :stop :time (mp-time-ms) :info (no-output (spp :name :u))) (p-history-module-history ph)))))

(defun production-history-recorder (cs)
  (let* ((history (get-module production-history))
         (best (car cs))
         (mismatched (set-difference (all-productions) cs))
         (block (make-p-history :mismatched mismatched :time (mp-time-ms))))
    (bt:with-lock-held ((p-history-module-lock history))
      (no-output
       (let ((ut (sgp :ut)))
         (when (and best 
                    (or (not (numberp ut))
                        (and (numberp ut) (>= (caar (spp-fct (list best :utility))) ut))))
           (setf (p-history-selected block) best))
         (dolist (x cs)
           (push (cons x (car (spp-fct (list x :utility :u))))
                 (p-history-info block)))
         
         (dolist (x mismatched)
           (let* ((reason (production-failure-reason x))
                  (index (position reason (p-history-module-why-not-list history) :test #'string-equal)))
             (unless index
               (setf index (length (p-history-module-why-not-list history)))
               (push-last reason (p-history-module-why-not-list history)))
             (push (list x index (caar (spp-fct (list x :u)))) (p-history-info block))))))
       
       (if (p-history-selected block)
           (setf (p-history-matched block) (cdr cs))
         (setf (p-history-matched block) cs))
       
      (push block (p-history-module-history history))
      nil)))




(defstruct (p-history-node (:conc-name phn-)) name color-starts color-ends cycle-starts cycle-ends links utilities)
(defstruct (p-history-link (:conc-name phl-)) target color count cycle from-time to-time)
(defstruct (p-history-display (:conc-name phd-)) name x y color width links starts ends utilities)
(defstruct (p-history-cache (:conc-name phc-)) offsets holes max-loops max height min-u max-u)

(defun parse-production-history-graph (data which)
  (let* ((module-data data)
        (nodes (mapcar (lambda (x) (make-p-history-node :name x :links nil)) (mapcar 'car (fourth (find "info" module-data :key 'second :test 'string-equal)))))
        
        (loops nil)
        (current-loop nil)
        (cycle 0)
        (top nil)
        (previous nil)
        (max-color 0)
        (max-cycle 0)
        (start-time nil)
        (min-utility nil)
        (max-utility nil)
        (stop-utilities nil))
    
    (dolist (x module-data)

      (unless (string-equal (second x) "info")
        
      (let ((selected (and (string-equal (second x) "conflict-resolution") (third x)))
            (matched (and (string-equal (second x) "conflict-resolution") (fourth x)))
            (time (first x)))
        
        (cond ((find which '(:all :freq :color :cycle))
               
               (when (or matched selected)
                 
                 (when (and (null top) selected)
                   (dolist (y matched)
                     (push y top))
                   (push selected top))
                 
                 (when (and previous matched)
                   
                   (dolist (y matched)
                     (unless (find y (phn-links previous) :key (lambda (z) (when (= (phl-cycle z) cycle) (phl-target z))))
                       (push (make-p-history-link :target y :cycle cycle :count nil :color -1 :from-time time :to-time time) (phn-links previous)))))
                 
                 (when selected
                   (when (find selected current-loop :key 'car :test 'string-equal)
                     ;; completed a loop
                     (let* ((existing-color (position (cons selected (mapcar 'car current-loop)) loops :test 'equalp))
                            (color (if existing-color existing-color (length loops))))
                       
                       (dolist (check (mapcar 'car current-loop))
                         (dolist (link (phn-links (find check nodes :key 'phn-name :test 'string-equal)))
                           (when (= -1 (phl-color link))
                             (setf (phl-color link) color))))
                       
                       (mapc (lambda (from+time to+time)
                               (let ((from (car from+time))
                                     (to (car to+time))
                                     (from-time (cdr from+time))
                                     (to-time (cdr from+time)))
                                 (awhen (find to (phn-links (find from nodes :key 'phn-name :test 'string-equal)) :test 'string-equal :key (lambda (z) (if (and (null (phl-count z)) (= (phl-cycle z) cycle)) (phl-target z) "")))
                                        (setf (phn-links (find from nodes :key 'phn-name :test 'string-equal)) (remove it (phn-links (find from nodes :key 'phn-name :test 'string-equal)))))
                                 (push (make-p-history-link :target to :color color :count 1 :cycle cycle :from-time from-time :to-time to-time) (phn-links (find from nodes :key 'phn-name :test 'string-equal)))))
                         current-loop (cons (cons selected time) (butlast current-loop)))
                       
                       (pushnew  color (phn-color-starts (find (caar (last current-loop)) nodes :key 'phn-name :test 'string-equal)))
                       (push  (cons cycle (cdar (last current-loop))) (phn-cycle-starts (find (caar (last current-loop)) nodes :key 'phn-name :test 'string-equal)))
                       (pushnew  color (phn-color-ends (find selected nodes :key 'phn-name :test 'string-equal)))
                       (push  (cons cycle time) (phn-cycle-ends (find selected nodes :key 'phn-name :test 'string-equal)))
                       
                       (when (> cycle max-cycle)
                         (setf max-cycle cycle))
                       
                       (when (> color max-color)
                         (setf max-color color))
                       
                       (unless existing-color
                         (push-last (cons selected (mapcar 'car current-loop)) loops)))
                     (incf cycle)
                     (setf current-loop nil))
                   (push (cons selected time) current-loop)
                   (setf previous (find selected nodes :key 'phn-name :test 'string-equal)))))
              
              ((find which '(:run :run-color))
               (when (or matched selected
                         (not (string-equal (second x) "conflict-resolution")))
                 
                 (when (string-equal (second x) "start")
                   (setf start-time time))
                 
                 (when (and (null top) selected)
                   (dolist (y matched)
                     (push y top))
                   (push selected top))
                 
                 (when (and previous matched)
                   
                   (dolist (y matched)
                     (unless (find y (phn-links previous)  :test 'string-equal :key (lambda (z) (if (= (phl-cycle z) cycle) (phl-target z) "")))
                       (push (make-p-history-link :target y :cycle cycle :count nil :color -1 :from-time time :to-time time) (phn-links previous)))))
                 
                 (when selected
                   
                   (push (cons selected time) current-loop)
                   (setf previous (find selected nodes :key 'phn-name :test 'string-equal)))
                 
                 (when (and current-loop (string-equal "stop" (second x)))
                   ;; completed a loop
                   (let* ((existing-color (position (mapcar 'car current-loop) loops :test 'equalp))
                          (color (if existing-color existing-color (length loops))))
                     
                     (dolist (check (mapcar 'car current-loop))
                       (dolist (link (phn-links (find check nodes :key 'phn-name :test 'string-equal)))
                         (when (= -1 (phl-color link))
                           (setf (phl-color link) color))))
                     
                     (mapc (lambda (from+time to+time)
                             (let ((from (car from+time))
                                   (to (car to+time))
                                   (from-time (cdr from+time))
                                   (to-time (cdr to+time)))
                               (awhen (find to (phn-links (find from nodes :key 'phn-name :test 'string-equal)) :test 'string-equal :key (lambda (z) (if (and (null (phl-count z)) (= (phl-cycle z) cycle)) (phl-target z) "")))
                                      (setf (phn-links (find from nodes :key 'phn-name :test 'string-equal)) (remove it (phn-links (find from nodes :key 'phn-name :test 'string-equal)))))
                               (push (make-p-history-link :target to :color color :count 1 :cycle cycle :from-time from-time :to-time to-time) (phn-links (find from nodes :key 'phn-name :test 'string-equal)))))
                       (cdr current-loop) (butlast current-loop))
                     
                     (pushnew  color (phn-color-starts (find (caar (last current-loop)) nodes :key 'phn-name :test 'string-equal)))
                     (pushnew  (cons cycle start-time) (phn-cycle-starts (find (caar (last current-loop)) nodes :key 'phn-name :test 'string-equal)))
                     (pushnew  color (phn-color-ends (find (caar current-loop) nodes :key 'phn-name :test 'string-equal)))
                     (push  (cons cycle time) (phn-cycle-ends (find (caar current-loop) nodes :key 'phn-name :test 'string-equal)))
                     
                     (when (> cycle max-cycle)
                       (setf max-cycle cycle))
                     
                     (when (> color max-color)
                       (setf max-color color))
                     
                     (unless existing-color
                       (push-last (mapcar 'car current-loop) loops)))
                   (incf cycle)
                   (setf current-loop nil)
                   
                   ;; don't want the dashed lines from end of
                   ;; one run going to non-starters for the next
                   ;; loop i.e. matched but not selected.
                   (setf previous nil))))
              
              (t  ;;(find which '(:utility))
               (when (or matched selected (not (string-equal (second x) "conflict-resolution")))
                 
                 (when (and (null top) selected)
                   (dolist (y matched)
                     (push y top))
                   (push selected top))
                 
                 (when (and previous matched)
                   
                   (dolist (y matched)
                     (unless (find y (phn-links previous)  :test 'string-equal :key (lambda (z) (if (= (phl-cycle z) cycle) (phl-target z) "")))
                       (push (make-p-history-link :target y :cycle cycle :count nil :color -1 :from-time time :to-time time) (phn-links previous)))))
                 
                 (when selected
                   
                   (push (cons selected time) current-loop)
                   (setf previous (find selected nodes :key 'phn-name :test 'string-equal)))
                 
                 (when (string-equal "stop" (second x))
                   (setf stop-utilities (third x)))
                 
                 (when (and current-loop (string-equal "reward" (second x)))
                   
                   ;; set the current scores
                   (dolist (reward (fourth x))
                     (push-last (cons cycle (second reward)) (phn-utilities (find (first reward) nodes :key 'phn-name :test 'string-equal)))
                     (when (or (null min-utility) (< (second reward) min-utility))
                       (setf min-utility (second reward)))
                     
                     (when (or (null max-utility) (> (second reward) max-utility))
                       (setf max-utility (second reward))))
                   
                   ;; completed a loop
                   (let* ()
                     
                     (mapc (lambda (from+time to+time)
                             (let ((from (car from+time))
                                   (to (car to+time))
                                   (from-time (cdr from+time))
                                   (to-time (cdr to+time)))
                               (awhen (find to (phn-links (find from nodes :key 'phn-name :test 'string-equal)) :test 'string-equal :key (lambda (z) (if (and (null (phl-count z)) (= (phl-cycle z) cycle)) (phl-target z) "")))
                                      (setf (phn-links (find from nodes :key 'phn-name :test 'string-equal)) (remove it (phn-links (find from nodes :key 'phn-name :test 'string-equal)))))
                               (push (make-p-history-link :target to :color 0 :count 1 :cycle cycle :from-time from-time :to-time to-time) (phn-links (find from nodes :key 'phn-name :test 'string-equal)))))
                       (cdr current-loop) (butlast current-loop))
                     
                     
                     (pushnew  (cons cycle (cdar (last current-loop))) (phn-cycle-starts (find (caar (last current-loop)) nodes :key 'phn-name :test 'string-equal)))
                     (push (cons cycle time) (phn-cycle-ends (find (caar current-loop) nodes :key 'phn-name :test 'string-equal)))
                     
                     (when (> cycle max-cycle)
                       (setf max-cycle cycle))
                     
                     ;; don't want the dashed lines from end of
                     ;; one loop going to non-starters for the next
                     ;; loop i.e. matched but not selected.
                     (setf previous nil))
                   (incf cycle)
                   (setf current-loop nil))))))))
    
    (when (and (eq which :utility) stop-utilities)
      (dolist (reward stop-utilities)
        (push-last (cons cycle (second reward)) (phn-utilities (find (first reward) nodes :key 'phn-name :test 'string-equal)))
        (when (or (null min-utility) (< (second reward) min-utility))
          (setf min-utility (second reward)))
        
        (when (or (null max-utility) (> (second reward) max-utility))
          (setf max-utility (second reward)))))
               
    
    (when (or (> (length current-loop) 0))
      (mapc (lambda (from+time to+time) 
              (let ((from (car from+time))
                    (to (car to+time))
                    (from-time (cdr from+time))
                    (to-time (cdr from+time)))
                (push (make-p-history-link :target to :color (length loops) :count 1 :cycle cycle :from-time from-time :to-time to-time) (phn-links (find from nodes :key 'phn-name :test 'string-equal)))))
        (cdr current-loop) (butlast current-loop))
      
      (pushnew (length loops) (phn-color-starts (find (caar (last current-loop)) nodes :key 'phn-name :test 'string-equal)))
      (push  (cons cycle (cdar (last current-loop))) (phn-cycle-starts (find (caar (last current-loop)) nodes :key 'phn-name :test 'string-equal)))
      (pushnew  (length loops) (phn-color-ends previous))
      (push (cons cycle (cdar current-loop)) (phn-cycle-ends previous))
      
      (setf max-color (length loops))
      (setf max-cycle cycle))
    
    (list top (when previous (phn-name previous)) nodes (list max-color max-cycle) (list min-utility max-utility))))


(defun filter-links (links which number)
  (case which
    ((:all :freq)
     (setf links (remove-duplicates links :key (lambda (x) (cons (phl-target x) (phl-count x))) :test 'equalp))
     (remove-if (lambda (x) (and (null (phl-count x)) (find (phl-target x) links :key (lambda (y) (when (phl-count y) (phl-target y))) :test 'string-equal))) links))
    ((:run :cycle :utility)
     (remove-if-not (lambda (x) (= (phl-cycle x) number)) links))
    ((:run-color :color)
     (remove-duplicates (remove-if-not (lambda (x) (= (phl-color x) number)) links) :key 'phl-target :test 'string-equal))))



(defun create-production-graph-coords (module-data which) ; number = generate all,  with-labels t,  show-unused = t
  (let* ((raw-data (json:decode-json-from-string module-data))
         (layer-data (parse-production-history-graph raw-data :cycle))
         (data (if (find which '(:freq :all :color :cycle)) layer-data (parse-production-history-graph raw-data which)))
         (min-u (first (fifth data)))
         (max-u (second (fifth data)))

         (all-production-names (mapcar 'car (fourth (find "info" raw-data :key 'second :test 'string-equal))))
         (unused all-production-names)
         (current (car data))
         (next nil)
         (layers nil)
         (start (caar data))
         (end (second data))
         (max-loops (case which
                      ((:all :freq) 0)
                      ((:run-color :color) (first (fourth data)))
                      ((:run :cycle :utility) (second (fourth data))))))
    
    (while current
      (dolist (x current)
        (setf unused (remove x unused :test 'string-equal)))
      (dolist (x current)
        (dolist (y (phn-links (find x (third layer-data) :key 'phn-name :test 'string-equal)))
          (when (and (phl-count y) (find (phl-target y) unused :test 'string-equal))
            (setf unused (remove (phl-target y) unused :test 'string-equal))
            (push (phl-target y) next))))
      (push current layers)
      (setf current next)
      (setf next nil))
    
    (when unused 
      (push unused layers))
    
    (let ((x-space 40)
          (y-space 90)
          (max 0)
          (cur 0)
          (y 65)
          (offsets nil)
          (widths nil)
          (holes nil)
          (max-width (* 10 (apply 'max (mapcar 'length all-production-names)))))
      
      (dolist (layer (reverse layers))
        (let ((hole (list (round x-space 2))))
          (setf cur 0)
          (dolist (item layer)
            (let ((width (if (eq which :utility) max-width (* 10 (length item)))))
              (push (make-p-history-display :name item :links (awhen (find item (third data) :key 'phn-name :test 'string-equal) (phn-links it)) :width width :x (+ cur (round width 2)) :y y 
                                            :starts (case which
                                                      ((:all :freq) (if (eq item start) (list 0) nil))
                                                      ((:cycle :run :utility)  (phn-cycle-starts (find item (third data) :key 'phn-name :test 'string-equal)))
                                                      ((:color :run-color)  (phn-color-starts (find item (third data) :key 'phn-name :test 'string-equal))))
                                            
                                            :ends (case which
                                                    ((:all :freq) (if (string-equal item end) (list 0) nil))
                                                    ((:cycle :run :utility)  (phn-cycle-ends (find item (third data) :key 'phn-name :test 'string-equal)))
                                                    ((:color :run-color)  (phn-color-ends (find item (third data) :key 'phn-name :test 'string-equal))))
                                            
                                            :color (if (find item unused :test 'string-equal) "gray" "black")
                                            :utilities (awhen (find item (third data) :key 'phn-name :test 'string-equal) (phn-utilities it)))
                    offsets) ; (cons item (cons (+ cur (round width 2)) y)) offsets)
              (when (= (length hole) 1)
                (push (- cur (round x-space 2)) hole))
              (push (+ cur width (round x-space 2)) hole)
              (incf cur (+ width x-space))))
          (decf cur x-space)
          (when (> cur max)
            (setf max cur))
          (push (cons cur y) widths)
          (push-last y hole)
          (push hole holes)
          (incf y (+ 30 y-space))))
      
      (incf max (* 2 x-space))
      (setf holes (mapcar (lambda (x) (reverse (cons (- max (round x-space 2)) x))) holes))
      
      (dolist (w widths)
        (unless (= (car w) max)
          (dolist (o offsets)
            (when (= (phd-y o) (cdr w))
              (incf (phd-x o) (round (- max (car w)) 2))))
          
          (let ((h (find (cdr w) holes :key 'car)))
            (setf holes (cons (concatenate 'list (subseq h 0 2)  (mapcar (lambda (x) (+ x (round (- max (car w)) 2))) (subseq (butlast h) 2)) (last h)) (remove h holes))))))
 
      (p-history-display-output raw-data which t offsets holes max-loops max y min-u max-u x-space y-space))))
      
(defun p-history-display-output (data which with-labels offsets holes max-loops max height min-u max-u x-space y-space)
  (let ((min-time -1)
        (max-time -1)
        (links nil)
        (max-link-count 0)
        (equal-link-counts t)
        (all nil)
        (info (find "info" data :key 'second :test 'string-equal)))
     
    (when (and (numberp min-u) (numberp max-u) (= min-u max-u))
      (setf min-u (1- min-u))
      (setf max-u (1+ max-u)))
    
    (dotimes (number (1+ max-loops))
      (setf links nil)
      
      (dolist (o offsets)
      (dolist (link (filter-links (phd-links o) which number))
        ;; for now just using a next closest hole metric for 
        ;; multi-level links but at some point may want to try
        ;; a shortest path instead or some sort of non-intersecting algorithm
        
        (let* ((target (find (phl-target link) offsets :key 'phd-name :test 'string-equal))
               (used (if (string-equal (phd-color target) "black") 1 0))
               (link-count (if (find which '(:freq #|:run :run-color if I want to do these too need to filter on color/run as well |#)) (count-if (lambda (x) (string-equal (phl-target x) (phl-target link))) (phd-links o)) nil)))
          
          (when (and (numberp link-count) (> link-count max-link-count))
            (unless (zerop max-link-count)
              (setf equal-link-counts nil))
            (setf max-link-count link-count))
          
          (cond ((string-equal (phd-name o) (phd-name target))  ;; self link
                 
                 ;;; if left end loop on left side
                 ;;; if right end loop on right side
                 ;;; if in middle loop on bottom
                 
                 (cond ((= (phd-x o) (apply 'min (mapcar 'phd-x (remove-if-not (lambda (xx) (= (phd-y xx) (phd-y o))) offsets)))) ;; left end uses both left sides
                        (push (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count used
                                    (- (phd-x o) (round (phd-width o) 2)) (phd-y o)
                                    (- (phd-x o) (round (phd-width o) 2) 15) (phd-y o)
                                    (- (phd-x o) (round (phd-width o) 2) 15) (+ (phd-y o) 30)
                                    (- (phd-x o) (round (phd-width o) 2) -15) (+ (phd-y o) 30)
                                    (- (phd-x o) (round (phd-width o) 2) -15) (+ (phd-y o) 15)
                                    )
                              links))
                       ((= (phd-x o) (apply 'max (mapcar 'phd-x (remove-if-not (lambda (xx) (= (phd-y xx) (phd-y o))) offsets)))) ;; right end uses both right sides
                        (push (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count used
                                    (+ (phd-x o) (round (phd-width o) 2)) (phd-y o)
                                    (+ (phd-x o) (round (phd-width o) 2) 15) (phd-y o)
                                    (+ (phd-x o) (round (phd-width o) 2) 15) (+ (phd-y o) 30)
                                    (+ (phd-x o) (round (phd-width o) 2) -15) (+ (phd-y o) 30)
                                    (+ (phd-x o) (round (phd-width o) 2) -15) (+ (phd-y o) 15)
                                    )
                              links))
                       (t
                        (push (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count used
                                    (+ (phd-x o) 5) (+ (phd-y o) 15)
                                    (+ (phd-x o) 10) (+ (phd-y o) 25)
                                    (- (phd-x o) 10) (+ (phd-y o) 25)
                                    (- (phd-x o) 5) (+ (phd-y o) 15)
                                    )
                              links))))
                
                ((= (phd-y o) (phd-y target)) ;; same row
                 
                 ;; link bottom right of leftmost to bottom left of rightmost
                 
                 (if (> (phd-x target) (phd-x o))     ;; left to right
                     (push (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count used
                                 (+ (phd-x o) (round (phd-width o) 2)) (+ (phd-y o) 15)
                                 (+ (+ (phd-x o) (round (phd-width o) 2)) (round (- (- (phd-x target) (round (phd-width target) 2)) (+ (phd-x o) (round (phd-width o) 2))) 2)) (+ (phd-y o) 15 (round y-space 3))
                                 (- (phd-x target) (round (phd-width target) 2)) (+ (phd-y o) 15)
                                 )
                           links)
                   ;; right to left
                   (push (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count used
                               (- (phd-x o) (round (phd-width o) 2)) (+ (phd-y o) 15)
                               (- (- (phd-x o) (round (phd-width o) 2)) (round (- (- (phd-x o) (round (phd-width o) 2)) (+ (phd-x target) (round (phd-width target) 2))) 2)) (+ (phd-y o) 15 (round y-space 3))
                               (+ (phd-x target) (round (phd-width target) 2)) (+ (phd-y o) 15)
                               )
                         links)))
                
                ((> (phd-y target) (phd-y o)) ;; forward i.e. down
                 
                 ;; if one layer down draw bottom center to top center directly
                 ;; otherwise draw bottom center to top center going through next closest hole
                 
                 (if (= (- (phd-y target) (phd-y o)) (+ 30 y-space))
                     (push (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count
                                 used
                                 (phd-x o) (+ (phd-y o) 15)
                                 (phd-x target) (- (phd-y target) 15)
                                 )
                           links)
                   
                   
                   (push (concatenate 'list (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count used
                                                  (phd-x o) (+ (phd-y o) 15))
                           (let ((x (phd-x o))
                                 (vals nil))
                             (dotimes (i (- (/ (- (phd-y target) (phd-y o)) (+ 30 y-space)) 1))
                               (let* ((y (+ (phd-y o) (* (1+ i) (+ 30 y-space))))
                                      (hs (find y holes :key 'car))
                                      (min max)
                                      (min-x nil))
                                 (dolist (j (cdr hs))
                                   (when (< (abs (- x j)) min)
                                     (setf min (abs (- x j)))
                                     (setf min-x j)))
                                 (push min-x vals)
                                 (push (- y 20) vals) ;; a little slack above and below
                                 (push min-x vals)
                                 (push (+ y 20) vals)
                                 (setf x min-x)))
                             (reverse vals))
                           
                           (list (phd-x target) (- (phd-y target) 15))
                           )                         
                         links)))
                (t ;; backward i.e. up
                 
                 ;; going up draw from 1/3 left or right of center top to l/r 1/3 from center bottom
                 ;; which left and which right depends on positions
                 ;; if only one level apart then pick both sides based on position of from relative to to
                 ;;  - if from left of to then go from left to left otherwise go right to right
                 ;; if multiple levels then 
                 ;;   if from is leftmost in it's row start and end are both left
                 ;;   if from is rightmost in it's row start and end are both right
                 ;;   if to is left of from then use left for from and right for to
                 ;;   if to is right of from then use right for from and left for to
                 ;;   if same column then check which side of screen left start is on
                 ;;     if it's on the left use left for both
                 ;;     otherwise use right for both
                 ;;  all intermediates are based on next closest hole (going up)
                 
                 (if (= (- (phd-y o) (phd-y target)) (+ 30 y-space))
                     
                     ;; one level up 
                     (if (<= (phd-x o) (phd-x target))
                         (push (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count
                                     used
                                     (- (phd-x o) (round (phd-width o) 6)) (- (phd-y o) 15)
                                     (- (phd-x target) (round (phd-width target) 6)) (+ (phd-y target) 15) )
                               links)
                       (push (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count
                                   used
                                   (+ (phd-x o) (round (phd-width o) 6)) (- (phd-y o) 15)
                                   (+ (phd-x target) (round (phd-width target) 6)) (+ (phd-y target) 15) )
                             links))
                   
                   ;; multiple levels
                   
                   (let ((start-x nil)
                         (end-x nil))
                     (cond ((and (= (phd-x o) (apply 'min (mapcar 'phd-x (remove-if-not (lambda (xx) (= (phd-y xx) (phd-y o))) offsets))))
                                 (<= (- (phd-x target) (round (phd-width target) 6)) (round max 2)))   ;; left end uses both left sides if target point also on left
                            (setf start-x (- (phd-x o) (round (phd-width o) 6)))
                            (setf end-x (- (phd-x target) (round (phd-width target) 6))))
                           ((and (= (phd-x o) (apply 'max (mapcar 'phd-x (remove-if-not (lambda (xx) (= (phd-y xx) (phd-y o))) offsets))))
                                 (>= (+ (phd-x target) (round (phd-width target) 6)) (round max 2))) ;; right end uses both right sides if target also on right
                            (setf start-x (+ (phd-x o) (round (phd-width o) 6)))
                            (setf end-x (+ (phd-x target) (round (phd-width target) 6))))
                           ((< (phd-x target) (phd-x o)) ; target is to the left so start on left and end on right
                            (setf start-x (- (phd-x o) (round (phd-width o) 6)))
                            (setf end-x (+ (phd-x target) (round (phd-width target) 6))))
                           ((> (phd-x target) (phd-x o)) ; target is to the right so start on right and end on left
                            (setf start-x (+ (phd-x o) (round (phd-width o) 6)))
                            (setf end-x (- (phd-x target) (round (phd-width target) 6))))
                           ((< (phd-x o) (round max 2)) ;; same column so use the side of screen to judge and same for both
                            ;; left is left
                            (setf start-x (- (phd-x o) (round (phd-width o) 6)))
                            (setf end-x (- (phd-x target) (round (phd-width target) 6))))
                           (t ;; same column so use the side of screen to judge and same for both
                            ;; right is right
                            (setf start-x (+ (phd-x o) (round (phd-width o) 6)))
                            (setf end-x (+ (phd-x target) (round (phd-width target) 6)))))
                     
                     (push (concatenate 'list 
                             (list (phd-name o) (phd-name target) (if (phl-count link) "black" "gray") link-count used start-x (- (phd-y o) 15))
                             
                             (let ((x start-x)
                                   (vals nil))
                               (dotimes (i (- (/ (- (phd-y o) (phd-y target)) (+ 30 y-space)) 1))
                                 (let* ((y (- (phd-y o) (* (1+ i) (+ 30 y-space))))
                                        (hs (find y holes :key 'car))
                                        (min max)
                                        (min-x nil))
                                   (dolist (j (cdr hs))
                                     (when (< (abs (- x j)) min)
                                       (setf min (abs (- x j)))
                                       (setf min-x j)))
                                   
                                   (push min-x vals)
                                   (push (+ y 20) vals)
                                   
                                   (push min-x vals)
                                   (push (- y 20) vals)
                                   
                                   (setf x min-x)))
                               (reverse vals))
                             
                             (list end-x (+ (phd-y target) 15))
                             )
                           links))))))))
    
    (push-last (append  
            (let ((results nil))
              (dolist (x offsets)
                
                (when with-labels
                  (push (list "label"
                          (phd-name x) (phd-x x) (phd-y x) 
                          (- (phd-x x) (round (phd-width x) 2))
                          (- (phd-y x) 15)
                          (+ (phd-x x) (round (phd-width x) 2))
                          (+ (phd-y x) 15)
                          (if (keywordp (phd-color x)) "black" (phd-color x))
                          (cadr (assoc (phd-name x) (fourth info) :test 'string-equal)))
                        results))
                
                
                (when (eq which :utility)
                  (awhen (assoc number (phd-utilities x))
                         (push (list "box" (phd-name x)
                                 (- (phd-x x) (round (phd-width x) 2) -4)
                                 (- (phd-y x) 11)
                                 (+ (- (phd-x x) (round (phd-width x) 2) -4) (floor (* (- (phd-width x) 8) (/ (- (cdr it) min-u) (- max-u min-u)))))
                                 (- (phd-y x) 9)
                                     "blue"
                                     (if (string-equal (phd-color x) "black") 1 0))
                               results))
                
                  (awhen (assoc (1+ number) (phd-utilities x))
                         
                         (push (list "box" (phd-name x)
                          (- (phd-x x) (round (phd-width x) 2) -4)
                          (+ (phd-y x) 11)
                          (+ (- (phd-x x) (round (phd-width x) 2) -4) (floor (* (- (phd-width x) 8) (/ (- (cdr it) min-u) (- max-u min-u)))))
                          (+ (phd-y x) 9)
                                     "blue"
                                     (if (string-equal (phd-color x) "black") 1 0))
                        results)))

                (awhen (find number (phd-starts x) :key (if (or (eq which :cycle) (eq which :run) (eq which :utility)) 'car 'identity))
                       
                  (when  (or (eq which :cycle) (eq which :run) (eq which :utility))
                    (setf min-time (cdr it)))
                       
                  (push (list "box" (phd-name x)
                          (- (phd-x x) (round (phd-width x) 2) 2)
                          (- (phd-y x) 17)
                          (+ (phd-x x) (round (phd-width x) 2) 2)
                          (+ (phd-y x) 17)
                          "green" 1) results))
                
                (awhen (find number (phd-ends x) :key (if (or (eq which :cycle) (eq which :run) (eq which :utility)) 'car 'identity))
                  (when  (or (eq which :cycle) (eq which :run) (eq which :utility))
                    (setf max-time (cdr it)))
                       
                  (push (list "box" (phd-name x)
                          (- (phd-x x) (round (phd-width x) 2) -2)
                          (- (phd-y x) 13)
                          (+ (phd-x x) (round (phd-width x) 2) -2)
                          (+ (phd-y x) 13)
                          "red" 1) results)))
              results)
            
            (mapcar (lambda (x)
                      (list "link" (first x) (second x) (third x) 
                            (if (or (null (fourth x))
                                    equal-link-counts
                                    (= 1 max-link-count)
                                    (= 0 max-link-count))
                                1
                              (let ((max-width x-space))
                                (max 1 (round (* max-width (/ (fourth x) max-link-count))))))
                            (fifth x)
                            (nthcdr 5 x)))
              ;; because frequency needs to count links there are potentially duplicates but only need
              ;; to send over one of each...
              (remove-duplicates links :test (lambda (x y) (and (eq (first x) (first y)) (eq (second x) (second y)) (eq (third x) (third y))))))
            
            (when (and (or (eq which :cycle) (eq which :run) (eq which :utility))
                       (> min-time -1) (> max-time -1))
              (list
               (list "min_time" min-time)
               (list "max_time" max-time)))
            
            
            
            #| show the holes for debugging 
               (let ((z nil))
                (dolist (x holes)
                  (mapcar (lambda (y)
                            (push (format nil "link blue ~d ~d ~d ~d" y (- (car x) 15) y (+ (car x) 15)) z))
                    (cdr x)))
                z)
              |#
            
                  )
          all))
    (push (list "size" max height) all)))


;;; A command to allow getting .dot output of the graphs without using the environment.

#|
(defun production-transition-graph (&optional (which :all))
  (verify-current-model 
   "Cannot generate a production transition graph because there is no current model."
   
   (unless (find which '(:all :cycle :run :unique-cycle :unique-run))
     (print-warning "Invalid graph type.  Must be one of (:all :cycle :run :unique-cycle :unique-run).")
     (return-from production-transition-graph))
   
   (when (eq which :unique-cycle)
     (setf which :color))
   (when (eq which :unique-run)
     (setf which :run-color))
   
   (let* ((data (parse-production-history-graph (list (mapcar 'list (all-productions)) (get-module production-history)) which))
          (unused (all-productions))
          (start (caar data))
          (end (second data))
          (max-loops (case which
                       ((:all :freq) 0)
                       ((:run-color :color) (1+ (first (fourth data))))
                       ((:run :cycle) (1+ (second (fourth data)))))))
     
     (cond ((eq which :all)
            (command-output "digraph \"~s\" {" (current-model))
            (command-output "  \"~s\" [ color = green ];" start)
            
            (unless (eq start end)
              (command-output "  \"~s\" [ color = red ];" end))
            
            (dolist (node (third data))
              
              (let* ((transitions (remove-duplicates (phn-links node) :test (lambda (x y) (and (eq (phl-target x) (phl-target y)) (eql (phl-count x) (phl-count y))))))
                     (links (remove-if (lambda (x) (and (null (phl-count x)) (find (phl-target x) (remove x transitions) :key 'phl-target))) transitions)))
                (dolist (link links)
                  (if (phl-count link)
                      (progn
                        (setf unused (remove (phl-target link) unused))
                        (command-output "  \"~s\" -> \"~s\" ;" (phn-name node) (phl-target link)))
                    (command-output "  \"~s\" -> \"~s\" [ color = gray style = dashed ] ;" (phn-name node) (phl-target link))))))
            (dolist (node unused)
              (command-output "  \"~s\" [color = gray] ;" node))
            (command-output "}"))
       
           ((or (eq which :color) (eq which :run-color))
            (command-output "digraph \"~s\" {" (current-model))
            (dotimes (i max-loops)
              (command-output "  subgraph cluster_~d {~%    label = \"~a ~d\";" i (if (eq which :color) "unique cycle" "unique run") (1+ i))
              (let ((node-names (mapcar (lambda (x) (cons x (new-name-fct x))) (mapcar 'phn-name (third data))))
                    (start-node (phn-name (find i (third data) :test 'member :key 'phn-color-starts)))
                    (end-node (phn-name (find i (third data) :test 'member :key 'phn-color-ends)))
                    (used nil))
                
                
                (command-output "    \"~s\" [ color = green ];" (cdr (assoc start-node node-names))) 
                (unless (eq start-node end-node)
                  (command-output "    \"~s\" [ color = red ];" (cdr (assoc end-node node-names))))
                
                (pushnew (assoc start-node node-names) used)
                (pushnew (assoc end-node node-names) used)
                
                (dolist (node (third data))
             
                  (let* ((transitions (remove-duplicates (remove-if-not (lambda (x) (= (phl-color x) i)) (phn-links node)) :test (lambda (x y) (and (eq (phl-target x) (phl-target y)) (eql (phl-count x) (phl-count y))))))
                         (links (remove-if (lambda (x) (and (null (phl-count x)) (find (phl-target x) (remove x transitions) :key 'phl-target))) transitions)))
                    (dolist (link links)
                      (pushnew (assoc (phl-target link) node-names) used)
                      
                      (if (phl-count link)
                          (command-output "    \"~s\" -> \"~s\" ;" (cdr (assoc (phn-name node) node-names)) (cdr (assoc (phl-target link) node-names)))
                        (command-output "    \"~s\" -> \"~s\" [ color = gray style = dashed ] ;" (cdr (assoc (phn-name node) node-names)) (cdr (assoc (phl-target link) node-names)))))))
                
                (dolist (x used)
                  (command-output "    \"~s\" [ label = \"~s\" ];" (cdr x) (car x)))
                
                (command-output "  }")))
            (command-output "}"))
           ((or (eq which :cycle) (eq which :run))
            
            (command-output "digraph \"~s\" {" (current-model))
            (dotimes (i max-loops)
              (command-output "  subgraph cluster_~d {~%    label = \"~a ~d\";" i which (1+ i))
              (let ((node-names (mapcar (lambda (x) (cons x (new-name-fct x))) (mapcar 'phn-name (third data))))
                    (start-node (phn-name (find i (third data) :test 'member :key (lambda (y) (mapcar 'car (phn-cycle-starts y))))))
                    (end-node (phn-name (find i (third data) :test 'member :key (lambda (y) (mapcar 'car (phn-cycle-ends y))))))
                    (used nil))
                
                
                (command-output "    \"~s\" [ color = green ];" (cdr (assoc start-node node-names))) 
                (unless (eq start-node end-node)
                  (command-output "    \"~s\" [ color = red ];" (cdr (assoc end-node node-names))))
                
                (pushnew (assoc start-node node-names) used)
                (pushnew (assoc end-node node-names) used)
                
                (dolist (node (third data))
             
                  (let* ((transitions (remove-duplicates (remove-if-not (lambda (x) (= (phl-cycle x) i)) (phn-links node)) :test (lambda (x y) (and (eq (phl-target x) (phl-target y)) (eql (phl-count x) (phl-count y))))))
                         (links (remove-if (lambda (x) (and (null (phl-cycle x)) (find (phl-target x) (remove x transitions) :key 'phl-target))) transitions)))
                    (dolist (link links)
                      (pushnew (assoc (phl-target link) node-names) used)
                      
                      (if (phl-count link)
                          (command-output "    \"~s\" -> \"~s\" ;" (cdr (assoc (phn-name node) node-names)) (cdr (assoc (phl-target link) node-names)))
                        (command-output "    \"~s\" -> \"~s\" [ color = gray style = dashed ] ;" (cdr (assoc (phn-name node) node-names)) (cdr (assoc (phl-target link) node-names)))))))
                
                (dolist (x used)
                  (command-output "    \"~s\" [ label = \"~s\" ];" (cdr x) (car x)))
                
                (command-output "  }")))
            (command-output "}"))))))

|#


(defun reset-p-history-module (module)
  (bt:with-lock-held ((p-history-module-lock module))
    (setf (p-history-module-history module) nil)
    (setf (p-history-module-why-not-list module) nil)))
  

(defun create-production-history-module (x)
  (declare (ignore x))
  (make-p-history-module))

(defun delete-production-history-module (x)
  (bt:with-lock-held ((p-history-module-lock x))
    (setf (p-history-module-alive x) nil)))

(define-module-fct 'production-history nil nil  
  :creation 'create-production-history-module
  :delete 'delete-production-history-module
  :reset 'reset-p-history-module
  :run-start 'production-history-start-markers
  :run-end 'production-history-stop-markers
  :version "2.1"
  :documentation "Module to record production history for display in the environment.")
  


(defun get-p-history-data ()
  (let ((module (get-module production-history)))
    (when module
      (bt:with-lock-held ((p-history-module-lock module))
        (let (data)
          (dolist (x (p-history-module-history module))
            (push 
             (case (p-history-tag x)
               (:start (list (p-history-time x) "start"))
               (:stop (list (p-history-time x) "stop" (p-history-info x)))
               (:reward (list (p-history-time x) "reward" (p-history-reward x) (p-history-info x)))
               (t (list (p-history-time x) "conflict-resolution" (p-history-selected x) (p-history-matched x) (p-history-mismatched x) (p-history-info x))))
             data))
          (push (list 0 "info" (p-history-module-why-not-list module) (mapcar (lambda (x) (list x (printed-production-text x nil))) (all-productions))) data)
          data)))))


(defun enable-p-history () 
  (no-output
   (unless (find 'production-history-recorder (car (sgp :conflict-set-hook)))
     (sgp :conflict-set-hook production-history-recorder))
   (unless (find 'production-history-reward-markers (car (sgp :reward-notify-hook)))
     (sgp :reward-notify-hook production-history-reward-markers)))
  (let ((m (get-module production-history)))
    (bt:with-lock-held ((p-history-module-lock m))
      (setf (p-history-module-enabled m) t))))

(defun disable-p-history ()
  (no-output
   (let ((old-hooks (car (sgp :conflict-set-hook))))
     (when (find 'production-history-recorder old-hooks)
       (sgp :conflict-set-hook (:remove production-history-recorder)))
     (setf old-hooks (car (sgp :reward-notify-hook)))
     (when (find 'production-history-reward-markers old-hooks)
       (sgp :reward-notify-hook (:remove production-history-reward-markers)))))
  (let ((m (get-module production-history)))
    (bt:with-lock-held ((p-history-module-lock m))
      (setf (p-history-module-enabled m) nil))))

(defun p-history-status ()
  (let ((m (get-module production-history)))
    (if m
        (bt:with-lock-held ((p-history-module-lock m))
          (no-output 
           (values 
            (and (find 'production-history-recorder (car (sgp :conflict-set-hook)))
                 (find 'production-history-reward-markers (car (sgp :reward-notify-hook)))
                 t)
            (when (p-history-module-history m) t)
            (p-history-module-alive m))))
      (values nil nil nil))))


(define-history "production-history" enable-p-history disable-p-history p-history-status get-p-history-data t)


(defun compute-production-grid (p-data)
  (let ((data (json:decode-json-from-string p-data))
        (results nil)
        (p-list nil))
    (dolist (x data)
      (case (read-from-string (second x))
        (info
         (push-last x results)
         (setf p-list (mapcar 'first (fourth x))))
        (conflict-resolution
         (push-last (list (first x)
                          "conflict-resolution"
                          (if (third x) t)
                          (let (r)
                            (dolist (y p-list)
                              (push-last (cond ((string-equal y (third x))
                                                (let ((details (find y (sixth x) :key 'first :test 'string-equal)))
                                                  (list 0 (second details) (third details))))
                                               ((find y (fourth x) :test 'string-equal)
                                                (let ((details (find y (sixth x) :key 'first :test 'string-equal)))
                                                  (list 1 (second details) (third details))))
                                               ((find y (fifth x) :test 'string-equal)
                                                (let ((details (find y (sixth x) :key 'first :test 'string-equal)))
                                                  (list 2 (second details))))
                                               (t
                                                (list -1)))
                                         r))
                            r))
                    results))))
    results))
    

(define-history-processor "production-history" "production-grid" compute-production-grid)

(defun production-graph-all (x) 
  (create-production-graph-coords x :all))
(defun production-graph-freq (x) 
  (create-production-graph-coords x :freq))
(defun production-graph-cycles (x) 
  (create-production-graph-coords x :cycle))
(defun production-graph-u-cycles (x) 
  (create-production-graph-coords x :color))
(defun production-graph-runs (x) 
  (create-production-graph-coords x :run))
(defun production-graph-u-runs (x) 
  (create-production-graph-coords x :run-color))
(defun production-graph-utility (x) 
  (create-production-graph-coords x :utility))

(define-history-processor-fct "production-history" "production-graph-all" 'production-graph-all)
(define-history-processor-fct "production-history" "production-graph-freq" 'production-graph-freq)
(define-history-processor-fct "production-history" "production-graph-cycles" 'production-graph-cycles)
(define-history-processor-fct "production-history" "production-graph-u-cycles" 'production-graph-u-cycles)
(define-history-processor-fct "production-history" "production-graph-runs" 'production-graph-runs)
(define-history-processor-fct "production-history" "production-graph-u-runs" 'production-graph-u-runs)
(define-history-processor-fct "production-history" "production-graph-utility" 'production-graph-utility)


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
