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
;;; Filename    : printing.lisp
;;; Version     : 2.0
;;; 
;;; Description : Module that provides model output control.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] * Add a warning level parameter and handling.
;;;             : [ ] * Consider using the ACT-R logical host as the default for
;;;             :       when a string of name only is used for a trace instead of
;;;             :       just letting the system defaults kick in.
;;; 
;;; ----- History -----
;;;
;;; 2004.09.17 Dan
;;;             : Creation
;;; 2005.01.17 Dan
;;;             : * Added the :cmdt parameter to handle outputting of commands
;;;             :   independently of other model output.
;;; 2005.01.18 Dan
;;;             : * Moved the printing-module structure to internal-structures.
;;; 2005.01.27 Dan
;;;             : * Updated the version to 1.0a3.
;;;             : * Added the :trace-filter parameter and the function
;;;             :   production-firing-only as a possible filter.
;;;             : * Added the function filter-output-events which uses the
;;;             :   trace-filter to determine whether to output info or not.
;;; 2005.02.02 Dan
;;;             : * Added the :trace-detail parameter to control how much
;;;             :   gets printed in the trace - depends on the new specification
;;;             :   of the event's output parameter.
;;; 2005.02.03 Dan
;;;             : * Moved production-firing-only to the procedural-cmds file.
;;; 2005.02.21 Dan
;;;             : * Cleared the to do that was completed and added a new one.
;;; 2005.02.28 Dan
;;;             : * Fixed a bug in the reseting when an output stream was a
;;;             :   file.
;;; 2005.04.19 Dan
;;;             : * Added event-displayed-p as a user function for events.
;;; 2005.05.11 Dan
;;;             : * Changed the default for trace-detail to medium because
;;;             :   it's been bulked up and may be more reasonable as the
;;;             :   general trace.
;;; 2005.09.08 Dan
;;;             : * Added the :model-warnings parameter to suppress all "model-
;;;             :   warning" calls.
;;; 2006.01.17 Dan
;;;             : * Updated the version to 1.0 since there haven't been any
;;;             :   recent problems it's time to drop the "a".
;;; 2006.01.18 Dan
;;;             : * Adding a new paramter :show-all-slots to allow one to hide
;;;             :   "unset" extended slots in chunks if desired (the default
;;;             :   is to hide them).
;;; 2006.07.17 Dan
;;;             : * Added a with-model-fct to filter-output-events so that the
;;;             :   printing module of the model in which the event was 
;;;             :   generated is used to make the determination instead of the
;;;             :   current model's.
;;; 2006.07.20 Dan
;;;             : * Realized that the change above isn't quite right because
;;;             :   break events don't have a model, so in that case it just
;;;             :   uses the "first" model.
;;; 2006.10.11 Dan
;;;             : * Realized while doing some profiling on a different change
;;;             :   that the backqouted code in filter-output-events was really
;;;             :   slow (in ACL 20+% of the model's time was spent evaluating 
;;;             :   that).  I've replaced that with a call to a separate 
;;;             :   function and things seem to be back to better performance.
;;; 2006.10.16 Dan
;;;             : * Bug in the last change doesn't work in all Lisps - fixed
;;;             :   that now (difference between #' and ' for a function name
;;;             :   that gets passed to eval).
;;; 2007.04.13 Dan
;;;             : * Added the :cbct parameter which determines whether or not
;;;             :   an extra event shows in the trace indicating when a buffer
;;;             :   copies a chunk to give the old and new names.
;;; 2008.10.30 Dan
;;;             : * Added the show-copy-buffer-trace function to avoid needing
;;;             :   an sgp in the buffer code.
;;; 2010.05.03 Dan
;;;             : * Changed filter-test so that it doesn't display ":output 'high"
;;;             :   events in the medium trace detail level.
;;; 2012.09.21 Dan
;;;             : * Set suppress-cmds back to nil upon a reset because apparently
;;;             :   even with an unwind-protect it's possible for it to get stuck
;;;             :   somehow.
;;; 2013.08.16 Dan
;;;             : * Fix a problem with CCL and the environment which seems like
;;;             :   it can only be dealt with here (I don't like implementation
;;;             :   hacks in the main code).  By default an opened stream is 
;;;             :   only available to the thread which opens it, but if one sets
;;;             :   :v or :cmdt to a pathname and then Loads the file through
;;;             :   the environment in CCL that stream is unavailable to the
;;;             :   thread which "runs" the model and an error results.  The 
;;;             :   fix is to specify :sharing :lock in the open command and
;;;             :   that has to happen here.
;;; 2014.06.26 Dan
;;;             : * More error protection around the setting of :v and :cmdt
;;;             :   when files/streams are used.
;;; 2014.08.14 Dan
;;;             : * The :show-all-slots parameter is unnecessary now.
;;; 2015.06.01 Dan [1.1]
;;;             : * Adding a one-time-model-warning command (it's in misc-utils
;;;             :   with the other output commands) which allows one to specify
;;;             :   a "tag" for the warning and it will only print a warning
;;;             :   with a given tag (equal test) the first time it occurs after 
;;;             :   a reset.
;;; 2016.04.11 Dan [1.2]
;;;             : * Adding a parameter to have the module save the trace info
;;;             :   whether or not it's being printed and a command to print
;;;             :   out a saved trace at any detail level for an optionally
;;;             :   specified subsegment.  For now this is going to be crude and
;;;             :   just store a list of the formatted output for each event
;;;             :   since that's easy but costly in terms of space.  Some sort 
;;;             :   of caching would probably be better for space (since something
;;;             :   like conflict-resolution is going to show up a lot) but has
;;;             :   a bigger time cost since a lookup would have to happen for
;;;             :   every event.
;;; 2016.04.12 Dan
;;;             : * Added a get-saved-trace function to allow access to the data
;;;             :   without having to print it out or wrap it in a no-output, and
;;;             :   include the time and output level with that.
;;; 2016.04.13 Dan
;;;             : * Fixed a copy-and-paste error in get-saved-trace.
;;; 2016.04.14 Dan
;;;             : * Might as well save the event itself and recreate the trace
;;;             :   after the fact if needed. 
;;;             : * Don't return the trace items from show-saved-trace since
;;;             :   get-saved-trace can be used for that -- have each do its
;;;             :   own thing.
;;; 2016.04.21 Dan
;;;             : * Minor improvement to internals of show and get saved-trace.
;;; 2016.09.28 Dan
;;;             : * Changed printing-module-event-hook to just be nil or cons since
;;;             :   there's only one meta-process.
;;; 2017.02.27 Dan
;;;             : * Clear the capture warnings flag and any recorded warnings 
;;;             :   when the module is reset.  [Clears all of them since it's a
;;;             :   class allocated slot, but since there's only one mp a reset
;;;             :   affects all models anyway.]
;;; 2017.06.15 Dan
;;;             : * Wrapped the lock around the warning capture clearing for
;;;             :   thread safety.
;;;             : * Make event-displayed-p, filter-events, and filter-output-events
;;;             :   thread safe.
;;; 2017.06.16 Dan
;;;             : * Use the trace lock for access to the saved trace info.
;;; 2017.07.13 Dan
;;;             : * Protect the cbct parameter through show-copy-buffer-trace.
;;; 2017.09.19 Dan [1.3]
;;;             : * Remove the :save-trace parameter because that functionality
;;;             :   is being handled by a history stream now outside of the
;;;             :   module.
;;; 2018.02.05 Dan
;;;             : * Allow the :trace-filter parameter to accept a command name
;;;             :   string.
;;;             : * Update event-displayed-p to accept a real event or id, and
;;;             :   always pass the id to the trace-filter fn.
;;; 2018.02.07 Dan
;;;             : * The trace filter needs to get the model name.
;;; 2018.02.22 Dan
;;;             : * Add a remote version of event-displayed-p.
;;; 2018.06.14 Dan
;;;             : * Updated doc string for event-displayed-p command.
;;; 2019.01.16 Dan
;;;             : * Adjust filter-event so it only needs to grab the lock once.
;;; 2019.06.18 Dan
;;;             : * Change the ccl hack from :sharing :lock to :sharing :external.
;;; 2020.01.13 Dan [1.4]
;;;             : * Removed the #' and lambdas from the module interface. 
;;; 2020.06.03 Dan
;;;             : * Have the filter-test return the value of v as well so that
;;;             :   scheduling can skip the model-output immediately if it's
;;;             :   off.
;;; 2020.07.24 Dan
;;;             : * Fixed the problem when :v and :cmdt were both set to files
;;;             :   because they used separate streams and thus were both
;;;             :   writing to different positions in the file.  Now, if they're
;;;             :   set to a file/pathname it uses one stream for both.
;;; 2021.10.19 Dan [2.0]
;;;             : * Set the required flag on the module.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
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


(defun create-printing-module (model-name)
  (declare (ignore model-name))
  (make-instance 'printing-module))

(defun verify-printing-param (param)
  (or (null param) (eq param t) (streamp param) (pathnamep param)
      (and (stringp param) (parse-namestring param))))

(defun safe-close-printing-stream (target other cmd)
  (if (act-r-output-shared target)
      (setf (act-r-output-shared other) nil (act-r-output-shared target) nil)
    (multiple-value-bind (result err)
        (ignore-errors (close (act-r-output-stream target)))
      (when (and err (subtypep (type-of err) 'condition))
        (print-warning "Error encountered when trying to close the file associated with the ~s parameter:~% ~s" cmd err))
      result)))

(defun safe-open-printing-stream (name target other cmd)
  (if (and (act-r-output-file other)
           (equalp name (act-r-output-name other)))
      ;; if it's the same name as the other one
      ;; indicate they're shared and return the other's stream
      (progn
        (setf (act-r-output-shared target) t)
        (setf (act-r-output-shared other) t)
        (setf (act-r-output-name target) name)
        
        (act-r-output-stream other))
      
    (multiple-value-bind (result err)
        (ignore-errors (open (parse-namestring name)
                             :direction :output :if-exists :append 
                             :if-does-not-exist :create
                             #+:ccl :sharing #+:ccl :external))
      (if (and err (subtypep (type-of err) 'condition))
          (progn
            (setf (act-r-output-name target) nil)
            (setf (act-r-output-file target) nil)
            (print-warning "Error encountered when trying to open the file associated with the ~s parameter:~% ~s" cmd err)
            (print-warning "The ~s parameter is being set to t instead." cmd))
        (progn
          (setf (act-r-output-name target) name)
          ))
      result)))

(defun printing-module-param (module param)
  (bt:with-recursive-lock-held ((printing-module-param-lock module)) 
    (if (consp param)
        (case (car param)
          (:v
           (when (act-r-output-file (printing-module-v module))
             (safe-close-printing-stream (printing-module-v module) (printing-module-c module) :v)
             (setf (act-r-output-file (printing-module-v module)) nil)
             (setf (act-r-output-name (printing-module-v module)) nil))
           
           (setf (act-r-output-stream (printing-module-v module))
             
             (cond ((or (pathnamep (cdr param)) (stringp (cdr param)))
                    (aif (safe-open-printing-stream (cdr param) (printing-module-v module) (printing-module-c module) :v)
                         (progn
                           (setf (act-r-output-file (printing-module-v module)) t)
                           it)
                         t))
                   (t 
                    (setf (act-r-output-file (printing-module-v module)) nil)
                    (setf (act-r-output-name (printing-module-v module)) nil)
                    (cdr param)))))
          (:cmdt
           (when (act-r-output-file (printing-module-c module))
             (safe-close-printing-stream (printing-module-c module) (printing-module-v module) :cmdt)
             (setf (act-r-output-file (printing-module-c module)) nil)
             (setf (act-r-output-name (printing-module-c module)) nil))
           
           (setf (act-r-output-stream (printing-module-c module))
             
             (cond ((or (pathnamep (cdr param)) (stringp (cdr param)))
                    (aif (safe-open-printing-stream (cdr param) (printing-module-c module) (printing-module-v module) :cmdt)
                         (progn
                           (setf (act-r-output-file (printing-module-c module)) t)
                           it)
                         t))
                   (t 
                    (setf (act-r-output-file (printing-module-c module)) nil)
                    (setf (act-r-output-name (printing-module-c module)) nil)
                    (cdr param)))))
          (:trace-filter
           (setf (printing-module-filter module) (cdr param)))
          (:trace-detail
           (setf (printing-module-detail module) (cdr param)))
          (:model-warnings
           (setf (printing-module-model-warnings module) (cdr param)))
          (:cbct
           (setf (printing-module-cbct module) (cdr param)))
          
          )
      
      (case param
        (:v (act-r-output-stream (printing-module-v module)))
        (:cmdt (act-r-output-stream (printing-module-c module)))
        (:trace-filter (printing-module-filter module))
        (:trace-detail (printing-module-detail module))
        (:model-warnings (printing-module-model-warnings module))
        (:cbct (printing-module-cbct module))
        ))))

(defun reset-printing-module (module)
  (bt:with-recursive-lock-held ((printing-module-param-lock module))
    (when (act-r-output-file (printing-module-v module))
      (close (act-r-output-stream (printing-module-v module)))
      (setf (act-r-output-file (printing-module-v module)) nil))
    (setf (act-r-output-stream (printing-module-v module)) t)
  
    (when (act-r-output-file (printing-module-c module))
      (close (act-r-output-stream (printing-module-c module)))
      (setf (act-r-output-file (printing-module-c module)) nil))
    (setf (act-r-output-stream (printing-module-c module)) t)
  
    (setf (printing-module-one-time-tags module) nil)
    (setf (printing-module-filter module) nil)
    (setf (printing-module-detail module) 'high)
    (setf (printing-module-suppress-cmds module) nil))
    
    
  (bt:with-recursive-lock-held ((printing-module-lock module))
    (setf (printing-module-capture-warnings module) nil)
    (setf (printing-module-captured-warnings module) nil)))


(defun delete-printing-module (module)
  (reset-printing-module module))

(defun trace-detail-value-test (x)
  (or (eq x 'high)
      (eq x 'medium)
      (eq x 'low)))


(define-module-fct 'printing-module nil 
  (list 
   (define-parameter :v :documentation "Verbose controls model output"
     :default-value t
     :warning "must be t, nil, a stream, pathname or namestring"
     :valid-test 'verify-printing-param)
   (define-parameter :cmdt :documentation "Commands trace controls output of commands"
     :default-value t
     :warning "must be t, nil, a stream, pathname or namestring"
     :valid-test 'verify-printing-param)
   (define-parameter :trace-filter :documentation "Function to limit output shown in the trace"
     :default-value nil
     :warning "must be a function, string naming a command, or nil"
     :valid-test 'local-or-remote-function-or-nil)
   (define-parameter :trace-detail :documentation "Determines which events show in the trace"
     :default-value 'medium
     :warning "Must be one of high, medium, or low"
     :valid-test 'trace-detail-value-test)
   (define-parameter :model-warnings :documentation "Whether to output model warnings"
     :default-value t
     :warning "must be t or nil"
     :valid-test 'tornil)
   (define-parameter :cbct :documentation "Whether or not to show an event in the trace when a buffer copies a chunk"
     :default-value nil
     :warning "must be t or nil"
     :valid-test 'tornil)
   )
  :version "2.0"
  :documentation "Coordinates output of the model."
  :creation 'create-printing-module
  :reset 'reset-printing-module
  :delete 'delete-printing-module
  :params 'printing-module-param
  :required t)

(defun filter-output-events (event)
  (with-model-eval (if (act-r-event-model event) (act-r-model-name (act-r-event-model event)) (first (mp-models))) ;; just use the first if there isn't one (a break event)
    (filter-test event)))

(defun filter-test (event)
  (let ((module (get-module printing-module)))
    (when module 
      (let (d f v)
        (bt:with-recursive-lock-held ((printing-module-param-lock module)) 
          (setf d (printing-module-detail module))
          (setf f (printing-module-filter module))
          (setf v (printing-module-v module)))
        (values 
         (and (case d
                (low (eq (act-r-event-output event) 'low))
                (medium (or (eq (act-r-event-output event) 'low)
                            (eq (act-r-event-output event) 'medium)))
                (high t))
              (or (null f)
                  (dispatch-apply f (act-r-event-num event))))
         v)))))
  

(defun event-displayed-p (evt)
  (let ((event (if (act-r-event-p evt) evt (get-event-by-id evt))))
    (and event
         (act-r-event-output event)
         (filter-output-events event))))

(add-act-r-command "event-displayed-p" 'event-displayed-p "Return whether the specified event will be shown in the trace based on its output value, the current trace detail, and the current trace filter. Params: event-id")


(defun show-copy-buffer-trace ()
  (let ((module (get-module printing-module)))
    (bt:with-recursive-lock-held ((printing-module-param-lock module))
      (printing-module-cbct module))))


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
