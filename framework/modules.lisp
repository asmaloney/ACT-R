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
;;; Filename    : modules.lisp
;;; Version     : 3.2
;;; 
;;; Description : Code for defining and using the modules.
;;; 
;;; Bugs        : 
;;;
;;; To do       : X Finish the documentation.
;;;             : X Write an undefine function.
;;;             : X Test better so that a module can't be created with models
;;;             :   already defined otherwise bad things happen.
;;;             : * Look into an undefine "on the fly" for when a remotely
;;;             :   defined module goes away.
;;; 
;;; ----- History -----
;;;
;;; 2004.08.13 Dan
;;;             : Creation.
;;;
;;; 2004.12.23 Dan
;;;             : Modified get-module and get-module-fct so that :device
;;;             :   returns the current device interface.
;;; 2005.01.12 Dan
;;;             : * Since the device is now a true module don't need to
;;;             :   do the hack listed above - the :device module is the current
;;;             :   device-interface.
;;;             : * Changed the update function so that it gets the old and
;;;             :   new times as well as the module instance.
;;; 2005.02.22 Dan
;;;             : * Updated the to do list with a new issue.
;;; 2005.02.25 Dan
;;;             : * Removed the ~\newline usages because that causes problems
;;;             :   when a Lisp only wants to see native new lines there.
;;; 2005.03.23 Dan
;;;             : * Changed how the reset parameter to define-module is
;;;             :   interpreted to allow for different reset possibilities.
;;;             :   There are two possible reset functions now - one before
;;;             :   and one after the parameters take the default values.
;;;             : * Added the secondary-reset-module function to support
;;;             :   the additional reset function.
;;; 2005.03.24 Dan
;;;             : * Patched define-module relative to the reset change so
;;;             :   that it doesn't break on fboundp in Lispworks.
;;; 2005.04.19 Dan
;;;             : * Added all-module-names function to make some things easy.
;;; 2005.05.03 Dan
;;;             : * Added some more warnings to define-module-fct.
;;; 2005.08.09 Dan 
;;;             : * Modified define-module so that you can't add a module while
;;;             :   there are any models defined or any meta-processes other
;;;             :   than the default.
;;;             : * Made undefine-module work now so that one can remove a
;;;             :   module to redefine it - don't want to make that automatic
;;;             :   to prevent any module collision issues.
;;;             : * Update the version to 1.0.
;;; 2005.08.16 Dan
;;;             : * Update query-module so that one can now query "error t"
;;;             :   or "error nil" but the module writer doesn't have to do
;;;             :   anything else - just reporting "state error" queries is
;;;             :   sufficient because the mapping occurs automatically i.e.
;;;             :   if "state error" returns t then "error t" is t and "error
;;;             :   nil" is nil or if "state error" is returns nil then "error
;;;             :   t" is nil and "error nil" is t.   
;;; 2006.11.15 Dan
;;;             : * Changed the warnings in the buffer-mod-module function to
;;;             :   be a little clearer.
;;; 2006.11.20 Dan
;;;             : * Added the warn-module and warn-module? commands so that
;;;             :   a module can receive an advance warning of an approaching
;;;             :   action if it needs to do something.  That warning gets 
;;;             :   called when a production which will make a request to the 
;;;             :   module is selected (during the conflict resolution process).
;;;             :   Right now it's only needed by vision to lock-out the proc-
;;;             :   display, but other modules may need such functionality too.
;;;             : * The warning keyword parameter has been added to define-module
;;;             :   for setting the warning function.
;;; 2007.06.19 Dan
;;;             : * Removed a duplicate definition of warn-module.
;;; 2008.08.28 Dan
;;;             : * Fixed get-module so that it always returns two values - even
;;;             :   when there is no current model or meta-process.
;;; 2008.08.29 Dan
;;;             : * Fixed a bug with the test for multiple meta-processes in
;;;             :   undefine-module-fct.
;;;             : * Fixed a typo in a warning of undefine-module-fct.
;;; 2008.09.03 Dan
;;;             : * Changed the define-module macro to only splice in the 
;;;             :   version and docs if provided so that the warning displays
;;;             :   the same as it does for define-module-fct in that respect.
;;; 2008.12.01 Dan
;;;             : * Added the option of specifying a third reset function when
;;;             :   defining a module.  The third reset function gets called
;;;             :   after the user code is evaled.
;;; 2009.09.09 Dan
;;;             : * Define-module updated to allow the specification of the
;;;             :   new multi-buffers.  An entry for the buffers-list can now
;;;             :   have an optional 6th element.  If a buffer's definition 
;;;             :   list has a non-nil 6th element then it will be created as
;;;             :   a multi-buffer.  If that 6th element is the keyword :search
;;;             :   then it will be a searchable buffer.
;;;             :   The module definition can also now provide two additional 
;;;             :   functions which may be used in conjunction with a searchable
;;;             :   buffer.  They are specified with the :search and :offset 
;;;             :   keywords.  The search function will be called at the start
;;;             :   of a conflict-resolution event for each searchable buffer.
;;;             :   It will be passed the module instance and a buffer name.
;;;             :   It should return a list of the chunk names from the buffer's
;;;             :   chunk set in the order in which they should be searched (car
;;;             :   of the list will be first searched).  If no such function is
;;;             :   provided then the chunks will be searched in an arbitrary
;;;             :   order.
;;;             :   The offset function will be called near the end of the conflict-
;;;             :   resolution event (after production matching but before the
;;;             :   final choice).  It will be passed the module instance, the
;;;             :   name of the buffer and a list of the chunks which were matched
;;;             :   in productions that may be fired.  It should return a list of
;;;             :   numbers that represent the offsets to the utilities for the
;;;             :   productions which matched those chunks i.e. the first element
;;;             :   in the list returned will be the offset for productions which
;;;             :   matched the first chunk in the list provided and so on.  If no
;;;             :   offset function is specified or no list returned by it then no
;;;             :   offset will be applied and the standard utility will be used
;;;             :   to choose the production to fire.
;;; 2009.09.11 Dan
;;;             : * Added functions to call the search and offset hooks of the
;;;             :   module given only the buffer name.
;;; 2010.01.15 Dan
;;;             : * Added the :search and :offset keywords to the define-module
;;;             :   macro too.
;;; 2010.12.22 Dan
;;;             : * Added the run-notify option to a module to allow it to provide
;;;             :   a hook for when the scheduler starts running.
;;; 2011.01.11 Dan
;;;             : * Added the run-over-notify option to go along with the run-notify
;;;             :   which gets called at the completion of a run.
;;;             : * Changing the define-module keywords to :run-start and :run-end.
;;; 2011.04.27 Dan
;;;             : * Added some declaims to avoid compiler warnings about 
;;;             :   undefined functions.
;;; 2014.03.17 Dan [2.0]
;;;             : * Changed warn-module because now it's called with a chunk-spec
;;;             :   instead of just the type.
;;; 2014.09.26 Dan
;;;             : * All-module-names now returns a list sorted by module name
;;;             :   which should be used anywhere the current maphashing is done.
;;; 2014.11.11 Dan
;;;             : * Fixed a bug in define-module with the check for multiple
;;;             :   meta-processes.
;;; 2016.09.28 Dan
;;;             : * Removed calls to meta-process-name and checks for more than
;;;             :   one meta-process from define and undefine module.
;;; 2017.06.15 Dan
;;;             : * Making get-module-fct and max-module-name-length thread safe.
;;; 2017.06.29 Dan
;;;             : * Making everything here thread safe.
;;; 2017.12.07 Dan
;;;             : * Adding remote module commands for add and undefine, and also
;;;             :   adjusting everything to actually work with a remotely defined
;;;             :   module (calling dispatch fn's and converting results as needed).
;;; 2017.12.18 Dan
;;;             : * Getting the declaim right for define-parameter so that it
;;;             :   doesn't result in warnings/errors in ACL and CCL.
;;; 2018.02.23 Dan
;;;             : * Allow an external module to specify its own instance instead
;;;             :   of forcing it to be the model name.
;;; 2018.06.14 Dan
;;;             : * Rework the parameters for the remote define module so that
;;;             :   it uses the options list functionality and looks a lot more
;;;             :   like define-module, and then just pass the parameters to the
;;;             :   internal command instead of having a parallel remote version.
;;; 2018.07.27 Dan
;;;             : * Added a test for buffer-list actually being a list in define-
;;;             :   module.
;;;             : * Process-parameters should pass the param name or a list of two
;;;             :   values to a remote command not a list of 1 or two items.
;;;             : * Get-module always returns two values now.
;;; 2018.08.01 Dan
;;;             : * Process-parameters should always pass a list to the remote
;;;             :   command because testing for 1 or 2 elements in a list is
;;;             :   something that should be easy in any language, but testing
;;;             :   for the difference between a string and a list of two items
;;;             :   is likely more complicated.
;;; 2019.01.17 Dan
;;;             : * Fixed a bug in a warning with m-buffer-offset.
;;; 2019.03.15 Dan
;;;             : * Getting rid of most get-abstract-module calls by storing the
;;;             :   abstract-module in the table with the instance so that get-
;;;             :   module returns both -- the exists is the abstract!
;;; 2019.04.03 Dan
;;;             : * The functions called from internal code (like reset-module)
;;;             :   are now passed the module instance and abstract module.
;;; 2019.04.15 Dan
;;;             : * Replace splice-into-list with splice-into-position-des and
;;;             :   don't setf with the returned value since it's destructive.
;;; 2019.05.24 Dan [3.0] 
;;;             : * Add code to check that a remote module still has valid  
;;;             :   interface functions and undefine it if it doesn't.  Can only
;;;             :   be called at clear-all time, and clear-all now does so.
;;; 2019.07.02 Dan
;;;             : * Actually set the external flag on the remote modules.
;;; 2020.01.31 Dan [3.1]
;;;             : * Added a remote all-module-names.
;;; 2020.08.25 Dan
;;;             : * Changed the declaim for buffers to avoid warning in SBCL.
;;; 2020.09.10 Dan [3.2]
;;;             : * Added the check-module-version command for testing the 
;;;             :   version string of a module against a provided version.
;;; 2020.09.11 Dan 
;;;             : * Deleted a needed function for check-module-version before
;;;             :   I committed the last one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; The globals and structures are not for general use.  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;; define-module
;;; 
;;; (defmacro define-module (module-name buffer-list 
;;;                          &key (version "Unspecified") (documentation "")
;;;                               (creation nil) (reset nil) (query nil)
;;;                               (request nil) (buffer-mod nil) (params nil) 
;;;                               (init-params nil) (delete nil) 
;;;                               (notify-on-clear nil)))
;;;                                                       
;;; module-name a symbol that will name the module
;;; buffer-list a list of the buffer name specifications that this module will use
;;; version a string that can be used for version control and bug tracking
;;; documentation a string that describes the module
;;; creation a function, function name or nil
;;; reset a function, function name or nil 
;;; query a function, function name or nil 
;;; request a function, function name or nil 
;;; buffer-mod a function, function name or nil 
;;; init-params a function, function name or nil 
;;; params a function, function name or nil 
;;; delete a function, function name or nil 
;;; notify-on-clear a function, function name or nil
;;; 
;;; define-module creates a new module for the system which will be referenced by module-name and provides the buffers named in buffer-list as an interface.  The operation of the module is controlled by the remaining parameters which are described in detail below.  If a module is successfully defined, then module-name is returned.
;;; 
;;; If there is already a module named module-name or buffers with any of the names in buffer-list then a warning is displayed, no module is created and nil is returned.
;;; 
;;; An element of the buffer-list can be a symbol that names the buffer or a list with additional details about the buffer.  If it is a list, then the first element must be a symbol that names the buffer, the second element describes the spreading activation parameter for the buffer, the third specifies any request parameters and the fourth any query parameters.  
;;; 
;;; The spreading activation description can be a keyword that specifies the parameter to use in setting the activation spreading from the buffer (the default parameter name will be :buffer-activation where buffer  is the name of the buffer) or a list of the keyword that specifies the parameter and a default value for the activation parameter (the default default value is 0 and should be left there) or nil if the default values should be used for both (necessary if the third or fourth elements for the buffer details are needed).
;;; 
;;; The request parameters specification is a list of the keywords that can be used in any request for this buffer regardless of the chunk-type used in the request.
;;; 
;;; The query parameters specification is a list of symbols (not keywords) which can be used as the slot names for a query request.
;;; 
;;; 
;;; version should be set to a string that indicates in some fashion a version for this module.  Every module should have a version and every update to the module should change that version.  This will be displayed when the mp-print-versions command is called.
;;; 
;;; documentation should be a string that contains some brief documentation about the purpose of the module. This will also be displayed when the mp-print-versions command is called.
;;; 
;;; The remainder of the parameters (creation, reset, state, request, buffer-mod, params, delete, and notify-on-clear) are for specifying the functions that interface the module to the framework.  Those functions will be called by the framework as necessary.  The situations in which they will be called and the parameters that will be passed to them are described below.  All of the functions are optional, and by leaving the corresponding parameter as nil, the module will not be called for that particular situation.
;;; 
;;; creation
;;;  
;;; The creation function will be called only once per instantiation of the module, when a model is first created.  The creation function will be passed one parameter which will be the name of the model in which the module is currently being instantiated.
;;; 
;;; It should return something that identifies this instance of the module for use by the other functions of the module as described below.  The return value of the creation function is the "instance" of this module.  If there is no create function for a module the instance will be nil for all instantiations of that module.
;;; 
;;; reset
;;; 
;;; The reset function will be called after the creation function is called and every time a model containing an instance of the module is reset.  The reset function will be passed one parameter which will be the instance of the module for that model.
;;; 
;;; The reset function should be used to reinitialize the module and typical tasks would be to define chunk-types and chunks that are used by the module. 
;;; 
;;; The return value from the reset function is ignored. 
;;; 
;;; query 
;;; 
;;; The query function is used to report on the state of the module and its buffer or any other "instant" tests the module provides.  It will be called in response to any query request being made to any of the buffers of the module.  The query function will be passed four parameters.  The first will be the instance of the module for the model in which the buffer request was made.  The second will be the name of the buffer to which the request was made, the third will be the name of a slot specification and the fourth will be a value for that slot. 
;;; 
;;; If the return value from the query function is nil then that indicates that the requested test has failed.  Any other value is considered a successful response to the query.
;;; 
;;; If a module provides a query function it must be able to respond to the slot state with possible values: busy, free, or error and the slot buffer with possible values: empty, full, or stuffed(?).
;;; 
;;; Stylistically, this function should not have any persistent or time delayed effects nor should it schedule any actions because that is what the "true" requests are for and the distinction should be maintained for consistency.
;;; 
;;; request
;;; 
;;; The request function is used to respond to requests made to the buffers of the module.  It will be called in response to requests being made to any of the buffers of the module.  The request function will be passed three parameters.  The first will be the instance of the module for the model in which the buffer request was made.  The second will be the name of the buffer to which the request was made, and the third will be the chunk specification that was sent to the buffer.  
;;; 
;;; The return value from the request function is ignored.
;;; 
;;; Stylistically, this function should schedule events for buffer changes or other actions that it does as a response to the request, particularly if those events are to occur at a future time.
;;; 
;;; buffer-mod
;;; 
;;; The buffer-mod function is used to respond to requests made to the module to modify the chunk in any of the module's buffers.  The buffer-mod function will be passed three parameters.  The first will be the instance of the module for the model in which the buffer-mod request was made.  The second will be the name of the buffer to which the request was made, and the third will be a list of chunk modifications indicating how to modify the chunk in the buffer.  
;;; 
;;; The return value from the buffer-mod function is ignored.
;;; 
;;; Stylistically, this function should schedule the buffer modification or any other actions that it does as a response to the request, particularly if those events are to occur at a future time.
;;; 
;;; init-params 
;;; 
;;; The init-params function is used to specify the parameter values that are maintained and used by the module. It will be called after the reset function of the module is called. It should take one parameter, which will be the instance of the module in the model in which the request is being made.  
;;; 
;;; The init-params function should return a list of parameters which have been generated by calling define-parameter.  The parameters of that list do not need to be unique i.e. every instance of the module could return the same list of parameters that was built once when the module code was loaded.  
;;; 
;;; The parameters specified will then be made available to the user through the sgp command and the params function of the module (described next) will be used to control them. 
;;; 
;;; params 
;;; 
;;; The params function is used to control the parameters of a module. The params function is called in two situations both in response to the sgp command.  It will be either a request for the current value of a parameter of the module or to set a new value for a parameter of the module.  
;;; 
;;; The params function will be called with two parameters. The first will be the instance of the module in the model in which the request is being made.  The second parameter will be either the name of a parameter for the module or a cons. If it is the name of a parameter, then it is a request for the current value of that parameter, and the params function should return that value.
;;; 
;;; In the case where the second parameter is a cons, the car is a parameter name and the cdr is a value.  This is either a request to set the value of the named parameter if it is owned by the module, or a notification that a non-owned parameter has been changed.  If it is a parameter that the module owns then the value is what was passed to sgp and it has already passed the valid test if one was provided when creating the parameter.  The function should handle the request to change the parameter however the module needs that to be done, and then return the current value of that parameter.  Note that how a module "sets" parameters is entirely up to the module implementer, and there is nothing that requires it to return the same value as the one requested for the setting.
;;; 
;;; If the parameter is one which the module does not own, then the value is the one that was returned from the owning module which may or may not be the same as the value which was passed to sgp.  For a non-owned parameter the return value of the params function is ignored.
;;; 
;;; delete  
;;; 
;;; The delete function will be called once when a model with an instance of the module is deleted. The delete function will be passed one parameter which will be the instance of the module in the model which is being deleted.  
;;; 
;;; The return value from the delete function is ignored.
;;; 
;;; notify-on-clear
;;; 
;;; The notify-on-clear function will be called when any buffer in the model is cleared, regardless of which module defined that buffer.  The notify-on-clear function will be passed three parameters.  The first will be the instance of the module in that model.  The second will be the name of the buffer which is being cleared and the third will be the name of the chunk that is being cleared from that buffer.  
;;; 
;;; The return value of this function is ignored.
;;; 
;;; The reason for such a test is to enable the functioning of the declarative memory module to have chunks enter declarative memory from the buffers without having to "build that in" and could allow for the creation of alternate declarative memory systems that would not require modifying the internals of the framework.  I don't foresee any module other than declarative memory using it at this point, but perhaps once it is there maybe other uses will be found.
;;; 
;;; 
;;; get-module 
;;; 
;;; (defmacro get-module (module-name))
;;; (defun get-module-fct (module-name))
;;; 
;;; module-name a symbol which is the name of a module
;;; 
;;; If module-name is the name of a module in the current model then the instantiation of that module in the current model is returned.  
;;; 
;;; If module-name does not name a module in the current model or there is no current model then a warning is printed and nil is returned.
;;; 
;;; This exists so that if a module provides functions that are called other than through the buffer it can get the correct instantiation of itself to use.  It is not really for general purpose use because the instantiations of a module are really only meaningful within the code of the module.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Redefinition requires explicit removal of the existing module first.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(declaim (ftype (function (t) t) buffers-module-name))
(declaim (ftype (function (t) t) remove-modules-parameters))
(declaim (ftype (function (t) t) uninstall-buffers))
(declaim (ftype (function (t) t) buffer-exists))
(declaim (ftype (function (t t) t) install-parameters))
(declaim (ftype (function (t t) t) install-buffers))
(declaim (ftype (function (t) t) parse-parameters))
(declaim (ftype (function (t) t) parse-buffers))
(declaim (ftype (function (&optional t) t) buffers))
(declaim (ftype (function (t) t) chunk-spec-to-id))
(declaim (ftype (function (t &key (:owner t) (:valid-test t) (:default-value t) (:warning t) (:documentation t)) t) define-parameter))
         
;;; The top level tabel that holds all the modules and details.
;;; Holds all the modules that have been defined.

(defvar *modules-lookup* (make-act-r-modules)
  "The table of all defined modules")

;;; For safety reasons need to know which modules are defined
;;; remotely so that they can be verified at clear-all time.

(defvar *remote-act-r-modules* nil)

;;; Some macros to hide the global from direct use in other files


(defun max-module-name-length ()
  "Length of the longest module's name"
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (act-r-modules-name-len *modules-lookup*)))

(defun global-modules-table ()
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (act-r-modules-table *modules-lookup*)))

(defun all-module-names ()
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (act-r-modules-sorted-names *modules-lookup*)))

(add-act-r-command "all-module-names" 'all-module-names "Returns a list of the names of all currently defined modules. No params.")

(defun notified-modules ()
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (act-r-modules-notify *modules-lookup*)))

(defun run-notify-modules ()
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (act-r-modules-run-notify *modules-lookup*)))

(defun run-over-notify-modules ()
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (act-r-modules-run-over-notify *modules-lookup*)))

(defun updating-modules ()
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (act-r-modules-update *modules-lookup*)))


(defmacro define-module (module-name buffer-list params-list
                                     &key (version nil) 
                                     (documentation nil)
                                     (creation nil) (reset nil) (query nil)
                                     (request nil) (buffer-mod nil) (params nil) 
                                     (delete nil) 
                                     (notify-on-clear nil)
                                     (update nil)
                                     (warning nil)
                                     (search nil)
                                     (offset nil)
                                     (run-start nil)
                                     (run-end nil))
  `(define-module-fct ',module-name ',buffer-list ',params-list ,@(when version `(:version ,version))
     ,@(when documentation `(:documentation ,documentation))
     :creation ',creation :reset ',reset :query ',query :request ',request
     :buffer-mod ',buffer-mod :params ',params  
     :delete ',delete :notify-on-clear ',notify-on-clear :update ',update :warning ',warning
     :search ',search :offset ',offset :run-start ',run-start :run-end ',run-end))
  
  
(defun define-module-fct (module-name buffer-list params-list
                                      &key (version "" version?)
                                      (documentation "" docs?)
                                      creation reset query
                                      request buffer-mod params 
                                      delete  notify-on-clear update warning
                                      search offset run-start run-end)  
  (unless (and version? docs?)
    (print-warning "Modules should always provide a version and documentation string."))
  
  (cond ((mp-models)
         (print-warning "Cannot create a new module when there are models defined."))       
        
        ((null module-name)
         (print-warning "Nil is not a valid module-name. No module defined."))
        ((not (symbolp module-name))
         (print-warning "~s is not a valid module name.~%No module defined." module-name))
        ((valid-module-name module-name)
         (print-warning "Module ~S already exists and cannot be redefined.  Delete it with undefine-module first if you want to redefine it." module-name))
        ((not (listp buffer-list))
         (print-warning "Buffer-list must be a list of buffer names or buffer description lists, but given ~s. Module ~s not defined." buffer-list module-name))
        ((and buffer-list (null query))
         (print-warning "A module with a buffer must support queries.~%Module ~s not defined." module-name))
        ((and buffer-list (some #'(lambda (x) 
                                    (buffer-exists (cond ((listp x) (car x))
                                                         (t x)))) 
                                buffer-list))
         (print-warning "A buffer name requested when defining module ~s is already used by another module." module-name)
         (print-warning "Existing buffer names are: ~s" (buffers))
         (print-warning "Module attempted to create buffers: ~s" buffer-list)
         (print-warning "Module ~s not defined" module-name))
        
        ((not (or (and (not (listp reset)) (local-or-remote-function-or-nil reset))
                  (and (listp reset)
                       (<= (length reset) 3)
                       (every 'local-or-remote-function-or-nil reset))))
         (print-warning
          "Reset parameter is not a function, functon name, nil or a list of one, two, or three such items."))
        
        ((not (and (local-or-remote-function-or-nil creation)  (local-or-remote-function-or-nil query)
                   (local-or-remote-function-or-nil request) (local-or-remote-function-or-nil buffer-mod) (local-or-remote-function-or-nil params) 
                   (local-or-remote-function-or-nil delete) (local-or-remote-function-or-nil search) (local-or-remote-function-or-nil offset) 
                   (local-or-remote-function-or-nil notify-on-clear) (local-or-remote-function-or-nil update) (local-or-remote-function-or-nil run-start)
                   (local-or-remote-function-or-nil run-end)))
         (print-warning "Invalid parameter for a module call-back function")
         (do ((items (list creation query request buffer-mod params delete notify-on-clear update warning search offset run-start run-end)
                     (cdr items))
              (names '(creation query request buffer-mod params delete notify-on-clear update warning search offset run-start run-end)
                     (cdr names)))
             ((null items))
           (unless (local-or-remote-function-or-nil (car items))
             (print-warning "Parameter: ~s is not a function, function name, or nil" (car names))))
         (print-warning "Module ~s not defined" module-name))
        ((notevery 'act-r-parameter-p params-list)
         (print-warning "Invalid params-list ~s.~%Module ~s not defined." params-list module-name))
        ((and params-list (null params))
         (print-warning "Must specify a param function because parameters are used.~%Module ~s not defined." module-name))
        (t
         (let ((buffers (parse-buffers buffer-list))
               (parameters (parse-parameters params-list)))
           (cond ((eq :error buffers)
                  (print-warning "Error in module buffer definitions.~%Module ~s not defined." module-name))
                 ((eq :error parameters)
                  (print-warning "Error in module parameter definitions.~%Module ~s not defined." module-name))
                 (t
                  (let ((new-mod (make-act-r-module :name module-name
                                                    :buffers buffers
                                                    :version version
                                                    :documentation documentation
                                                    :creation creation
                                                    :reset (if (listp reset)
                                                               (first reset)
                                                             reset)
                                                    :secondary-reset (if (listp reset)
                                                                         (second reset)
                                                                       nil)
                                                    :tertiary-reset (if (listp reset)
                                                                        (third reset)
                                                                      nil)
                                                    :query query
                                                    :request request
                                                    :buffer-mod buffer-mod
                                                    :params params
                                                    :delete delete
                                                    :notify-on-clear notify-on-clear
                                                    :update update
                                                    :warn warning
                                                    :search search
                                                    :offset offset
                                                    :run-notify run-start
                                                    :run-over-notify run-end)))
                     (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
                       (setf (gethash module-name (act-r-modules-table *modules-lookup*)) new-mod)
                       (if (null (act-r-modules-sorted-names *modules-lookup*))
                           (push module-name (act-r-modules-sorted-names *modules-lookup*))
                         (splice-into-position-des (act-r-modules-sorted-names *modules-lookup*) 
                                           (aif (position-if (lambda (x)
                                                               (string< (symbol-name module-name) (symbol-name x)))
                                                             (act-r-modules-sorted-names *modules-lookup*))
                                                it
                                                (length (act-r-modules-sorted-names *modules-lookup*)))
                                           module-name))
                       (incf (act-r-modules-count *modules-lookup*))
                       (when (> (length (format nil "~S" module-name)) (act-r-modules-name-len *modules-lookup*))
                         (setf (act-r-modules-name-len *modules-lookup*) (length (format nil "~S" module-name))))
                       
                       (when notify-on-clear 
                         (push module-name (act-r-modules-notify *modules-lookup*)))
                       
                       (when update 
                         (push module-name (act-r-modules-update *modules-lookup*)))
                       
                       (when run-start 
                         (push module-name (act-r-modules-run-notify *modules-lookup*)))
                       
                       (when run-end 
                         (push module-name (act-r-modules-run-over-notify *modules-lookup*))))
                    
                    (install-buffers module-name buffers)
                    (install-parameters module-name parameters)
                    
                    module-name)))))))
                                               

(defun parse-remote-params (params-list)
  (mapcar (lambda (x)
            (multiple-value-bind (valid params) 
                (process-options-list (second x) "parse-remote-params" '(:owner :valid-test :default-value :warning :documentation))
              (when valid
                (apply 'define-parameter (string->name (first x)) (convert-options-list-items params '(:default-value :valid-test) nil)))))
    params-list))
    
(defun parse-remote-buffers (buffer-list)
  (if (listp buffer-list)
      (mapcar (lambda (x)
                (if (atom x)
                    (string->name x)
                  (cond ((< (length x) 5)
                         (string->name-recursive x))
                        ((= (length x) 5)
                         (append (string->name-recursive (subseq x 0 4)) (list (fifth x))))
                        (t
                         (append (string->name-recursive (subseq x 0 4)) (list (fifth x)) (string->name-recursive (nthcdr 5 x)))))))
        buffer-list)
    buffer-list))
    
(defun remote-define-module (name buffer-list param-list &optional interface-list)
  
  (multiple-value-bind (valid interface)
      (process-options-list interface-list "define-module" '(:version :documentation :creation :reset :query :request :buffer-mod :params :delete :notify-on-clear :update :warning :search :offset :run-start :run-end))
    (if valid
        (let ((success (apply 'define-module-fct (string->name name) (parse-remote-buffers buffer-list) (parse-remote-params param-list) interface)))
          (when success
            (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
              (let ((m (gethash success (act-r-modules-table *modules-lookup*))))
                (setf (act-r-module-external m) t))
              (push success *remote-act-r-modules*)))
          success)  
      nil)))

(add-act-r-command "define-module" 'remote-define-module "Create a new module. Params: name (buffer-spec*) (param-spec*) {< version, documentation, creation , reset, query, request, buffer-mod, params, delete, notify-on-clear, update, warning, search, offset, run-start, run-end >}")


(defun verify-remote-modules ()
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (dolist (module *remote-act-r-modules*)
      (let ((m (gethash module (act-r-modules-table *modules-lookup*))))
        (when (some (lambda (fn)
                      (and (stringp fn) (not (check-act-r-command fn))))
                    (list (act-r-module-creation m)
                          (act-r-module-reset m)
                          (act-r-module-secondary-reset m)
                          (act-r-module-tertiary-reset m)
                          (act-r-module-query m)
                          (act-r-module-request m)
                          (act-r-module-buffer-mod m)
                          (act-r-module-params m)
                          (act-r-module-delete m)
                          (act-r-module-notify-on-clear m)
                          (act-r-module-update m)
                          (act-r-module-warn m)
                          (act-r-module-search m)
                          (act-r-module-offset m)
                          (act-r-module-run-notify m)
                          (act-r-module-run-over-notify m)))
          (undefine-module-fct module)
          (print-warning "Module ~s undefined because remote commands no longer available." module)
          (setf *remote-act-r-modules* (remove module *remote-act-r-modules*)))))))
                          
                    

;;; Since a module can't be redefined interactive creation of new 
;;; modules requires a way to get rid of one on the fly

(defmacro undefine-module (module-name)
  `(undefine-module-fct ',module-name))

(defun undefine-module-fct (module-name)
  (cond ((mp-models)
         (print-warning "Cannot delete a module when there are models defined."))
        ((not (valid-module-name module-name))
         (print-warning "~S is not the name of a currently defined module." module-name))
        (t
         
         (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
           (uninstall-buffers (act-r-module-buffers 
                               (gethash module-name (act-r-modules-table *modules-lookup*))))
           
           (remove-modules-parameters module-name)
           
           ;; take it out of the table
           
           (remhash module-name (act-r-modules-table *modules-lookup*))
           
           (setf (act-r-modules-sorted-names *modules-lookup*) (remove module-name (act-r-modules-sorted-names *modules-lookup*)))
           
           (decf (act-r-modules-count *modules-lookup*))
         
           ;;; if it was the longest name remeasure the rest
         
           (when (= (length (format nil "~S" module-name)) 
                    (act-r-modules-name-len *modules-lookup*))
           
             (setf (act-r-modules-name-len *modules-lookup*)
               (apply #'max (mapcar #'(lambda (x)
                                        (length (format nil "~S" x)))
                              (all-module-names)))))
         
           (setf (act-r-modules-notify *modules-lookup*) 
             (remove module-name (act-r-modules-notify *modules-lookup*)))
           
           (setf (act-r-modules-run-notify *modules-lookup*) 
             (remove module-name (act-r-modules-run-notify *modules-lookup*)))
           
           (setf (act-r-modules-run-over-notify *modules-lookup*) 
             (remove module-name (act-r-modules-run-over-notify *modules-lookup*)))
           
           (setf (act-r-modules-update *modules-lookup*) 
             (remove module-name (act-r-modules-update *modules-lookup*))))
         t)))


(defun remote-undefine-module (name)
  (undefine-module-fct (string->name name)))

(add-act-r-command "undefine-module" 'remote-undefine-module "Remove the named module from the system.  Params: module-name." t)
    
(defmacro get-module (module-name)
  `(get-module-fct ',module-name))

(defun get-module-fct (module-name)
  (let (instance)
    (values (verify-current-model
             "get-module called with no current model."
             (multiple-value-bind (mod present)
                 (gethash module-name (let ((model (current-model-struct))) (bt:with-lock-held ((act-r-model-modules-lock model)) (act-r-model-modules-table model))))
               (if present
                   (progn (setf instance (cdr mod)) (car mod))
                 (print-warning "~s is not the name of a module in the current model." module-name))))
            instance)))


(defun external-get-module (module-name)
  (get-module-fct (string->name module-name)))

(add-act-r-command "get-module" 'external-get-module "Return the instance of the named module in the current model. Params: module-name")

(defun get-abstract-module (module-name)
  (bt:with-recursive-lock-held ((act-r-modules-lock *modules-lookup*))
    (gethash module-name (act-r-modules-table *modules-lookup*))))

(defun valid-module-name (name)
  (if (get-abstract-module name)
      t
    nil))


(defun process-parameters (module-name param)
  (multiple-value-bind (instance module) (get-module-fct module-name)
    (if module
        (awhen (act-r-module-params module) ;; should be guranteed
               (if (act-r-module-external module)
                   (if (consp param)
                       (dispatch-apply it instance (list (car param) (cdr param)))
                     (dispatch-apply it instance (list param)))
                   (funcall it instance param)))
      (print-warning "There is no module named ~S. Cannot process parameters for it." module-name))))


(defun instantiate-module (module-name model-name)
  (let ((module (get-abstract-module module-name)))
    (if module
        (let ((instance (when (act-r-module-creation module)
                          (dispatch-apply (act-r-module-creation module) model-name))))
          
          ;; store the abstract in the table with the instance!
          (cons instance module))
      (print-warning "There is no module named ~S. Cannot instantiate it." module-name))))

(defun reset-module (module-alist)
  (let ((module (cdr module-alist))
        (instance (car module-alist)))
    (awhen (act-r-module-reset module)
           (dispatch-apply it instance))))
  
(defun secondary-reset-module (module-alist)
  (let ((module (cdr module-alist))
        (instance (car module-alist)))
    (awhen (act-r-module-secondary-reset module)
           (dispatch-apply it instance))))

(defun tertiary-reset-module (module-alist)
  (let ((module (cdr module-alist))
        (instance (car module-alist)))
    (awhen (act-r-module-tertiary-reset module)
           (dispatch-apply it instance))))

(defun query-module (module-alist buffer-name query value)
  (let ((module (cdr module-alist))
        (instance (car module-alist)))
    (aif (act-r-module-query module)
         (cond ((eq query 'error)
                (if value
                    (dispatch-apply it instance buffer-name 'state 'error)
                  (not (dispatch-apply it instance buffer-name 'state 'error))))
               (t
                (dispatch-apply it instance buffer-name query value)))
         (print-warning "Module ~s does not support queries." (act-r-module-name module)))))


(defun warn-module? (module-alist)
  (if (act-r-module-warn (cdr module-alist)) t nil))

(defun warn-module (module-alist buffer-name chunk-spec)
  (let ((module (cdr module-alist))
        (instance (car module-alist)))
    (aif (act-r-module-warn module)
         (if (act-r-module-external module)
             (dispatch-apply it instance buffer-name (chunk-spec-to-id chunk-spec))
           (funcall it instance buffer-name chunk-spec))
         (print-warning "Module ~s does not require warnings." (act-r-module-name module)))))

(defun request-module (module-alist buffer-name chunk-spec)
  (let ((module (cdr module-alist))
        (instance (car module-alist)))
    (aif (act-r-module-request module)
         (progn
           (if (act-r-module-external module)
               (dispatch-apply it instance buffer-name (chunk-spec-to-id chunk-spec))
             (funcall it instance buffer-name chunk-spec))
           t)
         (print-warning "Module ~s does not handle requests." (act-r-module-name module)))))

(defun buffer-mod-module (module-alist buffer-name chunk-mods)
  (let ((module (cdr module-alist))
        (instance (car module-alist)))
    (aif (act-r-module-buffer-mod module)
         (progn
           (if (act-r-module-external module)
               (dispatch-apply it instance buffer-name (chunk-spec-to-id chunk-mods))
             (funcall it instance buffer-name chunk-mods))
           t)
         (print-warning "Module ~s does not support buffer modification requests." (act-r-module-name module)))))

(defun delete-module (module-name)
  (multiple-value-bind (instance module)
      (get-module-fct module-name)
    (if module
        (awhen (act-r-module-delete module)
               (dispatch-apply it instance))
      (print-warning "There is no module named ~S in the current model. Cannot delete an instance of it." module-name))))
  
(defun notify-module (module-name buffer-name chunk-name)
  (multiple-value-bind (instance module)
      (get-module-fct module-name)
    (if module
        ;; this only gets called if there is such a function
        ;; so no need to double check it
        (dispatch-apply (act-r-module-notify-on-clear module) instance buffer-name chunk-name)
      (print-warning "There is no module named ~S in the current model. Cannot notify it of a buffer's clearing." module-name))))

(defun update-the-module (module-name old-time new-time)
  (multiple-value-bind (instance module)
      (get-module-fct module-name)
    (if module
        ;; this only gets called if there is such a function
        ;; so no need to double check it
        (dispatch-apply (act-r-module-update module) instance old-time new-time)
      (print-warning "There is no module named ~S in the current model. Cannot update it." module-name))))

(defun run-notify-module (module-name)
  (multiple-value-bind (instance module)
      (get-module-fct module-name)
    (if module
        ;; this only gets called if there is such a function
        ;; so no need to double check it
        (dispatch-apply (act-r-module-run-notify module) instance)
      (print-warning "There is no module named ~S in the current model. Cannot notify it of a run start." module-name))))

(defun run-over-notify-module (module-name)
  (multiple-value-bind (instance module)
      (get-module-fct module-name)
    (if module
        ;; this only gets called if there is such a function
        ;; so no need to double check it
        (dispatch-apply (act-r-module-run-over-notify module) instance)
      (print-warning "There is no module named ~S in the current model. Cannot notify it of a run ending." module-name))))

(defun m-buffer-search (buffer-name)
  (aif (buffers-module-name buffer-name)
       (module-m-buffer-search it buffer-name)
       (print-warning "m-buffer-search cannot search ~s because it does not name a valid buffer.")))

(defun module-m-buffer-search (module-name buffer-name)
  (multiple-value-bind (instance module)
      (get-module-fct module-name)
    (if module
        (aif (act-r-module-search module)
             (values t (dispatch-apply it instance buffer-name))
             (values nil nil))
      (print-warning "There is no module named ~S in the current model. Cannot perform buffer search." module-name))))

(defun m-buffer-offset (buffer-name c-list)
  (aif (buffers-module-name buffer-name)
       (module-m-buffer-offset it buffer-name c-list)
       (print-warning "m-buffer-offset cannot get values for ~s because it does not name a valid buffer." buffer-name)))

(defun module-m-buffer-offset (module-name buffer-name c-list)
  (multiple-value-bind (instance module)
      (get-module-fct module-name)
    (if module
        (aif (act-r-module-offset module)
             (values t (dispatch-apply it instance buffer-name c-list))
             (values nil nil))
      (print-warning "There is no module named ~S in the current model. Cannot get buffer offsets." module-name))))


;;; Add a way to test against a module's version number assuming that it's set as a sequence of numbers 
;;; separated by periods.  Useful for things in extras or user added extensions since those won't 
;;; correspond to an ACT-R version change.

(defun valid-module-version-string (val)
  (and (stringp val)
       (every (lambda (x) (find x '(#\. #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))) val)))

(defun module-version-numbers (s)
  (read-from-string (format nil "(~a)" (substitute #\space #\. s))))

(defun check-module-version (module version &optional warn-if-newer)
  (let ((m (get-abstract-module module)))
    (if m
        (if (valid-module-version-string (act-r-module-version m))
            (if (valid-module-version-string version)
                (let ((check-version (module-version-numbers version))
                      (module-version (module-version-numbers (act-r-module-version m))))
                  (mapcar (lambda (m-v t-v)
                            (if (< m-v t-v)
                                (return-from check-module-version
                                  (print-warning "Module ~s with version ~s does not meet the version test for ~s"
                                                module (act-r-module-version m) version))
                              (when (> m-v t-v)
                                (if warn-if-newer 
                                    (return-from check-module-version
                                      (print-warning "Module ~s with version ~s is newer than the version test for ~s"
                                                     module (act-r-module-version m) version))
                                  (return-from check-module-version t)))))
                    module-version check-version)
                  (if (> (length check-version) (length module-version))
                      (print-warning "Module ~s with version ~s does not meet the version test for ~s"
                                    module (act-r-module-version m) version)
                    t))
              (print-warning "Version number ~s given to check module ~s is not a string of numbers" version module))
          (print-warning "Cannot check version because module ~s has version ~s which is not a string of numbers" module (act-r-module-version m)))
      (print-warning "Cannot check version because no module with name ~s" module))))


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
