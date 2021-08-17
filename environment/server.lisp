;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : server.lisp
;;; Version     : 3.0
;;; 
;;; Description : Contains no system dependent code because it relies on the
;;;             : definitions in uni-files.
;;;             : Holds the Environment global variables and the
;;;             : code to open and manage an environment socket connection. 
;;; Bugs        : 
;;; 
;;; Todo        : [ ] Currently stopping the environment doesn't cause the
;;;             :     Tk side to cleanly destroy all windows and remove the
;;;             :     handlers.  That could be an issue for something that
;;;             :     needs to do some cleanup (like the history tools table).
;;;             :     For now, special casing that table in the closing,
;;;             :     but should look into having the close message be more
;;;             :     thorough (some quick fix attempts ran into ugly issues
;;;             :     with timing/waiting issues).
;;; 
;;; ----- History -----
;;;
;;; 05/10/2002  Dan
;;;             : Added this header
;;; 05/20/2002  Dan
;;;             : Think I've finally got it working on Macs w/ OS < 10 and MCL
;;;             : and for now w/OSX it's got to be OpenMcl which also seems
;;;             : to work (though it's not in here yet...)
;;; 05/21/2002  Dan
;;;             : Serious reorganization took place with the files...
;;; 05/22/2002  Dan
;;;             : Fixed some more MCL problems.  At this point it's working
;;;             : mostly, so I'm not wasting any more time 'fixing' it there
;;;             : since MacOS < 10 is "dead"...
;;;             : Changed close-connection so that killing the process was
;;;             : optional and altered the message-process and process-
;;;             : connection functions so that they don't kill their own
;;;             : process (doesn't seem to change things, but seemed like a
;;;             : good idea and I thought it'd fix a closeing bug I had
;;;             : in MCL, but it didn't).
;;; 05/23/2002  Dan
;;;             : Made a very subtle change, that may have a big impact
;;;             : later on.  When a handler is created it preforms an initial
;;;             : update - that's as it was, but now that first time the
;;;             : handler itself is passed as the parameter to the updater.
;;;             : Thus if an updater needs to use its parameter it had better
;;;             : check its type because that first time it'll be something
;;;             : other than it normally expects.
;;; 08/23/2002  Dan
;;;             : Updated the version number to 6.0b3, which doesn't 
;;;             : correspond to the versions in the headers, so I
;;;             : need to fix that at some point for consistency...
;;; 09/17/2002  Dan
;;;             : Updated the environment and ACT-R version numbers...
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Added the conditionalized in-package to message-process
;;;             : to make building a standalone easier.
;;;             : Changed the environment version to correspond to the
;;;             : file version numbers and add a -S if it's the standalone.
;;; 01/20/2003  Dan
;;;             : Updated version number again for the env because
;;;             : of a quick change for the class.
;;; 07/07/2003  Dan
;;;             : Updated version because I rolled in most of Mike's newest
;;;             : RPM (my UWI and ACL device files were newer).
;;; 08/15/2003  Dan
;;;             : Updated version to 1.3 because I've added the support for
;;;             : CMUCL from Ethan Glasser-Camp at RPI.
;;; 12/19/2003  Dan
;;;             : Updated version to 1.4 for this file because I've fixed
;;;             : a bug in the uni-files and moved the ACT-R version
;;;             : variable to the actr5.lisp file itself.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info, updated the version to 1.5
;;;             : and removed the ACT-R version variable since that's
;;;             : in the main ACT-R file now.
;;; -----------------------------------------------------------------------
;;; 2005.04.11  Dan [2.0]
;;;             : Start of the move to ACT-R 6.0.
;;; 2007.08.03  Dan
;;;             : * Moved the *environment-sockets* defvar to env-module
;;;             :   to avoid a compiler warning.
;;; 2007.08.13  Dan
;;;             : * Adding reset as a possible update condtion for handlers.
;;; 2007.08.17 Dan
;;;             : * Added a without-interrupts call to close-connection to
;;;             :   fix a problem in stopping things under LispWorks.
;;; 2008.01.15 Dan
;;;             : * Changed close-connection because SBCL doesn't have
;;;             :   without-interrupts in the default package...
;;; 2008.04.08 Dan
;;;             : * Refixed that last update using uni-without-interrupts.
;;;             :   Also added the require-compiled of uni-files just to be
;;;             :   safe.
;;; 2008.05.16 Dan
;;;             : * Added the conflict-nil option for when to update the
;;;             :   handler.  That's the same as conflict except that it 
;;;             :   doesn't pass the return value back to the conflict-set-
;;;             :   hook.
;;; 2009.01.08 Dan
;;;             : * Fixed an issue with message-process which could cause
;;;             :   problems with some Lisps (CCLx86 in particular).
;;; 2009.04.13  Dan
;;;             : * Uni-send-string doesn't have an automatic newline now,
;;;             :   so need to put one in the string to be sent.
;;; 2009.04.14  Dan
;;;             : * Seems that adding the newline here breaks the stopping
;;;             :   of the connection.  However, since it doesn't seem to be 
;;;             :   necessary I'm just dropping it again.
;;; 2009.06.08  Dan
;;;             : * Adding the run-environment command which spawns the
;;;             :   environment app and then makes the connection.
;;;             :   Only available for LispWorks under MacOSX or Windows and
;;;             :   ACL under MacOSX or Windows at this time.
;;; 2009.06.09 Dan
;;;             : * Upped the default delay on run-environment to 12 seconds
;;;             :   to be safer.
;;; 2009.07.17 Dan
;;;             : * Added a test to the remove handler code so that it 
;;;             :   shouldn't throw an error on initial connection anymore
;;;             :   like it would occasionally for some Lisps.
;;; 2011.01.07 Dan
;;;             : * Lock all the background processes from the environment
;;;             :   for now until I come up with a better way to protect the
;;;             :   ACT-R code.
;;; 2011.01.14 Dan
;;;             : * Fixed the name of the application to run for the Mac
;;;             :   environment in the ACL run-environment command.
;;; 2011.01.14 Dan
;;;             : * Added a return reply for the keep alive message to 
;;;             :   possibly avoid some time out issues in Windows 7.
;;; 2011.02.21 Dan 
;;;             : * Added the ability to specify the new run-start and run-end hooks.
;;; -------------------------------------------------------------------------
;;; 2011.05.20 Dan [3.0]
;;;             : * Start of a complete overhaul to eliminate most of the 
;;;             :   global variable usage and better encapsulate things so
;;;             :   that multiple model support can be added.
;;; 2011.05.25 Dan 
;;;             : * Updated create message parsing to set the use-model 
;;;             :   slot.  The semantics now are that if there are only 6
;;;             :   items in the create no model is used, if there are 7 then
;;;             :   the 7th is either a model name to use when evaluating the
;;;             :   update or nil which means use the current-model explicitly
;;;             :   (which is not the same as not using a model).
;;;             : * Added the which-hook slot to the control structure and
;;;             :   set that in the hook updating functions.
;;; 2011.05.26 Dan
;;;             : * Fixed a cut-and-paste error in delete-post-hook-if-necessary.
;;; 2011.06.01 Dan
;;;             : * Changed the remove message handling to only evaluate it in
;;;             :   a model if that's a valid model now.
;;; 2011.09.02 Dan
;;;             : * Added a background option to connect-to-environment so
;;;             :   that it's possible to have the environment message handling
;;;             :   thread be the main one (for use with standalone versions that
;;;             :   have a slave Lisp running headless).
;;; 2011.09.07 Dan
;;;             : * Fixed some format strings that were missing '~'s.
;;; 2011.09.13 Dan
;;;             : * Readjust how the default run-environment gets defined to
;;;             :   avoid a warning for multiple definitions.
;;; 2011.11.07 Dan
;;;             : * Added a check for LW 6 to the windows version of run-
;;;             :   environment so it'll work there too.
;;; 2011.12.19 Dan
;;;             : * Added some checks to start-environment so it better warns
;;;             :   when the Lisp isn't able to use the Environment.
;;; 2012.02.01 Dan
;;;             : * Added some more declaims to avoid undefined function warnings
;;;             :   at load time.
;;; 2012.02.07 Dan
;;;             : * Fixed the declaim for update-handler.
;;; 2012.03.21 Dan
;;;             : * Fixed a bug with stopping the environment while the stepper
;;;             :   was open that prevented one from using the stepper if a
;;;             :   new environment connection was made.
;;; 2012.09.07 Dan
;;;             : * Removed the in-package from message-process based on the
;;;             :   :actr-env-alone feature.
;;; 2012.09.21 Dan
;;;             : * Trying to eliminate an intermittent environment error
;;;             :   with multiple models in CCL (perhaps elsewhere too just
;;;             :   unreported).  So adding additional lock-outs so that 
;;;             :   updates can't occur while the model is running by locking
;;;             :   during the pre-event-hook and unlocking during the post as
;;;             :   well as during anything else which calls update handlers.
;;;             : * The problem with that is if someone breaks the system
;;;             :   between pre and post it leaves the lock set and basically
;;;             :   kills the environment -- need a better alternative.
;;; 2013.02.22 Dan
;;;             : * Added the wait-for-environment command to provide a way
;;;             :   to check that it has processed all outstanding actions.
;;;             :   Primary intended use is with creating complex visible
;;;             :   virtual displays, especially those that update faster than 
;;;             :   real time, because if the "drawing" falls behind things
;;;             :   can be difficult to use or display errors could result.
;;; 2013.02.25 Dan
;;;             : * Adding run-environment commands for CCL in Win, Mac, and
;;;             :   linux.
;;;             : * Changed how the delay parameter works for run-environment.
;;;             :   It still waits that long before the first try to connect,
;;;             :   but it will now continue to try to connect after every 
;;;             :   delay seconds pass.
;;; 2013.02.26 Dan
;;;             : * Fixed a bug with the CCL Mac run-environment.
;;; 2013.12.18 Dan
;;;             : * Changed close-connection so that there's a parameter which
;;;             :   indcates whether or not to send the close message.
;;;             : * Changed the reception of a goodbye message so that it 
;;;             :   completely closes down the connection by killing the 
;;;             :   process as well.
;;; 2014.09.10 Dan
;;;             : * Changed the flag on run-environment for LispWorks to
;;;             :   :lispworks6 instead of 6.0 so that 6.1 also works (and
;;;             :   presumably any other 6.x they create).
;;; 2014.09.30 Dan
;;;             : * Changed safe-evaluation from a defmethod to defun since
;;;             :   it wasn't specifying types in the params list anyway.
;;; 2015.02.20 Dan
;;;             : * Connect-to-environment now passes the optional parameter
;;;             :   for capturing the current stream as t to uni-run-process
;;;             :   to avoid issues with the "AltConsole" in CCL.
;;; 2015.07.28 Dan
;;;             : * Changed the ACT-R6 logicals to ACT-R in the environment
;;;             :   directory pathing.
;;;             : * Changed ACT-R6;support to ACT-R-support in require compiled.
;;; 2015.08.03 Dan
;;;             : * Fix a bug with run-environment in CCL because with versions
;;;             :   1.9 or newer it doesn't properly create the command line to
;;;             :   evaluate when there are spaces in the path.
;;; 2015.08.10 Dan
;;;             : * Adjust the feature test for run-environment for Lispworks
;;;             :   because v7 is available and assuming it works the same as
;;;             :   the previous 2 so just removing the version check since
;;;             :   I doubt that anyone is using v4 at this point.
;;; 2016.01.14 Dan
;;;             : * Added a run-environment for ACL+Linux.
;;; 2016.04.14 Dan
;;;             : * Added a dummy-env-handler function which does nothing so
;;;             :   that I don't have to put the empty lambda in all the 
;;;             :   environment tools...
;;; 2016.06.02 Dan
;;;             : * Added the Todo about fixing the close problem.
;;;             : * Removed the old Todo because it seems like it's a non-
;;;             :   issue now.
;;;             : * The connection closing now clears the table of history
;;;             :   data in *history-recorder-data-cache* when all connections
;;;             :   are gone (fixing the close issue would eliminate the need
;;;             :   for this).
;;; 2016.06.09 Dan
;;;             : * To avoid an undefined variable warning using a function
;;;             :   to clear the history data cache which can be declaimed
;;;             :   and defined in the history code.
;;; 2017.08.08 Dan
;;;             : * Uni-without-interrupts isn't needed since uni-send-string
;;;             :   already has a lock around it...
;;; 2017.08.28 Dan
;;;             : * Removed everything except the run-environment functions
;;;             :   since that's all that's needed.
;;; 2017.10.13 Dan
;;;             : * Updated the linux version in anticipation of a real
;;;             :   application being there instead of assuming wish is 
;;;             :   available.
;;; 2018.03.30 Dan
;;;             : * Run-environment for Mac and Linux CCL restores the 
;;;             :   current directory -- why didn't it before?
;;; 2019.11.01 Dan
;;;             : * Change run-environment for the Macs to run the application
;;;             :   directly because it's not in an app bundle anymore.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;;; Adding a hack for a problem with how run-program creates the string that it
;;; sends to Windows to execute because it doesn't properly escape spaces as of
;;; CCL 1.9.
;;; The fix was found here: http://comments.gmane.org/gmane.lisp.openmcl.devel/9030

#+(and :ccl :windows :ccl-1.9)
(let ((*WARN-IF-REDEFINE-KERNEL* nil))
  (defun ccl::make-windows-command-line (strings)
    (setf strings (mapcar 'prin1-to-string strings))
    (reduce (lambda (s1 s2) (concatenate 'string s1 " " s2))
            (cdr strings) :initial-value (car strings))))


#+(and :ccl :windows)
(defun run-environment ()
  (run-program (namestring (translate-logical-pathname "ACT-R:environment;start environment.exe")) nil :wait nil))

#+(and :ccl :linux)
(defun run-environment ()
  (let ((c (ccl::cd "."))) 
    (ccl::cd "ACT-R:environment")
    (run-program "./start environment Linux" nil :wait nil)
    (ccl::cd c)))

#+(and :ccl :darwin)
(defun run-environment ()
  (let ((c (ccl::cd "."))) 
    (ccl::cd "ACT-R:environment")
    (run-program "./start-environment-osx" nil :wait nil)
    (ccl::cd c)))

#+(and :lispworks (or :win32 :win64))
(defun run-environment ()
  (sys:call-system "\"Start Environment.exe\"" :current-directory (translate-logical-pathname "ACT-R:environment") :wait nil))

#+(and :lispworks :macosx)
(defun run-environment ()
  (sys:call-system (format nil "'~a/Start Environment OSX.app/Contents/MacOS/start-environment-osx'"
                     (namestring (translate-logical-pathname "ACT-R:environment")))
                   :wait nil))


#+(and :allegro :mswindows)
(defun run-environment ()
  (let ((c (current-directory)))
    (chdir "ACT-R:environment")
    (run-shell-command "\"Start Environment.exe\"" :wait nil)
    (chdir c)))

#+(and :allegro :macosx)
(defun run-environment ()
  (let ((c (current-directory)))
    (chdir "ACT-R:environment")
    (run-shell-command "'Start Environment OSX.app/Contents/MacOS/start-environment-osx'" :wait nil)
    (chdir c)))


#+(and :allegro :linux)
(defun run-environment ()
  (let ((c (current-directory)))
    (chdir "ACT-R:environment;GUI")
    (run-shell-command "./starter.tcl" :wait nil)
    (chdir c)))

(unless (fboundp 'run-environment)
  (defun run-environment ()
    (print-warning "The run-environment command is not available for your current Lisp & OS combination.")))

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
