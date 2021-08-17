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
;;; Filename    : framework-loader.lisp
;;; Version     : 2.0
;;; 
;;; Description : Compiles (if necessary) and loads the files that implement
;;;               the framework core.
;;; 
;;; Bugs        : 
;;;
;;; To do       : Possibly need special cases for "standalones" as was done
;;;               with the ACT-R 5 loader.
;;;             : Get rid of smart-load and use the main loader's 
;;;               compile and load instead.
;;; 
;;; ----- History -----
;;;
;;; 2004.09.27 Dan
;;;             : Creation
;;; 2005.01.29 Dan
;;;             : * Removed the setting of *.lisp-pathname* and *.bin-pathname*
;;;             :   since that happens in the top loader (load-act-r-6.lisp).
;;;             : * Changed smart-load to use compile-and-load instead of doing
;;;             :   the same checks.
;;; 2005.08.11 Dan
;;;             : * Took act-gui-interface off of the list because it has been
;;;             :   moved to tools.
;;;             : * Changed version to 1.0.
;;;             : * Removed everything but the file list...
;;; 2007.01.15 Dan
;;;             : * Added version-string as the first file in the list now.
;;; 2010.11.15 Dan
;;;             : * Added system-parameters to the list.
;;; 2011.04.28 Dan
;;;             : * Just setf the list instead of defining it.
;;;             : * Take generic-interface.lisp out since it's always 
;;;             :   overridden by the virtual interface and possibly a Lisp
;;;             :   specific one too.
;;; 2016.11.16 Dan [2.0]
;;;             : * Added the dispatcher for the command interface.
;;; 2017.03.30 Dan 
;;;             : * Added the system-locks file because there's a lock needed
;;;             :   in the internal-macros code.
;;; 2017.03.31 Dan
;;;             : * The internal-macros file is no longer necessary.
;;; 2017.09.19 Dan
;;;             : * Added the history-recorder file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Compiles and loads only the framework pieces but not the general modules
;;; or the specific device interfaces.  
;;; Called by the top level loader so it's not really for general use, but 
;;; may be needed when testing updates.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; Nothing for use here.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Based on the loader from ACT-R/PM and ACT-R 5.  Supports the same Lisps
;;; as was done there.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Define the files to be loaded.

;;; this is always the generic list

(setf *file-list* '("version-string"
                    "internal-structures"
                    "system-locks"
                   ; "internal-macros"
                    "misc-utils"
                    "dispatcher"
                    "system-parameters"
                    "meta-process"
                    "chunk-types"
                    "chunks"
                    "modules"
                    "parameters"
                    "buffers"
                    "model"
                    "events"
                    "scheduling"
                    "chunk-spec"
                    "top-level"
                    "device-interface"
                    "vision-categorization"
                    "random"
                    "printing"
                    "naming-module"
                    "history-recorder"))


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
