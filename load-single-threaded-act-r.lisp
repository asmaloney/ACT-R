;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2019 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : load-single-threaded-act-r.lisp
;;; Version     : 2.0
;;; 
;;; Description : Top level loader for the whole ACT-R system.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [-] Test in a variety of Lisps for issues with the
;;;             :     logical hostname stuff.
;;;             : [ ] Now, look into using the clisp version in other
;;;             :     lisps because it seems cleaner/more generic than
;;;             :     the ones I put toghether...
;;;             : [x] Use compile-file-pathname instead of always adding a new
;;;             :     entry for *.fasl-pathname*.
;;; 
;;; ----- History -----
;;;
;;; 2019.04.19 Dan 
;;;             : * Custom loader that sets the flag to disable locking and
;;;             :   suppress any threaded operation.
;;; 2019.05.29 Dan [2.0]
;;;             : * Just set the switch and then load the main load file so
;;;             :   that I don't need to maintain two versions of basically the
;;;             :   same code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(pushnew :single-threaded-act-r *features*)

(setf (logical-pathname-translations "SINGLE-STUB")
  `(("**;*.*" ,(namestring (merge-pathnames "**/*.*" *load-truename*)))))

(load (translate-logical-pathname "SINGLE-STUB:load-act-r.lisp"))

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
