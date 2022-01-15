# 
# This is the main environment file.
# It contains the procedures and variables for the network
# communication, sources all of the other files, creates the
# control panel window as .control_panel, and opens the listening socket.

# This variable is used to conditionalize code that
# operates differently when the environment is being
# built into a standalone application.

global standalone_mode

# Use 0 for not standalone
# Use 1 for Windows standalone
# Use 2 for Mac standalone
# Use 3 for Android app

set standalone_mode 0

# This variable is an array that will be used to hold the
# table of Lisp handler names indexed by Tcl window name.
# These get entered by the register command coming from Lisp.

global handler_names

# valid_handler_name
# Given a tcl window (w) return true if there is 
# currently an entry in the table of that window's 
# Lisp handler and 0 if there is not.

proc valid_handler_name {w} {
  global handler_names
  return [llength [array names handler_names $w]] 
}

# get_handler_name
# Given a tcl window (w) return the name of the Lisp handler
# associated with that window.  There is no check to see if
# that window has a handler - that must be done prior to calling
# this procedure.

proc get_handler_name {w} {
  global handler_names
  return $handler_names($w)
}

# set_handler_name 
# Given a tcl window (w) and the name of a Lisp handler (h)
# set the handler entry of w to be h in the global lookup table.

proc set_handler_name {w h} {
  global handler_names
  set handler_names($w) $h
}


# This variable and the next procedure are used to implement
# basically a gentemp in Tcl.  The reason I need it is because every 
# dialog that comunicates with Lisp needs a unique name and 
# probably a variable to be set to return values.  This allows
# me to easily create multiple instances of the "standard" 
# environment dialogs.

global variable_count_array


# new_variable_name
# Given any variable name prefix (prefix) return that 
# prefix with the next number appended to it.  Doesn't 
# gurantee uniqueness though because if you use name#
# variables elswhere this could end up generating that
# same name.
 
proc new_variable_name {prefix} {
  global variable_count_array
  if [llength [array names variable_count_array $prefix]] {
    return "$prefix[incr variable_count_array($prefix)]" 
  } else {
    return "$prefix[set variable_count_array($prefix) 1]"
  }
}

# record the Environment's directory
global top_dir
global tcl_env_dir

set tcl_env_dir [pwd]

if {$standalone_mode != 3} {cd ..}

if {$standalone_mode == 0} {cd ..}

if {$standalone_mode == 2} {cd ..}

set top_dir [pwd]

if {$standalone_mode == 2} {cd applications}

if {$standalone_mode == 0} {cd "environment"}

if {$standalone_mode != 3} {cd "gui"}



# Define some general file handling procedures.
# Taken pretty much right from the Tcl/Tk book I've got

proc read_file { filename } {
  set data ""
  if {[file readable $filename]} {
    set fileid [open $filename "r"]
    set data [read $fileid]
    close $fileid
  }

  return $data
}

proc save_text {textwidget filename} {
  set data [$textwidget get 1.0 {end -1c}]
  write_data $data $filename
}

proc write_data {data filename} {
  set fileid [open $filename "w"]
  puts -nonewline $fileid $data
  # I know I shouldn't have to flush before closing, but I've
  # got a weird synchronization problem that seems to be caused
  # by just this type of thing
  flush $fileid
  close $fileid
}

proc append_data {data filename} {
  set fileid [open $filename "a"]
  puts -nonewline $fileid $data
  # I know I shouldn't have to flush before closing, but I've
  # got a weird synchronization problem that seems to be caused
  # by just this type of thing
  flush $fileid
  close $fileid
}


# Start by hiding .

wm withdraw .

# source all the init folder .tcl files.
# They're sourced in sorted order, so prepending a # to the
# beginning of the name helps enforce an order on them.

#cd [file join $tcl_env_dir init]

global init_error_msg

set init_error 0

foreach f [lsort [glob -nocomplain -directory [file join $tcl_env_dir init] *.tcl]] {
  if {[catch {source $f} init_error_msg] } {
    append_data "Error during init of $f: $init_error_msg\n" [file join $tcl_env_dir "error.log"]
    set init_error 1
  }
}


# Create the Control Panel here...

toplevel .control_panel

wm title .control_panel "Control Panel"

wm geometry .control_panel [get_configuration .control_panel]

# for now just have a label to report errors.

global global_status

label .control_panel.status -textvariable global_status -text "" -borderwidth 1 -font intro_s_font 
pack .control_panel.status

# Then it needs a "scrollable frame" for the controls since the window is 
# getting too big for small (laptop) displays.  Because of issues with
# including the libraries in the executables I'm not going to use the BWidgets or
# iwidgets extensions and just use a simple hack which is based on code
# found at http://www.tek-tips.com/viewthread.cfm?qid=372792 by 
# Ken Jones of Avia Training and Consulting, www.avia-training.com


# Create a "hull" frame to contain all other widgets 
# composing the scrollable frame.

frame .control_panel.frame
 
canvas .control_panel.frame.c -height 100 -width 50 -yscrollcommand ".control_panel.frame.ybar set"
scrollbar .control_panel.frame.ybar -orient vertical -command ".control_panel.frame.c yview"
 
# Create the frame that will actually
# hold all children of the scrollable
# frame. All children should be packed
# or gridded into this frame, *not* the
# hull frame or the canvas!

frame .control_panel.frame.c.contents
 
# Tell the canvas to display the frame,
# anchoring the upper-left hand corner
# of the frame in the upper-left hand
# corner of the canvas
 
set .control_panel.window.tag [.control_panel.frame.c create window 0 0 -anchor nw -window .control_panel.frame.c.contents -tag win]
 
# Use grid to display the canvas and its
# scrollbars. Handle resizing properly.
 
grid .control_panel.frame.c -row 0 -column 0 -sticky nsew -pady 4 -padx 2
grid .control_panel.frame.ybar -row 0 -column 1 -sticky ns -pady 4 -padx 2
grid columnconfigure .control_panel.frame 0 -weight 1
grid rowconfigure .control_panel.frame 0 -weight 1
 
# Detect <Configure> events on the frame
# to detect when it is first displayed
# and any time is is resized. In either
# case, recompute the visible bounds of
# the canvas and update its -scrollregion
# attribute.
 
# Not sure why I need to handle both, but need the first one
# to adjust it when the control panel window resizes and the
# second is needed so that the initial display is drawn correctly.
#

bind .control_panel.frame <Configure> {

  .control_panel.frame.c configure -scrollregion [.control_panel.frame.c bbox all]
  .control_panel.frame.c itemconfigure win -width [winfo width .control_panel.frame.c]
}

  
bind .control_panel.frame.c.contents <Configure> {

  .control_panel.frame.c configure -scrollregion [.control_panel.frame.c bbox all]
  .control_panel.frame.c itemconfigure win -width [winfo width .control_panel.frame.c]
}

pack .control_panel.frame -expand yes -fill both

# Now instead of packing the buttons directly on the control panel one has 
# to get the name of the correct parent window with the control_panel_name
# procedure something like this:
# button [control_panel_name].tool ...
# pack [control_panel_name].tool ...

proc control_panel_name {} {return ".control_panel.frame.c.contents"}


proc report_status {s} {
  global global_status
  global tcl_env_dir
  set global_status "ERROR Logged"
  append_data $s [file join $tcl_env_dir "error.log"]
}



# For debugging I need access to the console window, so 
# here's how to get it ->  shift-control-L

bind .control_panel <Shift-Control-Key-L> {console show}

proc clean_exit {} {
    save_window_positions
#    foreach w [winfo children .] { 
#      if {$w != ".control_panel"} { 
#        catch {destroy $w}
#      }
#    }
    destroy .
    set_return_result 1
}

wm protocol .control_panel WM_DELETE_WINDOW {
 
  global current_open_model 
  global standalone_mode
  global environment_socket

  set destroy 1
   
  if {$standalone_mode == 1} { # Windows version needs to kill the Lisp explicitly
    set answer [tk_messageBox -icon warning -title "Exit the Environment?" \
                 -message "Closing this window stops ACT-R. Do you want to quit now?" \
                 -type yesno]

    switch -- $answer {
      no {
        set destroy 0
      }
      yes {
        if {$current_open_model != ""} {
          set answer [tk_messageBox -icon warning -title "Model still open" \
                      -message "There is a model open.  Should the environment \
                       close the model before quitting (all unsaved\
                       changes will be lost if it is not closed)?" \
                      -type yesno]

          switch -- $answer {
            yes {
              close_model
            }
          }
        }
      }
    }
  } elseif {$current_open_model != ""} {
    set answer [tk_messageBox -icon warning -title "Model still open" \
                  -message "There is a model open.  Should the environment \
                            close the model before quitting (all unsaved\
                            changes will be lost if it is not closed)?" \
                  -type yesno]

    switch -- $answer {
      yes {
        close_model
      }
    }
  }

  if $destroy {
    clean_exit
  }
}



# start the lisp side running if necessary

if {$standalone_mode == 1} {
  
   set cur_dir [pwd]

   global top_dir

   cd [file join $top_dir apps]

   if [catch {exec "actr-s-64.exe" -V}] {
     if [catch {exec "actr-s-32.exe" -V}] {
       tk_messageBox -icon warning -title "No ACT-R available" \
                     -message "Cannot run the ACT-R application. Contact Dan for help." -type ok
       clean_exit
     } else {
      exec "./run-32.bat" &
     }
   } else {
      exec "./run-64.bat" &
   }
   cd $cur_dir
}


# Source the new-server file which actually impements the
# client code for connecting to the ACT-R DES server.

source "new-server.tcl"



# Connect to the Lisp server

set not_connected 1

set connect_addr ""

while {$not_connected} {
  if {[catch {start_connection $connect_addr} init_error_msg] } {
    set try_again [tk_messageBox -icon warning -title "Connection Problem" -type yesnocancel -message "Error occurred trying to connect to ACT-R\n$actr_address : $actr_port \nError: $init_error_msg \nYes to try again using the ACT-R address file.\nNo to try again using localhost.\nCancel to quit."]
    
    if {$try_again == "cancel"} {
      set not_connected 0
      tk_messageBox -icon warning -title "Environment terminated" -type ok -message "ACT-R Environment GUI components not started."
      clean_exit
    } else {
      if {$try_again == "no"} {
        set connect_addr [ip:address_local]
        if {$connect_addr == $actr_address} {
          set connect_addr "localhost"
        }
      } else {
        set connect_addr "" 
      }
    } 
  } else {
    set not_connected 0
  } 
}


set up_and_running 0

# source all the dialog folder .tcl files.
# They're sourced in sorted order, so prepending a number to the
# beginning of the name helps enforce an order on them.


foreach f [lsort [glob -nocomplain -directory [file join $tcl_env_dir dialogs] *.tcl]] {
  if { [catch {source $f} init_error_msg] } {
    append_data "Error during dialog loading of $f: $init_error_msg\n" [file join $tcl_env_dir "error.log"]
    set global_status "dialog error"
  }
}

set up_and_running 1

# Below is a modified version of the tk_dilog command which I've
# named my_tk_dialog.  It uses my text and button fonts to build
# the display.  I've included all the original copyrights and 
# license info along with it (instead of including the license
# file separately).
#
# dialog.tcl --
#
# This file defines the procedure tk_dialog, which creates a dialog
# box containing a bitmap, a message, and one or more buttons.
#
# RCS: @(#) $Id: dialog.tcl,v 1.14.2.3 2006/01/25 18:21:41 dgp Exp $
#
# Copyright (c) 1992-1993 The Regents of the University of California.
# Copyright (c) 1994-1997 Sun Microsystems, Inc.
#
# This software is copyrighted by the Regents of the University of
# California, Sun Microsystems, Inc., Scriptics Corporation, ActiveState
# Corporation and other parties.  The following terms apply to all files
# associated with the software unless explicitly disclaimed in
# individual files.
#
# The authors hereby grant permission to use, copy, modify, distribute,
# and license this software and its documentation for any purpose, provided
# that existing copyright notices are retained in all copies and that this
# notice is included verbatim in any distributions. No written agreement,
# license, or royalty fee is required for any of the authorized uses.
# Modifications to this software may be copyrighted by their authors
# and need not follow the licensing terms described here, provided that
# the new terms are clearly indicated on the first page of each file where
# they apply.
# 
# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# 
# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.
# 
# GOVERNMENT USE: If you are acquiring this software on behalf of the
# U.S. government, the Government shall have only "Restricted Rights"
# in the software and related documentation as defined in the Federal 
# Acquisition Regulations (FARs) in Clause 52.227.19 (c) (2).  If you
# are acquiring the software on behalf of the Department of Defense, the
# software shall be classified as "Commercial Computer Software" and the
# Government shall have only "Restricted Rights" as defined in Clause
# 252.227-7013 (c) (1) of DFARs.  Notwithstanding the foregoing, the
# authors grant the U.S. Government and others acting in its behalf
# permission to use and distribute the software in accordance with the
# terms specified in this license. 
#

#
# ::tk_dialog:
#
# This procedure displays a dialog box, waits for a button in the dialog
# to be invoked, then returns the index of the selected button.  If the
# dialog somehow gets destroyed, -1 is returned.
#
# Arguments:
# w -		Window to use for dialog top-level.
# title -	Title to display in dialog's decorative frame.
# text -	Message to display in dialog.
# bitmap -	Bitmap to display in dialog (empty string means none).
# default -	Index of button that is to display the default ring
#		(-1 means none).
# args -	One or more strings to display in buttons across the
#		bottom of the dialog box.

proc ::my_tk_dialog {w title text bitmap default args} {
    global tcl_platform
    variable ::tk::Priv

    # Check that $default was properly given
    if {[string is integer -strict $default]} {
	if {$default >= [llength $args]} {
	    return -code error "default button index greater than number of\
		    buttons specified for tk_dialog"
	}
      # Never call if -strict option is omitted in previous test !
    } elseif {"" eq $default} {
	set default -1
    } else {
	set default [lsearch -exact $args $default]
    }

    # 1. Create the top-level window and divide it into top
    # and bottom parts.

    destroy $w
    toplevel $w -class Dialog
    wm title $w $title
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW { }

    # Dialog boxes should be transient with respect to their parent,
    # so that they will always stay on top of their parent window.  However,
    # some window managers will create the window as withdrawn if the parent
    # window is withdrawn or iconified.  Combined with the grab we put on the
    # window, this can hang the entire application.  Therefore we only make
    # the dialog transient if the parent is viewable.
    #
    if {[winfo viewable [winfo toplevel [winfo parent $w]]] } {
	wm transient $w [winfo toplevel [winfo parent $w]]
    }

    set windowingsystem [tk windowingsystem]

    if {$tcl_platform(platform) eq "macintosh" || $windowingsystem eq "aqua"} {
	::tk::unsupported::MacWindowStyle style $w dBoxProc
    }

    frame $w.bot
    frame $w.top
    if {$windowingsystem eq "x11"} {
	$w.bot configure -relief raised -bd 1
	$w.top configure -relief raised -bd 1
    }
    pack $w.bot -side bottom -fill both
    pack $w.top -side top -fill both -expand 1

    # 2. Fill the top part with bitmap and message (use the option
    # database for -wraplength and -font so that they can be
    # overridden by the caller).

    option add *Dialog.msg.wrapLength 3i widgetDefault
    if {$tcl_platform(platform) eq "macintosh" || $windowingsystem eq "aqua"} {
	option add *Dialog.msg.font system widgetDefault
    } else {
	option add *Dialog.msg.font {Times 12} widgetDefault
    }

    label $w.msg -justify left -text $text -font label_font
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {$bitmap ne ""} {
	if {($tcl_platform(platform) eq "macintosh"
	     || $windowingsystem eq "aqua") && ($bitmap eq "error")} {
	    set bitmap "stop"
	}
	label $w.bitmap -bitmap $bitmap
	pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    # 3. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $args {
	button $w.button$i -text $but -font button_font -command [list set ::tk::Priv(button) $i]
	if {$i == $default} {
	    $w.button$i configure -default active
	} else {
	    $w.button$i configure -default normal
	}
	grid $w.button$i -in $w.bot -column $i -row 0 -sticky ew \
		-padx 10 -pady 4
	grid columnconfigure $w.bot $i
	# We boost the size of some Mac buttons for l&f
	if {$tcl_platform(platform) eq "macintosh" || $windowingsystem eq "aqua"} {
	    set tmp [string tolower $but]
	    if {$tmp eq "ok" || $tmp eq "cancel"} {
		grid columnconfigure $w.bot $i -minsize [expr {59 + 20}]
	    }
	}
	incr i
    }

    # 4. Create a binding for <Return> on the dialog if there is a
    # default button.

    if {$default >= 0} {
	bind $w <Return> "
	[list $w.button$default] configure -state active -relief sunken
	update idletasks
	after 100
	set ::tk::Priv(button) $default
	"
    }

    # 5. Create a <Destroy> binding for the window that sets the
    # button variable to -1;  this is needed in case something happens
    # that destroys the window, such as its parent window being destroyed.

    bind $w <Destroy> {set ::tk::Priv(button) -1}

    # 6. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set x [expr {[winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]}]
    set y [expr {[winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]}]
    # Make sure that the window is on the screen and set the maximum
    # size of the window is the size of the screen.  That'll let things
    # fail fairly gracefully when very large messages are used. [Bug 827535]
    if {$x < 0} {
	set x 0
    }
    if {$y < 0} {
	set y 0
    }
    wm maxsize $w [winfo screenwidth $w] [winfo screenheight $w]
    wm geometry $w +$x+$y
    wm deiconify $w

    tkwait visibility $w

    # 7. Set a grab and claim the focus too.

    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {$oldGrab ne ""} {
	set grabStatus [grab status $oldGrab]
    }
    grab $w
    if {$default >= 0} {
	focus $w.button$default
    } else {
	focus $w
    }

    # 8. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    vwait ::tk::Priv(button)
    catch {focus $oldFocus}
    catch {
	# It's possible that the window has already been destroyed,
	# hence this "catch".  Delete the Destroy handler so that
	# Priv(button) doesn't get reset by it.

	bind $w <Destroy> {}
	destroy $w
    }
    if {$oldGrab ne ""} {
	if {$grabStatus ne "global"} {
	    grab $oldGrab
	} else {
	    grab -global $oldGrab
	}
    }
    return $Priv(button)
}


