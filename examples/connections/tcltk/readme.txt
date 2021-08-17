Tested with Tcl/Tk 8.4.14

Source the simple.tcl file to add a command called "tcl-add" 
to ACT-R which takes two parameters and returns the sum of those
items if they are numbers.

It also opens a message box to show the version returned by ACT-R
since the puts and flush don't seem to work when sourced on
some machines.

Here's a sample call:

(evaluate-act-r-command "tcl-add" 3 4)