Need to have quicklisp installed to run this example:

https://www.quicklisp.org/


Load the simple.lisp file (into a Lisp other than the one which
is running ACT-R) to add a command called "remote-count" to ACT-R
which takes a list of items as its only parameter and returns the 
length of the list.

Here's a sample call:

(evaluate-act-r-command "remote-count" (list 1 2 3))