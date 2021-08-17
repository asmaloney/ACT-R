This directory contains an example of creating a new motor module
action using the style mechanism which is built into the motor
module.  By using a style to implement a new action many of the
details for performing the action and managing the internal states
of the motor module are handled automatically.  There is no
real documentation on the style mechanism at this point, but the
comments in this example should help.  One can also look at the
support/general-pm.lisp file to find how styles are actually
implemented for more details (although there are not a lot of 
comments describing things there).                                       

The new-style.lisp file contains the code to implement a new
style based action along with comments describing the details
of what is needed.  It only needs to be loaded once to add the
new action using extend-manual-requests.

The style-test-model.lisp file contains a simple model that
just makes a repeated request for the new action.