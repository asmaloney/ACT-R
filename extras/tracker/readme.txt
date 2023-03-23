This directory contains a module which can be used to allow a model
to estimate the value for a quantity based on abstract feedback that
it receives without explicit deliberation.

To use the module this call can be used when there is no current model:

(require-extra "tracker")

The recommended way to use that call is to place it after clear-all
in any model file which will use this extra.

Details on how it works are in the tracker documentation (.odt or .pdf
file depending on the source of the files).  The simple.lisp file 
contains a simple task and model showing how the tracker module
works (although it is not actually performing the type of task for
which it is intended).  The suspend-resume.lisp file has a similar
model and task to the one in simple.lisp, but it creates multiple 
trackers and uses the suspend and resume requests.  The biased.lisp
file shows the creation of trackers which do not have a uniform 
initial probability distribution by providing initial experiences
to create the starting quadratic.