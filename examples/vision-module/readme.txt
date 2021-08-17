
This directory contains examples of capabilities of the vision module which
may be useful but which are not covered thoroughly (or at all) in the tutorial.
Versions of many of these examples were also inlculded with previous versions
of the ACT-R software (prior to 7.6), but in those versions required creating
a "device" to perform those operations.  With ACT-R 7.6+ the device is no 
longer a monolithic interface for all of the perceptual/motor action and many
of the abilities which one had to create a device for no longer require a
device at all.  None of these examples require creating a new device, although
some do rely upon the "experiment window" device provided by the ACT-R GUI
Interface tools (called the AGI).

These examples cover both capabilities which are used in a model and those
which are used in the code to create the visual task/world for the model.
For the model based examples only a .lisp file is provided with the task
code written in Lisp since it is often trivial and not important for the
example.  For the examples which are providing information about ACT-R 
commands which can be used in code there is both a Lisp and a Python example
which use the same model file (the Python examples rely upon the actr.py module
included with the ACT-R tutorial for connecting to ACT-R and providing functions
to access the ACT-R commands).

