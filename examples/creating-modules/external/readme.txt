There are two examples here of replacing the internal ACT-R goal module
with one that is implemented in Python and one example of a replacement
for the ACT-R declarative memory module implemented in Python.

The goal.py version requires the general ACT-R interface implemented in
the actr.py file (found in the tutorial/python directory) to operate.
The goal_complete.py version contains its own stripped down version of
the communication code such that it is self contained and does not 
require the actr.py file, but it is perhaps still a little more general
than it needs to be.

Both work very similar to the normal ACT-R goal module and also define
the goal-focus and mod-focus macros which allow most of the tutorial 
models to work as-is with this replacement.  The two primary differences
between these and the default goal module are: there are no goal-focus-fct
and mod-focus-fct functions available so you'd have to call the command
through the dispatcher e.g. (call-act-r-command "goal-focus" 'name) and
for simplicity of the example the mod-focus command doesn't properly
handle the situation when there's a pending goal-focus (it shouldn't
schedule the mod-buffer-chunk directly in that case and instead should
schedule a custom action to make the change).

The simple_declarative.py file implements a small subset of the abilities
of the ACT-R declarative memory module using the actr.py interface. It is
sufficient to run the models of unit 1.  It implements the add-dm and dm
command macros which work like the original versions, and a version of 
the sdp macro which can only be used to see the very limited set of 
parameters recorded for the chunks in the declarative module.  The module
only makes the :lf parameter available and does not compute activations
for chunks.  It does however record a chunk's creation time and the 
reference counts as chunks are merged into memory (which are shown from 
sdp).  It will not work with the Stepper or declarative memory viewer
in the Environment.

After you import either of these goal modules and/or the declartive module
you should be able to load the tutorial unit 1 models just as before, and  
if you load the ACT-R testing code (provided with the source distribution)
and check the unit 1 model tests they should all pass the tests with no 
differences from the original results:

> (clear-all)
NIL
> (load "ACT-R:test-models;model-tester.cl")
T
> (run-actr-test "count")

###########################
Testing file "count"
; Loading C:\Users\db30\Desktop\actr7.x\test-models\count.lisp

No differences detected.
NIL
> (run-actr-test "addition")

###########################
Testing file "addition"
; Loading C:\Users\db30\Desktop\actr7.x\test-models\addition.lisp

No differences detected.
NIL
> (run-actr-test "semantic")

###########################
Testing file "semantic"
; Loading C:\Users\db30\Desktop\actr7.x\test-models\semantic.lisp
#|Warning: Creating chunk CATEGORY with no slots |#
#|Warning: Creating chunk PENDING with no slots |#
#|Warning: Creating chunk YES with no slots |#
#|Warning: Creating chunk NO with no slots |#

No differences detected.
NIL
> (run-actr-test "unit-1-together-1-mp")

###########################
Testing file "unit-1-together-1-mp"
; Loading C:\Users\db30\Desktop\actr7.x\test-models\unit-1-together-1-mp.lisp
#|Warning (in model SEMANTIC): Creating chunk CATEGORY with no slots |#
#|Warning (in model SEMANTIC): Creating chunk PENDING with no slots |#
#|Warning (in model SEMANTIC): Creating chunk YES with no slots |#
#|Warning (in model SEMANTIC): Creating chunk NO with no slots |#

No differences detected.
NIL
