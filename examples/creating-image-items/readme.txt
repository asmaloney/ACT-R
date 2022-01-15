This directory contains files that demonstrate how to use the
image item available through the AGI.

The creating-an-image.lisp and creating-an-image.py files 
define simple test functions which open an experiment window
and add an image item.  If a visible window is requested then
the .gif files from the gui/AGI-images directory are displayed
in the visible virtual window created by the ACT-R Environment.
Details on using the test functions can be found in the comments
of those files.

The creating-an-image-model.lisp file defines a model which
will interact with the image displays created by those test
functions.  The model will attened to each item in the display,
print out the visual-location and visual chunk which it gets,
and then click on the image.


The background-image.lisp and background_image.py files define
a simple test function which opens a window and dispays an image
and then adds additional features for the model to see which
correspond to items in that image file.  This use of the image
item is really more to provide a "pretty" display than something
for the model to see, but there is still a feature created for it
which may require some care in the model to not "see" that image.

The background-model.lisp file contains a model which attends to
the custom features which are created over the background image.
