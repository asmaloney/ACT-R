
The emma.lisp file implements the EMMA extension which extends the attention
shift action of the default ACT-R vision module as described in the paper in
the "CSR01.pdf" file to adjust the encoding times and account for eye position
(with two differences noted below).

To load this extension you should call (require-extra "emma"), and placing that
call into a model file which needs it will make sure it is available.

To use the extension you must also set the :emma parameter to t in the model.
No other changes are necessary to use EMMA in a model, but you may need to adjust
the scaling parameters and object frequencies to fit the appropriate attention 
shift times for your task.  The parameters for scaling and the command for 
adjusting frequencies are described below.

This implementation differs from the description in the paper because it 
uses the preparation cost and randomization mechanisms of the standard ACT-R
perceptual and motor modules instead of those in the paper.  The paper indicates
that there is a fixed cost for preparation, defaulting to 135ms, but for this
extension the preparation cost is based on the number of features which need
to be prepared relative to the previous action performed.  There are three 
features for these actions: the style of the action (which is always saccade),
the distance of the saccade, and the direction of the saccade.  Each feature 
requires a preparation cost which defaults to 50ms (settable by a parameter 
shown below) unless it matches the previous action in which case it has no cost.
That means the preparation cost varies from 0 to 150ms for the saccades.  The 
style will always match if there was a previous action (after a reset or visual
clear request there will not be a previous action), the distances will match if
their difference is 2 degrees of visual angle or less, and the angles will match
if they are within 90 degrees of each other.  With respect to the randomization
of times, the paper says that a Gaussian noise distribution is used, but the
times in this implementation use the same randomization as other modules which
is uniform and based on a range specified using the :randomize-time parameter 
which defaults to no randomization and when enabled with t results in a range 
from 2/3 to 4/3 of the time (other ranges can also be specified as described 
in the ACT-R reference manual).  The paper also says that the noise in the 
saccade locations is computed using a Gaussian distribution, but this module
uses the act-r-noise command which only approximates a Gaussian distribution
using a logistic distribution for that noise value.

This extension is implemented as an additional module for the model and it 
adds an additional buffer called emma.  That buffer however does not accept
requests and only exists for purposes of tracking the activity of the module
(for example the graphic tracing tools of the ACT-R Environment will display 
module activity similar to the images in figure 2 of the EMMA paper).

The following additional model parameters are added to the system for working
with EMMA:

:emma 

  This parameter controls whether the EMMA extension is enabled for the model.
  The default value is nil which is disabled, and setting it to t will enable it.

:visual-encoding-factor

  This parameter is the K value in the encoding time equation.  Defaults to .006

:visual-encoding-exponent

  This parameter is the k value in the encoding time equation.  Defaults to .4

:saccade-feat-time

  The time in seconds to prepare each feature of a saccade.  Defaults to .05

:saccade-init-time

  The non-labile portion of the saccade execution time in seconds.  Defaults to .05

:saccade-base-time

  The fixed saccade execution time in seconds.  Defaults to .02

:eye-saccade-rate

  The additional time cost in seconds of a saccade movement per degree of visual 
  angle subtended by the movement.  Defaults to .002

:vis-obj-freq

  The frequency for visual features which have not been set explicitly. Defaults to .1

:freq-feature
  
  The slot of a visual feature which is used to determine the frequency.  Can be set
  to a symbol or string which names the slot.  Defaults to value.

:eye-spot-color

  A switch to indicate whether the position of the eye is drawn in visible virtual 
  windows when the vision module's :show-focus parameter is enabled.  If set to nil
  then no spot is drawn.  If set to t or a symbol or string which names a color then
  the spot will be drawn in that color (or blue if set to t).  Default value is blue.


These commands are available for working with emma (the same name is used for the
Lisp function and the remote command string name).


register-feature-frequency

This command takes two parameters: feature and frequency
It sets the frequency for any visual object with the indicated feature value to
the frequency indicated.  The frequency must be a number between 0 and 1 (inclusive).
It returns the frequency upon success or nil if there is a problem.

set-eye-location

This command takes one parameter: location
It sets the current position of the model's eye fixation to be the given location.
The location must be a list with at least two values where the first specifies
the x coordinate and the second the y coordinate of the location.  It returns
the location value upon success or nil if there is a problem.

current-eye-location

This command takes no parameters.
It returns a list with the current position of the model's eye fixation.
The location will be a list with at least two values where the first specifies
the x coordinate and the second the y coordinate of the location, but if there
is a problem (no current model or EMMA is not enabled) it will return nil.


The module will also generate a signal named "current-eye-location" when the
eye position changes which will be passed two parameters indicating the new
x and y coordinates respectively.
