ACT-Touch is a set of manual motor request extensions for ACT-R along with
a new device (called touch) which can be used with those extensions. These 
manual request extensions constitute theoretical claims predicting motor 
preparation and execution times for certain manual gestures commonly used 
with multitouch computer displays. These manual request extensions follow 
ACT-R's theoretical claims about how cognition interfaces with action to 
output information from the human to the environment, which in turn 
originated with the EPIC architecture.

As ACT-Touch extends ACT-R's framework with additional manual movement 
vocabulary, many movement styles are analogous to extant ACT-R movement styles
in the sense that a movement is composed of a certain set of features which 
specify the movement such as a hand, a finger, a direction and a distance. 
Consequently ACT-Touch's movement styles are subject to the same constraints
and caveats as ACT-R's, e.g., finger positions that would be physically 
impossible for any physical human hand to attain are specifiable for a cognitive
model, and so it is up to the modeler to consider such things.

ACT-Touch is implemented as Lisp code that can be loaded with the ACT-R software.
There are 3 files included in this directory (in addition to this readme file):

 - act-touch.lisp, which is the set of manual request extensions and the 
   device definition.
 - touch-demo-model.lisp, a demonstration model which uses the new motor
   actions both with and without the new touch device.
 - LGPL.txt, a text file of the LGPL license under which this software is
   distributed.


To use ACT-Touch the act-touch.lisp file must be loaded after loading ACT-R.
The easiest way to do that is to use the ACT-R command require-extra like
this:

(require-extra "act-touch")

That will load the act-touch extenision if it has not already been loaded,
and putting that in any model file which uses the extension will make sure
it gets loaded when needed.


This version of ACT-Touch is only compatible with ACT-R 7.12 or newer, and is
included with the ACT-R source code.  Information on other versions can be found
on Cogscent, LLC's website: http://cogscent.com/.

The documentation for this version of ACT-Touch is included below.  Documentation
from an older version can be found in the directory extras/act-touch/depricated/
of the ACT-R source code distribution, but that is provided only for basic reference
purposes because the operation of the software with this version of ACT-R differs
significantly from that prior documentation.  

The touch-demo-model.lisp file shows the use of all of the new motor extensions, 
the use of the touch device, and the signals that it generates.


In additon to the motor extensions and the touch device there is also a module 
called touch-interface created to provide the modeler with a set of parameters 
which can be used to configure the new motor extensions and the touch device.
That module does not provide any cognitive functionality for the model -- it is
only there to provide the parameters to the modeler.  Those new parameters will
be described first.

:finger-height 

   The distance between the model's fingers and the surface to be touched, measured
   in inches.  This affects the timing of tap and tap-hold actions, and the default
   value is 1 inch.

:finger-spacing 

   The distance between the finger tips of the model's fingers measured in inches
   on the touch device.  The model's fingers are spaced this far apart with the 
   thumb being one spacing horizontal from the index finger and two spacings 
   vertically.  The default value is .75 inches.

:left-start 

   The coordinates, measured in pixels, for where the model's left hand should 
   be located (referenced by the index finger) when moved to a touch device which
   does not include any additional details.  It must be a list of two values and the
   first is the x position and the second the y position.  The default value is (0 300).

:right-start 

   The coordinates, measured in pixels, for where the model's right hand should 
   be located (referenced by the index finger) when moved to a touch device which
   does not include any additional details.  It must be a list of two values and the
   first is the x position and the second the y position.  The default value is (1024 300).

:show-fingers

   A boolean value indicating whether the touch device should display the positions of
   the fingers on an experiment window with which it is associated.  If it is set to 
   t then the touch device will send notifications to the experiment window device
   when the hands are moved to or from the device and when a finger is moved on the
   touch device.  Those signals will be described below along with how the default
   experiment window viewer in the ACT-R Environment handles them.  The default value 
   is nil (do not send notifications).

:swipe-speed 

   This is the default value for the speed of a swipe action if one is not provided
   in the request.  It must be an integer from 1 to 5 (inclusive).  The default value is 3.

:swipe-width 

   This is the size in pixels to use for the target when computing the time it takes to
   perform a swipe action using Fitts law.  The default value is 500.

:thumb-width 

   This is the width of the model's thumbs, measured in inches, and is used when computing
   the time it takes to perform a pinch action using Fitts law where the thumb width is the
   size of the target.  The default value is 1.

:touch-noise 

   A boolean value indicating whether the move-hand-touch actions should include noise when
   computing the location of the hand after the action.  If set to nil (the default value)
   then the actions will always result in the hand being located at the desired coordinates.
   If set to t then the final location of the hand will be noisy (the shape of that noise
   is described with the action).

:touch-drag-delay

   This parameter indicates whether the move-hand-touch actions should take longer when 
   a finger is held down.  The default value is nil which means that it does not increase
   the time of the action.  If it is set to t then the control order of the action
   for the Fitts Law calculation is increased by .8 (the control order scales the time
   of the action by a factor of 2^c where c is the control order and the default control
   order for move-hand-touch actions is 0).  If this parameter is set to a number that
   value is used as the increment for the control order.

:touch-rotate-width

   This parameter determines the size of the target used in computing the timing for a rotate
   action using Fitts law.  The target size is the length of the arc subtended by an angle of
   the specified number of degrees on the circle of rotation (effectively this is setting the 
   accuracy of the rotation, but currently the rotations are not noisy).  The default value is
   5.


There are several new motor actions provided with the ACT-Touch extension.  Those actions can
be used with any device (or no device) currently installed for the model's hands, but if the
device does not support the appropriate notifications used then the results may not be very
useful.  In particular, since the coordinates for these actions are measured in pixels, but
the motor module does not operate in pixel space, the device is required to maintain the
appropriate positioning to create the appropriate timing (in the same way that cursor devices
maintain a position separate from that of the model's hand).  

When describing the request to perform the actions, if a slot-value pair is optional it will
have curly braces around it.  If a slot requires specific values, those will be shown
between square brackets with vertical lines separating the options.  Other values for a slot
will be described in a set of angle brackets -- that description indicates a single value.

When describing the notifications that the actions send to the device all values are constants
except those in angle brackets which describe a value provided.



Move-hand-touch

A move-hand-touch action results in the model moving a hand to point the index finger of that
hand at a visual location or visual object chunk provided.  It is performed using a ply action 
from the standard motor module actions and is very similar to the move-cursor action which is used
with mouse and joystick devices.  The timing of the action is computed using Fitts law with the
coefficient being the value of the motor module's :cursor-fitts-coeff parameter (default .1), the
distance traveled in pixels, and the approach width of the target item in pixels, and if the 
:touch-drag-delay parameter is set and any finger of the indicated hand is currently down then
the control order of the action will be increased from 0 to the value indicated to slow the 
action. 

If the :touch-noise paramter is set to t, then the final location of the hand will be noisy with
the location being determined by adding two noise components chosen from a logistic distribution
(approximation of a normal distribution) based on the width of the target (w) with a standard 
deviation along the axis of movement of:

(w / 4.133) * (sqrt(3) / pi)

and a standard deviation perpendicular to the axis of movement of:

.75 * (w / 4.133) * (sqrt(3) / pi)

as described in:

Gallagher, M. A., & Byrne, M. D. (2013). The devil is in the distribution: Refining and ACT-R model
of a continuous motor task. In Proceedings of the 12th International Conference on Cognitive Modeling.

and 

May, K. (2012). A model of error in 2D pointing tasks. Undergraduate Honors Thesis, Rice University,
Houston, TX.


The manual request can optionally specify the hand, but if not provided defaults to the right hand,
and must provide at least one of the obj or loc slots for the target item.

+manual>
  cmd move-hand-touch
  { hand [ left | right ] }
  { obj <visual object chunk> }
  { loc <visual-location chunk> }

It sends six notifications to the device associated with the hand which performed the move-hand-touch
at the execution time.

A list of lists indicating the details of the action:

((model <model name>) (style move-hand-touch) (hand <requested hand>) (loc <ending x coordinate> <ending y coordinate>))

A set-finger-position for each finger on the hand (index, middle, ring, pinkie, thumb) based on 
the movement assuming all the fingers move together:
    
(set-finger-position <requested hand> <finger> <finger's new x position> <finger's new y position>)


Tap

A tap action results in a finger moving to temporarily contact the touch surface at its 
current location and then returning to its initial position ready to act again.  It is similar
to the standard motor module punch action except that because the finger must be moved through
a distance to strike the surface (unlike punch which assumes the finger is on the key/button)
the timing is based on that of a peck action which is computed using Fitts law with the coefficient
being the value of the motor module's :peck-fitts-coeff parameter (default .075), the distance
specified by the :finger-height parameter, and a target width of 1 degree of visual angle.

The manual request requires a hand and finger value:

+manual>
  cmd tap
  hand [ left | right ]
  finger [ index | middle | ring | pinkie | thumb ]

It sends three notifications to the device associated with the hand which performed the tap.

At the execution time it sends a list of lists indicating the details of the action:

((model <model name>) (style tap) (hand <requested hand>) (finger <requested finger>))

Also at execution time it sends a notification list indicating that the finger is down:

(set-finger-down <requested hand> <requested finger>)

At :motor-burst-time after execution it sends a notification list indicating that the finger is up again:

(set-finger-up <requested hand> <requested finger>)


Tap-hold

A tap-hold action is like a tap action, except that the finger remains in contact with the
surface -- a finger moves to contact the touch surface at its current location and stays there.
The timing is based on that of a peck action which is computed using Fitts law with the coefficient
being the value of the motor module's :peck-fitts-coeff parameter (default .075), the distance
specified by the :finger-height parameter, and a target width of 1 degree of visual angle.

The manual request requires a hand and finger value:

+manual>
  cmd tap-hold
  hand [ left | right ]
  finger [ index | middle | ring | pinkie | thumb ]

It sends two notifications to the device associated with the hand which performed the tap-hold.

At the execution time it sends a list of lists indicating the details of the action:

((model <model name>) (style tap-hold) (hand <requested hand>) (finger <requested finger>))

Also at execution time it sends a notification indicating that the finger is down:

(set-finger-down <requested hand> <requested finger>)


Tap-release

A tap-release action ends a tap-hold action and lifts the finger from the surface at its
current position and is then ready to act again.  It's cost is simply the motor module's 
:burst-time parameter (default .05 seconds) since it is not a targeted action.

The manual request requires a hand and finger value:

+manual>
  cmd tap-release
  hand [ left | right ]
  finger [ index | middle | ring | pinkie | thumb ]

It sends two notifications to the device associated with the hand which performed the tap-release.

At the execution time it sends a list of lists indicating the details of the action:

((model <model name>) (style tap-release) (hand <requested hand>) (finger <requested finger>))

Also at execution time it sends a notification indicating that the finger is up:

(set-finger-up <requested hand> <requested finger>)


Swipe

A swipe action involves the model placing one or more fingers in contact with the surface,
moving them together across the surface for a specified distance and direction with no
specific target, optionally specifying a speed from 1-5 (5 being fastest), and then lifting
them from the surface and returning them to where they started.  The timing is computed using
Fitts law with the distance traveled in pixels and a target width as specified by the 
:swipe-width parameter.  The coefficient for the calculation is 1/(speed^2) where the
speed is either the given speed if provided, and if not, the value of the :swipe-speed parameter.
Note, depending upon the distance traveled and the setting of :swipe-width it is possible 
for different speed swipes to actually take the same amount of time to complete either due
to rounding (times are rounded to the millisecond) or because of the minimum action time
set for the motor module (based on the values of :motor-burst-time and/or :min-fitts-time).

The request requires a hand, initial finger, number of fingers, distance (in pixels), and 
direction (in radians).  It can optionally specify the speed.

+manual>
  cmd swipe
  hand [ left | right ]
  finger [ index | middle | ring | pinkie | thumb ]
  r <distance in pixels>
  theta <direction in radians>
  num-fngrs <total number of fingers>
  { speed [1 | 2 | 3 | 4 | 5] }


It sends several notifications to the device associated with the hand performing the swipe.

At the execution time it sends a notification with the swipe action details:

((model <model name>) (style swipe) (hand <requested hand>) (finger <requested initial finger>)
 (distance <pixel distance>) (direction <angle in radians>) (speed <speed value>) 
 (count <number of fingers swiped>))

At initiation time it sends a finger down notice for each finger involved in the swipe
(starting with the indicated finger and then counting through them in the order index, middle,
ring, pinkie, and thumb):
  
(set-finger-down <requested hand> <each swiped finger>)

At execution time each finger is moved to the end of the swipe:

(set-finger-position <requested hand> <swiped finger> <that finger's x position at end of swipe> <that finger's y position at end of swipe>)

At :motor-burst-time after execution it sends a notification indicating that each finger is up again:

(set-finger-up <requested hand> <each swiped finger>)

At action finish time it sets each finger back to its starting position:

(set-finger-position <requested hand> <swiped finger> <that finger's starting x position> <that finger's starting y position>)


Pinch

A pinch action involves the model moving a finger and thumb to touch the surface at a 
specified distance apart.  Then moving them each equally along the line connecting 
the surface contact points until they are a specified distance apart, and then lifting
them from the surface and returning them to their starting locations.  The timing for
a pinch is computed using Fitts law with the coefficient being the value of the motor
module's :peck-fitts-coeff parameter (default .075), the distance traveled in pixels
between the starting and ending positions, and the width of the models thumb as specified
with the :thumb-width parameter (converted to pixels).

The request requires a hand, finger, starting width in pixels, and ending width in pixels
(a pinch can go in either direction -- the ending width does not need to be smaller than 
the starting width).

+manual>
  cmd pinch
  hand [ left | right ]
  finger [ index | middle | ring | pinkie ]
  start-width <pixel size to start>
  end-width <pixel size at end>


It sends several notifications to the device associated with the hand performing the pinch.

At the execution time it sends a notification with the pinch action details:

((model <model name>) (style pinch) (hand <requested hand>) (finger <requested finger>)
 (range <starting pixel distance> <ending pixel distance>))

At initiation time it moves the finger and thumb to be the given distance apart (by moving them
equally along the line through their starting positions) and sends a finger down notice for each:

(set-finger-position <requested hand> thumb <start of pinch thumb x position> <start of pinch thumb y position>)
(set-finger-position <requested hand> <finger> <start of pinch finger x position> <start of pinch finger y position>)
(set-finger-down <requested hand> thumb)
(set-finger-down <requested hand> <finger>)

At execution time the thumb and finger are moved to the ending position of the pinch:

(set-finger-position <requested hand> thumb <end of pinch thumb x position> <end of pinch thumb y position>)
(set-finger-position <requested hand> <finger> <end of pinch finger x position> <end of pinch finger y position>)

At :motor-burst-time after execution it sends a notification indicating that each is up again:

(set-finger-up <requested hand> thumb)
(set-finger-up <requested hand> <finger>)

At action finish time it sets each back to its starting position:

(set-finger-position <requested hand> thumb <thumb x position before pinch> <thumb y position before pinch>)
(set-finger-position <requested hand> <finger> <finger x position before pinch> <finger y position before pinch>)


Rotate

A rotate action involves the model moving a finger and thumb to touch the surface at  
their current locations.  Then moving them each rotationally by a given angle around 
a circle defined with a diamater of the distance between them and a center at the 
midpoint of the line segment between them, lifting them from the surface, and returning
them to their starting locations.  The timing for a rotation is computed using Fitts law
with the coefficient being the value of the motor module's :peck-fitts-coeff parameter 
(default .075), the distance traveled along the circle in pixels, and a target width
which is the length of the arc subtended by an angle of :touch-rotate-width on the circle
of rotation.

The request requires a hand, finger, and rotation angle (positive is clockwise and negative
is counter-clockwise).

+manual>
  cmd rotate
  hand [ left | right ]
  finger [ index | middle | ring | pinkie ]
  rotation <angle in radians>


It sends several notifications to the device associated with the hand performing the rotate.

At the execution time it sends a notification with the pinch action details:

((model <model name>) (style rotate) (hand <requested hand>) (finger <requested finger>)
 (direction <requested angle>))

At initiation time it sets the finger and thumb down on the surface:

(set-finger-down <requested hand> thumb)
(set-finger-down <requested hand> <finger>)

At execution time the thumb and finger are moved to the ending position of the rotation:

(set-finger-position <requested hand> thumb <end of rotate thumb x position> <end of rotate thumb y position>)
(set-finger-position <requested hand> <finger> <end of rotate finger x position> <end of rotate finger y position>)

At :motor-burst-time after execution it sends a notification indicating that each is up again:

(set-finger-up <requested hand> thumb)
(set-finger-up <requested hand> <finger>)

At action finish time it sets each back to its starting position:

(set-finger-position <requested hand> thumb <thumb x position before rotate> <thumb y position before rotate>)
(set-finger-position <requested hand> <finger> <finger x position before rotate> <finger y position before rotate>)





Devices for touch actions

The actions above indicate the notifications that are sent to the device to specify what
the model's hands are doing.  In addition to those, there are three notifications which 
those actions use to acquire the current positional information from the device to 
calculate the appropriate timing:

(finger-position <hand> <finger>)

The device should return a list of three values containing the x and y coordinates for the 
finger on the specified hand in pixels and whether the finger is above the surface or on
the surface represented as 0 if the finger is down and 1 if it is up, respectively.

(finger-down <hand> <finger>)

The device should return a true value if the specified finger on the given hand is currently
down (on the surface) and a false value (nil) if not.

(any-finger-down <hand>)

The device should return a true value if any of the fingers on the given hand are currently
down (on the surface) and a false value (nil) if not.



Provided device for touch actions

The ACT-Touch extension includes a device called "touch" which can be used with the new
motor actions.  The touch device will generate signals which can be monitored for each 
of the action notifications which it receives.  It provides a new motor command for moving
a model's hand to the "touch" device and a user command for placing a model's hand on the touch
device prior to running.

Installing a "touch" device

Only one "touch" device can be installed at a time.  There are three options for installing 
a "touch" device.  

The default operation is available simply by installing the device with no details:

(install-device '("motor" "touch"))

One can also install the device providing the starting pixel locations for the left and
right hands when they are moved to the device (which override the :left-start and 
:right-start parameters).  Both locations must be provided as a list of 2 element lists
as the device details:

(install-device '("motor" "touch" ((<left x> <left y>) (<right x> <right y>)))

One can also install the device and associate it with an experiment window from the ACT-R
AGI (which has already also been installed) by providing the window identifier (the value 
returned from open-exp-window) as the details.  When associated with an experiment window
the starting locations for the hands are the middle of the corresponding edge of the window.
Additionally, the "touch" device will send notifications to the experiment window with
information about finger positions (described below):

(install-device '("motor" "touch" <window identifier>))


The "touch" device is located at position 6,9 in the motor module's coordinates for the hand
positions (which is below the center of the space bar for the default keyboard device's
location).  To place the model's hands on the device the modeler can use the start-hand-at-touch
command (available as a Lisp function and remote command).

Start-hand-at-touch {hand}

hand := [ right | left ]

If the optional hand parameter is not provided it defaults to right.  The indicated hand
will be placed on an installed "touch" device prior to running the model.  If there is no
"touch" device installed, then a default "touch" device will be installed before placing 
the indicated hand on it.

Also, there is a new motor action called hand-to-touch which can be used to have the model
move a hand to the "touch" device.

Hand-to-touch

A hand-to-touch action results in the model moving a hand from its current location to the
"touch" device at the appropriate starting position for that hand on the device.  It is 
performed using a ply action from the standard motor module actions.  The timing of the action
is computed using Fitts law with the coefficient being the value of the motor module's 
:cursor-fitts-coeff parameter (default .1), the distance traveled in the motor module's
coordinates for the hand from its current position to position 6,9, and the width of the target
being 4.

The manual request can optionally specify the hand, but if not provided defaults to the right hand.

+manual>
  cmd hand-to-touch
  { hand [ left | right ] }

It sends six notifications to the device associated with the hand which performed the hand-to-touch
action at the execution time.

A list of lists indicating the details of the action:

((model <model name>) (style move-hand-touch) (hand <requested hand>) (loc <starting x pixel coordinate> <starting y pixel coordinate>))

A set-finger-position for each finger on the hand (index, middle, ring, pinkie, thumb) based on 
the default finger locations for the device:
    
(set-finger-position <requested hand> <finger> <finger's starting x position> <finger's starting y position>)



"touch" device signals

When the action notifications are received by the "touch" device (the notifications which have
a style value) it will generate a signal which can be monitored.  Here are the signals and the
parameters passed to them based on the style of the action:

move-hand-touch style generates a "touch-point" signal.  It is passed four parameters: the model name,
the hand that performed the action, the new x position, the new y position.

tap style generates a "touch-tap" signal.  It is passed five parameters: the model name, the hand that
performed the tap, the finger that performed the tap, the x position of the tap, the y position of the tap.

tap-hold style generates a "touch-tap-hold" signal.  It is passed five parameters: the model name, the hand that
performed the tap-hold, the finger that performed the tap-hold, the x position of the tap-hold, the y position of
the tap-hold.

tap-release style generates a "touch-tap-release" signal.  It is passed five parameters: the model name, the hand
that performed the tap-release, the finger that performed the tap-release, the x position of the tap-release, the 
y position of the tap-release.

swipe style generates a "touch-swipe" signal. It is passed seven parameters: the model name, the hand that performed
the swipe, the initial finger of the swipe, the distance swiped, the direction swiped, the total number of fingers
swiped, the swipe speed.

pinch style generates a "touch-pinch" signal.  It is passed five parameters: the model name, the hand that performed
the pinch, the finger that performed the pinch, the starting width of the pinch, the ending width of the pinch.

rotate style generates a "touch-rotate" signal.  It is passed four parameters: the model name, the hand that performed
the rotate, the finger that performed the rotate, the angle rotated.



"touch" device and AGI window

When a "touch" device is associated with an experiment window any tap actions will result
in a click notification being sent to the window device at the point of the tap.  That click
will trigger actions of the AGI window in the same way a mouse click does (pressing buttons
and triggering image actions).

If the "touch" device is associated with a window and the :show-fingers parameter is set to t
then it will send notifications to that window whenever a finger's position changes on the 
"touch" device (either moving to a new location, moving up or down, or the hand moving away 
from the device).

All movements on the "touch" device result in a notification for the new location of the finger
on a hand indicating the x and y pixel coordinates and a z value of 0 for down or 1 for up.

(showfinger <hand> <finger> <x> <y> <z>)

If a hand moves away from the device then a notification for each finger will be sent to 
indicate that like this:

(removefinger <hand> <finger>)
 
If the AGI window is a visible virtual window being displayed by the ACT-R Environment then
it will respond to those notifications to draw the locations of the fingers.  Each finger
will be drawn as a brown circle containing two letters.  The first letter will be L or R 
indicating the hand and the second letter will be I(ndex), M(iddle), R(ing), P(inkie), or 
T(humb) to indicate the finger.  If the finger is above the surface the circle will have a white
background and if it is touching the surface the circle will have a brown background.
