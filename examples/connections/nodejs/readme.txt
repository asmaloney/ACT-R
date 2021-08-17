
There are three examples here, which were only tested on Windows with 
Node.js version 10.13.0. 

The first example, simple.js, just shows a simple connection to
the ACT-R server and can be run from a command prompt with:

C:\actr7.x\examples\connections\nodejs>node simple.js

It adds a command called "node-js-add" which takes two parameters and 
returns the sum of those items.  It's lacking some error handling,
so should only be called with numbers.

Here's a sample call:

(evaluate-act-r-command "node-js-add" 3 8)

The other two examples depend upon two additional libraries for node.js
which were installed using npm like this:

C:\actr7.x\examples\connections\nodejs>npm install --save express

C:\actr7.x\examples\connections\nodejs>npm install --save socket.io

The second example, relay-virtual-view.js, opens a web server on 
port 3000 which serves two pages.  The default page is found in
the index.html file and it displays a canvas which shows all the
text, buttons, and lines drawn by the AGI as well as the virtual
mouse cursor and models' visual attention marker for visible
virtual windows.  Those windows can be interacted with by a
person as well, and thus one can watch or perform all of the
tasks from the tutorial through a browser window.  The other
page it serves is /goal.html which displays a single button
that says Update.  Pressing that button will cause the window
to print the current contents of the model's goal buffer above 
the Update button.


It can be run like this:

C:\actr7.x\examples\connections\nodejs>node relay-virtual-view.js

Then you can open a browser on the same machine and go to:

localhost:3000

or 

localhost:3000/goal.html


or on a different machine you could use the full IP address of the
computer running the example instead of localhost.



The third example, environment.js, extends the relay-virtual-view
example.  It uses port 4000 and serves two pages: 

<address>:4000

which returns the environment.html page, and:

<address>:4000/expwindow.html

which returns the expwindow.html page (which is basically
just a copy of the index.html file from the previous example
to display the visible virtual windows).  There is also a file
called act-r.html which can be opened in a browser and has links
to those two pages using "localhost" which should connect to a
version running on the same machine.

The environment.html page provides a subset of the tools that
are available through the ACT-R Environment application.  It 
currently includes the current model display/selection, the
ability to pick files for ACT-R to load if running on the same
machine as the ACT-R system, the same control items (stepper, run,
reset, reload, event queue, and running indicator), all of the basic
inspectors except for the parameter viewer (buffers, declarative, 
procedural, visicon and audicon), and the connection and command 
viewer.  It can be used with, or instead of, the Tcl/Tk based 
Environment and applications are included with the standalone 
versions as an alternative to the Tcl/Tk Environment application.



Extra note:

The package.json file can be used with the pkg tool to build
a self contained executable of the environment.js example.
If pkg is installed:

npm install -g pkg

then this should build Linux, Mac OSX, and Windows executables:

pkg .


Those executables are currently included and moved to the apps
directory for use with the standalone versions.

