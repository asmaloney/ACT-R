Threaded Cognition
ACT-R Implementation
Dario Salvucci & Niels Taatgen

This directory contains the code that implements the threaded cognition theory of concurrent 
multitasking.  To use the extension call (require-extra "threads") and that call can be
placed into any model file which needs the extension to have it loaded automatically.
You can also install the code into your ACT-R setup permanently by placing the "new-threads.lisp" 
file into the "actr7/modules/" directory.  The code will compile and load automatically during 
the next startup of the ACT-R system.  

The sample model has the require-extra call and thus can be loaded and run once ACT-R 7
is loaded.

For more information on using threading in your own models, please see the user guide 
"threads.pdf" also found in this directory.

For more information about the theory, please see: Salvucci, D. D., & Taatgen, N. A. (2008).  
Threaded cognition: An integrated theory of concurrent multitasking.  Psychological Review, 115, 101-130.
