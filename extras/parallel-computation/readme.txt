This directory contains code which provides functions for performing
computations in parallel using multiple threads with "mapcar like"
parallel functions.

The recommended way to load this is to use require-extra:

(require-extra "parallel-computation")

in any file which uses the functions defined here.

The basic procedure for using this is to create a "team" of workers.  Then, that
team, a function, and list of parameters can be passed to one of the parallel 
running functions.  The parallel function will create a list of results based
on the function and parameters provided by breaking the parameters into sublists
and having the workers of the team process those sublists in parallel.  The return 
values from the parallel function will then be a list of results and an indication 
of whether there was any error.  When the team is no longer needed it should be 
released to end the worker processes that were created.

See the comments in the parallel-computation.lisp file for details on the
commands available and an example which uses them.

