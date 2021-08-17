This directory contains very simple examples of creating a connection
to ACT-R in different languages.  In each example it opens a socket
to connect to ACT-R, indicates a name for the connection which can
then be found in the ACT-R connections list, evaluates the ACT-R 
"act-r-version" command, prints the result, then adds a new command 
and loops forever responding to evaluate requests of that command. 

All of the examples use the files written by ACT-R to the current 
user's home directory to indicate the address and port for the 
socket connection.  If ACT-R is not running on the same machine (and
thus didn't write those files) it will be necessary to create those
files to use these examples.

For consistency and ease of creation, the examples all work basically
the same way and write the JSON output explicitly, but use a "library"
in the language to parse the incoming data appropriately. 

Some of the validation checks on the communication are unnecessary
in the examples because if a connection has not added a command the
only thing it will be sent are responses to evaluate requests it has
made and it will only be asked to evaluate commands which it has
added.  However, they are included for example purposes since some
additional testing will be necessary for a system which needs a more 
general connection capable of both sending and receiving evaluation
requests simultaneously (which will likely also require a lot more
complicated code with multiple threads and appropriate access 
protection).

Many of these examples were not written by an experienced programmer 
in that language, and thus may not be the best way to handle any of
the things they do.  However, they should be easy enough to understand
and adapt for someone that does know the language.

The readme.txt file with each describes any additional details about
requirements to use the code and an example call from ACT-R (Lisp)
to evaluate the remote command using the evaluate-act-r-command of
ACT-R which returns multiple values.  The first is either t or nil
indicating whether the command succeeded.  If successful the rest
of the return values are those returned by the command called.  If
not successful the second return value is the string of the error
message reported while trying to evaluate the command.