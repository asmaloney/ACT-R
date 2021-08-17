Tested with: gcc version 4.8.5 20150623 (Red Hat 4.8.5-28) (GCC)

Uses the Jansson library for parsing JSON strings:

http://www.digip.org/jansson/

If the Jansson library is installed in the ACT-R examples/connections/c
directory this call would build an a.out that can be run:

gcc simple.c -B . -L ./lib -ljansson

Running that file will add a command called "c-add" to ACT-R which 
takes two parameters and returns the sum of those items.

Here's a sample call:

(evaluate-act-r-command "c-add" 3 4)


This example could use a lot more error checking and parameter testing,
but is already a little long for a 'simple' example.