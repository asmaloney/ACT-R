This directory contains a file which provides the ability to run
multiple instances of a Lisp based task and ACT-R model in parallel
using multiple threads.

To load it you should use:

(require-extra "parallel-models")


The basic procedure for using this is to create a set of independent ACT-R
instances, instantiate the model to be run in each, and then use those to 
run multiple copies of a model in parallel and get the results of each returned
in a list.

Each of the ACT-R instances is created in its own package and loads the full
ACT-R sources into that package.  Instantiating the model involves compiling
and then loading the model file(s) into each of those separate packages.  There
are some restrictions because of that:

  - The ACT-R instances created do not start a remote connection which is
    why everything must be done within Lisp.

  - The model code should not call reload since there will only be a single
    compiled file which will be specific to one of the packages.

  - The model code should not include any package specific code, or if it 
    does, then it must use the full package specifier for all references to 
    that code.

  - The model file(s) must include all the code necessary to run it in ACT-R.

  - All of the experiment and model code must be thread-safe.

  - Any code which writes out data should be sure to use unique file names
    and/or separate output streams i.e. if everything is writing to the
    default *standard-output* it could be very difficult to read, or worse
    could lead to errors if the Lisp's output functions aren't actually
    thread safe on the default streams (surprising but seems to be true
    in some cases!).

Since the values returned from the model runs will each be generated in a
different package it is probably best to avoid returning symbols as results
since they will be symbols from that package.

Because this recompiles the original ACT-R sources you will need to force
them to be recompiled the next time you start a Lisp and load ACT-R since
the existing "fasls" will be compiled for a package which doesn't exist.

How many instances to create for best performance is going to depend on
a lot of factors.  Perhaps the biggest is memory availability and usage
within the Lisp being used since each instance is a full copy of the ACT-R
system code.  That's likely going to set the upper limit of how many you
can create and actually run.  I can get about a half dozen in ACL and
CCL on a Windows 7 machine with 8GB of RAM before things start to become
unstable.


Below is an example of running the starting zbrodoff model from unit 4 of 
the tutorial using this version of ACL:
 
International Allegro CL Enterprise Edition
9.0 [Windows *SMP*] (Jun 15, 2016 12:41)
Copyright (C) 1985-2012, Franz Inc., Oakland, CA, USA.  All Rights Reserved.


Running the experiment 4 times sequentially took 26.443 seconds
and running it in 4 parallel instances took 9.734 seconds (although
that didn't include averaging the results of the 4 runs since the
zbrodoff-compare function doesn't return the results but that
would have only added a very small amount of time to the end).




######### Loading of ACT-R 7 is complete #########
CG-USER(2): (compile-and-load "ACT-R:tutorial;lisp;zbrodoff.lisp")
;;; Compiling file ACT-R:tutorial;lisp;zbrodoff.lisp (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.lisp)
;;; Writing fasl file ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
;;; Fasl write complete
; Fast loading ACT-R:tutorial;lisp;zbrodoff.fasl (C:\Users\db30\Desktop\actr7.x\tutorial\lisp\zbrodoff.fasl)
#|Warning: Non-ACT-R messages during load of "ACT-R:tutorial;unit4;zbrodoff-model.lisp":
; Loading
;    C:\Users\db30\Desktop\actr7.x\tutorial\unit4\zbrodoff-model.lisp

 |#
T
CG-USER(3): (time (zbrodoff-compare 4))
CORRELATION:  0.286
MEAN DEVIATION:  1.312

              2 (64)      3 (64)      4 (64)
Block  1  2.297 (64)  2.796 (64)  3.301 (64)
Block  2  2.301 (64)  2.798 (64)  3.299 (64)
Block  3  2.297 (64)  2.798 (64)  3.300 (64)
; cpu time (non-gc) 23.171875 sec user, 1.390625 sec system
; cpu time (gc)     1.937500 sec user, 0.000000 sec system
; cpu time (total)  25.109375 sec user, 1.390625 sec system
; cpu time (thread) 20.640625 sec user, 0.500000 sec system
; real time  26.443000 sec (100.2%)
; space allocation:
;  22,587,443 cons cells, -3,614,648,864 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
NIL
CG-USER(4): (clear-all)
NIL
CG-USER(5): (require-extra "parallel-models")
;;; Compiling file C:\Users\db30\Desktop\actr7.x\extras\parallel-models\parallel-models.lisp
;;; Writing fasl file C:\Users\db30\Desktop\actr7.x\extras\parallel-models\parallel-models.fasl
;;; Fasl write complete
; Fast loading C:\Users\db30\Desktop\actr7.x\extras\parallel-models\parallel-models.fasl
T
CG-USER(6): (create-parallel-instances 4)

< deleted 4 instances of compiling and loading the ACT-R code >

CG-USER(7): (instantiate-parallel-models "ACT-R:tutorial;lisp;zbrodoff.lisp")

< deleted 4 instances of compiling and loading the experiment code >

4
CG-USER(8): (time (run-parallel-models "zbrodoff-compare"  60 '(1)))
; cpu time (non-gc) 36.312500 sec user, 1.875000 sec system
; cpu time (gc)     2.171875 sec user, 0.000000 sec system
; cpu time (total)  38.484375 sec user, 1.875000 sec system
; cpu time (thread) 7.531250 sec user, 0.046875 sec system
; real time  9.734000 sec (414.6%)
; space allocation:
;  21,063,595 cons cells, 1,126,146,176 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
(NIL NIL NIL NIL)
NIL
CG-USER(9): (destroy-parallel-instances)
4
