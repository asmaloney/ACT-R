This directory contains the ACT-R tutorial.  The purpose of 
the tutorial is to provide instruction on how to create 
models using the ACT-R software.  It assumes that one has 
some familiarity with the theory of ACT-R.  Thus, it is
recommended that before starting the tutorial one has read
other background materials to become acquainted with the
theory.  

Some recommended starting places would be the paper "An 
Integrated Theory of the Mind" available from the ACT-R 
website:

http://act-r.psy.cmu.edu/?post_type=publications&p=13623

or the book:

"How Can the Human Mind Occur in the Physical Universe?".


The tutorial consists of several units, each focused on
a particular aspect of the architecture.  There is a 
separate directory for each unit which contains that unit's 
texts and ACT-R model files.  For some of the units, it is 
also necessary to run experiments for the models to perform.
Those experiments have been written in both ANSI Common Lisp
and Python 3 and are found in the lisp and python directories
of the tutorial directory.  The unit texts will be either 
Microsoft Word documents or PDF files (depending on where you
got the ACT-R files), and the model files will be text files 
with a ".lisp" extension.  The primary text for each unit will
 be named "unit#" (where # represents the current unit number).
In addition to that there may be one or two other texts for a
unit.  If there is a file named "unit#_code" that document will
describe new ACT-R commands introduced in the main unit text
and also describe the code and ACT-R commands needed to 
implement the experiments used for the models in that unit.  If
there is a file named "unit#_modeling" then that will provide 
additional information about using the ACT-R software and 
Environment tools by walking through the process of fixing a 
model which does not run as it should.
