This directory contains a speculative module which reintroduces 
the learning of the Sji values from experience instead of being 
based on just the current contents of DM.

The details of the calculation can be found on the slides in the
Anderson_ACT-R-2020-workshop.pdf.  One minor difference between
the slides and this implementation is that this module updates
the information based on all of the buffers for which the W is
non-zero when the model is defined (by default only the imaginal 
buffer does which matches the description in the slides).

To enable the new mechanism the :eal parameter must be set to the
decay value (recommended .5).  Additionally the :alp parameter
can be set to a value to represent the prior frequency value
(defaults to .1).  There is currently another parameter called
:opa which if set to t (the default) only adds one instance of
the :alp value into the calculation for F (the total history),
but if set to nil will add n instances in where n is the number
of different chunks which have been "needed" (retrieved or 
cleared from a buffer which spreads activation). 

If the :ol parameter is set to a non-nil value then the optimized
learning equation will be used in computing the F(x) values.

There are two simulations of the fan experiment similar to the
model in unit 5 of the tutorial included in the al-fan-study.lisp
and al-fan-history.lisp files.  Those files determine the data
based on the activations of the sentence chunks after the items
have been studied (much like the unit 5 model which performs
one retreival of each item).  The al-fan-study version assumes no
prior history with either the probes or target sentences (like
the unit 5 model) and implements the study phase as one initial
presentation of each sentence and then two passes through a
drop-out phase which assumes the model always responds correctly.
The al-fan-history version is set up to allow one to specify the
details of the past histories of the items and some details of 
the study phase.  For both files, that is all performed in code
instead of writing the productions to do the task.  Details can 
be found in the comments of those files.

