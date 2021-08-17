This directory contains the code to implement a modification to the activation
calculation for declarative retrievals.  The purpose of this modifiction is to
decrease the effect of noise on the activation of chunks as their activation 
increases with practice.

To use this extension this line should be placed in the model file after the
call to clear-all:

(require-extra "adaptive-noise")

Then, setting the :uan parameter to t in the model will enable the change.

When enabled the equations for activations changes from:

 B + S + P + n1 + n2
 
  B = base-level activation
  S = spreading activation
  P = partial matching penalty
  n1 = instantaneous noise (:ans)
  n2 = permanent noise (:pas) 

to this:

 B' + S + P + n2

 where B' is set with this equation
 
            n
 B' = ln( [sum t[i]^-d)] + exp(n1))
           i=1
 
 or the appropriate approximation when optimized learning is enabled.

The change is to move the instantaneous noise inside the log with the 
base level computation.

The noise needs to be exponentiated to avoid the possibility of a negative
value inside the log calculation.  That effectively makes it a log-logistic
noise distribution instead of a logistic noise distribution, and that means
the final activation will always be greater than just the base-level unlike
the standard activation calculation.  Therefore, the retrieval threshold
and latency factor parameters will need to be adjusted for a model relative
to what they would be if the model were to be using the standard calculation,
but since activations don't have a natural zero it is effectively just shifting
the scale.  The median value of the noise component will be 1, and the mean
is only defined if :ans < 1 in which case it is: 

    pi * :ans
   -----------
  sin (pi * :ans)

If base-level learning is not enabled then enabling this change does not 
affect the system since the base-level component will be equal to the noise
component that would be added without this change.

--------------------------------------------------------------------------------

There are several files included with this extra:

  - readme.txt : this file
  - adaptive-noise.lisp : the file that defines the module which implements the
                          updated activation calculation
  - test-model.lisp : a file which defines a simple model that can perform
                      retrievals between two competing chunks.  This can be
                      used to compare the results between using this extra and
                      using the normal activation equation.
  - example-data : a directory containg text files of the results of running
                   the test-model with and without uan enabled in both the
                   case where the items have equal rehearsals and a case where
                   the retrieval itself adds an extra reference to only the
                   retrieved item, and a spreadsheet with graphs of those data.
                   For all of the tests a sequence of 300 retrievals were
                   performed with each item having 2 rehearsals between the
                   retrieval requests and the results were averaged over 5000 
                   runs of the task.
                   For each of the files listed below the * can be default, 
                   uan, reinforce, and reinforce-uan to indicate the condition
                   it represents.

  - example-data/*-choice.txt : the proportion of retrievals of the target
                                for each of the 300 retrieval requests (in
                                order from first to last), one per line.

  - example-data/*-activation.txt : the average activation of the chunk which
                                    was retrieved on each of the 300 requests,
                                    one per line.

  - example-data/*-latency.txt : the distribution of latency times for the
                                 first and last retrieval request.  The times
                                 are split into 101 bins and each line has 2
                                 pairs of 2 values separated by commas with 
                                 an additional empty value between the pairs.
                                 The first pair is the items for the first
                                 retrieval and the second pair is the items
                                 from the last retrieval.  The items in a 
                                 pair are the lower bound for bin and the
                                 count of items in that bin.
  - example-data/results.ods : a spreadsheet containing the data from those
                               text files along with graphs to compare the
                               differences.

                                 