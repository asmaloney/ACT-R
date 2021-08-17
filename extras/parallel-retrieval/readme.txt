This extra is not really usable at this time, but is being
kept around for reference and may lead to future updates of
the main code.


In the older versions of ACT-R it provided parallel functions
for finding chunks, computing activations, and parts of blending.
Those rarely provided any improvement to performance, and it turns
out they only really worked because the older systems weren't 
thread safe i.e. multiple threads could try to read and write
to the same underlying locations simultaneously (like chunk
parameters).

The find-matching-chunks update was implemented, but still
doesn't provide much in the way of a performance improvement
and does require some extra blocking -- effectively it
blocks anything that tries to access chunks in a model where
it's running.  The activation calculations however cannot be
done in parallel with the current thread safety mechanisms
without a complete rewrite of the underlying code.