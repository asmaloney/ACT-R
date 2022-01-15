# Implement a replacement declartive memory module which provides
# a very small subset of the standard version that is sufficient
# to run the unit 1 models as an example.
# Also uses some ACT-R tools (matching chunks and a merge function
# for the parameters) to make the implementation simpler.

# Depends on the actr interface from the tutorial.

import actr
import threading

# Remove the default module and the commands it creates which
# are being redefined.

actr.undefine_module('declarative')

actr.remove_command('add-dm')
actr.remove_command('dm')
actr.remove_command('sdp')

# To support multiple models keep a dictionary of model name to module instances

declarative_modules = {}

def current_module(name):
    return declarative_modules[name.lower()]


# A simple class to hold the module info

class declarative_module():

    def __init__(self):
        self.lock = threading.Lock() # To avoid potential issues always lock access when changing things
        self.chunks = []             # a simple list of all the chunks
        self.pending = False         # any event for an ongoing retrieval/failure
        self.error = False           # did the last request fail
        self.lf = 1                  # value of :lf parameter
        self.esc = False             # is the :esc parameter set
        

    def reset(self):
        self.lock.acquire()
        self.chunks = []
        self.pending = False
        self.error = False
        self.lock.release()


# Functions and ACT-R commands for the scheduled events to handle retrievals

def retrieved_chunk(name,chunk):
    
    module = current_module(name)

    module.lock.acquire()
    module.pending = False
    actr.schedule_set_buffer_chunk("retrieval",chunk,0,"declarative",":max")
    module.lock.release()

actr.add_command('retrieved-chunk',retrieved_chunk,'Python declarative module successful retrieval event.')


def retrieval_failure(name):

    module = current_module(name)

    module.lock.acquire()
    module.pending = False
    module.error = True
    actr.call_command("set-buffer-failure","retrieval")
    module.lock.release()

actr.add_command('retrieval-failure',retrieval_failure,'Python declarative module unsuccessful retrieval event.')

actr.add_command('start-retrieval',None,'Signal for the start of a retrieval request')



# Module interface functions

def create (name):
    global declarative_modules

    declarative_modules[name.lower()] = declarative_module()
    return name

actr.add_command('create-pDM',create,'Creation function for Python declarative module.')


def delete (name):
    global declarative_modules

    del declarative_modules[name.lower()] 

actr.add_command('delete-pDM',delete,'Deletion function for Python declarative module.')


def reset (name):
    current_module(name).reset()

actr.add_command('reset-pDM',reset,'Reset function for Python declarative module')


def query(name,buffer,slot,value):

    module = current_module(name)
    s = slot.lower()

    if hasattr(value,'lower'):
        v = value.lower()
    else:
        v = value
        
    module.lock.acquire()
    pending = module.pending
    error = module.error
    module.lock.release()

    if s == 'state':
        if v == 'free':
            return not(pending)
        elif v == 'busy':
            return pending
        elif v == 'error':
            return error
        else:
            actr.print_warning('Unknown state query %s to declarative module' % v)
            return False
    else:
        actr.print_warning('Unknown query %s %s to declarative module' % (s,v))
        return False

actr.add_command('query-pDM',query,'Query function for Python declarative module')


def request (name,buffer,spec):

    module = current_module(name)

    module.lock.acquire()

    module.error = False

    if module.pending :
        actr.call_command("model_warning","A retrieval event has been aborted by a new request")
        actr.call_command("delete-event",module.pending)
        module.pending = False


    actr.schedule_event_now("start-retrieval",None,"declarative",output="medium")

    # Let ACT-R find the match(es) from all the DM chunks

    match = actr.call_command("find-matching-chunks",spec,module.chunks)

    # if :esc is t then :lf controls the timing

    if module.esc :
        time = module.lf
    else:
        time = 0

    if match :
        module.pending = actr.schedule_event_relative(time,"retrieved-chunk",[match[0]],"declarative",destination="declarative",output="medium")
    else:
        module.pending = actr.schedule_event_relative(time,"retrieval-failure",None,"declarative",destination="declarative",output="medium")

    # should always free the chunk-spec resources when no longer needed

    actr.release_chunk_spec(spec)

    module.lock.release()

actr.add_command('request-pDM',request,'Request function for Python declarative module')


def params (name,param):

    module = current_module(name)

    module.lock.acquire()

    r = None

    if len(param) == 2:                 # a new value for a parameter
        if param[0].lower() == ":lf" :
            module.lf = param[1]
            r = param[1]
        elif param[0].lower() == ":esc" :
            module.esc = param[1]
    else:
        if param[0].lower() == ":lf" :
            r = module.lf    

    module.lock.release()

    return r

actr.add_command('params-pDM',params,'Parameter function for Python declarative module')


def buffer_cleared(name,buffer,chunk):

    module = current_module(name)

    module.lock.acquire()

    # create a chunk-spec that describes the chunk and use ACT-R's matching
    # to search the list of chunks for a match

    spec = actr.define_chunk_spec(chunk)
    match = actr.call_command("find-matching-chunks",spec,module.chunks)
    actr.release_chunk_spec(spec)

    if match : # merge it with the first one found

        actr.call_command("merge-chunks",match[0],chunk)

    else:  # otherwise check if we can store it

        if actr.call_command("chunk-not-storable",chunk) :

            # can't store it so store a copy

            copy = actr.copy_chunk(chunk)
            add_chunk_to_dm(module,copy)

        else :

            # safe to store it directly 

            add_chunk_to_dm(chunk)


    module.lock.release()

actr.add_command('cleared-pDM',buffer_cleared,'Chunk cleared from a buffer function for Python declarative module')


# Define the module with a buffer called retrieval, add the :lf parameter, monitor the :esc parameter,
# and get notified when a buffer is cleared.

actr.define_module('declarative',
                   [['retrieval']],
                   [[':lf', [['owner',True],['valid-test','nonneg'], # use an internal ACT-R function to test it
                             ['default-value',1.0],['warning','non-negative number'],
                             ['documentation','Latency Factor (from Python)']]],
                    [':esc',[['owner',False]]]],
                   [['version','1.1P'],
                    ['documentation','Simple (limited functionality) test of remote Python version of declarative module.'],
                    ['creation','create-pDM'],['delete','delete-pDM'],['query','query-pDM'],['request','request-pDM'],
                    ['notify-on-clear','cleared-pDM'],['reset','reset-pDM'],
                    ['params','params-pDM']])



# When a chunk enters DM make it immutable, set its initial parameters for creation time
# and reference count, and add it to the list of chunks.
# This assumes the module's lock is already held.

def add_chunk_to_dm (module,chunk):

    actr.call_command("make-chunk-immutable",chunk)

    actr.call_command("set-chunk-ct",chunk,actr.get_time())
    actr.call_command("set-chunk-count",chunk,1)

    module.chunks.append(chunk)


# Function to update the reference count automatically when chunks are merged.

def merge_chunk_counts(c1,c2):
    return(c1 + 1)

actr.add_command('merge-py-chunks',merge_chunk_counts,'Increment the reference count for merged chunks.')

# Create the new chunk parameters for creation time and reference count
    
actr.call_command("extend-chunks","ct")
actr.call_command("extend-chunks","count",[["merge-value-function","'merge-py-chunks'"]])


# This function is going to be callable from within productions just
# like the original ACT-R add-dm command.
#
# If there is a current model, pass the parameters off to define-chunks
# to create them and then add all of the created chunks into DM and
# return the names of those chunks.

def add_dm(*chunk_defs):

    model = actr.current_model()

    if not(model):
        actr.print_warning('Add-dm called with no current model.')
        return(False)

    else:
 
        chunks=actr.define_chunks(*chunk_defs)
  
        module = current_module(model)

        module.lock.acquire()

        for c in chunks:
          add_chunk_to_dm(module,c)

        module.lock.release()

        return (chunks)

actr.add_command('add-dm',add_dm,'Define chunks and add them to the Python declarative memory.',True,"add-dm",True)


# This replaces the original ACT-R dm command which can be used
# to print out all the chunks in DM or a specified subset of them.

def dm(*chunks):

    model = actr.current_model()

    if not(model):
        actr.print_warning('Dm called with no current model.')
        return(False)

    else:
        module = current_module(model)

        module.lock.acquire()

        current_chunks=module.chunks

        module.lock.release()

        if chunks == ():
            actr.pprint_chunks(*current_chunks)
            return(current_chunks)
        else:
            r = []

            for c in chunks:
                if c in current_chunks:
                    actr.pprint_chunks(c)
                    r.append(c)
            return (r)

actr.add_command('dm',dm,'Print chunks from Python declarative memory.',True,"dm")


# Replace the original ACT-R sdp command with this simplified version
# which only serves to print the parameters for the indicated DM chunks.

def sdp(*chunks):

    model = actr.current_model()

    if not(model):
        actr.print_warning('Sdp called with no current model.')
        return(False)

    else:
        module = current_module(model)

        module.lock.acquire()

        current_chunks=module.chunks

        module.lock.release()

        r = []

        if chunks == ():

            for c in current_chunks:
                ct = actr.call_command("chunk-ct",c)
                count = actr.call_command("chunk-count",c)
                actr.command_output("Paramters for chunk %s created: %d current count: %d" % (c,ct,count))
                r.append([c,ct,count])

        else:
            for c in chunks:
                if c in current_chunks:

                    ct = actr.call_command("chunk-ct",c)
                    count = actr.call_command("chunk-count",c)
                    actr.command_output("Paramters for chunk %s created: %d current count: %d" % (c,ct,count))
                    r.append([c,ct,count])

        return (r)

actr.add_command('sdp',sdp,'Print chunk parameters for chunks in Python declarative memory.',True,"sdp")


