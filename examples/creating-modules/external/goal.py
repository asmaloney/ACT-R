import actr
import threading


actr.undefine_module('goal')

actr.remove_command('goal-focus')
actr.remove_command('mod-focus')

goal_modules = {}

class goal_module():

    def __init__(self):
        self.lock = threading.Lock()
        self.delayed = None

    def reset(self):
        self.lock.acquire()
        self.delayed = None
        actr.set_parameter_value(':do-not-harvest','goal')
        self.lock.release()

        
def create (name):
    global goal_modules

    goal_modules[name.lower()] = goal_module()
    return name

actr.add_command('create-pgoal',create,'Creation function for Python goal module.')


def delete (name):
    global goal_modules

    del goal_modules[name.lower()] 

actr.add_command('delete-pgoal',delete,'Deletion function for Python goal module.')


def reset (name):
    goal_modules[name.lower()].reset()

actr.add_command('reset-pgoal',reset,'Reset function for Python goal module')


def query(name,buffer,slot,value):
    s = slot.lower()
    if hasattr(value,'lower'):
        v = value.lower()
    else:
        v = value
        
    if s == 'state':
        if v == 'free':
            return True
        elif v == 'busy':
            return False
        elif v == 'error':
            return False
        else:
            actr.print_warning('Unknown state query %s to goal module' % v)
            return False
    else:
        actr.print_warning('Unknown query %s %s to goal module' % (s,v))
        return False

actr.add_command('query-pgoal',query,'Query function for Python goal module')

def request (name,buffer,spec):
    chunk_desc = actr.chunk_spec_to_chunk_def(spec)
    
    if chunk_desc:
        actr.schedule_set_buffer_chunk('goal',spec,0,module='goal',priority= -1000)
        actr.schedule_event_now('release-chunk-spec-id',params=[spec],module='goal',priority= -1001,output=False)
    else:
        actr.print_warning('Invalid request made to the goal buffer.')

actr.add_command('request-pgoal',request,'Request function for Python goal module')


def buffer_mod(name,buffer,spec):
    actr.schedule_mod_buffer_chunk(buffer,spec,0,module='goal',priority=20)
    actr.schedule_event_now('release-chunk-spec-id',params=[spec],module='goal',priority=19,maintenance=True)        

actr.add_command('bmod-pgoal',buffer_mod,'Buffer modification request function for Python goal module')


actr.define_module('goal',[['goal',[':ga',0]]],None,[['version','1.1P'],['documentation','Replaced normal goal module with a remote Python version.'],
                   ['creation','create-pgoal'],['delete','delete-pgoal'],['query','query-pgoal'],['request','request-pgoal'],
                   ['buffer-mod','bmod-pgoal'],['reset',[None,'reset-pgoal']]])


def goal_focus (name=None):
    model = actr.current_model()

    if not(model):
        actr.print_warning('Goal-focus called with no current model.')
    else:
        module = goal_modules[model.lower()]

        if not(module):
            actr.print_warning('No goal module found for model %s when trying to use goal-focus.'%model)
        elif name:
            if actr.chunk_p(name):

                actr.schedule_set_buffer_chunk('goal',name,0,module='goal',priority=':max',requested=False)
                actr.schedule_event_after_module('goal','clear-delayed-goal',params=[model],module='goal',maintenance=True,output=False)
              
                module.lock.acquire()
                module.delayed = name
                module.lock.release()
                return name

            else:
                actr.print_warning('%s is not the name of a chunk in the current model - goal-focus failed' % name)

        else:
            chunk = actr.buffer_read('goal')
            module.lock.acquire()
            delayed = module.delayed
            module.lock.release()

            if not(chunk) and not(delayed):
                actr.command_output('Goal buffer is empty')
                return None
            elif not(chunk):
                actr.command_output('Will be a copy of %s when the model runs' % delayed)
                actr.pprint_chunks(delayed)
                return delayed
            elif not(delayed):
                actr.pprint_chunks(chunk)
                return chunk
            else:
                copy = actr.chunk_copied_from(chunk)

                if copy and (copy.lower() == delayed.lower()):

                    actr.pprint_chunks(chunk)
                    return chunk
                else:
                    actr.command_output('Will be a copy of %s when the model runs' % delayed)
                    actr.command_output('Currently holds:')
                    actr.pprint_chunks(chunk)
                    return delayed

actr.add_command('goal-focus',goal_focus,'Python version which schedule a chunk to enter the goal buffer at the current time or print the current goal buffer chunk. Params: {chunk-name}.',None,'goal-focus')

def clear_delayed_goal (model):
    module = goal_modules[model.lower()]

    if not(module):
        actr.print_warning('No goal module found for model %s when trying to clear delayed goal.'%model)
    else:
        module.lock.acquire()
        module.delayed = None
        module.lock.release()

actr.add_command('clear-delayed-goal',clear_delayed_goal,'Event for letting the goal module know a goal-focus has completed. Do not call directly.')


def mod_focus (*modifications):
    model = actr.current_model()

    if not(model):
        actr.print_warning('Mod-focus called with no current model.')
    else:
        module = goal_modules[model.lower()]

        if not(module):
            actr.print_warning('No goal module found for model %s when trying to use mod-focus.'%model)
        else:
            chunk = actr.buffer_read('goal')
            module.lock.acquire()
            delayed = module.delayed
            module.lock.release()

            if chunk or delayed:
                actr.schedule_mod_buffer_chunk('goal',modifications,0,module='goal',priority=':max')
                if delayed:
                    return delayed
                else:
                    return chunk
            else:
                actr.print_warning('No chunk currently in the goal buffer and no pending goal-focus chunk to be modified.')


actr.add_command('mod-focus',mod_focus,'Modify the chunk in the goal buffer using the slots and values provided. Params: {<slot-name> <new-slot-value>}*',None,'mod-focus')

              
# One minor note on this is that since the commands are on the Python side
# the current model needs to be explicitly set with actr.set_current_model 
# to be able to use the actr.goal_focus & actr.mod_focus commands since the
# dispatcher doesn't fill in a current model when a remote system sends the
# command to someone other than the internal code.  