import json
import threading
import socket
import time
import os

current_connection = None

class evaluator():
    def __init__(self,id):
        self.id = id
        self.lock = threading.Lock()
        self.cv = threading.Condition(self.lock)
        self.complete = False

    def notify_result(self):
        self.cv.acquire()
        self.complete = True
        self.cv.notify()
        self.cv.release()


class actr():
    
    def __init__(self,host="127.0.0.1",port=2650):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((host, port))
        self.connected = True
        self.cmd_id = 1
        self.actions = {}
        self.stream_lock = threading.Lock() 
        self.buffer = []
        self.commands = {}
        self.data_collector = threading.Thread(target=self.collect_data)
        self.data_collector.daemon = True
        self.data_collector.start()       
        self.id_lock = threading.Lock()
        self.echo_count = 0
        self.echo = False
        self.show_output = True

    def send(self,method,*params):
        d = {}
        r = evaluator(self.cmd_id)
        self.actions[self.cmd_id] = r

        d['method'] = method
        self.id_lock.acquire()
        d['id'] = self.cmd_id
        self.cmd_id += 1
        self.id_lock.release()
        d['params'] = params
        
        message = json.dumps(d) + chr(4)
        
        r.lock.acquire()
        
        self.stream_lock.acquire()
        self.sock.sendall(message.encode('utf-8'))
        self.stream_lock.release()
        
        while not r.complete:
          r.cv.wait()

        return [r.success] + r.results

    def collect_data(self):
        buffer= ''
        c = True
        while c:
            try:
                data = self.sock.recv(4096)
                buffer += data.decode('utf-8')
                while not chr(4) in buffer:
                    data = self.sock.recv(4096)
                    buffer += data.decode('utf-8')
                while chr(4) in buffer:
                    pos = buffer.find(chr(4))
                    message = buffer[0:pos]
                    pos += 1
                    buffer = buffer[pos:]
                    self.process_message(json.loads(message))
            except:
                if self.connected:
                    print("ACT-R connection error connection no longer available.")
                c = False

    def process_message (self,d):
        if 'result' in d.keys():
            id =d['id']
            r = self.actions[id]
            if d['error'] is None:
                r.success = True
                r.results = d['result']
            else:
                r.success = False
                errors=d['error']
                r.results = [errors['message']]

            self.actions.pop(id,None)
            r.notify_result()
        else:
            if d['method'] == "evaluate":
             
                thread = threading.Thread(target=self.run_command,args=[d['params'][0],d['id'],d['params'][1:]])
                thread.daemon = True
                thread.start()
            else:
                f={}
                f['id'] = d['id']
                f['result'] = None
                e={}
                e['message'] = "Invalid method name" + d['params'][0]
                f['error'] = e
                message = json.dumps(f) + chr(4)
                self.stream_lock.acquire()
                self.sock.sendall(message.encode('utf-8'))
                self.stream_lock.release()

    def run_command (self,command,id,params):

        if command in self.commands.keys():

            try:
                c = self.commands[command]
                result = c(*params)
            except:
                error = True
            else:
                error = None
        else:
            print("Evaluate received for %s which isn't a currently added command"%command)
            error = True

        f={}
        f['id'] = id

        if error:
            f['result'] = None
            f['error'] = {'message': "Error during run_command for %s which maps to %s with %s"%(command,self.commands[command],params)}

        elif result:
            f['result']= result
            f['error']= None
        else:
            f['result']= [None]
            f['error']= None

        message = json.dumps(f) + chr(4)
        self.stream_lock.acquire()
        self.sock.sendall(message.encode('utf-8'))
        self.stream_lock.release()

    def evaluate (self, *params):

        r = self.send ("evaluate", *params)
        
        if r[0] == False:
            for e in r[1:]:
                print (e)

            return False
        else:
            return r[1]

    def add_command(self,name,function,documentation="No documentation provided.",single=True,actr_name=None):

        if name not in self.commands.keys():
            self.commands[name] = function
        elif self.commands[name] == function:
            print("Command ",name," already exists for function ",function)
        else:
            print("Command ",name," already exists and is now being replaced by ",function)
            self.commands[name] = function

        existing = self.send("check",name)

        if existing[0] == True:
            if existing[1] == None:
                result = self.send("add",name,name,documentation,single,actr_name)
                if result[0]:
                    return result[1]
                else:
                    return False
            elif existing[2] == None:
                print("Cannot add command ",name, " because it has already been added by a different owner.")
                return False
            else:
                return True
        
        else:
            print("Invalid command name ",name," cannot be added.")
            return False


    def remove_command(self,name):

        if name in self.commands.keys():
            del self.commands[name]

        r = self.send('remove',name)

        if r[0] == False:
            for e in r[1:]:
                print (e)

            return False
        else:
            return True



def start (host="127.0.0.1",port=2650):

    global current_connection

    if current_connection == None: 
        current_connection = actr(host, port)
        return current_connection
    else:
        print("ACT-R is already connected.")
        return current_connection

def connection ():
    port = None
    host = None
    connect = None
    if current_connection == None:
        if os.path.isfile(os.path.expanduser("~/act-r-port-num.txt")):
            with open(os.path.expanduser("~/act-r-port-num.txt"), 'r') as f:
                try:
                    port = int(f.readline())
                except:
                    print("Problem reading ACT-R port number from ~s. Using default or 2650."%os.path.expanduser("~/act-r-port-num.txt"))
                    port = 2650
        else:
            port = 2650

        if os.path.isfile(os.path.expanduser("~/act-r-address.txt")):
            with open(os.path.expanduser("~/act-r-address.txt"), 'r') as f:
                try:
                    host = f.readline()
                except:
                    print("Problem reading ACT-R host from ~s. Using default of 127.0.0.1."%os.path.expanduser("~/act-r-address.txt"))
                    host = "127.0.0.1"
        else:
            host = "127.0.0.1"
        
        try:
            connect = start(host,port)
        except:
            print("Problem connecting to ACT-R at ~s:~s."%(host,port))
            return None
        else:
            return connect
    else:
        return current_connection

def stop():

    global current_connection

    if current_connection == None:
        print("No current ACT-R connection to stop.")
    else:
        print("Closing down ACT-R connection.")
        current_connection.connected = False
        current_connection.sock.close()
        current_connection = None


actr = connection()


actr.evaluate('undefine-module',None,'goal')
actr.remove_command('goal-focus')
actr.remove_command('mod-focus')


goal_modules = {}

class goal_module():

    def __init__(self):
        self.lock = threading.Lock()
        self.delayed = None

    def reset(self,model):
        self.lock.acquire()
        self.delayed = None
        actr.evaluate('set-parameter-value',model,':do-not-harvest','goal')
        self.lock.release()

        
def create (model,name):
    global goal_modules

    goal_modules[name.lower()] = goal_module()
    return name

actr.add_command('create_pgoal',create,'Creation function for Python goal module.')


def delete (model,name):
    global goal_modules

    del goal_modules[name.lower()] 

actr.add_command('delete_pgoal',delete,'Deletion function for Python goal module.')


def reset (model,name):
    goal_modules[name.lower()].reset(model)

actr.add_command('reset_pgoal',reset,'Reset function for Python goal module')


def query (model,name,buffer,slot,value):
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
            actr.evaluate('print-warning',model,'Unknown state query %s to goal module' % v)
            return False
    else:
        actr.evaluate('print-warning',model,'Unknown query %s %s to goal module' % (s,v))
        return False

actr.add_command('query_pgoal',query,'Query function for Python goal module')


def request (model,name,buffer,spec):
    chunk_desc = actr.evaluate('chunk-spec-to-chunk-def',model,spec)
    
    if chunk_desc:
        actr.evaluate('schedule-set-buffer-chunk',model,'goal',spec,0,[['module','goal'],['priority',-1000]])
        actr.evaluate('schedule-event-now',model,'release-chunk-spec-id',[['params',[spec]],['module','goal'],['priority',-1001],['output',False]])

    else:
        actr.evaluate('print-warning',model,'Invalid request made to the goal buffer.')

actr.add_command('request_pgoal',request,'Request function for Python goal module')

def buffer_mod(model,name,buffer,spec):
    actr.evaluate('schedule-mod-buffer-chunk',model,buffer,spec,0,[['module','goal'],['priority',20]])
    actr.evaluate('schedule-event-now',model,'release-chunk-spec-id',[['params',[spec]],['module','goal'],['priority',19],['maintenance',True]])        

actr.add_command('buffer_mod_pgoal',buffer_mod,'Buffer modification request function for Python goal module')


actr.evaluate('define-module',None,'goal',[['goal',[':ga',0]]],None,[['version','1.1P'],['documentation','Replaced normal goal module with a remote Python version.'],
              ['creation','create_pgoal'],['delete','delete_pgoal'],['query','query_pgoal'],['request','request_pgoal'],
              ['buffer-mod','buffer_mod_pgoal'],['reset',[None,'reset_pgoal']]])


def goal_focus (model,name=None):

    if not(model):
        actr.evaluate('print-warning',model,'Goal-focus called with no current model.')
    else:
        module = goal_modules[model.lower()]

        if not(module):
            actr.evaluate('print-warning',model,'No goal module found for model %s when trying to use goal-focus.'%model)
        elif name:
            if actr.evaluate('chunk-p',model,name):

                actr.evaluate('schedule-set-buffer-chunk',model,'goal',name,0,[['module','goal'],['priority',':max'],['requested',False]])
                actr.evaluate('schedule-event-after-module',model,'goal','clear_delayed_goal',[['module','goal'],['output',False],['maintenance',True]])
              
                module.lock.acquire()
                module.delayed = name
                module.lock.release()
                return name

            else:
                actr.evaluate('print-warning',model,'%s is not the name of a chunk in the current model - goal-focus failed' % name)

        else:
            chunk = actr.evaluate('buffer-read',model,'goal')
            module.lock.acquire()
            delayed = module.delayed
            module.lock.release()

            if not(chunk) and not(delayed):
                actr.evaluate('command-output',model,'Goal buffer is empty')
                return None
            elif not(chunk):
                actr.evaluate('command-output',model,'Will be a copy of %s when the model runs' % delayed)
                actr.evaluate('pprint-chunks',model,delayed)
                return delayed
            elif not(delayed):
                actr.evaluate('pprint-chunks',model,chunk)
                return chunk
            else:
                copy = actr.evaluate('chunk-copied-from',model,chunk)

                if copy and (copy.lower() == delayed.lower()):

                    actr.evaluate('pprint-chunks',model,chunk)
                    return chunk
                else:
                    actr.evaluate('command-output',model,'Will be a copy of %s when the model runs' % delayed)
                    actr.evaluate('command-output',model,'Currently holds:')
                    actr.evaluate('pprint-chunks',model,chunk)
                    return delayed

actr.add_command('goal-focus',goal_focus,'Python version which schedule a chunk to enter the goal buffer at the current time or print the current goal buffer chunk. Params: {chunk-name}.',None,'goal-focus')


def clear_delayed_goal (model):
    module = goal_modules[model.lower()]

    if not(module):
        actr.evaluate('print-warning',model,'No goal module found for model %s when trying to clear delayed goal.'%model)
    else:
        module.lock.acquire()
        module.delayed = None
        module.lock.release()

actr.add_command('clear_delayed_goal',clear_delayed_goal,'Event for letting the goal module know a goal-focus has completed. Do not call directly.')


def mod_focus (model,*modifications):

    if not(model):
        actr.evaluate('print-warning',model,'Mod-focus called with no current model.')
    else:
        module = goal_modules[model.lower()]

        if not(module):
            actr.evaluate('print-warning',model,'No goal module found for model %s when trying to use mod-focus.'%model)
        else:
            chunk = actr.evaluate('buffer-read',model,'goal')
            module.lock.acquire()
            delayed = module.delayed
            module.lock.release()

            if chunk or delayed:
                actr.evaluate('schedule-mod-buffer-chunk',model,'goal',modifications,0,[['module','goal'],['priority',':max']])
                if delayed:
                    return delayed
                else:
                    return chunk
            else:
                actr.evaluate('print-warning',model,'No chunk currently in the goal buffer and no pending goal-focus chunk to be modified.')


actr.add_command('mod-focus',mod_focus,'Modify the chunk in the goal buffer using the slots and values provided. Params: {<slot-name> <new-slot-value>}*',None,'mod-focus')

              