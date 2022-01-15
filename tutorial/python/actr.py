"""
This file implements a connection to the ACT-R remote interface
and defines functions which can call the ACT-R commands that are
used in the tasks from the ACT-R tutorial.

The call_command function can be used to call ACT-R commands
for which a corresponding function has not been provided.

It is not "the" ACT-R interface in Python.  It is only an interface
which is sufficient for using the ACT-R tutorial tasks from Python.

There are some assumptions about how the connection is implemented
and processed which may not be suitable for other purposes.  Also,
a simpler interface may be more useful in other cases when speed of
operation is important.

There is an example of a simpler interface that implements only a 
specific set of commands being made available to ACT-R in the file:

examples/creating-modules/external/goal_complete.py

There are of course many other ways one could also handle the
communication process.

"""

import json
import threading
import socket
import time
import os
import sys
import __main__
import imp


current_connection = None

class request():
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


locals = threading.local()

class actr():
    
    
    def __init__(self,host,port):
        self.interface = interface(host, port)
        if self.interface.connected :
            self.interface.echo_output()

    def evaluate (self, *params):
        
        try:
            m = locals.model_name
        except AttributeError:
            m = False
     
        p = list(params)

        p.insert(1,m)    

        r = self.interface.send ("evaluate", *p)
        
        if r[0] == False:
            for e in r[1:]:
                print (e)

            return False
        else:
            return r[1:]

    def evaluate_single(self,*params):
        r = self.evaluate(*params)

        if r:
            return r[0]
        else:
            return False

    def add_command(self,name,function,documentation="No documentation provided.",single=True,actr_name=None,encoded=False):
        if name in self.interface.commands.keys():
            if self.interface.commands[name] == function:
                print("Command ",name," already exists for function ",function)
            else:
                print("Command ",name," already exists and is now being replaced by ",function)
                self.interface.add_command(name,function)
 
        existing = self.interface.send("check",name)

        if function :
            call_name = name
        else:
            call_name = None

        if existing[0] == True:
            if existing[1] == None:
                result = self.interface.send("add",name,call_name,documentation,single,actr_name,encoded)
                if result[0]:
                    self.interface.add_command(name,function)
                    return result[1]
                else:
                    print(result[1])
                    return False
            elif existing[2] == None:
                print("Cannot add command ",name, " because it has already been added by a different owner.")
                return False
            else:
                return True
        
        else:
            print("Invalid command name ",name," cannot be added.")
            return False




    def monitor_command(self,original,monitor):
        r = self.interface.send("monitor",original,monitor)

        if r[0] == False:
            for e in r[1:]:
                print (e)

            return False
        else:
            return r[1:]

 
    def remove_command_monitor(self,original,monitor):
        r = self.interface.send("remove-monitor",original,monitor)

        if r[0] == False:
            for e in r[1:]:
                print (e)

            return False
        else:
            return r[1:]       

    def remove_command(self,name):
        if name not in self.interface.commands.keys():
            r = self.interface.send('remove',name)

            if r[0] == False:
                for e in r[1:]:
                    print (e)

                return False
            else:
                return True

        else:
            del self.interface.commands[name]
            r = self.interface.send("remove",name)
            
            if r[0] == False:
                for e in r[1:]:
                    print (e)

                return False
            else:
                return True


def start (host=None,port=None):

    global current_connection

    if current_connection == None: 
        portfile = os.path.join(os.path.expanduser("~"),"act-r-port-num.txt")
        hostfile = os.path.join(os.path.expanduser("~"),"act-r-address.txt")

        if port == None and os.path.isfile(portfile):
            with open(portfile, 'r') as f:
                try:
                    port = int(f.readline())
                except:
                    print("Problem reading ACT-R port number from",portfile,". Using default or 2650.")
                    port = 2650
        elif port == None:
            print("ACT-R port number file",portfile,"not found. Using default or 2650.")
            port = 2650

        if host == None and os.path.isfile(hostfile):
            with open(hostfile, 'r') as f:
                try:
                    host = f.readline()
                except:
                    print("Problem reading ACT-R host from",hostfile,". Using default of 127.0.0.1.")
                    host = "127.0.0.1"
        elif host == None:
            print("ACT-R host file",hostfile,"not found. Using default of 127.0.0.1.")
            host = "127.0.0.1"


        try:
            a = actr(host=host,port=port)
        except:
            print("Failed to connect to ACT-R with exception",sys.exc_info())
         
        else:   
            if a.interface.connected :
                a.interface.send("set-name","ACT-R Tutorial Python interface")
                current_connection = a
                return current_connection
            else:
                print("ACT-R connection NOT established, but no exception detected or already handled.")
    else:
        print("ACT-R is already connected.")
        return current_connection

def connection ():

    if current_connection == None:
        s = start()
        if s :
            print("ACT-R connection has been started.")
            return s
        else:
            print("Could not start ACT-R connection.")
    else:
        return current_connection

def stop():

    global current_connection

    if current_connection == None:
        print("No current ACT-R connection to stop.")
    else:
        print("Closing down ACT-R connection.")
        current_connection.interface.connected = False
        current_connection.interface.sock.close()
        current_connection = None

class interface():
    def __init__(self,host,port):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:

            self.sock.connect((host, port))
        except:
            self.connected = False
            print("Error trying to connect to ACT-R at",host,":",port,"with exception",sys.exc_info())
        else:
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
        r = request(self.cmd_id)
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


    def add_command(self,name,function):
        self.commands[name] = function

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
            if d['method'] == "evaluate" and d['params'][0] in self.commands.keys():
                thread = threading.Thread(target=self.run_command,args=[self.commands[d['params'][0]],d['params'][0],d['params'][1],d['id'],d['params'][2:]])
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

    def run_command (self,command,command_name,model,id,params):

        locals.model_name = model

        try:
            if command:
                if params == None:
                    result = command()
                else:
                    result = command(*params)
            else:
                result = True
        except:
            error = True
            problem = sys.exc_info()
        else:
            error = None

        f={}
        f['id'] = id

        if error:
            f['result'] = None
            f['error'] = {'message': "Error %s while evaluating a command in Python for command: %s, model: %s, parameters: %s"%(problem,command_name,model,params)}

        elif ((result is False) or (result is None)):

            f['result']= [None]
            f['error']= None

        else:
            if isinstance(result,tuple):
                f['result']= result
            else:
                f['result']= [result]
            f['error']= None

        message = json.dumps(f) + chr(4)
        self.stream_lock.acquire()
        self.sock.sendall(message.encode('utf-8'))
        self.stream_lock.release()
        
    def output_monitor(self,string):
        if self.show_output:
            print(string.rstrip())
        return True

    def echo_output(self):
        if not(self.echo): 
            if 'echo' not in self.commands.keys():
                self.add_command("echo",self.output_monitor)

            ready = False

            while not(ready):
                existing = self.send("check",'python-echo'+str(self.echo_count))

                if existing[1] == None:
                    self.send("add","python-echo"+str(self.echo_count),"echo","Trace monitor for python client.  Do not call directly.",True)
                    ready = True
                else:
                    self.echo_count += 1

        
            self.send("monitor","model-trace","python-echo"+str(self.echo_count))
            self.send("monitor","command-trace","python-echo"+str(self.echo_count))
            self.send("monitor","warning-trace","python-echo"+str(self.echo_count))
            self.send("monitor","general-trace","python-echo"+str(self.echo_count))
            self.echo = True
            return True

        else:
            print("echo_output called when output was already on.")
            return False

    def no_output(self):
    
        if self.echo:
            self.send("remove-monitor","model-trace","python-echo"+str(self.echo_count))
            self.send("remove-monitor","command-trace","python-echo"+str(self.echo_count))
            self.send("remove-monitor","warning-trace","python-echo"+str(self.echo_count))
            self.send("remove-monitor","general-trace","python-echo"+str(self.echo_count))
            self.send("remove","python-echo"+str(self.echo_count))
            self.echo = False
        else:
            print("no_output called when output was already off.")

current_connection = connection()

def current_model():
    try:
        m = locals.model_name
    except AttributeError:
        m = current_connection.evaluate_single('current-model')
    return m

def set_current_model(name):
    if name.lower() in (x.lower() for x in mp_models()):
        locals.model_name = name
    else:
        print("%s is not one of the currently available models: %s"%(name,mp_models()))


def reset ():
    return current_connection.evaluate_single("reset")

def reload (compile=False):
    return current_connection.evaluate_single("reload",compile)

def run (time, real_time=False):
    return current_connection.evaluate("run", time, real_time)

def run_full_time (time, real_time=False):
    return current_connection.evaluate("run-full-time", time, real_time)

def run_until_time (time, real_time=False):
    return current_connection.evaluate("run-until-time", time, real_time)

def run_n_events (event_count, real_time=False):
    return current_connection.evaluate("run-n-events", event_count, real_time)

def run_until_condition(condition,real_time=False):
    return current_connection.evaluate("run-until-condition", condition, real_time)

def buffer_chunk (*params):
    return current_connection.evaluate_single("buffer-chunk", *params)

def whynot (*params):
    return current_connection.evaluate_single("whynot", *params)

def whynot_dm (*params):
    return current_connection.evaluate_single("whynot-dm", *params)


def penable (*params):
    return current_connection.evaluate_single("penable", *params)

def pdisable (*params):
    return current_connection.evaluate_single("pdisable", *params)

def load_act_r_model (path):
    return current_connection.evaluate_single("load-act-r-model",path)

def load_act_r_code (path):
    return current_connection.evaluate_single("load-act-r-code",path)

def goal_focus (goal=None):
    return current_connection.evaluate_single("goal-focus",goal)

def clear_exp_window(win=None):
    return current_connection.evaluate_single("clear-exp-window",win)


def open_exp_window(title,visible=True,width=300,height=300,x=300,y=300):
    return current_connection.evaluate_single("open-exp-window", title, [["visible", visible], ["width", width],
                                                                         ["height", height], ["x", x], ["y", y]])

def add_text_to_exp_window(window,text,x=0,y=0,color='black',height=20,width=75,font_size=12):
    return current_connection.evaluate_single("add-text-to-exp-window", window, text,[["color", color], ["width", width],
                                                                                      ["height", height], ["x", x], ["y", y], 
                                                                                      ["font-size", font_size]])

def add_button_to_exp_window(window,text="",x=0,y=0,action=None,height=20,width=75,color='gray'):
    return current_connection.evaluate_single("add-button-to-exp-window",window,[["color", color], ["width", width],
                                                                                 ["height", height], ["x", x], ["y", y], 
                                                                                 ["text", text], ["action", action]])

def remove_items_from_exp_window(window,*items):
    return current_connection.evaluate_single("remove-items-from-exp-window",window,*items)


def install_device(device):
    return current_connection.evaluate_single("install-device",device)

def print_warning(warning):
    current_connection.evaluate("print-warning",warning)

def act_r_output(output):
    current_connection.evaluate("act-r-output",output)

def random(value):
    return current_connection.evaluate_single("act-r-random",value)


def add_command(name,function=None,documentation="No documentation provided.",single=True,local_name=None,encoded=False):
    return current_connection.add_command(name,function,documentation,single,local_name,encoded)

def monitor_command(original,monitor):
    return current_connection.monitor_command(original,monitor)
 
def remove_command_monitor(original,monitor):
    return current_connection.remove_command_monitor(original,monitor)

def remove_command(name):
    return current_connection.remove_command(name)

def print_visicon():
    return current_connection.evaluate_single("print-visicon")

def mean_deviation(results,data,output=True):
    return current_connection.evaluate_single("mean-deviation",results,data,output)

def correlation(results,data,output=True):
    return current_connection.evaluate_single("correlation",results,data,output)

def get_time(model_time=True):
    return current_connection.evaluate_single("get-time",model_time)

def buffer_status (*params):
    return current_connection.evaluate_single("buffer-status", *params)

def buffer_read (buffer):
    return current_connection.evaluate_single("buffer-read", buffer)

def clear_buffer (buffer):
    return current_connection.evaluate_single("clear-buffer", buffer)

def new_tone_sound (freq, duration, onset=False, time_in_ms=False):
    return current_connection.evaluate_single("new-tone-sound", freq, duration, onset, time_in_ms)

def new_word_sound (word, onset=False, location='external', time_in_ms=False):
    return current_connection.evaluate_single("new-word-sound", word, onset, location, time_in_ms)

def new_digit_sound (digit, onset=False, time_in_ms=False):
    return current_connection.evaluate_single("new-digit-sound", digit, onset, time_in_ms)

def define_chunks (*chunks):
    return current_connection.evaluate_single("define-chunks", *chunks)

def define_chunks_fct (chunks):
    return current_connection.evaluate_single("define-chunks", *chunks)

def add_dm (*chunks):
    return current_connection.evaluate_single("add-dm", *chunks)

def add_dm_fct (chunks):
    return current_connection.evaluate_single("add-dm-fct", chunks)

def pprint_chunks (*chunks):
    return current_connection.evaluate_single("pprint-chunks", *chunks)

def chunk_slot_value (chunk_name, slot_name):
    return current_connection.evaluate_single("chunk-slot-value", chunk_name, slot_name)

def buffer_slot_value (buffer_name, slot_name):
    return current_connection.evaluate_single("buffer-slot-value", buffer_name, slot_name)

def set_chunk_slot_value (chunk_name, slot_name, new_value):
    return current_connection.evaluate_single("set-chunk-slot-value", chunk_name, slot_name, new_value)

def mod_chunk (chunk_name, *mods):
    return current_connection.evaluate_single("mod-chunk", chunk_name, *mods)

def mod_focus (*mods):
    return current_connection.evaluate_single("mod-focus", *mods)

def chunk_p (chunk_name):
    return current_connection.evaluate_single("chunk-p",chunk_name)

def copy_chunk (chunk_name):
    return current_connection.evaluate_single("copy-chunk",chunk_name)

def extend_possible_slots (slot_name, warn=True):
    return current_connection.evaluate_single("extend-possible-slots",slot_name,warn)

def model_output (output_string):
    return current_connection.evaluate_single("model-output",output_string)


def set_buffer_chunk (buffer_name, chunk_name, requested=True):
    return current_connection.evaluate_single("set-buffer-chunk",buffer_name,chunk_name,requested)

def add_line_to_exp_window (window, start, end, color = False):
    if color:
        return current_connection.evaluate_single("add-line-to-exp-window",window,start,end,color)
    else:
        return current_connection.evaluate_single("add-line-to-exp-window",window,start,end)

def modify_line_for_exp_window (line, start, end, color = False):
    if color:
        return current_connection.evaluate_single("modify-line-for-exp-window",line,start,end,color)
    else:
        return current_connection.evaluate_single("modify-line-for-exp-window",line,start,end)

def start_hand_at_mouse ():
    return current_connection.evaluate_single("start-hand-at-mouse")

def schedule_event (time, action, params=None, module=':NONE', priority=0, maintenance=False, destination=None, details=None,output=True,time_in_ms=False,precondition=None):
    return current_connection.evaluate_single("schedule-event",time,action,[["params", params],["module", module],
                                                                            ["priority", priority],["maintenance", maintenance],
                                                                            ["destination", destination], ["details", details],
                                                                            ["output", output],["time-in-ms", time_in_ms],
                                                                            ["precondition", precondition]])

def schedule_event_now (action, params=None, module=':NONE', priority=0, maintenance=False, destination=None, details=None,output=True,precondition=None):
    return current_connection.evaluate_single("schedule-event-now",action,[["params", params],["module", module],
                                                                                   ["priority", priority],["maintenance", maintenance],
                                                                                   ["destination", destination], ["details", details],
                                                                                   ["output", output], ["precondition", precondition]])

def schedule_event_relative (time_delay, action, params=None, module=':NONE', priority=0, maintenance=False, destination=None, details=None,output=True,time_in_ms=False,precondition=None):
    return current_connection.evaluate_single("schedule-event-relative",time_delay,action,[["params", params],["module", module],
                                                                        ["priority", priority],["maintenance", maintenance],
                                                                        ["destination", destination], ["details", details],
                                                                        ["output", output],["time-in-ms", time_in_ms],
                                                                        ["precondition", precondition]])

def schedule_event_after_module (after_module, action, params=None, module=':NONE', maintenance=False, destination=None, details=None, output=True, precondition=None, dynamic=False, delay=True, include_maintenance=False):
    return current_connection.evaluate("schedule-event-after-module",after_module,action,[["params", params],["module", module],
                                                                        ["maintenance", maintenance],
                                                                        ["destination", destination], ["details", details],
                                                                        ["output", output],["delay", delay], ["dynamic", dynamic],
                                                                        ["precondition", precondition],["include-maintenance", include_maintenance]])


def schedule_break_relative (time_delay,time_in_ms=False, priority=":max", details=None):
    return current_connection.evaluate_single("schedule-break-relative",time_delay,[["time-in-ms", time_in_ms],["priority", priority],["details",details]])

def mp_show_queue(indicate_traced=False):
    return current_connection.evaluate_single("mp-show-queue",indicate_traced)

def print_dm_finsts():
    return current_connection.evaluate_single("print-dm-finsts")

def spp (*params):
    return current_connection.evaluate_single("spp", *params)

def mp_models():
    return current_connection.evaluate_single("mp-models")

def all_productions():
    return current_connection.evaluate_single("all-productions")

def buffers():
    return current_connection.evaluate_single("buffers")

def printed_visicon():
    return current_connection.evaluate_single("printed-visicon")

def print_audicon():
    return current_connection.evaluate_single("print-audicon")

def printed_audicon():
    return current_connection.evaluate_single("printed-audicon")

def printed_parameter_details(param):
    return current_connection.evaluate_single("printed-parameter-details",param)

def sorted_module_names():
    return current_connection.evaluate_single("sorted-module-names")

def modules_parameters(module):
    return current_connection.evaluate_single("modules-parameters",module)

def modules_with_parameters():
    return current_connection.evaluate_single("modules-with-parameters")

def used_production_buffers():
    return current_connection.evaluate_single("used-production-buffers")

def record_history(*params):
    return current_connection.evaluate_single("record-history",*params)

def stop_recording_history(*params):
    return current_connection.evaluate_single("stop-recording-history",*params)

def get_history_data(history,*params):
    return current_connection.evaluate_single("get-history-data",history,*params)

def history_data_available(history,file=False,*params):
    return current_connection.evaluate_single("history-data-available",history,file,*params)

def process_history_data(processor,file=False,data_params=None,processor_params=None):
    return current_connection.evaluate_single("process-history-data",processor,file,data_params,processor_params)

def save_history_data(history,file,comment="",*params):
    return current_connection.evaluate_single("save-history-data",history,file,comment,*params)


def dm (*params):
    return current_connection.evaluate_single("dm", *params)

def sdm (*params):
    return current_connection.evaluate_single("sdm", *params)


def get_parameter_value(param):
    return current_connection.evaluate_single("get-parameter-value",param)

def set_parameter_value(param,value):
    return current_connection.evaluate_single("set-parameter-value",param,value)


def get_system_parameter_value(param):
    return current_connection.evaluate_single("get-system-parameter-value",param)

def set_system_parameter_value(param,value):
    return current_connection.evaluate_single("set-system-parameter-value",param,value)


def sdp (*params):
    return current_connection.evaluate_single("sdp", *params)


def simulate_retrieval_request (*spec):
    return current_connection.evaluate_single("simulate-retrieval-request", *spec)

def saved_activation_history ():
    return current_connection.evaluate_single("saved-activation-history")

def print_activation_trace (time, ms = True):
    return current_connection.evaluate_single("print-activation-trace",time,ms)

def print_chunk_activation_trace (chunk, time, ms = True):
    return current_connection.evaluate_single("print-chunk-activation-trace",chunk,time,ms)

def pp (*params):
    return current_connection.evaluate_single("pp", *params)

def trigger_reward(reward,maintenance=False):
    return current_connection.evaluate_single("trigger-reward",reward,maintenance)


def define_chunk_spec (*spec):
    return current_connection.evaluate_single("define-chunk-spec", *spec)

def chunk_spec_to_chunk_def(spec_id):
    return current_connection.evaluate_single("chunk-spec-to-chunk-def", spec_id)

def release_chunk_spec(spec_id):
    return current_connection.evaluate_single("release-chunk-spec-id", spec_id)
   


def schedule_simple_set_buffer_chunk (buffer, chunk, time, module='NONE', priority=0, requested=True):
    return current_connection.evaluate_single("schedule-simple-set-buffer-chunk",buffer,chunk,time,module,priority,requested)

def schedule_simple_mod_buffer_chunk (buffer, mod_list_or_spec, time, module='NONE', priority=0):
    return current_connection.evaluate_single("schedule-simple-mod-buffer-chunk",buffer,mod_list_or_spec,time,module,priority)


def schedule_set_buffer_chunk (buffer, chunk, time, module=':NONE', priority=0, output='low',time_in_ms=False,requested=True):
    return current_connection.evaluate_single("schedule-set-buffer-chunk",buffer,chunk,time,[["module", module],
                                                                        ["priority", priority],["output", output],["time-in-ms", time_in_ms],
                                                                        ["requested", requested]])

def schedule_mod_buffer_chunk (buffer, mod_list_or_spec, time, module=':NONE', priority=0, output='low',time_in_ms=False):
    return current_connection.evaluate_single("schedule-mod-buffer-chunk",buffer,mod_list_or_spec,time,[["module", module],
                                                                        ["priority", priority],["output", output],["time-in-ms", time_in_ms]])


def undefine_module(name):
    return current_connection.evaluate_single("undefine-module", name)


def delete_chunk(name):
    return current_connection.evaluate_single("delete-chunk", name)

def purge_chunk(name):
    return current_connection.evaluate_single("purge-chunk", name)



def define_module (name, buffers,params,interface=None):
    return current_connection.evaluate_single("define-module", name, buffers, params, interface)


def command_output(string):
    return current_connection.evaluate_single("command-output",string)

def chunk_copied_from(chunk_name):
    return current_connection.evaluate_single("chunk-copied-from",chunk_name)


def mp_time ():
    return current_connection.evaluate_single("mp-time")

def mp_time_ms ():
    return current_connection.evaluate_single("mp-time-ms")

def predict_bold_response(start=None,end=None,output=None):
    if start == None:
        return current_connection.evaluate_single("predict-bold-response")
    elif end == None:
        return current_connection.evaluate_single("predict-bold-response", start)
    elif output == None:
        return current_connection.evaluate_single("predict-bold-response", start, end)
    else:
        return current_connection.evaluate_single("predict-bold-response", start, end, output)

def pbreak (*params):
    return current_connection.evaluate_single("pbreak", *params)

def punbreak (*params):
    return current_connection.evaluate_single("punbreak", *params)

def create_image_for_exp_window(window,text,file,x=0,y=0,width=50,height=50,action=None):
    return current_connection.evaluate_single("create-image-for-exp-window", window, text, file,
                                              [['x', x],['y', y],['width', width],['height', height],['action', action]])

def add_image_to_exp_window(window,text,file,x=0,y=0,width=50,height=50,action=None):
    return current_connection.evaluate_single("add-image-to-exp-window", window, text, file,
                                              [['x', x],['y', y],['width', width],['height', height],['action', action]])

def add_items_to_exp_window(window, *items):
    return current_connection.evaluate_single("add-items-to-exp-window",window, *items)


def add_visicon_features(*features):
    return current_connection.evaluate_single("add-visicon-features",*features)

def delete_visicon_features(*features):
    return current_connection.evaluate_single("delete-visicon-features",*features)

def delete_all_visicon_features():
    return current_connection.evaluate_single("delete-all-visicon-features")

def modify_visicon_features(*features):
    return current_connection.evaluate_single("modify-visicon-features",*features)

def running():
    return current_connection.evaluate_single("act-r-running-p")


def stop_output():
    current_connection.interface.no_output()

def resume_output():
    current_connection.interface.echo_output()

def hide_output():
    current_connection.interface.show_output = False

def unhide_output():
    current_connection.interface.show_output = True

def visible_virtuals_available():
    return current_connection.evaluate_single("visible-virtuals-available?")

def process_events():
    time.sleep(0)

def permute_list(l):

    indexes = list(range(len(l)))
    new_indexes = current_connection.evaluate_single("permute-list",indexes)
    result = []
    for i in new_indexes:
        result.append(l[i])
    return result

def call_command(command,*parameters):
    return current_connection.evaluate_single(command,*parameters)


def import_from_path(fullpath,reload=False):
    """ 
    Import a file with full path specification. Allows one to
    import from anywhere, something __import__ does not do. 
    """
    path, filename = os.path.split(fullpath)
    filename, ext = os.path.splitext(filename)
    if ext == '.py':
        sys.path.insert(0, path)
        module = __import__(filename)
        if reload:   ## not used at this point
            imp.reload(module)
        del sys.path[0]
        return module
    else:
        return False


def env_loader(path):
    """
    Ugly solution to something probably not necessary,
    but seems some novice ACT-R users that wanted to use
    Python wanted to use the 'load ACT-R code' button for
    the Python files too.  So, this provides a way that
    such a button could be implemented and make the module
    available directly from the interactive prompt from which
    actr was imported so that it would still match the tutorial
    descriptions.
    """
    global __main__  

    try:
        module=import_from_path(path)
     
        if module:
            setattr(__main__,module.__name__,module)
            return True
        else:
            return "Only a .py file can be imported"
    except:
      print("Problem with trying to import from ",path)
      print(sys.exc_info())
      return str(sys.exc_info()[1])


from pathlib import Path

starting_dir = Path(__file__).parent.absolute()

def env_loader_no_path(file):
    """
    Add the current file's path to the file name given and then
    pass it off to env_loader
    """

    return(env_loader(starting_dir.joinpath(file)))


add_command("Python-import-from-file",env_loader,"Import a Python module and make it available directly from the interactive prompt. Params: pathname")
    
add_command("load-python-module-html",env_loader_no_path,"Import a python module from the directory containing the actr.py module and make it available directly from the interactive prompt. Params: filename")


