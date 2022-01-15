# ACT-R tutorial unit 3 code for a simple task to 
# investigate potential perceptual and motor issues
# with models.

import actr

actr.load_act_r_model("ACT-R:tutorial;unit3;perceptual-motor-issues-model.lisp")

response = []
response_time = False


def respond_to_key_press (model,key):
    global response,response_time

    if not(response_time):
        response_time = actr.get_time()
    response.append(key.upper())


def task(which=False):
    global response,response_time
   
    actr.reset()
    alphabet = ["A","B","C","D","E","F","G","H",
                "I","J","K","L","M","N","O","P","Q",
                "R","S","T","U","V","W","X","Y","Z"]
    letter = actr.permute_list(alphabet)[0]
    alphabet = ["Z"] + alphabet

    if which == 'next' or which == 'previous':
        task = which
    elif actr.random(2) == 0:
        task = 'next'
    else: 
        task = 'previous'

    time = 1500 + actr.random(1000)

    window = actr.open_exp_window("Simple task")

    actr.install_device(window)
   
    actr.add_command("pm-issues-response",respond_to_key_press,
                     "Perceptual-motor issues task response")
    actr.monitor_command("output-key","pm-issues-response")

    actr.add_text_to_exp_window(window, letter, x=130, y=150)

    actr.add_command("pm-issue-display",display_prompt,
                     "Perceptual-motor issues task prompt display")

    actr.schedule_event_relative(time,"pm-issue-display",params=[window,task],time_in_ms=True)

    response = []
    response_time = False


    actr.run(10,True)    

    actr.remove_command("pm-issue-display")
    actr.remove_command_monitor("output-key","pm-issues-response")
    actr.remove_command("pm-issues-response")

    if (len(response) == 2 and response_time > time and response[0] == letter and
         ((task == 'next' and alphabet.index(response[0]) == (alphabet.index(response[1]) - 1)) or
          (task == 'previous' and alphabet.index(response[0]) == (alphabet.index(response[1]) + 1)))):
        result = True
    else:
        result = False

    return [task,result]

def display_prompt(window,prompt):

    actr.clear_exp_window()
    actr.add_text_to_exp_window(window, prompt, x=125, y=150)

