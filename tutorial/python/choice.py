import actr

actr.load_act_r_model("ACT-R:tutorial;unit6;choice-model.lisp")

choice_data = [0.664, 0.778, 0.804, 0.818]

response = False

def respond_to_key_press (model,key):
    global response

    response = key

def person():
    global response

    if actr.visible_virtuals_available():
        window = actr.open_exp_window("Choice Experiment",visible=True)
    
        actr.add_command("choice-response",respond_to_key_press,"Choice task key response")
        actr.monitor_command("output-key","choice-response")

        actr.add_text_to_exp_window (window, 'choose', x=50, y=100)

        response = ''
            
        while response == '':
            actr.process_events()

        actr.clear_exp_window(window)
    
        if actr.random(1.0) < .9:
            answer = 'heads'
        else:
            answer = 'tails'

        actr.add_text_to_exp_window (window, answer, x=50, y=100)

        start = actr.get_time(False)
            
        while (actr.get_time(False) - start) < 1000:
            actr.process_events()

        actr.remove_command_monitor("output-key","choice-response")
        actr.remove_command("choice-response")
 
        return response


def model ():
    None

def data (n):
    None