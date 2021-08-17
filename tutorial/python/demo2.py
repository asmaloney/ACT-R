
import actr

actr.load_act_r_model("ACT-R:tutorial;unit2;demo2-model.lisp")

response = False

def respond_to_key_press (model,key):
    global response

    response = key
    actr.clear_exp_window()


def experiment (human=False):
  
    actr.reset()
  
    items = actr.permute_list(["B","C","D","F","G","H","J","K","L",
                               "M","N","P","Q","R","S","T","V","W",
                               "X","Y","Z"])
    text1 = items[0]
    window = actr.open_exp_window("Letter recognition")

    actr.add_text_to_exp_window(window, text1, x=125, y=150)

    actr.add_command("demo2-key-press",respond_to_key_press,
                     "Demo2 task output-key monitor")
    actr.monitor_command("output-key","demo2-key-press")

    global response
    response = False
    
    if human == True:
        if actr.visible_virtuals_available():
            while response == False:
                actr.process_events()
      
    else:
        actr.install_device(window)
        actr.run(10,True)
    
    actr.remove_command_monitor("output-key","demo2-key-press")
    actr.remove_command("demo2-key-press")
    
    return response
