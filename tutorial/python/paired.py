import actr

actr.load_act_r_model("ACT-R:tutorial;unit4;paired-model.lisp")

response = False
response_time = False

pairs = list(zip(['bank','card','dart','face','game','hand','jack','king','lamb','mask',
             'neck','pipe','quip','rope','sock','tent','vent','wall','xray','zinc'],
            ['0','1','2','3','4','5','6','7','8','9','0','1','2','3','4','5','6','7','8','9']))

latencies = [0.0, 2.158, 1.967, 1.762, 1.680, 1.552, 1.467, 1.402]
probabilities = [0.0, .526, .667, .798, .887, .924, .958, .954]

def task (size,trials,human=False):

    actr.add_command("paired-response",respond_to_key_press,
                     "Paired associate task key press response monitor")
    actr.monitor_command("output-key","paired-response")

    result = do_experiment(size,trials,human)

    actr.remove_command_monitor("output-key","paired-response")
    actr.remove_command("paired-response")

    return result


def respond_to_key_press (model,key):
    global response,response_time

    response_time = actr.get_time(model)
    
    response = key

    
def do_experiment(size, trials, human):

    if human and not(actr.visible_virtuals_available()):
        actr.print_warning("Cannot run the task as a person without a visible window available.")
    else:

        actr.reset()

        result = []
        model = not(human)
        window = actr.open_exp_window("Paired-Associate Experiment", visible=human)

        if model:
            actr.install_device(window)

        for i in range(trials):
            score = 0
            time = 0

            for prompt,associate in actr.permute_list(pairs[20 - size:]):
         
                actr.clear_exp_window(window)
                actr.add_text_to_exp_window (window, prompt, x=150 , y=150)

                global response
                response = ''
                start = actr.get_time(model)

                if model:
                    actr.run_full_time(5)
                else:
                    while (actr.get_time(False) - start) < 5000:
                        actr.process_events()

                if response == associate:
                    score += 1
                    time += response_time - start
            
                actr.clear_exp_window(window)
                actr.add_text_to_exp_window (window, associate, x=150 , y=150)
                start = actr.get_time(model)
           
                if model:
                    actr.run_full_time(5)
                else:
                    while (actr.get_time(False) - start) < 5000:
                        actr.process_events()

            if score > 0:
                average_time = time / score / 1000.0
            else:
                average_time = 0

            result.append((score/size,average_time))

        return result

def experiment(n):
    
    for i in range(n):

        if i == 0:
            data = task(20,8)
        else:
            data = list(map(lambda x,y: (x[0] + y[0],x[1] + y[1]),data,task(20,8)))

    output_data(data,n)

def output_data(data,n):

    print_results(list(map(lambda x: x[1]/n,data)),latencies,"Latency")
    print_results(list(map(lambda x: x[0]/n,data)),probabilities,"Accuracy")


def print_results(predicted,data,label):

    print()
    print(label)
    actr.correlation(predicted,data)
    actr.mean_deviation(predicted,data)
    print("Trial    1       2       3       4       5       6       7       8")
    print("     ",end='')
    for i in predicted:
        print('%8.3f' % i,end='')
    print()
