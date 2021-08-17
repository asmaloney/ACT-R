import actr

actr.load_act_r_model("ACT-R:tutorial;unit3;subitize-model.lisp")

response = False
response_time = False

exp_data = [.6,.65,.7,.86, 1.12,1.5,1.79,2.13,2.15,2.58]

def number_to_word (n):
    map = ['','one','two','three','four','five','six','seven','eight','nine','ten']
    return map[n]

def number_to_string (n):
    if n == 10:
        return "0"
    else:
        return str(n)


def trial (n,human=False):

    actr.reset()

    points = generate_points(n)
    window = actr.open_exp_window("Subitizing Experiment")
    start = actr.get_time(not(human))
    
    for point in points:
        actr.add_text_to_exp_window(window, "x", x=point[0], y=point[1])

    global response,response_time

    response = ''
    response_time = False

    if human:
        if actr.visible_virtuals_available():
            actr.add_command("subitize-response",respond_to_key_press,
                             "Subitize task human response")
            actr.monitor_command("output-key","subitize-response")

            answer = number_to_string(n)
            while response == '':
                actr.process_events()

            actr.remove_command_monitor("output-key","subitize-response")
            actr.remove_command("subitize-response")
    else:
        actr.add_command("subitize-response",record_model_speech,
                         "Subitize task model response")
        actr.monitor_command("output-speech","subitize-response")

        answer = number_to_word(n)
        actr.install_device(window)

        actr.run(30,True)

        actr.remove_command_monitor("output-speech","subitize-response")
        actr.remove_command("subitize-response")

    if response != '' and response.lower() == answer.lower():
        return [(response_time - start) / 1000.0, True]
    else:
        return [30, False]

def experiment(human=False):

    results = []
    for items in actr.permute_list([10,9,8,7,6,5,4,3,2,1]):
      results.append((items,trial(items,human)))

    results.sort()
    report_data(list(map(lambda x: x[1],results)))

def report_data(data):

    rts = list(map(lambda x: x[0],data))
    actr.correlation(rts,exp_data)
    actr.mean_deviation(rts,exp_data)
    print_results(data)

def print_results(data):

    print("Items    Current Participant   Original Experiment")
    for count, d, original in zip(range(len(data)),data,exp_data):
        print("%3d        %5.2f  (%-5s)            %5.2f" % 
              (count+1,d[0],d[1],original))

def generate_points(n):
    p=[]
    for i in range(n):
        p.append(new_distinct_point(p))
    return p

def new_distinct_point(points):

    while True:
        x = actr.random(240)+20
        y = actr.random(240)+20
        if not(any(too_close(x,y,p) for p in points)):
            break;
    return [x,y]

def too_close (x,y,p):
    if (abs(x-p[0]) < 40) and (abs(y-p[1]) < 40):
        return True
    else:
        return False

def respond_to_key_press (model,key):
    global response,response_time

    response_time = actr.get_time(False)
    response = key

def record_model_speech (model,string):
    global response,response_time

    response_time = actr.get_time(True)
    response = string
