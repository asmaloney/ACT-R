import actr

actr.load_act_r_model ("ACT-R:tutorial;unit6;bst-model.lisp")

target = None
current_stick = None
current_line = None
done = False
choice = None
window = None
visible = False

exp_data = [20, 67, 20, 47, 87, 20, 80, 93, 83, 13, 29, 27, 80, 73, 53]
exp_stims = [[15,250,55,125],[10,155,22,101],[14,200,37,112],
             [22,200,32,114],[10,243,37,159],[22,175,40,73],
             [15,250,49,137],[10,179,32,105],[20,213,42,104],
             [14,237,51,116],[12,149,30,72],[14,237,51,121],
             [22,200,32,114],[14,200,37,112],[15,250,55,125]]

no_learn_stims = [[15,200,41,103],[10,200,29,132]]

def build_display (a,b,c,goal):
    global window,target,current_stick,done,current_line,choice

    target = goal
    current_stick = 0
    done = False
    choice = None
    current_line = None
    window = actr.open_exp_window("Building Sticks Task",visible=visible,width=600,height=400)

    actr.add_button_to_exp_window(window, text="A", x=5, y=23, action=["bst-button-pressed",a,"under"], height=24, width=40)
    actr.add_button_to_exp_window(window, text="B", x=5, y=48, action=["bst-button-pressed",b,"over"], height=24, width=40)
    actr.add_button_to_exp_window(window, text="C", x=5, y=73, action=["bst-button-pressed",c,"under"], height=24, width=40)
    actr.add_button_to_exp_window(window, text="Reset", x=5, y=123, action="bst-reset-button-pressed", height=24, width=65)

    actr.add_line_to_exp_window(window,[75,35],[a + 75,35],"black")
    actr.add_line_to_exp_window(window,[75,60],[b + 75,60],"black")
    actr.add_line_to_exp_window(window,[75,85],[c + 75,85],"black")
    actr.add_line_to_exp_window(window,[75,110],[goal + 75,110],"green")

def button_pressed(len,dir):
    global choice,current_stick

    if not(choice):
        choice = dir

    if not(done):
        if current_stick > target:
            current_stick -= len
        else:
            current_stick += len

        update_current_line()

    
def reset_display():
    global current_stick

    if not(done):
        current_stick = 0
        update_current_line()


actr.add_command("bst-button-pressed",button_pressed,"Choice button action for the Building Sticks Task.  Do not call directly")
actr.add_command("bst-reset-button-pressed",reset_display,"Reset button action for the Building Sticks Task.  Do not call directly")


def update_current_line():
    global current_line,done

    if current_stick == target:
        done = True
        actr.modify_line_for_exp_window(current_line, [75,135], [target + 75,135])
        actr.add_text_to_exp_window(window, "Done", x=180, y=200)
    elif current_stick == 0:
        if current_line:
            actr.remove_items_from_exp_window(window,current_line)
            current_line = None
    elif current_line:
        actr.modify_line_for_exp_window(current_line,[75,135],[current_stick + 75,135])
    else:
        current_line = actr.add_line_to_exp_window(window,[75,135],[current_stick + 75,135],"blue")


def do_experiment(sticks, human=False):
    build_display(*sticks)
  
    if human:
        if actr.visible_virtuals_available():
            wait_for_human()
    else:
        actr.install_device(window)
        actr.start_hand_at_mouse()
        actr.run(60,visible)

def wait_for_human ():
    while not(done):
        actr.process_events()

    start = actr.get_time(False)
    while (actr.get_time(False) - start) < 1000:
        actr.process_events()

def bst_set(human,vis,stims,learn=True):
    global visible

    result = []

    visible = vis

    for stim in stims:
        if not(learn) and not(human):
            actr.reset()
        do_experiment(stim,human)
        result.append(choice)

    return result

def test(n,human=False):
    
    l = len(no_learn_stims)

    result = [0]*l

    if human or (n == 1):
        v = True
    else:
        v = False

    for i in range(n):

        d = bst_set(human,v,no_learn_stims,False)

        for j in range(l):
            if d[j] == "over":
                result[j] += 1

    return result

def experiment(n,human=False):

    l = len(exp_stims)

    result = [0] * l
    p_values = [["decide-over",0],["decide-under",0],["force-over",0],["force-under",0]]
    for i in range(n):
        actr.reset()
        d = bst_set(human,human,exp_stims)
        for j in range(l):
            if d[j] == "over":
                result[j] += 1

        actr.hide_output()  

        for p in p_values:
            p[1]+=production_u_value(p[0])

        actr.unhide_output()

    result = list(map(lambda x: 100 * x / n,result))

    if len(result) == len(exp_data):
        actr.correlation(result,exp_data)
        actr.mean_deviation(result,exp_data)

    print()
    print("Trial ",end="")
    for i in range(l):
        print("%-8d"%(i + 1),end="")
    print()

    print("  ",end="")
    for i in range(l):
        print("%8.2f"%result[i],end="")

    print()
    print()

    for p in p_values:
        print("%-12s: %6.4f"%(p[0],p[1]/n))


    
def production_u_value(prod):
    return actr.spp(prod,":u")[0][0]