import actr
import math
import numbers

cat1 = [["small","large","medium","small"],
        ["medium","small","large","medium"],
        ["small","medium","medium","medium"],
        ["small","medium","large","medium"],
        ["small","medium","large","large"]]
  
cat2 = [["large","small","small","small"],
        ["medium","medium","small","large"],
        ["large","small","large","small"],
        ["large","small","large","large"],
        ["large","small","small","large"]]

cat_data = [0.975, 0.85, 0.987, 1.0, 0.963, 0.075, 0.138, 0.087, 0.05, 0.025, 0.937, 0.544, 0.988, 0.087]

stims = [[-1.025,  0.493,  0.048,  -0.666],
       [-0.172,  -0.557,  0.337,  0.163],
       [-0.98,  0.275,  -0.005,  -0.067],
       [-0.951,  0.259,  0.399,  0.093],
       [-0.96,  0.198,  0.38,  0.527],
       [0.665,  -0.441,  -0.508,  -0.396],
       [-0.059,  0.243,  -0.602,  0.624],
       [0.586,  -0.511,  0.381,  -0.507],
       [0.823,  -0.539,  0.332,  0.633],
       [0.823,  -0.504,  -0.487,  0.776],
       [-1.114,  -0.52,  0.636,  -0.028],
       [-0.154, -0.562,  -0.043,  0.057],
       [-0.856,  0.197,  0.241,  0.007],
       [0.704,  -0.287,  -0.164,  0.178]]

sigma2 = .15
size_mappings = {"small":-.9, "medium":0, "large":.9}
slots = ["eh", "es", "nl", "mh"]

offset = 0

def scale_sim(x, max):
    return( (x / max) - 1.0)

def normal(x,sigma2,m):
    return ( ( 1 / math.sqrt(2 * math.pi * sigma2)) * math.exp(- (((x - m) * (x - m)) / (2 * sigma2))))

max_norm = normal(0,sigma2,0)

def size_similarities(a, b):

    if isinstance(b,numbers.Number) and a.lower() in size_mappings:
        return(scale_sim(normal((b - offset),sigma2,size_mappings[a.lower()]),max_norm))
    else:
        return False

actr.add_command("size-similarities", size_similarities,"Categorize model's similarity hook function.")

def stimulus(a,b,c,d):
    global offset,slots

    offset = 0
    slots = ["eh", "es", "nl", "mh"]
    result,response = trial(a,b,c,d)
    if response:
        return (result)
    else:
        return (False)

def attribute(name,value):

    goal = actr.define_chunks(["state","add-attribute","name",name,"value", (value + offset)])[0]
    actr.goal_focus(goal)
    actr.run(20)

def experiment(n,new_offset=0,s1="eh",s2="es",s3="nl",s4="mh"):
    global offset,slots

    if all(list(map(lambda x: isinstance(x,str),[s1,s2,s3,s4]))):
        s1 = s1.lower()
        s2 = s2.lower()
        s3 = s3.lower()
        s4 = s4.lower()

        if s1 in [s2,s3,s4] or s2 in [s1,s3,s4] or s3 in [s1,s2,s4] or s4 in [s1, s2, s3]:
            actr.print_warning("Duplicate slot names provided. Using default slots.")
            slots = ["eh", "es", "nl", "mh"]
        elif 'category' in [s1,s2,s3,s4]:
            actr.print_warning("Slot named category cannot be used. Using default slots.")
            slots = ["eh", "es", "nl", "mh"]
        else:
            slots=[s1,s2,s3,s4]
    else:
        actr.print_warning("Not all slot names provided are strings. Using default slots.")
        slots = ["eh", "es", "nl", "mh"]

    if isinstance(new_offset,numbers.Number):
        offset = new_offset
    else:
        offset = 0

    trials = len(cat_data)

    results = [0] * trials
    counts = [0] * trials

    for i in range(n):
        answers,responses = do_experiment()
        results = list(map(lambda x,y: x + y,results,answers))
        counts = list(map(lambda x,y: x + y,counts,responses))

    results=list(map(lambda x: x/n,results))

    offset = 0

    actr.correlation(results,cat_data)
    actr.mean_deviation(results,cat_data)

    print("P(C=1)")
    print("        ",end="")
    for i in range(trials):
        print("(%4d) " % counts[i],end="")
    print()
    print("data  ",end="")
    for i in range(trials):
        print("%7.3f" % cat_data[i],end="")
    print()
    print("model ",end="")
    for i in range(trials):
        print("%7.3f" % results[i],end="")
    print()

def do_experiment():
    category_one = []
    answered = []

    for s in stims:
        category,response = trial(*s)
        if category == 1:
            category_one.append(1)
        else:
            category_one.append(0)
        if response:
            answered.append(1)
        else:
            answered.append(0)

    return((category_one,answered))
    

def trial(*features):

    actr.reset()
 
    for slot,value in actr.permute_list(list(zip(slots,features))):
        attribute(slot,value)

    goal = actr.define_chunks(["state","categorize"])[0]
    actr.goal_focus(goal)
    actr.run(20)
    
    answer = actr.chunk_slot_value(actr.buffer_read("imaginal"),"category")

    if isinstance(answer,numbers.Number):
        if answer == 1 or answer == 2:
            return((answer,True))
        else:
            actr.model_output("Model responded with invalid category.")
            return((0,False))
    else:
        actr.model_output("Model did not respond or provided a non-numeric category.")
        return((0,False))

def create_example_memories():

    for s in slots:
        actr.extend_possible_slots(s,False)
        actr.define_chunks([s,"isa","chunk"])

    for c in cat1:
        chunk = ["isa","example","category",1]
        for slot,value in list(zip(slots,c)):
            chunk.append(slot)
            chunk.append(value)
        actr.add_dm(chunk)

    for c in cat2:
        chunk = ["isa","example","category",2]
        for slot,value in list(zip(slots,c)):
            chunk.append(slot)
            chunk.append(value)
        actr.add_dm(chunk)

actr.add_command("create-example-memories",create_example_memories,"Categorize task function to add the initial example chunks to simulate the training process.")

actr.load_act_r_model("ACT-R:tutorial;unit8;categorize-model.lisp")
    
  


    