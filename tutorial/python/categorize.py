# ACT-R tutorial unit8 categorization experiment.
# This task sequentially presents the model with 
# features which the model must classify as
# being small, medium, or large given a numeric
# description, and then after those features have
# been encoded it must make a choice as to which
# category of items it belongs based on the 
# examples that it has pre-encoded in declarative
# memory.  It is an abstraction and simplification
# of a face categorizing task:
#
# Nosofsky, R. M. (1991). Tests of an exemplar model for relating 
# perceptual classification and recognition memory. Journal of Experimental 
# Psychology: Human Perception and Performance. 17, 3-27.

# Import the actr module for tutorial tasks, the math
# module for sqrt, pi, and exp, and the numbers module
# for the Number class.

import actr
import math
import numbers

# These are the feature sets for the categories (based on
# the general values).

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

# This is the data indicating the category 1 choice proportions 
# for the set of stims below (represented by their underlying
# normalized numeric values)

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


# Global values for the similarity distribution, the mapping of
# sizes to an anchor point, the default slot names, and the
# offset to use for the presented stimulus values.

sigma2 = .15
size_mappings = {"small":-.9, "medium":0, "large":.9}
slots = ["eh", "es", "nl", "mh"]
offset = 0

# Functions for computing the similarity values using a
# normal distribution around the anchor point for a value
# and then scaling them from -1 to 0.

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

# categorize-stimulus resets the model and then presents the four
# feature values provided (which should be numbers from -2 to 2) to
# the model with the default slot names for features and then 
# creates a goal chunk with state categorize for the model to 
# determine a category for the features it encoded.  If the 
# model provides a category of 1 or 2 (by setting the category slot
# of the chunk in the imaginal buffer) then that value is
# returned, otherwise it returns nil.

def stimulus(a,b,c,d):
    global offset,slots

    offset = 0
    slots = ["eh", "es", "nl", "mh"]
    result,response = trial(a,b,c,d)
    if response:
        return (result)
    else:
        return (False)

# categorize-attribute takes two values which represent the
# name and value for an attribute in the task.  it presents
# the attribute to the model by setting slots of the chunk in
# the goal buffer and then running the model.  The state slot
# is set to add-attribute, the name slot is set to the name
# provided, and the value slot is set to the value provided.
# The model should encode that value into a general description
# (small, medium, or large) and store that into a slot of the
# chunk in the imaginal buffer with the provided name.
# It does not reset the model.

def attribute(name,value):

    actr.schedule_set_buffer_chunk("goal",["state","add-attribute","name",name,"value", (value + offset)], 0)
    actr.run(20)

# categorize-experiment takes one required value which
# is how many times to run the whole experiment (one presentation
# of each of the 14 testing stims).  It has one optional parameter
# which indicates an offset to add to the values that are presented
# if it is provided, and accepts 4 additional parameters which
# specify the names of the attributes to present to the model (the
# default names will be used if none are provided).  It runs the
# experiments, determines the proportion of category choices for 
# each item, reports the fit to the experimental data, and prints
# out the proportion of choices for category 1.

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

    actr.schedule_set_buffer_chunk("goal",["state","categorize"], 0)
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

# create-example-memories is called in the model 
# definition to add chunks for the training examples
# to the model's declarative memory.  The chunks are
# created with the appropriate slots for the features
# based on the values provided by the modeler to
# run the experiment or the default slots if not 
# running the experiment or alternate names were
# not provided.

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

# Need to load the model after the "create-example-memories" command
# has been added since that command is called during the model
# creation.

actr.load_act_r_model("ACT-R:tutorial;unit8;categorize-model.lisp")
    
  


    