import actr

actr.load_act_r_model("ACT-R:tutorial;unit5;fan-no-pm-model.lisp")

person_location_data = [1.11, 1.17, 1.22,
                        1.17, 1.20, 1.22,
                        1.15, 1.23, 1.36,
                        1.20, 1.22, 1.26,
                        1.25, 1.36, 1.29,
                        1.26, 1.47, 1.47]

def sentence (person, location, target, term):

    actr.reset()

    if term == 'person':
        actr.pdisable("retrieve-from-location")
    else:
        actr.pdisable("retrieve-from-person")

    actr.mod_chunk("goal","arg1",person,"arg2",location,"state","test")

    response_time = actr.run(30)[0]
    response = actr.chunk_slot_value(actr.buffer_read("goal"),"state")

    if target:
        if response.lower() == "'k'".lower():
            return (response_time ,True)
        else:
            return (response_time ,False)
    else:
        if response.lower() == "'d'".lower():
            return (response_time ,True)
        else:
            return (response_time ,False)
   

    
def do_person_location(term):

    data = []

    for person,location,target in [("'lawyer'", "'store'", True),
                                   ("'captain'", "'cave'", True),
                                   ("'hippie'", "'church'", True),
                                   ("'debutante'", "'bank'", True),
                                   ("'earl'", "'castle'", True),
                                   ("'hippie'", "'bank'", True),
                                   ("'fireman'", "'park'", True),
                                   ("'captain'", "'park'", True),
                                   ("'hippie'", "'park'", True),
                                   ("'fireman'", "'store'", False),
                                   ("'captain'", "'store'", False),
                                   ("'giant'", "'store'", False),
                                   ("'fireman'", "'bank'", False),
                                   ("'captain'", "'bank'", False),
                                   ("'giant'", "'bank'", False),
                                   ("'lawyer'", "'park'", False),
                                   ("'earl'", "'park'", False),
                                   ("'giant'", "'park'", False)]:

        data.append(sentence(person,location,target,term))

    return data


def experiment():

    output_person_location(list(map(lambda x,y:((x[0]+y[0])/2,(x[1] and y[1])),
                                    do_person_location('person'),
                                    do_person_location('location'))))


def output_person_location(data):

    rts = list(map(lambda x: x[0],data))

    actr.correlation(rts,person_location_data)
    actr.mean_deviation(rts,person_location_data)

    print("\nTARGETS:\n                         Person fan")
    print("  Location      1             2             3")
    print("    fan")
    
    for i in range(3):
        print("     %d      " % (i+1),end="")
        for j in range(3):
            print("%6.3f (%-5s)" % (data[j + (i * 3)]),end="")
        print()

    print()
    print("FOILS:")
    for i in range(3):
        print("     %d      " % (i+1),end="")
        for j in range(3):
            print("%6.3f (%-5s)" % (data[j + ((i + 3) * 3)]),end="")
        print()
