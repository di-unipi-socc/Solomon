import random
import pyswip

import time
import json

SIMS = 30

DEPTS = [2,4,6,8]
ROOMS = 100
_USERS = 500
_GUESTS = 250
MAX_LAMP_ONOFF = 5
MAX_LAMP_DIMM = 5

_maxTemperatureConsumption = 50
_maxLightConsumption = 50

SEED = 1

FILE = "input.pl"

random.seed(SEED)

RES = {}

def my_query(s, prolog):
    q = prolog.query(s)
    result = next(q) 
    return result

for DEPT in DEPTS:
    mediate = 0
    associate = 0
    optimise = 0
    delta = 0
    tot = 0

    USERS = _USERS*DEPT
    GUESTS = _GUESTS*DEPT
    maxTemperatureConsumption = _maxTemperatureConsumption*DEPT   
    maxLightConsumption = _maxLightConsumption*DEPT
    for _ in range(SIMS):

        with open(FILE, "w+") as f:
            f.write(f":- discontiguous user/2.\n\
        :- discontiguous roles/2.\n\
        :- discontiguous sensor/2.\n\
        :- discontiguous actuator/2.\n\
        :- discontiguous actuatorType/2.\n\
        :- discontiguous actuatorCapacity/2.\n\
        :- discontiguous actuatorConsumption/2.\n\
        :- discontiguous zone/1.\n\
        :- discontiguous subzone/2.\n\
        :- discontiguous set/4.\n\
        :- discontiguous propertyInstance/5.\n\
        :- discontiguous sensorValue/2.\n\
        :- discontiguous actuatorSetting/2.\n\
        \n\
        :- use_module(library(lists)).\n\
        :- consult(policies).\n\
        \n\
        \n\
        maxTemperatureConsumption({maxTemperatureConsumption}).\n\
        maxLightConsumption({maxLightConsumption}).\n\
        \n\
        :- write('DEPTS: '), writeln({DEPT}),\
            write('ROOMS: '), writeln({ROOMS}),\
            write('USERS: '), writeln({USERS}),\
            write('GUESTS: '), writeln({GUESTS}),\
            write('MAX_LAMP_ONOFF: '), writeln({MAX_LAMP_ONOFF}),\
            write('MAX_LAMP_DIMM: '), writeln({MAX_LAMP_DIMM}),\
            write('maxTemperatureConsumption: '), writeln({maxTemperatureConsumption}),\
            write('maxLightConsumption: '), writeln({maxLightConsumption}).\
        \n\
        \n\
        minimumAllowedTemperature(18).\n\
        maximumAllowedTemperature(25).\n\
        \n\
        \n\
        propertyType(temp).\n\
        propertyType(light).\n\
        \n\
        \n\
        zone(campus).\n\
        zone(public_campus).\n\
        subzone(public_campus, campus).\n\n\n")
            

            f.write(f"actuator(heating_public, temp).\nactuatorConsumption(heating_public, {random.randint(1,10)}).\n\n")



            for d in range(DEPT):

                for l in range(MAX_LAMP_ONOFF):
                    f.write(f"actuator(lampOnOff{l}_dept{d}_public, light).\nactuatorConsumption(lampOnOff{l}_dept{d}_public, {random.randint(1,10)}).\n")
                    f.write(f"actuatorCapacity(lampOnOff{l}_dept{d}_public, {random.randint(15,100)}).\n")
                    f.write(f"actuatorType(lampOnOff{l}_dept{d}_public, onOff).\n\n")
                    f.write(f"actuatorSetting(lampOnOff{l}_dept{d}_public, {random.choice([0,100])}).\n\n")

                for l in range(MAX_LAMP_DIMM):
                    f.write(f"actuator(lampDimm{l}_dept{d}_public, light).\nactuatorConsumption(lampDimm{l}_dept{d}_public, {random.randint(1,10)}).\n")
                    f.write(f"actuatorCapacity(lampDimm{l}_dept{d}_public, {random.randint(15,100)}).\n")
                    f.write(f"actuatorType(lampDimm{l}_dept{d}_public, dimm).\n")
                    f.write(f"actuatorSetting(lampDimm{l}_dept{d}_public, {random.randint(0,100)}).\n\n")

                f.write(f"sensor(temperature_dept{d}_public, temp).\n")
                f.write(f"sensorValue(temperature_dept{d}_public, {random.randint(15, 32)}).\n")
                f.write(f"sensor(light_dept{d}_public, light).\n")
                f.write(f"sensorValue(light_dept{d}_public, {random.randint(0, 100)}).\n\n")
                
                f.write(f"zone(dept{d}).\n")
                f.write(f"subzone(dept{d}, campus).\n\n")

                f.write(f"zone(dept{d}_public).\n")
                f.write(f"subzone(dept{d}_public, public_campus).\n")
                f.write(f"subzone(dept{d}_public, dept{d}).\n\n")

                f.write(f"propertyInstance(dept{d}_public, temperature, temp, [heating_public], [temperature_dept{d}_public]).\n")
                
                lamps = []
                for l in range(random.randint(0, MAX_LAMP_ONOFF)):
                    lamps.append(f"lampOnOff{l}_dept{d}_public")   
                for l in range(random.randint(0, MAX_LAMP_DIMM)):
                    lamps.append(f"lampDimm{l}_dept{d}_public")

                f.write(f"propertyInstance(dept{d}_public, light, light, {lamps}, [light_dept{d}_public]).\n\n".replace("'",""))

                f.write(f"zone(dept{d}_private).\n")
                f.write(f"subzone(dept{d}_private, dept{d}).\n\n")

                for r in range(ROOMS):
                    f.write(f"actuator(heating_dept{d}_private_{r}, temp).\nactuatorConsumption(heating_dept{d}_private_{r}, {random.randint(1,10)}).\n\n")

                onoff = random.randint(0, MAX_LAMP_ONOFF)
                dimm = random.randint(0, MAX_LAMP_DIMM)

                for l in range(onoff):
                    f.write(f"actuator(lampOnOff{l}_dept{d}_private_{r}, light).\nactuatorConsumption(lampOnOff{l}_dept{d}_private_{r}, {random.randint(1,10)}).\n")
                    f.write(f"actuatorCapacity(lampOnOff{l}_dept{d}_private_{r}, {random.randint(15,100)}).\n")
                    f.write(f"actuatorType(lampOnOff{l}_dept{d}_private_{r}, onOff).\n")
                    f.write(f"actuatorSetting(lampOnOff{l}_dept{d}_private_{r}, {random.choice([0,100])}).\n\n")

                for l in range(dimm):
                    f.write(f"actuator(lampDimm{l}_dept{d}_private_{r}, light).\nactuatorConsumption(lampDimm{l}_dept{d}_private_{r}, {random.randint(1,10)}).\n")
                    f.write(f"actuatorCapacity(lampDimm{l}_dept{d}_private_{r}, {random.randint(15,100)}).\n")
                    f.write(f"actuatorType(lampDimm{l}_dept{d}_private_{r}, dimm).\n")
                    f.write(f"actuatorSetting(lampDimm{l}_dept{d}_private_{r}, {random.randint(0,100)}).\n\n")


                    f.write(f"sensor(temperature_dept{d}_private_{r}, temp).\n")
                    f.write(f"sensorValue(temperature_dept{d}_private_{r}, {random.randint(15, 32)}).\n")
                    f.write(f"sensor(light_dept{d}_private_{r}, light).\n")
                    f.write(f"sensorValue(light_dept{d}_private_{r}, {random.randint(0, 100)}).\n\n")

                    f.write(f"zone(dept{d}_private_room{r}).\n")
                    f.write(f"subzone(dept{d}_private_room{r}, dept{d}_private).\n\n")
                    f.write(f"propertyInstance(dept{d}_private_room{r}, temperature, temp, [heating_dept{d}_private_{r}], [temperature_dept{d}_private_{r}]).\n")
                
                    lamps = []
                    for l in range(onoff):
                        lamps.append(f"lampOnOff{l}_dept{d}_private_{r}")   
                    for l in range(dimm):
                        lamps.append(f"lampDimm{l}_dept{d}_private_{r}")

                    f.write(f"propertyInstance(dept{d}_private_room{r}, light, light, {lamps}, [light_dept{d}_private_{r}]).\n\n".replace("'",""))

            d = random.randint(0, DEPT-1)
            r = random.randint(0, ROOMS-1)
            f.write(f"user(the_rector, [campus]).\n")
            f.write(f"roles(the_rector, [rector, professor]).\n")
            f.write(f"set(the_rector, dept{d}_private_room{r}, temperature, {random.randint(14, 34)}).\n")
            f.write(f"set(the_rector, dept{d}_private_room{r}, light, {random.choice(['low', 'medium', 'high'])}).\n\n")

            for d in range(DEPT):
                f.write(f"user(head_dept{d}, [public_campus, dept{d}]).\n")
                f.write(f"roles(head_dept{d}, [head_dept, professor]).\n")
                f.write(f"set(head_dept{d}, dept{d}_public, temperature, {random.randint(14, 34)}).\n")
                f.write(f"set(head_dept{d}, dept{d}_public, light, {random.choice(['low', 'medium', 'high'])}).\n\n")


            for u in range(USERS):
                role = random.choice(["professor","researcher","secretary"])

                d = random.randint(0, DEPT-1)
                r = random.randint(0, ROOMS-1)

                if role == "professor" or role == "researcher":
                    f.write(f"user(user{u}, [public_campus, dept{d}_private_room{r}]).\n")
                else:
                    f.write(f"user(user{u}, [public_campus]).\n")
                f.write(f"roles(user{u}, [{role}]).\n")

                if role == "professor" or role == "researcher":
                    f.write(f"set(user{u}, dept{d}_private_room{r}, temperature, {random.randint(14, 34)}).\n")
                    f.write(f"set(user{u}, dept{d}_private_room{r}, light, {random.choice(['low', 'medium', 'high'])}).\n\n")
                else:
                    f.write(f"set(user{u}, dept{d}_public, temperature, {random.randint(14, 34)}).\n")
                    f.write(f"set(user{u}, dept{d}_public, light, {random.choice(['low', 'medium', 'high'])}).\n\n")

            for g in range(GUESTS):
                d = random.randint(0, DEPT-1)
                f.write(f"user(guest{g}, [public_campus]).\n")
                f.write(f"roles(guest{g}, []).\n")
                f.write(f"set(guest{g}, dept{d}_public, temperature, {random.randint(14, 34)}).\n")
                f.write(f"set(guest{g}, dept{d}_public, light, {random.choice(['low', 'medium', 'high'])}).\n\n")
        
        time.sleep(1)

        prolog = pyswip.Prolog()
        prolog.consult("input.pl")
        time.sleep(1)

        start_time = time.time()
        res = my_query("make, time_react(M,A,O,T).",prolog)
        end_time = time.time()

        delta += end_time - start_time
        mediate += res["M"]
        associate += res["A"]
        optimise += res["O"]
        tot += res["T"]

        print(f'{res["M"]}, {res["A"]}, {res["O"]}, {res["T"]}, {end_time - start_time}')

        with open("raw.csv", "a") as f:
            f.write(f'{DEPT}, {res["M"]}, {res["A"]}, {res["O"]}, {res["T"]}, {end_time - start_time}\n')

    mediate /= SIMS
    associate /= SIMS
    optimise /= SIMS
    delta /= SIMS
    tot /= SIMS
    print(f"*** {mediate}, {associate}, {optimise}, {tot}, {delta} ***")

    RES[DEPT] = {
        "mediate": mediate,
        "associate": associate,
        "optimise": optimise,
        "delta": delta,
        "tot": tot
    }

    with open("results.json", "w") as f:
        json.dump(RES, f)

print(RES)







