import sys
import os
import time
import random
from multiprocessing import Process, Lock, Condition, Value, Array

class Road:
    def __init__(self):
        self.lock = Lock()

        self.nb_cross  = Value('i', 0)
        self.dir_cross = Value('i',0)
        self.close_cross = Value('i', 0)
        self.allowed = Value ('i', 0)
        self.waiting_list = Array('i',[0]*2, lock=False)
        
        self.cond = [Condition(self.lock), Condition(self.lock)]
        
    def enter_road(self, direction):
        with self.lock:
            while(self.nb_cross.value > 0 or (self.dir_cross == direction and self.waiting_list[(direction+1)%2] ):
                self.waiting_list[direction]+=1
                self.cond[direction].wait()
                self.allowed.value = self.waiting_list[direction]
                self.dir_cross = direction
            

    def exit_road(self):
        pass

def drive(road_type, direction, identifier):
    print("Vehicule %d, coming from %d goes through the %s" % (identifier, direction, road_type))
    time.sleep(random.random())

def vehicule(nb_times, direction, road):
    identifier = os.getpid()
    random.seed(identifier)
    for i in range(nb_times):
        drive("Double road", direction, identifier)
        road.enter_road(direction)
        print("Vehicule %d, coming from %d enters the small road" % (identifier, direction))
        drive("Small road", direction, identifier)
        road.exit_road()
        print("Vehicule %d, coming from %d exits the small road" % (identifier, direction))
    print("Vehicule %d, coming from %d finishes" % (identifier, direction))
        
if __name__ == '__main__':
    if len(sys.argv) != 4:
        print("Usage : %s <Nb vehicules sens O> <Nb vehicules sens 1> <Nb passages sur VU>" % sys.argv[0]);
        sys.exit(1)

    nb_vehicules = [int(sys.argv[1]), int(sys.argv[2])]
    nb_times = int(sys.argv[3])
    
    road = Road()

    processes = []
    for v in range(nb_vehicules[0]):
        v0 = Process(target=vehicule, args=(nb_times, 0, road))
        v0.start()
        processes.append(v0)

    for v in range(nb_vehicules[1]):
        v1 = Process(target=vehicule, args=(nb_times, 1, road))
        v1.start()
        processes.append(v1)
        


    for process in processes:
        process.join()
