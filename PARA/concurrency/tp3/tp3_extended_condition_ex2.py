import sys
import time
import random
from multiprocessing import Process, Lock, Condition, Value, Array

class ExtendedCondition:
    def __init__(self, lock):
        self.lock = lock
        self.rank = Value('i', 1)
        self.cond = [Condition(self.lock), Condition(self.lock)]
        self.waiting = Array('i', [0]*2, lock=False)

    def wait(self, rank=1):
        self.rank.value = rank
        self.waiting[rank] += 1
        self.cond[rank].wait()
        self.waiting[rank] -= 1

    def notify(self):
        if self.waiting[0] > 0:
            self.cond[0].notify()
        else:
            self.cond[1].notify()
        pass

    def empty(self):
        return self.waiting[self.rank] == 0

class EI:
    def __init__(self, NBI):
        self.lock = Lock()
        self.cond = ExtendedCondition(self.lock)        

        self.NBI = NBI
        self.iso_used = Value('i',0)
            
    def get_into(self, disability):
        with self.lock:
            while(self.iso_used.value == self.NBI):
                self.cond.wait(disability)
                self.iso_used.value += 1

    def vote(self,identifier):
        print("Vote effectué pour ", identifier)

    def get_out(self):
        self.iso_used.value -= 1
        self.cond.notify()

def process(identifier, synchro):
    disability = random.randint(0,1)
    print("Handicap pour {} {}", identifier, disability)
    
    synchro.get_into(disability)
    synchro.vote(identifier)
    synchro.get_out()
    
        
if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: %s <Nb electeur> <Nb isoloir> \n" % sys.argv[0])
        sys.exit(1)

    NBE = int(sys.argv[1])
    NBI = int(sys.argv[2])

    synchro = EI(NBE)
    
    processes = []

    for id_electeur in range(NBE):
        electeur = Process(target=process, args=(id_electeur,synchro))
        electeur.start()
        processes.append(electeur)

    for process in processes:
        process.join()
