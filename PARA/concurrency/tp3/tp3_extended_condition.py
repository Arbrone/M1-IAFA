import sys
import time
import random
from multiprocessing import Process, Lock, Condition, Value

class ExtendedCondition:
    def __init__(self,lock):
        self.lock = lock

    def wait(rank=1):
        pass

    def notify():
        pass

    def empty():
        pass

class RW:
    def __init__(self):
        self.lock = Lock()
        self.nb_read = Value('i', 0)
        self.cond_read  = Condition(self.lock)
        self.cond_write = Condition(self.lock)
        self.writing_on = Value('i', 0)
        self.reading_on = Value('i',0)
        self.cpt = Value('i',0)

    def start_read(self):
        with self.lock:
            while(self.writing_on.value == 1 or self.cpt.value > 0):
                self.cond_read.wait()
            self.nb_read.value = int(self.nb_read.value + 1)
            self.cond_read.notify()

    def end_read(self):
        with self.lock:
            self.nb_read.value = int(self.nb_read.value - 1)
            if(self.nb_read.value == 0):
                self.cond_write.notify()


    def start_write(self):
        with self.lock:
            while (self.writing_on.value == 1 or self.nb_read.value > 0):
                self.cpt.value = int(self.cpt.value + 1)
                self.cond_write.wait()
                self.cpt.value = int(self.cpt.value - 1)
            self.writing_on.value = int(1)


    def end_write(self):
        with self.lock:
            self.writing_on.value = int(0)
            self.cond_read.notify()
            self.cond_write.notify()

def process_writer(identifier, synchro):
    synchro.start_write()
    for _ in range(5):
        with open('LectRed_shared', 'a') as file_id:
            txt=' '+str(identifier)
            file_id.write(txt)
            print('Writer', identifier, 'just wrote', txt)
        time.sleep(.1 + random.random())            
    synchro.end_write()
    
def process_reader(identifier, synchro):
    synchro.start_read()
    position = 0
    result = ''
    while True:
        time.sleep(.1 + random.random())            
        with open('LectRed_shared', 'r') as file_id:
            file_id.seek(position, 0)
            txt = file_id.read(1)
            if len(txt) == 0:
                break
            print('Reader', identifier, 'just read', txt)
            result += txt
            position+=1
    print(str(identifier)+':',result)
    synchro.end_read()
        
if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: %s <Nb reader> <Nb writer> \n" % sys.argv[0])
        sys.exit(1)

    nb_reader = int(sys.argv[1])
    nb_writer = int(sys.argv[2])

    synchro = RW()

    # To initialize the common data
    with open('LectRed_shared', 'w') as file_id:
        file_id.write('')
    
    processes = []
    for id_writer in range(nb_writer):
        writer = Process(target=process_writer, args=(id_writer,synchro))
        writer.start()
        processes.append(writer)

    for id_reader in range(nb_reader):
        reader = Process(target=process_reader, args=(id_reader,synchro))
        reader.start()
        processes.append(reader)

    for process in processes:
        process.join()
