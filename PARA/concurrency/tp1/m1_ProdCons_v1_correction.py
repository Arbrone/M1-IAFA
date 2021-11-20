import time
import random
import sys
from multiprocessing import Process, Lock, Condition, Value, Array

class Buffer:
    def __init__(self, nb_cases):
        self.nb_cases = nb_cases
        self.lock = Lock()
        self.cond_prod = Condition(self.lock)
        self.cond_cons = Condition(self.lock)
        self.storage_val = Array('i', [-1] * nb_cases, lock=False)
        self.storage_type = Array('i', [-1] * nb_cases, lock=False)
        self.ptr_prod = Value('i', 0, lock=False)
        self.ptr_cons = Value('i', 0, lock=False)
        self.empty = Value('i', nb_cases, lock=False)

    def produce(self, msg_val, msg_type, msg_source):
        with self.lock:
            while self.empty.value == 0:
                self.cond_prod.wait()
            self.empty.value -= 1
            position = self.ptr_prod.value
            self.storage_val[position] = msg_val
            self.storage_type[position] = msg_type
            self.ptr_prod.value = (position + 1) % self.nb_cases
            print('%3d produced %3d (type:%d) in place %3d and the buffer is\t\t %s' %
                  (msg_source, msg_val, msg_type, position, self.storage_val[:]))
            self.cond_cons.notify()

    def consume(self, id_cons):
        result = None
        with self.lock:
            while self.empty.value == self.nb_cases:
                self.cond_cons.wait()
            self.empty.value += 1
            position = self.ptr_cons.value
            result = self.storage_val[position]
            result_type = self.storage_type[position]
            self.storage_val[position] = -1
            self.storage_type[position] = -1
            self.ptr_cons.value = (position + 1) % self.nb_cases
            print('\t%3d consumed %3d (type:%d) in place %3d and the buffer is\t %s' %
                  (id_cons, result, result_type, position, self.storage_val[:]))
            self.cond_prod.notify()
        return result


def producer(msg_val, msg_type, msg_source, nb_times, buffer):
    for _ in range(nb_times):
        time.sleep(.1 + random.random())
        buffer.produce(msg_val, msg_type, msg_source)
        msg_val += 1

def consumer(id_cons, nb_times, buffer):
    for _ in range(nb_times):
        time.sleep(.5 + random.random())
        buffer.consume(id_cons)


if __name__ == '__main__':
    if len(sys.argv) != 6:
        print("Usage: %s <Nb Prod <= 20> <Nb Conso <= 20> <Nb Cases <= 20> <Nb iter Cons> <Nb iter Cons>" % sys.argv[0])
        sys.exit(1)

    nb_prod = int(sys.argv[1])
    nb_cons = int(sys.argv[2])
    nb_cases = int(sys.argv[3])
    nb_times_prod = int(sys.argv[4])
    nb_times_cons = int(sys.argv[5])

    buf = Buffer(nb_cases)

    producers, consumers = [], []

    for id_prod in range(nb_prod):
        msg_val_start, msg_type, msg_source = id_prod * nb_times_prod, id_prod % 2, id_prod
        prod = Process(target=producer, args=(msg_val_start, msg_type, msg_source, nb_times_prod, buf))
        producers.append(prod)
        prod.start()

    for id_cons in range(nb_cons):
        cons = Process(target=consumer, args=(id_cons, nb_times_cons, buf))
        consumers.append(cons)
        cons.start()

    for prod in producers:
        prod.join()

    for cons in consumers:
        cons.join()
