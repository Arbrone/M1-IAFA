#! /usr/bin/python3

import time
import random
import sys
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

if rank == 0:
    start_time = time.time()
    nb = [1000010000]
else:
    nb = {}


local_nb = comm.bcast(nb, root=0)
local_nb = local_nb[0]//size
# local_nb = comm.scatter(nb, root=0)//size
local_inside = 0
random.seed(rank)
for _ in range(local_nb):
    x = random.random()
    y = random.random()
    if x*x + y*y <= 1:
        local_inside += 1

# info entendu faire un all reduce
res = comm.reduce(local_inside, op=MPI.SUM, root=0)

if rank == 0:
    end_time = time.time()
    print("Pi =", 4 * res/nb[0], "in ", end_time-start_time, 'seconds')
