import time
import random
import sys
from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

def syracuse(n):
    steps = 0
    while n > 1:
        if n%2 == 0:
            n = n//2
        else:
            n = 3*n+1
        steps += 1
    return steps


#########################################
#										#
#			  SEQUENTIELLE  			#
#										#
#########################################

def max_syracuse(n):
    max = 0
    for i in range(n+1):
        res = syracuse(i)
        if res > max:
            max = res
    return max



def total_steps(n):
    if(n > 1):
        return syracuse(n) + total_steps(n-1)
    else:
        return 0

#########################################
#										#
#			    PARA BLOCK  			#
#										#
#########################################

# def get_local_max(debut, fin):
#     max = 0
#     for i in range(debut,fin+1):
#         #print(i)
#         res_syr = syracuse(i) 
#         if res_syr > max:
#             max = res_syr
#     return max

# N = 4
# debut = rank * N//size

# if rank == size-1:
#     fin = N
# else:
#     fin = debut + N//size


# local_max = get_local_max(debut,fin)

# max = comm.reduce(local_max,op=MPI.MAX,root=0)

# if rank == 0:
#     print(max)

#########################################
#										#
#			    PARA DISTRI  			#
#										#
#########################################

if rank == 0:
    N = 4
    max = 0
    data = list(range(1,N+1))

    for i in range(0, len(data)):
        
        req_send = comm.isend([data[i]], dest=i%size, tag=11)
        req_result = comm.irecv(source=0, tag=12)
        print("debut wait")
        res = req_result.wait()
        print("fin wait")
        if res[0] > max:
            max = res
    print(max)
    
else:
    req = comm.irecv(source=0, tag=11)
    # This process can continue working during the communication
    data = req.wait()
    req = comm.isend(syracuse(data[0]), dest=0, tag=12)

# def get_local_max(debut, fin):
#     max = 0
#     for i in range(debut,fin+1, size):
#         #print(i)
#         res_syr = syracuse(i) 
#         if res_syr > max:
#             max = res_syr
#     return max

# N = 4
# debut = rank * N//size

# local_max = get_local_max(debut,N)

# max = comm.reduce(local_max,op=MPI.MAX,root=0)

# if rank == 0:
#     print(max)

