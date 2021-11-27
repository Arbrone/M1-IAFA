# version sequentielle
# python3 n-bodies.py 12 1000

# version parallele
# mpirun -n 3 python3 n-bodies.py 12 1000

from mpi4py import MPI
import sys
import math
import random
import matplotlib.pyplot as plt
import time

# split a vector "x" in "size" part. In case it does not divide well, the last one receives one less than others


def split(x, size):
    n = math.ceil(len(x) / size)
    return [x[n*i:n*(i+1)] for i in range(size-1)]+[x[n*(size-1):len(x)]]

# unsplit a list x composed of lists


def unsplit(x):
    y = []
    n = len(x)
    for i in range(n):
        for j in range(len(x[i])):
            y.append(x[i][j])
    return y


solarmass = 1.98892e30


def circlev(rx, ry):
    r2 = math.sqrt(rx*rx+ry*ry)
    numerator = (6.67e-11)*1e6*solarmass
    return math.sqrt(numerator/r2)

# from http://physics.princeton.edu/~fpretori/Nbody/code.htm


class Data_item:

    def __init__(self, id, positionx, positiony, speedx, speedy, weight):
        self.id = id
        self.positionx = positionx
        self.positiony = positiony
        self.weight = weight

        # the center of the world, very heavy one...
        if positionx == 0 and positiony == 0:
            self.speedx = 0
            self.speedy = 0
        else:
            if speedx == 0 and speedy == 0:			# initial values
                magv = circlev(positionx, positiony)
                absangle = math.atan(math.fabs(positiony/positionx))
                thetav = math.pi/2-absangle
                phiv = random.uniform(0, 1)*math.pi
                self.speedx = -1 * \
                    math.copysign(1, positiony)*math.cos(thetav)*magv
                self.speedy = math.copysign(1, positionx)*math.sin(thetav)*magv
                # Orient a random 2D circular orbit
                if (random.uniform(0, 1) <= .5):
                    self.speedx = -self.speedx
                    self.speedy = -self.speedy
            else:
                self.speedx = speedx
                self.speedy = speedy

    def __str__(self):
        return "ID="+str(self.id)+" POS=("+str(self.positionx)+","+str(self.positiony)+") SPEED=("+str(self.speedx)+","+str(self.speedy)+") WEIGHT="+str(self.weight)


def display(m, l):
    for i in range(len(l)):
        print("PROC"+str(rank)+":"+m+"-"+str(l[i]))


def displayPlot(d):
    plt.gcf().clear()			# to remove to see the traces of the particules...
    plt.axis((-1e17, 1e17, -1e17, 1e17))
    xx = [d[i].positionx for i in range(len(d))]
    yy = [d[i].positiony for i in range(len(d))]
    plt.plot(xx, yy, 'ro')
    plt.draw()
    plt.pause(0.00001)			# in order to see something otherwise too fast...


def interaction(i, j):
    dist = math.sqrt((j.positionx-i.positionx)*(j.positionx-i.positionx) +
                     (j.positiony-i.positiony)*(j.positiony-i.positiony))
    if i == j:
        return (0, 0)
    g = 6.673e-11
    factor = g * i.weight * j.weight / (dist*dist+3e4*3e4)
    return factor * (j.positionx-i.positionx) / dist, factor * (j.positiony-i.positiony) / dist


def update(d, f):
    dt = 1e11
    vx = d.speedx + dt * f[0]/d.weight
    vy = d.speedy + dt * f[1]/d.weight
    px = d.positionx + dt * vx
    py = d.positiony + dt * vy
    return Data_item(id=d.id, positionx=px, positiony=py, speedx=vx, speedy=vy, weight=d.weight)


def signature(world):
    s = 0
    for d in world:
        s += d.positionx+d.positiony
    return s


def init_world(n):
    data = [Data_item(id=i, positionx=1e18*math.exp(-1.8)*(.5-random.uniform(0, 1)), positiony=1e18*math.exp(-1.8) *
                      (.5-random.uniform(0, 1)), speedx=0, speedy=0, weight=(random.uniform(0, 1)*solarmass*10+1e20)) for i in range(n-1)]
    data.append(Data_item(id=nbbodies-1, positionx=0, positiony=0,
                speedx=0, speedy=0, weight=1e6*solarmass))
    return data

#########################################
#										#
#			   SELF FUNC     			#
#										#
#########################################

def sum_forces(list_forces):
	total = (0,0)
	for i in range(len(list_forces)):
		total = (total[0]+list_forces[i][0], total[1]+list_forces[i][1])
	return total

def update_world(world, debut, fin):
    res = world
    for i in range(debut, fin):
        force = []
        for j in world:
            force.append(interaction(world[i], j))
        res[i] = update(world[i],sum_forces(force))
    return res

def calc_force(world, debut, fin,nbbodies):
    for i in range(debut, fin):
        force = []
        for j in world:
            force.append(interaction(world[i], j))
        res[i] = update(world[i],sum_forces(force))
    return res

def work_range(rank, size, nbbodies):
    debut = rank * nbbodies//size
    if rank == size-1:
        fin = nbbodies
    else:
        fin = debut + nbbodies//size
    return(debut, fin)

#########################################
#										#
#			    PARA BLOCK  			#
#										#
#########################################

nbbodies = int(sys.argv[1])
NBSTEPS = int(sys.argv[2])

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()


if rank == 0:
    # à modifier si on veut que le monde créé soit différent à chaque fois
    random.seed(0)

    plt.draw()
    plt.show(block=False)
    world = init_world(nbbodies)
else:
    world = {}

debut, fin = work_range(rank, size, nbbodies)

print("-------------------------")
print("rank : ",rank)
print("debut : ",debut)
print("fin : ",fin)
print("world len : ", len(world))
print("-------------------------")

# mettre tout le tableau monde à 0
# faire la somme en ajoutant 

for i in range(NBSTEPS):

    world = comm.bcast(world,root=0)
    updated_local = update_world(world, debut, fin)

    if rank == 0:
        for j in range(size):
            get_req = comm.send(world,dest=0,tag=11)
        # recevoir les nouveaux mondes
        # afficher
        pass
    else:
        # envoyer local world

        # synchroniser les threads
         
        # recevoir nouveau monde 
        pass


local_data = world[debut:fin+1]

    # envoi des resultats de calculs sur le rank 0
send_req = comm.send(world,dest=0,tag=11)


#afficher le nouveau world sur le rank 0
if rank == 0:
    for i in range(NBSTEPS):
        req = comm.irecv(MPI.ANY_SOURCE,tag=11)
        world = req.wait()
        displayPlot(world)
    
    sig = signature(world)
    print("world signature : ", sig)
#recommencer tant que step != NBSTEPS