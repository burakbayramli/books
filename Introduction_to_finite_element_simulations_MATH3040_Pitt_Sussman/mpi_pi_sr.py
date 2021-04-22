"""
First program to compute pi using MPI
This uses send/recv pairs
"""
from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

NumIntervals = 100000 # a million gives all printed digits ok

h = 1.0/NumIntervals
myPieceOfPi = np.array([0.0])
for i in range(rank, NumIntervals, size):
    x = h * (i-0.5) #center of interval
    myPieceOfPi += 4.0*h / (1.0+x**2)

leader=0

wholePi=np.empty(1)

if rank == leader:
    assert(leader == 0)
    wholePi = myPieceOfPi.copy()
    for n in range(1, size):
        comm.Recv(myPieceOfPi, source=n)
        wholePi += myPieceOfPi
else:
    comm.Send(myPieceOfPi, dest=leader)
        
if rank == leader:
    print "Pi=", wholePi, " error=", np.abs(wholePi - np.pi)
