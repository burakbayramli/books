from mpi4py import MPI
import numpy as np

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

NumIntervals = 100000

h = 1.0/NumIntervals
myPieceOfPi = np.array([0.0])
for i in range(rank, NumIntervals, size):
    x = h*(i-0.5) #center of interval
    myPieceOfPi += 4.0*h / (1.0+x**2)

wholePi = np.empty(1)
comm.Reduce(myPieceOfPi, wholePi, op=MPI.SUM, root=0)

if rank == 0:
    print "Pi=", wholePi, " error=", np.abs(wholePi - np.pi)
