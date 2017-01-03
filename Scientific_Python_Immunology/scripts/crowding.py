"""Simulation of bacterial colony patterns with CA

From http://www.math.ubc.ca/~keshet/pubs/BardLeahCA.pdf
JTB 1993, 160:97

"""

import numpy
import pylab    

if __name__ == '__main__':
    a1 = 60 # food for growth
    a2 = 10 # food for sustenance
    T = 8 # time interval beteen cell divisions
    delta = 0.1 # fraction diffusing per time step
    theta = 3000 # threshold for new bacterial cell
    # crowding function influences likelihood of new bacteria formation

    nx = 601 # size in x direction
    ny = 601 # size in y direction
    shape = numpy.array([nx, ny])
    nsteps = 1000 # number of time steps

    crowd = numpy.array([0,40,40,40,30,20,10,0,0]) 
    food = 100.0*numpy.ones(shape)
    cells = numpy.zeros(shape, 'int')
    # seed with initial bacterium in center
    cells[nx/2, ny/2] = 1

    mean = numpy.zeros(shape)
    newcells = numpy.zeros(shape, 'int')
    nbrs = numpy.zeros(shape, 'int')

    k = 0
    for t in range(nsteps):
        print t
        # diffuse
        mean[1:-1,1:-1] = (
            food[0:-2,0:-2]+4*food[0:-2,1:-1]  +food[0:-2,2:] +
            4*food[1:-1,0:-2]                  +4*food[1:-1,2:] +
            food[2:,0:-2]  +4*food[2:,1:-1]  +food[2:,2:])/20.0
        eaten = newcells*a1 + cells*a2
        food = (1-delta)*food + delta*mean - eaten
        food[food<0] = 0
        
        if t%T == 0:
            nbrs[1:-1,1:-1] =  (
                cells[0:-2,0:-2]+cells[0:-2,1:-1]+cells[0:-2,2:] +
                cells[1:-1,0:-2]                 +cells[1:-1,2:] +
                cells[2:,0:-2]  +cells[2:,1:-1]  +cells[2:,2:])
            f = numpy.choose(nbrs, crowd)
            f *= food
            
            r = numpy.random.random(shape)

            newcells[(f > theta) & (r < 0.5)] = 1
            cells[(cells==1) | (newcells==1)] = 1
            pylab.imsave('img%04d.png' % k, cells)
            k += 1
        # print food
