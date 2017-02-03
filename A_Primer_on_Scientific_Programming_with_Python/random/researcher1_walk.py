def random_walk_2D(np, ns, plot_step):
    xpositions = zeros(np)
    ypositions = zeros(np)
    # extent of the axis in the plot:
    xymax = 3*sqrt(ns); xymin = -xymax

    NORTH = 1;  SOUTH = 2;  WEST = 3;  EAST = 4  # constants

    for step in range(ns):
        for i in range(np):
            direction = random.randint(1, 4)
            if direction == NORTH:
                ypositions[i] += 1
            elif direction == SOUTH:
                ypositions[i] -= 1
            elif direction == EAST:
                xpositions[i] += 1
            elif direction == WEST:
                xpositions[i] -= 1

        # plot just every plot_step steps:
        if (step+1) % plot_step == 0:
            plot(xpositions, ypositions, 'ko',
                 axis=[xymin, xymax, xymin, xymax],
                 title='%d researchers after %d months, without strategy' % \
                       (np, step+1),
                 hardcopy='tmp_%03d.eps' % (step+1))
    return xpositions, ypositions

# main program:
import random
random.seed(10)
import sys
from scitools.std import zeros, plot, sqrt, movie

np        = int(sys.argv[1])  # number of particles
ns        = int(sys.argv[2])  # number of steps
plot_step = int(sys.argv[3])  # plot every plot_step steps
x, y = random_walk_2D(np, ns, plot_step)
#movie('tmp_*.eps', encoder='mpeg_encode')
