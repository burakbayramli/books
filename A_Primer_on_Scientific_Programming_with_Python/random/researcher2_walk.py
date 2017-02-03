def random_walk_2D(np, ns, plot_step):
    xpositions = zeros(np)
    ypositions = zeros(np)
    # extent of the axis in the plot:
    ymin = -4
    ymax = 8*sqrt(ns)
    xmax = 3*sqrt(ns); xmin = -xmax

    for step in range(ns):
        for i in range(np):
            r = random.randint(0, 10)
            if 0 <= r <= 5:
                ypositions[i] += 1
            elif r == 6:
                ypositions[i] -= 1
            elif 7 <= r <= 8:
                xpositions[i] += 1
            elif 9 <= r <= 10:
                xpositions[i] -= 1

        # plot just every plot_step steps:
        if (step+1) % plot_step == 0:
            plot(xpositions, ypositions, 'ko',
                 axis=[xmin, xmax, ymin, ymax],
                 title='%d researchers after %d months, with ownership of strategy' % \
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
