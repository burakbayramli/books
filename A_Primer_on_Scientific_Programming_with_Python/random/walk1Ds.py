"""As walk1Dp.py but with computing statistics."""
import random
import time, sys
import numpy
from scitools.std import plot, hardcopy, compute_histogram
random.seed(11)

try:
    np = int(sys.argv[1])
    ns = int(sys.argv[2])
except IndexError:
    np = 100   # no of particles
    ns = 100   # no of steps

positions = numpy.zeros(np)      # all particles start at x=0
HEAD = 1;  TAIL = 2              # constants

xmax = numpy.sqrt(ns); xmin = -xmax  # initial extent of plot axis
y = positions.copy()             # y position is always 0
nbins = int(3*xmax)              # no of intervals in histogram
ymax_prev = 1                    # for axis in plot, prev. step
ymin = -0.1                      # min limit for y axis

for step in range(ns):
    for p in range(np):
        coin = random.randint(1,2)  # flip coin
        if coin == HEAD:
            positions[p] += 1   # one unit length to the right
        elif coin == TAIL:
            positions[p] -= 1   # one unit length to the left

    # Statistics
    mean_pos = numpy.mean(positions)
    stdev_pos = numpy.std(positions)

    pos, freq = compute_histogram(positions, nbins,
                                  piecewise_constant=True)


    # Extend x axis limits?
    if min(positions) < xmin:  xmin -= 2*numpy.sqrt(ns)
    if max(positions) > xmax:  xmax += 2*numpy.sqrt(ns)

    # "Intelligent" choice of y axis limits:
    # estimate ymax on the y axis from 1.1*max(freq)
    # (1.1 to get some space), do not change ymax
    # unless it deviates more than 0.1 from the previous
    # value, and let ymin = -0.1*ymax.
    
    ymax = 1.1*max(freq)  # axis for freq
    if abs(ymax - ymax_prev) < 0.1:
        # Do not change axis
        ymax = ymax_prev
    else:
        # Keep new value
        ymax_prev = ymax
        ymin = -0.1*ymax

    # Graphical vertical lines for mean and +/- stdev:
    yminv, ymaxv = 0, ymax/1.1  # axis for vert. lines
    xmean, ymean   = [mean_pos, mean_pos],     [yminv, ymaxv]
    xstdv1, ystdv1 = [stdev_pos, stdev_pos],   [yminv, ymaxv]
    xstdv2, ystdv2 = [-stdev_pos, -stdev_pos], [yminv, ymaxv]

    plot(positions, y, 'ko3',     # particles as circles
         pos, freq, 'r',          # histogram
         xmean, ymean, 'r2',      # mean position as thick line
         xstdv1, ystdv1, 'b2',    # +1 standard dev.
         xstdv2, ystdv2, 'b2',    # -1 standard dev.
         axis=[xmin, xmax, ymin, ymax],
         title='random walk of %d particles after %d steps' % \
               (np, step+1))
    time.sleep(0.2)             # pause before next move
savefig('tmp.eps')
