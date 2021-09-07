
"""
Alternative interact function that creates a JSAnimation figure that can
be viewed online, e.g. on Github or nbviewer.
"""

from __future__ import print_function

print("Will create JSAnimation figures instead of interactive widget")

def interact(f, **kwargs):
    from utils import animation_tools
    from IPython.display import display
    from numpy import arange
    from matplotlib import pyplot as plt
    fargs = {}
    for key in kwargs.keys():
        try:
            fargs[key] = kwargs[key].value
        except:
            pass  # if initial value not set for this parameter
    if ('t' in fargs.keys()):
        t0 = kwargs['t'].min
        t1 = kwargs['t'].max
        try:
            dt = kwargs['t'].step
        except:
            dt = (t1 - t0)/10.
        times = arange(t0,t1+dt,dt)
        figs = []
        for t in times:
            fargs['t'] = t
            fig = plt.figure()
            fargs['fig'] = fig
            f(**fargs)
            figs.append(fig)
            plt.close(fig)
        images = animation_tools.make_images(figs)
        anim = animation_tools.JSAnimate_images(images, figsize=(8,4))
        display(anim)
    else:
        # just make one static plot if 't' is not a key:
        f(**fargs)
