
"""
Alternative interact function that creates a single static figure that can
be viewed online, e.g. on Github or nbviewer, or for use with nbconvert.
"""

from __future__ import print_function

print("Will create static figures with single value of parameters")

def interact(f, **kwargs):
    fargs = {}
    for key in kwargs.keys():
        try:
            fargs[key] = kwargs[key].value
        except:
            pass  # if initial value not set for this parameter
    f(**fargs)
    if ('t' in fargs.keys()):
        if fargs['t'] == 0:
            fargs['t'] = 0.2
            f(**fargs)
