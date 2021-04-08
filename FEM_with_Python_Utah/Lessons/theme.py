import sys
import json
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.pylab as pylab
from contextlib import contextmanager
from IPython.core.display import HTML
from os.path import dirname, realpath, join

# Add the root directory to the path so that the femlib files
# are directly importable
D = realpath(dirname(__file__))
sys.path.insert(0, realpath(join(D, '../')))

def equal_axis():
    pylab.rcParams['figure.figsize'] = 10, 10
    plt.axis('equal')

def reset_axis():
    pylab.rcParams['figure.figsize'] = 11, 4

def set_figsize(x=11, y=4):
    pylab.rcParams['figure.figsize'] = x, y

@contextmanager
def figsize(x=11, y=4):
    '''Temporarily set the figure size using 'with figsize(a, b):'

    '''
    size = pylab.rcParams['figure.figsize']
    set_figsize(x, y)
    yield
    pylab.rcParams['figure.figsize'] = size

@contextmanager
def numpy_precision(precision):
    old = np.get_printoptions()['precision']
    np.set_printoptions(precision=precision)
    yield
    np.set_printoptions(old)

def _decode_list(data):
    rv = []
    for item in data:
        if isinstance(item, unicode):
            item = item.encode('utf-8')
        elif isinstance(item, list):
            item = _decode_list(item)
        elif isinstance(item, dict):
            item = _decode_dict(item)
        rv.append(item)
    return rv

def _decode_dict(data):
    rv = {}
    for key, value in data.iteritems():
        if isinstance(key, unicode):
            key = key.encode('utf-8')
        if isinstance(value, unicode):
            value = value.encode('utf-8')
        elif isinstance(value, list):
            value = _decode_list(value)
        elif isinstance(value, dict):
            value = _decode_dict(value)
        rv[key] = value
    return rv

def load_style(directory=D, name='styles/custom2.css'):
    f = join(directory, "styles/538.json")
    if sys.version_info[0] >= 3:
        s = json.load(open(f))
    else:
        s = json.load(open(f), object_hook=_decode_dict)
    plt.rcParams.update(s)
    reset_axis()
    np.set_printoptions(suppress=True)
    f = join(directory, name)
    style = open(f, 'r').read()
    return HTML(style)
