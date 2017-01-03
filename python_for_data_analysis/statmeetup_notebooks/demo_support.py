from numpy import nan
from numpy.random import randn
import numpy as np
np.set_printoptions(precision=4)


from pandas.core.common import adjoin
from pandas import *
from pandas.io.data import DataReader
import pandas.util.testing as tm
tm.N = 10

import statsmodels.api as sm
import statsmodels.datasets as datasets

from qqplot import *

import matplotlib.pyplot as plt
from matplotlib import cm
import matplotlib as mpl
import scipy.stats as stats
import matplotlib.pyplot as plt

# import pandas.rpy.common as com

# baseball = com.load_data('baseball', package='plyr')
# ff = com.load_data('french_fries', package='reshape2')

baseball = DataFrame.load('baseball')
ff = DataFrame.load('ff')

plt.rc('figure', figsize=(10, 6))
np.random.seed(123456)

panel = Panel.load('data_panel')
panel = panel.drop(['IBM'], axis='minor')

close_px = panel['close']

# convert closing prices to returns
rets = close_px / close_px.shift(1) - 1

def side_by_side(*objs, **kwds):
    space = kwds.get('space', 4)

    reprs = [repr(obj).split('\n') for obj in objs]
    print adjoin(space, *reprs)

def plot_acf_multiple(ys, lags=20):
    """

    """
    from statsmodels.tsa.stattools import acf
    # hack
    old_size = mpl.rcParams['font.size']
    mpl.rcParams['font.size'] = 8

    plt.figure(figsize=(10, 10))
    xs = np.arange(lags + 1)

    acorr = np.apply_along_axis(lambda x: acf(x, nlags=lags), 0, ys)

    k = acorr.shape[1]
    for i in range(k):
        ax = plt.subplot(k, 1, i + 1)
        ax.vlines(xs, [0], acorr[:, i])

        ax.axhline(0, color='k')
        ax.set_ylim([-1, 1])

        # hack?
        ax.set_xlim([-1, xs[-1] + 1])

    mpl.rcParams['font.size'] = old_size

def load_mplrc():
    import re

    path = 'matplotlibrc'
    regex = re.compile('(.*)[\s]+:[\s]+(.*)[\s]+#')
    for line in open(path):
        m = regex.match(line)
        if not m:
            continue

        cat, attr = m.group(1).strip().rsplit('.', 1)
        plt.rc(cat, **{attr : m.group(2).strip()})


load_mplrc()
