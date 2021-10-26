import os
import sys

import numpy as np
import matplotlib.pyplot as plt

module_path = os.path.abspath(os.path.join('../code/'))
if module_path not in sys.path:
    sys.path.append(module_path)

# Run this line as soon as script is called
plt.style.use('../code/style.mplstyle')

def get_colors(n, start=0.2, end=1, rev=True):
    if rev:
        return plt.cm.gist_heat_r(np.linspace(start, end, n))
    return plt.cm.gist_heat(np.linspace(start, end, n))  