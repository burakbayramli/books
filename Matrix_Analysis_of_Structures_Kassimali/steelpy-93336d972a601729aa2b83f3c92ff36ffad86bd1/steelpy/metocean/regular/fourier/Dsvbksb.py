#
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
from array import array
#from dataclasses import dataclass
#import math
#from typing import NamedTuple, Tuple, Union, List, Dict


# package imports
import numpy as np

#
#
def dsvbksb(u: array, w: array, v: array, m: int, n: int, b: array):
    """
    """
    #1 / 0
    tmp = np.array([np.sum([u[i][j] * b[i] for i in range(1, m + 1)]) / w[j]
                    if w[j] != 0 else 0 for j in range(n + 1)])
    #
    x = np.sum(v[:n + 1, :n + 1] * tmp[:n + 1], axis=1)
    #x = [np.sum([v[j][jj] * tmp[jj] for jj in range(n + 1)])
    #     for j in range(n + 1)]
    return np.array(x)
#
#
#
