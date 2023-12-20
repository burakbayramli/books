#
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
#from array import array
#from dataclasses import dataclass
#import math
#from typing import NamedTuple, Tuple, Union, List, Dict


# package imports
import numpy as np

#
def dpythag(a: float, b: float):
    """
    """
    absa = abs(a)
    absb = abs(b)

    try:
        1 / absb
        if absa > absb:
            return absa * np.sqrt(1.0 + DSQR(absb / absa))
        return absb * np.sqrt(1.0 + DSQR(absa / absb))
    except ZeroDivisionError:
        # if absb == 0.0:
        return 0.0
    #
    # return absb * math.sqrt(1.0 + DSQR(absa / absb))


#
#
def DSQR(a: float):
    """
    """
    dsqrarg = float(a)
    try:
        1 / dsqrarg
        return dsqrarg * dsqrarg
    except ZeroDivisionError:
        # if dsqrarg == 0.0:
        return 0.0
    # return dsqrarg * dsqrarg
#
#
