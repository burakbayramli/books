# -*- coding: utf-8 -*-

# © Copyright Charles Vanwynsberghe (2021)

# Pyworld3 is a computer program whose purpose is to run configurable
# simulations of the World3 model as described in the book "Dynamics
# of Growth in a Finite World".

# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software. You can use,
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info".

# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.

# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.

# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

import numpy as np
from scipy.integrate import odeint


def switch(var1, var2, boolean_switch):
    """
    Logical function returning var1 if boolean_switch is False, else var2.

    Parameters
    ----------
    var1 : any

    var2 : any

    boolean_switch : bool

    Returns
    -------
    var1 or var2

    """
    if np.isnan(var1) or np.isnan(var2):
        return np.nan
    else:
        if bool(boolean_switch) is False:
            return var1
        else:
            return var2


def clip(func2, func1, t, t_switch):
    """
    Logical function used as time switch to change parameter value.

    Parameters
    ----------
    func2 : any

    func1 : any

    t : float
        current time value.
    t_switch : float
        time threshold.

    Returns
    -------
    func2 if t>t_switch, else func1.

    """
    if np.isnan(func1) or np.isnan(func2):
        return np.nan
    else:
        if t <= t_switch:
            return func1
        else:
            return func2


def ramp(slope, t_offset, t):
    """
    Affine function with provided slope, clipped at 0 for t < t_offset.

    Parameters
    ----------
    slope : float
        ramp slope.
    t_offset : float
        time when ramps begins.
    t : float
        current time value.

    Returns
    -------
    slope * (t - t_offset) if t >= t_offset, else 0

    """
    if t < t_offset:
        return 0
    else:
        return slope * (t - t_offset)


# linear systems of order 1 or 3 for delay and smoothing
def func_delay1(out_, t_, in_, del_):
    """
    Computes the derivative of out_ at time t_, for the 1st order delay. Used
    in integration by odeint.

    """
    return (in_ - out_) / del_


class Smooth:
    """
    Delay information function of the 1st order for smoothing. Also named
    DLINF1 in Dynamo. Returns a class that is callable as a function (see
    Call parameters) at a given step k.

    Computes the smoothed vector out_arr from the input in_arr, at the step k.

    Parameters
    ----------
    in_arr : numpy ndarray
        input vector of the delay function.
    dt : float
        time step.
    t : numpy ndarray
        time vector.
    method : str, optional
        "euler" or "odeint". The default is "euler".

    Call parameters
    ---------------
    k : int
        current loop index.
    delay : float
        delay parameter. Higher delay increases smoothing.

    Call Returns
    ------------
    out_arr[k]

    """

    def __init__(self, in_arr, dt, t, method="euler"):
        self.dt = dt
        self.out_arr = np.zeros((t.size,))
        self.in_arr = in_arr  # use in_arr by reference
        self.method = method

    def __call__(self, k, delay):
        if k == 0:
            self.out_arr[k] = self.in_arr[k]
        else:
            if self.method == "odeint":
                res = odeint(func_delay1, self.out_arr[k-1],
                             [0, self.dt], args=(self.in_arr[k-1], delay))
                self.out_arr[k] = res[1, :]
            elif self.method == "euler":
                dout = self.in_arr[k-1] - self.out_arr[k-1]
                dout *= self.dt/delay
                self.out_arr[k] = self.out_arr[k-1] + dout

        return self.out_arr[k]


DlInf1 = Smooth


def func_delay3(out_, t_, in_, del_):
    """
    Computes the derivative of out_ at time t_, for the 3rd order delay. Used
    in integration by odeint.

    """
    dout_ = np.zeros((3,))
    dout_[0] = in_ - out_[0]
    dout_[1] = out_[0] - out_[1]
    dout_[2] = out_[1] - out_[2]

    return dout_ * 3 / del_


class Delay3:
    """
    Delay function of the 3rd order. Returns a class that is callable as a
    function (see Call parameters) at a given step k.

    Computes the delayed vector out_arr from the input in_arr, at the step k.

    Parameters
    ----------
    in_arr : numpy ndarray
        input vector of the delay function.
    dt : float
        time step.
    t : numpy ndarray
        time vector.
    method : str, optional
        "euler" or "odeint". The default is "euler".

    Call parameters
    ---------------
    k : int
        current loop index.
    delay : float
        delay parameter. Higher delay increases smoothing.

    Call Returns
    ------------
    out_arr[k]

    """

    def __init__(self, in_arr, dt, t, method="euler"):
        self.dt = dt
        self.out_arr = np.zeros((t.size, 3))
        self.in_arr = in_arr  # use in_arr as reference
        self.method = method
        if self.method == "euler":
            self.A_norm = np.array([[-1., 0., 0.],
                                    [1., -1., 0.],
                                    [0., 1., -1.]])
            self.B_norm = np.array([1, 0, 0])

    def _init_out_arr(self, delay):
        self.out_arr[0, :] = self.in_arr[0] * 3 / delay

    def __call__(self, k, delay):
        if k == 0:
            self._init_out_arr(delay)
        else:
            if self.method == "odeint":
                res = odeint(func_delay3, self.out_arr[k-1, :],
                             [0, self.dt], args=(self.in_arr[k-1], delay))
                self.out_arr[k, :] = res[1, :]
            elif self.method == "euler":
                dout = (self.A_norm  @ self.out_arr[k-1, :] +
                        self.B_norm * self.in_arr[k-1])
                dout *= self.dt*3/delay
                self.out_arr[k, :] = self.out_arr[k-1, :] + dout

        return self.out_arr[k, 2]


class Dlinf3(Delay3):
    """
    Delay information function of the 3rd order for smoothing. Returns a class
    that is callable as a function (see Call parameters) at a given step k.

    Computes the smoothed vector out_arr from the input in_arr, at the step k.

    Parameters
    ----------
    in_arr : numpy ndarray
        input vector of the delay function.
    dt : float
        time step.
    t : numpy ndarray
        time vector.
    method : str, optional
        "euler" or "odeint". The default is "euler".

    Call parameters
    ---------------
    k : int
        current loop index.
    delay : float
        delay parameter. Higher delay increases smoothing.

    Call Returns
    ------------
    out_arr[k]

    """

    def _init_out_arr(self, delay):
        self.out_arr[0, :] = self.in_arr[0]
