#
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
#from array import array
from dataclasses import dataclass
#import math
#from typing import NamedTuple, Tuple, Union, List, Dict

# package imports
from steelpy.metocean.regular.fourier.Subroutines import Newton, initial
from steelpy.metocean.regular.process.inout import title_block, output
from steelpy.metocean.regular.process.waveops import WaveRegModule, WaveItem, get_wave_data

import numpy as np

#
#
@dataclass
class WaveFourier(WaveItem):
    
    def __init__(self, H:float, d:float, title:str, 
                 T:float|None=None,
                 Lw:float|None = None, 
                 infinite_depth:bool=False,
                 current:float = 0.0, c_type:int = 1,
                 order:int=5, nstep:int=2,
                 number:int=40, accuracy:float=1e-6) -> None:
        """
        """
        super().__init__(H=H, Tw=T, d=d, title=title,
                         order=order, nstep=nstep, niter=number,
                         accuracy=accuracy,
                         current=current, c_type=c_type,
                         infinite_depth=infinite_depth)
    
    def __call__(self):
        """ Solver """
        try:
            self._z
        except AttributeError:        
            self.method = "Fourier"
            data = self.get_parameters()
            z, Y, B, Tanh = FourierMain(*data)
            self._z = z
            self._Y = Y
            self._B = B
            self._Tanh = Tanh
            self._wave_length = 2 * np.pi / z[ 1 ]
        #self._Highest = Highest         
        #return z, Y, B, Tanh

#
#
class FourierModule(WaveRegModule):
    __slots__ = [ 'order', 'nsteps', 'max_iter', 'accuracy',
                  '_labels', '_cases', 'c_type', '_current']

    def __init__(self, n:int=20, nstep:int=2,
                 number:int=40, accuracy:float=1e-6):
        """
        n      : Stokes order (5)
        nstep  : Number of height steps to reach H/d (2)
        number : Maximum number of iterations for each step (20)
        accuracy   : Criterion for convergence
        """
        super().__init__(n=n, nstep=nstep,
                         number=number, accuracy=accuracy)
    #
    def __setitem__(self, case_name: int, data: list[float]) -> None:
        """
        case_name : Wave name
         H : Wave height [unit length]
         T : Wave Period [second]
         d : Water depth LTH + Tide and Surge [unit length]
         Lw : Wave Length [unit length]
         Phase : Wave phase [degree]
         Order : ??
        """
        try:
            self._labels.index(case_name)
            raise Exception('    *** warning wave {:} already exist'
                            .format(case_name))
        except ValueError:
            self._labels.append(case_name)
            current = self._current._current
            c_type = self._current.c_type
            #data = get_wave_data(case_data)
            self._cases.append(WaveFourier(H=data[0], T=data[1], d=data[2], 
                                           title=case_name,
                                           order=self.order, nstep=self.nsteps, 
                                           number=self.max_iter,
                                           current=current, c_type=c_type,
                                           accuracy=self.accuracy,
                                           infinite_depth = self.infinite_depth))

#
# 
# Main program
#
#
#def FourierMain(h:float, t:Union[float,None], d:float, 
#                Lw:Union[float,None], is_finite:bool,
#                current:float, c_type:int=1,
#                n:int=20, nstep:int=2, number:int=40, accuracy:float=1e-5):
def FourierMain(MaxH:float, case:str,
                T:float|None,
                L:float|None, 
                c_type:int, current:float, 
                norder:int,   nstep:int,
                niter:int, accuracy:float,
                Height:float, is_finite:bool):
    """
    Input
    -------
    MaxH : H/d
    case : period/wavelength
    T : Dimensionless period / None
    L : Dimensionless wavelength / None
    c_type  : Current criterion, 1 - Eularian mean, 2 - Stokes depth integrated mean
    current : Current magnitude
    order :  Number of Fourier components or order of Stokes or cnoidal theory
    nsteps : Number of height steps to reach H/d
    niter  : Maximum number of iterations for each step (20)
    crit   : Criterion for convergence (1e-6)
    Height : ??
    finite_depth : True/False
    
    Output
    --------
    z : Solution vector
    Y : Discrete Fourier transform of the surface elevations.
    B : Fourier coefficients
    Tanh : 
    """
    #current=0.31
    crit = accuracy
    #pi = np.pi
    #g = 9.80665  # m/s^2
    #
    print("# Solution by {:}-term Fourier series".format(norder))
    method = ("Fourier method with {:} terms in series".format(norder))
    num = 2 * norder + 10
    dhe = Height / nstep
    dho = MaxH / nstep
    #
    #CC = np.identity(n=num+1)
    #CC = np.zeros((num+1, num+1))
    #for j in range(1, num+1):
    #    CC[j][j] = 1.0
    #
    #
    Y = np.zeros(num+1)
    sol = np.zeros((num+1, 2+1))
    B = np.zeros(norder+1)
    # Commence stepping through steps in wave height
    for ns in range(1, nstep+1):
        height = ns * dhe
        hoverd = ns * dho
        print("--- height step {:} of {:}".format(ns, nstep))
        # Calculate initial linear solution
        if ns <= 1:
            #sol, z, cosa, sina = initial(height, hoverd, current,
            #                             c_type, num, n)
            sol, z, cosa, sina = initial(height, hoverd, current, 
                                         c_type, num, norder,
                                         is_finite, case)            
        else:
            # Or, extrapolate for next wave height, if necessary
            z = [2.0 * sol[i][2] - sol[i][1] for i in range(num+1)]
        #
        # Commence iterative solution
        for itr in range(1, niter+1):
            print("# Iteration {:}:".format(itr))
            # Calculate right sides of equations and differentiate numerically
            # to obtain Jacobian matrix, : solve matrix equation
            z, error, Tanh = Newton(height, hoverd, z, cosa, sina,
                                    num, norder, current, c_type,
                                    is_finite, case)
            # Convergence criterion satisfied?
            print("# Mean of corrections to free surface: {: 1.4e}".format(error))
            
            criter = crit
            if ns == nstep:
                criter = 1.e-10
            #
            if error < criter * abs(z[1]) and itr > 1:
                break # Exit for
            #
            if itr == niter:
                print("# Note that the program still had not converged to the degree specified")
            # Operations for extrapolations if more than one height step used
            if ns == 1:
                sol[1: num+1, 2] = z[1: num+1]
                #for i in range(1, num+1):
                #    sol[i][2] = z[i]
            else:
                sol[1: num+1, 1] = sol[1: num+1, 2]
                sol[1: num+1, 2] = z[1: num+1]
                #for i in range(1, num+1):
                #    sol[i][1] = sol[i][2]
                #    sol[i][2] = z[i]
        #
        # Fourier coefficients (for surface elevation by slow Fourier transform)
        Y[0] = 0.0
        for j in range(1, norder+1):
            B[j] = z[j + norder + 10]
            sumf = 0.5 * (z[10] + z[norder + 10] * np.power(-1.0, float(j)))
            sumf += np.sum([z[10 + m] * cosa[(m * j) % (norder + norder)]
                            for m in range(1, norder)])
            Y[j] = 2.0 * sumf / norder
    #
    #  Highest wave - eqn (32) of Fenton (1990)
    #
    #L = 2 * pi / z[1]
    #Highest = ((0.0077829 * L * L * L + 0.0095721 * L * L + 0.141063 * L)
    #           / (0.0093407 * L * L * L + 0.0317567 * L * L + 0.078834 * L + 1))
    #    
    #
    # End stepping through wave heights
    method = "Fourier"
    title_block(method, c_type, current, z)
    output(norder, z, Y, B, Tanh, is_finite) 
    #print_output (n, z, Y, B, c_type, current, method)
    return z, Y, B, Tanh
# 
# 
#
