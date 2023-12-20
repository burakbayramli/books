# 
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
# Python stdlib imports
from array import array
from math import fsum
from collections.abc import Sequence
from numbers import Integral

# package imports
#from steelpy.process.math.vector import Vector
import numpy as np

# --------------------
# Matrix Operations       
# --------------------
#
#
def zeros(m, n=None, code:str = 'd'):
    """
    Create zero matrix
    """
    if n: 
        new_matrix = [array(code, [0 for row in range(n)]) 
                      for col in range(m)]
        #new_matrix = [[0 for row in range(n)] for col in range(m)]
        #new_matrix = np.zeros((m, n), dtype=np.float64, order='F')
    else: 
        new_matrix = array(code, [0 for row in range(m)])
    
    return new_matrix
#
def zeros_vector(m, n, code:str = 'd'):
    """
    Create zero matrix
    """
    new_matrix = [Vector([0 for row in range(n)]) for col in range(m)]
    return new_matrix
#
def ones(m, n=None, code:str = 'd'):
    """
    Create zero matrix
    """
    if n: 
        new_matrix = [array(code, [1 for row in range(n)]) 
                      for col in range(m)]        
        #new_matrix = [[1 for row in range(n)] for col in range(m)]
        #new_matrix = np.zeros((m, n), dtype=np.float64, order='F')
    else: 
        new_matrix = array(code, [1 for row in range(m)])
    
    return new_matrix
#
def to_matrix(l, n):
    try:
        return l.reshape(-1, n)
    except AttributeError:
        return [l[i:i+n] for i in range(0, len(l), n)]
#
#
def mtxmul(a, b, code:str = 'd'):
    """
    Multiply [a]{b} = {c}
    """
    try:
        return a @ b
    except TypeError:
        n = min(len(a), len(b))
        c = array(code, [fsum([a[i][j] * b[j] for j in range(n)])
                         for i in range(n)])
        return c
#
def transposeM(matrix):
    """
    """
    # rt = list( map( list, zip( *r_matrix ) ) )
    return list(zip(*matrix))
#
#
def trnsload(gloads:list, r_matrix:list):
    """
    Makes load vector transformations.
    """
    return np.transpose(r_matrix).dot(gloads)
#
#
def Tmatrix(Rmat):
    """
    """
    tmx = zeros(12, 12)
    #tmx = Matrix(12, 12)
    for j1 in range(0, 12, 3):
        for k in range(3):
            for l in range( 3 ):
                tmx[j1 + k][j1 + l]  = Rmat[k][l]
    return tmx
#
#
def matrix_full(x):
    """ """
    neq = len(x)
    iband = len(x[0])
    #
    a = zeros(neq, neq)
    # reassign to full form  
    for i in range(neq):
        jmax = min(iband, neq - i)
        for j in range(jmax):
            a[i][i + j] = x[i][j]
            a[i + j][i] = x[i][j]
    return a
#
#
# --------------------
#      
# --------------------
#
#
def linspace(start:int, stop:int, num:int, endpoint:bool=True):
    """ equally space numbers"""
    step = (stop - start) * 1.0 / (num)
    if endpoint:
        step = (stop-start) * 1.0 / (num - 1)
    return [start+i*step for i in range(num)]
#
class linspace2(Sequence):
    """linspace(start, stop, num) -> linspace object

    Return a virtual sequence of num numbers from start to stop (inclusive).

    If you need a half-open range, use linspace(start, stop, num+1)[:-1].
    """

    def __init__(self, start, stop, num):
        if not isinstance(num, Integral) or num <= 1:
            raise ValueError('num must be an integer > 1')
        self.start, self.stop, self.num = start, stop, num
        self.step = (stop - start) / (num - 1)

    def __len__(self):
        return self.num

    def __getitem__(self, i):
        if isinstance(i, slice):
            return [self[x] for x in range(*i.indices(len(self)))]
        if i < 0:
            i = self.num + i
        if i >= self.num:
            raise IndexError('linspace object index out of range')
        if i == self.num - 1:
            return self.stop
        return self.start + i * self.step

    def __repr__(self):
        return '{}({}, {}, {})'.format(type(self).__name__,
                                       self.start, self.stop, self.num)

    def __eq__(self, other):
        if not isinstance(other, linspace):
            return False
        return ((self.start, self.stop, self.num) ==
                (other.start, other.stop, other.num))

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash((type(self), self.start, self.stop, self.num))
#
#
# --------------------
#      
# --------------------
#
#
def remove_column_row(a: list, row: float, col: int):
    """ """
    without_row = np.delete(a, row, axis=0)
    return np.delete(without_row, col, axis=1)
#
#
def remove_column_of_zeros_and_shift_row(a, row, col):
    """ """
    without_row = np.delete(a, row, axis=0)
    without_row_and_col = np.delete(without_row, col, axis=1)
    z = np.zeros((1, len(without_row_and_col[0])))
    without_col_shifted_row = np.append(z, without_row_and_col, axis=0)
    return without_col_shifted_row
#
#
def swap_col(arr, start_index, last_index):
    """ """
    arr[:, [start_index, last_index]] = arr[:, [last_index, start_index]]
#
#
def swap_row(arr, start_index, last_index):
    """ """
    arr[[start_index, last_index],:] = arr[[last_index, start_index],:]
#