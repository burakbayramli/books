#
# Copyright (c) 2009 steelpy
#
from __future__ import annotations
# Python stdlib imports
from array import array
from copy import copy
from math import fsum
#import pickle
from dataclasses import dataclass
from typing import NamedTuple
#from itertools import chain
import time
#
# package imports
from steelpy.utils.math.operations import to_matrix
from steelpy.utils.dataframe.main import DBframework
#
import numpy as np
#from scipy.linalg import cholesky_banded, cho_solve_banded
#
#
# --------------------
# solver pure python
# --------------------
# 
#
def BAK(a: list[float], b: list[float]) -> list:
    """
    back substitution
    """
    #neq, iband = np.shape(a)
    neq = len(a)
    iband = len(a[0])
    wk = copy(b)
    # forward substitution
    for i in range(neq):
        j = max(i - iband + 1, 0)
        wk[i] -= fsum([a[k][i - k] * wk[k]
                       for k in range(j, i)])
        #
        #wk[i] -= np.sum(a[j: i, i - j: 0] * wk[j: i])
    # middle terms
    wk = array('d', [wk[i] / a[i][0] for i in range(neq)])
    #wk = wk[:neq] / a[:neq, 0]
    #wkk = copy(wk)
    # backward substitution
    for i in range(neq - 1, -1, -1):
        j = min(i + iband, neq)
        wk[i] -= fsum([a[i][k - i] * wk[k]
                       for k in range(i+1, j)])
        #
        #wk[i] -= np.sum(a[i, 1: j - i] * wk[i+1: j])
    #
    return wk
#
#
def solver_Mbanded(stf, nloads):
    """ """
    nloads = nloads.values        
    #
    # get displacement in global system
    #x = cho_solve_banded(stf, nloads)
    ndisp = BAK(stf, nloads)
    #
    return ndisp    
#
#
#
# ---------------------------------------------
# solver using numpy
# ---------------------------------------------
#
def solver_np(stf, nloads):
    """ """
    #nloads = df2.stack() #.values
    ndisp = np.linalg.solve(stf, nloads)
    #if not np.allclose(np.dot(stf, ndisp), nload): #, rtol=1e-05, atol=1e-08
    #    raise RuntimeError('Solution fail')
    #
    return ndisp
#
#
#
# ---------------------------------------------
#
# ---------------------------------------------
#
@dataclass
class StaticSolver:
    """ Linear static solver class"""
    __slots__ = ['_plane',  '_method']
    
    def __init__(self, plane: NamedTuple,
                 method:str|None = None) -> None:
        """
        plane : Plane system (3D/2D)
        """
        self._plane = plane
        self._method = method
    #
    #
    #
    #def Kglobal(self, jbc, Ka, Kg: list|bool = None) -> None:
    #    """Input global stiffness matrix"""
    #    with open("stfmx.f2u", "wb") as f:
    #        pickle.dump(jbc, f)
    #        pickle.dump(Ka, f)
    #
    def solve(self, Kg, Fn, jbc):
        """
        Linear Static Analysis (1st Order)
        
        Input: 
        Kg  : Global stiffness matrix
        Fn  : Node load dataframe
        jbc : Node boundary condition dataframe
        
        Return: 
        Udf : Node displacement global system dataframe
        """
        #
        order = "1st"
        print(f"** Solving U = K^-1 F [{order} order] ")
        start_time = time.time()
        #
        #file = open("stfmx.f2u", "rb")
        #df_jbc = pickle.load( file )
        #stf = pickle.load( file )
        #file.close()
        #
        dfnload = self._load_update(Fn)
        #
        solver = solver_np
        #if self._method == 'banded':
        #    solver = solver_Mbanded
        #
        Udf = self.DMS(Kg, dfnload, jbc, solver)
        #
        uptime = time.time() - start_time
        print(f"** Finish Time: {uptime:1.4e} sec")
        return Udf
    #
    def DMS(self, stf, dfnload, df_jbc, solver):
        """
        Direct Stiffness Method
        """
        dfbool, dfzeros = self._mask(df_jbc)
        jbcc = df_jbc.stack()
        blgrp = dfnload.groupby(['load_name', 'load_number', 
                                 'load_type','load_system'])
        #       
        dftemp = []
        for key, litem in blgrp:
            # map loading 
            df1 = litem.set_index(litem['node_name'])
            df2 = dfzeros.copy()
            df2.loc[df2.index.isin(df1['node_name'])] = df1[self._plane.hforce] #.astype('float64')
            # get load vector flatted
            df2 = df2.stack()
            nloads = df2[dfbool]
            # Solve displcements
            ndisp = iter(solver(stf, nloads))
            # reshape vector in matrix form [row, col]
            ndisp = [next(ndisp) if ieqnum != 0 else ieqnum
                     for ieqnum in jbcc]
            #
            ndisp = to_matrix(ndisp, self._plane.ndof)
            filldata = [[*key,  litem['load_title'].iloc[0],
                        nname, *ndisp[x]]
                        for x, nname in enumerate(df_jbc.index)]
            #
            dftemp.extend(filldata)
        #
        return self.df(dftemp)
        #return df_ndisp
    #
    #@property
    def df(self, dftemp):
        """displacement dataframe"""
        db = DBframework()
        header = ['load_name', 'load_number', 'load_type',
                  'load_system', 'load_title',
                  'node_name', *self._plane.hdisp]
        return db.DataFrame(data=dftemp, columns=header, index=None)
    #
    def _load_update(self, df_nload):
        """ """
        dfnload = (df_nload.groupby(['load_name', 'load_number', 'load_type',
                                     'load_title','load_system', 'node_name'])
                   [self._plane.hforce].sum())
        #
        dfnload.reset_index(inplace=True)
        return dfnload
    #
    def _mask(self, df_jbc):
        """ """
        # remove rows with zeros
        dfjbc = df_jbc.rename(columns=self._plane.colrename)
        dfjbc = dfjbc[df_jbc.any(axis=1)]
        dfjbc = dfjbc.replace(float(0.0), np.nan)
        dfjbc = dfjbc.notnull()
        #
        dfbool = dfjbc.stack()
        # Copy dataframe 
        #dfzeros = dfjbc.copy()
        #dfzeros.iloc[:] = 0
        #
        dfjbc.iloc[:] = float(0.0)
        #
        return dfbool, dfjbc
    #
    # -----------------------------------------------------------
    # Post-process
    # -----------------------------------------------------------
    #    
#
#