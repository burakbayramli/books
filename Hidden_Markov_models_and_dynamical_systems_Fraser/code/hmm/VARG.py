"""
VARG.py

This file implements "Vector AutoRegressive Gaussian" observation
models.  y is a list of lists

y[0][t] is a numpy.array containing the observation vector
y[1][t] is a numpy.array containing the context vector

P(y|s) = Normal(mu(s,y[1]),cov[s])_y[0] where mu(s,v) = A[s]*v

"""
Copyright = '''
Copyright 2005, 2007 Andrew M. Fraser, and 2013 Andrew M. Fraser and
Los Alamos National Laboroatory

This file is part of hmmds3.

Hmmds3 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Hmmds3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

See the file gpl.txt in the root directory of the hmmds3 distribution
or see <http://www.gnu.org/licenses/>.
'''
# I raise an exception if eigenvalues or determinants are small or
# big.  I also disregard state occupancy probabilities that are small

from hmm.Scalar import Discrete_Observations
from hmm.Scalar import initialize
import numpy as np
import numpy.linalg as LA

small = 1e-20
big = 1e+20

#------------------------------------------------------------
class VARG(Discrete_Observations):
    '''
    To be consistent with Scalar.Discrete_Observations a class must have
    the following methods:

    __init__(parameters)

    calc(y) where y is a list of sequences.  Returns P(s,y) likelihoods
    given states

    reestimate(w,y)

    join(ys) where ys is a list of sequences.  Returns concatenation of
        sequences and boundary points within them of the components.
    '''

    def random_out(self, s):
        raise RuntimeError(
            'random_out() not implemented for %s'%self.__class__)
    def __init__(self, params):
        if len(params) == 2:
            self.As, self.Icovs = params
            self.a = 4
            self.b = 0.1
        else:
            self.As, self.Icovs, self.a, self.b = params

        self.n_states = len(self.As)
        assert self.n_states == len(self.Icovs)
        self.dtype = [np.float64]
        self.P_Y = None
        self.fixed_var = False
        return # End of __init__()
    def freeze_var(self):
        self.fixed_var = True
    def thaw_var(self, a=None, b=None):
        if a is not None:
            self.a = a
        if b is not None:
            self.b = b            
        self.fixed_var = False
    def normalize(self):
        cr,cc = self.Icovs[0].shape
        assert cr == cc, 'Icovs[0]=%s'%(self.Icovs[0],)
        self.evals = np.empty((self.n_states,cr))
        self.evecs = np.empty((self.n_states,cr,cr))
        self.norms = np.empty((self.n_states,))
        for i in range(self.n_states):
            self.evals[i,:],self.evecs[i,:,:] = LA.eigh(self.Icovs[i])
            # Save sqrt eigenvalues of Cov not inverse Cov because they are
            # only used in simulate()
            self.evals[i] = 1/np.sqrt(self.evals[i])
            if self.evals[i].min() < small:
                raise RuntimeError('In normalize: Eigenvalue %f too small'%(
                    self.evals[i].min()))
            if self.evals[i].max() > big:
                raise RuntimeError('In normalize: Eigenvalue %f too big'%(
                    self.evals[i].max()))
            d = LA.det(self.Icovs[i])
            if d < small or d > big:
                raise RuntimeError('extreme determinant %f'%d)
            self.norms[i] = 1/np.sqrt((2*np.pi)**cr/d)
        return
    def calc(self, y):
        """
        Calculate and return likelihoods: self.P_Y[t,i] = P(y(t)|s(t)=i)

        Parameters
        ----------
        y_ : (y,)
            A sequence of vector observations and contexts

        Returns
        -------
        P_Y : array, floats
            P_Y.shape = (n_y, n_states)

        """
        n_y = len(y[0])
        self.P_Y = initialize(self.P_Y, (n_y, self.n_states))
        for t in range(n_y):
            for i in range(self.n_states):
                d = y[0][t] - np.dot(self.As[i], y[1][t])
                dQd = np.dot(d, np.dot(self.Icovs[i], d))
                if dQd > 300: # Underflow
                    self.P_Y[t,i] = 0
                else:
                    self.P_Y[t,i] = self.norms[i]*np.exp(-dQd/2)
        return self.P_Y

    def reestimate(self, # Resp instance
                   w,    # w[t,i] = prob s(t) = i
                   y):
        n_y, dim_Y = y[0].shape
        t, dim_X = y[1].shape
        assert t == n_y
        
        root_w = np.sqrt(w)
        sum_w = w.sum(axis=0)
        for i in range(self.n_states):
            wY = (root_w.T[i] * y[0].T).T
            wX = (root_w.T[i] * y[1].T).T
            AT,resids,rank,svals = LA.lstsq(wX, wY, rcond=1e-10)
            self.As[i] = AT.T
            if self.fixed_var:
                continue
            zT = wY - np.dot(wX,AT)
            ZZT = np.dot(zT.T,zT)
            # MAP with an inverse Wishart prior
            Cov = (self.b * np.eye(dim_Y) + ZZT)/(self.a + sum_w[i])
            self.Icovs[i] = LA.inv(Cov)
        self.normalize()
        return # End of reestimate()
    def __str__(self # VARG
                ):
        save = np.get_printoptions
        np.set_printoptions(precision=3)
        rv = 'Model %s instance\n'%self.__class__
        for i in range(self.n_states):
            rv += 'For state %d:\n'%i
            rv += ' Icov = \n%s\n'%self.Icovs[i]
            rv += ' A = %s'%self.As[i]
            rv += ' norm = %f\n'%self.norms[i]
        np.set_printoptions(save)
        return rv

# End of class VARG

# Local Variables:
# mode: python
# End:
