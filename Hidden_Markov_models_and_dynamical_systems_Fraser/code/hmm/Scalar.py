""" Scalar.py: Implements scalar observation models for hmms in base.py.

"""
Copyright = '''
Copyright 2003, 2007, 2008, 2012 Andrew M. Fraser, and 2013 Andrew
M. Fraser and Los Alamos National Laboroatory

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
import numpy as np

def initialize(x, shape, dtype=np.float64):
    '''Service fuction.  If x has right shape return it, otherwise
    allocate array of correct shape and type.

    '''
    if x == None or x.shape != shape:
        return np.empty(shape, dtype)
    return x
## ----------------------------------------------------------------------
class Prob(np.ndarray):
    '''Subclass of ndarray for probability matrices.  P[a,b] is the
    probability of b given a.  The class has additional methods and is
    designed to enable alternative implementations that run faster or
    in less memory but may be implemented by uglier code.

    '''
    # See http://docs.scipy.org/doc/numpy/user/basics.subclassing.html
    def normalize(self):
        '''
        Make each row a proability that sums to one

        Parameters
        ----------
        None

        Returns
        -------
        None
        '''
        s = self.sum(axis=1)
        for i in range(self.shape[0]):
            self[i, :] /= s[i]
    def assign_col(self, i, col):
        '''
        Replace column of self with data specified by the parameters

        Parameters
        ----------
        i : int
            Column index
        col : array_like
            Column data

        Returns
        -------
        None
        '''
        self[:, i] = col
    def likelihoods(self, v):
        '''Likelihoods for vector of data

        Given T = len(v) and self.shape = (M,N), return L with L.shape
        = (T,M) and L[t,a] = Prob(v[t]|a)

        Parameters
        ----------
        v : array_like
            Time series of observations

        Returns
        -------
        L : array

        '''
        return self[:, v].T
    def cost(self, nu, py):
        ''' Efficient calculation of np.outer(nu, py)*self (where * is
        element-wise)
        '''
        return (self.T*nu).T*py
    def inplace_elementwise_multiply(self, a):
        '''
        Replace self with product of self and argument

        Parameters
        ----------
        a : array

        Returns
        -------
        None
        '''
        self *= a
    def step_forward(self, a):
        '''
        Replace values of argument a with matrix product a*self

        Parameters
        ----------
        a : array

        Returns
        -------
        None
        '''
        a[:] = np.dot(a, self)
    def step_back(self, a):
        '''
        Replace values of argument a with matrix product self*a

        Parameters
        ----------
        a : array

        Returns
        -------
        None
        '''
        a[:] = np.dot(self, a)
    def values(self):
        '''
        Produce values of self

        This is a hack to free subclasses from the requirement of self
        being an nd_array

        Parameters
        ----------
        None

        Returns
        -------
        v : array
        '''
        return self
def make_prob(x):
    '''Make a Prob instance.

    Used as an argument of base.HMM.__init__ so that one may use other
    functions that create instances of other classes instead of Prob,
    eg, one that uses sparse matrices.

    Parameters
    ----------
    x : array_like
        Conditional probabilites x[i,j] the probability of j given i

    Returns
    -------
    p : Prob instance

    '''
    x = np.array(x)
    return Prob(x.shape, buffer=x.data)

def make_random(shape):
        '''
        Make random Prob of given shape

        Parameters
        ----------
        shape

        Returns
        -------
        p
        '''
        from numpy.random import random
        P = make_prob(random(shape))
        P.normalize()
        return P
class Discrete_Observations:
    '''The simplest observation model: A finite set of integers.

    Parameters
    ----------
    P_YS : array_like
        Conditional probabilites P_YS[s,y]

    '''
    def __init__(self,  # Discrete_Observations instance
                 P_YS):
        self.P_YS = make_prob(P_YS)
        self.cum_y = np.cumsum(self.P_YS, axis=1)
        self.P_Y = None
        self.dtype = [np.int32]
        return
    def __str__(self):
        return 'P_YS =\n%s'%(self.P_YS,)
    def random_out(self, # Discrete_Observations instance
                   s):
        ''' For simulation, draw a random observation given state s

        Parameters
        ----------
        s : int
            Index of state

        Returns
        -------
        y : int
            Random observation drawn from distribution conditioned on state s

        '''
        import random
        return  (np.searchsorted(self.cum_y[s],random.random()),)
    def calc(self, # Discrete_Observations instance
             y_):
        """
        Calculate and return likelihoods: self.P_Y[t,i] = P(y(t)|s(t)=i)

        Parameters
        ----------
        y_ : list
            Has one element which is a sequence of integer observations

        Returns
        -------
        P_Y : array, floats

        """
        y = y_[0]
        n_y = len(y)
        n_states = len(self.P_YS)
        self.P_Y = initialize(self.P_Y, (n_y, n_states))
        self.P_Y[:, :] = self.P_YS.likelihoods(y)
        return self.P_Y
    def join(self, # Discrete_Observations instance
             ys):
        """Concatenate and return multiple y sequences.

        Also return information on sequence boundaries within
        concatenated list.  Code also works on more complex subclasses
        for which each y has more than one component.

        Parameters
        ----------
        ys : list
            A list of observation sequences.  Default int, but must match
            method self.P_Y() if subclassed

        Returns
        -------
        n_seg : int
            Number of component segments
        t_seg : list
            List of ints specifying endpoints of segments within y_all
        y_all : list
            Concatenated list of observations

        """
        t_seg = [0] # List of segment boundaries in concatenated ys
        y_all = []
        n_components = len(ys[0])
        for i in range(n_components):
            y_all.append([])
        for seg in ys:
            for i in range(n_components):
                y_all[i] += list(seg[i])
            t_seg.append(len(y_all[0]))
        for i in range(n_components):
            y_all[i] = np.array(y_all[i], self.dtype[i])
        return len(t_seg)-1, t_seg, y_all
    def reestimate(self,      # Discrete_Observations instance
                   w,         # Weights
                   y_,        # Observations
                   warn=True
                   ):
        """
        Estimate new model parameters

        Parameters
        ----------
        w : array
            w[t,s] = Prob(state[t]=s) given data and old model
        y : list
            y[0] is a sequence of integer observations
        warn : bool
            If True and y[0].dtype != np.int32, print warning

        Returns
        -------
        None
        """
        y = y_[0]
        n_y = len(y)
        if not (type(y) == np.ndarray and y.dtype == np.int32):
            y = np.array(y, np.int32)
            if warn:
                print('Warning: reformatted y in reestimate')
        assert(y.dtype == np.int32 and y.shape == (n_y,)),'''
                y.dtype=%s, y.shape=%s'''%(y.dtype, y.shape)
        for yi in range(self.P_YS.shape[1]):
            self.P_YS.assign_col(
                yi, w.take(np.where(y==yi)[0], axis=0).sum(axis=0))
        self.P_YS.normalize()
        self.cum_y = np.cumsum(self.P_YS, axis=1)
        return

class Gauss(Discrete_Observations):
    '''Scalar Gaussian observation model

    The probability of obsrevation y given state s is:
    P(y|s) = 1/\sqrt(2\pi sigma2[s]) exp(-(y-mu[s])^2/(2sigma2[s])

    Parameters
    ----------
    mu : array_like
        array of means; a value for each state
    sigma2 : array_like
        array of variances; a value for each state

    '''
    def __init__(self,  # Gauss observation model instance
                 pars
    ):
        mu, sigma2 = pars
        self.mu = np.array(mu, np.float64)
        self.sigma2 = np.array(sigma2, np.float64)
        self.sigma = np.sqrt(self.sigma2)
        self.norm = 1/np.sqrt(2*np.pi*self.sigma2)
        self.dtype = [np.float64]
        return
    def __str__(self):
        return '    mu=%s\nsigma2=%s '%(self.mu, self.sigma2)
    def random_out(self, # Gauss observation model instance 
                   s):
        ''' For simulation, draw a random observation given state s

        Parameters
        ----------
        s : int
            Index of state

        Returns
        -------
        y : float
            Random observation drawn from distribution conditioned on state s

        '''
        import random
        return  (random.gauss(self.mu[s], self.sigma[s]),)
    def calc(self, # Gauss observation model instance 
             y_
         ):
        """
        Calculate and return likelihoods: self.P_Y[t,i] = P(y(t)|s(t)=i)

        Parameters
        ----------
        y_ : list
            Has one element which is a sequence of float observations

        Returns
        -------
        P_Y : array, floats

        """
        y = np.array(y_[0])
        d = self.mu - y.reshape((-1, 1))
        self.P_Y = np.exp(-d*d/(2*self.sigma2))*self.norm
        return self.P_Y
    def reestimate(self,      # Gauss observation model instance 
                   w,         # Weights
                   y_,        # Observations
                   warn=True
               ):
        """
        Estimate new model parameters

        Parameters
        ----------
        w : array
            w[t,s] = Prob(state[t]=s) given data and old model
        y : list
            y[0] is a sequence of scalar float observations

        Returns
        -------
        None
        """
        y = y_[0]
        wsum = w.sum(axis=0)
        self.mu = (w.T * y).sum(axis=1)/wsum
        d = (self.mu - y.reshape((-1, 1)))*np.sqrt(w)
        self.sigma2 = (d*d).sum(axis=0)/wsum
        self.sigma = np.sqrt(self.sigma2)
        self.norm = 1/np.sqrt(2*np.pi*self.sigma2)
        return
class Class_y(Discrete_Observations):
    '''Observation model with classification
    
    Parameters
    ----------
    pars : (y_class, theta, c2s)
    y_class : class
        y_class(theta) should yield a model instance for observations without
        class
    theta : object
        A python object that contains parameter[s] for the observation model
    c2s : dict
        classification labels are keys and each value is a list of states
        contained in the classification.

    '''
    def __init__(self, # Class_y instance
                 pars):
        y_class, y_pars, c2s = pars
        self.y_mod = y_class(y_pars)
        self.dtype = [np.int32, np.int32]
        self.P_Y = None
        self.g = None
        states = {}  # Count states
        for c in c2s:
            for s in c2s[c]:
                states[s] = True
        n_states = len(states)
        n_class = len(c2s)
        self.c2s = np.zeros((n_class, n_states), np.bool)
        self.s2c = np.empty(n_states, dtype=np.int32)
        for c in c2s:
            for s in c2s[c]:
                self.s2c[s] = c
                self.c2s[c,s] = True
        return
    def __str__(self):
        return('%s with c2s =\n%s\n%s'%(
                self.__class__, self.c2s.astype(np.int8), self.y_mod))
    def set_dtype(self, dtype):
        '''Necessary if data for self.y_mod.calc and self.y_mod.join is not
        np.int32.

        '''
        self.dtype = dtype
        return
    def random_out(self, # Class_y instance
                   s):
        '''Simulate.  Draw a random observation given state s.

        Parameters
        ----------
        s : int
            Index of state

        Returns
        -------
        y, c : int, int
            A tuple consisting of an observation and the class of the state
        '''
        return self.s2c[s], self.y_mod.random_out(s)[0]
    def calc(self, # Class_y instance
             cy):
        """
        Calculate and return likelihoods: P_Y[t,i] = P(y(t)|s(t)=i)*g(s,c[t])

        g(s,c) is a gate function that is one if state s is in class c
        and is zero otherwise.

        Parameters
        ----------
        yc : array_like
            A sequence of y,c pairs.  y[t] = yc[t][0] and c[t] = yc[t][1]

        Returns
        -------
        P_Y : array, floats

        """
        c = cy[0]
        y = cy[1:]
        n_y = len(c)
        n_class, n_states = self.c2s.shape
        self.g = initialize(self.g,(n_y, n_states),np.bool)
        self.g[:,:] = self.c2s[c,:]
        self.P_Y = self.y_mod.calc(y) * self.g
        return self.P_Y
    def reestimate(self,  # Class_y instance
                   w, cy):
        """
        Estimate new model parameters

        Parameters
        ----------
        w : array
            w[t,s] = Prob(state[t]=s) given data and old model
        cy : list
            c,y = cy; Where c/y is an array of classifications/observations

        Returns
        -------
        None
        """
        self.y_mod.reestimate(w, cy[1:])
        return
def _test():
    import base
    P_S0 = [0.67, 0.33]
    P_SS = [
        [0.93, 0.07],
        [0.13, 0.87]]
    mu = [-1.0, 1.0]
    var = np.ones(2)
    model = base.HMM(P_S0, P_S0, (mu, var), P_SS, Gauss)
    S, Y = model.simulate(100)
    Y = np.array(Y, np.float64)
    E = model.decode(Y)
    P_SS = np.ones((2,2))/2
    mu = [-2, 2]
    var = np.ones(2)*4
    model = base.HMM(P_S0, P_S0, (mu, var), P_SS, Gauss)
    model.train(Y, n_iter=15)
    print('trained model:\n%s'%model)

if __name__ == "__main__":
    _test()
#--------------------------------
# Local Variables:
# mode: python
# End:
