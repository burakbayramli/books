'''ApOb.py

Observation model classes for apnea data and utilities for reading the data.

To be consistent with Scalar.Discrete_Observations a class must have
the following methods:

__init__(parameters)

calc(y) where y is a list of sequences.  Returns P(s,y) likelihoods
given states

reestimate(w,y)

join(ys) where ys is a list of sequences.  Returns concatenation of
    sequences and boundary points within them of the components.

There are apnea observation model classes: "Resp", "Heart_Rate", and
"Both".  Resp models the high frequency variability via a 3-d
_respiration_ model for components of an LDA of spectrograms of a
high-pass band.  Heart_Rate models the scalar time series of low
frequency heart rate variability via Gaussians with affine
autoregressive means.  And Both combines the models.

FixMe: Remember that the relative weighting in class Both could be a
parameter.

'''
Copyright = '''Copyright 2003, 2007, 2008, 2012 Andrew M. Fraser, and 2013
Andrew M. Fraser and Los Alamos National Laboroatory

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
small = 1e-25
SamPerMin = 10 # Samples per minute.

from hmm.Scalar import Discrete_Observations # join method gets used
from hmm.Scalar import Class_y
from hmm.Scalar import initialize
import numpy as np
import math
import numpy.linalg as LA
LAI = LA.inv

mark_dict = {'N':0,'A':1}
def fetch_ann(Annotations, name):
    """Does the work of fetch_annotations.  Only one sample per minute.

    """
    F = open(Annotations,'r')
    parts = F.readline().split()
    while len(parts) is 0 or parts[0] != name:
        parts = F.readline().split()
    hour = 0
    letters = []
    for line in F:
        parts = line.split()
        if len(parts) != 2:
            break
        assert (int(parts[0]) == hour),'parts=%s\nhour=%d'%(parts, hour)
        hour += 1
        letters += parts[1]
    notes = []
    for t in range(len(letters)):
        notes.append(mark_dict[letters[t]])
    return np.array(notes, np.bool)

def fetch_annotations(Annotations,name):
    ''' Get apnea/normal annotations

    Parameters
    ----------
    Annotations : path
        File that contains expert markings for all records
    name : string
        Name of record for which to fetch annotations, eg, "a01"

    Returns
    -------
    notes : array
        Boolean sequence with 10 samples per minute.  True if apnea.

    FixMe: replace code in respire.py with this.
    '''
    return fetch_ann(Annotations,name).repeat(SamPerMin)
def read_expert(path, arg=None):
    '''Variant of fetch_annotations() designed to match the call of
    read_lphr and read_resp.  For example, if path were
    "../raw_data/apnea/summary_of_training/a01", this would return
    fetch_annotations('../raw_data/apnea/summary_of_training', 'a01')

    '''
    import os.path
    return [fetch_annotations(*os.path.split(path))]

def read_data(data_file):
    '''Read in "data_file" as a 2-d array

    Parameters
    ----------
    data_file : path

    Returns
    -------
    data : array
        data[i,j] is the jth number on the ith line

    '''
    data = [[float(x) for x in line.split()] for line in open(data_file, 'r')]
    return np.array(data).T

def read_lphr(path, AR):
    """ Read a file and create observation list for class Heart_Rate

    Notes for context: context[t, 0] = 1.0, context[1] = hr[t-1],
    context[t, AR] = hr[t-AR]

    FixMe: test this function

    Parameters
    ----------
    path : string
        Path of low pass heart rate data file to read
    AR : int
        Auto-regressive order of the model.  Context vector length

    Returns
    -------
    hr : array
        hr[t] is the scaled low pass heart rate at time t (6 second intervals)
    context : array
        context[t, 0] = 1, and context[t, s] = hr[t-s] for s in [1,AR]

    """
    raw = read_data(path)[2]
    n_y = len(raw)
    hr = np.empty(n_y)
    context = np.empty((n_y,AR+1))
    for t in range(AR+2):
        context[t, :] = raw[0]  # No real data before t=0.  Is this OK?
        context[t, 0:min(AR+1,t+1)] = raw[:t+1][::-1][0:min(AR+1,t+1)]
        hr[t] = raw[t]
    for t in range(AR+2, n_y):
        context[t,1:] = raw[t-1:t-AR-1:-1]
        hr[t] = raw[t]
    raw.sort()
    scale = 2.0/raw[int(n_y*.8)]
    hr *= scale
    context *= scale
    context[:,0] = 1
    return hr, context

def read_resp(path, arg=None):
    """ Read respiration data and create observation list for class Resp

    Note: Explain respiration vectors here

    Parameters
    ----------
    path : string
        Path of respiration data file to read

    Returns
    -------
    data : list
        Single entry is time series of respiration vectors

    """
    return (read_data(path)[1:].T,)

def read_records(
        readers,  # (read_lphr, read_resp), (read_lphr,) or (read_resp,)
        paths,    # list of strings pointing to data dirs
        args,     # (AR, None), (AR,) or (None,)
        records   # List of records to process, eg, ['a01','a02',...]
        ):
    """Read the records specified.  Return dict of observation lists.
       Each list should be suitable for the join() or calc() methods
       of an observation model.

    """
    from os.path import join
    assert len(readers) == len(paths)
    assert len(readers) == len(args)
    rv = {}
    for name in records:
        rv[name] = []
        lengths = []
        for reader, path, arg in zip(readers, paths, args):
            x = reader(join(path, name), arg)
            lengths.append(len(x[0]))
            rv[name] += x
        L = min(lengths)
        for i in range(len(rv[name])):
            rv[name][i] = rv[name][i][:L]
    return rv

def build_data(y_mod, args, use_class=True):
    '''Make dict of observation lists.

    args.expert   Path to file of expert markings
    args.resp_dir Directory of respiration data
    args.hr_dir   Directory of low pass heart rate data
    args.record   List of records

    For each record in args.record, make a list suitable for the
    join() and calc() methods of y_mod.  Data locations are specified
    by a combination of args.expert, args.resp_dir, and args.hr_dir
    appropriate for the class of y_mod.

    '''
    y_class = y_mod.__class__
    if y_class is Class_y:
        if use_class:
            readers = [read_expert]
            paths = [args.expert]
            args_ = [None]
        else:
            readers = []
            paths = []
            args_ = []
        y_mod = y_mod.y_mod
        y_class = y_mod.__class__
    else:
        assert args.expert is None
        readers = []
        paths = []
        args_ = []
    if y_class in (Heart_Rate, Both, fudge_pow):
        if y_class is Heart_Rate:
            assert args.resp_dir is None
            AR = y_mod.A.shape[1] - 1
        else:
            AR = y_mod.hr_mod.A.shape[1] - 1
        readers.append(read_lphr)
        paths.append(args.hr_dir)
        args_.append(AR)
    if y_class in (Resp, Both, fudge_pow):
        if y_class is Resp:
            assert args.hr_dir is None
        readers.append(read_resp)
        paths.append(args.resp_dir)
        args_.append(None)
    return read_records(readers, paths, args_, args.record)
class Resp(Discrete_Observations):
    """ Observation model for respiration signal.

    y_ = (y,) where y.shape = (T, 3)
    
    """
    def random_out(self, s):
        raise RuntimeError(
            'random_out() not implemented for %s'%self.__class__)
    def __init__(self, params):
        mu, Icov, norm = params
        self.mu=np.array(mu)      # n_states x 3
        self.Icov=np.array(Icov)  # n_states x 3 x 3
        self.norm=np.array(norm)  # n_states
        self.n_states = len(self.norm)
        self.dtype = [np.float64]
        assert(self.mu.shape == (self.n_states, 3))
        assert(self.Icov.shape == (self.n_states, 3, 3))
        self.P_Y = None
        return
    def __str__(self # Resp
                ):
        save = np.get_printoptions
        np.set_printoptions(precision=3)
        rv = 'Model %s instance\n'%self.__class__
        for i in range(self.n_states):
            rv += 'For state %d:\n'%i
            rv += ' Icov = \n%s\n'%self.Icov[i]
            rv += ' mu = %s'%self.mu[i]
            rv += ' norm = %f\n'%self.norm[i]
        np.set_printoptions(save)
        return rv
    def calc(self, y_):
        """
        Calculate and return likelihoods: self.P_Y[t,i] = P(y(t)|s(t)=i)

        Parameters
        ----------
        y_ : (y,)
            A sequence of vector observations.  y.shape = (n_y, 3)

        Returns
        -------
        P_Y : array, floats
            P_Y.shape = (n_y, n_states)

        """
        y = y_[0]
        n_y = len(y)
        self.P_Y = initialize(self.P_Y, (n_y, self.n_states))
        for t in range(n_y):
            for i in range(self.n_states):
                d = (y[t]-self.mu[i])
                dQd = np.dot(d, np.dot(self.Icov[i], d))
                if dQd > 300: # Underflow
                    self.P_Y[t,i] = 0
                else:
                    self.P_Y[t,i] = self.norm[i]*math.exp(-dQd/2)
        return self.P_Y
    def reestimate(self, # Resp instance
                   w,    # w[t,i] = prob s(t) = i
                   y_):
        y = y_[0]
        n_y, Dim = y.shape
        assert Dim == 3
        assert n_y > 50
        wsum = w.sum(axis=0)
        self.mu = (np.inner(y.T, w.T)/wsum).T
        # Inverse Wishart prior parameters.  Without data sigma_sq = b/a
        a = 4
        b = 0.1
        for i in range(self.n_states):
            rrsum = np.zeros((Dim,Dim))
            for t in range(n_y):
                r = y[t]-self.mu[i]
                rrsum += w[t,i]*np.outer(r, r)
            cov = (b*np.eye(Dim) + rrsum)/(a + wsum[i])
            det = LA.det(cov)
            assert (det > 0.0)
            self.Icov[i,:,:] = LAI(cov)
            self.norm[i] = 1.0/(math.sqrt((2*math.pi)**Dim*det))
        return
class Heart_Rate(Resp):
    """ Autoregressive observation model for heart rate signal.
    y[0][t] = hr
    y[1][t] = context
    p(y|s,c) = Normal((y-A[s]c),Var[s])
    """
    def __init__(self, params):
        A, Var, norm = params
        self.A=np.array(A)
        self.Var=np.array(Var)
        self.norm=np.array(norm)
        self.n_states = len(self.norm)
        self.dtype = [np.float64]*2
        self.P_Y = None
    def __str__(self # Heart_Rate
                ):
        save = np.get_printoptions
        np.set_printoptions(precision=3)
        rv = 'Model %s instance\n'%self.__class__
        for i in range(self.n_states):
            rv += 'For state %d:\n'%i
            rv += ' A = \n%s'%self.A[i]
            rv += ' Var = %s'%self.Var[i]
            rv += ' norm = %f\n'%self.norm[i]
        np.set_printoptions(save)
        return rv
    def calc(self, y):
        hr = y[0]
        context = y[1]
        n_y = len(hr)
        self.P_Y = initialize(self.P_Y, (n_y, self.n_states))
        d =  hr - np.inner(self.A,context)
        for i in range(self.n_states):
            z = np.minimum(d[i]*d[i]/(2*self.Var[i]),300.0)
            # Cap z to stop underflow
            self.P_Y[:,i] = np.exp(-z)*self.norm[i]
        return self.P_Y
    def reestimate(self, # Heart_Rate instance
                   w,    # w[t,i] = prob s(t) = i
                   y):
        hr = y[0]
        context = y[1]
        n_y, dim = context.shape
        mask = w >= small    # Small weights confuse the residual
                             # calculation in least_squares()
        w2 = mask*w
        wsum = w2.sum(axis=0)
        w1 = np.sqrt(w2)      # n_y x n_states array of weights
        # Inverse Wishart prior parameters.  Without data, sigma = b/a
        a = 4
        b = 16
        for i in range(self.n_states):
            w_hr = w1.T[i] * hr           # Tx1
            w_context = (w1.T[i] * context.T).T # Tx(Dim-1)
            A,resids,rank,s = LA.lstsq(w_context, w_hr)
            z = w_hr - np.inner(w_context,A) # z[t] is a number
            zz = float(np.inner(z,z))  # zz is a number
            self.Var[i] = (b+zz)/(a+wsum[i])
            self.A[i,:] = A
            self.norm[i] = 1/math.sqrt(2*math.pi*self.Var[i])
        return
class Both(Resp):
    """ Observe both heart rate and respiration signals
    y = (hr, context, resp)
    """
    def __init__(self, params):
        hr_params, resp_params = params
        self.hr_mod = Heart_Rate(hr_params)
        self.resp_mod = Resp(resp_params)
        self.n_states = self.hr_mod.n_states
        self.dtype = [np.float64]*3
    def __str__(self # Both
                ):
        return '''Model %s instance with heart rate component:
%s

and Respiration component:
%s'''%(self.__class__,self.hr_mod, self.resp_mod)
    def calc(self,  # Both instance
             y
             ):
        hr, context, resp = y
        self.P_Y = self.hr_mod.calc((hr, context)) * self.resp_mod.calc((resp,))
        return self.P_Y
    def reestimate(self, # Both instance
                   w,    # w[t,i] = prob s(t) = i
                   y):
        hr, context, resp = y
        self.hr_mod.reestimate(w,(hr, context))
        self.resp_mod.reestimate(w,(resp,))
        return


class fudge_pow(Both):
    '''Variant of class "Both" with parameters fudge and pow.

    "fudge" multiplies all probabilities for normal states, and the
    heart rate component of the likelihood is raised to power "pow"

    '''
    def __init__(self, both, fudge, pow_, s2c):
        self.hr_mod = both.hr_mod
        self.resp_mod = both.resp_mod
        self.n_states = both.hr_mod.n_states
        self.dtype = both.dtype
        self.P_Y = both.P_Y
        self.fudge = fudge
        self.pow = pow_
        self.s2c = s2c
    def calc(self,  # fudge_pow instance
             y
             ):
        hr, context, resp = y
        self.P_Y = self.hr_mod.calc((hr, context))**self.pow
        self.P_Y *= self.resp_mod.calc((resp,))
        for s in range(self.n_states):
            if self.s2c[s] == 0:
                self.P_Y[:, s] *= self.fudge
        return self.P_Y

#Local Variables:
#mode:python
#End:
