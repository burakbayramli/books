''' Scalar.py: Implements basic HMM algorithms.  Default observation
models are defined in the "Scalar" module.

'''
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
from hmm.Scalar import initialize, Prob, Discrete_Observations, Class_y
from hmm.Scalar import make_prob

class HMM:
    '''A Hidden Markov Model implementation.

    Parameters
    ----------
    P_S0 : array_like
        Initial distribution of states
    P_S0_ergodic : array_like
        Stationary distribution of states
    y_params : python object
        Observation model parameters
    P_SS : array_like
        P_SS[a,b] = Prob(s(1)=b|s(0)=a)
    y_class : class
       Observation model created by: y_class(y_params)
    prob : function, optional
        Function to make conditional probability matrix

    Examples
    --------
    Illustrate/Test some methods by manipulating the HMM in Figure 1.6
    of the book.

    >>> P_S0 = np.array([1./3., 1./3., 1./3.])
    >>> P_S0_ergodic = np.array([1./7., 4./7., 2./7.])
    >>> P_SS = np.array([
    ...         [0,   1,   0],
    ...         [0,  .5,  .5],
    ...         [.5, .5,   0]
    ...         ],np.float64)
    >>> P_YS = np.array([
    ...         [1, 0,     0],
    ...         [0, 1./3., 2./3.],
    ...         [0, 2./3., 1./3.]
    ...         ])
    >>> mod = HMM(P_S0,P_S0_ergodic,P_YS,P_SS)
    >>> S,Y = mod.simulate(500)
    >>> Y = (np.array(Y[0], np.int32),)
    >>> E = mod.decode(Y)
    >>> table = ['%3s, %3s, %3s'%('y','S','Decoded')]
    >>> table += ['%3d, %3d, %3d'%triple for triple in zip(Y[0], S, E[:10])]
    >>> for triple in table:
    ...     print(triple)
      y,   S, Decoded
      2,   1,   1
      2,   1,   1
      1,   2,   2
      0,   0,   0
      1,   1,   1
      1,   2,   2
      2,   1,   1
      1,   2,   2
      2,   1,   1
      2,   2,   1
    >>> L = mod.train(Y,n_iter=4)
    it= 0 LLps=  -0.920
    it= 1 LLps=  -0.918
    it= 2 LLps=  -0.918
    it= 3 LLps=  -0.917
    >>> print(mod)
    <class '__main__.HMM'> with 3 states
    P_S0         = [ 0.     0.963  0.037]
    P_S0_ergodic = [ 0.142  0.58   0.278]
    P_SS =
    [[ 0.     1.     0.   ]
     [ 0.     0.519  0.481]
     [ 0.512  0.488  0.   ]]
    P_YS =
    [[ 1.     0.     0.   ]
     [ 0.     0.335  0.665]
     [ 0.     0.726  0.274]
    
    '''
    def __init__(
        self,         # HMM instance
        P_S0,         # Initial distribution of states
        P_S0_ergodic, # Stationary distribution of states
        y_params,     # Parameters of observation model
        P_SS,         # P_SS[a,b] = Prob(s(1)=b|s(0)=a)
        y_class=Discrete_Observations,
        prob=make_prob# Function to make conditional probability matrix
        ):
        '''Builds a new Hidden Markov Model
        '''
        self.n_states = len(P_S0)
        self.P_S0 = np.array(P_S0)
        self.P_S0_ergodic = np.array(P_S0_ergodic)
        self.P_SS = prob(P_SS)
        self.y_mod = y_class(y_params)
        self.alpha = None
        self.gamma = None
        self.beta = None
        self.n_y = None
        return # End of __init__()
    def P_Y_calc(self, # HMM instance
                 y):
        '''Calculate the observation probabilities.

        Also store result in self and assign self.n_y.

        Parameters
        ----------
        y : list
            List of sequences of observation components.

        Returns
        -------
        P_Y : array
            1-d numpy array of probabilities.
        '''
        self.P_Y = self.y_mod.calc(y)
        self.n_y = len(self.P_Y)
        return self.P_Y
    def forward(self # HMM instance
            ):
        '''
        Recursively calculate state probabilities

        Requires that observation probabilities have already been calculated

        Parameters
        ----------
        None

        Returns
        -------
        L : float
            Average log (base e) likelihood per point of entire observation
            sequence 

        Bullet points
        -------------

        On entry:

        * self          is an HMM

        * self.P_Y      has been calculated

        * self.n_y      is length of Y

        * self.n_states is number of states

        Bullet points
        -------------

        On return:

        * self.gamma[t] = Pr{y(t)=y(t)|y_0^{t-1}}
        * self.alpha[t,i] = Pr{s(t)=i|y_0^t}
        * return value is log likelihood of all data

        '''

        # Ensure allocation and size of alpha and gamma
        self.alpha = initialize(self.alpha, (self.n_y, self.n_states))
        self.gamma = initialize(self.gamma, (self.n_y,))
        last = np.copy(self.P_S0.reshape(-1)) # Copy
        for t in range(self.n_y):
            last *= self.P_Y[t]              # Element-wise multiply
            self.gamma[t] = last.sum()
            last /= self.gamma[t]
            self.alpha[t, :] = last
            self.P_SS.step_forward(last)
        return (np.log(self.gamma)).sum() # End of forward()
    def backward(self # HMM instance
    ):
        '''
        Baum Welch backwards pass through state conditional likelihoods.

        Calculates values of self.beta which "reestimate()" needs.

        Parameters
        ----------
        None

        Returns
        -------
        None
        
        Bullet points
        -------------

        On entry:

        * self     is an HMM

        * self.P_Y has been calculated

        * self.gamma has been calculated by forward

        Bullet points
        -------------

        On return:

        * for each state i, beta[t,i] = Pr{y_{t+1}^T|s(t)=i}/Pr{y_{t+1}^T}

        '''
        # Ensure allocation and size of beta
        self.beta = initialize(self.beta, (self.n_y, self.n_states))
        last = np.ones(self.n_states)
        # iterate
        for t in range(self.n_y-1, -1, -1):
            self.beta[t, :] = last
            last *= self.P_Y[t]
            last /= self.gamma[t]
            self.P_SS.step_back(last)
        return # End of backward()
    def train(self,  # HMM instance
              y, n_iter=1, display=True):
        '''Based on observations y, do n_iter iterations of model reestimation

        Use Baum-Welch algorithm to search for maximum likelihood
        model parameters.

        Parameters
        ----------
        y : array_like
            Sequence of integer observations
        n_iter : int, optional
            Number of iterations
        display : bool, optional
            If True, print the log likelihood per observation for each
            iteration

        Returns
        -------
        LLL : list
            List of log likelihood per observation for each iteration

        '''
        # Do (n_iter) BaumWelch iterations
        LLL = []
        for it in range(n_iter):
            self.P_Y_calc(y)
            LLps = self.forward()/self.n_y # log likelihood per step
            if display:
                print("it= %d LLps= %7.3f"%(it, LLps))
            LLL.append(LLps)
            self.backward()
            self.reestimate(y)
        return LLL # End of train()
    def reestimate(self,  # HMM instance
                   y):
        '''Reestimate model parameters

        Code here updates state transition probabilities and initial
        state probabilities.  Contains a call to the y_mod.reestimate
        method that updates observation model parameters.

        Parameters
        ----------
        y : array
            Sequence of observations

        Returns
        -------
        None

        '''
        u_sum = np.zeros((self.n_states, self.n_states), np.float64)
        for t in np.where(self.gamma[1:]>0)[0]: # Skip segment boundaries
            u_sum += np.outer(self.alpha[t]/self.gamma[t+1],
                              self.P_Y[t+1]*self.beta[t+1, :])
        self.alpha *= self.beta
        wsum = self.alpha.sum(axis=0)
        self.P_S0_ergodic = np.copy(wsum)
        self.P_S0 = np.copy(self.alpha[0])
        for x in (self.P_S0_ergodic, self.P_S0):
            x /= x.sum()
        assert u_sum.shape == self.P_SS.shape
        self.P_SS.inplace_elementwise_multiply(u_sum)
        self.P_SS.normalize()
        self.y_mod.reestimate(self.alpha,y)
        return # End of reestimate()
    def decode(self,  # HMM instance
               y, P_Y=None):
        '''
        Find the most likely state sequence for a given observation sequence

        Parameters
        ----------
        y : array_like
            Sequence of observations
        P_Y : array_like
            Array of probabilities of observations

        Returns
        -------
        ss : array
            Maximum likelihood state sequence

        '''
        if P_Y is None:
            P_Y = self.P_Y_calc(y)
        pred = np.empty((self.n_y, self.n_states), np.int32) # Best predecessors
        ss = np.ones((self.n_y, 1), np.int32)       # State sequence
        nu = P_Y[0] * self.P_S0
        for t in range(1, self.n_y):
            cost = self.P_SS.cost(nu, P_Y[t])# P_SS*outer(nu, P_Y[t])
            pred[t] = cost.argmax(axis=0)    # Best predecessor
            nu = np.choose(pred[t],cost)     # Cost of best paths to each state
            nu /= nu.max()                   # Prevent underflow
        last_s = np.argmax(nu)
        for t in range(self.n_y-1, -1, -1):
            ss[t] = last_s
            last_s = pred[t,last_s]
        return ss.flat # End of viterbi
    # End of decode()
    def class_decode(
        self,  # HMM instance
        y      # Observations
        ):
        '''
        Tries to find maximum likelihood class sequence.

        Note that the observation class provided to HMM must, like
        *Class_y*, have a c2s (class to state) array.

        Parameters
        ----------
        y : array_like
            Sequence of observations

        Returns
        -------
        cs : array
            Estimated classification sequence

        Examples
        --------

        >>> from scipy.linalg import circulant
        >>> np.set_printoptions(precision=3, suppress=True)
        >>> c2s = {
        ...     0:[0,1],
        ...     1:[2,3],
        ...     2:[4,5],
        ...     }
        >>> P_S0 = np.ones(6)/6.0
        >>> P_SS = circulant([0,  0, 0, 0, .5, .5])
        >>> P_YS = circulant([.4, 0, 0, 0, .3, .3])
        >>> pars = (Discrete_Observations, P_YS, c2s)
        >>> mod = HMM(P_S0, P_S0, pars, P_SS, Class_y, make_prob)
        >>> S,CY = mod.simulate(1000)
        >>> CY = [np.array(CY[0], np.int32), np.array(CY[1], np.int32)]
        >>> p_s = 0.7*P_SS + 0.3/6
        >>> p_y = 0.7*P_YS + 0.3/6
        >>> pars = (Discrete_Observations, p_y, c2s)
        >>> mod = HMM(P_S0, P_S0, pars, p_s, Class_y, make_prob)
        >>> L = mod.train(CY, n_iter=20, display=False)

        Maximum likelihood estimation (training) yeilds a model that
        is similar to the model used to make the data.

        >>> print(mod)
        <class '__main__.HMM'> with 6 states
        P_S0         = [ 0.  1.  0.  0.  0.  0.]
        P_S0_ergodic = [ 0.184  0.161  0.151  0.177  0.174  0.153]
        P_SS =
        [[ 0.     0.485  0.515  0.     0.     0.   ]
         [ 0.     0.     0.351  0.649  0.     0.   ]
         [ 0.     0.     0.     0.484  0.516  0.   ]
         [ 0.     0.     0.     0.     0.543  0.457]
         [ 0.586  0.     0.     0.     0.     0.414]
         [ 0.539  0.461  0.     0.     0.     0.   ]]
        <class 'hmm.Scalar.Class_y'> with c2s =
        [[1 1 0 0 0 0]
         [0 0 1 1 0 0]
         [0 0 0 0 1 1]]
        P_YS =
        [[ 0.439  0.281  0.28   0.     0.     0.   ]
         [ 0.     0.362  0.314  0.324  0.     0.   ]
         [ 0.     0.     0.418  0.305  0.277  0.   ]
         [ 0.     0.     0.     0.384  0.283  0.333]
         [ 0.282  0.     0.     0.     0.391  0.328]
         [ 0.301  0.366  0.     0.     0.     0.333]

        Here are some of the log liklihoods per observation from the
        sequence of training iterations.  Note that they increase
        monotonically and that at the end of the sequence the change
        per iteration is less that a part in a thousand.

        >>> for i in [0, 1, 2, len(L)-2, len(L)-1]:
        ...     print('%2d: %6.3f'%(i, L[i]))
         0: -1.972
         1: -1.682
         2: -1.657
        18: -1.641
        19: -1.641

        Next, we drop the simulated class data from YC and demonstrate
        Viterbi decoding of the class sequence.  We designed the model
        so that decoding would be good rather than perfect, but there
        are no errors in this short sequence.  In the sequence below
        we've printed the observation y[i], the simulated class c[i]
        and the decoded class d[i], ie,

        y[i]  c[i]  d[i]

        >>> D = mod.class_decode((CY[1][:5],))
        >>> for (c, y, d) in zip(CY[0], CY[1], D):
        ...     print('%3d, %3d, %3d'%(y, c, d))
          2,   0,   0
          3,   1,   1
          0,   2,   2
          2,   0,   0
          1,   0,   0

        '''
        P_Y = self.y_mod.y_mod.calc(y)
        old_set = set([ClassHistory(
            tuple(),            # Empty history
            self.P_S0_ergodic,  # phi, ie, conditional utility of states
            1.0,                # score
            self.y_mod.c2s,     # c2s[c,s] == 1 if s in c
            self.P_SS           # P_SS[a,b] = P(b|a)
            )])
        n_c, n_s = self.y_mod.c2s.shape
        for t in range(len(y[0])):
            max_h = None        # new history with maximum score
            by_class = np.empty(5, dtype=np.object_)
            for c in range(n_c):
                by_class[c] = []# list of histories that end in c
            for history in old_set:
                for new_h in history.fork(P_Y[t]):
                    c = new_h.path[-1]
                    by_class[c].append(new_h)
                    if max_h is None or new_h.score > max_h.score:
                        max_h = new_h
            new_set = set()
            for c in range(n_c):
                if len(by_class[c]) == 0:
                    continue
                new_set.add(max(by_class[c], key=lambda x: x.score))
                for s in range(n_s):
                    if self.y_mod.c2s[c,s] == 0:
                        continue
                    new_set.add(max(by_class[c],
                        key=lambda x: x.score*x.phi[s]))
            for history in new_set:
                history.score /= max_h.score
            old_set = new_set
        return max_h.path
    def broken_decode(self, y): # Algorithm from first edition of book
        c2s = self.y_mod.c2s
        n_c = len(c2s)
        n_y = len(y[0])
        s1 = np.arange(self.n_states, dtype=np.int32) #Index for cs_cost -> phi
        c1 = self.y_mod.s2c[s1] # Index for cs_cost -> phi
        P_Y = self.y_mod.y_mod.calc(y) # P_Y[t,s] = prob(Y=y[t]|state=s)

        # Do partial first iteration before loop
        pred = np.empty((n_y,n_c),np.int32)
        phi = self.P_S0_ergodic * P_Y[0]
        nu = np.dot(c2s,phi)
        phi /= np.maximum(1.0e-300, np.dot(np.dot(c2s, phi), c2s))

        for t in range(1,n_y): # Main loop
            ss_cost = self.P_SS.cost(np.dot(nu, c2s)*phi, P_Y[t])
            cs_cost = np.dot(c2s, ss_cost)    # Cost for class-state pairs
            cc_cost = np.dot(cs_cost, c2s.T)  # Cost for class-class pairs
            pred[t] = cc_cost.argmax(axis=0)  # Best predecessor for each class
            nu = np.choose(pred[t], cc_cost)  # Class cost given best history
            nu /= nu.max()
            phi = cs_cost[pred[t,c1],s1] # phi[s] is prob(state=s|best
                                         # class sequence ending in c[s])
            phi /= np.maximum(1.0e-300, np.dot(np.dot(c2s, phi), c2s))
        # Backtrack
        seq = np.empty((n_y,),np.int32)
        last_c = np.argmax(nu)
        for t in range(n_y-1,-1,-1):
            seq[t] = last_c
            last_c = pred[t, last_c]
        return seq
    def initialize_y_model(self, y, s_t=None, segs=None):
        ''' Given data and plausible P_SS, make plausible y_model.
        '''
        n_y = len(y[0])
        if s_t is None:
            s_t = np.array(self.state_simulate(n_y), np.int32)
        alpha = np.zeros((n_y, self.n_states))
        t = np.arange(n_y)
        alpha[t,s_t] = 1
        self.alpha = alpha
        self.beta = alpha.copy()
        self.gamma = np.ones(n_y)
        self.P_Y = np.ones((n_y, self.n_states))
        if segs is not None:
            for b in segs[1:-1]:
                self.gamma[b] = -1
        self.reestimate(y)

    def state_simulate(
            self,  # HMM instance
            length, mask=None, seed=3):
        ''' Generate a random sequence of states

        Parameters
        ----------

        length : int
            Number of time steps to simulate
        mask : array/None
            If mask.shape[t, i] is False, state i is forbidden at time t
        seed : int, optional
            Seed for random number generator

        Returns
        -------

        states : list
            Sequence of states
        '''
        import numpy.random
        numpy.random.seed(seed)
        P_Y = numpy.random.random((length, self.n_states))
        self.n_y = length
        if mask is not None:
            P_Y *= mask
        return self.decode(None, P_Y) # End of state_simulate()
    def simulate(
            self,  # HMM instance
            length, seed=3):
        '''
        Generate a random sequence of observations of given length

        Parameters
        ----------

        length : int
            Number of time steps to simulate
        seed : int, optional
            Seed for random number generator

        Returns
        -------

        states : list
            Sequence of states
        outs : list
            Sequence of observations
        '''
        import random
        random.seed(seed)
        # Initialize lists
        outs = []
        states = []
        # Set up cumulative distributions
        cum_init = np.cumsum(self.P_S0_ergodic[0])
        cum_tran = np.cumsum(self.P_SS.values(), axis=1)
        # cum_rand generates random integers from a cumulative distribution
        cum_rand = lambda cum: np.searchsorted(cum, random.random())
        # Select initial state
        i = cum_rand(cum_init)
        # Select subsequent states and call model to generate observations
        for t in range(length):
            states.append(i)
            outs.append(self.y_mod.random_out(i))
            i = cum_rand(cum_tran[i])
        # Take "transpose" of outs.  Observations are lists of time series 
        outs_T = []
        for x in outs[0]:
            outs_T.append([x])
        for t in range(1,len(outs)):
            for i in range(len(outs_T)):
                outs_T[i].append(outs[t][i])
        return (states, outs_T) # End of simulate()
    def link(
            self,    # HMM instance
            from_, to_, p):
        ''' Create (or remove) a link between state "from_" and state "to_".

        The strength of the link is a function of both the argument
        "p" and the existing P_SS array.  Set P_SS itself if you
        need to set exact values.  Use this method to modify topology
        before training.

        Parameters
        ----------
        from_ : int
        to_ : int
        p : float

        Returns
        -------
        None

        FixMe: No test coverage.  Broken for cython code
        '''
        self.P_SS[from_,to_] = p
        self.P_SS[from_, :] /= self.P_SS[from_, :].sum()
    def __str__(
            self   # HMM instance
    ):
        save = np.get_printoptions
        np.set_printoptions(precision=3)
        rv = '''
%s with %d states
P_S0         = %s
P_S0_ergodic = %s
P_SS =
%s
%s''' % (self.__class__, self.n_states, self.P_S0, self.P_S0_ergodic,
         self.P_SS.values(), self.y_mod
         )
        np.set_printoptions(save)
        return rv[1:-1]
    def multi_train(
            self,         # HMM instance
            ys,           # List of observation sequences
            n_iter=1,
            boost_w=None, # Optional weight of each observation for reestimation
            display=True
        ):
        '''Train on multiple sequences of observations

        Parameters
        ----------
        ys : list
            list of sequences of integer observations
        n_iter : int, optional
            Number of iterations
        boost_w : array, optional
            Weight of each observation for reestimation
        display : bool, optional
            If True, print the log likelihood per observation for each
            segment and each iteration

        Returns
        -------
        avgs : list
            List of log likelihood per observation for each iteration

        Examples
        --------
        Same model as used to demonstrate train().  Here simulated
        data is broken into three independent segments for training.
        For each iteration, "L[i]" gives the log likelihood per data
        point of segment "i".  Note that L[2] does not improve
        monotonically but that the average over segments does.

        >>> P_S0 = np.array([1./3., 1./3., 1./3.])
        >>> P_S0_ergodic = np.array([1./7., 4./7., 2./7.])
        >>> P_SS = np.array([
        ...         [0,   1,   0],
        ...         [0,  .5,  .5],
        ...         [.5, .5,   0]
        ...         ],np.float64)
        >>> P_YS = np.array([
        ...         [1, 0,     0],
        ...         [0, 1./3., 2./3.],
        ...         [0, 2./3., 1./3.]
        ...         ])
        >>> mod = HMM(P_S0,P_S0_ergodic,P_YS,P_SS)
        >>> S,Y = mod.simulate(600)
        >>> ys = []
        >>> for i in [1,2,0]:
        ...     ys.append([x[200*i:200*(i+1)] for x in Y])
        >>> A = mod.multi_train(ys,3)
        i=0: L[0]=-0.9162 L[1]=-0.9142 L[2]=-0.9275 avg=-0.9193117
        i=1: L[0]=-0.9122 L[1]=-0.9082 L[2]=-0.9252 avg=-0.9152414
        i=2: L[0]=-0.9112 L[1]=-0.9080 L[2]=-0.9249 avg=-0.9147362

        '''
        n_seg, t_seg, y_all = self.y_mod.join(ys)
        avgs = n_iter*[None] # Average log likelihood per step
        t_total = t_seg[-1]
        alpha_all = initialize(self.alpha, (t_total, self.n_states))
        beta_all = initialize(self.beta, (t_total, self.n_states))
        gamma_all = initialize(self.gamma, (t_total,))
        P_S0_all = np.empty((n_seg, self.n_states))
        #P_S0_all are state probabilities at the beginning of each segment
        for seg in range(n_seg):
            P_S0_all[seg, :] = self.P_S0.copy()
        for i in range(n_iter):
            if display:
                print('i=%d: '%i, end='')
            tot = 0.0
            # Both forward() and backward() should operate on each
            # training segment and put the results in the
            # corresponding segement of the the alpha, beta and gamma
            # arrays.
            P_Y_all = self.P_Y_calc(y_all)
            for seg in range(n_seg):
                self.n_y = t_seg[seg+1] - t_seg[seg]
                self.alpha = alpha_all[t_seg[seg]:t_seg[seg+1], :]
                self.beta = beta_all[t_seg[seg]:t_seg[seg+1], :]
                self.P_Y = P_Y_all[t_seg[seg]:t_seg[seg+1]]
                self.gamma = gamma_all[t_seg[seg]:t_seg[seg+1]]
                self.P_S0 = P_S0_all[seg, :]
                LL = self.forward() #Log Likelihood
                if display:
                    print('L[%d]=%7.4f '%(seg,LL/self.n_y), end='')
                tot += LL
                self.backward()
                P_S0_all[seg, :] = self.alpha[0] * self.beta[0]
                self.gamma[0] = -1 # Don't fit transitions between segments
            avgs[i] = tot/t_total
            if i>0 and avgs[i-1] >= avgs[i]:
                print('''
WARNING training is not monotonic: avg[%d]=%f and avg[%d]=%f
'''%(i-1,avgs[i-1],i,avgs[i]))
            if display:
                print('avg=%10.7f'% avgs[i])
            # Associate all of the alpha and beta segments with the
            # states and reestimate()
            self.alpha = alpha_all
            self.beta = beta_all
            self.gamma = gamma_all
            self.P_Y = P_Y_all
            if boost_w != None:
                self.alpha *= BoostW
            self.n_y = len(P_Y_all)
            self.reestimate(y_all)
        self.P_S0[:] = P_S0_all.sum(axis=0)
        self.P_S0 /= self.P_S0.sum()
        return avgs

class ClassHistory:
    ''' For keeping track of good class histories
    To sort a list of histories: L.sort(key=lambda x: x.score)
    '''
    def __init__(self, path, phi, score, c2s, P_SS):
        self.path = path    # Sequence of past classes
        self.phi = phi      # phi[s] = P(s| y_0^t, path)
        self.score = score  # P(y_0^t, path)/norm
        self.c2s = c2s      # c2s[c,s] = 1 if class c contains state s
        self.P_SS = P_SS    # P_SS[a,b] = P(b|a)
        return
    def fork(self, # ClassHistory instance
             P_Y   # P_Y[s] = P(y(t)|s
    ):
        ''' Make and return a list of children
        '''
        import math

        n_classes, n_states = self.c2s.shape
        phi = self.phi
        self.P_SS.step_forward(phi)  # Modify phi in place
        phi = self.c2s*(phi*P_Y)     # phi[c,s] = P(s|c, path, y_0^{t-1})
        for c in range(n_classes):
            path = self.path + (c,)
            s = phi[c].sum()
            if s > 0:
                score = s * self.score
                yield ClassHistory(path, phi[c]/s, score, self.c2s, self.P_SS)
        return
def _test():
    import doctest
    doctest.testmod()

if __name__ == "__main__":
    _test()
#--------------------------------
# Local Variables:
# mode: python
# End:
