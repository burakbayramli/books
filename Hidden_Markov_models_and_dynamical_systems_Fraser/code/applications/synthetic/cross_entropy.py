"""corss_entropy.py 2013 modification/rewrite of Sparse_hmm_lor.py

For making the "likeLor" figure in that plots cross entropy vs number
of states, going up to millions of states.

Builds discrete output HMMs of quantized observations of the Lorenz
system by using information about the continuous states of the system.
Goal is to build an HMM that gets close to the entropy bound
estimated via Lyapunov exponents.

ToDo:

1. Perhaps restructure to let Model.__init__ call for more simulations
   of training data till performance on validation set saturates.

Notes:

1. 33 seconds to generate 8e6 points for training

2. For state dict use quantized (x,y,z).  Since range of each is about
   10, OK to use same quantization scale for each.

"""
Copyright = '''
Copyright 2013 Andrew M. Fraser and Los Alamos National Laboroatory

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
class Model:
    '''
    '''
    def __init__(self, q, xyz, res, safe):
        '''Build an HMM using a true state sequence, "xyz".  Obtain discrete
        sttes by quantizing all three components of xyz using
        resolution "res".  Estimate probabilities by counting, except
        use "safe" as the minimum probability of observing each of the
        4 possible outputs from each state.

        '''
        n = len(q)
        assert n == len(xyz)
        states = {tuple((xyz[0,:]/res).astype(np.int32)): (0, q[0])}
        state = 0
        pairs = {}
        for i in range(1,n):
            last = state
            key = tuple((xyz[i,:]/res).astype(np.int32))
            if key not in states:
                states[key] = (len(states), q[i])
            state = states[key][0]
            pair = (last, state)
            if pair in pairs:
                pairs[pair] += 1
            else:
                pairs[pair] = 1
        self.n_states = len(states)

        from scipy.sparse import dok_matrix
        t = dok_matrix((self.n_states, self.n_states))
        t.update(pairs)

        from hmm.C import cscProb as sparse_prob
        self.P_SS = sparse_prob(t)
        self.P_SS.normalize()

        from hmm.Scalar import Prob
        self.P_YS = Prob((self.n_states, 4))
        self.P_YS[:,:] = safe
        for state, y in states.values():
            self.P_YS[state, y] = 1.0 - 3*safe
        return
    def like(self, Y):
        import math
        n_states = self.n_states
        n_y = len(Y)
        alpha = np.ones(n_states)/n_states
        rv = 0.0
        for i in range(n_y):
            alpha *= self.P_YS[:,Y[i]] # Check Scalar.calc
            gamma = alpha.sum()
            rv += math.log(gamma)
            alpha /= gamma
            self.P_SS.step_forward(alpha)
        return rv/n_y
        
import sys
def main(argv=None):
    import argparse

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    parser = argparse.ArgumentParser(
        description='Study cross entropy dependence on number of states')
    parser.add_argument(
        '--exponents', type=float, nargs=3, default=[-1, 5, 0.5],
        help='Quantization resolution in powers of 2 (start, stop, step)')
    parser.add_argument('--n_relax', type=int, default=10000,
                       help='Number of steps for relaxing to attractor')
    parser.add_argument('--n_train', type=int, default=100000,
                       help='Number of samples for estimating cross entropy')
    parser.add_argument('--n_test', type=int, default=10000,
                       help='Number of samples for building model')
    parser.add_argument('--resultfile', type=argparse.FileType('w'),
                        help='Write results to this file')
    parser.add_argument('--s', type=float, default=10.0,
                       help='Lorenz s parameter')
    parser.add_argument('--r', type=float, default=28.0,
                       help='Lorenz r parameter')
    parser.add_argument('--b', type=float, default=8.0/3,
                       help='Lorenz b parameter')
    parser.add_argument('--dt', type=float, default=0.15,
                       help='Sample interval')
    parser.add_argument('--IC', type=float, nargs=3,
                        default=[11.580, 13.548, 28.677],
                        help='Initial conditions')
    parser.add_argument('--safety', type=float, default=1e-4,
                       help='Minimum conditional observation probability')
    args = parser.parse_args(argv)
    
    from lor_C import Lsteps

    xyz_train = Lsteps(np.array(args.IC), args.s, args.b, args.r, args.dt,
                 args.n_relax)
    xyz_train = Lsteps(xyz_train[0], args.s, args.b, args.r, args.dt,
                       args.n_train)
    q_train = np.ceil(xyz_train[:,0]/10+1).astype(np.int8)
    q_test = Lsteps(xyz_train[0], args.s, args.b, args.r, args.dt,
                       args.n_test)
    q_test = np.ceil(q_test[:,0]/10+1).astype(np.int8)
    for exponent in np.arange(*args.exponents):
        model = Model(q_train, xyz_train, 2**exponent, args.safety)
        log_like = model.like(q_test)
        print('%5.2f %6.3f %6d'%(exponent, log_like, model.n_states)
              ,file=args.resultfile)

    return 0

if __name__ == "__main__":
    sys.exit(main())

#Local Variables:
#mode:python
#End:
