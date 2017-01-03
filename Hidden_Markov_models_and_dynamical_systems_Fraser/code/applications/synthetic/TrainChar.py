""" TrainChar.py y_data out_data

Read an integer time series from the file "y_data" and write 5
training characteristics to a file named "out_data".
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
import sys

def random_hmm(Card_Y, N_states, seed):
    '''
    '''

    from hmm.C import HMM
    from Scalar import make_random as r_p
    from numpy import random
    random.seed(seed)
    P_S0 = r_p((1,N_states))[0]
    P_S0_ergodic = r_p((1,N_states))[0]
    P_ScS = r_p((N_states,N_states))
    P_YcS = r_p((N_states,Card_Y))
    return HMM(P_S0, P_S0_ergodic, P_YcS, P_ScS)

def main(argv=None):
    import numpy as np
    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    assert len(argv) == 2
    y_file, out_name = argv

    niterations = 500
    N_states = 12
    Y = np.array([int(line) for line in open(y_file,'r')], np.int32)
    Card_Y = Y.max()
    Y -= 1
    n_seeds = 5
    LL = np.empty((niterations, n_seeds))
    for seed in range(n_seeds):
        model = random_hmm(Card_Y, N_states, seed)
        LL[:, seed] = model.train([Y], niterations, display=False)
    f = open(out_name, 'w')
    for i in range(niterations):
        print('%3d'%i, (n_seeds*' %7.4f')%tuple(LL[i]), file=f)
    f.close()
    return 0

if __name__ == "__main__":
    sys.exit(main())

#Local Variables:
#mode:python
#End:
