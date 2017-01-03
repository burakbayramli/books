''' MakeModel.py <H_dir> <data_dir> <data_file> <model_file>
 EG. python MakeModel.py data lorenz.4 m12s.4y
'''
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
import os.path
import pickle
import random
import numpy as np

from itertools import dropwhile

def skip_header(lines):
    isheader = lambda line: line.startswith("#")
    return dropwhile(isheader, lines)
    
def read_data(data_dir, data_file):
    '''Read data and return as numpy array
    '''
    lines = skip_header(open(os.path.join(data_dir, data_file), 'r'))
    y = np.array([int(line)-1 for line in lines],np.int32)
    return (y,), y.max()+1

#from hmm.base import HMM
#from hmm.C import HMM_SPARSE as HMM
from hmm.C import HMM

def main(argv=None):
    '''Call with arguments: n, data_dir, data_file, model_file
    
    n = number of iterations
    
    data_dir = directory for data and resulting model

    data_file = name of data file

    model_file = name of the file into which resulting model to be written
    '''

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    nstates = 12
    n, data_dir, data_file, model_file = argv
    niterations = int(n) # maximum number of iterations
    
    Y, cardy = read_data(data_dir, data_file)

    from Scalar import make_random as random_p
    from numpy import random
    random.seed(6)
    P_S0 = random_p((1,nstates))[0]
    P_S0_ergodic = random_p((1,nstates))[0]
    P_ScS = random_p((nstates,nstates))
    P_YcS = random_p((nstates,cardy))

    # Train the model
    mod = HMM(P_S0,P_S0_ergodic,P_YcS,P_ScS)
    mod.train(Y,n_iter=niterations)

    # Strip alpha, beta, and Py, then save model in <model_file>
    mod.alpha = None
    mod.beta = None
    mod.Py = None
    pickle.dump(mod, open(os.path.join(data_dir, model_file), 'wb'))
    return 0

if __name__ == "__main__":
    sys.exit(main())

#Local Variables:
#mode:python
#End:
