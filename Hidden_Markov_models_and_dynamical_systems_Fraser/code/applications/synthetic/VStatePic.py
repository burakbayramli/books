"""
VStatePic.py data_dir y_name

Creates varg_stateN (N in 0..11) in the directory named by data

y[0][t] is a numpy.array containing the observation
y[1][t] is a numpy.array containing the context
context = (y[0][t-1],y[0][t-2],...,y[0][t-taumax],1.0)
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
import numpy as np
import sys

def main(argv=None):
    '''Call with arguments: data_dir vector_file

    Writes files named ['state%d'%n for n in range(nstates)] to the
    data_dir.  Each file consists of points in vector_file that are
    decoded to the the state number specified in the name.  The states
    are assigned by using the model in model_file to Viterbi decode
    the data in data_file.

    '''
    from os.path import join
    from MakeModel import skip_header

    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    data_dir, vector_file, state_file = argv

    # Read in time series of vectors
    vectors = np.array([list(map(float,line.split())) for line in
               skip_header(open(join(data_dir, vector_file),'r'))])
    n_y, Odim = vectors.shape
    n_y -= 1
    Cdim = Odim + 1
    assert Cdim == 4
    N_states = 12

    data = vectors[1:, :]
    context = np.empty((n_y, Cdim))
    context[:,:-1] = vectors[:-1,:]
    context[:,-1] = 1
    Y = [data, context]

    states = np.array(# Read in time series of states
        [list(map(int,line.split())) for line in
                       skip_header(open(join(data_dir, state_file),'r'))])
    model = MakeVARG_HMM(N_states,Odim,Cdim, Y, states) # Make initial model
    # Recall:   Cov = (b * np.eye(dim_Y) + ZZT)/(a + sum_w[i])
    for a,b in ((1e6, 4e6), (4.0, 1.0), (1.0, 0.25), (0.0, 0.0)):
        model.y_mod.thaw_var(a=a,b=b)
        model.train(Y, 10)
    states = model.decode(Y)                 # Do Viterbi decoding

    f = list(open(join(data_dir, 'varg_state'+str(s)), 'w') for
             s in range(N_states))
    for t in range(n_y):
        print('%7.4f %7.4f %7.4f'%tuple(data[t]), file=f[states[t]])
    return 0

def MakeVARG_HMM(Nstates,Odim,Cdim, Y, states):
    '''Returns a normalized random initial model
    '''
    from hmm.base import HMM
    from hmm.VARG import VARG
    P_S0 = np.empty(Nstates)
    P_S0_ergodic = np.empty(Nstates)
    P_ScS = np.empty((Nstates,Nstates))
    Icovs = np.empty((Nstates, Odim, Odim))
    params = (np.zeros((Nstates, Odim, Cdim)), # As
              np.empty((Nstates, Odim, Odim))  #Icovs
    )
    model = HMM(P_S0,P_S0_ergodic,params,P_ScS, VARG)
    model.initialize_y_model(Y, states)
    return model
    
if __name__ == "__main__":
    sys.exit(main())
                      
#Local Variables:
#mode:python
#End:
