'''mod_init.py script to create initial models for work on the
CINC2000 data.

Argument     Usual Value
-----------------------------------------------------------------
HR_dir       derived_data/apnea/low_pass_heart_rate 

Resp_Dir     derived_data/apnea/respiration

Expert       raw_data/apnea/summary_of_training

Model_Dir    derived_data/apnea
=================================================================
This script writes the following five models to "Model_Dir"

init_A2:        Two state, no class, AR=4.

mod_C1:         One state, no class, AR=4.  Make with one pass in this script

init_H:         Two class, topology in Fig 6.9a. Resp and AR=2

init_M:         Two class, topology in Fig 6.9b. Resp and AR=4

init_L:         Two class, topology in Fig 6.9b. Resp and AR=4 
=================================================================

Everything is hard coded with "magic" values.  From the book, I copy
the following specifications:

mod_A2: A two state HMM with AR-4 models for the lprh (low pass heart
        rate) data and Gaussian models for resp (the respiration
        data).  No classification used.

mod_C1: A one state model with AR-4 for lphr and a single Gaussian for
        resp

In the three models below, each state is a member of a class.

mod_H: Topology in Fig 6.9a.  Used if first pass stat is above 3.0.
AR order = 2

mod_M: Topology in Fig 6.9b.  Used if first pass stat is between 2.39
       and 3.0 AR order 4.

mod_L: Topology in Fig 6.9b.  Used if first pass stat is below 2.39.
       AR order 4.

Found AR orders by looking in hmmdsbook/code/Apnea/Makefile.

'''
Copyright = '''
Copyright2013 Andrew M. Fraser and Los Alamos National Laboroatory

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
def main(argv=None):
    '''Call with arguments: 

    '''

    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    assert len(argv) == 4
    hr_dir, resp_dir, expert, model_dir = argv

    import os
    import numpy as np
    from hmm.base import HMM
    from hmm import Scalar
    import ApOb
    import pickle
    def randomize(mod, args):
        '''Initialize a model with asymmetric parameters by simulating a
        random but consistent state sequence and fitting actual data
        assuming that sequence.

        '''
        y_mod = mod.y_mod
        n_seg, segs, data = y_mod.join(
            list(ApOb.build_data(mod.y_mod, args).values()))
        n_y = len(data[0])
        if y_mod.__class__ is Scalar.Class_y:
            c_series = data[0]
            s_t = mod.state_simulate(n_y, mask=y_mod.c2s[c_series])
        else:
            s_t = mod.state_simulate(n_y)
        s_t = np.array(s_t, np.int32)
        mod.initialize_y_model(data, s_t, segs)
        mod.P_S0 = mod.P_S0_ergodic
        return
    # Set up lists of records by the groups used for training
    all_records = os.listdir(hr_dir)
    a_records = [x for x in all_records if x.startswith('a')]
    b_records = [x for x in all_records if x.startswith('b')]
    c_records = [x for x in all_records if x.startswith('c')]

    # Create raw models.  P_SS* (and c2s if applicable) give the right
    # topology.  Space for the output model parameters is allocated.
    # Initial values of those parameters will be given later.
    resp_n = lambda n: (np.empty((n,3)), np.empty((n,3,3)), np.empty((n,1)))
    ar_m_n = lambda m, n: (np.empty((n, m+1)), np.empty((n,1)), np.empty((n,1)))
    
    P_SS_1 = np.ones((1,1), np.float64)
    P_S0_1 = np.ones((1,), np.float64)
    P_SS_2 = np.ones((2,2), np.float64)/2
    P_S0_2 = np.ones((2,), np.float64)/2

    mod_A2 = HMM(P_S0_2, P_S0_2, (ar_m_n(4, 2), resp_n(2)), P_SS_2, ApOb.Both)
    mod_C1 = HMM(P_S0_1, P_S0_1, (ar_m_n(4, 1), resp_n(1)), P_SS_1, ApOb.Both)

    #    1  2  3  4  5  6  7  8  9  10 11 12 13 14
    P_SS_69a = Scalar.Prob((14,14), dtype=np.float64, buffer=np.array([
        [1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], #1
        [1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0], #2
        [0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], #3
        [0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], #4
        [0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0], #5
        [0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0], #6
        [0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0], #7
        [0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0], #8
        [0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0], #9
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0], #10
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0], #11
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1], #12
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0], #13
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1]  #14
    ]))
    P_SS_69a.normalize()
    P_S0_14 = np.ones(14)/14
    c2s = {0:np.arange(7, dtype=np.int32), 1:np.arange(7, 14, dtype=np.int32)}
    mod_H = HMM(P_S0_14, P_S0_14,
                (ApOb.Both, (ar_m_n(2, 14), resp_n(14)), c2s),
                P_SS_69a, Scalar.Class_y)
    #    1  2  3  4  5  6  7  8  9  10
    P_SS_69b = Scalar.Prob((10, 10), dtype=np.float64, buffer=np.array([
        [1, 1, 0, 0, 0, 0, 0, 0, 0, 0], #1
        [1, 1, 1, 1, 1, 0, 1, 0, 0, 0], #2
        [0, 1, 1, 0, 0, 0, 0, 0, 0, 0], #3
        [0, 1, 0, 1, 0, 1, 0, 0, 0, 0], #4
        [0, 1, 0, 0, 1, 0, 0, 0, 0, 0], #5
        [0, 0, 0, 1, 0, 1, 0, 0, 0, 0], #6
        [0, 1, 0, 0, 0, 0, 1, 1, 1, 0], #7
        [0, 0, 0, 0, 0, 0, 1, 1, 0, 0], #8
        [0, 0, 0, 0, 0, 0, 1, 0, 1, 1], #9
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 1]  #10
    ]))
    P_SS_69b.normalize()
    P_S0_10 = np.ones(10)/10
    c2s = {0:np.arange(6, dtype=np.int32), 1:np.arange(6, 10, dtype=np.int32)}
    mod_M = HMM(P_S0_10, P_S0_10,
                (ApOb.Both, (ar_m_n(4, 10), resp_n(10)), c2s), 
                P_SS_69b, Scalar.Class_y)
    mod_L = HMM(P_S0_10, P_S0_10,
                (ApOb.Both, (ar_m_n(4, 10), resp_n(10)), c2s), 
                P_SS_69b, Scalar.Class_y)

    for mod in (mod_L, mod_M, mod_H):
        mod.y_mod.set_dtype([np.int32, np.float64, np.float64, np.float64])

    class ARGS:
        '''Class for args instances for reading data
        '''
        def __init__(self, record, expert=None, resp_dir=None, hr_dir=None):
            self.record = record
            self.expert = expert
            self.resp_dir = resp_dir
            self.hr_dir = hr_dir
            return
        def __str__(self):
            return('expert=%s, resp_dir=%s hr_dir=%s\nrecord=%s'%(
                self.expert, self.resp_dir, self.hr_dir, self.record))

    # Train each of the five base models with a single pass through
    # the appropriate data with a randomly chosen state sequence
    marked = a_records + b_records + c_records
    for model, args, name in (
            (mod_A2,
             ARGS(a_records, hr_dir=hr_dir, resp_dir=resp_dir),
             'init_A2'),
            (mod_C1,
             ARGS(c_records, hr_dir=hr_dir, resp_dir=resp_dir),
             'mod_C1'),
            (mod_H,
             ARGS(a_records, hr_dir=hr_dir, resp_dir=resp_dir,
                  expert=expert),
             'init_H'),
            (mod_M, ARGS(a_records + b_records + c_records, hr_dir=hr_dir,
                         resp_dir=resp_dir, expert=expert),
             'init_M'),
            (mod_L,
             ARGS(c_records, hr_dir=hr_dir, resp_dir=resp_dir, expert=expert),
             'init_L'),
        ):
        randomize(model, args)
        # Drop big arrays to save memory/disk
        model.alpha = None
        model.beta = None
        model.gamma = None
        model.P_Y = None
        pickle.dump(model, open(os.path.join(model_dir, name), 'wb'))
    return 0

if __name__ == "__main__":
    sys.exit(main())
    
#Local Variables:
#mode:python
#End:
