''' ScalarGaussian.py data_dir

Writes file SGO_sim with the following columns:

    time simulated_state simulated_observation decoded_state

Writes file SGO_train containing:

    Two trained models

'''
Copyright = '''
Copyright 2005, 2007 Andrew M. Fraser, and 2013 Andrew
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
import sys
import numpy as np
from hmm import base, Scalar

P_SS = [
    [0.93, 0.07],
    [0.13, 0.87]]
P_S0 = [13./20, 7./20.]
mu = [-1.0, 1.0]
var = np.ones(2)
model_2a = base.HMM(P_S0, P_S0, (mu, var), P_SS, Scalar.Gauss)

P_SS = [
    [0.5, 0.5],
    [0.5, 0.5]]
P_S0 = [0.5, 0.5]
mu = [-2.0, 2.0]
var = np.ones(2)*2
model_2e = base.HMM(P_S0, P_S0, (mu, var), P_SS, Scalar.Gauss)


P_SS = [
    [0.5, 0.5],
    [0.5, 0.5]]
P_S0 = [0.5, 0.5]
mu = [0.0, 3.6]
var = [4.0**2, 0.126**2]
model_3a = base.HMM(P_S0, P_S0, (mu, var), P_SS, Scalar.Gauss)


def main(argv=None):

    from os.path import join

    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    data_dir = argv[0]
    T = 100
    s, y = model_2a.simulate(T)
    y = np.array(y, np.float64)
    s_hat = model_2a.decode(y)

    f = open(join(data_dir, 'SGO_sim'), 'w')
    for t in range(T):
        print('%2d %1d %7.3f %1d'%(t, s[t], y[0][t], s_hat[t]), file=f)
    model_2e.train(y, n_iter=50, display=False)
    f = open(join(data_dir, 'SGO_train'), 'w')
    print('model_2e after 50 training iterations=\n%s'%model_2e, file=f)
    model_3a.train(y, n_iter=6, display=False)
    print('\nmodel_3a after 6 training iterations=\n%s'%model_3a, file=f)
    return 0
if __name__ == "__main__":
    sys.exit(main())

# Local Variables:
# mode: python
# End:
