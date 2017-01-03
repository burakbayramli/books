"""
Laser_data.py:  python3 Laser_data.py raw_data/LP5.DAT derived_data/laser

Script that makes data for the following laser figures:
    In Chapter 1:
        LaserLP5.pdf       Figure 1.1
        LaserLogLike.pdf   Figure 1.2
        LaserStates.pdf    Figure 1.3
        LaserForecast.pdf  Figure 1.4
    In Chapter 3:
        LaserHist.pdf      Figure 3.1

Requires:
    <data_dir>/LP5.DAT    Tang's real laser data
Produces: <derived_data/laser>/
    LaserLP5       Simulated laser data for figure LaserLP5
    LaserLogLike   Dependence of LL on s and b for figure LaserLogLike
    LaserStates    Sequence of 250 states from EKF for figure LaserStates
    LaserForecast  Sequence of 400 scalar values for figure LaserForecast
    LaserHist      Histogram 600 values for figure LaserHist in Chap 3
"""
Copyright = '''
Copyright 2005, 2008 Andrew M. Fraser, and 2013 Andrew M. Fraser and
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
import sys, os, math, numpy, numpy.linalg as LA, EKF, copy, lorenz
import pickle, scipy.optimize as O
from os.path import join

def ParamCalc(rad, Tr, r, s, b, ts, offset, scale, dev_epsilon, dev_eta):
    """ Translates specification of initial condition (rad,Tr) with
    radius rad from one of the unstable complex fixed points and
    relaxation time Tr to (ic[0],ic[1],ic[2]) thus creating a
    parameter vector suitable for EKF.LogLike().
    """
    root = math.sqrt(b * (r - 1)) # A frequently used intermediate term
    f    = numpy.array([root, root, (r - 1)]) # One of three fixed points
    DF   = numpy.array([[-s, s, 0],[1, -1, -root], [root, root, -b]])
    # DF is the derivative of F at the fixed point f
    [val, vec] = LA.eig(DF)
    v  = vec.T[1].real
    u  = vec.T[1].imag
    w  = (u - (u[2] / v[2]) * v)
    x0 = f - rad * (f[0] / w[0]) * w
    # x0 is in the plane of unstable complex pair of eigenvectors at
    # radius rad from f
    ic = lorenz.Lsteps(x0, lorenz.F, s, b, r, Tr, 2)[-1,:]
    # Now ic is result of trajectory starting at x0 after relaxation
    # time Tr.  The idea is avoid funny looking transients.
    return([ic[0], ic[1], ic[2], r, s, b, ts, offset, scale, dev_epsilon,
            dev_eta])

def StepNt(P, Nt):
    """ Simulate a sequence of Nt measurements by: (1) Calculate
    initial condidtions from radius and time.  (2) Integrate
    Lorenz. (3) Square first component, scale, and add offset.
    P = [rad, Tr, r, s, b, ts, offset, scale, noise]
    """
    params = ParamCalc(*(list(P[0:8])+[0,0])) # Stick 0, 0 on end of P
    ic = params[0:3]
    r,s,b,ts,offset,scale = params[3:9]
    X = lorenz.Lsteps(ic, lorenz.F, s,b,r,ts,Nt)
    ys = X[:, 0] * X[:, 0] * scale + offset
    return ys

def NegLogLike(P):
    """Use EKF.LogLike to calculate the log likelihood of the first Nt
    values in the Y sequence.
    """
    global Y, Nt,best
    rad,Tr = P[0:2]
    devEp,devEta = P[8:10]
    if rad * rad > 0.25 or Tr < 0.5 or devEp < 0.0 or devEta < 0.0:
        return 1e20 # If parameters are bad, return big number
    try:
        L = ParamCalc(*P[:10])
        cost = -EKF.LogLike(L, Y[0:Nt]) + (devEta/1e-2)**2
    except (ValueError, RuntimeError ) as err:
        #print 'NegLogLike returning 1e20 because ', err
        return 1e20
    if cost < best-1:
        best = cost
        print("best=%f"%best)
    return cost

def NLL_Fixed_Noise(P):
    """Use EKF.LogLike to calculate the log likelihood of the first Nnll
    values in the Y sequence.  Like NegLogLike() but noise is fixed.
    """
    global Y, best
    Nnll = 250
    rad  = P[0]
    Tr   = P[1]

    if rad ** 2 > 0.25 or Tr < 0.5:
        return 1e20
    try:
        L = ParamCalc(*(list(P[0:8])+[4.0/P[7], 1e-7]))
        # P[7] is scale.  The above line sticks dev_eps = 4.0 channels
        # and dev_eta = 1e-7 on to the end of the parameter list and
        # converts from (rad,Tr) to ic.
        LL = EKF.LogLike(L, Y[0:Nnll])
    except ValueError as err:
        #print 'NLL_Fixed_noise returning 1e20 because ', err
        return 1e20
    if -LL < best-1:
        best = -LL
        print("best=%f"%best)
    return(-LL)
def P_print(O_Par):
    print("""  ic=[%7.4f, %7.4f, %7.4f],
  r=%-7.4f, s=%-7.4f, b=%-7.4f,
  ts=%7.4f, offset=%-7.4f, scale=%-7.4f, sigma_epsilon=%-7.4f, sigma_eta=%-7.4g
"""%tuple(O_Par))

def Optimize(data_dir, Params):
    # Params = [rad,  Tr,  r,  s,  b,  ts, offset, scale, dev_Ep, dev_eta]
    # Params = [  0,   1,  2,  3,  4,   5,      6,     7,      8,       9]
    try:
        R = pickle.load(open(join(data_dir,'opt_Fixed_results'),'rb'))
    except:
        best = 1e20
        print('Before optimization of NLL_Fixed_Noise()')
        R=O.fmin_powell(NLL_Fixed_Noise,Params,xtol=1e-5,ftol=1e-5)
        pickle.dump(R,open(join(data_dir,'opt_Fixed_results'),'wb')) 
        print('After optimization NLL_Fixed_Noise()=%f'%NLL_Fixed_Noise(R))
    Params = list(R) + [4.0/R[7], 1e-7]
    P_print(ParamCalc(*Params[0:10]))
    try:
        P = pickle.load(open(join(data_dir,'opt_General_results'),'rb'))
    except:
        best = 1e20
        print('Before general optimization')
        P = O.fmin_powell(NegLogLike,Params)
        pickle.dump(P,open(join(data_dir,'opt_General_results'),'wb'))
        print('After general optimization\nNegLogLike(P)=',NegLogLike(P))
    O_Par = ParamCalc(*P[0:10])
    return O_Par
def LaserLogLike(data_dir, O_Par,
                 N_step, # Number of steps away from peak in each direction
                 Nt
             ):
    s = O_Par[4]
    b = O_Par[5]
    Frange = numpy.arange(-N_step,N_step+1,1.0)
    s_range = (Frange * 5e-3/N_step + 1.0)*s
    b_range = (Frange * 5e-3/N_step + 1.0)*b
    Par = copy.copy(O_Par)
    f = open(join(data_dir,'LaserLogLike'), 'w')
    for ss in s_range:
        for bb in (b_range):
            Par[4] = ss
            Par[5] = bb
            ll = EKF.LogLike(Par, Y[0:Nt])
            print(ss, bb, ll, file=f)
        print(' ', file=f)
    f.close()
def LaserStates(data_dir, Nt, O_Par):
    global Y
    aM = Nt*[None]
    ll = EKF.LogLike(O_Par,Y[0:Nt],aM=aM)
    f = open(join(data_dir,'LaserStates'), 'w')
    for x in aM:
        print(x.A[0,0],x.A[2,0], file=f)
    f.close()
    return aM
def forecast(data_dir, Nt, Nf, ic, s, b, r, ts, scale, offset):
    X = lorenz.Lsteps(ic, lorenz.F, s,b,r,ts,Nf+1)
    f = open(join(data_dir,'LaserForecast'),'w')
    for t in range(Nf):
        x0 = float(X[t+1,0])
        print(t+Nt,Y[Nt+t],(x0*x0*scale + offset), file=f)
    f.close()
def hist(data_dir):
    ''' Make "LaserHist" (histogram of the first 600 samples)
    '''
    global Y
    Count = numpy.zeros(256,numpy.int32)
    for y in Y[0:600]:
        Count[y] += 1
    f = open(data_dir + '/LaserHist', 'w')
    for t in range(0,100):
        print(t,Count[t], file=f)
    f.close()
best = 1e20
def LaserLP5(data_dir):
    '''Write LaserLP5 which Contains the first 250 points of laser data
    file and 250 simulated points.  To get parameters for the
    simulation, apply fmin to go to a local minimum starting from a
    point that I found by broadly and coarsely surveying parameter
    space.  I did the broad survey with an old script called
    survey4.py that is not part of this distribution.  The orbits go
    around each lobe 5 times before swithcing.  The optimization finds
    an unstable period 5 orbit that closely matches the laser data.
    Note that the simulated orbit is unstable, while the laser data
    certainly comes from a stable orbit.

    '''
    global best, Y
    def error250(P):
        """Calculate an error for the difference between the first 250
        points of actual laser data and the simulation that the parameters
        in P specify. I use this with O.fmin to improve the match between the
        simulation and the data for the first 250 points
        """
        global best
        rad = P[0]
        if rad*rad > 0.25:
            return 1e20
        try:
            sim = StepNt(P, 250)
        except ValueError as err:
            #print 'call StepNt failed at relax with ', err
            return 1e20
        e = 0.0
        for t in range(250):
            e += ((Y[t] - sim[t]) ** 2) / Y[t]
        if e < best-1:
            best = e
            print("best=%f"%best)
        return e

    # From Survey4.py
    #ic     = [  4.09750891,   4.70477676,  22.17708015  ]
    r      = 21.16
    s      = 1.792
    b      = 0.366
    ts     = 0.143
    offset = 15.95
    scale  = 7.5
    rad    = 0.32915
    Tr     = 1.645

    Params = [rad, Tr, r, s, b, ts, offset, scale]
    try:  # Speeds debugging by saving results of long calculations
        R = pickle.load(open(join(data_dir,'opt_250_results'),'rb'))
    except:
        best = 1e20
        print('Before optimization of error250()')
        R = O.fmin_powell(error250, Params, xtol = 1e-6, ftol = 1e-6)
        pickle.dump(R,open(join(data_dir,'opt_250_results'),'wb'))
        print('After optimization of error250(), error=%6.2f'%error250(R))
    Nt  = 250
    sim = StepNt(R,Nt)
    f = open(data_dir + '/LaserLP5','w')
    for t in range(Nt):
        print(t, Y[t], sim[t], file=f)
    f.close()
    return Params

import sys
def main(argv=None):
    '''Call with arguments: raw_data/LP5.DAT derived_data/laser
    To make:

    LaserLP5       Simulated laser data for figure LaserLP5
    LaserLogLike   Dependence of LL on s and b for figure LaserLogLike
    LaserStates    Sequence of 250 states from EKF for figure LaserStates
    LaserForecast  Sequence of 400 scalar values for figure LaserForecast
    LaserHist      Histogram 600 values for figure LaserHist in Chap 3

    '''

    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    assert len(argv) == 2
    in_name, result_dir = argv

    global Y, Nt
    Nt = 250
    # Read the input data.  The first and last line are not data, skip them
    Y = [int(x.split()[1]) for x in (open(in_name,'r').readlines())[1:-1]]
    # Next, calculate and write 'LaserLP5'.  Includes optimization that
    # takes about 8 minutes
    Params = LaserLP5(result_dir)
    hist(result_dir) # Write histogram of first 600 samples
    # 8 minutes (0:08) to get opt_250_results. 2:18 to get opt_Fixed_results
    O_Par = Optimize(result_dir, Params)
    print('After Optimize():')
    P_print(O_Par)
    aM = LaserStates(result_dir, Nt, O_Par)
    ic = numpy.array(aM[-1]).flatten()
    r, s, b, ts, offset, scale, dev_epsilon, dev_eta = O_Par[3:]
    forecast(result_dir, Nt, 400, ic, s, b, r, ts, scale, offset)
    # LaserLogLike takes 48 minutes
    LaserLogLike(result_dir, O_Par, 5, Nt)
if __name__ == "__main__":
    #sys.exit(main(argv=('../../../raw_data/LP5.DAT', 
    #                    '../../../derived_data/laser/')))
    sys.exit(main())

# Local Variables:
# mode: python
# End:
