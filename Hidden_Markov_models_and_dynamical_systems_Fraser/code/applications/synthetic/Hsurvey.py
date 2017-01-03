"""
Hsurvey.py code to make the data for figure ToyH (Fig 5.3, the
relative entropy of a Kalman filter)

Invoke with: python Hsurvey.py data_dir

Creates files: HtauS and Hsurvey in the data_dir

Hsurvey has information for a response surface plot of -cross entropy
as a function of sample interval tau_s and log model measurement noise
sigma_epsilon

HtauS has information for a 2-d figure that has the following plots of
cross entropy vs tau_s:
 One that follows the response surface down its ridge
 One set at the known simulated measurement noise level
 A line calculated by Eqn. 5.2 in the book
"""
Copyright = '''
Copyright 2005 Andrew M. Fraser, and 2013 Andrew
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
from numpy.linalg import inv as LAI
from scipy.special import erf as ERF
import lorenz
import EKF

# Global parameters
s=10.0
r = 28.0
b = 8.0/3
ts = 0.15  # Will be changed as global.  lorstep in EKF needs global

def G_func(t, x):
    ''' Given state "x", return g(x) \equiv observation(x) and dg/dx
    '''
    x_0 = x[0]
    DG = np.zeros((1,3))
    DG[0,0] = 1
    return np.array((x_0,)),DG

def Filter(Y,            # Observation sequence
           mux,          # Initial state mean
           Ystep,        # Quantization of forecasts
           sigma_epsilon,# scalar variance of measurement noise
           sigma_eta,    # scalar variance of state noise
           X
):
    '''Run Kalman filter on sequence Y.  Calculate and return AILLY
    (Average Integrated Log Likelihood of Y)

    '''
    Nt = len(Y)
    # Initial state variance
    Sigmax = np.eye(3)*1e-3
    # Define functions for EKF
    state_noise = np.eye(3)*sigma_eta
    SigmaEta = lambda t,x: state_noise
    measurement_noise = np.eye(1)*sigma_epsilon
    SigmaEpsilon = lambda t,x: measurement_noise
    F = lambda t,x: lorenz.Ltan_one(x,s,b,r,ts)
    # Call the filtering function
    RD = {'mu_y':[], 'I_y':[]}
    EKF.ForwardEKF(Y, mux, Sigmax, SigmaEta, SigmaEpsilon, F, G_func, RD)
    # Generate error time series
    AILLY = 0.0 # Average Incremental Log Likelihood of Y
    for t in range(Nt):
        y_hat = RD['mu_y'][t]
        Y_t = Y[t][0]
        var_y = 1/RD['I_y'][t][0,0]
        #Integrate over bin size here
        top = Y_t + Ystep/2
        bottom = Y_t - Ystep/2
        if var_y < (Ystep**2)*1e-4: # small var_y.  Forecast either in or out
            if bottom < y_hat and y_hat < top:
                like0 = 1.0
            else:
                like0 = 0.0
        else:
            dev = np.sqrt(var_y*2.0) #2.0 for erf definition
            z0 = (bottom - y_hat)/dev
            z1 = (top - y_hat)/dev
            like0 = (ERF(z1) - ERF(z0))/2
        # Use gaussian mixture forecast.  The safety component has
        # dev=20 and weight a = 1e-3
        dev = 20.0
        z0 = (Y[t][0] - Ystep/2 - y_hat)/dev
        z1 = (Y[t][0] + Ystep/2 - y_hat)/dev
        like1 = (ERF(z1) - ERF(z0))/2
        a = 1.0e-3
        like = (1-a)*like0 + a*like1
        if like > 0:
            AILLY +=  np.log(like)
        else:
            print('''Error: t=%d like=%f, like0=%f, like1=%f,
y_hat=%f Y_t=%f var_y=%f
top=%f bottom=%f
dev=%f z0=%f z1=%f'''%(
    t, like, like0, like1, y_hat, Y_t, var_y, top, bottom, dev, z0, z1))
    AILLY/= Nt;
    return AILLY

def calculate(DevEta,     # sqrt(variance of measurement noise)
              DevEpsilon, # sqrt(variance of state noise)
              ts,         # Sample interval
              Nt,         # Number of samples
              Ystep       # Quantization size for y
):
    """ Integrate the Lorenz system create X and Y and quantize Y
    """
    (X, Y, F, G) = EKF.TanGen0(DevEta=DevEta, DevEpsilon=DevEpsilon,
                               s=s,r=r,b=b,ts=ts,Nt=Nt,
                               )
    Y = np.array(Y)
    Y = np.ceil(Y/Ystep - 0.5)*Ystep
    return (np.array(X),Y)

def survey(data_dir, Nt, Ystep, DevEpsilon, DevEta):
    '''Do a two dimensional survey over sampling interval ts and
    DevEpsilon, the scale of the observation noise, and write results
    to a file in data_dir.

    '''
    global ts
    SigmaEta = DevEta*DevEta
    Tsteps = np.arange(.02,.51,.02)
    dys = np.arange(-3.5,-5.6,-.1)
    f = open(data_dir+'/Hsurvey','w')
    for ts in Tsteps:
        DevEpsilon = 1e-10 # Square root of the variance of measurement noise
        SigmaEpsilon = DevEpsilon**2
        (X, Y) = calculate(DevEta, DevEpsilon, ts, Nt, Ystep)
        for dy in dys:
            DevEpsilon = 10**dy
            SigmaEpsilon = DevEpsilon**2
            AILLY = Filter(Y, X[0], Ystep, SigmaEpsilon, SigmaEta, X)
            print(dy, ts, AILLY[0], file=f)
    f.close()

def main(argv=None):
    import argparse
    global ts

    if argv is None:                    # Usual case
        argv = sys.argv[1:]
    assert len(argv) == 1
    data_dir = argv[0]

    Ystep = 1.0e-4    # Quantization of y.  Forecasts have probability
    Nt = 2000
    DevEpsilon = 1e-4 # Square root of the variance of measurement noise
    SigmaEpsilon = DevEpsilon**2
    DevEta = 1e-6 # Square root of the variance of state noise
    SigmaEta = DevEta**2

    # First write intercept and slope for Eqn 5.2 in the book
    z = 0.5/np.sqrt(2)
    like = (ERF(z) - ERF(-z))/2
    y0 = np.log(like) # The intercept for the "entropy line" is the log
                      # probability of the interval +/- .5 for a normal
                      # distribution.
    f = open (data_dir+'/HtauS','w')
    print("# y = %f -0.906 * t\n" % y0, file=f)


    survey(data_dir, Nt, Ystep, DevEpsilon, DevEta)

    # Next create data for two lines.  One at -4.0 and one on the ridge
    Tsteps = np.arange(.02,.51,.02)
    dy1 = -4.0
    dy2 = -4.85
    for ts in Tsteps:      # Survey sampling intervals ts
        DevEpsilon = 1e-10 # Square root of the variance of measurement noise
        SigmaEpsilon = DevEpsilon**2
        (X,Y) = calculate(DevEta, DevEpsilon, ts, Nt, Ystep) # Create the data
        DevEpsilon = 10**dy1
        SigmaEpsilon = DevEpsilon**2
        AILLY0 = Filter(Y, X[0], Ystep, SigmaEpsilon, SigmaEta, X)
        DevEpsilon = 10**(dy2 + 0.4*ts)
        SigmaEpsilon = DevEpsilon**2
        AILLY1 = Filter(Y, X[0], Ystep, SigmaEpsilon, SigmaEta, X)
        print('%4.2f %6.4f %6.4f'%(ts, AILLY0, AILLY1), file=f)
    f.close()

if __name__ == "__main__":
    sys.exit(main())

#Local Variables:
#mode:python
#End:
