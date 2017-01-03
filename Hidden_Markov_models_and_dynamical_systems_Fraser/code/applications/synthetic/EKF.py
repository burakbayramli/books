"""Caution, I took this file from webhmmds and it used numpy matrices
which I have been replacing with numpy arrays.  I may have failed to to
that completely.  I should write a test for each function in this file
but I have not spent the time yet.

EKF.py Module containing functions that help with extended Kalman
filtering for the Lorenz system.

Contents:
def ForwardEKF()
def BackwardEKF()
def TanGen()
def ForwardK()
def BackwardK()
def LogLike()
"""
Copyright = ''' Copyright 2005, 2008 Andrew M. Fraser, and 2013 Andrew
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
def symmetric(M):
    '''For debugging, check some easy properties of a covariance matrix:
    Symmetry and nonnegative on diagonal.  Allow small asymmetry but
    return truly symmetric (M + M.T)/2.

    '''
    I,J = M.shape
    assert I==J, 'M=\n%s'%M
    for i in range(I):
        assert M[i,i]>=0, 'M=\n%s'%M
        for j in range(J):
            assert (np.abs(M[i,j]) < 1e-30 or np.abs((M[i,j] - M[j,i])/M[i,j]) 
                    < 1e-7), 'Assymentric at (%d,%d) M= \n%s'%(i,j,M)
    return (M+M.T)/2
from numpy.linalg import inv as LAI
import numpy as np

def ForwardEKF(
    Y,         # Observations.  Shape = (Nt,dim_y)
    mu_a,      # Mean of initial state (forecast)
    Sig_a,     # Covariance of initial state (forecast)
    Sig_Eta,   # Sig_Eta(t, x) is Covariance of dynamical noise
    Sig_Ep,    # Sig_Ep(t, x) is Covariance of measurement noise
    F,         # (x[t+1], dx[t+1]/dx[t]) = F(t, x[t])
    G,         # (y, dy/dx) = G(t, x)
    rd={}      # Return dictionary.  Key=variable, value=list
    ):
    """ This started as a general extended Kalman filter function, but
    lapsed into a function that is specific for the laser data.
    """
    
    (Nt,dim_y) = Y.shape
    (dim_x,) = mu_a.shape
    assert (dim_x,dim_x) == Sig_a.shape
    Id = np.eye(dim_x)  # Identity matrix
    for t in range(Nt):
        mu_y, Gt = G(t,mu_a)     # Forcast y and derivative wrt mu_a
        error_y = Y[t] - mu_y
        # Update: Calculate \Sigma_\alpha(t) and \mu_\alpha(t) Y[t]
        I_y = LAI( np.dot(Gt, np.dot(Sig_a, Gt.T)) + Sig_Ep(t,mu_a))
        # I_y is the inverse covariance of the y forecast
        K = np.dot( np.dot(Sig_a, Gt.T), I_y) # Kalman gain matrix 4.44c
        I_gain = np.dot(np.dot(K,Gt), Sig_a)  # Info gain (jargon error) from y
        Sig_alpha = Sig_a - I_gain            # Updated state variance 4.44a
        mu_alpha = mu_a + np.dot(K, error_y)  # Updated state mean
        # Forecast: Calcualte mean mu_a(t+1) and variance Sigma_a(t+1)
        mu_a, Ft = F(t,mu_alpha) # Calculate state forecast and derivative
        Sig_a = np.dot(Ft, np.dot(Sig_alpha, Ft.T)) + Sig_Eta(t,mu_a) # 4.38
        Sig_a = (Sig_a + Sig_a.T)/2 # Symmetrize, perhaps unnecessary
        # Record requested values
        D = locals()
        for key in rd:
            rd[key].append(D[key])
    if len(rd) == 0:
        return (mu_alpha, Sig_alpha)
    return rd # End of ForwardEKF() #
def BackwardEKF(
    Y,         # Observations
    SigmaEtaT, # Covariances of dynamical noise (time dependent)
    SEI,       # Inverse covariance of measurement noise
    alphaM,    # Corrections to ICs for integrations in forward
    aM,        # Results of integrations in forward
    F,         # F[t] is the derivative of the forward state map
    betaM,     # Mean of back forcast
    betaSI     # Inverse covariance of back forecast
    ):
    # Abbreviate common functions
    mubeta = np.zeros(3,np.float32)       # Corrections to alpha
    Sigmabeta = np.eye(3,np.float32)*1e10 # Should be 1/zero
    Gt = np.zeros((1,3),np.float32)       # Storage for tangent to G
    for t in range(len(Y)-1,-1,-1):
        # Save the current values of the beta parameters
        SigmabetaI =  LAI(Sigmabeta)
        betaSI[t] = SigmabetaI
        betaM[t] = mubeta
        # Unpack remembered values
        FI = LAI(F[t])
        SigmaEta = SigmaEtaT[t]
        # x0 is the back forecast estimate for time t
        x0 = mubeta[0]+alphaM[t][0]+aM[t][0]
        dy = Y[t] - x0*x0
        x0 = aM[t][0]+alphaM[t][0]
        Gt[0,0] = 2*x0
        # Do an update: Calculate \Sigma_b(t) and \mu_b(t) using dy
        SIb = SigmabetaI + np.dot(Gt.T,np.dot(SEI,Gt))
        Sigmab = LAI(SIb)
        mub = alphaM[t] + mubeta + np.dot(Sigmab,np.dot(Gt.T,np.dot(SEI,dy)))
        # mub is the updated deviation of mean from aM[t]
        if t < 1:
            return [mub,Sigmab]
        # Back forecast: Calcualte \mu_beta(t-1) and \Sigma_beta(t-1)
        mubeta = np.dot(FI,mub)
        Sigmabeta = np.dot(FI,np.dot(SigmaEta + Sigmab,FI.T))
################ End of BackwardEKF #####################

"""
TanGen() Creates 4 time series: X, a sample path of the noisy Lorenz
system, Y, a sequence of observations, F a sequence of tangents to the
state dynamics, G a sequence of tangents to the observation functions
"""
def TanGen(
    DevEta = 0.001,             # Dynamical noise
    DevEpsilon = 0.004,         # Measurement noise
    s=10.0, r = 28.0, b = 8.0/3,# Lorenz parameters
    ts = 0.15,                  # Sample interval
    Nt = 300,                   # Number of samples
    trelax = 7.5                # Time to relax to attractor
    ):
    import lorenz, pickle, random # FixMe: use numpy.random
    RG = random.gauss
    # Storage for initial conditions
    ic = np.ones(3,np.float32)
    # Relax to the attractor:
    Nrelax = 3
    temp = np.ones((Nrelax,3),np.float32)    # Storage for results
    NumpyLor.LstepsPN(ic,s,b,r,trelax,Nrelax,temp)
    ic = temp[Nrelax-1]                                # reset ICs

    X = []
    Y = []
    F = []
    G = []
    tempTan = np.ones((3,3),np.float32)  # Storage for one step integration
    for t in range(Nt):
        X.append(ic.copy())
        Y.append(np.array([ic[0]**2+RG(0.0,DevEpsilon)]))
        G.append(np.array([[2*ic[0],0,0]],))
        # Call to integrator.  ic is overwritten with result
        NumpyLor.LtanstepPN(ic,s,b,r,ts,ic,tempTan)
        # Add random Gaussian noise to ic
        ic = ic + np.array([RG(0.0,DevEta),RG(0.0,DevEta),RG(0.0,DevEta)])
        ic = ic.astype(np.float32)
        F.append(tempTan.copy())
    results = [X, Y, F, G]
    f = open('XYGF','w')
    pickle.dump(results,f,protocol=1)
    f.close()
    return (X, Y, F, G)
"""
ForwardK A forward Kalman filter that requires state and measurement
derivatives and a reference state trajectory.
"""
def ForwardK(
    X,             # The actual state vectors
    Y,             # The ovservations
    F,             # The derivative of the state map
    G,             # The derivative of observation wrt to state
    SigmaEta_list, # List of state noises
    SEI_list,      # List of inverse measurement noises
    alphaSI,       # Pass empty list.  Return inverse covaraince of
                   # up-dated state estimates
    alphaM         # Pass empty list.  Return Means of up-dated
                   # state estimates
    ):
    
    #  The next four values should be parameters?
    Sigma_a = np.identity(3,np.float32)*5
    mux = np.ones(3,np.float32)*1
    SigmaEta = np.eye(3,np.float32)*1
    SEI = np.ones((1,1),np.float32)*1 # Sigma_epsilon^{-1}
    for t in range(len(X)):
        # Calculate error of forecast observation.
        ythat = X[t][0]*X[t][0] + np.dot(G[t],mux)
        dy = Y[t] - ythat
        # Do an update: Calculate \Sigma_\alpha(t) and \mu_\alpha(t)
        # using forecast error (eq:KUpdate in book)
        SIalpha = LAI(Sigma_a) + np.dot(G[t].T,np.dot(SEI,G[t]))
        Sigmaalpha = LAI(SIalpha)
        mualpha = mux + np.dot(Sigmaalpha,np.dot(G[t].T,np.dot(SEI,dy)))
        
        alphaSI.append(SIalpha)
        alphaM.append(mualpha)
        # Forecast: Calcualte \mu_x(t+1) and \Sigma_x(t+1), eq:Kfore
        # in book.  Note mualpha is mean deviation from X
        mux =  np.dot(F[t],mualpha)
        Sigma_a = np.dot(F[t],np.dot(Sigmaalpha,F[t].T)) + SigmaEta
"""
BackwardK A backward Kalman filter that requires state and measurement
derivatives and a reference state trajectory.  The filter tracks
deviations from the reference trajecory.
"""
def BackwardK(
    X,             # The actual state vectors
    Y,             # The ovservations
    F,             # The derivative of the state map
    G,             # The derivative of observation wrt to state
    SigmaEta_list, # List of state noises
    SEI,           # Inverse measurement noise
    betaSI,        # Pass empty list.  Return inverse covaraince of
                   # filtered state estimates
    betaM          # Pass empty list.  Return Means of filtered
                   # state estimates
    ):

    # Initialize beta, the backcast for T-1
    mubeta =  np.zeros(3,np.float32)
    Sigmabeta = np.eye(3,np.float32)*1e10 # Should be 1/zero
    
    for t in range(len(X)-1,-1,-1): # Start at T-1 and step by 1 to 0
        SigmaEta = SigmaEta_list[t]  # Read state noise
        SigmabetaI =  LAI(Sigmabeta) # Invert state variance
        # Save the current values of the beta (backcast) parameters
        betaSI[t] = SigmabetaI
        betaM[t] = mubeta

        # Calculate error of backcast observation
        ythat = X[t][0]*X[t][0] + np.dot(G[t],mubeta)
        dy = Y[t] - ythat
        # Do an update: Calculate \Sigma_b(t) and \mu_b(t) using y(t)
        # eq:BUpdate in book
        SIb = SigmabetaI + np.dot(G[t].T,np.dot(SEI,G[t]))
        Sigmab = LAI(SIb)            # Invert state variance
        mub = mubeta + np.dot(Sigmab,np.dot(G[t].T,np.dot(SEI,dy)))
        
        # Backcast: Calculate \mu_beta(t-1) and \Sigma_beta(t-1).
        # Note that mub is the mean of the updated distribution for
        # the deviation from X[t]
        if t>0:
            # Invert F[t-1], the derivative of X from t-1 to t
            FI = LAI(F[t-1])
            # eq:BFore(a&b) in book
            mubeta =  np.dot(FI,mub)
            Sigmabeta = np.dot(FI,np.dot((SigmaEta+Sigmab),FI.T))

###################### Def forwardEKF0() ####################
# Like forwardEKF(), but observations are X_0 not X_0**2
def ForwardEKF0(
    Y,         # Observations
    SigmaEtaT, # Covariances of dynamical noise (time dependent)
    SEI,       # Inverse covariance of measurement noise
    mux,       # Mean of initial state.  Later, mean of forecast
    Sigmax,    # Covariance of initial state.  Later cov of forecast
    F,         # Integrator: F(ic,fc,D) takes initial condition ic
               # and produces final condition fc and derivative D
    alphaM,    # Corrections to means of updtated state distributions
    alphaSI,   # Inverse covariances of updtated state distributions
    aM,        # Means of forecast state distributions
    aSigma,    # Covariances of forecast state distributions
    DF,        # Derivatives of state map
    ):
    
    Ft = np.zeros((3,3),np.float32)  # Storage for tangent to F
    Gt = np.zeros((1,3),np.float32)  # Storage for tangent to G
    for t in range(len(Y)):
        SigmaEta = SigmaEtaT[t]
        #Gt[0][0] = 2*mux[0]      # Estimated derivative of Y wrt X
        #dy = Y[t] - mux[0]**2    # Error of forecast Y
        Gt[0][0] = 1.0            # Estimated derivative of Y wrt X
        dy = Y[t] - mux[0]        # Error of forecast Y
        aM[t] = mux.copy()        # backward needs the forecast for time t
        aSigma[t] = Sigmax.copy()
        
        # Update: Calculate \Sigma_\alpha(t) and \mu_\alpha(t) using y(t)
        SIalpha = LAI(Sigmax) + np.dot(Gt.T,np.dot(SEI,Gt))
        Sigmaalpha = LAI(SIalpha)
        mualpha = np.dot(Sigmaalpha,np.dot(Gt.T,np.dot(SEI,dy)))
        alphaM[t] = mualpha         # Save only correction
        ic = mux + mualpha
        # Check for big ic
        R = np.sqrt(np.dot(ic,ic))
        if R > 100:
            print("t=",t,"R=",R,"Shrinking ic")
            ic = ic/R
        # Forecast: Calcualte \mu_x(t+1) and \Sigma_x(t+1)
        F(ic,mux,Ft)# Calculate mux and Ft
        Sigmax = np.dot(Ft,np.dot(Sigmaalpha,Ft.T)) + SigmaEta

        # Remember values you need for backward and smoothing
        alphaSI[t] = SIalpha
        DF[t] = Ft.copy()
################# End of ForwardEKF0() ####################
def TanGen0(
    DevEta = 0.001,             # Dynamical noise
    DevEpsilon = 0.004,         # Measurement noise
    s=10.0, r = 28.0, b = 8.0/3,# Lorenz parameters
    ts = 0.15,                  # Sample interval
    Nt = 300,                   # Number of samples
    trelax = 1.0                # Time to relax to attractor
    ):
    """
    Simulate the evolution of the whole system for Nt time steps.
    Record and return the sequence of states (X), observations (Y),
    derivatives of state maps (F) and derivatives of observation maps
    (G).  TanGen0() is like TanGen(), but observation is X_0 instead
    of X_0**2
    """

    import lorenz, numpy as np, random
    assert trelax < 2.0,"Integrator doesn't work with big times"
    RG = random.gauss
    # Storage for initial conditions
    ic = np.ones(3)
    # Relax to the attractor:
    Nrelax = 30
    temp = lorenz.Lsteps(ic, lorenz.F, s,b,r,trelax,Nrelax)
    ic = temp[-1]  # reset ICs
    X = []
    Y = []
    F = []
    G = []
    for t in range(Nt):
        X.append(ic)
        Y.append(np.array([ic[0]+RG(0.0,DevEpsilon)]))
        G.append(np.array([[1.0,0,0]]))
        # Call to integrator.  ic is overwritten with result
        ic,tan = lorenz.Ltan_one(ic,s,b,r,ts)
        ic = ic + np.array([RG(0.0,DevEta),RG(0.0,DevEta),RG(0.0,DevEta)])
        F.append(tan)
    return (X, Y, F, G)
#End of TanGen0()

###################### Def LogLike() ####################
"""
LogLike(L,Y) takes parameters in list L and observations in list Y
and returns the negative log likelihood

"""

def LogLike(L,        # Lorenz/Laser parameters
            Y,        # Time series of observations
            aM=None   # Forecast means
            ): # L is the argument list.  Unpack it first
    ic = L[0:3]
    r = L[3]
    s = L[4]
    b = L[5]
    ts = L[6]
    offset = L[7]
    scale = L[8]
    sigma_epsilon = L[9]  # measurment noise
    sigma_eta = L[10]     # Dynamic noise
    
    def lorstep(x):
        # Return mu_x(t+1) and derivative F(t)
        # Check for big ic
        R = np.sqrt(float(np.dot(x,x)))
        if R > 100:
            print("R=%6.0f Shrinking ic"%R)
            x /= R
        return lorenz.Ltan_one(x,s,b,r,ts)
    def Tang_G(x):
        ''' Measurement function and its derivative for Tang's laser data
        '''
        x_0 = x[0]
        DG = np.zeros(1,3)
        DG[0,0] = 2*x_0
        return np.array((x_0*x_0,)),DG
    Nt = len(Y)
    yso = np.empty((Nt,1))  # Scaled and offset
    for t in range(Nt):
        yso[t] = float(Y[t] - offset)/scale
    SigmaEta = np.mat(np.eye(3))*(sigma_eta**2)
    SigmaEtaT = Nt*[SigmaEta]  # Less memory than np.empty((Nt,3,3))
    SigmaEpsilon = np.mat(np.eye(1))*sigma_epsilon**2
    Sigma_x = np.mat(np.eye(3))*1e-2 # Parameter?
    # Allocate lists of results so that they can be indexed
    alphaSig = Nt*[None]
    alphaM = Nt*[None]
    if aM == None:
        aM = Nt*[None]
    aSigma = Nt*[None]
    ic = np.mat(np.array(ic).reshape((3,1)))
    ForwardEKF(yso, SigmaEtaT,SigmaEpsilon, ic, Sigma_x, lorstep, Tang_G,
               DF=None,alphaM=None, alphaSig=None ,aM=aM, aSigma=aSigma)

    # Calculate the log likelihood
    LL = 0.0
    for t in range(Nt):
        xt = float(aM[t][0])
        Gt = 2*xt
        sigma_gamma = float(Gt*aSigma[t][0,0]*Gt + SigmaEpsilon[0,0])
        ye = float(yso[t][0] - xt**2)       # Forecast Y error
        sigma_gamma *= scale**2
        ye *= scale
        try:
            ill = -0.5*(np.log(2*np.pi*sigma_gamma) + ye*ye/sigma_gamma )
        except (ValueError, OverflowError) as error_string:
            print('inc log like (ill) is sick: %s\nt=%d '%(error_string,t))
            print('ye=%f, sigma_gamma=%f'%(ye,sigma_gamma))
            ill = -0.5*(np.log(2*np.pi*sigma_gamma))
        LL += ill
    return (LL)

###################### End of LogLike() ####################

#Local Variables:
#mode:python
#End:
