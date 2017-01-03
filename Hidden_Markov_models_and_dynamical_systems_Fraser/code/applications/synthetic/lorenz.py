'''lorenz.py

This file may be imported into other scripts to provide a python
interface to tools for integrating the lorenz system (eg,
scipy.integrate or gsl).  It may also be called as "main" to make data
files.  Here is the Lorenz system

.. math::
    \dot x = s(y-x)

    \dot y = rx -xz -y

    \dot z = xy -bz

Here are the fuctions that the original version of this module provided:
Lsteps, Ltan_steps, Ltan_one

>>> IC = np.array([0.1,0.3,2.0])
>>> x = Lsteps(IC,F,10.0,8.0/3,28.0,0.01,4)
>>> for y in x:
...     print('%5.2f %5.2f %5.2f'%tuple(y))
 0.10  0.30  2.00
 0.12  0.33  1.95
 0.14  0.36  1.90
 0.16  0.39  1.85

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
import sys, numpy as np

def F(x,t,s,b,r):
    ''' Lorenz vector field
    '''
    return np.array([
        s*(x[1]-x[0]),
        x[0]*(r - x[2])-x[1],
        x[0]*x[1]-b*x[2]
        ])
def F_tan(
        x_aug, # 12 dimensional augmented vector 
        t, s, b, r):
    ''' Lorenz vector field augmented with tangent space
    '''
    rv = np.empty(12)
    x = x_aug[:3]
    rv[:3] = F(x, t, s, b, r)

    DF = np.array([ # The derivative of F wrt x
        [-s,     s,    0],
        [r-x[2], -1,   -x[0]],
        [x[1],   x[0], -b]])
    D = x_aug[3:].reshape((3,3))
    rv[3:] = np.dot(DF,D).reshape(-1)
    return rv
def Lsteps(IC,      # IC[0:3] is the initial condition
           f,       # Function that evaluates derivitive
           s, b, r, # These are the Lorenz parameters
           T_step,  # The time between returned samples
           N_steps  # N_steps The number of returned samples
           ):
    '''Integrate the Lorenz ODE and return a time series

    Parameters
    ----------
    IC : array_like floats
        Initial conditions.  Returned as first point in result
    s : float
    b : float
    r : float
        s,b and r are the parameters of the Lorenz system
    T_step : float
        Sampling interval for returned time series
    N_steps : int
        Number of samples to return
    
    Returns
    -------
    V : array_like, 2-d
        The resulting time series.  V.shape (N_steps,3)
    '''
    from scipy.integrate import odeint
    t = np.arange(N_steps,dtype=float)*T_step
    return odeint(f, np.array(IC), t, args=(s,b,r))
def Ltan_steps(IC,       # IC[0:12] is the initial condition
               s, b, r,  # These are the Lorenz parameters
               T_step,   # The time between returned samples
               N_steps
               ):
    """ Return RV[0:N_steps+1,0:4,0:3) where RV[t,0,0:3] is the initial x
    vector and RV[t,1:4,0:3] is the derivative after integrating t time
    steps.
    """
    from scipy.integrate import odeint
    t = np.arange(N_steps,dtype=float)*T_step
    return odeint(F_tan, np.array(IC), t, args=(s,b,r))
def Ltan_one(IC,       # IC[0:3] is the initial condition
             s, b, r,  # These are the Lorenz parameters
             T_step    # The time step
             ):
    """ Integrate initial condition IC and the tangent (initialize here as
    identity) one step.  Return a pair consisting of the final
    location x(t) and the derivative D.
    """
    IC_4_3 = np.zeros((4,3),np.float64)
    IC_4_3[0,0:3] = IC
    IC_4_3[1,0] = 1.0
    IC_4_3[2,1] = 1.0
    IC_4_3[3,2] = 1.0
    RV2 = Ltan_steps(IC_4_3.reshape(-1),s,b,r,T_step,2)
    RV = RV2[1]
    X = RV[0:3]
    D = RV[3:].reshape((3,3))
    return X,D
def main(argv=None):
    '''Writes time series to files specified by options --xyzfile and or
    --quantfile.  If neither is specified, simpy runs doctest.

    Bullet points
    -------------

    Options:

    * --L

    * --IC

    * --s

    * --r

    * --b

    * --dt

    * --levels

    * --quantfile

    * --xyzfile

    '''
    import argparse

    if argv is None:                    # Usual case
        argv = sys.argv[1:]

    parser = argparse.ArgumentParser(
        description='Make files derived from Lorenz simulations')
    parser.add_argument('--L', type=int, default=100,
                       help='Number of samples')
    parser.add_argument('--IC', type=float, nargs=3,
                        default=[11.580, 13.548, 28.677],
                        help='Initial conditions')
    parser.add_argument('--s', type=float, default=10.0,
                       help='Lorenz s parameter')
    parser.add_argument('--r', type=float, default=28.0,
                       help='Lorenz r parameter')
    parser.add_argument('--b', type=float, default=8.0/3,
                       help='Lorenz b parameter')
    parser.add_argument('--dt', type=float, default=0.15,
                       help='Sample interval')
    #FixMe: --levels not used
    parser.add_argument('--levels', type=int, default=4,
                        help='Number of quatization levels')
    parser.add_argument('--quantfile', type=argparse.FileType('w'),
                       help='Write quantized data to this file')
    parser.add_argument('--xyzfile', type=argparse.FileType('w'),
                       help='Write x,y,z data to this file')
    parser.add_argument('--TSintro', help='Write data to this directory')
    parser.add_argument('--time', action='store_true',
                       help='Do some timing tests')
    args = parser.parse_args(argv)
    args_d = args.__dict__
    if args.time:
        import time
        from lor_C import Lsteps as Csteps
        from lor_C import f_lor
        t0 = time.time()
        N = 2000
        xyz = Lsteps(np.array(args.IC),F,args.s,args.b,args.r,args.dt,N)
        print(xyz[10])
        t1 = time.time()
        xyz = Lsteps(np.array(args.IC),f_lor,args.s,args.b,args.r,args.dt,N)
        print(xyz[10])
        t2 = time.time()
        xyz = Csteps(np.array(args.IC),args.s,args.b,args.r,args.dt,N)
        print(xyz[10])
        t3 = time.time()
        print('''%6.4f seconds for odeint with python F
%6.4f seconds for odeint with cython F
%6.4f seconds for cython rk4'''%(
                t1-t0, t2-t1, t3-t2))
    elif args.xyzfile==None and args.quantfile==None:
        import doctest
        doctest.testmod()
    else:
        xyz = Lsteps(np.array(args.IC),F,args.s,args.b,args.r,args.dt,args.L)
        for v in xyz:
            print((3*'%6.3f ')%tuple(v),file=args.xyzfile) 
            print('%d'%int(np.ceil(v[0]/10+2)),file=args.quantfile) 
    if args.TSintro is not None:
        from os.path import join
        xyz = Lsteps(args.IC, F, args.s, args.b, args.r, args.dt/50, args.L)
        # Write x[0] to TSintro_fine with time step .003
        f = open(join(args.TSintro,'TSintro_fine'),'w')
        for i in range(0,args.L):
            print((2*'%6.3f ')%(args.dt/50 * i, xyz[i,0]),file=f)
        # Write x[0] to TSintro_qt with time step .15
        f = open(join(args.TSintro,'TSintro_qt'),'w')
        for i in range(0,args.L,50):
            print((2*'%6.3f ')%(args.dt/50 * i, xyz[i,0]),file=f)
        # Write quantized x[0] to TSintro_qtx with time step .15
        q = np.ceil(xyz[:,0] / 10 + 2)
        f = open(join(args.TSintro,'TSintro_qtx'),'w')
        for i in range(0,args.L,50):
                  print('%2d %6.3f '%(i/50, q[i]),file=f)
    return 0

if __name__ == "__main__":
    sys.exit(main())

#---------------
# Local Variables:
# eval: (python-mode)
# End:
