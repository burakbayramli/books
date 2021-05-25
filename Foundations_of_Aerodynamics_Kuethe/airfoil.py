# Python 3
# Translation of FORTRAN program p. 133 from
# https://github.com/tristanCB/airfoil
# This program translates the FORTRAN program on p. 133 of 
# Foundations of Aerodynamics: Bases of Aerodynamic Design — Fourth Edition" from Kuethea. M. and Chowc-Y (1968)
# Author: Tristan CB
# Dependencies
import numpy as np
import math
# Summary of program
"""
This prgram computes velocity and pressure coefficients
around an airfoil, whose contor is approximated by vortex panels
of linearly varying strength.
Assumptions: 2D incompressible flow
"""
#
# Ideas
# Encoding equations into CNN would it be more efficient?
# Using it as a loss function in a ML model would be neat
#
# Will have to find ways of using CRAMER and DETERM subroutines ...

def determ(ARRAY, N, checkPrecision = False):
    """
    DETERM is the value of the determinant of an N*N matrix called ARRAY,
    computed by the technique of pivotal condensation. This function is 
    taken from p. 113 - 114 of Chow (1979)

    Notes on comparing it's precision and speed with numpy.
    """
    A = np.zeros((N,N))
    for i in range(N):
        for j in range(N):
            A[i,j] = ARRAY[i,j]
    M = 0
    while True:
        K = M + 1
        for i in range(K,N):
            RATIO = A[i,M]/A[M,M]
            for ij in range(K,N):
                A[i,ij] = A[i,ij] - A[M,ij]*RATIO
        if M == N-1:
            # Go to 4
            DETERM = 1
            for L in range(N):
                # print(f"A[L,L]: {A[L,L]}")
                DETERM = DETERM * A[L,L]
                # print(f"DETERM is now {DETERM}")
            # print(f"Returning {DETERM}")

            # Checks 
            if checkPrecision == True:
                deciNpDetC              = np.linalg.det(ARRAY)
                deciPivCondensationChow = DETERM
                diff = 1 - deciPivCondensationChow/deciNpDetC
                print(
                        f"""
                        Numpy func (np.linalg.det(ARRAY))   ==> {deciNpDetC}
                        Det. by pivitoal condensation       ==> {deciPivCondensationChow}
                        Diff                                  > {diff}
                        """)
                assert abs(diff) < 0.0001
                
            return DETERM
        else:
            M = M + 1
    pass

def cramer(C, A, X, N):
    """
    This is a funtion to emulate the subroutine cramer as used in the FORTRAN77 program
    Airfoil.
    This solves a set of algebraic equations
        C[i,j]*X[j] = A[i], i=1,2,3,...,N
    it was teken from p. 114 of Chow (1979)
    Note: Dimensions in the following statement must be equal to the numerical value of N.
    """
    CC = np.zeros((N,N))
    DENOM = determ(C, N)
    # np.set_printoptions(formatter={'float': '{: 0.6f}'.format})


    for k in range(N):
        for i in range(N):
            for j in range(N):
                CC[i,j] = C[i,j]
        for i in range(N):
            CC[i,k] = A[i]

        NUNOM = determ(CC, N)
        X[k] = NUNOM / DENOM
        # print(f"DENOM --> {DENOM}")
        # print(f"NUNOM --> {NUNOM}")
        # print(f"GAMMA will now be? --> {X[k]}")
    return X

def panelMethod(XB: np.array([]),YB: np.array([]), angle=8.0) -> dict:
    """
    # Specify coordinates (XB, YB) of boundary points on airfoil surface. 
    # The last point coinsides with the first
    XB = np.array([1.,.933,.750,.500,.250,.067,.0,.067,.25,.500,.750,.933, 1.0])
    YB = np.array([.0,-.005,-.017,-.033,-.042,-.033,.0,.045,.076,.072,.044,.013,0.])
    """
    # Get problem size depending on input data
    numberOfPanels = XB.shape[0]-1
    M = numberOfPanels
    MP1 = M + 1

    # Coordinates of control and boundary points
    # XB, YB  = (np.zeros((M)) for i in range(2))
    X, Y    = (np.zeros((M)) for i in range(2))
    # Represents γ' in text
    GAMMA   = np.zeros((M))

    S, SINE, COSINE, THETA, V, CP, RHS  = (np.zeros((M)) for i in range(7))
    CN1, CN2, CT1, CT2 =  (np.zeros((M,M)) for i in range(4))
    AN = np.zeros((MP1,MP1))

    AT = np.zeros((M,MP1))

    PI = 4.0 * np.arctan(1.0)
    ALPHA = angle * PI/180

    # Watch out the negative 1 was added befcause of zero based indexing??
    for i in range(M):
        IP1         = i + 1
        X[i]        = 0.5*(XB[i] + XB[IP1])
        Y[i]        = 0.5*(YB[i] + YB[IP1])
        S[i]        = np.sqrt( (XB[IP1] - XB[i])**2 + (YB[IP1] - YB[i])**2 )
        THETA[i]    = np.arctan2( (YB[IP1] - YB[i]), (XB[IP1] - XB[i]))
        SINE[i]     = np.sin(THETA[i])
        COSINE[i]   = np.cos(THETA[i])
        RHS[i]      = np.sin(THETA[i] - ALPHA)

    for i in range(M):
        for j in range(M):
            if i == j:
                # print(f"I is {i}, J is {j}") # UID for debugging
                CN1[i,j] = -1.0
                CN2[i,j] = 1.0
                CT1[i,j] = 0.5*PI
                CT2[i,j] = 0.5*PI
                # continue # break # still unsure about this
            else:
                A = (-(X[i] - XB[j]))*COSINE[j] - (Y[i]-YB[j])*SINE[j]
                B = (X[i]-XB[j])**2 + (Y[i]-YB[j])**2
                # B = ((X[i]-XB[j])**2) + ((Y[i] + YB[j])**2) # Watch out
                C = np.sin(THETA[i] - THETA[j])
                D = np.cos(THETA[i] - THETA[j])
                E = (X[i] - XB[j]) * SINE[j] - (Y[i] - YB[j])*COSINE[j]
                # print(f"ID: {i+1} {j+1}")
                F = np.log(1.0 + S[j]*(S[j] + 2.0 * A)/B)
                if np.isnan(F):
                    F = np.nan_to_num(F)
                G = np.arctan2(E*S[j], B+A*S[j])
                P = ( (X[i] - XB[j]) * np.sin(THETA[i] - 2*THETA[j]) 
                    + (Y[i] - YB[j]) * np.cos(THETA[i] - 2*THETA[j]) ) 
                Q = ( (X[i] - XB[j]) * np.cos(THETA[i] - 2*THETA[j]) 
                    - (Y[i] - YB[j]) * np.sin(THETA[i] - 2*THETA[j]) )
                CN2[i,j] = D + (0.5*Q*F/S[j]) - ((A*C + D*E)*G/S[j])
                CN1[i,j] = 0.5*D*F + C*G - CN2[i,j]
                CT2[i,j] = C + 0.5*P*F/S[j] + ((A*D-C*E)*G)/S[j]
                CT1[i,j] = ((0.5*C*F) - (D*G) - (CT2[i,j]))
                # For debugging
                # print(f"A, B, C, D, E, F, G, P, Q =====> {A}, {B}, {C}, {D}, {E}, {F}, {G}, {P}, {Q}")
                # print("CN2[i,j] ------>", CN2[i,j])
                # print("CN1[i,j] ------>", CN1[i,j])
                # print("CT2[i,j] ------>", CT2[i,j])
                # print("CT1[i,j] ------>", CT1[i,j])
    # print(A, B, C, D, E, F, G, P, Q)

    ##################
    # Coords (X,Y) of control point and panel length s are computed
    # for each of the vortex panels. RHS represents the right-hand side of eq. 47 p. 130.

    # Compute influence coeffs. in eqs. 47 and 49. respect.
    for i in range(M):
        AN[i,0] = CN1[i,0]
        AN[i,M] = CN2[i,M-1]
        AT[i,0] = CT1[i,0]
        AT[i,M] = CT2[i,M-1]
        for j in range(1,M):
            AN[i,j] = CN1[i,j] + CN2[i,j-1]
            AT[i,j] = CT1[i,j] + CT2[i,j-1]
    AN[M,0] = 1
    AN[M,M] = 1
    for j in range(1,M):
        AN[M,j] = 0
    RHS = np.pad(RHS, (0, 1), 'constant')

    # Solve eq. 47 for dimensionless strengths gamma using cramer's rule. 
    # Then compute and print dimensionless velocity and pressure coeefffs 
    # at control points.
    GAMMA = np.pad(GAMMA, (0, 1), 'constant')
    cramer(AN, RHS, GAMMA, MP1)
    for i in range(M):
        V[i] = np.cos(THETA[i] - ALPHA)
        for j in range(MP1):
            V[i] = V[i] + AT[i,j] * GAMMA[j]
            CP[i] = 1 - V[i]**2


    results = {}
    I = [i for i in range(M)]
    variables           = ["I","X", "Y","THETA", "S","GAMMA", "V", "CP"]
    corespondingArr  = [I, X, Y, THETA, S, GAMMA, V, CP]
    for i, ij in zip(variables,corespondingArr):
        results[i] = ij
    #     print(i)
    #     print(ij)
    return results

if __name__ == "__main__":
    # Uses matplotlib to plot Cp over and under airfoil
    import argparse
    import matplotlib.pyplot as plt
    from utils import rotateNumpy, plotFoil

    parser = argparse.ArgumentParser()
    
    parser.add_argument('--alpha',      type=float, help='Angle of attack. Float.', 
                        default=8.0)
    
    parser.add_argument('--NACA4Digit', type=int,   help='NACA 4 digits series number to be plotted', 
                        default=2412)
	
    args = parser.parse_args()

    # Parse user inputs
    angle           = args.alpha
    seriesNumber    = args.NACA4Digit
    # Show Foil  -- Recreation of fig frosm Kuethea (1968)

    # Numbers of panels
    panels = [12,48]
    plotFoil(seriesNumber,angle,panels=panels).show()
    plt.clf()

    # END #
    print(f"Done plotting NACA {seriesNumber}.")
    exit()
