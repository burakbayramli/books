#!/usr/bin/env python
#
# Written by:
# -- 
# John L. Weatherwax                2007-07-05
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

import numpy; from numpy import asarray, asmatrix, linalg, identity

def structure_model_eq_10_8( c, sigmau2 ): 
    """
    This code implements Equation 10.8 (the expresions for theta_1 and Omega) for the model

    m_t = m_{t-1} + u_t 
    p_{1,t} = m_t + c q_t 
    p_{2,t} = m_{t-1}

    in the book Empirical Market Microstructure by Joel Hasbrouck.
    """
    
    Theta_1 = asmatrix( [ [ -c**2, c**2 ], [ sigmau2, -sigmau2 ] ] )/( c**2 + sigmau2 )
    Omega   = asmatrix( [ [ c**4 + 3 * c**2 * sigmau2 + sigmau2**2, c**2 * sigmau2 ], [ c**2 * sigmau2, c**2 * sigmau2 ] ] )/( c**2 + sigmau2 )
    return Theta_1, Omega 


def structure_model_eq_10_8_alternative_ordering( c, sigmau2 ): 
    """
    This code implements Equation 10.8 (the expresions for theta_1 and Omega) for the model

    m_t = m_{t-1} + u_t 
    p_{1,t} = m_{t-1}
    p_{2,t} = m_t + c q_t 

    in the book Empirical Market Microstructure by Joel Hasbrouck.  Note that this is a different ordering of prices than before.
    """
    
    Theta_1 = asmatrix( [ [ -sigmau2, sigmau2 ], [ c**2, -c**2 ], ] )/( c**2 + sigmau2 )
    Omega   = asmatrix( [ [ c**2 * sigmau2, c**2 * sigmau2 ], [ c**2 * sigmau2, c**4 + 3 * c**2 * sigmau2 + sigmau2**2 ] ] )/( c**2 + sigmau2 )
    return Theta_1, Omega 


def main():

    print "Duplicate numerical results in the section 'Stacked Models of Multiple Prices'"
    # 
    Omega = asmatrix( [ [ 109.0, 95.3 ], [ 95.3, 109.3 ] ] )
    Fprime = linalg.cholesky( Omega )
    print "Fprime= ", Fprime

    print "Exercise 10.1: (first ordering of prices)"
    # 
    c, sigmau2 = 2.0, 1.0 

    Theta_1, Omega = structure_model_eq_10_8( c, sigmau2 ) 
    print "Theta_1= ", Theta_1
    print "Omega= ", Omega
    Fprime = linalg.cholesky( Omega ) 
    print "Fprime= ", Fprime 
    print "checking: Fprime * Fprime.T= ", Fprime * Fprime.T

    T1 = identity( 2 ) + Theta_1 * 1.0
    print "Compute the matrix theta(L=1)= ", T1
    print "Its first row is= ", T1[0,]
    T1Fprime =  asmatrix( T1[0,] ) * Fprime 
    print "[ theta(1) ]_1 F' = ", T1Fprime
    print "each element squared= ", asarray(T1Fprime)**2
    print "( [ theta(1) ]_1 F' ) ( [ theta(1) ]_1 F' )' = ", T1Fprime * T1Fprime.T 

    print "Exercise 10.1: (second ordering of prices)"
    #
    Theta_1, Omega = structure_model_eq_10_8_alternative_ordering( c, sigmau2 ) 
    print "Theta_1= ", Theta_1
    print "Omega= ", Omega
    Fprime = linalg.cholesky( Omega ) 
    print "Fprime= ", Fprime 
    print "checking: Fprime * Fprime.T= ", Fprime * Fprime.T

    T1 = identity( 2 ) + Theta_1 * 1.0
    print "Compute the matrix theta(L=1)= ", T1
    print "Its first row is= ", T1[0,]
    T1Fprime =  asmatrix( T1[0,] ) * Fprime 
    print "[ theta(1) ]_1 F' = ", T1Fprime
    print "each element squared= ", asarray(T1Fprime)**2
    print "( [ theta(1) ]_1 F' ) ( [ theta(1) ]_1 F' )' = ", T1Fprime * T1Fprime.T 

if __name__ == "__main__":
    main()
    
