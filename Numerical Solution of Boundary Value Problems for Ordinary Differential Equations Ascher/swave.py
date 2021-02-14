# https://pythonhosted.org/scikits.bvp_solver/examples/examples.example5.html
# based on the Fortran code found at http://cs.stmarys.ca/~muir/BVP_SOLVER_Files/swave.f90

'''
Created on Apr 29, 2009

@author: johnsalvatier
'''
from __future__ import division
import scikits.bvp_solver
import numpy
import pylab
from numpy import array

"""
 "Shock Wave", a test problem in Ascher, Mattheij, and
 Russell, Numerical Solution of Boundary Value Problems 
 for Ordinary Differential Equations", Classics in Applied 
 Mathematics Series, SIAM, Philadelphia, 1995. Ch1 page 21 Example 1.17
 
 Describes a shock in a one dimensional nozzle. The system is defined by 
 steady state Navier Stokes.
 
 You may attempt to view the example though Google Books (first result):
 http://books.google.com/books?id=03O-S66kpMQC&lpg=PP1&dq=%22Numerical%20Solution%20of%20Boundary%20Value%20Problems%20for%20Ordinary%20Differential%20Equations%22%22&pg=PA21#v=onepage&q=shock%20wave&f=false
 
 These kinds of problems generally require continuation methods, where
 you repeatedly solve the problem using smaller and smaller eps value
 but here we solve the bvp directly.
 
 
 problem specification:
 
     eps * A(x) * u*u"  - [(1 + g)/2 - eps * A'(x)] * u * u' +
     u'/u + A'(x)/A(x) * (1 - (g - 1)/2 * u**2 ) = 0 
     
     0 < x < 1
     
     where A(x) = 1 + x**2 
     g = 1.4
     
     BC: 
     u(0) = .9129
     u(1) = .375
 
 A is the area of the nozzle at x
 u is a normalized velocity
 eps is an inverse reynolds number
"""

eps = array(.01)
gamma = array(1.4)

def function( t, y):


    term1 = 1.0/ (eps * (1.0 + t**2))
    term2 = .5 + .5 * gamma - 2.0 * eps * t
    
    F = array([y[1]      , #evaluate ODE number 0
                  (term1/y[0]) * (term2 * y[0] * y[1] - y[1]/y[0] -
                                  (2.0 * t/(1.0 + t**2)) * (1.0 - .5 *(gamma - 1.0) * y[0] **2))       ]) #evaluate ODE number 1
        
    return F 

def boundary_conditions(Ya, Yb):

    BCa = array([Ya[0] - .9129      ]) #evaluate left BC number 0

    BCb = array([Yb[0] - .375      ]) #evaluate right BC number 0
    return BCa, BCb

def function_derivative( t, y):

    term1 = 1.0/ (eps * (1.0 + t**2))
    term2 = .5 + .5 * gamma - 2.0 * eps * t

    #evaluate function derivative with respect to variables    #increasing differentiation index
    #(Yi) ---->

    dFdY = array([[0.0      ,1.0      ], #ODE number 0
                  [term1*( 2.0*y[1]/(y[0]**3) + 2.0*t/((1.0+t**2)*y[0]**2) + (t/(1.0+t**2))*(gamma-1.0))      ,
                   (term1/y[0])*( term2*y[0] - 1.0/y[0])      ]]) #ODE number 1
    
    return dFdY

def boundary_conditions_derivative( Ya, Yb):

    #evaluate left boundary conditions derivative with respect to variables
    #increasing differentiation index
    # dBC1/dYa1----->dBC1/dYai
    dBCadYa = array([[1.0      ,0.0      ]]) #left BC number 0

    #evaluate right boundary conditions derivative with respect to variables
    #increasing differentiation index
    # (Yb,i) ----->
    dBCbdYb = array([[1.0      ,0.0      ]]) #right BC number 0

    return dBCadYa, dBCbdYb


problem_definition = scikits.bvp_solver.ProblemDefinition(num_ODE = 2,
                                                  num_parameters = 0,
                                                  num_left_boundary_conditions = 1,
                                                  boundary_points = (0.0, 1.0),
                                                  function = function,
                                                  boundary_conditions = boundary_conditions,
                                                  function_derivative = function_derivative,
                                                  boundary_conditions_derivative = boundary_conditions_derivative)


"""
The initial guess for the solution is based on straight lines joining 
the BCs for each differential equation in the first order system. 
The BCs for component 1 define a line. There are no BCs for the
second component and y2 = y1', so we use the derivative of the
guess for y1.
"""

slope = array(.375 - .9129)
x = array([0.0 ,  0.11111,  0.22222,  0.33333,  0.44444,  0.55555,  0.66666,  0.77778,  0.88888,  1.0   ]) #
#x = numpy.linspace(0,1,20)


solution = scikits.bvp_solver.solve(bvp_problem = problem_definition,
                                        initial_mesh = x,
                                        solution_guess = [ .9129 + slope *x, x*0 + slope     ],
                                        trace = 0)


points = numpy.linspace(0,1, 200)
pylab.plot(points, solution(points)[0,:],'-')
pylab.plot(points, solution(points)[1,:],'-')
pylab.show()
