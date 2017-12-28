import sympy as sy
import matplotlib.pyplot as plt
import numpy as np
plt.close()
sy.init_printing()
#From  Johansson, R Numerical Python p.209 onward.
#Create symbols for the ODE
r,k,N0, t = sy.symbols('r k N_0 t' )
N = sy.symbols('N', function=True)
ode = N(t).diff(t) - (r*N(t)*(1-N(t)/k))# - N(t)**2/(1+N(t)**2))
sy.pprint(sy.Eq(ode))
#solve the ODE
ode_sol = sy.dsolve(ode, N(t))
ics = {N(0):N0}
print('+'*30)
print('Iniital conditions:')
sy.pprint(ics)
#Find the value of the integration constants
C_eq = sy.Eq(ode_sol.lhs.subs(t,0).subs(ics),ode_sol.rhs.subs(t,0))
print('+'*30)
print('Create equations at N(0))')
sy.pprint(C_eq)
print()
#Solve for C_eq
C_sol = sy.solve(C_eq,sy.Symbol('C1'))
soln = ode_sol.subs(sy.Symbol('C1'),C_sol[0])
sy.pprint(C_sol)
sy.pprint(soln)
print('+'*30)

ode_func = sy.lambdify(t, soln.rhs.subs({N0:100,r:.2,k:1000}) )
sy.pprint(ode_func(20))
# print('Compare to http://www.ugrad.math.ubc.ca/coursedoc/math100/notes/diffeqs/cool.html')
print()
print('Now use a theano function allowing broadcastable arrays')
from sympy.printing.theanocode import theano_function
theano_ode = theano_function([t],[soln.rhs.subs({N0:.15,r:.2,k:.4})],dims={t:1})
tt = np.linspace(0,80)
theano_output = theano_ode(tt)
sy.pprint(theano_output)
plt.plot(tt, theano_output)
plt.show()
#See http://matthewrocklin.com/blog/work/2013/04/05/SymPy-Theano-part-3
#for better examples on using Theano....
