"""Solutions for 'Arrays and Matrices' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('arrays_and_matrices_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function, division
from numpy import array, matrix, dot, mat, arange
# <demo> --- stop ---
# Exercise 1
u = array([1.0,1,2,3,5,8])
v = array([[1.0],[1],[2],[3],[5],[8]])
x = array([[1.0,0],[0,1]])
y = array([[1.0,2],[3,4]])
z = array([[1.0,2,1,2],[3,4,3,4],[1,2,1,3]])
w = array([[1.0,0,1,0],[0,1,0,1],[1,2,1,2],[3,4,3,4]])


print("u+v.T:")
print(u+v.T)
print("v+u:")
print(v+u)
print("v+u.T :")
print(v+u.T )
u.shape = 1,6
print("dot(v,u):")
print(dot(v,u))
print("dot(u,v):")
print(dot(u,v))
print("dot(x,y):")
print(dot(x,y))

# <demo> --- stop ---
# Exercise 4
u = mat(u)
v = mat(v)
x = mat(x)
y = mat(y)
z = mat(z)
w = mat(w)

print("u+v.T:")
print(u+v.T)
print("v+u.T :")
print(v+u.T )
print("v*u:")
print(v*u)
print("u*v:")
print(u*v)
print("x*y:")
print(x*y)

# <demo> --- stop ---
# Exercise 3
u = array([1.0,1,2,3,5,8])
v = array([[1.0],[1],[2],[3],[5],[8]])
x = array([[1.0,0],[0,1]])
y = array([[1.0,2],[3,4]])
z = array([[1.0,2,1,2],[3,4,3,4],[1,2,1,3]])
w = array([[1.0,0,1,0],[0,1,0,1],[1,2,1,2],[3,4,3,4]])

a = array([3.0,2])
b = array([[3.0],[2]])
c = array([3.0,2,1,0])
d = array([[3.0],[2],[1],[0]])

print("u+b:")
print(u+b)
print("u+d:")
print(u+d)
print("v+a:")
print(v+a)
print("v+c:")
print(v+c)
print("x+a:")
print(x+a)
print("x+b:")
print(x+b)
print("y+a:")
print(y+a)
print("y+b:")
print(y+b)
print("z+c:")
print(z+c)
print("w+c:")
print(w+c)
print("w+d:")
print(w+d)

# <demo> --- stop ---
# Exercise 3
print("x/1.0:")
print(x/1.0)
print("1.0/x:")
print(1.0/x)

# <demo> --- stop ---
# Exercise 5
print("(x+y)**2:")
print((x+y)**2)
print("x**2+x*y+y*x+y**2:")
print(x**2+x*y+y*x+y**2)

x = mat(x)
y = mat(y)

print("(x+y)**2:")
print((x+y)**2)
print("x**2+x*y+y*x+y**2:")
print(x**2+x*y+y*x+y**2)

# <demo> --- stop ---
# Exercise 6
x = array([[1.0,0],[0,1]])
y = array([[1.0,2],[3,4]])
print("x**2+2*x*y+y**2:")
print(x**2+2*x*y+y**2)

x = mat(x)
y = mat(y)
print("x**2+2*x*y+y**2:")
print(x**2+2*x*y+y**2)

# <demo> --- stop ---
# Exercise 7
# Arrays are diagonal 2-dimensional
# Matrix uses matrix_power, not element-by-elemtn
x1 = array([[1.0,0],[0,4]])
x2 = matrix([[1.0,0],[0,4]])
y = 2
print("x1 ** y:")
print(x1 ** y)
print("x2 ** y:")
print(x2 ** y)


# <demo> --- stop ---
# Exercise 8
a = arange(3.0)
b = a
c = a

print("a*b + a*c:")
print(a*b + a*c)
print("a*b + c:")
print(a*b + c)
print("a*(b+c):")
print(a*(b+c))

# <demo> --- stop ---
# Exercise 9
# If x is array, y must be broadcastable with x, 
# w must be broadcastable with (x**y)
# z must be broadcastable with ((x**y)*w)

# <demo> --- stop ---
# Exercise 10
print("-2**4:")
print(-2**4)
print("(-2)**4:")
print((-2)**4)
print("-2*-2*-2*-2:")
print(-2*-2*-2*-2)
