from classes import Y   # class Y is in module classes
y = Y(3)                # make an instance y with v0=3
v1 = y.value(0.2)       # evaluate the formula for t=0.2
v2 = y.value(t=0.3)     # another call
y.formula()
print y.v0              # see what v0 was set to (3)
print y.g               # see the value of g
print v1, v2
y(0.2)

y = Y(1.5)
y(0.2)
print y


def diff(f, x, h=1.0E-10):
    return (f(x+h) - f(x))/float(h)

y1 = Y(1)
print diff(y1.value, 0, 1)

from classes import VelocityProfile

v1 = VelocityProfile(R=1, beta=0.06, mu0=0.02, n=0.1)
from scitools.std import *
r = linspace(0, 1, 50)
v = v1.value(r)
plot(r, v, label=('r', 'v'), title='Velocity profile')


from classes import Account
a1 = Account('John Olsson', '19371554951', 20000)
a2 = Account('Liz Olsson',  '19371564761', 20000)
a1.deposit(1000)
a1.withdraw(4000)
a2.withdraw(10500)
a1.withdraw(3500)
print "a1's balance:", a1.balance
a1.dump()
a2.dump()

a1.no = '19371564764'
a1.balance = 100
a1.dump()

from classes import AccountP as Account
a1 = Account('John Olsson', '19371554951', 20000)
a1.deposit(1000)
a1.withdraw(4000)
a1.withdraw(3500)
a1.dump()
print a1._balance       # it works, but a convention is broken
print a1.get_balance()  # correct way of viewing the balance
a1._no = '19371554955'  # this is a "serious crime"


from classes import Person
p1 = Person('Hans Petter Langtangen',
            office_phone='67828283', email='hpl@simula.no')
p2 = Person('Aslak Tveito', office_phone='67828282')
p2.add_email('aslak@simula.no')
phone_book = [p1, p2]                             # list
for person in phone_book:
    person.dump()

phone_book = {'Langtangen': p1, 'Tveito': p2}     # dict is better
for person in sorted(phone_book):
    phone_book[person].dump()
    
from classes import Circle
c = Circle(2, -1, 5)
print 'A circle with radius %g at (%g, %g) has area %g' % \
      (c.R, c.x0, c.y0, c.area())

from classes import Derivative
from math import *
df = Derivative(sin)
print dir(df)
x = pi
df(x)
cos(x)  # exact
def g(t):
    return t**3

dg = Derivative(g)
t = 1
dg(t)  # compare with 3 (exact)


from Polynomial import Polynomial
p1 = Polynomial([1, -1])
p2 = Polynomial([0, 1, 0, 0, -6, -1])
p3 = p1 + p2
print p3.coeff
p4 = p1*p2
print p4.coeff
p5 = p2.derivative()
print p5.coeff
x = 0.5
p1_plus_p2_value = p1(x) + p2(x)
p3_value = p3(x)
print p1_plus_p2_value - p3_value
p2.differentiate()
print p2.coeff
p4 = p2.derivative()
print p2.coeff, p4.coeff
print p1
print p2
print p3
print p3.simplestr()
print p4

from Vec2D import Vec2D
u = Vec2D(0,1)
v = Vec2D(1,0)
w = Vec2D(1,1)
a = u + v
print a
type(a)
a == w
a = u - v
print a
a = u*v
print a
type(a)
print abs(u)
u == v
u != v

from Complex import Complex

u = Complex(2,-1)
v = Complex(1)
w = u + v
print w
w != u
u < v

w = u + 4.5   # this causes an error
w = 4.5 + u   # this too

w = u - 4.5   # this should work
print w
w = 4.5 - u
print w


sys.path.insert(0, os.path.join(os.pardir, 'diffeq'))
from Newton import Newton

def f(x):
    return 100000*(x - 0.9)**2 * (x - 1.1)**3


x = linspace(0.87, 1.15, 100)
y = f(x)
y0 = zeros(len(x))
plot(x, y, x, y0, axis=[x[0],x[-1],-1.15,y[-1]], hardcopy='tmp.eps')
df = Derivative(f)
Newton(f, 1.01, df, epsilon=1E-5)
Newton(f, 1.01, df, epsilon=1E-10)
Newton(f, 0.92, df, epsilon=1E-10)

def df_exact(x):
    return 100000*(2*(x-0.9)*(x-1.1)**3 + \
                   (x-0.9)**2*3*(x-1.1)**2)

Newton(f, 1.01, df_exact, epsilon=1E-5)
def fm(x):
    return f(x)/100000.0

dfm = Derivative(fm)
Newton(fm, 1.01, dfm, epsilon=1E-10)

from IntervalMath import IntervalMath as I
y_0 = 1
g = 9.81
T = sqrt(2*y_0/g)
T
Tm = 0.45
y_0 = I(0.99, 1.01)
T = I(Tm*0.95, Tm*1.05)
print T
g = 2*y_0*T**(-2)
g
g.width_in_percent()
# Computing with mean values
T = float(T)
y = 1
g = 2*y_0*T**(-2)
print '%.2f' % g

v0 = I(4, 6)
t = I(0.6, 0.7)
g = 9.81
y = v0*t - 0.5*t**2
print 'v0 =', v0, ', t =', t, ', y =', y
print float(y)
v0 = float(v0);  t = float(t);  y = v0*t - 0.5*t**2
print y

from math import pi
R = I(6*0.9, 6*1.1)   # 20 % error
V = (4./3)*pi*R**3
print R
print V
print float(V)
R.width_in_percent()
V.width_in_percent()
# Mean values
R = float(R)
V = (4./3)*pi*R**3
print V
