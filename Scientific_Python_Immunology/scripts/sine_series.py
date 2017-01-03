from sympy import *
import pylab

x = symbols('x')
sin(x).series(x, 0, 10)

xs = pylab.linspace(-pylab.pi, pylab.pi, 100)
pylab.plot(xs, sin(xs), '--',
           label='sin(x)', linewidth=3)
pylab.plot(xs, xs, '-',
           label='O(1)')
pylab.plot(xs, xs - xs**3./6, '-',
           label='O(3)')
pylab.plot(xs, xs - xs**3./6 + xs**5./120, '-',
           label='O(5)')
pylab.plot(xs, xs - xs**3./6 + xs**5./120 - xs**7./5040,  '-', 
           label='O(7)')
pylab.plot(xs, xs - xs**3./6 + xs**5./120 - xs**7./5040 + xs**9./362880, '-',
           label='O(9)')
pylab.axvline(0)
pylab.axhline(0)
pylab.legend(loc='best')
pylab.axis([-pylab.pi, pylab.pi, -1, 1])
pylab.show()
