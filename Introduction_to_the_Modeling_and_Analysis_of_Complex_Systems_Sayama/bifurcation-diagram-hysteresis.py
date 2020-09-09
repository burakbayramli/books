from pylab import *

def r(x):
    return -x + x**3

range1 = linspace(-1.3, -1./sqrt(3))
range2 = linspace(-1./sqrt(3), 1./sqrt(3))
range3 = linspace(1./sqrt(3), 1.3)
plot(r(range1), range1, 'b-', linewidth = 3)
plot(r(range2), range2, 'r--', linewidth = 3)
plot(r(range3), range3, 'b-', linewidth = 3)
plot([r(-1./sqrt(3)), r(1./sqrt(3))], [-1./sqrt(3), 1./sqrt(3)], 'go')
xlabel('r')
ylabel('x_eq')

show()
