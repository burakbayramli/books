# to be run by scitools file2interactive

from approx1D import *

x = sym.Symbol('x')
f = 10*(x-1)**2-1
u = least_squares(f=f, psi=[1, x, x**2], Omega=[1, 2])
print(u)
print(sym.expand(f))

# show how equal x**i functions are (ill-conditioning)
import numpy as np
x = np.linspace(1, 2, 1001)
import matplotlib.pyplot as plt
#import scitools.std as plt
for i in range(15):
    plt.plot(x, x**i, '-')
    plt.hold('on')

plt.savefig('tmp_basis_xini.pdf')
plt.savefig('tmp_basis_xini.png')
plt.show()
