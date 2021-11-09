# some simple helper routines for the SymPy eigenvalue stuff for hydro

import sympy

u, c = sympy.symbols("u c")
plus = u + c
zero = u
minus = u - c

class Eigenvector(object):
    def __init__(self, name, ev, r, l=None):
        self.name = name
        self.ev = ev
        self.l = l
        self.r = r

    def __lt__(self, other):
        if self.name == "minus":
            return other
        elif self.name == "plus":
            return plus
        else:
            if other == "minus":
                return self
            else:
                return other

    def __str__(self):
        return "{} wave, r = {}, l = {}".format(self.eigenvalue, self.r, self.l)


def eigensystem(A):
    # get the left and right eigenvectors that diagonalize the system.
    # it is best to use sympy diagonalize() for this purpose than getting
    # the left and right eigenvectors independently.

    e = []

    R, D = A.diagonalize()

    # the columns of R are the right eigenvectors and the diagonal
    # element of D is the corresponding eigenvalues

    for n in range(A.shape[0]):
        r = R.col(n)
        ev = D[n,n]

        # which eigenvalue are we?
        if sympy.simplify(ev - minus) == 0:
            name = "minus"
        elif sympy.simplify(ev - plus) == 0:
            name = "plus"
        elif sympy.simplify(ev - zero) == 0:
            name = "zero"
        else:
            return None

        # normalize the right eigenvector
        v = r[0]
        if v != 0:
            r = r/v

        e.append(Eigenvector(name=name, ev=ev, r=r))

    # now sort the system from smallest (u-c) to largest (u+c)
    e.sort()

    # now let's construct the R with this sorting
    for n in range(A.shape[0]):
        R[:,n] = e[n].r

    # the left eigenvector matrix, L, is just the inverse
    L = R**-1

    for n in range(A.shape[0]):
        e[n].l = L.row(n)

    return e

