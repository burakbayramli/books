# An example of general linear least squares fitting.  Here our basis
# functions can be non-linear, but we fit to a linear combination of
# the basis functions.  We consider simple polynomials (1, x, x**2, ...)
# and the Legendre polynomials
#
# M. Zingale

import numpy as np
import matplotlib.pyplot as plt


def poly_basis(M, x):
    """ the basis functions for the fit -- here x**n """

    j = np.arange(M)

    # simple polynomials
    return x**j


def leg_basis(M, x):
    """ the basis functions for the fit -- here they are the Legendra
    polynomials """


    # note that the legendre polynomials are orthogonal in the interval
    # [-1, 1] -- we need to convert our x to that range

    basis = []

    for m in range(M):
        c = np.zeros(M)
        c[m] = 1.0

        basis.append(np.polynomial.legendre.legval(x, c))

    return np.array(basis)


def y_experiment2(a1, a2, a3, sigma, x):
    """ return the experimental data in a quadratic + random fashion,
        with a1, a2, a3 the coefficients of the quadratic and sigma is
        the error.  This will be poorly matched to a linear fit for
        a3 != 0 """

    N = len(x)

    # randn gives samples from the "standard normal" distribution
    r = np.random.randn(N)

    y = a1 + a2*x + a3*x*x + sigma*r

    return y


def general_regression(x, y, sigma, M, basis):
    """ here, M is the number of fitting parameters.  We will fit to
        a function that is linear in the a's, using the basis functions
        from basis() """


    N = len(x)

    # construct the design matrix -- A_{ij} = Y_j(x_i)/sigma_i -- this is
    # N x M.  Each row corresponds to a single data point, x_i, y_i
    A = np.zeros((N, M), dtype=np.float64)

    for i in range(N):
        A[i,:] = basis(M, x[i])/sigma[i]

    # construct the MxM matrix for the linear system, A^T A:
    ATA = np.transpose(A) @ A
    print("size of A^T A:", ATA.shape)
    print("condition number of A^T A:", np.linalg.cond(ATA))

    # construct the RHS
    b = np.transpose(A) @ (y/sigma)

    # solve the system
    a = np.linalg.solve(ATA, b)

    # return the chisq
    chisq = 0
    for i in range(N):
        chisq += (np.sum(a*basis(M, x[i])) - y[i])**2/sigma[i]**2

    chisq /= N-M

    return a, chisq


#-----------------------------------------------------------------------------
N = 40
x = np.linspace(0, 100.0, N)


#-----------------------------------------------------------------------------
# test 1 -- quadratic data with M = 3

print("\ntest 1: quadratic data with M = 3, simple poly\n ")

plt.clf()

# make up the experimental data with errors

sigma = 5.0*np.ones(N)

y = y_experiment2(2.0, 1.50, -0.02, sigma, x)

plt.errorbar(x, y, yerr=sigma, fmt="o")


# do the regression with M = 3 (1, x, x^2)
M = 3
a, chisq = general_regression(x, y, sigma, M, poly_basis)

print("a = ", a)

plt.plot(x, a[0] + a[1]*x + a[2]*x*x )

print("reduced chisq = ", chisq)

plt.savefig("general-regression-M3.png")




#-----------------------------------------------------------------------------
# test 2 -- quadratic data with M = 10

print("\ntest 2: quadratic data with M = 10, simple poly\n ")

plt.clf()

# same data as above
plt.errorbar(x, y, yerr=sigma, fmt="o")


# do the regression with M = 10 (1, x, x^2, ...)
M = 10
a, chisq = general_regression(x, y, sigma, M, poly_basis)

print("a = ", a)

yfit = np.zeros((N), dtype=x.dtype)
for i in range(N):
    base = poly_basis(M, x[i])
    yfit[i] = np.sum(a*base)

plt.plot(x, yfit)

print("reduced chisq = ", chisq)

plt.savefig("general-regression-M10.png")



#-----------------------------------------------------------------------------
# test 3 -- quadratic data with M = 3 with Legendre polynomial basis

print("\ntest 3: quadratic data with M = 10, Legendre poly\n ")

plt.clf()

# same data as above
plt.errorbar(x, y, yerr=sigma, fmt="o")


# do the regression with M = 3 Legendra polynomials
M = 10
a, chisq = general_regression(x, y, sigma, M, leg_basis)

print("a = ", a)

yfit = np.zeros((N), dtype=x.dtype)
for i in range(N):
    base = leg_basis(M, x[i])
    yfit[i] = np.sum(a*base)

plt.plot(x, yfit)

print("reduced chisq = ", chisq)

plt.savefig("general-regression-M10-leg.png")


#-----------------------------------------------------------------------------
# test 4 -- quadratic data in [-1, 1] with simple polynomials

print("\ntest 4: quadratic data on [-1,1] with M = 10, simple poly\n ")

plt.clf()

N = 40
x = np.linspace(-1.0, 1.0, N)

# make up the experimental data with errors

sigma = 1.0*np.ones(N)

y = y_experiment2(2.0, 1.50, -0.02, sigma, x)

plt.errorbar(x, y, yerr=sigma, fmt="o")


# do the regression with M = 10 (1, x, x^2)
M = 10
a, chisq = general_regression(x, y, sigma, M, poly_basis)

print("a = ", a)

yfit = np.zeros((N), dtype=x.dtype)
for i in range(N):
    base = poly_basis(M, x[i])
    yfit[i] = np.sum(a*base)

plt.plot(x, yfit)


print("reduced chisq = ", chisq)

plt.savefig("general-regression-M10-m1p1.png")



#-----------------------------------------------------------------------------
# test 5 -- quadratic data in [-1, 1] with Legendre polynomials

print("\ntest 5: quadratic data on [-1,1] with M = 10, Legendre poly\n ")

plt.clf()

plt.errorbar(x, y, yerr=sigma, fmt="o")


# do the regression with M = 10 (1, x, x^2)
M = 10
a, chisq = general_regression(x, y, sigma, M, leg_basis)

print("a = ", a)

yfit = np.zeros((N), dtype=x.dtype)

for i in range(N):
    base = leg_basis(M, x[i])
    yfit[i] = np.sum(a*base)

plt.plot(x, yfit)


print("reduced chisq = ", chisq)

plt.savefig("general-regression-M10-m1p1-leg.png")
