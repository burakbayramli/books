import numpy as np
import matplotlib.pyplot as plt

def FE_logistic(u0, dt, N):
    u = np.zeros(N+1)
    u[0] = u0
    for n in range(N):
        u[n+1] = u[n] + dt*(u[n] - u[n]**2)
    return u

def quadratic_roots(a, b, c):
    delta = b**2 - 4*a*c
    r2 = (-b + np.sqrt(delta))/float(2*a)
    r1 = (-b - np.sqrt(delta))/float(2*a)
    return r1, r2

def BE_logistic(u0, dt, Nt, choice='Picard',
                eps_r=1E-3, omega=1, max_iter=1000):
    if choice == 'Picard1':
        choice = 'Picard'
        max_iter = 1

    u = np.zeros(Nt+1)
    iterations = []
    u[0] = u0
    for n in range(1, Nt+1):
        a = dt
        b = 1 - dt
        c = -u[n-1]
        if choice in ('r1', 'r2'):
            r1, r2 = quadratic_roots(a, b, c)
            u[n] = r1 if choice == 'r1' else r2
            iterations.append(0)

        elif choice == 'Picard':
            def F(u):
                return a*u**2 + b*u + c

            u_ = u[n-1]
            k = 0
            while abs(F(u_)) > eps_r and k < max_iter:
                u_ = omega*(-c/(a*u_ + b)) + (1-omega)*u_
                k += 1
            u[n] = u_
            iterations.append(k)

        elif choice == 'Newton':
            def F(u):
                return a*u**2 + b*u + c

            def dF(u):
                return 2*a*u + b

            u_ = u[n-1]
            k = 0
            while abs(F(u_)) > eps_r and k < max_iter:
                u_ = u_ - F(u_)/dF(u_)
                k += 1
            u[n] = u_
            iterations.append(k)
    return u, iterations

def CN_logistic(u0, dt, Nt):
    u = np.zeros(Nt+1)
    u[0] = u0
    for n in range(0, Nt):
        u[n+1] = (1 + 0.5*dt)/(1 + dt*u[n] - 0.5*dt)*u[n]
    return u

def quadratic_root_goes_to_infinity():
    """
    Verify that one of the roots in the quadratic equation
    goes to infinity.
    """
    for dt in 1E-7, 1E-12, 1E-16:
        a = dt
        b = 1 - dt
        c = -0.1
        print(dt, quadratic_roots(a, b, c))

print('sympy calculations')
import sympy as sym
dt, u_1, u = sym.symbols('dt u_1 u')
r1, r2 = sym.solve(dt*u**2 + (1-dt)*u - u_1, u)
print(r1)
print(r2)
print(r1.series(dt, 0, 2))
print(r2.series(dt, 0, 2))

T = 9
try:
    dt = float(sys.argv[1])
    eps_r = float(sys.argv[2])
    omega = float(sys.argv[3])
except:
    dt = 0.8
    eps_r = 1E-3
    omega = 1
N = int(round(T/float(dt)))

u_BE3, iter_BE3 = BE_logistic(0.1, dt, N, 'Picard', eps_r, omega)
print(iter_BE3)
print('Picard mean no of iterations (dt=%g):' % dt, int(round(np.mean(iter_BE3))))

u_FE = FE_logistic(0.1, dt, N)
u_BE1, _ = BE_logistic(0.1, dt, N, 'r1')
u_BE2, _ = BE_logistic(0.1, dt, N, 'r2')
u_BE31, iter_BE31 = BE_logistic(0.1, dt, N, 'Picard1', eps_r, omega)
u_BE3, iter_BE3 = BE_logistic(0.1, dt, N, 'Picard', eps_r, omega)
u_BE4, iter_BE4 = BE_logistic(0.1, dt, N, 'Newton', eps_r, omega)
u_CN = CN_logistic(0.1, dt, N)

from numpy import mean
print('Picard mean no of iterations (dt=%g):' % dt, int(round(np.mean(iter_BE3))))
print('Newton mean no of iterations (dt=%g):' % dt, int(round(np.mean(iter_BE4))))

t = np.linspace(0, dt*N, N+1)
plt.plot(t, u_FE, t, u_BE2, t, u_BE3, t, u_BE31, t, u_BE4, t, u_CN)
plt.legend(['FE', 'BE exact', 'BE Picard', 'BE Picard1', 'BE Newton', 'CN gm'])
plt.title('dt=%g, eps=%.0E' % (dt, eps_r))
plt.xlabel('t')
plt.ylabel('u',)
filestem = 'logistic_N%d_eps%03d' % (N, np.log10(eps_r))
plt.savefig(filestem + '_u.png')
plt.savefig(filestem + '_u.pdf')
plt.figure()
plt.plot(list(range(1, len(iter_BE3)+1)), iter_BE3, 'r-o',
         list(range(1, len(iter_BE4)+1)), iter_BE4, 'b-o')
plt.legend(['Picard', 'Newton'])
plt.title('dt=%g, eps=%.0E' % (dt, eps_r))
plt.axis([1, N+1, 0, max(iter_BE3 + iter_BE4)+1])
plt.xlabel('Time level')
plt.ylabel('No of iterations')
plt.savefig(filestem + '_iter.png')
plt.savefig(filestem + '_iter.pdf')
