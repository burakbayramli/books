# Just use odespy...

import odespy
import matplotlib.pyplot as plt

def f(u, t, omega=2):
    u, v = u
    return [v, -omega**2*u]

def demo():
    from numpy import pi, linspace, cos
    omega = 2
    P = 2*pi/omega
    dt = P/20
    T = 40*P
    X_0 = 2
    RK4 = odespy.RK4(f, f_args=[omega])
    RK4.set_initial_condition([X_0, 0])
    N_t = int(round(T/dt))
    u, t = RK4.solve(linspace(0, T, N_t+1))
    u, v = u[:,0], u[:,1]

    # Last p periods
    p = 10
    m = p*20
    fig = plt.figure()
    l1, l2 = plt.plot(t[-m:], u[-m:], 'b-', t[-m:], X_0*cos(omega*t)[-m:], 'r--')
    fig.legend((l1, l2), ('numerical', 'exact'), 'lower left')
    plt.xlabel('t')
    plt.show()
    plt.savefig('tmp.pdf'); plt.savefig('tmp.png')

demo()
