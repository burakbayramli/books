# an example of steepest (gradient) descent minimization

import numpy as np
import matplotlib.pyplot as plt

def rosenbrock(x0, x1, a, b):
    return (a - x0)**2 + b*(x1 - x0**2)**2

def drosdx(x, a, b):
    x0 = x[0]
    x1 = x[1]
    return np.array([-2.0*(a - x0) - 4.0*b*(x1 - x0**2)*x0,
                     2.0*b*(x1 - x0**2)])

def main():

    xmin = -2.0
    xmax = 2.0
    ymin = -1.0
    ymax = 3.0

    a = 1.0
    b = 100.0

    N = 256
    x = np.linspace(xmin, xmax, N)
    y = np.linspace(ymin, ymax, N)

    x2d, y2d = np.meshgrid(x, y, indexing="ij")

    plt.imshow(np.log10(np.transpose(rosenbrock(x2d, y2d, a, b))), 
               origin="lower",
               extent=[xmin, xmax, ymin, ymax])

    plt.colorbar()
    plt.tight_layout()

    plt.savefig("min_2d_start.png", dpi=150)


    # do descent
    xp = np.array([-1.0, 1.5])
    xp_old = 1000*xp

    eps = 1.e-5

    eta = 0.003

    while np.linalg.norm(xp - xp_old) > eps:
        xp_old[:] = xp[:]
        grad = drosdx(xp, a, b)

        # eta_pred = rosenbrock(xp[0], xp[1], a, b)/np.linalg.norm(grad)
        # print("eta predict = ", eta_pred)
        xp[:] += -eta * grad[:]

        plt.plot([xp_old[0], xp[0]], [xp_old[1], xp[1]], color="C1")

    plt.scatter([xp[0]], [xp[1]], marker="o", color="C1")

    print(xp)

    plt.savefig("min_2d_descent.png", dpi=150)

if __name__ == "__main__":
    main()
