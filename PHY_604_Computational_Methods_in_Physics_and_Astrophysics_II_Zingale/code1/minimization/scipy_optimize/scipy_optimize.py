# an example of steepest (gradient) descent minimization

import numpy as np
import matplotlib.pyplot as plt
import scipy.optimize as optimize

def rosenbrock(x, a, b):
    return (a - x[0])**2 + b*(x[1] - x[0]**2)**2

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

    f = np.zeros((N, N), dtype=np.float64)

    for i in range(N):
        for j in range(N):
            f[i,j] = rosenbrock([x[i], y[j]], a, b)

    plt.imshow(np.log10(np.transpose(f)),
               origin="lower",
               extent=[xmin, xmax, ymin, ymax])

    plt.colorbar()
    plt.tight_layout()

    plt.savefig("min_2d_start.png", dpi=150)


    # use SciPy's optimization
    min_history = []

    res = optimize.minimize(rosenbrock, [-1.0, 1.5], args=(a, b),
                            callback=lambda x: min_history.append(x))

    print(res.x)

    for n in range(1, len(min_history)):
        plt.plot([min_history[n-1][0], min_history[n][0]],
                 [min_history[n-1][1], min_history[n][1]], color="C1")

    plt.scatter([res.x[0]], [res.x[1]], marker="o", color="C1")

    plt.savefig("min_2d_scipy.png", dpi=150)

if __name__ == "__main__":
    main()
