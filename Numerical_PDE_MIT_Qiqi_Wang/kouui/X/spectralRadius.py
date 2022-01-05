"""
2017NumericalMethodsOfPDE, lectureX2
compute and show iteration matrix and spectral radius of Jacobi/G-S

date : 2018-06-17
author: kouui
"""

import numpy as np
import matplotlib.pyplot as plt

if __name__ == "__main__":

    key = ("Jacobi","Gauss-Seidel")[0]

    n = 100
    if key == "Jacobi":
        J = np.diag(np.ones(n-1), +1) + np.diag(np.ones(n-1), -1)
        J *= 0.5
        iterationMatrix = J
    elif key == "Gauss-Seidel":
        pass
    else:
        assert False, "bad key string!"

    eigvals = np.linalg.eigvals(iterationMatrix)
    eigvals.sort()

    print("spectral radius of {} is: {}".format(key,max(abs(eigvals))))

    fig, ax = plt.subplots(1,1, figsize=(7,5), dpi=100)
    ax.plot(eigvals, "o", markersize=3,markerfacecolor="None",
    markeredgecolor="black",markeredgewidth=1)
    plt.show()
