import euler_mol
import numpy as np
from euler_mol import URHO, UMX, UENER
import matplotlib.pyplot as plt

class AdvectionTest(euler_mol.Simulation):

    def init_cond(self, U):

        # make density a Gaussian
        U[:, URHO] = 1.0 + np.exp(-60.0*(self.gr.x - 0.5)**2)

        # make the velocity constant
        U[:, UMX] = U[:, URHO] * 1.0

        # make the pressure constant
        p = 1.0
        U[:, UENER] = p/(self.params['gamma'] - 1.0) + 0.5 * U[:, UMX]**2/U[:, URHO]

        self.U_init = U.copy()


if __name__ == "__main__":
    params = {'tmax': 1.0,
              'gamma': 0.8,
              'cfl': 0.8,
              'bcs': 'periodic',
              'verbose': 0}


for N in [32, 64, 128, 256, 512]:

    adv = AdvectionTest(N, params)
    gr, U = adv.mol_update()

    if N == 64:
        plt.plot(gr.x, U[:, URHO], 'x')
        plt.plot(gr.x, adv.U_init[:, URHO])

        plt.savefig("convergence.png")

    print(N, gr.norm(U - adv.U_init))
