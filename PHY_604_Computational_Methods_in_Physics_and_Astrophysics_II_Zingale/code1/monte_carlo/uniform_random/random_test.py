# Do a simple random number generator based on the linear congruential
# generator

import matplotlib.pyplot as plt

class Rand(object):
    def __init__(self, seed):
        self.seed = seed

        self.a = 16807        # 7**5
        self.c = 0
        self.M = 2147483647   # 2**31 -1

    def ran(self):
        xn = (self.a*self.seed + self.c) % self.M
        self.seed = xn
        # note that by dividing by M and not M-1, we will never get 1,
        # so this gives #s in the range [0, 1)
        return xn/float(self.M)


def test_random():
    r = Rand(1)

    x = []
    for i in range(10000):
        x.append(r.ran())

    # make pairs out of successive points
    x1 = x[1:]
    x = x[:-1]

    plt.scatter(x, x1, s=5)
    plt.xlabel(r"$x_i$")
    plt.ylabel(r"$x_{i+1}$")

    plt.xlim(0,1)
    plt.ylim(0,1)

    plt.savefig("random.png", dpi=150)


if __name__ == "__main__":
    test_random()




