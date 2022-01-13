import numpy as np
import matplotlib.pyplot as plt

class Particle(object):
    """a particle on the lattice.  When this has stuck to a wall or another
    particle, we will set stuck=True"""

    def __init__(self, i, j):
        self.i = i
        self.j = j

        self.stuck = False

        # this will keep track of the order in which the particles stick
        self.order = -1

class Lattice(object):

    def __init__(self, N):
        """a Lattice is an N x N grid that particles diffuse on.  They stick
        to the edges and move randomly, but cannot move outside the domain or
        onto another particle (if they want to, then we stick)"""

        self.N = N

        # ideally, N should be odd, so there is a single unique center
        self.center = (N//2, N//2)

        self.particles = []

        # if we take too many diffusion steps, we've done something wrong
        self.max_iters = 10*N*N

    def on_boundary(self, i, j):
        """ determine if we are on the boundary """
        return i == 0 or i == self.N-1 or j == 0 or j == self.N-1

    def next_to_particle(self, i, j):
        """ determine if we are next to another particle """
        stick = False
        for p in self.particles:
            if (p.i == i and p.j == j+1 or
                p.i == i and p.j == j-1 or
                p.i == i+1 and p.j == j or
                p.i == i-1 and p.j == j):
                stick = True
                break

        return stick

    def add_and_diffuse_particle(self):
        """ add a new particle and diffuse it until it sticks """

        p = Particle(self.center[0], self.center[1])

        # make sure we haven't evolved to the point that adding a point
        # at the center is already stuck
        if self.next_to_particle(p.i, p.j):
            return -1

        p.order = len(self.particles) + 1

        for n in range(self.max_iters):
            # move in a random direction (up, down, left, right)
            idir = np.random.randint(1, 5)

            if idir == 1:
                inew = p.i + 1
                jnew = p.j

            elif idir == 2:
                inew = p.i - 1
                jnew = p.j

            elif idir == 3:
                inew = p.i
                jnew = p.j + 1

            elif idir == 4:
                inew = p.i
                jnew = p.j - 1

            # have we stuck?
            p.i = inew
            p.j = jnew

            if self.on_boundary(p.i, p.j) or self.next_to_particle(p.i, p.j):
                print(p.i, p.j, self.on_boundary(p.i, p.j), self.next_to_particle(p.i, p.j))
                p.stuck = True
                self.particles.append(p)
                break

        if not p.stuck:
            print("warning... failed to stick")
            return -1

        return 0

    def plot(self):
        data = np.zeros((self.N, self.N))
        for p in self.particles:
            data[p.i, p.j] += 1

        plt.imshow(data)
        plt.savefig("dla.png", dpi=150)


def main():

    l = Lattice(101)
    for n in range(5000):
        ierr = l.add_and_diffuse_particle()
        if ierr == -1:
            print("halting at n = {}".format(n))
            break

    l.plot()


if __name__ == "__main__":
    main()


