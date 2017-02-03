class Gravity:
    """Gravity force between two physical objects."""

    def __init__(self, m, M):
        self.m = m           # mass of object 1
        self.M = M           # mass of object 2
        self.G = 6.67428E-11 # gravity constant, m**3/kg/s**2

    def force(self, r):
        G, m, M = self.G, self.m, self.M
        return G*m*M/r**2

    def visualize(self, r_start, r_stop, n=100):
        from scitools.std import plot, linspace
        r = linspace(r_start, r_stop, n)
        g = self.force(r)
        title='Gravity force: m=%g, M=%g' % (self.m, self.M)
        plot(r, g, title=title)

# SI units m and kg:
mass = {'sun': 1.99E+30, 'earth': 5.97E+24,
        'moon': 7.35E+22, 'rocket': 7.57E+5}
distance = {('earth', 'rocket'): 6.6E+6,
            ('earth', 'sun'): 1.5E+11,
            ('earth', 'moon'): 3.85E+8}


if __name__ == '__main__':
    object = 'earth', 'rocket'
    gravity = Gravity(mass[object[0]], mass[object[1]])
    r = distance[(object[0], object[1])]
    Fg = gravity.force(r)
    print 'force between %s and %s: %E N' % (object[0], object[1], Fg)
    gravity.plot(r - 0.2*r, r + 0.2*r)

