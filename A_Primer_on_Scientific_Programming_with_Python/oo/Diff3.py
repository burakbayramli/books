import numpy as np

class Diff3:
    table = {
    ('central', 2):
    [      0,       0,      0, -1./2, 0, 1./2,       0,      0,       0],
    ('central', 4):
    [      0,       0,  1./12, -2./3, 0, 2./3,  -1./12,      0,       0],
    ('central', 6):
    [      0,   -1./60, 3./20, -3./4, 0, 3./4,  -3./20,  1./60,       0],
    ('central', 8):
    [ 1./280, -4./105, 12./60, -4./5, 0, 4./5, -12./60, 4./105, -1./280],
    ('forward', 1):
    [      0,       0,      0,     0, 1,    1,       0,      0,       0],
    ('forward', 3):
    [      0,       0,      0, -2./6,-1./2, 1,   -1./6,      0,       0],
    ('backward', 1):
    [      0,       0,      0,    -1, 1,    0,       0,      0,       0],
    }
    def __init__(self, f, h=1.0E-9, type='central', order=2):
        self.f, self.h, self.type, self.order = f, h, type, order
        self.weights = np.array(Diff3.table[(type, order)])

    def __call__(self, x):
        f, h = self.f, self.h
        f_values = np.array([f(x+i*h) for i in range(-4,5)])
        return np.dot(self.weights, f_values)/h

if __name__ == '__main__':
    from math import *
    mycos = Diff3(sin, type='central', order=4)
    print mycos(pi), cos(pi)


