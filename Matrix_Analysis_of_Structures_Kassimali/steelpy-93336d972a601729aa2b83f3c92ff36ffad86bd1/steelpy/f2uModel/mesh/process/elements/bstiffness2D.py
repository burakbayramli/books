# ------------------------------------------------------------------------
# The following function is implemented in Python by Professor Terje Haukaas
# at the University of British Columbia in Vancouver, Canada. It is made
# freely available online at terje.civil.ubc.ca together with notes,
# examples, and additional Python code. Please be cautious when using
# this code; it may contain bugs and comes without any form of warranty.
# ------------------------------------------------------------------------
import numpy as np

def beam2D_K(L:float, A:float, I:float, E:float, 
             beta:int, nu:float=0.30):
    #
    #A = 1250 / 1000**2 #m2
    #I = 275 * 10**-6 #m4
    #
    # Calculate alpha for shear deformation (assume beta=0 means user doesn't want this)
    if beta > 0.0:
        G = E / (2.0 * (1 + nu))
        alpha = 12.0 * E * I / (G * beta * A * L**2)
    else:
        alpha = 0

    # Local stiffness matrix
    #Kl = np.array([(E*A/L,   0,            0,         -E*A/L, 0,            0),
    #               (0,       12*E*I/L**3, -6*E*I/L**2, 0,    -12*E*I/L**3, -6*E*I/L**2),
    #               (0,      -6*E*I/L**2,   (4+alpha)*E*I/L,    0,     6*E*I/L**2,   (2-alpha)*E*I/L),
    #               (-E*A/L,  0,            0,          E*A/L, 0,            0),
    #               (0,      -12*E*I/L**3,  6*E*I/L**2, 0,     12*E*I/L**3,  6*E*I/L**2),
    #               (0,      -6*E*I/L**2,   (2-alpha)*E*I/L,    0,     6*E*I/L**2,   (4+alpha)*E*I/L)])
    #
    Kl = np.array([(E*A/L,             0,                 0,    -E*A/L,                  0,                 0),
                   (0,       12*E*I/L**3,        6*E*I/L**2,         0,       -12*E*I/L**3,        6*E*I/L**2),
                   (0,        6*E*I/L**2,   (4+alpha)*E*I/L,         0,        -6*E*I/L**2,   (2-alpha)*E*I/L),
                   (-E*A/L,            0,                 0,     E*A/L,                  0,                 0),
                   (0,      -12*E*I/L**3,       -6*E*I/L**2,         0,        12*E*I/L**3,       -6*E*I/L**2),
                   (0,        6*E*I/L**2,   (2-alpha)*E*I/L,         0,        -6*E*I/L**2,   (4+alpha)*E*I/L)])

    Kl = np.multiply(Kl, 1.0/(1.0 + alpha))

    return Kl
#
#
def beam2D_Geom(L, N):
    """Geometric stiffness matrix"""
    # Geometric stiffness matrix
    Kgeometric = np.array([(0,   0,        0,     0,    0,         0),
                   (0,  6/(5*L), -1/10,   0, -6/(5*L), -1/10),
                   (0, -1/10,     2*L/15, 0,  1/10,    -L/30),
                   (0,   0,        0,     0,     0,        0),
                   (0, -6/(5*L),  1/10,   0,  6/(5*L),  1/10),
                   (0, -1/10,    -L/30,   0,  1/10,     2*L/15)])

    Kgeometric = np.multiply(Kgeometric, N)
    return Kgeometric
#
#
def Rmatrix2D(node1, node2):
    """Transformation matrix Tlg"""
    delta_x = node2.x - node1.x
    delta_y = node2.y - node1.y
    # Transformation matrix Tlg
    if delta_x == 0:
        if delta_y < 0:
            theta = -np.pi/2.0
        else:
            theta = np.pi/2.0
    else:
        theta = np.arctan(delta_y/delta_x)

    c = np.cos(theta)
    s = np.sin(theta)
    Tlg = np.array([(c, s, 0,  0, 0, 0),
                    (-s, c, 0,  0, 0, 0),
                    (0,  0, 1,  0, 0, 0),
                    (0,  0, 0,  c, s, 0),
                    (0,  0, 0, -s, c, 0),
                    (0,  0, 0,  0, 0, 1)])
    #
    return Tlg
#
#