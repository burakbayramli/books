import sys
import logging
import numpy as np
from os.path import dirname, realpath

# insert fem-with-python directory in to path
D = dirname(dirname(realpath(__file__)))
sys.path.insert(0, D)

from femlib.ifem import simplemesh, solve_system, write_results, view_results

def uniform_bar(xa, xb, num_elem, A, E, P, filename=None):
    '''Computes the displacements, reactions, stresses, and strains in a
    uniform bar fixed at its origin subject to force P at its end using finite
    elements

    Parameters
    ----------
    xa, xb : real
        Locations of origin and end of domain, respectively. Giving for the
        domain length L=xb-xa
    num_elem : int
        Number of elements in the mesh
    A, E : real
        Bar area and elastic stiffness
    P : real
        Load applied to bar's end

    Returns
    -------
    u, r : ndarray (num_dof,)
        Nodal displacements
    strain, stress : ndarray (num_elem,)
        Element strains and stresses

    '''
    # generate mesh
    nodes, vertices, elements, connect = simplemesh(xa, xb-xa, num_elem)

    # Element stiffnesses
    k = np.zeros(num_elem)
    for (el, econn) in enumerate(connect):
        i = nodes.index(econn[0])
        j = nodes.index(econn[1])
        k[el] = A * E / (vertices[j] - vertices[i])

    # Fix the origin
    bcs = [(nodes[0], 'x', 0.)]

    # Apply point force at end
    cfs = [(nodes[-1], 'x', P)]

    # find displacements and reactions
    u, r = solve_system(nodes, vertices, elements, connect, k, bcs, cfs)

    # post process
    # element local stress and strain
    strain = np.zeros(num_elem)
    for (el, econn) in enumerate(connect):
        # Strategy: from econn, find the appropriate DOF indices for the left
        # and right nodes. Using the indices, determine the change in length
        # of the element and divide by the original element length. This is
        # the strain for the el'th element.
        i = nodes.index(econn[0])
        j = nodes.index(econn[1])
        strain[el] = (u[j] - u[i]) / (vertices[j] - vertices[i])
    stress = E * strain

    if filename is not None:
        node_data = {'U': u, 'R': r}
        element_data = {'S': stress, 'E': strain}
        write_results(filename, nodes, vertices, elements, connect,
                      node_data, element_data)
        view_results(filename)

    return u, r, strain, stress

# --------------------------------------------------------------------------- #
# ------------------------------ Testing  Code ------------------------------ #
# --------------------------------------------------------------------------- #

def test_uniform_bar():
    '''unit test for uniform bar code'''
    logging.info('testing uniform_bar... ')
    xa, xb, n = 0., 1., 10
    A, E, P = 1., 100., 10.
    u, r = uniform_bar(xa, xb, n, A, E, P)

    # check the error in displacement and reaction
    K = A * E / (xb - xa)
    d = P / K
    assert areclose(d, u[-1])
    assert areclose(P, -r[0])

    # FINISH ME
    # check the error in the strain and stress

    logging.info('success')

if __name__ == '__main__':
    test_uniform_bar()
