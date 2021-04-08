import logging
import numpy as np
from bc import bcsum

__all__ = ['solve']

def solve(A, b, u, *constraints, **kwargs):

    bc_method = kwargs.get('bc_method', 'default')
    method = kwargs.get('method', 'direct')

    # make sure forces and boundary conditions not applied to same node
    ok = 0
    errors = 0
    neumann = []
    dirichlet = []
    constrained_nodes = []
    for constraint in constraints:
        if constraint.type == 'dirichlet':
            ok = 1
            dirichlet.append(constraint)
        else:
            neumann.append(constraint)
        if np.any(np.in1d(constraint.nodes, constrained_nodes)):
            logging.error('multiple BCs applied to same node')
            errors += 1
        constrained_nodes.extend(constraint.nodes)

    methods = ('direct',)
    if method not in methods:
        a = ', '.join(methods)
        logging.error('expected method to be one of {0}, got {1}'.format(a, method))
        errors += 1

    # make sure that the system is stable
    if not ok:
        logging.error('system requires at least one prescribed displacement '
                      'to be stable')
        errors += 1

    if errors:
        raise SystemExit('stopping due to previous errors')

    if method == 'direct':
        return linear_solve(A, b, u, dirichlet, neumann, bc_method)

def linear_solve(A, b, u, dirichlet, neumann, bc_method):
    # Apply boundary conditions on copies of A and b that we retain A and b
    # for later use.
    Abc = np.array(A)
    bbc = np.array(b)

    X = 1.e9 * np.amax(A)
    for (node, dof, magnitude) in bcsum(neumann):
        i = u.V.mesh.dof_map(node) * u.V.num_dof_per_node + dof
        bbc[i] = magnitude

    for (node, dof, magnitude) in bcsum(dirichlet):
        i = u.V.mesh.dof_map(node) * u.V.num_dof_per_node + dof
        if bc_method == 'default':
            # Default method - apply bcs such that the global stiffness
            # remains symmetric

            # Modify the RHS
            bbc -= [Abc[j, i] * magnitude for j in range(u.V.num_dof)]
            bbc[i] = magnitude

            # Modify the stiffness
            Abc[i, :] = 0
            Abc[:, i] = 0
            Abc[i, i] = 1

        elif bc_method == 'penalty':
            raise NotImplementedError('bc_method=="penalty"')

        else:
            raise ValueError('unknown bc_method {0}'.format(bc_method))

    u += np.linalg.solve(Abc, bbc)
    r = np.dot(A, u.vector) - b

    return
