import sys
import logging
import numpy as np

from numerix import aslist

__all__ = ['solve_system', 'simplemesh', 'display_truss']
DOFS = {'x': 0, 'y': 1, 'z': 2}

def solve_system(nodes, vertices, elements, connect, k, bcs, cfs,
                 num_dof_per_node=None, bc_method='default'):
    '''Computes the dependent variable of a linear system Ku = F by
    finite elements

    Parameters
    ----------
    nodes : list
        nodes[i] is the label of node i
    vertices : array
        vertices[i] is the nodal coordinates of the node labeled nodes[i]
    elements : list
        elements[i] is the label of element i
    connect :
        connect[i,j] is the jth node of element i
    k : list
        k[i] is the stiffness of the ith element
    bcs : list
        List of nodal boundary conditions. bcs[i] = (n, dof, bc) where n is
        the node number, dof is one of x or y, and bc is the magnitude of the
        nodal displacement
    cfs : list
        List of nodal forces.  cfs[i] = (n, dof, f) where n is the node
        number, dof is one of x or y, and f is the magnitude of the force

    Returns
    -------
    u : ndarray
        Nodal displacements
    r : ndarray
        Nodal reactions

    '''
    nodes = aslist(nodes)
    elements = aslist(elements)
    vertices = np.asarray(vertices)
    connect = np.asarray(connect)

    # Check that user input is consistent
    if len(vertices.shape) == 1:
        # we want vertices to have at least 1 column
        vertices = np.reshape(vertices, (vertices.shape[0], 1))
    num_node, num_dim = vertices.shape
    errors = 0
    num_elem = connect.shape[0]
    if num_elem != len(k):
        logging.error('wrong number of element stiffnesses')
        errors += 1
    if connect.shape[1] != 2:
        logging.error('expected 2 nodes per element')
        errors += 1
    nun = len(np.unique(connect))
    if nun != num_node:
        s = 'orphaned nodes in vertices'
        if nun < num_node:
            logging.error(s)
            errors += 1
        else:
            logging.warn(s)

    # make sure forces and boundary conditions not applied to same node
    bci = [bc[0] for bc in bcs]
    if any(cf[0] in bci for cf in cfs):
        logging.error('concentrated force and displacement '
                      'imposed on same node[s]')
        errors += 1

    # make sure that the system is stable
    if len(bcs) <= 0:
        logging.error('system requires at least one prescribed displacement '
                      'to be stable')
        errors += 1

    if errors:
        raise SystemExit('stopping due to previous errors')

    # total number of degrees of freedom
    num_dof_per_node = num_dof_per_node or num_dim
    num_dof = num_node * num_dof_per_node

    # Assemble stiffness
    K = np.zeros((num_dof, num_dof))
    nd = num_dof_per_node
    for (el, econn) in enumerate(connect):
        i = nodes.index(econn[0]) * num_dof_per_node
        j = nodes.index(econn[1]) * num_dof_per_node
        K[i:i+nd, i:i+nd] += k[el]
        K[j:j+nd, j:j+nd] += k[el]
        K[i:i+nd, j:j+nd] -= k[el]
        K[j:j+nd, i:i+nd] -= k[el]

    # Assemble force
    F = np.zeros(num_dof)
    for (node, dof, cf) in cfs:
        i = nodes.index(node) * num_dof_per_node + DOFS[dof.lower()]
        F[i] = cf

    # Apply boundary conditions on copies of K and F that we retain K and F
    # for later use.
    Kbc = np.array(K)
    Fbc = np.array(F)
    X = 1.e9 * np.amax(k)
    for (node, dof, bc) in bcs:
        i = nodes.index(node) * num_dof_per_node + DOFS[dof.lower()]
        if bc_method == 'default':
            # Default method - apply bcs such that the global stiffness
            # remains symmetric

            # Modify the RHS
            Fbc -= [Kbc[j, i] * bc for j in range(num_dof)]
            Fbc[i] = bc

            # Modify the stiffness
            Kbc[i, :] = 0
            Kbc[:, i] = 0
            Kbc[i, i] = 1

        elif bc_method == 'penalty':
            Kbc[i, i] = X
            Fbc[i] = X * bc

        else:
            raise ValueError('unknown bc_method {0}'.format(method))

    u = np.linalg.solve(Kbc, Fbc)
    r = np.dot(K, u) - F

    return u, r

def simplemesh(ox, dx, nx):
    '''Simple wrapper to mesh.genmesh_1d'''
    import mesh
    return mesh.genmesh_1d(ox, dx, nx)

def display_truss(points, connect, filename=None, color='g', co=False,
            overlay=None, ol='Deformed', oc='b', label='Undeformed', offset=1):
    '''Display the nodal coordinates and element connectivity

    Parameters
    ----------
    points : ndarray, (num_nodes, 2)
        points[i, j] - jth coordinate of node i
    connect : ndarray, (num_elem, 2)
        connect[i, j] - jth node of element i
    filename : str, optional [None]
        If given, the name of a file to save the plot to
    color : str, optional [g]
        Plot color
    overlay : ndarray, (num_nodes, 2), optional [None]
        Truss points to overlay
    oc : str, optional [b]
        Overlay color
    offset : int, optional [1]
        Offset.  This routine assumes that node and element numbering is
        1 based.  Set offset to 0 for 0 based.

    Notes
    -----
    if filename is None, the plot will be shown and will clog
    the shell until closed.  Any advice on how to get the plot to not
    clog the shell is much appreciated!

    '''
    import matplotlib.pyplot as plt
    def margin(points, alt=None):
        '''Set a margin to be 2% of largest value'''
        m = .02 * np.max(np.abs(points))
        if alt is None:
            return m
        return max(.02 * np.max(np.abs(alt)), m)

    def lims(points, alt=None):
        dm = margin(points, alt=alt)
        m, M = np.amin(points), np.amax(points)
        if alt is not None:
            m = min(m, np.amin(alt))
            M = max(M, np.amax(alt))
        return m - dm, M + dm

    connect = np.asarray(connect)
    points = np.asarray(points)
    # close off elements?
    co = co or connect.shape[1] > 2

    if len(points.shape) == 1:
        points = np.reshape(points, (-1, 2))

    point_sets = [(points, color, label)]
    altx = alty = None
    if overlay is not None:
        if overlay.shape != points.shape:
            error('points and overlay must have same shape')
        point_sets.append((overlay, oc, ol))
        altx = overlay[:, 0]
        alty = overlay[:, 1]

    # determine limits for plot and plot the points
    plt.xlim(lims(points[:, 0], altx))
    plt.ylim(lims(points[:, 1], alty))

    for (points, c, l) in point_sets:
        plt.scatter(points[:, 0], points[:, 1], color=c, edgecolor=c, label=l)
        # loop through elements and connect vertices with lines
        for (el, nodes) in enumerate(connect):
            nodes = [n-offset for n in nodes]
            for (i, b) in enumerate(nodes[1:], start=1):
                a = nodes[i-1]
                x = [points[a, 0], points[b, 0]]
                y = [points[a, 1], points[b, 1]]
                plt.plot(x, y, color=c, linestyle='-')
            if co:
                # close out plot
                a = nodes[0]
                x = [points[a, 0], points[b, 0]]
                y = [points[a, 1], points[b, 1]]
                plt.plot(x, y, color=c, linestyle='-')

    plt.legend(loc='best')
    if filename is None:
        plt.show()
    else:
        plt.savefig(filename, transparent=True)

def write_results(filename, nodes, vertices, elements, connect,
                  node_data, element_data):
    '''Write results to the open database file

    Parameters
    ----------
    nodes : list
        nodes[i] is the label of node i
    vertices : array
        vertices[i] is the nodal coordinates of the node labeled nodes[i]
    elements : list
        elements[i] is the label of element i
    connect :
        connect[i,j] is the jth node of element i
    node_data, element_data : dict
        dict[key] is an array of values for key

    '''
    from mesh import Mesh
    from fileio import File
    from data import StepRepository
    from constants import NODE, ELEMENT, SCALAR, VECTOR

    mesh = Mesh(type='free', nodes=nodes, vertices=vertices,
                elements=elements, connect=connect, block='Block-1')

    steps = StepRepository()

    X = mesh.vertices
    nodes = mesh.nodes
    elements = mesh.elements

    def typ(a):
        try:
            len(a)
            return SCALAR
        except:
            return VECTOR

    def fd(d, p):
        # format the incoming dictionary
        return dict([(k, {'data': v, 'position': p, 'type': typ(v)})
                     for (k, v) in d.items()])

    # put all data in single formatted dictionary
    all_data = fd(node_data, NODE)
    all_data.update(fd(element_data, ELEMENT))

    # zeroth and first step
    S0, S1 = 'Step 0', 'Step 1'
    steps.Step(S0)
    steps[S0].Frame(0, 0)
    steps.Step(S1)
    steps[S1].Frame(0, 1)

    for (key, info) in all_data.items():

        data = info['data']
        labels = nodes if info['position'] == NODE else elements
        args = (key, info['type'], info['position'], mesh)
        zero = np.zeros_like(data)

        steps[S0].frames[0].FieldOutput(*args)
        steps[S0].frames[0].field_outputs[key].add_data(labels, zero)

        steps[S1].frames[0].FieldOutput(*args)
        steps[S1].frames[0].field_outputs[key].add_data(labels, data)

    file = File(filename, 'w')
    file.initialize(mesh.dimension, mesh.num_node, mesh.nodes, mesh.vertices,
                    mesh.num_elem, mesh.elements, mesh.connect,
                    mesh.element_blocks, steps[S0].frames[0].field_outputs)
    file.put_steps(steps.values())

def view_results(filename):
    from viewer import launch_viewer
    launch_viewer([filename])

# --------------------------------------------------------------------------- #
# ------------------------------ Testing  Code ------------------------------ #
# --------------------------------------------------------------------------- #

def test_solve_system():
    from numerix import areclose, allclose
    sys.stdout.write('testing solve_system... ')
    sys.stdout.flush()

    n = 1000
    offset = 0
    xa, xb = 0., 1.
    nodes, vertices, elements, connect = simplemesh(xa, xb, n)

    A, E, P = 1., 100., 10.
    k = np.zeros(n)
    for (el, econn) in enumerate(connect):
        i, j = nodes.index(econn[0]), nodes.index(econn[1])
        k[el] = A * E / (vertices[j] - vertices[i])

    delta = .05
    bcs = [(nodes[0],'x', delta)]
    cfs = [(nodes[-1], 'x', P)]
    u, r = solve_system(nodes, vertices, elements, connect, k, bcs, cfs)

    # check the error
    K = A * E / (xb - xa)
    d = P / K + delta
    assert areclose(d, u[-1])
    assert areclose(P, -r[0])
    assert allclose(r[1:], 0.)
    sys.stdout.write('success\n')

if __name__ == '__main__':
    test_solve_system()
