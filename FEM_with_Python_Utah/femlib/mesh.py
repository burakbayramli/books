import re
import numpy as np
from collections import OrderedDict
import StringIO

from numerix import aslist, midpoint
from constants import *

__all__ = ['Mesh']

DOMAINS = {'ILO': (0, 0), 'IHI': (0, 1),
           'JLO': (1, 0), 'JHI': (1, 1),
           'KLO': (2, 0), 'KHI': (2, 1)}

BLOCK_ID_START = 100
NSET_ID_START = 200
SSET_ID_START = 300
MAX_NUM_SETS = 100
NONODE = -10

def Mesh(ox=0., lx=1., nx=1, offset=1, order=1, type='uniform',
         nodes=None, vertices=None, elements=None, connect=None, dimension=None,
         file=None, string=None, block=None):
    '''Mesh factory method.  Interfaces with the FiniteElementMesh class'''

    if type == 'uniform':
        nodes, vertices, elements, connect = genmesh_1d(ox, lx, nx, offset, order)

    elif type == 'free':
        if vertices is None:
            raise ValueError('free mesh requires vertices be given')
        if connect is None:
            raise ValueError('free mesh requires element connectivity be given')
        connect = np.asarray(connect)
        vertices = np.asarray(vertices)
        if dimension is not None and vertices.shape[1] != dimension:
            raise ValueError('dimension not consistent with vertices')
        if nodes is None:
            nodes = [i + offset for i in range(vertices.shape[0])]
        if elements is None:
            elements = [i + offset for i in range(connect.shape[0])]

    elif file is not None:
        nodes, vertices, elements, connect = parse_mesh_file(file)

    elif string is not None:
        file = StringIO.StringIO(string)
        nodes, vertices, elements, connect = parse_mesh_file(file)

    else:
        raise ValueError('{0} is not a recognized mesh type'.format(type))

    mesh = FiniteElementMesh(nodes, vertices, elements, connect)

    if block:
        mesh.ElementBlock(name=block, elements=elements)

    return mesh

class BlockRepository(OrderedDict):
    @property
    def names(self):
        return [v.name for v in self.values()]
    def get_block_by_element(self, element):
        for eb in self.values():
            if element in eb.elements:
                return eb

class ElementBlock:
    def __init__(self, name, elements):
        self.name = name
        self.elements = aslist(elements)

class NodesetRepository(OrderedDict):
    @property
    def names(self):
        return [v.name for v in self.values()]

class Nodeset:
    def __init__(self, name, nodes):
        self.name = name
        self.nodes = np.array(nodes)

class FiniteElementMesh(object):
    def __init__(self, nodes, vertices, elements, connect):
        '''Instantiate the finite element mesh

        Parameters
        ----------
        nodes : list of int
            nodes[i] is the node label of the ith node
        vertices : ndarray of real
            vertices[i] are the nodal coordinates of the ith node
        elements : list of int
            elements[i] is the element label of the ith element
        connect : ndarray
            connect[i, j] is the jth node of the ith element

        '''
        # get nodal coordinates
        self.nodes = aslist(nodes)
        self.vertices = np.asarray(vertices)
        if len(self.vertices.shape) == 1:
            self.dimension = 1 # hard coded for now
        else:
            self.dimension = self.vertices.shape[1]

        # get element connectivity
        self.elements = aslist(elements)
        self.connect = np.asarray(connect)

        # Repository to hold element blocks, sidesets
        self.blocks = BlockRepository()
        self.nodesets = NodesetRepository()

    def dof_map(self, nodes):
        '''Maps node labels to the index (dof) in the vertices array

        Parameters
        ----------
        nodes : array_like or int
            Node labels

        Returns
        -------
        dofs : list of int or int
            The dof numbers corresponding to nodes

        '''
        return smart_index(self.nodes, nodes)

    def element_index(self, elements):
        '''Maps element labels to the index in the connect array

        Parameters
        ----------
        elements : array_like or int
            Element labels

        Returns
        -------
        indices : list of int or int
            The indices to connect corresponding to elements

        '''
        return smart_index(self.elements, elements)

    def connectivity(self, elements=None):
        '''Return element connectivity

        Parameters
        ----------
        elements : array_like of int [None]
            List of elements

        Returns
        -------
        conn : ndarray of int
            The element[s] connectivity
            conn[i] are the nodes of the ith [requested] element

        '''
        if elements is None:
            return self.connect
        connect = self._filter_conn(self.connect[self.element_index(elements)])
        return np.array(connect, dtype=np.int)

    def get_coords(self, label, position):
        if position == ELEMENT:
            i = self.elements.index(label)
            ij = self.dof_map(self.connect[i])
            return midpoint(self.vertices[ij])
        elif position == NODE:
            i = self.nodes.index(label)
            return self.vertices[i]

    @staticmethod
    def _filter_conn(connect):
        if len(connect.shape) == 1:
            return np.array([n for n in connect if n != NONODE])
        return np.array([[n for n in row if n != NONODE] for row in connect])

    @property
    def num_elems_per_block(self):
        return np.array([eb.num_elem for eb in self.element_blocks])

    @property
    def elems_per_block(self):
        '''Return the element IDS of each block

        Returns
        -------
        a : ndarray, (num_elem_blk,)
            a[i] is the number of elements in block i

        Notes
        -----
        Elements are stored contiguously by block.  Using this function,
        the elements in a block can be isolated.

        '''
        return [eb.elements for eb in self.element_blocks]

    @property
    def element_blocks(self):
        return self.blocks.values()

    def num_elem_in_block(self, label):
        return self.blocks[name].num_elem

    def coordinates(self, nodes=None):
        '''Return nodal coordinates

        Parameters
        ----------
        nodes : array_like of int [None]
            List of nodes

        Returns
        -------
        coords : ndarray of real
            The nodal coordinates
            coords[i] are the coordinates of the ith [requested] node

        '''
        if nodes is None:
            nodes = self.nodes
        return self.vertices[self.dof_map(nodes)]

    @property
    def num_node(self):
        return self.vertices.shape[0]

    @property
    def num_elem(self):
        return self.connect.shape[0]

    @property
    def boundary(self):
        return self.vertices[self.dof_map(self.boundary_nodes)]

    @property
    def boundary_nodes(self):
        i, j = np.argmin(self.vertices), np.argmax(self.vertices)
        return [self.nodes[i], self.nodes[j]]

    def extend(self, dx, nx, block=None, order=1):
        '''Extend the current mesh

        Parameters
        ----------
        dx : real
            Length to extend the mesh
        nx : int
           The number of elements

        '''
        dx = float(dx)
        nx = int(nx)
        ox = self.boundary[-1]
        o = max(self.nodes)
        nodes, vertices, elements, connect = genmesh_1d(ox, dx, nx, offset=o,
                                                        order=order)

        # extend the element map and connectivity
        n = connect.shape[1]
        N = self.connect.shape[1]
        if n - N:
            # extending with different element order, fill lesser order with
            # NONODEs
            if n > N:
                a = np.ones(self.connect.shape[0], dtype=np.int32) * NONODE
                self.connect = np.c_[self.connect, a]
            else:
                a = np.ones(connect.shape[0], dtype=np.int32) * NONODE
                connect = np.c_[connect, a]
        self.connect = np.vstack((self.connect, connect))
        self.elements.extend(elements)

        # extend the nodes and vertices
        n = len(nodes) - 1
        self.vertices = np.append(self.vertices, vertices[1:])
        self.nodes.extend(nodes[1:])

        if block:
            self.ElementBlock(name=block, elements=elements)

    def get_node_labels(self, nodes, nodeset, region):
        if all([x is None for x in (nodeset, nodes, region)]):
            # require one
            raise ValueError("1 of [nodeset, nodes, region] keywords required")

        if len([x for x in (nodeset, nodes, region) if x is not None]) > 1:
            raise ValueError("1 of [nodeset, nodes, region] keywords required")

        if nodeset:
            try:
                labels = self.nodesets[nodeset].labels
            except KeyError:
                raise KeyError("{0}: invalid node set ID".format(nodeset))

        elif nodes:
            labels = [node for node in nodes]

        else:
            labels = self.nodes_in_region(region)

        return labels

    def ElementBlock(self, name=None, elements=None):
        '''Assign elements to an element block'''
        if name is None:
            i = 1
            while True:
                name = 'Block-{0}'.format(i)
                if name not in self.blocks:
                    break
                i += 1
                if i > MAX_NUM_SETS:
                    raise ValueError('maximum number of blocks exceeded')
        if name in self.blocks:
            raise ValueError('{0}: element block already exists'.format(name))

        if isstr(elements):
            if elements.lower() == 'all':
                elements = [x for x in self.elements]

            elif elements.lower() == 'unassigned':
                assigned = []
                for elem_blk in self.element_blocks:
                    assigned.extend(elem_blk.elements)
                elements = list(set(self.elements) - set(assigned))
                elements = sorted(elements)

            else:
                raise ValueError('{0}: unrecognized option'.format(elements))

        else:
            elements = np.asarray(elements, dtype=np.int)

        elements = aslist(elements)
        try:
            elconn = self.connectivity(elements)
        except ValueError:
            raise ValueError('cannot assign elements of different '
                             'order to block {0}'.format(name))
        self.blocks[name] = ElementBlock(name, elements)
        return self.blocks[name]

    def Nodeset(self, name=None, nodes=None, region=None, tol=None):
        """Assign nodesets to the mesh"""
        if name is None:
            i = 1
            while True:
                name = "Nodeset-{0}".format(i)
                if name not in self.nodesets:
                    break
                i += 1
                if i > MAX_NUM_SETS:
                    raise AFEPYError("maximum number of nodesets exceeded")
        if name in self.nodesets:
            raise ValueError("{0}: nodeset already exists".format(name))

        ns_nodes = []
        if nodes is not None:
            ns_nodes = [node for node in nodes]

        if region is not None:
            nodes = self.nodes_in_region(region, tol=tol)
            if not nodes:
                raise ValueError("no nodes found in region {0}".format(region))
            ns_nodes.extend(nodes)

        self.nodesets[name] = Nodeset(name, ns_nodes)
        return self.nodesets[name]

    def nodes_in_region(self, region, tol=None):
        found_nodes = []
        #tjfulle: tol should be based on characteristic element length
        tol = tol or 1.E-08
        region = re.sub('(?i)x', 'self.vertices', region)
        mynodes = np.asarray(self.nodes)
        for r in region.split("&&"):
            nodes = set(mynodes[eval('np.where({0})'.format(r))])
            found_nodes.append(nodes)
        nodes = found_nodes[0]
        for others in found_nodes[1:]:
            nodes = nodes & others
        return list(nodes)

    def run_diagnostics(self):
        self.find_orphans()

    def find_orphans(self):
        orphans = []
        for element in self.elements:
            orphaned = True
            for elem_blk in self.element_blocks:
                if element in elem_blk.elements:
                    orphaned = False
                    break
            if orphaned:
                orphans.append(element)
        if orphans:
            o = ', '.join('{0}'.format(x) for x in orphans)
            raise ValueError('Orphaned elements detected.  All elements '
                             'must be assigned to an element block.  '
                             'Orphaned elements:\n {0}'.format(o))

class SingleElementMesh3D(FiniteElementMesh):
    def __init__(self):
        n = [1, 2, 3, 4, 5, 6, 7, 8]
        x = np.array([[-1, -1, -1], [ 1, -1, -1],
                      [ 1,  1, -1], [-1,  1, -1],
                      [-1, -1,  1], [ 1, -1,  1],
                      [ 1,  1,  1], [-1,  1,  1]],
                      dtype=np.float64) * .5
        e = [1]
        c = np.array([range(1, 9)], dtype=np.int)
        super(SingleElementMesh3D, self).__init__(n, x, e, c)
        self.ElementBlock(name='Block-1', elements=e)

    def get_coords(self, label, position):
        if position == NODE:
            return self.vertices[self.nodes.index(label)]
        if position == ELEMENT:
            return [0., 0., 0.]

def genmesh_1d(ox, dx, nx, offset=1, order=1):
    '''Create verticies and connectivity for a linear 1d mesh

    Parameters
    ----------
    ox : real
        Location of origin
    dx : real
        Increment from origin. i.e., the element domain is ox to ox+dx
    nx : int
        Number of elements in the mesh
    order : int, optional [1]
        Element order.  1->linear, 2->quadratic, etc.

    Returns
    -------
    nodes : list of int
        nodes[i] is the node label of the ith node
    vertices : ndarray of real
        vertices[i] are the nodal coordinates of the ith node
    elements : list of int
        elements[i] is the element label of the ith element
    connect : ndarray
        connect[i, j] is the jth node of the ith element

    '''
    # element and node mapping
    if order == 1:
        num_node = nx + 1
        econn = lambda nodes, el: (nodes[el], nodes[el+1])
    elif order == 2:
        num_node = 2 * nx + 1
        econn = lambda nodes, el: (nodes[2*el], nodes[2*el+2], nodes[2*el+1])
    else:
        raise ValueError('unknown mesh order {0}'.format(repr(order)))

    # node map and nodal coordinates
    nodes = [i + offset for i in range(num_node)]
    vertices = np.linspace(ox, ox + dx, num_node)

    # element map and connectivity
    elements = [i + offset for i in range(nx)]
    connect = np.array([econn(nodes, el) for el in range(nx)])

    return nodes, vertices, elements, connect

def index(a, i):
    for (idx, ival) in enumerate(a):
        if i == ival:
            return idx
    raise IndexError('list index out of range')

def smart_index(a, b):
    try:
        return [index(a, i) for i in b]
    except TypeError:
        return index(a, b)

def isstr(a):
    try:
        a + ''
        return True
    except TypeError:
        return False

if __name__ == '__main__':
    f = lambda x: x
    nodes, vertices, elements, connect = genmesh_1d(0, 1, 4)
    print connect
    nodes, vertices, elements, connect = genmesh_1d(0, 1, 4, order=2)
    print connect

    mesh = Mesh(0, 1, 4, block='Block-1')
    mesh.extend(1, 2, order=2, block='Block-2')
    try:
        mesh.ElementBlock(elements='all')
        print 'failed'
    except ValueError:
        # should fail
        print 'success!'
