import numpy as np
from numerix import midpoint
from data import *
from constants import *

__all__ = ['FunctionSpace', 'Function']

class FunctionSpace(object):
    def __init__(self, mesh, dict):
        '''The finite element function space

        Parameters
        ----------
        mesh : Mesh instance
            The mesh
        dict : dict
            dict[name] is the element type of mesh block name

        '''
        if not mesh.blocks:
            # look for special block 'All' in dict
            if len(dict) == 1 and dict.keys()[0].lower() == "all":
                key = dict.keys()[0]
                mesh.ElementBlock(name=key, elements="all")
            else:
                raise ValueError('no blocks assigned to mesh')

        # Create element blocks for the function space, separate from the mesh
        mesh.run_diagnostics()
        self.mesh = mesh
        self.dimension = self.mesh.dimension
        self.elements = np.empty(self.mesh.num_elem, dtype=np.object)
        self.num_elem = self.mesh.num_elem

        # assign properties to elements and determine if all blocks have been
        # assigned properties
        unassigned = []
        self.elems_per_block = []
        for eb in self.mesh.blocks.values():
            try:
                eclass = dict[eb.name]
            except KeyError:
                unassigned.append(eb.name)
                continue

            for element in eb.elements:
                econn = self.mesh.connectivity(element)
                ecoords = self.mesh.coordinates(nodes=econn)
                i = self.mesh.element_index(element)
                self.elements[i] = eclass(element, econn, ecoords)

            eb.num_gauss = self.elements[i].num_gauss
            eb.elem_name = self.elements[i].name
            eb.num_node_per_elem = self.elements[i].num_points
            eb.num_dof_per_node = self.elements[i].num_dof_per_node
            self.elems_per_block.append(eb.elements)

        if unassigned:
            u = ", ".join(unassigned)
            raise ValueError("Element block without properties detected.  "
                             "All element blocks must be assigned properties.  "
                             "Unassigned element blocks:\n {0}".format(u))

        # determine total number of dofs, look for conflicting nodes
        node_dofs = {}
        for e in self.elements:
            i = self.mesh.element_index(e.label)
            n = e.num_dof_per_node
            for node in e.connect:
                try:
                    nn = node_dofs[node]
                    if nn == n:
                        continue
                    raise ValueError("conflicting dofs in node "
                                     "{0}".format(node))
                except KeyError:
                    node_dofs[node] = n
        self.num_dof = sum(node_dofs.values())
        self.num_dof_per_node = self.elements[0].num_dof_per_node
        self.X = self.mesh.vertices.copy()
        self.dofs = np.arange(self.num_dof).reshape(self.mesh.num_node, -1)

    def element_from_label(self, label):
        return self.elements[self.mesh.element_index(label)]

    @property
    def size(self):
        return self.mesh.num_elem

    def int_phi_phi(self, fun=None, derivative=[False, False], coords=None):
        '''Assemble Integrate[fun(x) phi(x) phi(x) dx] or with dphi/dx

        '''
        A = np.zeros((self.num_dof, self.num_dof))

        for element in self.elements:

            # degrees of freedom for this element's nodes
            node_dofs = self.mesh.dof_map(element.connect)

            # coordinates for this element's nodes
            if coords is not None:
                xp = coords[node_dofs]
            else:
                xp = self.X[node_dofs]

            for j in range(element.num_dof):

                # create a function defined on the natural coordinates of the
                # element
                if derivative[1]:
                    # chain rule: dxi/dx = 1 / J
                    f1 = lambda xi: (element.shape.grad(xi) /
                                     element.jacobian(xi, coords=xp))[j]
                else:
                    f1 = lambda xi: element.shape.eval(xi)[j]

                A[node_dofs, node_dofs[j]] += element.integrate(
                    f1=f1, f2=fun, derivative=derivative[0], atgauss=(True, False))

        return A

    def int_phi(self, fun=None, derivative=False, coords=None):
        '''Assemble B = Integrate[fun(x) phi(x) dx or with dphi/dx

        '''
        B = np.zeros(self.num_dof)
        # loop over elements
        for element in self.elements:
            # coordinates for this element's nodes
            node_dofs = self.mesh.dof_map(element.connect)
            if coords is not None:
                xp = coords[node_dofs]
            else:
                xp = self.X[node_dofs]
            B[node_dofs] += element.integrate(
                f1=fun, derivative=derivative, coords=xp)
        return B

class Function:
    def __init__(self, V):
        self.V = V
        self.x = V.X.copy()
        self.vector = np.zeros_like(self.x)

        self.num_node = V.mesh.num_node
        self.num_elem = V.mesh.num_elem

        self.time = 0.
        self.increment = 1.
        self.steps = StepRepository()

        self.save_step(0, 0)

    def __setitem__(self, index, value):
        self.vector.__setitem__(index, value)
        self.save_step(self.time, self.increment)

    def __getitem__(self, index):
        return self.vector.__getitem__(index)

    def __iadd__(self, value):
        self.vector += value
        self.save_step(self.time, self.increment)
        return self

    def save_step(self, time, increment):
        '''Take a snapshot of the state at the end of this step'''

        self.x += self.vector

        # create a new step
        name = 'Step {0}'.format(len(self.steps))
        step = self.steps.Step(name)
        frame = step.Frame(time, increment)

        vertices = self.V.mesh.vertices
        nodes = self.V.mesh.nodes

        # node data
        fo = frame.FieldOutput('U', VECTOR, NODE, self.V.mesh)
        fo.add_data(nodes, [(a,) for a in self.vector])

        # update element states to end of step
        for eb in self.V.mesh.element_blocks:
            data = {}
            for label in eb.elements:
                el = self.V.element_from_label(label)
                ij = self.V.mesh.dof_map(el.connect)
                el.update(self.vector[ij])
                for (i, v) in enumerate(el.var_names):
                    data.setdefault(v, []).append(el.variables[i])

            # save the outputs
            for (key, values) in data.items():
                field = frame.FieldOutput(key, SCALAR, ELEMENT, self.V.mesh, s=1)
                field.add_data(eb.elements, [(a,) for a in values])

        self.time += increment
        return self

    def alpha(self):
        fo = self.steps.values()[0].frames[0].field_outputs
        return (self.V.mesh.dimension, self.V.mesh.num_node, self.V.mesh.nodes,
                self.V.mesh.vertices, self.V.mesh.num_elem, self.V.mesh.elements,
                self.V.mesh.connect, self.V.mesh.element_blocks, fo)


if __name__ == '__main__':
    from mesh import Mesh
    from element import Element
    mesh = Mesh(type='uniform', ox=0., lx=1., nx=10)
    mesh.ElementBlock(name='Block-1', elements='all')
    mesh.extend(1., 10, block='Block-2')
    V = FunctionSpace(mesh, {'Block-1': Element(type='link2'),
                             'Block-2': Element(type='link2')})
    u = Function(V)
