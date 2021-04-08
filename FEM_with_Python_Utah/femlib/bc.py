import numpy as np
__all__ = ['DirichletBC', 'NeumannBC']

def bcsum(bcs):
    if not bcs:
        return []
    bc = bcs[0].copy()
    for a in bcs[1:]:
        bc += a
    return bc

class BoundaryCondition(object):

    def copy(self):
        return type(self)(copy_from=self)

    def __init__(self, V=None, magnitude=None, dofs=None, nodes=None,
                 nodeset=None, region=None, components=None, copy_from=None):
        if copy_from is not None:
            self.data = [x for x in copy_from.data]
        else:
            if V is None:
                raise ValueError("missing V")
            self.nodes = V.mesh.get_node_labels(nodes, nodeset, region)
            components = self.format_components(V, magnitude, dofs, components)
            self.data = []
            for node in self.nodes:
                for (dof, magnitude) in enumerate(components):
                    if magnitude is None:
                        continue
                    self.data.append([node, dof, magnitude])

    @staticmethod
    def format_components(V, magnitude, dofs, components):
        ndof = V.num_dof_per_node

        if components is not None:
            assert len(components) == ndof
            return np.array(components, dtype=np.float64)

        if magnitude is None:
            magnitude = 0.

        components = np.array([None] * ndof)
        if dofs is None:
            dofs = [0, 1, 2][:ndof]
        elif dofs in (0, 1, 2):
            dofs = [dofs,]
        else:
            dofs = [dof_id(dof) for dof in dofs]
        components[dofs] = magnitude
        return components

    def __iter__(self):
        return iter(self.data)

    def __iadd__(self, other):
        self.data.extend(other.data)
        return self

    def __add__(self, other):
        self.data.extend(other.data)
        return self

    def __radd__(self, other):
        if other:
            self.data.extend(other.data)
        return self

class DirichletBC(BoundaryCondition):
    type = 'dirichlet'

class NeumannBC(BoundaryCondition):
    type = 'neumann'
