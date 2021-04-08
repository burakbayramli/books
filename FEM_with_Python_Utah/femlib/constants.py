import numpy as np

# Field types
SCALAR = 0
VECTOR = 1
TENSOR_3D = 2
NUM_TENSOR_3D = 6
TENSOR_3D_FULL = 3
NUM_TENSOR_3D_FULL = 9
ARRAY = 4
FIELD_TYPES = {SCALAR: 'scalar', VECTOR: 'vector', TENSOR_3D: 'tensor_3D',
               TENSOR_3D_FULL: 'tensor_3D_full', ARRAY: 'array'}

def COMPONENT_LABELS(type, dimension=None):
    if type == VECTOR:
        return ('X', 'Y', 'Z')[:dimension]
    elif type == TENSOR_3D:
        return ('XX', 'YY', 'ZZ', 'XY', 'YZ', 'XZ')
    elif type == TENSOR_3D_FULL:
        return ('XX', 'XY', 'XZ', 'YX', 'YY', 'YZ', 'ZX', 'ZY', 'ZZ')

# Field positions
NODE = 0
ELEMENT = 1
INTEGRATION_POINT = 2
FIELD_POSITIONS = {NODE: 'node', ELEMENT: 'element',
                   INTEGRATION_POINT: 'integration point'}

# Valid invariants
MAGNITUDE = 0
MISES = 1
PRES = 2
EQ = 3
V = 4
INVARIANTS = {MAGNITUDE: 'magnitude', PRES: 'pres', EQ: 'equiv', V: 'V',
              MISES: 'mises'}

# Misc. constant arrays and scalars
Z6 = np.zeros(6)
I6 = np.array([1., 1., 1., 0., 0., 0.])
I9 = np.array([1., 0., 0., 0., 1., 0., 0., 0., 1.])
VOIGHT = np.array([1, 1, 1, 2, 2, 2], dtype=np.float64)

DEFAULT_TEMP = 298.

ROOT2 = np.sqrt(2.0)
ROOT3 = np.sqrt(3.0)
TOOR2 = 1.0 / ROOT2
TOOR3 = 1.0 / ROOT3
ROOT23 = ROOT2 / ROOT3
