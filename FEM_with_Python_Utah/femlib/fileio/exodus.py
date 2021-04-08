import re
import os
import datetime
import numpy as np
from os.path import basename, join, splitext, isfile

from _netcdf import NetCDFFile
from femlib.constants import *
from femlib.data import Step, StepRepository
from femlib.mesh import Mesh
from femlib.numerix import aslist, asarray, asstring

__all__ = ['File']

def cat(*args):
    return ''.join(str(a).strip() for a in args)

def adjstr(string):
    return '{0:32s}'.format(string)[:32]

def stringify(a):
    try:
        return ''.join(a).strip()
    except TypeError:
        return [''.join(row).strip() for row in a]

# --- Data types
FLOAT = 'f'
CHAR = 'c'
INT = 'i'

# --- Global dimensions and variables
DIM_LEN_STRING     = 'len_string'
DIM_LEN_LINE       = 'len_line'
DIM_FOUR           = 'four'
DIM_NUM_DIM        = 'num_dim'
DIM_NUM_QA         = 'num_qa_rec'
DIM_TIME_STEP      = 'time_step'
DIM_MAX_STEPS      = 'max_steps'
VAR_TIME_WHOLE     = 'time_whole'
VAR_QA_RECORDS     = 'qa_records'
VAR_COOR_NAMES     = 'coor_names'
VAR_COOR_NAME      = lambda i: cat('coord', 'xyz'[i])

# --- Element dimensions and variables
DIM_NUM_ELEM       = 'num_elem'
DIM_NUM_ELEM_VAR   = 'num_elem_var'
DIM_NUM_EL_BLK     = 'num_el_blk'
VAR_ELEMENTS       = 'elements'
VAR_NAME_ELEM_VAR  = 'name_elem_var'
VAR_EB_PROP1       = 'eb_prop1'
VAR_EB_STATUS      = 'eb_status'
VAR_EB_NAMES       = 'eb_names'
VAR_ELEM_MAP       = lambda i: cat('elem_map', i)
DIM_NUM_EL_IN_BLK  = lambda i: cat('num_el_in_blk', i)
DIM_NUM_NOD_PER_EL = lambda i: cat('num_nod_per_el', i)
VAR_CONNECT        = lambda i: cat('connect', i)
VALS_ELEM_VAR      = lambda i, j: cat('vals_elem_var', i, 'eb', j)

# --- Node dimensions and variables
DIM_NUM_NODES      = 'num_nodes'
DIM_NUM_NOD_VAR    = 'num_nod_var'
VAR_NODES          = 'nodes'
VAR_NAME_NOD_VAR   = 'name_nod_var'
VALS_NOD_VAR       = lambda i: cat('vals_nod_var', i)

# --- Node set dimensons and variables
DIM_NUM_NODE_SETS  = 'num_node_sets'
DIM_NUM_NOD_NS     = lambda i: cat('num_nod_ns', i)
VAR_NS_PROP1       = 'ns_prop1'
VAR_NS_NAMES       = 'ns_names'
VAR_NODE_NS        = lambda i: cat('node_ns', i)

# --- Global variable dimensions and variables
DIM_NUM_GLO_VAR    = 'num_glo_var'
VALS_GLO_VAR       = 'vals_glo_var'

# --- Side set dimensions and variables
DIM_NUM_SIDE_SETS  = 'num_side_sets'
DIM_NUM_SIDE_SS    = lambda i: cat('num_side_ss', i)
VAR_SS_PROP1       = 'ss_prop1'
VAR_SS_NAMES       = 'ss_names'
VAR_SIDE_SS        = lambda i: cat('side_ss', i)
VAR_ELEM_SS        = lambda i: cat('elem_ss', i)

# --- Field variable dimensions and variables (femlib specific)
DIM_NUM_FIELD      = 'num_field'
VAR_STEP_NUM       = 'step_num'
VAR_STEP_NAMES     = 'step_names'
VAR_FIELD_ELEM_VAR = 'field_elem_var'
VAR_FIELD_NOD_VAR  = 'field_nod_var'
VAR_FO_PROP1       = 'fo_prop1'
VAR_FO_NAMES       = 'fo_names'
VAR_FO_TYPES       = 'fo_types'
VAR_FO_VALINV      = 'fo_valinv'

VALID_INVARIANTS   = 'valid_invariants'
POSITION           = 'position'
TYPE               = 'type'
NAME               = 'name'
DATA               = 'data'
LABELS             = 'labels'
COMPONENT_LABELS   = 'component_labels'

def File(filename, mode='r'):
    if mode not in 'wr':
        raise ValueError('unknown File mode {0}'.format(mode))
    if mode == 'r':
        return EXOFileReader(filename)
    return EXOFileWriter(filename)

class _EXOFile(object):
    mode = None
    def view(self):
        from viewer import launch_viewer
        launch_viewer([self.filename])

    def close(self):
        self.fh.close()

class EXOFileWriter(_EXOFile):
    mode = 'w'
    def __init__(self, filename):
        '''
        Notes
        -----
        The EXOFile class is an interface to the Exodus II api. Its methods
        are named after the analogous method from the Exodus II C bindings,
        minus the prefix 'ex_'.

        '''
        self.fh = NetCDFFile(filename, mode='w')
        self.jobid = splitext(basename(filename))[0]
        self.filename = filename

        self.initialized = False
        self.viewable = False

    def update(self):
        pass

    def __lshift__(self, u):
        if not self.initialized:
            self.initialize(*u.alpha())
        for step in u.steps.values():
            self.put_step(step)
        self.close()

    def initialize(self, dimension, num_node, nodes, vertices,
                   num_elem, elements, connect, element_blocks, field_outputs,
                   node_sets=None, side_sets=None):
        '''Writes the initialization parameters to the EXODUS II file'''

        # ------------------------------------------------------------------- #
        # -------------------------------- standard ExodusII dimensioning --- #
        # ------------------------------------------------------------------- #
        self.fh.floating_point_word_size = 4
        self.fh.version = 5.0300002
        self.fh.file_size = 1
        self.fh.api_version = 5.0300002
        self.fh.title = 'finite element simulation'

        self.fh.filename = basename(self.filename)
        self.fh.jobid = self.jobid

        self.fh.createDimension(DIM_LEN_STRING, 33)
        self.fh.createDimension(DIM_LEN_LINE, 81)
        self.fh.createDimension(DIM_FOUR, 4)

        self.fh.createDimension(DIM_NUM_DIM, dimension)
        self.fh.createDimension(DIM_NUM_NODES, num_node)
        self.fh.createDimension(DIM_NUM_ELEM, num_elem)

        # node and element number maps
        self.fh.createVariable(VAR_NODES, INT, (DIM_NUM_NODES,))
        self.fh.variables[VAR_NODES][:] = nodes
        self.fh.createVariable(VAR_ELEMENTS, INT, (DIM_NUM_ELEM,))
        self.fh.variables[VAR_ELEMENTS][:] = elements

        # ------------------------------------------------------------------- #
        # ---------------------------------------------------- QA records --- #
        # ------------------------------------------------------------------- #
        now = datetime.datetime.now()
        day = now.strftime("%m/%d/%y")
        hour = now.strftime("%H:%M:%S")
        self.fh.createDimension(DIM_NUM_QA, 1)
        self.fh.createVariable(VAR_QA_RECORDS, CHAR,
                               (DIM_NUM_QA, DIM_FOUR, DIM_LEN_STRING))
        self.fh.variables[VAR_QA_RECORDS][0, 0, :] = adjstr('femlib')
        self.fh.variables[VAR_QA_RECORDS][0, 1, :] = adjstr(self.jobid)
        self.fh.variables[VAR_QA_RECORDS][0, 2, :] = adjstr(day)
        self.fh.variables[VAR_QA_RECORDS][0, 3, :] = adjstr(hour)

        # ------------------------------------------------------------------- #
        # ------------------------------------------------- record arrays --- #
        # ------------------------------------------------------------------- #
        self.fh.createDimension(DIM_TIME_STEP, None)
        self.fh.createVariable(VAR_TIME_WHOLE, FLOAT, (DIM_TIME_STEP,))
        self.fh.createVariable(VAR_STEP_NUM, INT, (DIM_TIME_STEP,))
        self.fh.createDimension(DIM_MAX_STEPS, 100) # arbitrary number
        self.fh.createVariable(VAR_STEP_NAMES, CHAR,
                               (DIM_MAX_STEPS, DIM_LEN_STRING))

        self.step_count = 0

        # ------------------------------------------------------------------- #
        # ------------------------------------------------- field outputs --- #
        # ------------------------------------------------------------------- #
        fields = field_outputs.values()

        nev = sum([len(fo.keys) for fo in fields if fo.position==ELEMENT])
        self.fh.createDimension(DIM_NUM_ELEM_VAR, nev)
        self.fh.createVariable(VAR_NAME_ELEM_VAR, CHAR,
                               (DIM_NUM_ELEM_VAR, DIM_LEN_STRING))
        self.fh.createVariable(VAR_FIELD_ELEM_VAR, INT, (DIM_NUM_ELEM_VAR,))

        nnv = sum([len(fo.keys) for fo in fields if fo.position==NODE])
        self.fh.createDimension(DIM_NUM_NOD_VAR, nnv)
        self.fh.createVariable(VAR_NAME_NOD_VAR, CHAR,
                               (DIM_NUM_NOD_VAR, DIM_LEN_STRING))
        self.fh.createVariable(VAR_FIELD_NOD_VAR, INT, (DIM_NUM_NOD_VAR,))

        self.fh.createDimension(DIM_NUM_FIELD, len(fields))
        self.fh.createVariable(VAR_FO_PROP1, INT, (DIM_NUM_FIELD,))
        self.fh.createVariable(VAR_FO_NAMES, CHAR,
                               (DIM_NUM_FIELD, DIM_LEN_STRING))
        self.fh.createVariable(VAR_FO_TYPES, INT, (DIM_NUM_FIELD,))
        self.fh.createVariable(VAR_FO_VALINV, CHAR,
                               (DIM_NUM_FIELD, DIM_LEN_STRING))
        self.fh.variables[VAR_FO_PROP1][:] = np.arange(len(fields)) + 1

        i = j = 0
        for (n, fo) in enumerate(fields):
            self.fh.variables[VAR_FO_NAMES][n, :] = adjstr(fo.name)
            self.fh.variables[VAR_FO_TYPES][n] = fo.type
            string = adjstr(asstring(fo.valid_invariants, 0))
            self.fh.variables[VAR_FO_VALINV][n] = string
            for key in fo.keys:
                key = adjstr(key)
                if fo.position == ELEMENT:
                    self.fh.variables[VAR_NAME_ELEM_VAR][i, :] = key
                    self.fh.variables[VAR_FIELD_ELEM_VAR][i] = n
                    i += 1
                elif fo.position == NODE:
                    self.fh.variables[VAR_NAME_NOD_VAR][j, :] = key
                    self.fh.variables[VAR_FIELD_NOD_VAR][j] = n
                    j += 1
                else:
                    raise ValueError('unknown field output position')

        # ------------------------------------------------------------------- #
        # -------------------------------------------- node set meta data --- #
        # ------------------------------------------------------------------- #
        nns = 0 if node_sets is None else len(node_sets)
        if nns:
            self.fh.createDimension(DIM_NUM_NODE_SETS, nns)
            # node set IDs - standard map
            prop1 = np.arange(nns, dtype=np.int32) + 1
            self.fh.createVariable(VAR_NS_PROP1, INT, (DIM_NUM_NODE_SETS,))
            self.fh.variables[VAR_NS_PROP1][:] = prop1
            self.fh.variables[VAR_NS_PROP1].name = 'ID'
            self.fh.createVariable(VAR_NS_NAMES, CHAR,
                                   (DIM_NUM_NODE_SETS, DIM_LEN_STRING))
            for (i, ns) in enumerate(node_sets, start=1):
                self.fh.variables[VAR_NS_NAMES][i-1][:] = adjstr(ns.name)
                self.fh.createDimension(DIM_NUM_NOD_NS(i), len(ns.nodes))
                self.fh.createVariable(VAR_NODE_NS(i), INT, (DIM_NUM_NOD_NS(i),))
                self.fh.variables[VAR_NODE_NS(i)][:] = ns.nodes

        # ------------------------------------------------------------------- #
        # -------------------------------------------- side set meta data --- #
        # ------------------------------------------------------------------- #
        nss = 0 if side_sets is None else len(side_sets)
        if nss:
            self.fh.createDimension(DIM_NUM_SIDE_SETS, nss)
            # side set IDs - standard map
            prop1 = np.arange(nss, dtype=np.int32) + 1
            self.fh.createVariable(VAR_SS_PROP1, INT, (DIM_NUM_SIDE_SETS,))
            self.fh.variables[VAR_SS_PROP1][:] = prop1
            self.fh.variables[VAR_SS_PROP1].name = 'ID'
            self.fh.createVariable(VAR_SS_NAMES, CHAR,
                                   (DIM_NUM_SIDE_SETS, DIM_LEN_STRING))
            for (i, ss) in enumerate(side_sets, start=1):
                self.fh.variables[VAR_SS_NAMES][i-1][:] = adjstr(ss.name)
                self.fh.createDimension(DIM_NUM_SIDE_SS(i), len(ss.sides))
                self.fh.createVariable(VAR_SIDE_SS(i), INT, (DIM_NUM_SIDE_SS(i),))
                self.fh.variables[VAR_SIDE_SS(i)][:] = ss.sides
                self.fh.createVariable(VAR_ELEM_SS(i), INT, (DIM_NUM_ELEM_SS(i),))
                self.fh.variables[VAR_ELEM_SS(i)][:] = ss.elements

        # ------------------------------------------------------------------- #
        # --------------------------------------- element block meta data --- #
        # ------------------------------------------------------------------- #
        # block IDs - standard map
        num_el_blk = len(element_blocks)
        self.fh.createDimension(DIM_NUM_EL_BLK, num_el_blk)

        prop1 = np.arange(num_el_blk, dtype=np.int32) + 1
        self.fh.createVariable(VAR_EB_PROP1, INT, (DIM_NUM_EL_BLK,))
        self.fh.variables[VAR_EB_PROP1][:] = prop1
        self.fh.variables[VAR_EB_PROP1].name = 'ID'

        self.fh.createVariable(VAR_EB_STATUS, INT, (DIM_NUM_EL_BLK,))
        self.fh.variables[VAR_EB_STATUS][:] = np.ones(num_el_blk, dtype=int)

        self.fh.createVariable(VAR_EB_NAMES, CHAR, (DIM_NUM_EL_BLK, DIM_LEN_STRING))
        for (i, block) in enumerate(element_blocks, start=1):
            self.fh.variables[VAR_EB_NAMES][i-1][:] = adjstr(block.name)

            # block connect
            ij = [elements.index(e) for e in block.elements]
            block_conn = np.array([[n for n in c if n >= 0] for c in connect[ij]])

            ne, nn = block_conn.shape
            d1, d2 = DIM_NUM_EL_IN_BLK(i), DIM_NUM_NOD_PER_EL(i)
            self.fh.createDimension(d1, ne)
            self.fh.createDimension(d2, nn)

            # element map
            elem_map = VAR_ELEM_MAP(i)
            self.fh.createVariable(elem_map, INT, (d1,))
            self.fh.variables[elem_map][:] = block.elements

            # set up the element block connectivity
            self.fh.createVariable(VAR_CONNECT(i), INT, (d1, d2))
            self.fh.variables[VAR_CONNECT(i)][:] = block_conn

            # element type
            if dimension == 1:
                elem_type = 'TRUSS'
            elif dimension == 2:
                if nn == 3:
                    elem_type = 'TRI'
                elif nn == 4:
                    elem_type = 'QUAD'
            elif dimension == 3:
                if nn in (4, 6):
                    elem_type = 'TET'
                elif nn in (8, 20):
                    elem_type = 'HEX'

            self.fh.variables[VAR_CONNECT(i)].elem_type = elem_type

            for j in range(1, nev+1):
                self.fh.createVariable(VALS_ELEM_VAR(j,i),
                                       FLOAT, (DIM_TIME_STEP, d1))

        # ------------------------------------------------------------------- #
        # ------------------------------------------------ node meta data --- #
        # ------------------------------------------------------------------- #
        if len(vertices.shape) == 1:
            vertices = np.reshape(vertices, (num_node, dimension))
        self.fh.createVariable(VAR_COOR_NAMES, CHAR, (DIM_NUM_DIM, DIM_LEN_STRING))
        for i in range(dimension):
            self.fh.variables[VAR_COOR_NAMES][i][:] = adjstr(VAR_COOR_NAME(i))
            self.fh.createVariable(VAR_COOR_NAME(i), FLOAT, (DIM_NUM_NODES,))
            self.fh.variables[VAR_COOR_NAME(i)][:] = vertices[:, i]

        for j in range(1, nnv+1):
            self.fh.createVariable(VALS_NOD_VAR(j), FLOAT,
                                   (DIM_TIME_STEP, DIM_NUM_NODES))

        # ------------------------------------------------------------------- #
        # ------------------------------------ global variables meta data --- #
        # ------------------------------------------------------------------- #
        self.fh.createDimension(DIM_NUM_GLO_VAR, 1)
        self.fh.createVariable(VALS_GLO_VAR, FLOAT, (DIM_TIME_STEP, ))

        self.initialized = True
        return

    def put_steps(self, steps):
        for step in steps:
            self.put_step(step)
        self.close()

    def put_step(self, step):

        assert self.initialized

        self.fh.variables[VAR_STEP_NAMES][self.step_count] = adjstr(step.name)

        for frame in step.frames:

            # write time value
            count = len(self.fh.variables[VAR_TIME_WHOLE].data)
            self.fh.variables[VAR_TIME_WHOLE][count] = frame.value
            self.fh.variables[VAR_STEP_NUM][count] = self.step_count
            self.fh.variables[VALS_GLO_VAR][count] = 0.

            # get node and element fields
            fields = frame.field_outputs.values()
            fo_n = [fo for fo in fields if fo.position==NODE]
            fo_e = [fo for fo in fields if fo.position==ELEMENT]

            # write out node fields
            a = stringify(self.fh.variables[VAR_NAME_NOD_VAR])
            for fo in fo_n:
                data = fo.get_data()
                if len(data.shape) == 1:
                    data = data.reshape(-1, 1)
                for (k, key) in enumerate(fo.keys):
                    i = a.index(key) + 1
                    self.fh.variables[VALS_NOD_VAR(i)][count] = data[:, k]

            # write element data to the appropriate element block
            a = stringify(self.fh.variables[VAR_NAME_ELEM_VAR])
            ebs = stringify(self.fh.variables[VAR_EB_NAMES])
            for fo in fo_e:
                for (j, eb) in enumerate(ebs, start=1):
                    bd = fo.get_data(block=eb)
                    if len(bd.shape) == 1:
                        bd = bd.reshape(-1, 1)
                    for (k, key) in enumerate(fo.keys):
                        i = a.index(key) + 1
                        self.fh.variables[VALS_ELEM_VAR(i,j)][count] = bd[:, k]

        self.step_count += 1
        return

class EXOFileReader(_EXOFile):
    mode = 'r'
    def __init__(self, filename):
        if not isfile(filename):
            raise IOError('no such file: {0}'.format(repr(filename)))
        self.filename = filename
        self.fh = NetCDFFile(filename, mode='r')
        self.read()

    def read(self):

        # --- read in the mesh
        # nodes and vertices
        nodes = self.fh.variables[VAR_NODES][:].tolist()
        dimension = self.fh.dimensions[DIM_NUM_DIM]
        vertices = np.empty((len(nodes), dimension))
        for i in range(self.fh.dimensions[DIM_NUM_DIM]):
            vertices[:, i] = self.fh.variables[VAR_COOR_NAME(i)][:]

        # elements, blocks, connectivity
        blocks = {}
        num_el_blk = self.fh.dimensions[DIM_NUM_EL_BLK]
        elements = self.fh.variables[VAR_ELEMENTS][:].tolist()
        emap = dict([(j, i) for (i, j) in enumerate(elements)])
        j = max(self.fh.dimensions[DIM_NUM_NOD_PER_EL(i+1)]
                for i in range(num_el_blk))
        connect = -np.ones((self.fh.dimensions[DIM_NUM_ELEM], j))
        for i in range(num_el_blk):
            name = stringify(self.fh.variables[VAR_EB_NAMES][i])
            els = self.fh.variables[VAR_ELEM_MAP(i+1)][:]
            j = [emap[e] for e in els]
            connect[j] = self.fh.variables[VAR_CONNECT(i+1)][:]
            blocks[name] = els

        # instantiate the mesh
        self.mesh = Mesh(type='free', nodes=nodes, vertices=vertices,
                         elements=elements, connect=connect)
        for (name, elements) in blocks.items():
            self.mesh.ElementBlock(name, elements=elements)

        self._steps = None

    def get_vertices(self):
        return self.mesh.vertices

    def get_nodes(self):
        return self.mesh.nodes

    @property
    def steps(self):
        if self._steps is None:
            self._steps = self.get_steps()
        return self._steps

    def get_steps(self):
        '''Read the step from the output database.  This is essentially the
        reverse operation of put_step

        '''
        steps = StepRepository()
        times = self.fh.variables[VAR_TIME_WHOLE][:]

        # set up step counting
        step_num = self.fh.variables[VAR_STEP_NUM][:].tolist()
        step_names = stringify(self.fh.variables[VAR_STEP_NAMES])

        for count in range(times.shape[0]):

            step_name = step_names[step_num[count]]
            if step_name not in steps:
                step = Step(step_name)
                if steps:
                    step.frames.append(steps.values()[-1].frames[-1])
                steps[step.name] = step

            time = 0. if count == 0 else times[count-1]
            increment = times[count] - time
            frame = step.Frame(time, increment)

            # get field data
            fields = {}
            for i in range(self.fh.dimensions[DIM_NUM_FIELD]):
                a = asarray(stringify(self.fh.variables[VAR_FO_VALINV][i]),
                            dtype=int)
                fields[i] = {NAME:stringify(self.fh.variables[VAR_FO_NAMES][i]),
                             TYPE: self.fh.variables[VAR_FO_TYPES][i],
                             VALID_INVARIANTS: a}

            # element data
            for i in range(self.fh.dimensions[DIM_NUM_EL_BLK]):
                block = stringify(self.fh.variables[VAR_EB_NAMES][i])
                labels = self.mesh.blocks[block].elements
                for j in range(self.fh.dimensions[DIM_NUM_ELEM_VAR]):
                    name = stringify(self.fh.variables[VAR_NAME_ELEM_VAR][j])
                    field = self.fh.variables[VAR_FIELD_ELEM_VAR][j]
                    fields[field][POSITION] = ELEMENT

                    data = self.fh.variables[VALS_ELEM_VAR(j+1,i+1)][count, :]
                    if i == 0:
                        fields[field].setdefault(DATA, []).append(data)
                        fields[field][LABELS] = aslist(labels)
                        if fields[field][TYPE] != SCALAR:
                            c = name.rsplit('_', 1)[1]
                            fields[field].setdefault(COMPONENT_LABELS, []).append(c)
                    else:
                        if fields[field][TYPE] != SCALAR:
                            c = name.rsplit('_', 1)[1]
                            k = fields[field][COMPONENT_LABELS].index(c)
                        else:
                            k = 0
                        d = fields[field][DATA][k]
                        fields[field][DATA][k] = np.append(d, data)
                        fields[field][LABELS].extend(labels)

            # node data
            for i in range(self.fh.dimensions[DIM_NUM_NOD_VAR]):
                name = stringify(self.fh.variables[VAR_NAME_NOD_VAR][i])
                field = self.fh.variables[VAR_FIELD_NOD_VAR][i]
                fields[field][POSITION] = NODE

                if fields[field][TYPE] != SCALAR:
                    c = name.rsplit('_', 1)[1]
                    fields[field].setdefault(COMPONENT_LABELS, []).append(c)

                data = self.fh.variables[VALS_NOD_VAR(i+1)][count, :]
                fields[field].setdefault(DATA, []).append(data)
                fields[field][LABELS] = self.mesh.nodes

            for (n, info) in fields.items():
                info[DATA] = np.column_stack(info[DATA])
                field = frame.FieldOutput(info[NAME], info[TYPE],
                                  info[POSITION], self.mesh,
                                  valid_invariants=info[VALID_INVARIANTS],
                                  component_labels=info.get(COMPONENT_LABELS),
                                  mode='r')
                field.add_data(info[LABELS], info[DATA])

        return steps

if __name__ == '__main__':
    from femlib.mesh import Mesh
    from femlib.funspace import FunctionSpace, Function
    from femlib.element import Element
    mesh = Mesh(type='uniform', ox=0., lx=1., nx=10)
    mesh.ElementBlock(name='Block-1', elements='all')
    mesh.extend(1., 10, block='Block-2')
    V = FunctionSpace(mesh, {'Block-1': Element(type='link2'),
                             'Block-2': Element(type='link2')})
    u = Function(V)
    u += np.linspace(1., 10., len(u.vector))
    field_outputs = u.steps.values()[0].frames[-1].field_outputs
    f = File('myfile', mode='w')
    f.initialize(mesh.dimension, mesh.num_node, mesh.nodes, mesh.vertices,
                 mesh.num_elem, mesh.elements, mesh.connect, mesh.element_blocks,
                 field_outputs)
    for step in u.steps.values():
        f.put_step(step)

    f.close()

    f = File('myfile', mode='r')
    steps = f.get_steps()
    for step in steps.values():
        for frame in step.frames:
            print step.name, frame.time, frame.increment
