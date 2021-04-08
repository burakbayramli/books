from os import remove
from os.path import splitext, isfile
from zipfile import ZipFile, ZIP_DEFLATED
from collections import OrderedDict
import xml.dom.minidom as dom

import numpy as np
from femlib.data import *
from femlib.mesh import Mesh
from femlib.constants import *
from femlib.numerix import asstring, asarray

__all__ = ['File']

__version__ = (0, 1, 0)
doc = dom.Document()

def File(filename, mode='r'):
    if mode not in 'wr':
        raise ValueError('unknown File mode {0}'.format(mode))
    if mode == 'r':
        return DBXFileReader(filename)
    return DBXFileWriter(filename)

class _DBXFile(object):
    mode = None
    def view(self):
        from viewer import launch_viewer
        launch_viewer([self.filename])

    def __del__(self):
        self.close()

    def close(self):
        try: self.zf.close()
        except: pass

class DBXFileWriter(_DBXFile):
    mode = 'w'
    def __init__(self, filename):
        self.filename = filename
        self.zf = ZipFile(filename, compression=ZIP_DEFLATED, mode='w')
        self.initialized = False

    def __lshift__(self, u):
        if not self.initialized:
            self.initialize(*u.alpha())
        self.put_steps(u.steps.values())

    def put_steps(self, steps):
        for step in steps:
            self.put_step(step)
        self.close()

    def initialize(self, dimension, num_node, nodes, vertices,
                   num_elem, elements, connect, element_blocks, fields=None):

        # mesh
        mesh = '<mesh>\n'

        # elements
        n, string = asstring(elements)
        mesh += '  <elements>{0}</elements>\n'.format(string)

        # connectivity
        n, string = asstring(connect)
        mesh += '  <connect offsets="{0}">{1}</connect>\n'.format(n, string)

        # nodes
        string = asstring(nodes, 0)
        mesh += '  <nodes>{0}</nodes>\n'.format(string)

        # vertices
        if len(vertices.shape) == 1:
            vertices = np.reshape(vertices, (vertices.shape[0], 1))
        n, string = asstring(vertices)
        mesh += '  <vertices offsets="{0}">{1}</vertices>\n'.format(n, string)

        # blocks
        mesh += '  <blocks>\n'
        for (i, block) in enumerate(element_blocks, start=1):
            mesh += '    <block name="{0}">\n'.format(block.name)
            string = asstring(block.elements, 0)
            mesh += '      <elements>{0}</elements>\n'.format(string)
            mesh += '    </block>\n'
        mesh += '  </blocks>\n</mesh>'

        self.zf.writestr('root/mesh.xml', mesh)
        self.initialized = True

    def put_step(self, step):
        assert self.initialized
        for (i, frame) in enumerate(step.frames, start=1):
            filename = 'root/steps/{0}/frame-{1}.xml'.format(step.name, i)
            self.put_frame(filename, frame)

    def put_frame(self, filename, frame):
        '''Write the frame to the frame xml file. We roll our own XML
        formatting because it is significantly faster

        '''

        el = '<frame time="{0}" increment="{1}">\n'.format(
            frame.time, frame.increment)

        # store each field
        for (j, fo) in enumerate(frame.field_outputs.values(), start=1):
            # field output
            el += '  <field_output name="{0}" position="{1}" type="{2}"'.format(
                fo.name, fo.position, fo.type)
            if fo.type != SCALAR:
                string = asstring(fo.component_labels, 0)
                el += ' component_labels="{0}"'.format(string)
            if fo.valid_invariants:
                string = asstring(fo.valid_invariants, 0)
                el += ' valid_invariants="{0}"'.format(string)
            el += '>\n'

            # --- data and labels
            string = asstring(fo.labels, 0)
            el += '    <labels>{0}</labels>\n'.format(string)
            n, string = asstring(fo.data)
            el += '    <data offsets="{0}">{1}</data>\n'.format(n, string)
            el += '  </field_output>\n'

        el += '</frame>'
        self.zf.writestr(filename, el)

        return

class DBXFileReader(_DBXFile):
    mode = 'r'
    def __init__(self, filename):
        if not isfile(filename):
            raise IOError('no such file: {0}'.format(repr(filename)))
        self.filename = filename
        self.zf = ZipFile(filename, compression=ZIP_DEFLATED, mode='r')
        self.read()

    def read(self):

        # --- read in the mesh
        doc = dom.parseString(self.zf.read('root/mesh.xml'))
        mesh = doc.getElementsByTagName('mesh')[0]

        # nodes and vertices
        el = mesh.getElementsByTagName('nodes')[0]
        nodes = asarray(el.firstChild.data, dtype=int)

        el = mesh.getElementsByTagName('vertices')[0]
        n = int(el.getAttribute('offsets'))
        vertices = asarray(el.firstChild.data, offsets=n)

        # elements and their connectivity
        el = mesh.getElementsByTagName('elements')[0]
        elements = asarray(el.firstChild.data, dtype=int)

        el = mesh.getElementsByTagName('connect')[0]
        n = int(el.getAttribute('offsets'))
        connect = asarray(el.firstChild.data, offsets=n, dtype=int)

        self.mesh = Mesh(type='free', nodes=nodes, vertices=vertices,
                         elements=elements, connect=connect)

        self._steps = None
        self._step_files = None

    def get_vertices(self):
        return self.mesh.vertices

    def get_nodes(self):
        return self.mesh.nodes

    @property
    def steps(self):
        if self._steps is None:
            self._steps = self.get_steps()
        return self._steps

    @property
    def step_files(self):
        if self._step_files is not None:
            return self._step_files

        def sort(item):
            return item.date_time

        # step/frame files
        l = [i for i in self.zf.infolist() if i.filename.startswith('root/steps')]

        # sorted by creation time
        files = [f.filename.rsplit('/',1)[0]
                 for f in sorted(l, key=lambda f: sort(f))]
        seen = set()
        self._step_files = [f for f in files if not (f in seen or seen.add(f))]
        return self._step_files

    def get_steps(self):
        if self._steps is not None:
            return self._steps

        steps = StepRepository()
        for (i, file) in enumerate(self.step_files):
            step = self._get_step(file)
            if steps:
                # insert the last frame of previous step as
                # this steps initial frame
                iframe = steps.values()[-1].frames[-1]
                step.frames.insert(0, iframe)
            steps[step.name] = step
        return steps

    def _get_step(self, file):
        '''Read the step from the output database.  This is essentially the
        reverse operation of put_step

        '''
        name = file.split('/')[-1].capitalize()
        step = Step(name)
        iframe = 1
        while 1:
            try:
                filename = '{0}/frame-{1}.xml'.format(file, iframe)
                doc = dom.parseString(self.zf.read(filename))
            except KeyError:
                break

            elx = doc.getElementsByTagName('frame'.format(iframe))[0]
            time = float(elx.getAttribute('time'))
            increment = float(elx.getAttribute('increment'))
            frame = step.Frame(time, increment)
            for el in elx.getElementsByTagName('field_output'):
                name = el.getAttribute('name')
                position = int(el.getAttribute('position'))
                dtype = int(el.getAttribute('type'))
                if dtype == ARRAY:
                    clabels = el.getAttribute('component_labels').split(' ')
                else:
                    clabels = None

                # invariants
                d = el.getAttribute('valid_invariants')
                invkeys = None if not d else asarray(d, dtype=int)

                # --- labels and data
                d = el.getElementsByTagName('labels')[0].firstChild.data
                labels = asarray(d, dtype=int)
                el1 = el.getElementsByTagName('data')[0]
                try:
                    n = int(el1.getAttribute('offsets'))
                    data = asarray(el1.firstChild.data, offsets=n)
                except AttributeError:
                    # no data for this field
                    continue
                field = frame.FieldOutput(name, dtype, position, self.mesh,
                    valid_invariants=invkeys, component_labels=clabels, mode='r')
                field.add_data(labels, data)

            iframe += 1

        return step

if __name__ == '__main__':
    from mesh import Mesh
    from funspace import FunctionSpace, Function
    from element import Element
    mesh = Mesh(type='uniform', ox=0., lx=1., nx=10)
    mesh.ElementBlock(name='Block-1', elements='all')
    mesh.extend(1., 10, block='Block-2')
    V = FunctionSpace(mesh, {'Block-1': Element(type='link2'),
                             'Block-2': Element(type='link2')})
    u = Function(V)
    u += np.linspace(1., 10., len(u.vector))
    f = File('myfile', mode='w')
    f << u

    f = File('myfile')
    nodes, vertices = f.get_nodes()

    time = f.steps['Step 1'].frames[-1].value
    print time
    fo = f.get_field_output(1, -1)
    u = fo['U']
    print u.get_data(sort=True)
    e = fo['E']
    print e.get_data(sort=True)
