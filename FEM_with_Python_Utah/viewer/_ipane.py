import numpy as np
from traits.api import *
from traitsui.api import *
from traitsui.message import error, message
from pyface.api import FileDialog, OK as pyOK
from traitsui.menu import Menu, Action, Separator
from os.path import realpath, basename, dirname

from femlib.fileio import File
from femlib.fileio.dbx import DBXFileReader
from femlib.fileio.exodus import EXOFileReader

class Frame(HasTraits):
    name = Str
    def __init__(self, name, index, parent):
        super(Frame, self).__init__(name=name, index=index, parent=parent)
    def dclicked(self):
        self.parent.frame_dclicked(self)

class Step(HasTraits):
    name = Str
    frames = List(Frame)
    def __init__(self, name, num_frame, parent):
        name_ = lambda i: 'Frame {0}'.format(i)
        frames = [Frame(name_(i), i, self) for i in range(num_frame)]
        super(Step, self).__init__(name=name, frames=frames, parent=parent)
    def dclicked(self):
        self.parent.step_dclicked(self.name, 0)
    def frame_dclicked(self, frame):
        self.parent.step_dclicked(self.name, frame.index)

class OutputDB(HasTraits):
    id = Int
    name  = Str
    filename = Str
    steps = List(Step)
    step_num = Int(0)
    hidden = Bool
    def __init__(self, filename, parent=None):
        db = File(realpath(filename), mode='r')
        filename = db.filename
        name = basename(filename)
        steps = []
        for step in db.steps.values():
            steps.append(Step(step.name, len(step.frames), self))
        kwds = {'name': name, 'filename': filename, 'db': db,
                'steps': steps, 'id': hashf(db.filename), 'hidden': False}
        super(OutputDB, self).__init__(**kwds)
        self.parent = parent

    def reload_data(self):
        self.db = File(self.filename)
        steps = []
        for step in self.db.steps.values():
            steps.append(Step(step.name, len(step.frames)))
        self.steps = steps

    @property
    def choices(self):
        choices = []
        for step in self.db.steps.values():
            for frame in step.frames:
                choices.extend([k for k in frame.field_outputs.keys()
                                if k not in choices])
        return choices

    def step_dclicked(self, name, j=None):
        self.parent.update(displayed=self.filename, step=name, frame=j)

    def dclicked(self):
        self.parent.update(displayed=self.filename)

def read(filename):
    return File(filename)

def hashf(filename):
    return hash(realpath(filename))

# DATA CLASSES
class Root(HasTraits):
    name        = Str('Root')
    outputdbs   = List(OutputDB)
    def __init__(self, files=None, parent=None):
        self.parent = parent
        self.outputdbs = []
        if files is not None:
            self.outputdbs.extend([OutputDB(f, parent=self) for f in files])
        super(Root, self).__init__()

    def open_outputdb(self):
        files = open_outputdbs()
        if not files:
            return
        for file in files:
            self.add_outputdb(file)
        self.refresh()

    def refresh(self):
        pass

    def add_outputdb(self, filename):
        if hashf(filename) in [f.id for f in self.outputdbs]:
            self.reload_outputdb(filename)
            return
        f = OutputDB(filename, parent=self)
        self.outputdbs.append(f)
        if self.parent:
            self.parent.onadd(f)

    def remove_outputdb(self, filename):
        fid = hashf(filename)
        for (i, file) in enumerate(self.outputdbs):
            if fid == file.id:
                break
        else:
            return
        f = self.outputdbs.pop(i)
        if self.parent:
            self.parent.onremove(f)

    def _index(self, filename):
        fid = hashf(filename)
        for (i, file) in enumerate(self.outputdbs):
            if  fid == file.id:
                return i

    def reload_outputdb(self, filename):
        i = self._index(filename)
        if i is None:
            return
        file = self.outputdbs[i]
        file.reload_data()
        if self.parent:
            self.parent.onreload()
        return

    def update(self, **kwds):
        self.parent.update(**kwds)

# View for objects that aren't edited
no_view = View()

class TreeHandler(Handler):

    def refresh(self, editor, object):
        editor.update_editor()

    # Output database actions
    def open_outputdb(self, editor, object):
        """Open file"""
        object.open_outputdb()
        editor.update_editor()

    def remove_outputdb(self, editor, object):
        """Close outputdbs"""
        # Remove the file from the list
        container = editor.get_parent(object)
        container.remove_outputdb(object.filename)

    def reload_outputdb(self, editor, object):
        container = editor.get_parent(object)
        container.reload_outputdb(object.filename)
        return

    def is_hidden(self, editor, object):
        parent = editor.get_parent(object)
        return parent.is_hidden(object.filename)

def step_dclicked(object):
    object.dclicked()

def frame_dclicked(object):
    object.dclicked()

def db_dclicked(object):
    object.dclicked()

# Actions used by tree editor context menu
open_outputdb_action = Action(
    name='Open output database',
    action='handler.open_outputdb(editor,object)')
reload_outputdb_action = Action(
    name='Reload',
    action='handler.reload_outputdb(editor,object)')
close_outputdb_action = Action(
    name='Close',
    action='handler.remove_outputdb(editor,object)')
refresh_action = Action(
    name='Refresh Tree',
    action='handler.refresh(editor,object)')

# Tree editor
tree_editor = TreeEditor(
    nodes = [
        TreeNode(node_for  = [Root],
                  children  = '',
                  label     = 'name',
                  view      = View(Group('name',
                                   orientation='vertical',
                                   show_left=True))),
        TreeNode(node_for  = [Root],
                  children  = 'outputdbs',
                  label     = '=Output Databases',
                  view      = no_view,
                  icon_group='icon/plot.png',
                  icon_open ='icon/plot.png',
                  menu      = Menu(open_outputdb_action)),
        TreeNode(node_for   = [OutputDB],
                  label     = 'name',
                  children  = 'steps',
                  icon_group ='icon/file.png',
                  icon_open  ='icon/file.png',
                  on_dclick = db_dclicked,
                  menu      = Menu(reload_outputdb_action,
                                   close_outputdb_action),
                 view = no_view),
        TreeNode(node_for   = [Step],
                 label     = 'name',
                 children  = 'frames',
                 icon_item ='',
                 icon_group ='',
                 icon_open ='',
                 on_click  =step_dclicked,
                 view      = no_view),
        TreeNode(node_for   = [Frame],
                 label     = 'name',
                 icon_item ='',
                 on_click  =frame_dclicked,
                 view      = no_view),
    ], editable=False, hide_root=True, lines_mode='on', auto_open=2)

class InfoPane(HasTraits):
    name    = Str('Information')
    root = Instance(Root)
    editor = tree_editor

    def __init__(self, files=None, parent=None):
        kwds = {'root': Root(files=files, parent=parent)}
        super(InfoPane, self).__init__(**kwds)

    # The main view
    view = View(
           Group(
               Item(
                    name = 'root',
                    id = 'root',
                    editor = tree_editor,
                    resizable = True),
                orientation = 'vertical',
                show_labels = False,
                show_left = True,),
            dock = 'horizontal',
            drop_class = HasTraits,
            handler = TreeHandler(),
            buttons = ['Undo', 'OK', 'Cancel'],
            resizable = True)

    def __getattr__(self, attr):
        # return this objects attribute first. if it doesn't exist, return the
        # root items
        try:
            HasTraits.__getattr__(self, attr)
        except AttributeError:
            return getattr(self.root, attr)

def open_outputdbs():
    """Open file"""
    wildcard = 'Output files (*.dbx;*.exo)|*.dbx;*.exo|'
    dialog = FileDialog(action="open files", wildcard=wildcard)
    if dialog.open() != pyOK:
        return []
    return dialog.paths

if __name__ == '__main__':

    import glob
    from os.path import join, realpath, dirname
    d = join(dirname(realpath(__file__)), '../../inputs')
    files = glob.glob(join(d, "*.exo"))

    ipane = InfoPane() #files=files)
    ipane.configure_traits()
