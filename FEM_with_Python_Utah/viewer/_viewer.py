import logging
import datetime
import warnings
import numpy as np
from os.path import dirname, realpath, join, isfile
warnings.simplefilter("ignore")

from chaco.api import *
from traits.api import *
from traitsui.api import *
from chaco.tools.api import *
from enable.api import ComponentEditor
from pyface.api import FileDialog, OK as pyOK
from chaco.example_support import COLOR_PALETTE
from traitsui.tabular_adapter import TabularAdapter
from traitsui.menu import MenuBar, ToolBar, Menu, Action, NoButtons

from ._plot import *
from ._ipane import *

icns = join(dirname(realpath(__file__)), 'icon')

class DisplayedFields(HasTraits):
    '''Dynamically redefine valid fields to display'''
    choice = Str
    displayed = Str
    choices = List(Str)
    _apply = Action(name='Apply', action='_apply')
    def __init__(self, parent, choices=None):
        self.parent = parent
        if choices is None: choices = []
        super(DisplayedFields, self).__init__(choices=choices)
    class MyHandler(Handler):
        def object_displayed_changed(self, info):
            info.object.parent.update(field=info.object.displayed)
    class MyHandler1(Handler):
        def _apply(self, info):
            info.object.displayed = info.object.choice
            info.object.parent.update(field=info.object.displayed)
        def closed(self, info, is_ok):
            if not is_ok:
                return
            info.object.displayed = info.object.choice
            info.object.parent.update(field=info.object.displayed)
    traits_view = View(
        Item(name='displayed',
             editor=EnumEditor(name='choices'),
             show_label=False),
        style='simple',
        handler=MyHandler())
    edit_view = View(Item(name='choices',
                          editor=ListStrEditor(multi_select=False,
                                               editable=False,
                                               selected='choice'),
                          show_label=False),
                     style='simple',
                     title='Field Variable',
                     buttons=['OK', 'Cancel', _apply],
                     handler=MyHandler1())
    def update(self, choices=None, field=None):
        if choices is not None:
            self.choices = Plotter.filter_choices(choices)
            self.displayed = self.choices[0]
        if field is not None:
            self.parent.update(field=field)

class DisplayedFiles(HasTraits):
    displayed = Str
    choices = List(Str)
    def __init__(self, parent, choices=None):
        self.parent = parent
        if choices is None: choices = []
        super(DisplayedFiles, self).__init__(choices=choices)
    class MyHandler(Handler):
        """Handler class to redefine the possible values of 'choices'."""
        choices = List(Str)
        def object_displayed_changed(self, info):
            if not info.initialized:
                return
            info.object.update(displayed=info.object.displayed)
        def object_choices_changed(self, info):
            self.choices = info.object.choices
    view = View(
        Item(name='displayed', editor=EnumEditor(name='handler.choices'),
             show_label=False),
        buttons   = ['OK'],
        style     = 'simple',
        resizable = True,
        handler   = MyHandler())
    def update(self, choices=None, displayed=None):
        if choices is not None:
            self.choices = choices
            self.displayed = choices[0]
        elif displayed is not None:
            self.parent.update(displayed=displayed)
    def append(self, filename):
        self.choices = self.choices + [filename]
    def remove(self, filename):
        self.choices = [f for f in self.choices if f != filename]
        if f == self.displayed:
            self.displayed = self.choices[0]

class Application(HasStrictTraits):
    plotter = Instance(Plotter)
    step = Str
    frame = Int(0)
    sfstr = Str
    displayed_fields = Instance(DisplayedFields)
    displayed_files = Instance(DisplayedFiles)
    print_filename = Str
    plotu = Button(label='', image=join(icns,'plot-undeformed.png'),
                   style='toolbar')
    plotd = Button(label='', image=join(icns,'plot-deformed.png'),
                   style='toolbar')
    plotcu = Button(label='', image=join(icns,'plot-contour-undeformed.png'),
                    style='toolbar')
    plotcd = Button(label='', image=join(icns,'plot-contour-deformed.png'),
                    style='toolbar')
    plotana = Button(label='', image=join(icns,'analytic-expr.png'),
                    style='toolbar')
    plotf = Button(label='', image=join(icns,'field.png'),
                   style='toolbar')
    plotp = Button(label='', image=join(icns,'camera.png'),
                   style='toolbar')
    plotpts = Button(label='', image=join(icns,'color-points.png'),
                     style='toolbar')
    plotscale = Button(label='', image=join(icns,'plot-scale.png'),
                       style='toolbar')
    frame_first = Button(label='', image=join(icns,'frame-first.png'),
                         style='toolbar')
    frame_prev = Button(label='', image=join(icns,'frame-previous.png'),
                        style='toolbar')
    frame_next = Button(label='', image=join(icns,'frame-next.png'),
                        style='toolbar')
    frame_last = Button(label='', image=join(icns,'frame-last.png'),
                        style='toolbar')
    ipane = Instance(InfoPane)
    def __init__(self, sources):
        """Put together information to be sent to Plotter information
        needed:

        variables : list
           list of variables that changed from one simulation to another
        x_idx : int
           column containing x variable to be plotted

        """
        traits = {"ipane": self.init(sources)}
        HasStrictTraits.__init__(self, **traits)
        self.plotter = Plotter()
        self.displayed_fields = DisplayedFields(self)
        self.displayed_files = DisplayedFiles(self)
        if self.ipane.root.outputdbs:
            self.plotter.set(self.ipane.root.outputdbs[0])
            self.displayed_fields.update(self.plotter.choices)
            files = [db.filename for db in self.ipane.root.outputdbs]
            self.displayed_files.update(choices=files)
            self.plotter.update(field=self.displayed_fields.displayed)

    def init(self, sources):

        errors = 0
        if not sources:
            return InfoPane(parent=self)
        return InfoPane(files=sources, parent=self)

    @on_trait_change('plotter.step')
    def _change_step(self):
        self.step = self.plotter.step
        self.sfstr = '{0}, Frame {1}'.format(self.step, self.frame)

    @on_trait_change('plotter.frame')
    def _change_frame(self):
        self.frame = self.plotter.frame
        self.sfstr = '{0}, Frame {1}'.format(self.step, self.frame)

    def _scale_changed(self, scale):
        if abs(scale) <= 1.e-16:
            scale = 1.0
        self.plotter.update(scale=scale)
        return

    def _frame_first_fired(self):
        self.plotter.update(frame=0)

    def _frame_prev_fired(self):
        self.plotter.update(dframe=-1)

    def _frame_next_fired(self):
        self.plotter.update(dframe=1)

    def _frame_last_fired(self):
        self.plotter.update(frame=-1)

    def onadd(self, f):
        '''File added, update the view'''
        self.displayed_files.append(f.filename)
        self.update(displayed=f.filename)
        return

    def onremove(self, f):
        '''File removed, update the view'''
        self.displayed_files.remove(f.filename)
        if not self.ipane.outputdbs:
            self.displayed_fields.update([])
            self.plotter.set([], [], {}, [])
            self.plotter.reset_plot()
        else:
            self.update()
        return

    def update(self, field=None, displayed=None, step=None, frame=None):
        '''File added, update the view'''

        displayed_file_changed = displayed != self.displayed_files.displayed

        if field is not None:
            self.plotter.update(field=field)

        elif step is not None and not displayed_file_changed:
            self.plotter.update(step=step, frame=frame)

        elif frame is not None and not displayed_file_changed:
            self.plotter.update(step=step, frame=frame)

        elif displayed is not None:
            for db in self.ipane.outputdbs:
                if db.filename == displayed:
                    break
            else:
                raise SystemExit('{0} is not a db'.format(displayed))
            self.plotter.set(db)
            self.displayed_fields.update(choices=self.plotter.choices)
            self.displayed_files.displayed = displayed
            self.plotter.update(field=self.displayed_fields.displayed,
                                step=step, frame=frame)

        return

    def onreload(self):
        '''File reloaded, update the view'''
        self.plotter.update(field=self.displayed_fields.displayed)
        return

    def change_field(self):
        self.displayed_fields.edit_traits(view='edit_view')

    def plot_analytic(self):
        self.plotter.edit_traits(view='analytic_view')

    def adjust_plot_scales(self):
        self.plotter.scaleobj.edit_traits(view='edit_view')

    def _plotpts_fired(self):
        self.plotter.update(plot_nodes=1)

    def _plotu_fired(self):
        self.plotter.update(deformed=0, contour=0)

    def _plotd_fired(self):
        self.plotter.update(deformed=1, contour=0)

    def _plotcu_fired(self):
        self.plotter.update(deformed=0, contour=1)

    def _plotcd_fired(self):
        self.plotter.update(deformed=1, contour=1)

    def _plotana_fired(self):
        self.plot_analytic()

    def _plotf_fired(self):
        self.change_field()

    def _plotscale_fired(self):
        self.adjust_plot_scales()

    def _plotp_fired(self):
        self.print_screen()

    def print_screen(self):
        """Open file"""
        wildcard = ('Screen Capture (*.png)|*.png|')
        dialog = FileDialog(action="save as", wildcard=wildcard)
        if dialog.open() != pyOK:
            return
        if not dialog.paths:
            return
        filename = dialog.paths[0]
        if not filename.endswith('.png'):
            filename += '.png'

        width, height = self.plotter.container.outer_bounds
        self.plotter.container.do_layout(force=True)
        gc = PlotGraphicsContext((width, height), dpi=72)
        gc.render_component(self.plotter.container)
        gc.save(filename, file_format='PNG')
        return

    def open_file(self):
        self.ipane.open_outputdb()

class ApplicationHandler(Handler):

    def open_file(self, info):
        info.object.open_file()

    def quit(self, info):
        info.ui.dispose()
        raise SystemExit(0)

    def print_screen(self, info):
        info.object.print_screen()

    def closed(self, info, is_ok):
        raise SystemExit(0)

def launch_viewer(sources=None):
    '''Create the plot window'''
    if sources is None:
        sources = []

    if not isinstance(sources, (list, tuple)):
        sources = [sources]

    h = -20
    info_pane = Item('ipane', show_label=False, resizable=False)
    plot_window = VGroup(spring,
        HGroup(Item('displayed_fields', show_label=False),
               spring,
               Item('displayed_files', show_label=False),
               spring,
               Item('frame_first', show_label=False),
               Item('frame_prev', show_label=False),
               Item('frame_next', show_label=False),
               Item('frame_last', show_label=False),
               Item('sfstr', show_label=False,
                    editor=TitleEditor())),
        HGroup(
            VGroup(Item('plotu', show_label=False),
                   Item('plotd', show_label=False),
                   Item('plotcu', show_label=False),
                   Item('plotcd', show_label=False),
                   Item('plotscale', show_label=False),
                   Item('plotpts', show_label=False),
                   '_',
                   #Item('plotf', show_label=False),
                   Item('plotana', show_label=False),
                   '_',
                   Item('plotp', show_label=False)),
            VGroup(Item('plotter', show_label=False, springy=True, resizable=True,
                        width=900, height=600))))

    menubar = MenuBar(
        Menu(
             Action(name = 'Open Output Database', action='open_file'),
             Action(name = 'Take Screenshot', action='print_screen'),
             Action(name = 'Quit', action='quit'),
             name='File'),
        Menu(name='Edit'),
        Menu(
             Action(name = 'Plot Analytic Function', action='plot_analytic'),
             Action(name = 'Field Variable', action='change_field'),
             Action(name = 'Scale', action='adjust_plot_scales'),
             name='Plot Options'),
        Menu(name='Help')
        )
    toolbar = None
    title = "Viewer"
    view = View(HSplit(info_pane, plot_window),
                style='custom', resizable=True, title=title,
                menubar=menubar, toolbar=toolbar)
    main_window = Application(sources=sources)
    main_window.configure_traits(view=view, handler=ApplicationHandler)
    return main_window

if __name__ == "__main__":
    main()
