import math
import numpy as np
from os.path import basename, dirname, realpath
from sympy import symbols, lambdify
from sympy.parsing.sympy_parser import parse_expr, standard_transformations

from chaco.api import *
from traits.api import *
from traitsui.api import *
from chaco.tools.api import *
from traitsui.message import error
from enable.api import ComponentEditor
from chaco.data_range_1d import DataRange1D
from pyface.api import FileDialog, OK as pyOK
from traitsui.tabular_adapter import TabularAdapter
from chaco.default_colormaps import hot, jet

from ._ipane import InfoPane, OutputDB

def Ones(n):
    return [1. for _ in range(n)]

__all__ = ["Plotter"]

class Scale(HasTraits):
    scale = Float(1.)
    _apply = Action(name='Apply', action='_apply')
    def __init__(self, **kwds):
        super(Scale, self).__init__(**kwds)

    class MyHandler(Handler):
        def _apply(self, info):
            if info.object.parent:
                info.object.parent.update(scale=info.object.scale)
        def closed(self, info, is_ok):
            if not is_ok:
                return
            if info.object.parent:
                info.object.parent.update(scale=info.object.scale)

    edit_view = View(
            HGroup(VGroup(
            Item("scale", label="Scale")),
            spring),
            title='Scale',
            buttons=['OK', 'Cancel', _apply],
            handler=MyHandler())

class AnalyticHandler(Handler):
    def _apply(self, info):
        expr = info.object.analytic_expr.strip()
        if expr:
            info.object.plot_analytic(expr)
    def closed(self, info, is_ok):
        if not is_ok:
            return
        expr = info.object.analytic_expr.strip()
        if expr:
            info.object.plot_analytic(expr)

class Plotter(HasTraits):
    plot = Instance(Plot)
    colorbar = Instance(ColorBar)
    container = Instance(HPlotContainer)
    choices = List(Str)
    filename = Str
    db = Instance(OutputDB)
    step = Str
    frame = Int(0)
    scale = Float(1.)
    scaleobj = Instance(Scale)
    plotted = Str
    deformed = Int(0)
    contour = Int(0)
    plot_nodes = Bool(True)
    analytic_expr = Str
    _apply = Action(name='Apply', action='_apply')
    traits_view = View(
        Item('container', editor=ComponentEditor(), show_label=False),
        resizable=True)
    analytic_view = View(Item(name='analytic_expr',
                              editor=TextEditor(multi_line=False),
                              show_label=False),
                         title='Analytic Expression',
                         width=200,
                         buttons=['OK', 'Cancel', _apply],
                         handler=AnalyticHandler())

    def __init__(self, outputdb=None):
        self.scaleobj = Scale(parent=self)
        if outputdb is not None:
            self.set(outputdb)

    def set(self, outputdb):
        self.filename = outputdb.filename
        self.db = outputdb
        self.choices = self.db.choices
        self.plotted = self.choices[0]
        self.deformed = 0
        self.contour = 0
        self.plot_nodes = True
        self.step = self.db.db.steps.keys()[0]
        self.frame = 0
        self.scale = 1.

    def _choices_changed(self):
        if self.choices:
             return
        self.scale = 1.
        try:
             self.plot.invalidate_and_redraw()
        except AttributeError:
             pass

    def create_plot(self):
        plot = Plot(padding=(50,5,5,35), fill_padding=True,
                    bgcolor="white", use_backbuffer=True,
                    border_visible=True)
        return plot

    def draw_plot(self, xp, yp, zp, field):

        self.plot.data.set_data("xp", xp)
        self.plot.data.set_data("yp", yp)

        # Plot the bar displacement
        self.plot.plot(("xp", "yp"), line_width=1.0, name=field,
                       bgcolor="white", line_style="solid",
                       border_visible=True)

        # Plot the nodes variable
        if self.plot_nodes:
            self.plot.plot(('xp', 'yp'), type='scatter',
                           marker='dot', marker_size=2, line_width=0,
                           name=field, bgcolor="white", border_visible=True)
        self.container = HPlotContainer(self.plot)

        if zp is not None:
            # plot a colored field
            xc = zp[0]
            zc = zp[1]
            yc = np.array([np.interp(x, xp, yp) for x in xc])
            self.plot.data.set_data("xc", xc)
            self.plot.data.set_data("yc", yc)
            self.plot.data.set_data("zc", zc)
            self.plot.plot(("xc", "yc", "zc"), type='cmap_scatter',
                           marker='dot', marker_size=2, line_width=0,
                           name=field, color_mapper=jet,
                           bgcolor="white", border_visible=True)

            # Create a colorbar
            colormap = self.plot.color_mapper
            crange = colormap.range
            self.colorbar = ColorBar(index_mapper=LinearMapper(range=crange),
                                     color_mapper=colormap,
                                     orientation="v", resizable="v",
                                     width=20, padding=20, name=field)
            self.colorbar.padding_top = self.plot.padding_top
            self.colorbar.padding_left = self.plot.padding_left
            self.colorbar.padding_bottom = self.plot.padding_bottom

            self.container.add(self.colorbar)

        return

    def update(self, field=None, scale=None, deformed=None, contour=None,
               step=None, dframe=None, frame=None, plot_nodes=None):
        if field == -1:
            return

        if field:
            self.plotted = field

        if step is not None:
            assert self.step in self.db.db.steps.keys()
            self.step = step

        if frame is not None:
            nframe = len(self.db.db.steps[self.step].frames)
            assert frame <= nframe - 1
            if frame < 0:
                frame = nframe - 1
            self.frame = frame

        if dframe is not None:
            step = self.step
            steps = self.db.db.steps
            istep = steps.keys().index(step)
            nstep = len(steps.keys())
            frame = self.frame + dframe
            nframe = len(steps[self.step].frames)
            if frame > nframe - 1:
                if istep == nstep - 1:
                    # end of steps, step
                    frame = nframe - 1
                else:
                    # cycle to next step
                    frame = 0
                    step = steps.keys()[istep+1]
            elif frame < 0:
                if istep == 0:
                    # beginning of steps, step
                    frame = 0
                else:
                    step = steps.keys()[istep-1]
                    frame = len(steps[step].frames) - 1

            self.step = step
            self.frame = frame

        if scale is not None:
            self.scale = scale

        if deformed is not None:
            self.deformed = deformed

        if contour is not None:
            self.contour = contour

        if plot_nodes is not None:
            self.plot_nodes = not self.plot_nodes

        self.refresh()

    def reset_plot(self):
        self.plot = self.create_plot()
        self.plot.data = ArrayPlotData()

    def refresh(self):

        self.reset_plot()

        if not self.db:
            return

        X = self.db.db.get_vertices()[:, 0].flatten()
        fo = self.db.db.steps[self.step].frames[self.frame].field_outputs
        U = fo['U'].get_data(sort=1, flatten=1)
        if self.deformed:
            x = X
            y = U * self.scale
        else:
            x = (X + U) * self.scale
            y = np.zeros_like(x)

        z = None
        if self.contour:
            z = fo[self.plotted].get_data(sort=1, coords=1, flatten=1)

        self.draw_plot(x, y, z, self.plotted)

        add_default_grids(self.plot)
        add_default_axes(self.plot)

        self.plot.index_range.tight_bounds = False
        self.plot.index_range.refresh()
        self.plot.value_range.tight_bounds = False
        self.plot.value_range.refresh()
        self.plot.x_grid.visible = False
        self.plot.y_grid.visible = False
        self.plot.x_axis.font = "modern 16"
        self.plot.y_axis.font = "modern 16"
        self.plot.tools.append(PanTool(self.plot))

        zoom = ZoomTool(self.plot, tool_mode="box", always_on=False)
        self.plot.tools.append(zoom)
        dragzoom = DragZoom(self.plot, drag_button="right")
        self.plot.tools.append(dragzoom)
        if self.colorbar:
            #tjfulle: doesn't work self.colorbar.legend.visible = True
            self.plot.legend.visible = True
            pass
        self.plot.invalidate_and_redraw()
        return

    def plot_analytic(self, expr):
        x = self.db.db.get_vertices()
        xv = symbols('x')
        expr = parse_expr(expr, transformations=standard_transformations)
        yf = lambdify(xv, expr, modules='numpy')
        y = yf(x)
        self.plot.data.set_data("x", x)
        self.plot.data.set_data("y", y)

        # Plot the bar displacement
        self.plot.plot(("x", "y"), line_width=1.0, name='Analytic Expression',
                       bgcolor="white", line_style="solid",
                       border_visible=True)

    @staticmethod
    def filter_choices(a):
        return [x for x in a if x != 'U']

# Run the demo (if invoked from the command line):
if __name__== '__main__':
    # Create the demo:
    #demo.configure_traits()
    demo = Scale()
    demo.configure_traits(view='edit_view')
