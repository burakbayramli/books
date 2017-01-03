'''
Module for visualizing curves, including animation.
Different classes,

    CurveVizGnuplot, CurveVizBLT, CurveVizGrace,

implement (approximately) the same interface to various plotting
programs (Gnuplot, BLT, ).
There is a (factory) function, named graph, which offers a unified
interface to all the other plotting classes. The name of the plotting
program is a parameter to graph and the corresponding plotting class
instance is returned.

Example: make a plot using BLT

    from scitools.CurveViz import CurveVizBLT
    t = sequence(-4, 4, 0.1, Float)
    g = CurveVizBLT(coor=t, parent_frame=some_frame,
                    ymin=-1.2, ymax=1.2, xlabel='t')
    for p in sequence(0, 4, 0.25):
        u = exp(-p*p)*(sin(t) + 0.2*sin(5*t))
        g.plotcurve(u, legend='u(t); p=%g' % p)
    # programmer can issue any Pmw.Blt.Graph command, e.g.,
    g.g.xaxis_configure(logscale=1)  # log scale on x axis


All objects have a common interface described below.

    def graph(coor=None, program='Gnuplot',
              ymin=None, ymax=None,
              xmin=None, xmax=None,
              parent_frame=None, sleep=0.5,
              xlabel='', ylabel='', title='', window=1):
        """
        program: name of plotting program; 'Gnuplot', 'BLT', ...
        coor   : array of coordinates along the x axis
        xmin   : min coordinate on the x axis
        xmax   : max coordinate on the x axis
        ymin   : min coordinate on the y axis
        ymax   : max coordinate on the y axis
        xlabel : label on the x axis
        ylabel : label on the y axis
        title  : headline of the plot
        sleep  : pause between each window plot (for movies)
        window : plot on the screen (True) or not (False)
        parent_frame: parent widget if plotting program creates
        a part of a larger GUI (only present when using BLT)

        Example:
        from scitools.CurveViz import graph
        t = sequence(-4, 4, 0.1, Float)
        g = graph(coor=t, ymin=-1.2, ymax=1.2,
                  xlabel='t', program='Gnuplot')
        for p in sequence(0, 4, 0.25):
            u = exp(-p*p)*(sin(t) + 0.2*sin(5*t))
            g.plotcurve(u, legend='u(t); p=%g' % p)

The programmer have direct access to the plotting
program interface through g.g., e.g. (in case of Gnuplot):

        g.g('set nogrid')        # turn default grid off
        g.g('set key left box')  # legends in box to the left


    def plotcurve(self, y, legend='', ps=0, 
                  plotstyle=DEFAULTSTYLE):
        """
        Plot a curve. The variable y is a NumPy array, or
        a tuple (x,y) of compatible NumPy arrays for the data
        points, legend is the label of the curve, ps!=0
        indicates a hardcopy of the plot in PostScript format.
        If ps=1 the name of the PostScript file is "tmp_%04d.ps" % f,
        where f is a in internal counter. If ps is a string that
        string is taken as the filename.

        The plotstyle argument controls the linetype in the plot.
        DEFAULTSTYLE gives the plotting program's default lines.
        In general, plotstyle is a dictionary:
          'style': 'lines', 'dots', 'points', 'linepoints'
          'type' : 1, 2, 3, ...
          'size' : 1, 2, 3, ...
          'color' : 'red', 'black', ...
          Note: plotstyle!=DEFAULTSTYLE is not implemented.
        """
        raise AttributeError, 'not implemented in class %s' % \
              __class__.__name__

    def plotcurves(self, curves, ps=0, plotstyles=DEFAULTSTYLE):
        """
        Plot a set of curves. The variable 'curves' is
        a list/tuple of (array(s),label) tuples, e.g.,
        [(y1,'data'), (y2,'fit')] or [((x1,y1),'data'), (y2,'fit')],
        i.e., the first element in each tuple can either be an
        array with the y coordinates or a tuple pair (x,y) of
        x and d data points.
        
        If ps !=0 a hardcopy of the plot in PostScript format
        is made. If ps=1 the name of the PostScript file is
        "tmp_%04d.ps" % f, where f is a in internal counter.
        If ps is a string that string is taken as the filename.

        The plotstyles argument is a list/tuple of
        dictionaries describing the style of each curve;
        plotstyles[i] describes the data in curves[i][0].
        Each plotstyles[i] item follows the syntax of the
        plotstyle parameter as documented in plotcurve.
        Note: plotstyles!=DEFAULTSTYLE is not implemented.
        """
        raise AttributeError, 'not implemented in class %s' % \
              __class__.__name__
'''

import time
try:
    from Tkinter import *
    import Pmw
except:
    pass

from numpy import *
try:
    import Gnuplot
except:
    pass # no Gnuplot
try:
    import gracePlot
except:
    pass


DEFAULTSTYLE = {'style' : 'lines', 'type' : 1, 'size' : 1, 'color' : 'red' }
COLORS = ('blue', 'red', 'green', 'black', 'brown', 'yellow', 'orange')


class CurveViz:
    """Base class for curve plotting."""
    def __init__(self, **kwargs):
        # first set default values of various properties:
        self._properties = {
          'coor': None,
          'ymin': None, 'ymax': None,
          'xmin': None, 'xmax': None,
          'window': True, 
          'sleep': 0.5,
          'xlabel': '', 'ylabel': '', 'title': '',
          'parent_frame': None,
          'final_sleep': 4,
          }
        for key in kwargs:
            if key in self._properties:
                self._properties[key] = kwargs[key]
            else:
                raise KeyError, \
                'constructur argument "%s" is not registered' % key

        self._p = self._properties  # short form
        self.frame_no = 0           # counter for ps files for movies

    def _set_coor(self, coor):
        self._properties['coor'] = coor
        # adjust max/min on xaxis if these are too small:
        if self._properties['xmin'] is not None and \
           self._properties['xmin'] > coor[0]:
            self._properties['xmin'] = coor[0]
        if self._properties['xmax'] is not None and \
           self._properties['xmax'] < coor[-1]:
            self._properties['xmax'] = coor[-1]

    def _get_coor(self):
        return self._properties['coor']

    x = property(fget=_get_coor, fset=_set_coor,
                 doc='coordinates along the x axis')

    coor = property(fget=_get_coor, fset=_set_coor,
                 doc='coordinates along the x axis')


    def init(self):
        """Initialize plot."""
        return

    def psplot(self, ps):
        if ps:
            self.frame_no += 1
            if isinstance(ps, str):
                filename = ps
            else:
                filename = 'tmp_%04d.ps' % self.frame_no
            self.pscommand(filename)
            print "saved plot to PostScript file", filename

    def configure(self, **kwargs):
        for key in kwargs.keys():
            if key == 'program':
                raise ValueError, \
                'program cannot be changed; make new object (call graph)'
            if key in self._properties:
                self._properties[key] = kwargs[key]
            else:
                raise KeyError, \
                      '%s is not a legal property (keyword argument '\
                      'to configure or constructor in class %s' % \
                      (key, self.__class__.__name__)
        self.init()

    def curvedata(self, y):
        """
        Return x,y data of a curve based on the y argument.
        y can be either a NumPy array or a (x,y) data tuple
        of two compatible NumPy arrays.
        If y is an array, return self.x, y.
        If y is tuple, return the tuple.
        """
        if isinstance(y, (tuple,list)):
            if len(y) != 2:
                raise ValueError, \
                      'y argument must be array or (array,array) tuple'
            else:
                xd, yd = y
        else:
            xd, yd = self.x, y
        if xd is None:
            raise ValueError, \
            'no coordinates are given, call configure(coor=xdata)'
        if len(yd) != len(xd):
            raise ValueError, \
            '%d x coordinates incompatible with %d y coordinates' % \
            (len(xd),len(yd))
        return xd, yd

        
class CurveVizGnuplot(CurveViz):
    def __init__(self, **kwargs):
        """
        Create a visualizer for curves, using Gnuplot.
        """
        CurveViz.__init__(self, **kwargs)

        self.g = Gnuplot.Gnuplot(persist=1)
        self.g('set grid')  # grid is default
        #self.g('set key left box')  # legends in box to the left
        self.g('set border 31 lt -1 lw 2.000')
        self.init()

    def init(self):
        xmin, xmax = self.x[0], self.x[-1]
        if self._p['xmin'] is not None:
            xmin = min(self._p['xmin'], self.x[0])
        if self._p['xmax'] is not None:
            xmax = max(self._p['xmax'], self.x[-1])
        self.g('set xrange[%g:%g]' % (xmin, xmax))

        if self._p['ymin'] is not None and self._p['ymax'] is not None:
            self.g("set yrange[%g:%g]" % (self._p['ymin'], self._p['ymax']))
        else:
            self.g('set autoscale')

        if self._p['xlabel']: self.g.xlabel(self._p['xlabel'])
        if self._p['ylabel']: self.g.ylabel(self._p['ylabel'])
        if self._p['title']: self.g.title(self._p['title'])

    def __del__(self):
        # need to pause Gnuplot so that plot data are read from
        # temporary files and plotted before the self.g instance
        # goes out of scope
        try:
            time.sleep(self._p['final_sleep'])  # may be cleaned up...
        except:
            pass
        
    def plotcurve(self, y, legend='', ps=0, 
                  plotstyle=DEFAULTSTYLE):
        """See CurveViz module for documentation."""
        x, y = CurveViz.curvedata(self, y)
        self.d = Gnuplot.Data(x, y, with_='lines', title=legend)
        if self._p['window'] != 0:
            self.g.plot(self.d)
        time.sleep(self._p['sleep'])
        self.psplot(ps)

    def plotcurves(self, curves, ps=0, plotstyles=DEFAULTSTYLE):
        """See CurveViz module for documentation."""
        self.d = []
        for y, legend in curves:
            x, y = CurveViz.curvedata(self, y)
            self.d.append(Gnuplot.Data(x, y, with_='lines', title=legend))
        if self._p['window'] != 0:
            #apply(self.g.plot, self.d)  # old syntax
            self.g.plot(*self.d)
        time.sleep(self._p['sleep'])
        self.psplot(ps)

    def pscommand(self, filename):
        self.g.hardcopy(filename=filename, 
                        mode='eps', color=0, enhanced=1,
                        fontname='Times-Roman', fontsize=20)
        # ensure that there is enough time to make the plot:
        if self._p['sleep'] < 1: time.sleep(1)
        
class CurveVizEasyviz(CurveViz):
    def __init__(self, **kwargs):
        """
        Create a visualizer for curves, using the scitools.easyviz
        interface to a range of plotting packages.
        """
        CurveViz.__init__(self, **kwargs)
        self._p['backend'] = kwargs.get('backend', 'matplotlib')
        import scitools.easyviz as ez
        self.g = ez
        self.init()

    def init(self):
        self.plot_kwargs = {}
        # self.plot_kwargs['axis'] is defined if both x and y min/max
        # values are given
        if self._p['ymin'] is not None and self._p['ymax'] is not None:
            yaxis = [self._p['ymin'], self._p['ymax']]
            xmin, xmax = self.x[0], self.x[-1]
            if self._p['xmin'] is not None:
                xmin = min(self._p['xmin'], self.x[0])
            if self._p['xmax'] is not None:
                xmax = max(self._p['xmax'], self.x[-1])
            self.plot_kwargs['axis'] = [xmin, xmax] + yaxis

        for name in 'xlabel', 'ylabel', 'title':
            if self._p[name]:
                self.plot_kwargs[name] = self._p[name]

        
    def plotcurve(self, y, legend='', ps=0, 
                  plotstyle=DEFAULTSTYLE):
        """See CurveViz module for documentation."""
        x, y = CurveViz.curvedata(self, y)
        self.data_arrays = [x, y]
        if self._p['window'] != 0:
            self.g.plot(*self.data_arrays, **self.plot_kwargs)
        time.sleep(self._p['sleep'])
        self.psplot(ps)

    def plotcurves(self, curves, ps=0, plotstyles=DEFAULTSTYLE):
        """See CurveViz module for documentation."""
        self.data_arrays = []
        for y, legend in curves:
            x, y = CurveViz.curvedata(self, y)
            self.data_arrays += [x, y]
        if self._p['window'] != 0:
            #apply(self.g.plot, self.d)  # old syntax
            self.g.plot(*self.data_arrays, **self.plot_kwargs)
        time.sleep(self._p['sleep'])
        self.psplot(ps)

    def pscommand(self, filename):
        kwargs = dict(savefig=filename)
        kwargs.update(self.plot_kwargs)
        self.g.plot(*self.data_arrays, **kwargs)
        

class CurveVizBLT(CurveViz):
    def __init__(self, blt_widget=None, side='top',
                 width=600, height=400, **kwargs):
        """
        Create a visualizer for curves, using BLT.
        See module documentation for interface description.
        Two additional keyword arguments are provided:

          blt_widget          available Pmw.Blt.Graph widget
                              (alternative to making a new one
                              in parent_frame, defaults to None)
          side                side=side is used when packing
                              a new Pmw.Blt.Graph widget in
                              parent_frame (defaults to 'top')

        Additional functions:
          erase   - erase all curves
          
        Example:
        from CurveViz import CurveVizBLT
        t = sequence(-4, 4, 0.1, Float)
        g = CurveVizBLT(coor=t, parent_frame=some_frame,
                        ymin=-1.2, ymax=1.2, xlabel='t')
        # work with g as with any other class in this module
        # programmer can issue any Pmw.Blt.Graph command, e.g.,
        g.g.xaxis_configure(logscale=1)  # log scale on x axis
            
        """
        CurveViz.__init__(self, **kwargs)

        if blt_widget is None:
            if self._p['parent_frame'] is None:
                raise ValueError, 'parent_frame argument is required'
            self.g = Pmw.Blt.Graph(self._p['parent_frame'],
                                   width=width, height=height)
            self.g.pack(side=side, expand=1, fill='both')
        else:
            self.g = blt_widget
        self.g.bind('<Button-1>', self.psdump)
        self.init()

    def init(self):
        if self.x is not None:
            self.g.xaxis_configure(min=self.x[0], max=self.x[-1])
        if self._p['ymin'] is not None and self._p['ymax'] is not None:
            self.g.yaxis_configure(min=self._p['ymin'], max=self._p['ymax'])
        if self._p['title']: self.g.configure(title=self._p['title'])
        if self._p['xlabel']: self.g.xaxis_configure(title=self._p['xlabel'])
        if self._p['ylabel']: self.g.yaxis_configure(title=self._p['ylabel'])

    def psdump(self, event):
        self.psplot(1)
        
    def __del__(self):
        # pause the graphics before everyting is killed...
        try:
            time.sleep(0)
        except:
            pass
        
    def erase(self):
        for name in self.g.element_names():
            self.g.element_delete(name)
            
    def plotcurve(self, y, legend='', ps=0, plotstyle=DEFAULTSTYLE):
        """See CurveViz module for documentation."""
        x, y = CurveViz.curvedata(self, y)
        self.erase()
        self.g.line_create('curve',
                           xdata=tuple(x), ydata=tuple(y),
                           label=legend, linewidth=2, 
                           dashes='', symbol='')
        # the next two are for animations:
        self.g.after(int(self._p['sleep']*1000))  # delay (milliseconds)
        self.g.update()   # update graph widget
        self.psplot(ps)

    def plotcurves(self, curves, ps=0, plotstyles=DEFAULTSTYLE):
        """See CurveViz module for documentation."""
        self.erase()
        curve_counter = 1
        color_index = 0; dash_index = 0
        for y, legend in curves:
            x, y = CurveViz.curvedata(self, y)
            # re-use colors but add dashes:
            if color_index > len(COLORS)-1:
                color_index = 0
                dash_index += 1
            if dash_index == 0: dashes=''
            else:               dashes=dash_index
            
            self.g.line_create('curve' + str(curve_counter),
                           xdata=tuple(x), ydata=tuple(y),
                           linewidth=2, label=legend,
                           dashes=dashes, symbol='',
                           color=COLORS[color_index])
            curve_counter += 1
            color_index += 1

        # the next two are for animations:
        self.g.after(int(self._p['sleep']*1000))  # delay (milliseconds)
        self.g.update()   # update graph widget
        self.psplot(ps)
        
    def psplot(self, ps):
        if ps != 0:
            self.frame_no += 1
            if isinstance(ps, str):
                filename = str
            else:
                filename = 'tmp_%04d.ps' % self.frame_no
            self.g.postscript_output(fileName=filename)
            print "saved plot to PostScript file", filename
        

class CurveVizGrace(CurveViz):
    def __init__(self, **kwargs):
        """
        Create a visualizer for curves, using Grace (xmgrace)
        """
        CurveViz.__init__(self, **kwargs)
        self._p['window'] = 1  # for now we need a window...
        self.g = gracePlot.gracePlot()
        self.init()

    def init(self):
        if self.x is not None:
            self.g.xlimit(lower=self.x[0], upper=self.x[-1])
        if self._p['ymin'] is not None and self._p['ymax'] is not None:
            self.g.ylimit(lower=self._p['ymin'], upper=self._p['ymax'])
        if self._p['xlabel']: self.g.xlabel(self._p['xlabel'])
        if self._p['ylabel']: self.g.ylabel(self._p['ylabel'])
        if self._p['title']: self.g.title(self._p['title'])

    def __del__(self):
        # need to pause so that plot data are read from
        # temporary files and plotted before the self.g instance
        # goes out of scope
        try:
            time.sleep(self._p['final_sleep'])  # may be cleaned up...
        except:
            pass
        
    def plotcurve(self, y, legend='', ps=0, 
                  plotstyle=DEFAULTSTYLE):
        """See CurveViz module for documentation."""
        x, y = CurveViz.curvedata(self, y)
        self.g.hold(0)  # do not allow multiple curves in one plot
        if self._p['window'] != 0:
            self.g.plot(x, y)
            if legend: self.g.legend([legend])
#        if self._p['ymin'] is not None and self._p['ymax'] is not None:
#            self.g.ylimit(lower=self._p['ymin'], upper=self._p['ymax'])
        time.sleep(self._p['sleep'])
        self.psplot(ps)

    def plotcurves(self, curves, ps=0, plotstyles=DEFAULTSTYLE):
        """See CurveViz module for documentation."""
        multiple = 0
        for y, legend in curves:
            x, y = CurveViz.curvedata(self, y)
            self.g.hold(multiple)
            if self._p['window'] != 0:
                self.g.plot(x, y)
            if not multiple:
                multiple = 1
        legends = [legend for y, legend in curves]
        self.g.legend(legends)
        time.sleep(self._p['sleep'])
        self.psplot(ps)

    def pscommand(self, filename):
        self.g.save(filename, format='eps')
        # ensure that there is enough time to make the plot:
        if self._p['sleep'] < 1: time.sleep(1)
        

def graph(program='Gnuplot', **kwargs):
    """
    Interface to various classes for curve plotting.
    graph is a factory function, which returns an instance
    of objects in the CurveViz hierarchy (CurveVizGnuplot,
    CurveVizBLT, CurveVizGrace, CurveVizEasyviz, etc.).
    
    Explanations of the arguments:
    
        program: name of plotting program; 'Gnuplot', 'BLT', 'Grace', ...
        coor   : array of coordinates along the x axis
        xmin   : min coordinate on the x axis
        xmax   : max coordinate on the x axis
        ymin   : min coordinate on the y axis
        ymax   : max coordinate on the y axis
        xlabel : label on the x axis
        ylabel : label on the y axis
        title  : headline of the plot
        sleep  : pause between each window plot (for movies)
        window : plot on the screen (True) or not (False)
        parent_frame: parent widget if plotting program creates
        a part of a larger GUI (only present when using BLT)

        Example:
        from CurveViz import graph
        t = sequence(-4, 4, 0.1, Float)
        g = graph(coor=t, ymin=-1.2, ymax=1.2,
                  xlabel='t', program='Gnuplot')
        # make animations:
        for p in sequence(0, 4, 0.25):
            u = exp(-p*p)*(sin(t) + 0.2*sin(5*t))
            g.plotcurve(u, legend='u(t); p=%4.2f' % p)

        # plot several curves in one plot:
        t = sequence(0, 10, 0.01)
        g.configure(coor=t) # change coordinate vector
        g.configure(ymin=-2, ymax=2, ylabel='u')
        u1 = sin(t)*t; u2 = sin(t)*sqrt(t)
        g.plotcurves([(u1,'t ampl.'),(u2,'sqrt(t) ampl.')], ps=1)

The programmer has direct access to the plotting
program interface through g.g., e.g. (in case of Gnuplot):

        g.g('set pointsize 10')
    """

    if program == 'Gnuplot':
        g = CurveVizGnuplot(**kwargs)
    elif program == 'BLT':
        g = CurveVizBLT(**kwargs)
    elif isinstance(program, Pmw.Blt.Graph):
        # used when PyShell is extended with a BLT graph:
        g = CurveVizBLT(**kwargs)
    elif program == 'Grace':
        g = CurveVizGrace(**kwargs)
    elif program == 'Easyviz':
        g = CurveVizEasyviz(**kwargs)
    else:
        raise ValueError, "program '%s' not supported" % program
    return g


# Note: _g._properties['final_sleep']=0 so if plot() is used in a script,
# curves may not be properly shown if the script terminates
# before Gnuplot goes out of scope. For interactive use,
# plot is ok.

def plot(x, y, legend='', ps=0):
    global _g
    # reuse old _g if possible
    _g = CurveVizGnuplot(coor=x)
    _g.plotcurve(y, legend=legend, ps=ps)

#>>> def plot(x,y,legend='',ps=0,plotstyle=DEFAULTSTYLE,**kwargs):
#	g.configure(**kwargs)
#	g.plotcurve((x,y),legend,ps,plotstyle)
    
  

def test1(program, parent=None):
    from CurveViz import graph
    t = linspace(-4, 4, 41)
    g = graph(coor=t, ymin=-1.2, ymax=1.2, xlabel='t',
              program=program, parent_frame=parent)
    # make animations:
    for p in linspace(0, 4, 2):
        u = exp(-p*p)*(sin(t) + 0.2*sin(5*t))
        g.plotcurve(u, legend='u(t); p=%4.2f' % p, ps=True)

    # plot several curves in one plot:
    t = linspace(0, 10, 1001)
    g.configure(coor=t) # change coordinate vector
    g.configure(ymin=None, ymax=None, ylabel='u')
    u1 = sin(t)*t; u2 = sin(t)*sqrt(t)
    g.plotcurves([(u1,'t ampl.'),(u2,'sqrt(t) ampl.')], ps=1)

# try (x,y) data for y argument:

def test2(program, parent=None):
    from CurveViz import graph
    t = linspace(-4, 4, 81)
    g = graph(coor=t, ymin=-1.2, ymax=1.2, xlabel='t',
              program=program, parent_frame=parent)
    # make animations:
    for p in linspace(0, 3, 2):
        u = exp(-p*p)*(sin(t) + 0.2*sin(5*t))
        g.plotcurve((t,u), legend='u(t); p=%4.2f' % p)

    # plot several curves in one plot:
    t = linspace(0, 10, 101)
    g.configure(coor=t) # change coordinate vector
    g.configure(ymin=-4, ymax=4, ylabel='u')
    u1 = sin(t)*t; u2 = sin(t)*sqrt(t)
    g.plotcurves([((t,u1),'t ampl.'),((t,u2),'sqrt(t) ampl.')], ps=1)

if __name__ == '__main__':
    print 'Easyviz'
    test1('Easyviz')
    test2('Easyviz')
    print 'Gnuplot'
    test1('Gnuplot')
    import sys
    test2('Gnuplot')
    time.sleep(6)
    print 'Tk/Pmw/BLT'
    root = Tk()
    Pmw.initialise(root)
    test1('BLT', root)
    test2('BLT', root)
    time.sleep(6)
    print 'Grace'
    test1('Grace')
    test2('Grace')
    
