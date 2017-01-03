#!/usr/bin/env python
"""Simple demo of the Pmw.Blt.Graph widget."""
import Pmw, Tkinter, string, random

class BltDemo1:
    def __init__(self, parent):
        self.master = parent
        self.ncurves = 3   # number of curves
        self.npoints = 20  # number of points on each curve

        # it is advantegous to store (x,y) data in
        # Pmw.Blt.Vector objects
        
        # use one x vector to hold the x coordinates of all
        # the self.ncurves curves:
        self.vector_x = Pmw.Blt.Vector()
        # use a list of y vectors(0:self.ncurves-1):
        self.vector_y = [Pmw.Blt.Vector() for y in range(self.ncurves)]
        self.fill_vectors()  # fill the vectors with data for testing

        # make graph widget:
        self.g = Pmw.Blt.Graph(self.master, width=500, height=300)
        self.g.pack(expand=True, fill='both')

        # define a list of colors for the various curves:
        colors = ['red','yellow','blue','green','black','grey']

        # plot each curve:
        # the x coordinates are in self.vector_x
        # the y coordinates are in self.vector_y[i]

        for i in range(self.ncurves):
            curvename = 'line' + str(i)
            self.g.line_create(
                curvename,              # used as identifier
                xdata=self.vector_x,    # x coords
                ydata=self.vector_y[i], # y coords
                color=colors[i],        # linecolor
                linewidth=1+i,          # (progressively thicker lines)
                dashes='',              # '': solid, number: dash
                label=curvename,        # legend
                symbol='',              # no symbols at data points
                )
        self.g.configure(title='My first BLT plot')
        self.g.yaxis_configure(min=0, max=8)        
        self.buttons = Pmw.ButtonBox(self.master,
                                     labelpos='n',
                                     label_text='Options:')
        self.buttons.pack(fill='both', expand=True, padx=10, pady=10)
        # add buttons:
        self.buttons.add('Move points',command=self.animate)
        self.buttons.add('Postscript', command=self.postscript)
        self.buttons.add('Grid on',    command=self.g.grid_on)
        self.buttons.add('Symbols',    command=self.symbols)
        self.buttons.add('Smooth',     command=self.smooth)
        self.buttons.add('Quit',       command=self.master.quit)
        self.buttons.alignbuttons() # nice loook...

    def symbols(self):
        """Turn on symbols (diamonds) at all points."""
        # get the names(identifiers) of all curves in the graph:
        curvenames = self.g.element_show()
        # foreach curve, add a diamond symbol, filled with the
        # color of the curve ('defcolor') and with a size of 2:
        for curvename in curvenames:
            self.g.element_configure(curvename, symbol='diamond',
                                     outlinewidth=2, fill='defcolor')

    def smooth(self):
        """Smooth the curves by cubic spline interpolation."""
        for curvename in self.g.element_show():
            self.g.element_configure(curvename, smooth='natural')
    
    def fill_vectors(self):
        """Fill Pmw.Blt vectors with (random) values."""
        # use random numbers for generating plot data:
        random.seed(9)                    # fix the seed for testing
        for index in range(self.npoints):
            self.vector_x.append(index)   # x coordinates
            for y in range(self.ncurves):
                self.vector_y[y].append(random.uniform(0,8))

    def animate(self,delay=100):
        """Adjust curves randomly, in an animated fashion."""
        curves = self.g.element_show()
        for index in range(self.npoints):
            for y in range(self.ncurves):
                self.vector_y[y][index] = random.uniform(0,8)
                # changing vector_y[y] affects the graph directly,
                # but not until the function returns
                # introduce a delay and call update (to force Tk to update
                # pending events, i.e., here the graph)
                self.master.after(delay)
                self.master.update()
        
    def postscript(self):
        """Generate a hardcopy of the plot in PostScript."""
        self.g.postscript_output(fileName='tmp2.ps',decorations='no')

if __name__ == '__main__':
    root = Tkinter.Tk()
    Pmw.initialise(root)
    import scitools.misc; scitools.misc.fontscheme1(root) # our own fonts...
    root.title('Simple Pmw.Blt.Graph demo')
    blt = BltDemo1(root)
    root.mainloop()
