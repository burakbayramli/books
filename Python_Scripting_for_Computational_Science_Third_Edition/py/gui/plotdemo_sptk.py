#!/usr/bin/env python
"""Simple demo of the TkPlotCanvas widget."""
import Pmw, Tkinter, string, random
from Scientific.TkWidgets.TkPlotCanvas import \
     PolyLine, PolyMarker, PlotGraphics, PlotCanvas
from numpy import zeros, array

class TkPlotCanvasDemo1:
    def __init__(self, parent):
        self.master = parent
        self.ncurves = 3   # number of curves
        self.npoints = 20  # number of points on each curve

        # here we store data in plain NumPy arrays
        self.vector_x = zeros(self.npoints)
        # use a list of y vectors(0:self.ncurves-1)
        self.vector_y = [zeros(self.npoints) \
                         for y in range(self.ncurves)]
        self.fill_vectors()  # fill the vectors with data for testing

        # make graph widget:
        self.g = PlotCanvas(self.master, 500, 300, zoom=True,
                            relief='sunken', border=2)
        self.g.pack(expand=True, fill='both')

        # define a list of colors for the various curves:
        colors = ['red','yellow','blue','green','black','grey']

        # plot each curve:
        # the x coordinates are in self.vector_x
        # the y coordinates are in self.vector_y[i]

        self.curves = []
        for i in range(self.ncurves):
            xy_pairs = array([self.vector_x,self.vector_y[i]]).transpose()
            c = PolyLine(xy_pairs,
                         width=1+i,
                         color=colors[i])
            self.curves.append(c)
        object = PlotGraphics(self.curves)
        self.g.draw(object, xaxis='automatic', yaxis='automatic')

        self.buttons = Pmw.ButtonBox(self.master,
                                     labelpos='n',
                                     label_text='Options:')
        self.buttons.pack(fill='both', expand=True, padx=10, pady=10)
        # add buttons:
        self.buttons.add('Move points',command=self.animate)
        self.buttons.add('Postscript', command=self.postscript)
        self.buttons.add('Symbols',    command=self.symbols)
        self.buttons.add('Quit',       command=self.master.quit)
        self.buttons.alignbuttons() # nice loook...

    def symbols(self):
        """Turn on symbols (triangles) at all points."""
        curves_wsymbol = [PolyMarker(curve.points,
                               color='blue', marker='triangle') \
                          for curve in self.curves]
        self.g.draw(PlotGraphics(curves_wsymbol)) # plot collection

    def fill_vectors(self):
        """Fill NumPy vectors with (random) values."""
        # use random numbers for generating plot data:
        random.seed(9)                    # fix the seed for testing
        for index in range(self.npoints):
            self.vector_x[index] = index   # x coordinates
            for y in range(self.ncurves):
                self.vector_y[y][index] = random.uniform(0,8)

    def animate(self,delay=100):
        """Adjust curves randomly, in an animated fashion."""
        self.g.clear()  # erase all plot data
        for curve in self.curves:
            p = curve.points  # [[x1,y1],[x,2,y2],...] (NumPy)
            for index in range(p.shape[0]):
                p[index][1] = random.uniform(0,8)
            self.g.draw(curve,xaxis='automatic',yaxis=(0,8)) # plot PolyLine
            self.master.after(delay)
            self.master.update()
        
    def postscript(self):
        """Generate a hardcopy of the plot in PostScript."""
        self.g.canvas.postscript(file='tmp2.ps')

if __name__ == '__main__':
    root = Tkinter.Tk()
    Pmw.initialise(root)
    import scitools.misc; scitools.misc.fontscheme1(root) # our own fonts...
    root.title('Simple TkPlotCanvas demo')
    sptkplot = TkPlotCanvasDemo1(root)
    root.mainloop()
