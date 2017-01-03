#!/usr/bin/env python

"""
Use Pmw.Blt to plot a curve, whose values are given as a stream
of numbers on standard input. The input format is as follows:

    xmin xmax ymin ymax
    x1 y1
    x2 y2
    ...
    xn yn
    end
"""

import Pmw, string, sys, re
from Tkinter import *

class RealTimeCurve:
    def __init__(self, master):
        self.master = master;
        self.vector_x = Pmw.Blt.Vector()
        self.vector_y = Pmw.Blt.Vector()
        self.g = Pmw.Blt.Graph(self.master,
                               # important to set equal width/height,
                               # otherwise we get an ellipse...
                               width=400, height=400  
                               )
        self.g.pack(expand=1, fill='both')

        # read stream of numbers, first line is xmin, xmax, ymin, ymax:
        s = sys.stdin.readline()
        xmin, xmax, ymin, ymax = map(float, string.split(string.strip(s)))
        #print "xmin=%g, xmax=%g, ymin=%g, ymax=%g" % (xmin,xmax,ymin,ymax)
        self.g.xaxis_configure(min=xmin, max=xmax)
        self.g.yaxis_configure(min=ymin, max=ymax)

        # read coordinates until a keyword "end":
        ncoor = 0
        while 1:
            s = sys.stdin.readline()
            if re.search("end", s): break
            ncoor = ncoor + 1
            x, y = map(float, string.split(string.strip(s)))
            #print "x=%g   y=%g" % (x,y)
            self.vector_x.append(x)
            self.vector_y.append(y)

            if ncoor == 2:
                self.g.line_create(
                    "curve",                # curvename
                    xdata=self.vector_x,    # x coords
                    ydata=self.vector_y,    # y coords
                    color='red',   
                    linewidth=2,  
                    dashes='',              # number: dash, '': solid
                    symbol='',              # no symbols at data points
                    label=''
                    )
            self.g.after(100)
            self.g.update()
            
        Button(self.master, text="Quit", command=self.master.quit).pack(pady=10)


if __name__ == '__main__':
    root = Tk()
    Pmw.initialise(root)
    import scitools.misc; scitools.misc.fontscheme1(root) # our own fonts...
    blt = RealTimeCurve(root)
    root.mainloop()




