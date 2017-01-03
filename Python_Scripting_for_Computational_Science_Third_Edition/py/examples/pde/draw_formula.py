#!/usr/bin/env python

import scitools.TkGUI
from numpy import *
from Tkinter import *
from CurveViz import CurveVizBLT

class Elliptic1DGUI:
    def __init__(self, parent):
        self.master = parent
        n = 200 # no of points in x grid
        self.xcoor = linspace(0, 1, n+1)
        width = 500; height = 200
        self.df = scitools.TkGUI.DrawFunction(
            self.xcoor, parent, xlabel='x', ylabel='k(x)',
            curvename='k(x)', ymin=0, ymax=10,
            width=width, height=height, yrange_widgets=True)
        self.df.pack()

        Button(parent, text='Compute solution',
               command=self.solution).pack(pady=4)

        self.g = CurveVizBLT(coor=self.xcoor, parent_frame=parent,
                             ymin=0.0, ymax=1.0,
                             xlabel='x', ylabel='u(x)',
                             width=width, height=height)
        self.g.g.grid_on() # special Pmw.Blt.Graph method

        Button(parent, text='Quit', command=self.master.destroy).pack(pady=4)

    def solution(self):
        x, k = self.df.get()
        integrand = 1.0/k
        # adjust for Trapezoidal rule:
        integrand[0] /= 2.0;  integrand[-1] /= 2.0
        # integrals:
        u = add.accumulate(integrand)
        d = sum(integrand)
        self.u = u/d
        self.g.plotcurve(self.u, legend='u(x)')

        
if __name__ == '__main__':
    root = Tk()
    #import scitools.misc; scitools.misc.fontscheme2(root)
    import scitools.misc; scitools.misc.fontscheme3(root)
    g = Elliptic1DGUI(root)
    root.mainloop()
