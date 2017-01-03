#!/usr/bin/env python
"""Use Pmw.Blt.Graph to animate a function f(x,t) in time."""
import Pmw
from Tkinter import *
from numpy import linspace, exp, sin, pi

class AnimateBLT:
    def __init__(self, parent, f, 
                 xmin, xmax, ymin, ymax, resolution=300):
        self.master = parent
        self.xmax = xmax; self.xmin = xmin
        self.ymax = ymax; self.ymin = ymin
        self.xresolution = resolution  # no of pts on the x axis
        top = Frame(self.master); top.pack()
        self.f = f
        self.g = Pmw.Blt.Graph(top, width=600, height=400)
        self.g.pack(expand=True, fill='both')
        Button(top, text='run', command=self.timeloop).pack()
        parent.bind('<q>', self.quit)

    def init(self):
        self.g.xaxis_configure(min=self.xmin, max=self.xmax)
        self.g.yaxis_configure(min=self.ymin, max=self.ymax)
        self.x = linspace(self.xmin, self.xmax, self.xresolution+1)
        self.y = self.f(self.x, self.t)
        if not self.g.element_exists('curve'):
            self.g.line_create('curve', color='red', linewidth=1,
                               xdata=tuple(self.x),
                               ydata=tuple(self.y),
                               dashes='', symbol='', label='')


    def update(self, y, t, counter):
        self.y = y
        self.g.element_configure('curve', ydata=tuple(self.y),
                                 label='%s(x,t=%.4f)' % \
                                 (self.f.__name__,t))
        self.g.after(40)  # delay (40 milliseconds)
        self.g.update()   # update graph widget
        self.g.postscript_output(fileName='frame_%05d.ps' % counter)

    def timeloop(self):
        # self.dt and self.tstop must be set!
        self.t = 0      # time parameter in (0,tstop)
        self.frame_counter = 1
        self.init()
        while self.t <= self.tstop:
            self.update(self.f(self.x, self.t), 
                        self.t, self.frame_counter)
            self.t += self.dt     # step forward in time
            self.frame_counter += 1

    def quit(self, event): self.master.destroy()

if __name__ == '__main__':
    root = Tk()
    Pmw.initialise(root)
    
    def f(x, t):  # test function
        return exp(-4*(x-t)**2)*sin(10*pi*x) # x is a NumPy vector
    anim = AnimateBLT(root, f, 0, 2, -1, 1, 300)
    anim.tstop = 2; anim.dt = 0.05
    root.mainloop()
