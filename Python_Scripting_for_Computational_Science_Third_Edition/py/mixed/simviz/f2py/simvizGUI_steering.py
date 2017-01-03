#!/usr/bin/env python
from Tkinter import *
import sys, math, os, shutil

import simviz_steering as S

class SimVizGUI:
    def __init__(self, master):
        """build the GUI"""
        self.master = master
        
        # place all the sliders in a left frame:
        slider_frame = Frame(self.master, borderwidth=2)
        slider_frame.pack(side='left')
        
        self.p = {}  # holds all Tkinter variables
        self.p['m'] = DoubleVar(); self.p['m'].set(1.0)
        self.slider(slider_frame, self.p['m'], 0, 5, 'm')
        
        self.p['b'] = DoubleVar(); self.p['b'].set(0.7)
        self.slider(slider_frame, self.p['b'], 0, 2, 'b')

        self.p['c'] = DoubleVar(); self.p['c'].set(5.0)
        self.slider(slider_frame, self.p['c'], 0, 20, 'c')

        self.p['A'] = DoubleVar(); self.p['A'].set(5.0)
        self.slider(slider_frame, self.p['A'], 0, 10, 'A')

        self.p['y0'] = DoubleVar(); self.p['y0'].set(0.2)
        self.slider(slider_frame, self.p['y0'], 0, 1, 'y0')

        self.p['step'] = IntVar(); self.p['step'].set(0)
        self.slider(slider_frame, self.p['step'], 0, 1000, 'step')

        # place the textentry widgets and the buttons in a frame
        # to the right of slider_frame:
        middle_frame = Frame(self.master, borderwidth=2)
        middle_frame.pack(side='left', anchor='n')

        # use a separate frame and the grid geometry manager to
        # pack the labels and entries in a table fashion (enables
        # nice alignment):
        entry_frame = Frame(middle_frame, borderwidth=2)
        entry_frame.pack(side='top', pady=20, padx=10)

        self.row_counter = 0 # updated in self.textentry(...)
        
        self.p['func'] = StringVar(); self.p['func'].set('y')
        self.textentry(entry_frame, self.p['func'], 'func')

        self.p['w'] = DoubleVar(); self.p['w'].set(2*math.pi)
        self.textentry(entry_frame, self.p['w'], 'w')

        self.p['tstop'] = DoubleVar(); self.p['tstop'].set(30.0)
        self.textentry(entry_frame, self.p['tstop'], 'tstop')

        self.p['dt'] = DoubleVar(); self.p['dt'].set(0.05)
        self.textentry(entry_frame, self.p['dt'], 'time step')

        self.p['case'] = StringVar(); self.p['case'].set('tmp1')
        self.textentry(entry_frame, self.p['case'], 'casename')

        # add some space (empty label):
        Label(middle_frame, text='').pack(side='top', pady=5)
        
        # add compute button:
        self.sim = Button(middle_frame, text='Compute',
                          width=8, command=self.compute)
        self.sim.pack(side='top', pady=5)

        # add quit button:
        Button(middle_frame, text='Quit', width=8,
               command=self.quit).pack(pady=5)
        
        # press q on the keyboard for exit:
        self.master.bind('<q>', self.quit)

        # include a sketch of the problem to be solved:
        sketch_frame = Frame(self.master, borderwidth=2)
        sketch_frame.pack(side='left')
        gifpic = os.path.join(os.environ['scripting'],
                              'src','misc','figs','simviz2.xfig.t.gif')
        self.sketch = PhotoImage(file=gifpic)
        # (images must be tied to a global or class variable in Py1.5)
        Label(sketch_frame, image=self.sketch).pack(side='top', pady=20)

        self.cwd = os.getcwd()  # current working directory

    def quit(self,event=None):
        """destroy GUI; called from Quit button or 'q' binding"""
        self.master.destroy()

    def textentry(self, parent, variable, label):
        """make a textentry field"""
        # pack a label and entry horizontally in a frame:
        l = Label(parent, text=label)
        l.grid(column=0, row=self.row_counter, sticky='w')
        widget = Entry(parent, textvariable=variable, width=8)
        widget.grid(column=1, row=self.row_counter)
        #widget.grid(column=1, sticky='w')
        self.row_counter += 1
        return widget

    def slider(self, parent, variable, low, high, label):
        """make a slider [low,high] tied to variable"""
        widget = Scale(parent, orient='horizontal',
          from_=low, to=high,  # range of slider
          # tickmarks on the slider "axis":
          tickinterval=(high-low)/5.0,
          # the steps of the counter above the slider:
          resolution=(high-low)/100.0,
          label=label,    # label printed above the slider
          length=300,     # length of slider in pixels
          variable=variable)  # slider value is tied to variable
        widget.pack(side='top')
        return widget

    def setprm(self):
        """transfer GUI parameters to oscillator code"""
        # safest to transfer via simviz_steering as that
        # module employs the parameters internally:
        S.m = self.p['m'].get(); S.b = self.p['b'].get()
        S.c = self.p['c'].get(); S.A = self.p['A'].get()
        S.w = self.p['w'].get(); S.y0 = self.p['y0'].get()
        S.tstop = self.p['tstop'].get()
        S.dt = self.p['dt'].get(); S.func = self.p['func'].get()
        S.setprm()
        
    def compute(self):
        """run oscillator code"""
        rewind_nsteps = S.step - self.p['step'].get()
        if rewind_nsteps > 0:
            print 'rewinding', rewind_nsteps, 'steps, ',
            S.rewind(rewind_nsteps)  # adjust time and step
            print 'time =', S.time
        nsteps = int((self.p['tstop'].get()-S.time)\
                     /self.p['dt'].get())
        print 'compute', nsteps, 'new steps'
        self.setprm()  # notify S and oscillator about new parameters
        S.run(nsteps)
        # S.step is altered in S.run so update it:
        self.p['step'].set(S.step)
        
root = Tk()               # root (main) window
simviz = SimVizGUI(root)
root.mainloop()
