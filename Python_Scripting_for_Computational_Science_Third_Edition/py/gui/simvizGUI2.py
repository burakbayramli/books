#!/usr/bin/env python
from Tkinter import *
import sys, math, os, shutil, tkMessageBox

class SimVizGUI:
    def __init__(self, parent):
        """Build the GUI."""
        self.master = parent
        
        # place all the sliders in a left frame:
        slider_frame = Frame(self.master)
        slider_frame.pack(side='left', padx=2, pady=2)
        
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

        # place the textentry widgets and the buttons in a frame
        # to the right of slider_frame:
        middle_frame = Frame(self.master)
        middle_frame.pack(side='left', anchor='n', padx=2, pady=2)

        # use a separate frame and the grid geometry manager to
        # pack the labels and entries in a table fashion (enables
        # nice alignment):
        entry_frame = Frame(middle_frame)
        entry_frame.pack(side='top', pady=22, padx=12)

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
        sketch_frame = Frame(self.master)
        sketch_frame.pack(side='left', padx=2, pady=2)
        gifpic = os.path.join(os.environ['scripting'],
                              'src','misc','figs','simviz2.xfig.t.gif')
        self.sketch = PhotoImage(file=gifpic)
        # (images must be tied to a global or class variable in Py1.5)
        Label(sketch_frame, image=self.sketch).pack(side='top', pady=20)

        self.cwd = os.getcwd()  # current working directory

    def quit(self,event=None):
        """Destroy GUI; called from Quit button or 'q' binding."""
        self.master.destroy()

    def textentry(self, parent, variable, label):
        """Make a textentry field tied to variable."""
        # pack a label and entry horizontally in a frame:
        l = Label(parent, text=label)
        l.grid(column=0, row=self.row_counter, sticky='w')
        widget = Entry(parent, textvariable=variable, width=8)
        widget.grid(column=1, row=self.row_counter)
        #widget.grid(column=1, sticky='w')
        self.row_counter += 1
        return widget

    def slider(self, parent, variable, low, high, label):
        """Make a slider [low,high] tied to variable."""
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

    def compute(self):
        """Run simviz1.py."""
        # add simviz1.py's directory to PATH:
        os.environ['PATH'] += os.pathsep + os.path.join(
            os.environ['scripting'], 'src', 'py', 'intro')
        cmd = 'simviz1.py '
        # join options; -X self.p['X'].get()
        opts = ['-%s %s' % (prm, str(self.p[prm].get()))
                for prm in self.p]
        cmd += ' '.join(opts)
        print cmd
        failure = os.system(cmd)
        if failure:
            tkMessageBox.Message(icon='error', type='ok',
                message='Underlying simviz1.py script failed',
                title='Error').show()

root = Tk()               # root (main) window
simviz = SimVizGUI(root)
root.mainloop()
