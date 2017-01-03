#!/usr/bin/env python
"""
As simviz1cpGUI.py, but the parameters have dimensions.
Only the import statement for SimViz differs from simviz1cpGUI.py.
"""

import sys, os, math
from scitools.TkGUI import Parameters, AutoSimVizGUI
from simviz1cp_unit import SimViz  

class SimVizGUI(SimViz):
    def __init__(self, parent, layout='sort'):
        self.cwd = os.getcwd()        
        self.p = Parameters(interface='GUI')
        self.master = parent
        self.initialize()

        self.GUI = AutoSimVizGUI()

        if layout == 'sort':
            # widgets sorted in columns:
            self.GUI.make_prmGUI(self.master, self.p,
                                 sort_widgets=1,
                                 height=300, pane=1)
        else:
            # only one column of input parameters:
            self.GUI.make_prmGUI(self.master, self.p,
                                 sort_widgets=0,
                                 height=300, pane=0)

        help = """\
Simulate: run oscillator code for solving the
differential equation for the spring system.

Visualize: run Gnuplot to make plots in PNG and PostScript
format and on the screen (optional). Plots are stored
in the subdirectory with name equal to 'case'.
"""
        self.GUI.make_buttonGUI(self.master,
            buttons=[('Simulate', self.simulate),
                     ('Visualize', self.visualize)],
            logo=os.path.join(os.environ['scripting'],
                 'src','misc','figs','simviz2.xfig.t.gif'),
            help=None)
#           help=help)


if __name__ == '__main__':
    from Tkinter import *
    import Pmw
    root = Tk()
    Pmw.initialise(root)
    import scitools.misc; scitools.misc.fontscheme2(root)
    root.title('Oscillator GUI')
    try:    layout = sys.argv[1]
    except: layout = 'nosort'
    widget = SimVizGUI(root, layout)
    root.mainloop()
