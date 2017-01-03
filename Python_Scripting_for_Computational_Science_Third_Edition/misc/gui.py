#!/usr/bin/env python

# automatically generated GUI script from the command
# generate_simvizGUI.py "-entry" "Young's modulus" "5000" "--Young's_modulus" "-slider" "Poisson's ratio" "0.25" "0:0.5" "--Poisson's_ratio" "-slider" "Plate thickness" "0.05" "0:0.4" "--thickness" "-slider" "Acceleration omega" "1" "0:5" "--omega" "-slider" "Acceleration impulse" "0.05" "0:2" "--impulse" "-slider" "Film gap" "0.2" "0:0.5" "--initial_gap" "-entry" "Viscosity" "1.0E-5" "--viscosity" "-option" "Gamma for gas" "0" "0:1.4" "--gamma" "-option" "Scheme" "backward" "Crank-Nicolson:backward" "--theta" "-help" "Interface to a squeeze film solver"

from scitools.ParameterInterface import Parameters, AutoSimVizGUI
import re, os, sys
# in case special buttons etc. are added with explicit
# Tkinter/Pmw code in this script, import these modules:
from Tkinter import *
import Pmw

class SimViz:
    def __init__(self):
        self.cwd = os.getcwd()
        self.p = Parameters(interface='plain')
        self.initialize()

    def initialize(self):
        """Define input parameters."""

        self.p.add("Young's modulus", "5000", str,
                   widget_type='entry')

        self.p.add("Poisson's ratio", "0.25", str,
                   widget_type='slider', values=['0', '0.5'])

        self.p.add("Plate thickness", "0.05", str,
                   widget_type='slider', values=['0', '0.4'])

        self.p.add("Acceleration omega", "1", str,
                   widget_type='slider', values=['0', '5'])

        self.p.add("Acceleration impulse", "0.05", str,
                   widget_type='slider', values=['0', '2'])

        self.p.add("Film gap", "0.2", str,
                   widget_type='slider', values=['0', '0.5'])

        self.p.add("Viscosity", "1.0E-5", str,
                   widget_type='entry')

        self.p.add("Gamma for gas", "0", str,
                   widget_type='option', values=['0', '1.4'])

        self.p.add("Scheme", "backward", str,
                   widget_type='option', values=['Crank-Nicolson', 'backward'])


    def usage(self):
        return 'Usage: ' + sys.argv[0] + ' ' + self.p.usage()

    def simulate(self):
        """Run simulator with input grabbed from the GUI."""
        if 'NOR' not in os.environ:
            print 'Diffpack is not accessible on this computer.'
            sys.exit(1)
        app = os.path.join(os.environ['NOR'],'doc','Book','src','app',
                           'SqueezeFilm','coupling','app')
        if not os.path.isfile(app):
            print 'The Diffpack application\n %s\nis not compiled' % app
            print 'Go to the directory and run Make MODE=opt'
            sys.exit(1)
        program = app
        cmd = program
        cmd += " --batch --Default Verify/test1b.i"
        cmd += " --Young's_modulus %s" % self.p["Young's modulus"]
        cmd += " --Poisson's_ratio %s" % self.p["Poisson's ratio"]
        cmd += " --thickness %s" % self.p["Plate thickness"]
        cmd += " --omega %s" % self.p["Acceleration omega"]
        cmd += " --impulse %s" % self.p["Acceleration impulse"]
        cmd += " --initial_gap %s" % self.p["Film gap"]
        cmd += " --viscosity %s" % self.p["Viscosity"]
        cmd += " --gamma %s" % self.p["Gamma for gas"]
        if self.p["Scheme"] == 'backward':
            theta = 1.0
        else:
            theta = 0.5
        cmd += " --theta %g" % theta

        os.system("RmCase SIMULATION")  # clean up previous runs
        failure = os.system(cmd)
        if failure:
            print "could not run", cmd
            sys.exit(1)

    def visualize(self):
        """Visualize solutions."""
        cmd = "..."
        failure = os.system(cmd)
        if failure:
            print 'could not run', cmd
            sys.exit(1)


class SimVizGUI(SimViz):
    def __init__(self, parent, layout=1):
        self.cwd = os.getcwd()        
        self.p = Parameters(interface='GUI')
        self.parent = parent
        self.initialize()

        self.GUI = AutoSimVizGUI()

        if layout == 1:
            # only one column of input parameters:
            self.GUI.make_prmGUI(self.parent, self.p,
                                 sort_widgets=0,
                                 height=300, pane=0)
        else:
            # widgets sorted in columns:
            self.GUI.make_prmGUI(self.parent, self.p,
                                 sort_widgets=1,
                                 height=300, pane=1)
        help = '''
Interface to a squeeze film solver
'''

        self.GUI.make_buttonGUI(self.parent,
            buttons=[('Simulate', self.simulate),
                     ('Visualize', self.visualize)],
            logo=None,
            help=help)  # help=None to avoid help button

        self.accl, self.defm, self.load = \
                  self.GUI.make_curveplotGUI(self.parent, 3,
                                             placement='right')

    def visualize(self):
        self.GUI.load_curveplot('..SIMULATION.curve_3',
                                self.accl, curvename='Acceleration')
        self.GUI.load_curveplot('..SIMULATION.curve_1',
                                self.defm, curvename='Displacement')
        self.GUI.load_curveplot('..SIMULATION.curve_2',
                                self.load, curvename='Pressure')

if __name__ == '__main__':
    # if no command-line arguments, run GUI:
    if len(sys.argv) == 1:
        root = Tk()
        Pmw.initialise(root)
        root.title('GUI')
        layout = 1
        widget = SimVizGUI(root, layout)
        root.mainloop()
    else:
        # command-line version
        adm = SimViz()
        if len(sys.argv) > 1:
            if sys.argv[1] == '-h':
                print adm.usage(); sys.exit(0)
        adm.p.parse_command_line(sys.argv[1:])
        adm.simulate()
        adm.visualize()

