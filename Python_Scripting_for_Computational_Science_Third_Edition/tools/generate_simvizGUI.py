#!/usr/bin/env python
import os, sys, re
"""
Automatic generation of a GUI for running a simulation and
visualization program. Based on class SimVizPrmGUI.
"""

usage="""
Usage: %s \ 
   -slider label default low:high cml-option \ 
   -entry label default cml-option \ 
   -option label default choice1:choice2:choice3:... cml-option \ 
   -toggle label default \
   -help description
          
Example: %s \ 
   -entry sigma 0.12 -s  \ 
   -option verbose off on:off -v \ 
   -slider 'stop time' 140 '0:200' -tstop
""" % (sys.argv[0],sys.argv[0])

if len(sys.argv) <= 1:
    print usage; sys.exit(1)
    
inputline = ' '.join(['"%s"' % x for x in sys.argv[1:]])

selfp_decl = ""   # self.p[name] = InputPrmGUI(... type declarations
labels = []
cmlopts = []
help = None

out = sys.stderr  # output for command-line option debugging
out.write('\n\n')

while len(sys.argv) > 1:
    opt = sys.argv[1];  del sys.argv[1]
    out.write(opt + '; ')
    if opt == "-slider":
        label = sys.argv[1];  del sys.argv[1]
        labels.append(label)
        out.write('label: %s ' % label)
        default = sys.argv[1];  del sys.argv[1]
        out.write('default: %s ' % default)
        values = sys.argv[1].split(":");  del sys.argv[1]
        out.write('values: %s ' % values)
        cmlopt = sys.argv[1];  del sys.argv[1]
        cmlopts.append(cmlopt)
        out.write('command-line option: %s ' % cmlopt)
        # treat all values/default as strings
        selfp_decl += """
        self.p.add("%s", "%s", str,
                   widget_type='slider', values=%s)
""" % (label, default, str(values))

    if opt == "-option":
        label = sys.argv[1];  del sys.argv[1]
        labels.append(label)
        out.write('label: %s ' % label)
        default = sys.argv[1];  del sys.argv[1]
        out.write('default: %s ' % repr(default))
        values = sys.argv[1].split(":");  del sys.argv[1]
        out.write('values: %s ' % values)
        cmlopt = sys.argv[1];  del sys.argv[1]
        cmlopts.append(cmlopt)
        out.write('command-line option: %s ' % cmlopt)
        # treat all values/default as strings
        selfp_decl += """
        self.p.add("%s", "%s", str,
                   widget_type='option', values=%s)
""" % (label, default, str(values))

    if opt == "-entry":
        label = sys.argv[1];  del sys.argv[1]
        labels.append(label)
        out.write('label: %s ' % label)
        default = sys.argv[1];  del sys.argv[1]
        out.write('default: %s ' % default)
        cmlopt = sys.argv[1];  del sys.argv[1]
        cmlopts.append(cmlopt)
        out.write('command-line option: %s ' % cmlopt)
        # treat default as string
        selfp_decl += """
        self.p.add("%s", "%s", str,
                   widget_type='entry')
""" % (label, default)

    if opt == "-toggle":
        label = sys.argv[1];  del sys.argv[1]
        labels.append(label)
        out.write('label: %s ' % label)
        default = sys.argv[1];  del sys.argv[1]
        out.write('default: %s ' % default)
        cmlopt = sys.argv[1];  del sys.argv[1]
        cmlopts.append(cmlopt)
        out.write('command-line option: %s ' % cmlopt)
        # treat default as integer
        selfp_decl += """
        self.p.add("%s", "%s", str,
                   widget_type='checkbutton')
""" % (label, default)

    if opt == "-help":
        help = sys.argv[1];  del sys.argv[1]
        out.write('help: %s ' % help)
    out.write('\n')

if help is None:
    helpstr = "None"
else:
    helpstr = "'''\n%s\n'''" % help
    
code = '''\
#!/usr/bin/env python

# automatically generated GUI script from the command
# %s %s

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
''' % (os.path.basename(sys.argv[0]), inputline)

code += selfp_decl

code += '''

    def usage(self):
        return 'Usage: ' + sys.argv[0] + ' ' + self.p.usage()

    def simulate(self):
        """Run simulator with input grabbed from the GUI."""
        program = '...someprogram...'
        cmd = program
'''

for i in range(len(labels)):
    opt = cmlopts[i]; label = labels[i]
    code += '''        cmd += """ ''' + opt + \
            ''' "%s" """ % self.p["''' + label + '''"]\n'''

code += '''
        if not os.path.isfile(program):
            print program, 'not found - have you compiled the application?'
            sys.exit(1)
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
        help = %s

        self.GUI.make_buttonGUI(self.parent,
            buttons=[('Simulate', self.simulate),
                     ('Visualize', self.visualize)],
            logo=None,
            help=help)  # help=None to avoid help button

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
''' % (helpstr)

"""
Documentation of how to extend this script:

not yet written...
"""
        
print code
