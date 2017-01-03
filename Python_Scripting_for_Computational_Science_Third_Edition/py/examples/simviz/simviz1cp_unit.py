#!/usr/bin/env python
"""
As simviz1cp.py, but quantities may have physical dimension.
"""
import sys, math, getopt, os, shutil, re, scitools.misc
from scitools.TkGUI import Parameters

class SimViz:
    def __init__(self):
        self.cwd = os.getcwd()
        self.p = Parameters(interface='plain')
        self.initialize()

    def initialize(self):
        """Define all input parameters."""
        self.p.add('m', 1.0, float,
                   widget_type='slider', values=(0,5), help='mass',
                   unit='kg')
        self.p.add('b', 0.7, float,
                   widget_type='slider', values=(0,2), help='damping',
                   unit='kg/s')
        self.p.add('c', 5.0, float,
                   widget_type='slider', values=(0,20), help='stiffness',
                   unit='kg/s**2')
        # 'func' must be 'y' here to fix the dimension of 'c'
        self.p.add('func', 'y', str,
                   widget_type='option', values=('y',),
                   help='spring model function')
        self.p.add('A', '5.0 N', float,
                   widget_type='slider', values=(0,10),
                   help='forced amplitude', unit='N')
        self.p.add('w', 2*math.pi, float,
                   widget_type='entry', help='forced frequency',
                   unit='1/s')
        self.p.add('y0', 0.2, float,
                   widget_type='slider', values=(0,1),
                   help='initial displacement',
                   unit='m')
        self.p.add('tstop', 30.0, float,
                   widget_type='entry', help='stop time', unit='s')
        self.p.add('dt', 0.05, float,
                   widget_type='entry', help='time step', unit='s')
        self.p.add('case', 'tmp1', str,
                   widget_type='entry', help='case name')
        self.p.add('screenplot', 1, int,
                   widget_type='checkbutton',
                   help='plot on the screen?')

    def usage(self):
        return 'Usage: ' + sys.argv[0] + ' ' + self.p.usage()

    def simulate(self):
        os.chdir(self.cwd)
        case = self.p['case']   # abbreviation
        # create a subdirectory:
        d = case            
        if os.path.isdir(d):
            shutil.rmtree(d)
        os.mkdir(d)         
        os.chdir(d)         

        # make input file to the program:
        f = open('%s.i' % case, 'w')
        f.write('%(m)g\n%(b)g\n%(c)g\n%(func)s\n%(A)g\n%(w)g\n'\
                '%(y0)g\n%(tstop)g\n%(dt)g\n' % self.p)
        f.close()
        # run simulator:
        cmd = 'oscillator < %s.i' % case  # command to run
        scitools.misc.system(cmd)

    def visualize(self):
        # make file with gnuplot commands:
        case = self.p['case']
        f = open(case + '.gnuplot', 'w')
        f.write("set title '%(case)s: m=%(m)g b=%(b)g c=%(c)g "\
                "f(y)=%(func)s A=%(A)g w=%(w)g y0=%(y0)g "\
                "dt=%(dt)g';\n" % self.p)
        if self.p['screenplot']:
            f.write("plot 'sim.dat' title 'y(t)' with lines;\n")
        f.write("""
set size ratio 0.3 1.5, 1.0;  
# define the postscript output format:
set term postscript eps monochrome dashed 'Times-Roman' 28;
# output file containing the plot:
set output '%s.ps';
# basic plot command
plot 'sim.dat' title 'y(t)' with lines;
# make a plot in PNG format:
set term png small;
set output '%s.png';
plot 'sim.dat' title 'y(t)' with lines;
        """ % (case,case))
        f.close()
        # make plot:
        cmd = 'gnuplot -geometry 800x200 -persist '+case+'.gnuplot'
        scitools.misc.system(cmd)

if __name__ == '__main__':
    adm = SimViz()
    if len(sys.argv) > 1:
        if sys.argv[1] == '-h':
            print adm.usage(); sys.exit(0)
    adm.p.parse_options(sys.argv[1:])
    adm.simulate()
    adm.visualize()
    adm.p.name2attr()
# end
