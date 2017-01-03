#!/usr/bin/env python
"""Class version of the simviz1.py script."""
import sys, math, getopt, os, shutil, scitools.misc

class SimViz:
    def __init__(self):
        self.cwd = os.getcwd()
        self.initialize()

    def initialize(self):
        """Set default values of all input parameters."""
        self.p = {}
        self.p['m'] = 1.0
        self.p['b'] = 0.7
        self.p['c'] = 5.0
        self.p['func'] = 'y'
        self.p['A'] = 5.0
        self.p['w'] = 2*math.pi
        self.p['y0'] = 0.2
        self.p['tstop'] = 30.0
        self.p['dt'] = 0.05
        self.p['case'] = 'tmp1'
        self.p['screenplot'] = 1

    def usage(self):
        s = '%s \n   [' % sys.argv[0]
        for name in self.p.keys():
            s += ' --' + name
        s += ' ]'
        return s
    
    def process_command_line_args(self, cmlargs):
        """Load data from the command line into self.p."""
        opt_spec = [ x+'=' for x in self.p.keys() ]
        try:
            options, args = getopt.getopt(cmlargs,'',opt_spec)

        except getopt.GetoptError:
            # illegal options
            print sys.exc_value  # explains what is wrong
            print self.usage()   # list command-line options
            sys.exit(1)

        for opt, val in options:
            key = opt[2:] # drop prefix --
            if   isinstance(self.p[key], float):  val = float(val)
            elif isinstance(self.p[key], int):    val = int(val)
            self.p[key] = val  

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
    adm.process_command_line_args(sys.argv[1:])
    adm.simulate()
    adm.visualize()
# end
