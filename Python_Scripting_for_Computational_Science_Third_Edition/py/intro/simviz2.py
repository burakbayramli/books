#!/usr/bin/env python
"""
As simviz1.py but with an additional option to fix the yaxis scale
when running Gnuplot.
"""

import sys, math

# default values of input parameters:
m = 1.0; b = 0.7; c = 5.0; func = 'y'; A = 5.0; w = 2*math.pi
y0 = 0.2; tstop = 30.0; dt = 0.05; case = 'tmp1'
screenplot = True
# no specification of y axis in plots by default:
ymin = None; ymin = None  

# read variables from the command line, one by one:
while len(sys.argv) > 1:
    option = sys.argv[1];           del sys.argv[1]
    if   option == '-m':
        m = float(sys.argv[1]);     del sys.argv[1]
    elif option == '-b':
        b = float(sys.argv[1]);     del sys.argv[1]
    elif option == '-c':
        c = float(sys.argv[1]);     del sys.argv[1]
    elif option == '-func':
        func = sys.argv[1];         del sys.argv[1]
    elif option == '-A':
        A = float(sys.argv[1]);     del sys.argv[1]
    elif option == '-w':
        w = float(sys.argv[1]);     del sys.argv[1]
    elif option == '-y0':
        y0 = float(sys.argv[1]);    del sys.argv[1]
    elif option == '-tstop':
        tstop = float(sys.argv[1]); del sys.argv[1]
    elif option == '-dt':
        dt = float(sys.argv[1]);    del sys.argv[1]
    elif option == '-noscreenplot':
        screenplot = False
    elif option == '-case':
        case = sys.argv[1];         del sys.argv[1]
    elif option == '-yaxis':
        ymin = float(sys.argv[1]); ymax = float(sys.argv[2])
        del sys.argv[1]; del sys.argv[1]
    else:
        print sys.argv[0],': invalid option',option
        sys.exit(1)

# create a subdirectory:
d = case              # name of subdirectory
import os, shutil
if os.path.isdir(d):  # does d exist?
    shutil.rmtree(d)  # yes, remove old directory
os.mkdir(d)           # make new directory d
os.chdir(d)           # move to new directory d

# make input file to the program:
f = open('%s.i' % case, 'w')
# write a multi-line (triple-quoted) string with
# variable interpolation:
f.write("""
        %(m)g
        %(b)g
        %(c)g
        %(func)s
        %(A)g
        %(w)g
        %(y0)g
        %(tstop)g
        %(dt)g
        """ % vars())
f.close()
# run simulator:
cmd = 'oscillator < %s.i' % case  # command to run
failure = os.system(cmd)
if failure:
    print 'running the oscillator code failed\n%s\n%s' % \
    (cmd, output);  sys.exit(1)

# make file with gnuplot commands:
f = open(case + '.gnuplot', 'w')
f.write("""
set title '%s: m=%g b=%g c=%g f(y)=%s A=%g w=%g y0=%g dt=%g';
""" % (case, m, b, c, func, A, w, y0, dt))
if ymin is not None and ymax is not None:
    f.write("set yrange [%g:%g];\n" % (ymin,ymax))
if screenplot:
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
""" % (case, case))
f.close()
# make plot:
cmd = 'gnuplot -geometry 800x200 -persist ' + case + '.gnuplot'
failure = os.system(cmd)
if failure:
    print 'running gnuplot failed\n%s\n%s' % \
    (cmd, output);  sys.exit(1)






