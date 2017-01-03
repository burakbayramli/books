#!/usr/bin/env python
import sys, math, os, pexpect

# default values of input parameters:
m = 1.0; b = 0.7; c = 5.0; func = 'y'; A = 5.0; w = 2*math.pi
y0 = 0.2; tstop = 30.0; dt = 0.05; case = 'tmp1'; screenplot = 1
remote_host = 'magnum.uio.no'
user = 'douglas'
if not '-passwdfile' in sys.argv:
    import getpass
    password = getpass.getpass('Password on %s@%s: ' % (user, remote_host))
else:
    # get password from a secret and highly protected file on my laptop...
    f = open('/work/scripting/trunk/.pw_douglas', 'r')
    password = f.read().strip()
    f.close()

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
        screenplot = 0
    elif option == '-case':
        case = sys.argv[1];         del sys.argv[1]
    elif option == '-host':
        remote_host = sys.argv[1];  del sys.argv[1]
    elif option == '-user':
        user = sys.argv[1];         del sys.argv[1]
    else:
        print sys.argv[0],': invalid option',option
        sys.exit(1)


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

# generate script to be run on the remote machine:
f = open('run_%s.py' % case, 'w')
f.write("""\
#!/usr/bin/env python
import os, shutil, sys
d = '%(case)s'
if os.path.isdir(d): 
    shutil.rmtree(d)
os.mkdir(d) 
os.rename('%(case)s.i', '%(case)s/%(case)s.i') # move .i file
os.chdir(d) 
cmd = '../oscillator < %(case)s.i'
failure = os.system(cmd)   
if failure:
    print 'running the oscillator code failed'
    print output; sys.exit(1)
""" % vars())
f.close()

# copy files to remote host:
cmd = 'scp %(case)s.i run_%(case)s.py '\
      '%(user)s@%(remote_host)s:tmp' % vars()
print cmd
# run application on remote host using pexpect to feed password etc.
child = pexpect.spawn(cmd)
child.expect('password:')
child.sendline(password)
print 'before:', child.before
print 'after:', child.after
child.expect(pexpect.EOF)  # important; wait for end of scp session
child.close()
# can inspect the dialog that pexpect automates by printing
# child.before and child.after, see pexpect documentation

# run on remove host:
cmd = 'ssh %(user)s@%(remote_host)s '\
      '"cd tmp; python run_%(case)s.py"' % vars()
print cmd
child = pexpect.spawn(cmd)
child.expect('password:')
child.sendline(password)
child.expect(pexpect.EOF)  # important; wait for end of scp session

# copy result file back to local host:
cmd = 'scp %(user)s@%(remote_host)s:tmp/%(case)s/sim.dat .' % vars()
print cmd
child = pexpect.spawn(cmd)
child.expect('password:')
child.sendline(password)
child.expect(pexpect.EOF)  # important; wait for end of scp session

import time; time.sleep(2) # sleep to make scp finish
if not os.path.isfile('sim.dat'):
    print 'no result file sim.dat - script aborted...'
    
# make file with gnuplot commands:
f = open(case + '.gnuplot', 'w')
f.write("""
set title '%s: m=%g b=%g c=%g f(y)=%s A=%g w=%g y0=%g dt=%g';
""" % (case,m,b,c,func,A,w,y0,dt))
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
""" % (case,case))
f.close()
# make plot on local host:
cmd = 'gnuplot -geometry 800x200 -persist ' + case + '.gnuplot'
failure = os.system(cmd)
if failure:
    print 'running gnuplot failed'; sys.exit(1)

# clean up:
import time; time.sleep(3)  # let Gnuplot have some time for plotting...
for file in ('run_%s.py' % case, '%s.i' % case, 'sim.dat'):
    print 'removing', file;  os.remove(file)
