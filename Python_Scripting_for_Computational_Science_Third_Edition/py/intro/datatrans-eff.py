#!/usr/bin/env python

# testing the efficiency of reading 100000 (x,y) data points
# from file and writing (x,f(y)) data back to file again

# note: there may be some overhead in calling os.system;
# compare with datatrans-eff.sh, which is an identical script,
# except that it is written in Bourne shell

import os, commands  # note: commands does not work on Windows

print 'generating input file with (x,y) coordinates... (takes some time)'
cmd = os.path.join(os.environ['scripting'], 'src', 'efficiency',
                   'datatrans','xygenerator.py') \
      + ' 0:10,0.0001 x*x > datatrans.tmp'
failure, output = commands.getstatusoutput('python ' + cmd)
print ' '

def runtest(dirname, file, description, results):
    path = os.path.join(dirname,file)
    print 'running', path, '\n   ' + description
    # (note: joining dirname and file may fail on Windows if directories in
    # the dirname path contain blanks, then it's better to add dir to PATH)
    t0 = os.times()
    failure, output = commands.getstatusoutput(path + ' datatrans.tmp tmp.1')
    t1 = os.times()
    cpu = t1[3] + t1[2] - t0[3] - t0[2]
    print '....CPU time: %.1f, user time: %.1f, system time: %.3f' % \
          (cpu, t1[2]-t0[2], t1[3]-t0[3])
    results.append((description, file, float(cpu)))

root = os.path.join(os.environ['scripting'], 'src', 'py', 'intro')
res = []  # collect results (CPU time) and associated problem description
runtest(root, 'datatrans1.py', 'plain Python', res)
runtest(root, 'datatrans2.py', 'Python w/plain lists', res)
runtest(root, 'datatrans3a.py', 'Python w/NumPy arrays and filetable', res)
runtest(root, 'datatrans3b.py', 'Python w/NumPy arrays and TableIO', res)
runtest(root, 'datatrans3c.py', 'Python w/NumPy arrays and split of file.read()', res)
runtest(root, 'datatrans3d.py', 'Python w/NumPy arrays and Scientific.IO.ArrayIO', res)

root = os.path.join(os.environ['scripting'], 'src', 'perl')
runtest(root, 'datatrans1.pl', 'plain Perl', res)

root = os.path.join(os.environ['scripting'], 'src', 'tcl')
runtest(root, 'datatrans1.tcl', 'plain Tcl', res)

print '\ncompiling C and C++ codes in scripting/src/misc/datatrans'
thisdir = os.getcwd()
os.chdir(os.path.join(os.environ['scripting'], 'src', 'efficiency',
                      'datatrans', 'C'))
failure, output = commands.getstatusoutput('./make.sh')
os.chdir('../C++')
failure, output = commands.getstatusoutput('./make.sh')
os.chdir(thisdir)
print ' '
root = os.path.join(os.environ['scripting'], 'src', 'efficiency',
                    'datatrans', 'C')
runtest(root, 'datatrans1.app', 'plain C', res)
root = os.path.join(os.environ['scripting'], 'src', 'efficiency',
                    'datatrans', 'C++')
runtest(root, 'datatrans1.app', 'plain C++', res)
runtest(root, 'datatrans1_eff.app', 'buffered output in C++', res)

# clean up:
#rm -f datatrans.tmp tmp.1 \
#  $scripting/src/misc/datatrans/C/datatrans1.app \
#  $scripting/src/misc/datatrans/C++/datatrans1.app

cpu_times = [cpu for descr, file, cpu in res]
best = min(cpu_times)
print '\n\nMinimum CPU time:', best
print 'Normalized values:'
for descr, file, cpu in res:
    print "%-20s  %s: %.1f" % (file, descr, cpu/best)

    
    




