#!/usr/bin/env python
"""
Administer compilation and linking of Fortran 77 programs.
Aimed at experimenting with compiler and optimization options
in a benchmark problem.
"""
import sys, os, re
from scitools.misc import findprograms

if len(sys.argv) < 5:
    print 'Usage: %s programname file1.f file2.f ... inputfile comment' \
          % sys.argv[0]
    sys.exit(1)

programname = sys.argv[1]
inputfile   = sys.argv[-2]
comment     = sys.argv[-1]
f77files    = sys.argv[2:-2]
    
    
compile_flags = '-c'
link_flags = '-o %s' % programname
libs = ''

# build a nested dictionary with platform, compiler, and options
# (the name cd stands for compiler data)
cd = {}

c1 = 'basunus.ifi.uio.no'  # computer 1
cd[c1] = {}
cd[c1]['data'] = 'Linux 2.2.15 i686, 500 MHz, 128 Mb'

c2 = 'skidbladnir.ifi.uio.no'  # computer 2
cd[c2] = {}
cd[c2]['data'] = 'SunOS 5.7, sparc Ultra-5_10'

c3 = 'pico'  # computer 3
cd[c3] = {}
cd[c3]['data'] = 'HP-UX B.11.11'

# define dictionaries for compiler properties and flags:

# g77 compiler:
g77 = {
    'name'          : 'g77',
    'description'   : 'GNU f77 compiler, v2.95.4',
    'compile_flags' : compile_flags + ' -pg',
    'link_flags'    : link_flags + ' -pg',
    'libs'          : libs,
    'test_flags' :
    ['-O0', '-O1', '-O2', '-O3','-O3 -ffast-math -funroll-loops',],
    'platform_specific_compile_flags' : {},
    'platform_specific_link_flags' : {},
    'platform_specific_libs' : { c1 : '-lf2c' },
    }

# Fujitsu f95 compiler:
f95_Fujitsu = {
    'name' : 'f95',
    'description'    : 'Fujitsu Fortran compiler, v1.0',
    'compile_flags'  : compile_flags,
    'link_flags'     : link_flags,
    'libs'           : libs,
    'test_flags' :
    ['-O0', '-O1', '-O2', '-O3', '-O3 -Kloop -KPENTIUM_PRO -x -',],
    'platform_specific_compile_flags' : {},
    'platform_specific_link_flags' : { c1 : '-static' },
    'platform_specific_libs' : { c1 : '-lf2c' },
    }

# Sun f77 compiler:
Sunf77 = {
    'name' : 'f77',
    'description' : 'Sun f77 compiler, v5.2',
    'compile_flags' : compile_flags,
    'link_flags'    : link_flags,
    'libs'          : '',
    'test_flags' :
    ['-O0', '-O1',  '-fast',],
    'platform_specific_compile_flags' : {},
    'platform_specific_link_flags' : {},
    'platform_specific_libs' : {},
    }

# Sun f95 compiler:
Sunf95 = {
    'name' : 'f95',
    'description' : 'Sun f95 compiler, v6.1',
    'compile_flags' : compile_flags,
    'link_flags'    : link_flags,
    'libs'          : '',
    'test_flags' :
    ['-O0', '-O1',  '-fast',],
    'platform_specific_compile_flags' : {},
    'platform_specific_link_flags' : {},
    'platform_specific_libs' : {},
    }

# HP f77 compiler:
HPf77 = {
    'name' : 'f77',
    'description' : 'HP f77 compiler',
    'compile_flags' : compile_flags + ' -p',
    'link_flags'    : link_flags + ' -p',
    'libs'          : '',
    'test_flags' :
    ['-O0', '-O1', '-O2', '-O3',],
    'platform_specific_compile_flags' : {},
    'platform_specific_link_flags' : {},
    'platform_specific_libs' : {},
    }

# HP f90 compiler:
HPf90 = {
    'name' : 'f90',
    'description' : 'HP f90 compiler',
    'compile_flags' : compile_flags + ' -p',
    'link_flags'    : link_flags + ' -p',
    'libs'          : '',
    'test_flags' :
    ['-O0', '-O1', '-O2', '-O3',],
    'platform_specific_compile_flags' : {},
    'platform_specific_link_flags' : {},
    'platform_specific_libs' : {},
    }

# add list of compilers on the various hosts
cd[c1]['compilers'] = [g77, f95_Fujitsu]
cd[c2]['compilers'] = [g77, Sunf77, Sunf95]
cd[c3]['compilers'] = [HPf90] # [g77, HPf77, HPf90]


# ------------------------------------------------------------------------

def run_profiler(programname):
    """grab data from gprof/prof output and format nicely"""

    # gprof needs gmon.out (from the last execution of programname)
    if os.path.isfile('gmon.out'):
        # run gprof:
        if not findprograms(['gprof']):
            print 'Cannot find gprof'
            return
        res = os.popen('gprof ' + programname)
        lines = res.readlines()
        failure = res.close()
        if failure:
            print 'Could not run gprof'; return
        # grab the table from the gprof output:
        for i in range(len(lines)):
            if re.search(r'\%\s+cumulative\s+self', lines[i]):
                startline = i
                break
        try:
            # we are interested in the 10 first lines of the table,
            # but if there is a blank line, we stop there
            stopline = 10
            i = 0
            for line in lines[startline:startline+stopline]:
                if re.search(r'^\s*$', line):
                    stopline = i;  break
                i = i + 1
            table = ''.join(lines[startline:startline+stopline])
            print table
            os.remove('gmon.out') # require new file for next run...
        except:
            print 'Could not recognize a table in gmon.out...'; return
    elif os.path.isfile('mon.out'):
        # run prof:
        if not findprograms(['prof']):
            print 'Cannot find gprof'
            return
        res = os.popen('prof ' + programname)
        lines = res.readlines()
        failure = res.close()
        if failure:
            print 'Could not run prof'; return
        for line in lines[0:10]: print line,
        
    else:  # no gmon.out or mon.out, cannot run gprof or prof
        print programname,\
              'was not compiled in profiling mode (-pg or -p?)'
        return

# convenient function for concatenating strings with blank
# as delimiter:
def join(*strings):
    res = ''
    for s in strings:
        if isinstance(s, basestring):  # string?
            res += s + ' '
        elif type(s) == type([]) or type(s) == type(()):
            res += ' '.join(s) + ' '
    return res

# ------------------------------------------------------------------------
# run through the various compiler and options for the
# present host:
host = os.environ['HOST']
for compiler in cd[host]['compilers']:

    print '\n\nRunning on %s: %s compiler (%s)' % \
          (host, compiler['name'], compiler['description'])
    for optimization_flags in compiler['test_flags']:
        # construct compilation command:
        compile = join(compiler['name'],
                       compiler['compile_flags'],
                       optimization_flags)
        if host in compiler['platform_specific_compile_flags']:
            compile += ' ' + compiler['platform_specific_compile_flags'][host]
        compile += ' ' + ' '.join(f77files)
        print compile
        failure1 = os.system(compile)

        # construct link command:
        objfiles = ''
        for file in f77files:
            ofile = re.sub(r'\.f$', '.o', file)
            ofile = os.path.basename(ofile)  # skip the path
            objfiles +=  ofile + ' '
        link = join(compiler['name'], compiler['link_flags'])
        if host in compiler['platform_specific_link_flags']:
            link += ' ' + compiler['platform_specific_link_flags'][host]
        link += ' ' + objfiles + ' ' + compiler['libs']
        if host in compiler['platform_specific_libs']:
            link += ' ' + compiler['platform_specific_libs'][host]
        print link
        failure2 = os.system(link)
        if not os.path.isfile(programname):
            print 'unsuccessful building of', programname
        else:
            # run benchmark:
            run = './%s < %s > tmp.out' % (programname,inputfile)
            print run
            t0 = os.times()
            failure = os.system(run)
            t1 = os.times()
            cpu_time = t1[2] + t1[3] - (t0[2] + t0[3])
            print 'CPU-time: %6.2f  ' % cpu_time, compiler['name'],\
                  optimization_flags, '>>>', comment
            # (could do timings of child process)
            if failure:
                print 'Unsuccessful execution of', run
            else:
                run_profiler(programname)


    
