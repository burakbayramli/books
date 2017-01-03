#!/usr/bin/env python

import os, sys

print

# check if various modules are available

import scitools.modulecheck as modulecheck

modulecheck.message("Tkinter")
modulecheck.message("Pmw")
modulecheck.PmwBlt(verbose=1)
    
n1 = modulecheck.message("numpy", msg="for numerical computations")
if not n1:
    print """
    You must install the numpy package.
    """
modulecheck.message("Gnuplot", msg="curve plotting tool")
modulecheck.message("Scientific", msg="for numerical computations")
modulecheck.message("scipy", msg="for numerical computations")

# these modules are referred to in the book, but not critical:
modulecheck.message("pymat", critical=0,
                    msg="Python-Matlab interface")
#modulecheck.message("GuiAppD", critical=0,
#     msg="Part of the PmwContribD package with many Pmw extensions")
#modulecheck.message("TableIO", critical=0,
#                    msg="reading of text file data into NumPy arrays (in C)")
#modulecheck.message("Itpl15", critical=0,
#                    msg="enables Perl-like interpolation in Python code")

from scitools.misc import findprograms

programs = {
    'gnuplot'    : 'plotting program',
    'gs'         : 'ghostscript, ps/pdf interpreter and previewer',
    'ps2pdf'     : 'ps/pdf converter, part of ghostscript',
    'convert'    : 'image conversion, part of the ImageMagick package',
    'animate'    : 'animation viewer, part of the ImageMagick package',
    'display'    : 'image viewer, part of the ImageMagick package',
    'latex'      : 'mathematical typesetting tool',
    'dvips'      : 'tool for converting latex to ps',
    'happydoc'   : 'generation of Python code documentation from doc strings',
    'epydoc'     : 'generation of Python code documentation from doc strings',
    'f2py'       : 'generator for Python interfaces to F77',
    'swig'       : 'generator for Python/Perl/Tcl interfaces to C/C++',
    }

installed = findprograms(programs.keys())
for program in installed.keys():
    if installed[program] is not None:
        print "You have %s (%s)\n   in %s" % \
              (program, programs[program], installed[program])
    else:
        print "*** Program", program, "was not found on the system"
        if sys.platform.startswith('win') and program in ('latex', 'dvips'):
            print "*** You don't have latex/dvips but on Windows this is usual"
        elif sys.platform.startswith('win') and program == 'gs':
            # no link/script turning C:\gs\gs814... into 'gs', that's ok
            pass
        else:
            print "           .....(%s)" % programs[program]

# check if Gnuplot is compiled with PNG support
# (we test this on Unix only where PNG support can be forgotten in
# the manual build process; on Windows the Gnuplot program does not
# have a command-line prompt, it opens in a separate GUI so os.popen
# will not work):
if os.name == 'posix':
    gnuplot = os.popen("gnuplot", "w")
    gnuplot.write("""
    set term png; exit
    """)
    failure = gnuplot.close()
    if failure:
        print "*** gnuplot is not compiled with PNG suppert (--with-png)"
    else:
        print "You have gnuplot with PNG suppert"

# check environment variables:
if 'scripting' not in os.environ:
    print """
*** The environment variable 'scripting' is not defined!
    You need to set that variable in a set-up file for your environment.
    (It should point to the parent dir of the src dir containing
    the scripting examples from the book, check ch. 1 in the book.)
"""
    if sys.platform.startswith('win'):
        print """
    On Windows you can write something like
        set scripting=C:\Documents and Settings\hpl\My Documents\scripting
    in the C:\autoexec.bat file
"""
    else: 
        print """
    In a Bash environment can add something like this to the .bashrc file:
        export scripting=$HOME/scripting
"""

else:
    print 'The "scripting" environment variable is set to\n  %s' % \
          os.environ['scripting']
    # check that $scripting/src/tools is in the PYTHONPATH variable:
    # (not necessary anymore)
'''
    scripting_bin = os.path.join(os.environ["scripting"],"src","tools")
    if os.environ.get("PYTHONPATH","").find(scripting_bin) == -1:
        print """
*** %s
    is not contained in the PYTHONPATH variable. Add this directory
    to the PYTHONPATH variable in the set-up file for your environment.
    In a Bash environment you can add this line to the .bashrc file:
        export PYTHONPATH=$PYTHONPATH:$scripting/src/tools
""" % scripting_bin
'''

if not os.path.join(os.environ['scripting'], 'src', 'tools') in \
   os.environ['PATH'].split(os.pathsep):
    print """
*** The PATH variable does not contain the src/tools directory.
"""
if 'PYTHONSRC' not in os.environ:
    print """
*** The PYTHONSRC environment variable, pointing to the root directory
    of your Python source code tree, is not defined, and this variable
    can be convenient to have (see Appendix A in the book).
"""
else:
    print 'The "PYTHONSRC" environment variable is set to\n  %s' % \
          os.environ['PYTHONSRC']

if os.name == 'posix':
    if 'SYSDIR' not in os.environ:
        print """
*** The SYSDIR environment variable, pointing to the root directory
    of your tree of external software sources, is not defined.
    It can be convenient to define it (see Appendix A in the book).
"""
    elif 'MACHINE_TYPE' not in os.environ:
        # check MACHINE_TYPE only if SYSDIR is defined...
        print """
*** The MACHINE_TYPE environment variable, reflecting the hardware
    platform (often set equal to uname -a on Unix machines),
    is not defined. It can be convenient to define it (see Appendix A
    in the book).
"""

        
installed = findprograms(['oscillator'])
if installed['oscillator']:
    print "You have the 'oscillator' simulator\n   in %s" % \
          installed['oscillator']
else:
    if os.name == 'posix':
        print """
*** The 'oscillator' program was not found.
    Go to $scripting/src/app/oscillator/F77 and run make.sh to install it
    on Unix machines.
"""
        if 'MACHINE_TYPE' not in os.environ:
            print """
    You have to define the MACHINE_TYPE variable for getting
    make.sh to work.
    If you are on a Windows machine, it's easier to copy
    src/app/oscillator/Python/oscillator.py to 'oscillator' in
    some directory contained in the PATH variable.
"""
    elif sys.platform.startswith('win'):
        print """
*** The oscillator program was not found on Windows. This is strange...
    %scripting%\src\tools\oscillator.bat should always work...
"""



print """
Check out lines starting with ***, they point out missing tools/variables etc.
"""

