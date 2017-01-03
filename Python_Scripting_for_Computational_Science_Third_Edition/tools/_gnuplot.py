#!/usr/bin/env python
"""
Cross-platform executing of the Gnuplot plotting program with a
Gnuplot scriptfile as argument. This _gnuplot.py script accepts the
same command-line arguments as on Unix systems, but on
Windows the scriptfile (if present) is renamed to GNUPLOT.INI
and gnuplot is run without arguments.
"""

import sys, os

if len(sys.argv) < 2:
    print 'Error: not enough arguments!'; sys.exit(1)

if sys.platform.startswith('win'):
    # parse the command-line; it's easy since we drop all X11 options and
    # only need to grab the scriptfile name, and it's the last argument:
    if len(sys.argv) > 1:
        scriptfile = sys.argv[-1]
        if os.path.isfile('GNUPLOT.INI'):
            os.remove('GNUPLOT.INI')
        os.rename(scriptfile, 'GNUPLOT.INI')
    failure = os.system('wgnuplot')  # Gnuplot 4.0
    if failure:
        print '''Could not find Gnuplot on Windows.
              The name of the file is wgnupl32.exe. Either you have
              not installed Gnuplot or the path to the .exe file is
              not registered in your %PATH% environment variable.
              '''
        sys.exit(1)
elif os.name == 'posix':
    if len(sys.argv) > 1:
        failure = os.system('gnuplot ' + ' '.join(sys.argv[1:]))
    else:
        failure = os.system('gnuplot')
    if failure:
        print 'Could not run Gnuplot.'
        sys.exit(1)
else:
    print 'Platform', sys.platform, 'on OS', os.name, 'is not supported'
    sys.exit(1)
