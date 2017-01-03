This directory contains the source distribution for gPy. gPy is free
software, but it is Copyright (C) 2007 The University of York. It is
distributed under the terms of the GNU General Public License.  See
the file COPYING for copying permission.

WHAT IS GPY?

gPy is a collection of Python modules developed to support the
teaching of "Algorithms for Graphical Models" at the University of
York.

DOCUMENTATION
You can find HTML documentation for the API in Doc/API

UNITTESTS
cd to the test directory and do "python runtest.py" to do all tests.

REQUIREMENTS

Python 2.4 or higher. The installation procedure assumes you are using
Linux, but only one module is not pure Python, so it should not be too
difficult to get working under Windows.

PRE-INSTALLATION INSPECTION

To run the scripts in the Scripts directory, just cd to that directory
and ensure that Python can reach the gPy package in the gPy
directory. Doing

setenv PYTHONPATH ..

should do. Best to do this in a new shell to avoid messing up any
existing value for PYTHONPATH. Run each script with "python <<SCRIPTNAME>>"

INSTALL

Just type:
python setup install

or, if you want this installed in your personal space:

python setup.py install --home=~

CONTACT:

Send comments, bug reports, etc to jc-gpy@cs.york.ac.uk
