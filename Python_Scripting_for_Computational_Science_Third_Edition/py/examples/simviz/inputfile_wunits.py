#!/usr/bin/env python
"""
Parse files that set various parameters according to the following syntax:

set parametername = value ! comment

Example:

set boundary temperature = 2.2 ! applies to lower boundary

One can also specify values with an optional unit:

set parametername = value ! [unit] comment
set parametername = value unit ! [unit] comment

Example:

set heat flux = 10.2 ! [K/m] in-flux from basement
set heat flux = 10.2 K/cm ! [K/m] in-flux from basement

Lines with other instructions than set parameter = value are left untouched.

Usage:

>>> from inputfile_wunits import *
>>> import StringIO  # fake a file
>>> text = StringIO.StringIO('''\
... set alpha = 2.0  ! amplitude
... set beta = 1.1   ! [m**2] base area measured in square meters
... set gamma = 0.1 km/h ! [m/s]
... ''')
>>> parsed_lines, output_lines = parse_input_file(text.readlines())
>>> import pprint
>>> print pprint.pformat(parsed_lines)
[{'comment': '! amplitude',
  'parameter': 'alpha',
  'ref_unit': None,
  'unit': None,
  'value': 2.0},
 {'comment': '! [m**2] base area measured in square meters',
  'parameter': 'beta',
  'ref_unit': 'm**2',
  'unit': 'm**2',
  'value': 1.1000000000000001},
 {'comment': '! [m/s]',
  'parameter': 'gamma',
  'ref_unit': 'm/s',
  'unit': 'km/h',
  'value': 0.02777777777777778}]
>>> print pprint.pformat(output_lines)
['set alpha = 2.0 ! amplitude\n',
 'set beta = 1.1 ! [m**2] base area measured in square meters\n',
 'set gamma = 0.0277777777778 ! [m/s]\n']
>>> # set new values:
>>> new_values = {'alpha' : 3.3, 'beta' : '2 cm**2'}
>>> lines = prms2lines(new_values, parsed_lines)
>>> for line in lines:
...     print line,
... 
set alpha = 3.3 ! amplitude
set beta = 0.0002 ! [m**2] base area measured in square meters
set gamma = 0.0277777777778 ! [m/s]
"""

import sys, re, scitools.misc
from scitools.TkGUI import Parameters, AutoSimVizGUI
from Scientific.Physics.PhysicalQuantities import PhysicalQuantity
from Tkinter import *

def parse_input_file(lines):
    line_re = re.compile(r'set (.*?)\s*=\s*([^!]*)\s*(!?.*)')
    comment_re = re.compile(r'!\s*\[(.*?)\](.*)')
    value_re = re.compile(r'([0-9.Ee\-+]+)\s+([A-Za-z0-9*/.()]+)')
    parsed_lines = []        # list of dictionaries
    output_lines = []        # new lines without units

    for line in lines:
        m = line_re.search(line)
        # split line into parameter, value, and comment:
        if m:
            parameter = m.group(1).strip()
            value = m.group(2).strip()
            try:  # a comment is optional
                comment = m.group(3)
            except:
                comment = ''
            ref_unit = None; unit = None  # default values
            if comment:
                # does the comment contain a unit specification?
                m = comment_re.search(comment)
                if m:
                    ref_unit = m.group(1)
                    # is the value of the form 'value unit'?
                    m = value_re.search(value)
                    if m:
                        number, unit = m.groups()
                    else: # no unit, use the reference unit
                        number = value;  unit = ref_unit
                        value += ' ' + ref_unit
                    # value now has value _and_ unit
                    # convert unit to ref_unit:
                    pq = PhysicalQuantity(value)
                    pq.convertToUnit(ref_unit)
                    value = str(pq).split()[0]
            # convert value (str) to float, int, list, ..., str:
            value = scitools.misc.str2obj(value)
            output_lines.append('set %s = %s %s\n' % \
                                (parameter, value, comment))
            parsed_lines.append({'parameter' : parameter,
                                 'value' : value, # in ref_unit
                                 'ref_unit' : ref_unit,
                                 'unit' : unit,
                                 'comment' : comment})
        else:  # not a line of the form: set parameter = value
            output_lines.append(line)
            parsed_lines.append(line)
    return parsed_lines, output_lines

def lines2prms(parsed_lines, parameters=None):
    if parameters is None:
        parameters = Parameters(interface='GUI')
    for line in parsed_lines:
        if isinstance(line, dict):
            comment = line['comment']
            if line['ref_unit'] is not None:
                # parameter has value with unit:
                help = 'unit: '+line['ref_unit']+'; '+comment[1:]
                unit = line['ref_unit']
                str2type = float  # unit conversions -> float
            else:
                help = comment[1:]
                unit = None
                str2type = line['value'].__class__
            parameters.add(name=line['parameter'],
                           default=line['value'],
                           str2type=str2type,
                           widget_type='entry',
                           help=help, unit=unit)
    return parameters

def GUI(parameters, root):
    gui = AutoSimVizGUI()
    gui.make_prmGUI(root, parameters, height=300)
    Button(root, text='Quit', command=root.destroy).pack()
    root.mainloop()
    return parameters

def prms2lines(new_parameters, parsed_lines):
    output_lines = []
    for line in parsed_lines:
        if isinstance(line, str):
            output_lines.append(line)
        else:
            # line is a dictionary; turn it into a line
            prm = line['parameter']
            if prm in new_parameters:
                value = new_parameters[prm]
            else:
                value = line['value']
            comment = line['comment']
            output_lines.append('set %s = %s %s\n' % \
                                (prm, value, comment))
    return output_lines

# testing:

def _test():
    from StringIO import StringIO
    ifile = StringIO("""\
set gridfile = somefile.grid
set add boundary indicator nodes = n=1 b4=[0,1]x[-2,-2]
set time step = 0.5 h ! [s]
set heat heatflux 1 = 0.01  ! [K/m]
set heat heatflux 2 = 0.02  ! [K/m]
set time points for plot = [0, 1.5, 3, 10, 100]
sub heat LinEqAdmFE  ! submenu
sub heat Matrix_prm
set heat matrix type = MatSparse
set max no of iterations = 120
ok  ! return back to previous level
ok
ok
""")
    parsed_lines, dummy = parse_input_file(ifile.readlines())
    import pprint
    print 'parsed_lines:\n', pprint.pformat(parsed_lines)
    print 'output_lines:\n', pprint.pformat(dummy)
    # set some new values (with units :-)
    new_values = {}
    new_values['heat heatflux 1'] = '1 K/cm'
    new_values['heat heatflux 2'] = '4.5 K/cm'
    new_values['gridfile'] = 'my.grid'
    new_values['max no of iterations'] = 30
    lines = prms2lines(new_values, parsed_lines)
    print '\n\nnew input file:\n', ''.join(lines)


if __name__ == '__main__':
    filename = sys.argv[1]
    if filename == '_test':
        # run test function:
        _test()
        sys.exit(0)
        
    f = open(filename, 'r')
    lines = f.readlines()
    f.close()

    parsed_lines, new_lines = parse_input_file(lines)
    for line in new_lines:
        print line,  # (line has a trailing newline, print adds one so remove)
    for i in parsed_lines:
        print i
    root = Tk()  # Tk() must have been called before class Parameters works
    p = lines2prms(parsed_lines)
    p = GUI(p, root)  # read values from a GUI
    lines = prms2lines(p, parsed_lines)
    print '\n\n\nfinal output:\n'
    sys.stdout.writelines(lines)
    



                    
            

