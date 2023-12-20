# 
# Copyright (c) 2018-2020 steelpy


# Python stdlib imports
#import re
#from typing import NamedTuple
#

# package imports
from steelpy.process.io_module.text import search_line

#
def print_header_ellipse():
    #
    #
    printout = []
    printout.append("{:}\n".format(87 * "_"))
    printout.append("\n")
    printout.append("                                    Geometry                     Gammas Power Functions\n")
    printout.append("Section Type            Base   [a]  Height [b]  Theta[deg]            p          q\n")
    printout.append("{:}\n".format(87 * "."))
    printout.append("\n")
    
    return printout
# 
def print_header():
    #
    printout = []
    printout.append("{:_<87}\n".format(""))
    printout.append("\n")
    printout.append("{:28s} TUBULAR {:14s} WEB(i) {:12s} FLANGE(i)\n"
                    .format("", "", ""))
    
    printout.append("Section Type {:10s} Diameter   Thickness   Depth".format(""))
    printout[-1] += "   Thickness    Width    Thickness\n"
    
    printout.append("{:.<87}\n".format(""))
    printout.append("\n")
    return printout
#
def print_properties(section):
    """
    """
    printout = []
    printout.append("\n")
    printout.append("{:_<87}\n".format(""))
    printout.append("\n")
    printout.append("{:30s}SECTION DERIVED PROPERTIES\n".format(""))

    printout.append("Section Name {:3s} Area {:6s} Iy {:8s} Iz {:8s}"
                    .format("", "", "", ""))
    printout[-1] += " Zcy {:7s} SFy {:7s} J\n".format("", "")
    
    printout.append("{:28s} Zey {:7s} Zez {:7s} Zcz {:7s} SFz {:7s} Cw\n"
                    .format("", "", "", "", ""))

    printout.append("{:28s} Zpy {:7s} Zpz {:7s} SCy\n"
                    .format("", "", ""))

    printout.append("Axis System {:16s} ry {:8s} rz {:8s} SCz\n"
                    .format("", "", ""))
    
    printout.append("{:.<87}\n".format(""))
    #
    # Ouput
    #
    printout.append("{:15s} {: 1.4E} {: 1.4E} {: 1.4E} {: 1.4E}".
                    format(section.name, 
                           section.area.convert('millimetre^2').value, 
                           section.Iy.convert('millimetre^4').value,
                           section.Iz.convert('millimetre^4').value, 
                           section.Zc.convert('millimetre').value))
    try:
        printout[-1] += " {: 1.4E}".format( section.SFy.value)
    except AttributeError:
        printout[-1] += "{:12s}".format(" ")
    try:
        printout[-1] += " {: 1.4E}\n".format(section.J.convert('millimetre^4').value)
    except AttributeError:
        printout[-1] += "\n"

    
    printout.append("{:27s} {: 1.4E} {: 1.4E} {: 1.4E}".
                    format("", section.Zey.convert('millimetre^3').value, 
                           section.Zez.convert('millimetre^3').value, 
                           section.Yc.convert('millimetre').value))
    try:
        printout[-1] += " {: 1.4E}".format( section.SFz.value)
    except AttributeError:
        printout[-1] += "{:12s}".format("")
    try:
        printout[-1] += " {: 1.4E}\n".format(section.Cw.convert('millimetre^6').value)
    except TypeError:
        printout[-1] += "\n"    
    except AttributeError:
        printout[-1] += "\n"
    
    try:
        printout.append(" ^ {:5s} {:>18} {: 1.4E} {: 1.4E}".
                        format("z","", section.Zpy.convert('millimetre^3').value, 
                               section.Zpz.convert('millimetre^3').value))
    except AttributeError:
        printout.append(" ^ {:5s} {:>54}".format("z",""))
    try:
        printout[-1] += " {: 1.4E}\n".format(section.SCz.convert('millimetre').value)
    except AttributeError:
        printout[-1] += "\n"
    
    printout.append(" +  > {:5s} {:>15} {: 1.4E} {: 1.4E}".
                    format("y","", 
                           section.ry.convert('millimetre').value, 
                           section.rz.convert('millimetre').value))
    try:
        printout[-1] += " {: 1.4E}\n".format(section.SCy.convert('millimetre').value)
    except AttributeError:
        printout[-1] += "\n"
    
    return printout
#