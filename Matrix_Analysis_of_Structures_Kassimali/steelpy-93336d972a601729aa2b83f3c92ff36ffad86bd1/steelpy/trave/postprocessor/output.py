# Copyright (c) 2009 steelpy
#
from __future__ import annotations
# Python stdlib imports
from dataclasses import dataclass
from typing import NamedTuple
#
# package imports
from steelpy.utils.dataframe.main import DBframework
#
#
#
#
# --------------------
# printing
# --------------------
#
#
def print_ndeflections(dispresult, plane):
    """
    """
    output = "\n"
    output += "{:}\n".format(52 * '-')
    output += "** Node Displacements\n"
    output += "{:}\n".format(52 * '-')
    #
    if plane.plane2D:
        header2 = "     node     x [ m]     y [ m]   rz [rad]\n"
    else:
        header2 = "     node     x [ m]     y [ m]     z [ m]   rx [rad]   ry [rad]   rz [rad]\n"    
    #
    ndgrp = dispresult.groupby('load_type')
    ndtype = ndgrp.get_group('basic')
    nditems = ndtype.groupby('load_name')
    #
    # Basic
    for key, wk in nditems:
        header1 =  wk[['load_name', 'load_number', 'load_title', 'load_system']].values
        output += "-- Basic Load  Name: {:}  Number: {:}  Title: {:}  System: {:}\n".format(*header1[0])
        output += header2
        output += printout(wk.node_name, wk[plane.hdisp].values)
        output += "\n"
    #
    # Combination
    try:
        ndtype = ndgrp.get_group('combination')
        nditems = ndtype.groupby('load_name')
        #
        output += '{:}\n'.format(52 * '-')
        for key, wk in nditems:
            header1 =  wk[['load_name', 'load_number', 'load_title', 'load_system']].values
            output += "-- Load Combination  Name: {:}  Number: {:}  Title: {:}  System: {:}\n".format(*header1[0])
            output += header2
            ndsum =  wk.groupby('node_name')[plane.hdisp].sum()
            output += printout(ndsum.index, ndsum.values)
            output += "\n"
    except KeyError:
        pass
    #
    return output
#
def print_nreactions(reactions, plane):
    """
    :return:
    """
    output = "\n"
    output += "{:}\n".format(52 * '-')
    output += "** Node Reactions\n"
    output += "{:}\n".format(52 * '-')
    #
    if plane.plane2D:
        header2 = "     node    Fx [ N]    Fy [ N]  Mz[ N-m ]\n"
    else:
        header2 = "     node    Fx [ N]    Fy [ N]    Fz [ N]  Mx[ N-m ]  My[ N-m ]  Mz[ N-m ]\n"
    #
    nreacgrp =  reactions.groupby('load_type')
    nrtype = nreacgrp.get_group('basic')
    nritems = nrtype.groupby('load_name')
    #
    # Basic
    for key, wk in nritems:
        header1 =  wk[['load_name', 'load_number', 'load_title', 'load_system']].values
        output += "-- Basic Load  Name: {:}  Number: {:}  Title: {:}  System: {:}\n".format(*header1[0])
        output += header2
        output += printout(wk.node_name, wk[plane.hforce].values)
        output += "\n"
    #
    # Combinations
    try:
        nrtype = nreacgrp.get_group('combination')
        nritems = nrtype.groupby('load_name')
        #
        output += '{:}\n'.format(52 * '-')
        for key, wk in nritems:
            header1 =  wk[['load_name', 'load_number', 'load_title', 'load_system']].values
            output += "-- Load Combination  Name: {:}  Number: {:}  Title: {:}  System: {:}\n".format(*header1[0])
            output += header2
            output += printout(wk.node_name, wk[plane.hforce].values)
            output += "\n"
    except KeyError:
        pass
    #
    return output 
#
#
def get_max_displacement(dispp):
    """ """
    columns = list(zip(*dispp))
    #
    maxval = []
    nodeitem = []
    #
    output = "\n"
    output += "Maximum displacements\n"
    for column in columns:
        nmax = [max(column), min(column)]
        maxval.append(nmax[0] if nmax[0] > abs(nmax[1]) else nmax[1])
        nodeitem.append(column.index(maxval[-1]))
    #
    output += "node {:>10}/n".format("")
    for _node in nodeitem:
        output += "{:}{:>10}/n".format(_node, "")
    #
    output = "\n"
    output += "value \n"
    for _value in maxval:
        output += "{: 1.3e} \n".format(_value)
    #
    return output
#
def printout(node_id:list, disp:list, str1=""):
    """
    Report the relative displacement of the structure
    """
    output = ""
    # TODO: send this to mesh node?
    for x, item in enumerate(node_id):
        output += "{:9d} ".format(item)
        for _disp in disp[x]:
            output += "{: 1.3e} ".format(_disp)
        output += "\n"
    #
    return output
#
#
# --------------------
# beam print out
# --------------------
#
def print_bforces(bforce, plane):
    """
    """
    blgrp = bforce.groupby('load_type')
    #
    output = "\n"
    output += '{:}\n'.format(52 * '-')
    output += "** Element Forces\n"
    output += '{:}\n'.format(52 * '-')
    #   
    # basic
    bltype = blgrp.get_group('basic')
    blitems = bltype.groupby('load_name')
    #
    # basic
    for key, wk in blitems:
        header =  wk[['load_name', 'load_number', 'load_title', 'load_system']].values
        output += "-- Basic Load  Name: {:}  Number: {:}  Title: {:}  System: {:}".format(*header[0])
        #
        output += print_int_bforces(wk, plane)
        #print("")
    #
    # combination
    try:
        bltype = blgrp.get_group('combination')
        blitems = bltype.groupby('load_name')
        #
        output += '{:}\n'.format(52 * '-')
        for key, wk in blitems:
            header =  wk[['load_name', 'load_number', 'load_title', 'load_system']].values
            output += "-- Load Combination  Name: {:}  Number: {:}  Title: {:}  System: {:}".format(*header[0])
            #
            output += print_int_bforces(wk, plane)
            #print("")
    except KeyError:
        pass
    #
    return output
#
#
def print_int_bforces(bforces, plane):
    """
    """
    mgroup = bforces.groupby("element_name") #.groups .get_group([0,1])
    #
    output = "\n"
    if plane.plane2D:
        output += "     Beam  Len[ m]    Fx [ N]    Fy [ N]  Mz[ N-m ]\n"
    else:
        output += "     Beam  Len[ m]    Fx [ N]    Fy [ N]    Fz [ N]  Mx[ N-m ]  My[ N-m ]  Mz[ N-m ]\n"
    #
    for key, mgroup in mgroup:
        output += "{:9d} ".format(key)
        items = mgroup[plane.bhforce].values
        #output += "\n"
        for x, member in enumerate(items):
            try:
                1 / x
                output += "{:} ".format(9 * " ")
            except ZeroDivisionError:
                pass
            
            output += "{:1.2e} ".format(member[0])
            for i in range(plane.ndof):
                output += "{: 1.3e} ".format(member[i+1])
            output += "\n"
    #
    return output
#
#
def print_beam_disp(bdisp, plane):
    """ """
    blgrp = bdisp.groupby('load_type')
    #
    output = "\n"
    output += '{:}\n'.format(52 * '-')
    output += "** Element Displacement\n"
    output += '{:}\n'.format(52 * '-')
    #   
    # basic
    bltype = blgrp.get_group('basic')
    blitems = bltype.groupby('load_name')     
    for key, wk in blitems:
        header =  wk[['load_name', 'load_number', 'load_title', 'load_system']].values
        output += ("-- Basic Load  Name: {:}  Number: {:}  Title: {:}  System: {:}\n"
                   .format(*header[0]))
        #
        output += print_int_bdisp(wk, plane)
        #print("")
    #
    # combinations    
    try:
        bltype = blgrp.get_group('combination')
        blitems = bltype.groupby('load_name')        
        output += '{:}\n'.format(52 * '-')
        for key, wk in blitems:
            header =  wk[['load_name', 'load_number', 'load_title', 'load_system']].values
            output += ("-- Load Combination  Name: {:}  Number: {:}  Title: {:}  System: {:}\n"
                       .format(*header[0]))
            #
            output += print_int_bdisp(wk, plane)
            #print("")
    except KeyError:
        pass
    #
    return output
#
#
def print_int_bdisp(eforces, plane):
    """
    """
    mgroup = eforces.groupby("element_name")
    #
    output = ""
    if plane.plane2D:
        output += "     Beam  Len[ m]     x [ m]     y [ m]   rz [rad]\n"
    else:
        output += "     Beam  Len[ m]     x [ m]     y [ m]     rx [rad]   ry [rad]   rz [rad]\n"
    #
    for key, mgroup in mgroup:
        output += "{:9d} ".format(key)
        items = mgroup[plane.bhdisp].values
        
        for x, member in enumerate(items):
            try:
                1 / x
                output += "{:} ".format(9 * " ")
            except ZeroDivisionError:
                pass
            
            output += "{:1.2e} ".format(member[0])
            for i in range(plane.ndof):
                output += "{: 1.3e} ".format(member[i+1])
            output += "\n"
    #
    return output
#
#
# --------------------
# output
# --------------------
#
# Node
#
class NodeDeflection(NamedTuple):
    """ """
    deflection: DBframework
    plane: tuple
    #
    def __str__(self):
        """ """
        return print_ndeflections(dispresult=self.deflection,
                                  plane=self.plane)
#
class NodeForce(NamedTuple):
    """ """
    force: DBframework
    plane: tuple
    #
    def __str__(self):
        """ """
        return print_nforce(dispresult=self.force,
                            plane=self.plane)
#
#
class NodeReaction(NamedTuple):
    """ """
    reaction: DBframework
    plane: tuple
    #
    def __str__(self):
        """ """
        return print_nreactions(reactions=self.reaction,
                                plane=self.plane)
    #
#
#
# Beam
#
class BeamForce(NamedTuple):
    """ Basic load transfer"""
    force: DBframework
    plane: tuple
    #
    def __str__(self):
        """ """
        return print_bforces(bforce=self.force,
                             plane=self.plane)
    #
#
#
class BeamDeflection(NamedTuple):
    """ Basic load transfer"""
    deflection: DBframework
    plane: tuple
    #
    def __str__(self):
        """ """
        return print_beam_disp(bdisp=self.deflection,
                                  plane=self.plane)
#
#
# --------------------
# Results
# --------------------
#
#
class Node(NamedTuple):
    """ """
    results: DBframework
    reac: DBframework
    plane: tuple
    #
    #
    @property
    def force(self):
        """ node force"""
        header = self.results.columns.tolist()
        header = list(set(header) - set(self.plane.hdisp))
        return NodeForce(self.results[header], plane=self.plane)
    #
    @property
    def deflection(self):
        """ node displacement"""
        header = self.results.columns.tolist()
        header = list(set(header) - set(self.plane.hforce))
        return NodeDeflection(self.results[header], plane=self.plane)
    #
    @property
    def reaction(self):
        """ """
        return NodeReaction(self.reac, plane=self.plane)
    #
    def __str__(self) -> str:
        """ """
        output = "\n"
        output += self.deflection.__str__()
        output += self.reaction.__str__()
        return output
#
#
#
class Beam(NamedTuple):
    """ """
    results: DBframework
    plane: tuple
    #
    @property
    def force(self):
        """beam integration forces"""
        header = self.results.columns.tolist()
        header = list(set(header) - set(self.plane.bhdisp))
        header = ['node_end', *header]
        return BeamForce(self.results[header], plane=self.plane)
    #
    @property
    def deflection(self):
        """beam integration forces"""
        header = self.results.columns.tolist()
        header = list(set(header) - set(self.plane.bhforce))
        header = ['node_end', *header]
        return BeamDeflection(self.results[header], plane=self.plane)  
    #
    def __str__(self) -> str:
        """ """
        output = "\n"
        output += self.force.__str__()
        output += self.deflection.__str__()       
        return output
