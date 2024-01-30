# ***********************************
#  Author: Pedro Jorge De Los Santos     
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
"""
The purpose of this module is to provide tools to build 
a model automatically from text files with coordinates 
and connectivities.
"""
import numpy as np
import re

FLOATS = "[-+]?([0-9]*\.[0-9]+|[0-9]+)"

def read_file(filename):
    mshfile = open(filename,"r")
    msh = mshfile.readlines()
    mshfile.close()
    return msh

def parse_nodes(line):
    p = re.compile(FLOATS)
    nd = [float(k) for k in p.findall(line)]
    return nd[1::]
    
def parse_elements(line):
    p = re.compile(FLOATS)
    elm = [int(k) for k in p.findall(line)]
    if len(elm) < 8: return []
    enum = elm[0]
    etype = elm[1]
    elmtags = elm[2:5]
    econn = elm[5::]
    if etype == 2: return econn
    else: return []
    
def isempty(iterable):
    return True if len(iterable)==0 else False

def read_msh(filename):
    msh = read_file(filename)
    APPEND_NODES = False
    APPEND_ELEMENTS = False
    nodes = []
    elements = []
    for line in msh:
        if "$Nodes" in line: APPEND_NODES = True
        if "$EndNodes" in line: APPEND_NODES = False
        if "$Elements" in line: APPEND_ELEMENTS = True
        if "$EndElements" in line: APPEND_ELEMENTS = False
        if APPEND_NODES:
            nc = parse_nodes(line)
            if not isempty(nc): nodes.append(nc)
        if APPEND_ELEMENTS:
            ec = parse_elements(line)
            if not isempty(ec): elements.append(ec)
    return np.array(nodes), np.array(elements)



def ModelFromFiles(nodesfile,elementsfile,model):
    """
    Creates a model from ASCII files, where nodesfile contains 
    the coordinates X/Y and elementsfile contains the connectivity 
    of elements.
    """
    pass
    #~ dlm = "," # CSV values
    #~ NC = np.loadtxt(nodesfile, delimiter=dlm)
    #~ EC = np.loadtxt(elementsfile, delimiter=dlm)
    
    #~ for nd in NC:
        #~ cnode = Node( (nd[0], nd[1]) )
        #~ model.addNode(cnode)
        
    #~ for el in EC:
        #~ na = model.nodes[el[0]] 
        #~ nb = model.nodes[el[1]] 
        #~ cel = Spring((na,nb))
        #~ model.addElement(cel)
    
    
if __name__=='__main__':
    pass
