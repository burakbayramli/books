# ***********************************
#  Author: Pedro Jorge De Los Santos    
#  E-mail: delossantosmfq@gmail.com 
#  Blog: numython.github.io
#  License: MIT License
# ***********************************
import numpy as np
import numpy.linalg as la
import json
from nusa import *

# class NusaModelReader(object):
#   def __init__(self,filename):
#       self.filename = filename

def read_model(filename,mtype="spring"):
    if mtype == "spring":
        return _read_spring_model(filename)
    elif mtype == "truss":
        return _read_truss_model(filename)
    else:
        raise ValueError("mtype must be a valid model type (spring, truss, bar, beam, lineartriangle)")


def _read_truss_model(filename):
    nodes_data,elements_data,constraints_data,forces_data = _get_data_from_json(filename)

    nc = nodes_data
    ec = elements_data
    x,y = nc[:,0], nc[:,1]

    nodes = []
    elements = []

    for k,nd in enumerate(nc):
        cn = Node((x[k],y[k]))
        nodes.append(cn)
        
    for k,elm in enumerate(ec):
        i,j,E,A = int(elm[0]-1),int(elm[1]-1),elm[2],elm[3]
        ni,nj = nodes[i],nodes[j]
        ce = Truss((ni,nj), E, A)
        elements.append(ce)
        
    model = TrussModel("Truss Model")
    for n in nodes: 
        model.add_node(n)
    for e in elements: 
        model.add_element(e)
    
    for c in constraints_data:
        k,ux,uy = int(c[0]),c[1],c[2]
        if ~np.isnan(ux) and ~np.isnan(uy):
            model.add_constraint(nodes[k-1], ux=ux, uy=uy)
        elif ~np.isnan(ux):
            model.add_constraint(nodes[k-1], ux=ux)
        elif ~np.isnan(uy):
            model.add_constraint(nodes[k-1], uy=uy)
    
    for f in forces_data:
        k,fx,fy = int(f[0]),f[1],f[2]
        model.add_force(nodes[k-1],(fx,fy))

    return model



def _read_spring_model(filename):
    nodes_data,elements_data,constraints_data,forces_data = _get_data_from_json(filename)

    nc = nodes_data
    ec = elements_data
    x,y = nc[:,0], nc[:,1]

    nodes = []
    elements = []

    for k,nd in enumerate(nc):
        cn = Node((x[k],y[k]))
        nodes.append(cn)
        
    for k,elm in enumerate(ec):
        i,j,ke = int(elm[0]-1),int(elm[1]-1),elm[2]
        ni,nj = nodes[i],nodes[j]
        ce = Spring((ni,nj), ke)
        elements.append(ce)
        
    model = SpringModel("Truss Model")

    for n in nodes: 
        model.add_node(n)
    for e in elements: 
        model.add_element(e)
    
    for c in constraints_data:
        k,ux,uy = int(c[0]),c[1],c[2]
        if ~np.isnan(ux) and ~np.isnan(uy):
            model.add_constraint(nodes[k-1], ux=ux, uy=uy)
        elif ~np.isnan(ux):
            model.add_constraint(nodes[k-1], ux=ux)
        elif ~np.isnan(uy):
            model.add_constraint(nodes[k-1], uy=uy)
    
    for f in forces_data:
        k,fx,fy = int(f[0]),f[1],f[2]
        model.add_force(nodes[k-1],(fx,fy))

    return model



def _dicts2array(listofdicts):
    """
    Convert a list of dicts to numpy array [internal purposes only]
    """
    nel = len(listofdicts) 
    nch = len(listofdicts[0])
    keys = listofdicts[0].keys()
    array = np.zeros((nel,nch))
    for i,dc in enumerate(listofdicts):
        for j,key in enumerate(keys):
            value = dc[key]
            if value == "free": # in case of "free" constraints
                value = np.nan
            array[i,j] = value
    return array


def _get_data_from_json(filename):
    with open(filename, 'r') as nusafile:
        data = nusafile.read()
    obj = json.loads(data)
    nodes_data = _dicts2array(obj["nodes"])
    elements_data = _dicts2array(obj["elements"])
    constraints_data = _dicts2array(obj["constraints"])
    forces_data = _dicts2array(obj["forces"])
    return nodes_data,elements_data,constraints_data,forces_data


if __name__=='__main__':
    fname = "data/truss_model01.nusa"
    m1 = read_model(fname, "truss")
    m1.solve()
    m1.simple_report()