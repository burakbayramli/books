# 
# Copyright (c) 2009 steelpy
#
from __future__ import annotations
# Python stdlib imports
from collections.abc import Mapping
from typing import NamedTuple, Iterator
import re


# package imports
from .elements.boundary import BoundaryNodes
#
#
class BoundaryJoint:

    def __init__(self) -> None:
        """
        """
        self._nodes = BoundaryNodes()
    #
    #
    def supports(self, values:list|None=None):
        """"""
        return self.point(values)    
    #
    @property
    def point(self):
        """"""
        return self._nodes

    @point.setter
    def point(self, values):
        """"""
        for value in values:
            self._nodes[value[0]] = value[1:]


#
#
class BoundaryItem(NamedTuple):
    """
    """
    x: float
    y: float
    z: float
    rx: float
    ry: float
    rz: float
    number: int
    name: str|int
#
#
class BoundaryType:
    __slots__ = ["_cls", "_boundary_name"]
    
    def __init__(self, cls, boundary_name):
        """
        """
        self._cls = cls
        self._boundary_name = boundary_name
    #
    @property
    def support(self):
        """ """
        return self._cls._nodes.point[self._boundary_name]
    
    @support.setter
    def support(self, conditions):
        """ """
        self._cls._nodes.point[self._boundary_name] = conditions
        #print('--')
    #
    @property
    def point(self):
        """ """
        index = self._cls._labels.index(self._boundary_name)
        return self._cls._points[index]
    #
#
#
class ConceptBoundaries:
    
    __slots__ = ["_labels", "_nodes",
                 "f2u_points", "_points"]
    
    def __init__(self):
        """
        """
        self._nodes = BoundaryJoint()
        self._labels: list[str|int] = []
        self._points: list[tuple[float]] = []
    
    def __setitem__(self, support_name: int|str,
                    coordinates: list[float]|dict[str, float]) -> None:
        """
        """
        try:
            self._labels.index(support_name)
            raise Exception('boundary name {:} already exist'.format( support_name))
        except ValueError:
            self._labels.append(support_name)
            try:
                self._points.append((coordinates[0],
                                     coordinates[1],
                                     coordinates[2]))
            except IndexError:
                self._points.append((coordinates[0],
                                     coordinates[1], 0))
    
    def __getitem__(self, support_name:int) -> tuple:
        """
        node
        """
        return BoundaryType(cls=self, boundary_name=support_name)
    
    #
    #
    def __len__(self) -> float:
        return len(self._labels)

    def __iter__(self) -> Iterator:
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels
    #
    #
    def df(self, df, columns:dict|None=None):
        """ """
        self._labels = df.name.tolist()
        self._points = df[["x", "y", "z"]].values.tolist()
        points = df[["name", "support"]].values.tolist()
        self._supports.node = points
        #print('---')
    #
#
#
# 
class BoundaryConcept:
    """
    Boundary Condition :
    
    Point : free/fixed/supernode/manual
    Line
    
    """
    __slots__ = ['_labels', '_number', '_supports']
    
    def __init__(self, points):
        """
        """
        #self._number: list[int] = []
        #self._labels: list[str|int] = []
        #self._points: dict = {}
        #self._lines: dict = {}
        #
        #self._points = points
        #
        #self._nodes:dict = {} # BoundariesJoint()
        self._supports = BoundariesSupports(points)
    #
    #
    #
    #def __len__(self) -> float:
    #    return len(self._labels)
    #def __iter__(self):
    #    """
    #    """
    #    return iter(self._labels)
    #def __contains__(self, value) -> bool:
    #    return value in self._labels    
    #
    #
    def supports(self):
        """ """
        return self._supports
    #
    #def support(self, dof:list|tuple|str = 'fixed',
    #            name:str|None = None):
    #    """Boundary condition along an edge"""
    #    bname = name
    #    mnumber = next(self.get_number())
    #    if not bname:
    #        bname = f"sp_{mnumber}"        
    #    try:
    #        self._labels.index(bname)
    #        raise IOError(f'    *** warning support {bname} already exist')
    #    except ValueError:
    #        self._labels.append(bname)
    #        self._number.append(mnumber)
    #        fixity = self.get_fixity(dof)
    #        self._nodes[bname] = BoundariesJoint(fixity=fixity)
    #        return self._nodes[bname]
    #
    #def point(self, dof:list|tuple|str):
    #    """
    #    Boundary condition inserted at points
    #    """
    #    self._fixity = self.get_fixity(dof)
    #    #print('-->', fixity)
    #    return self._nodes
    #
    # ----------------------------
    # Operations
    # ----------------------------
    #
    #
    #def get_number(self, start:int=1):
    #    """
    #    """
    #    try:
    #        n = max(self._number) + 1
    #    except ValueError:
    #        n = start
    #    #
    #    while True:
    #        yield n
    #        n += 1
    #
    #
    #
    def df(self, df, columns:dict|None=None):
        """ """
        # types: support/line 
        grptype = df.groupby(['type'])
        #
        # support
        #
        support = grptype.get_group('support')
        grpboundary = support.groupby(['boundary'])
        for key, items in grpboundary:
            boundary = key[0]
            # FIXME: in case input is a list
            if boundary.lower() == 'free':
                continue
            self._supports[boundary] = boundary
            for item in items.itertuples():
                self._supports[boundary].points = item.x, item.y, item.z
        #
        # Lines 
        #print('---')    
#
#
class BoundariesSupports(Mapping):
    """
    """
    __slots__ = ['_labels', '_type', '_number', '_fixity',
                 '_nodes', '_line', '_nitems', '_litems',
                 '_points']
    
    def __init__(self, points):
        """
        """
        # concept points
        self._points = points
        #
        self._fixity:list = []
        self._labels:list[str|int] = []
        self._type:list[str|int] = []
        self._number:list[int] = []
        #
        self._nodes = BoundaryNodes()
        self._line = []
        #
        self._nitems: dict = {}
        self._litems: dict = {}        
    #
    def __setitem__(self, name: int|str,
                    dof:list|tuple|str) -> None:
        """
        """
        try:
            self._labels.index(name)
            raise IOError(f'    *** warning support {name} already exist')
        except ValueError:
            self._labels.append(name)
            self._nitems[name] = []
            self._litems[name] = []           
        #
        self._fixity.append(self.get_fixity(dof))
        #print('-->')
        
    #
    def __getitem__(self, name: int|str):
        """
        """
        try:
            index = self._labels.index(name)
            #fixity = self._fixity[index]
            return SupportItems(cls=self, name=name)
        except ValueError:
            raise IndexError(' ** Support {name} no valid')
        
        
    #
    def __len__(self) -> float:
        return len(self._labels)
    
    def __iter__(self):
        return iter(self._labels)
    
    def __contains__(self, value) -> bool:
        return value in self._labels    
    #    
    #
    # ----------------------------
    # Operations
    # ----------------------------
    #    
    #
    def get_fixity(self, fixity):
        """ """
        if isinstance(fixity, str):
            if re.match(r"\b(fix(ed)?)\b", fixity, re.IGNORECASE):
                return [1,1,1,1,1,1]
            elif re.match(r"\b(pinn(ed)?|roll)\b", fixity, re.IGNORECASE):
                return [1,1,1,0,0,0]
            elif re.match(r"\b(free)\b", fixity, re.IGNORECASE):
                return None
            else:
                raise IOError("boundary type {:} not implemented".format(fixity))
        elif isinstance(fixity, (list, tuple)):
            return fixity
        elif isinstance(fixity, dict):
            return [fixity['x'], fixity['y'], fixity['z'], 
                    fixity['rx'], fixity['ry'], fixity['rz']]
        else:
            raise Exception('   *** Boundary input format not recognized')
    #
#
#
#
class SupportItems:
    """
    """
    __slots__ = ['cls', '_name']
    
    def __init__(self, cls, name):
        """
        """
        self._name = name
        self.cls = cls
    # 
    #
    #def __setitem__(self, boundary_name: int|str,
    #                values: list|dict) -> None:
    #    """
    #    """
    #    1 / 0
    #    boundary_type = values[0]
    #    b_number = self._set_item(b_name=boundary_name, 
    #                              b_type=boundary_type)
    #    #
    #    if re.match(r"\b(node(s)?|support(s)?|constrain(s)?)\b", boundary_type, re.IGNORECASE):
    #        if isinstance(values[1], str):
    #            self._nodes[boundary_name] = values[1]
    #        else:
    #            self._nodes[boundary_name] = values[1:]
    #    #elif 'curve' == boundary_type :
    #    #    raise Exception('--> Mat type No ready')
    #    else:
    #        raise IOError(' Boundary type {:} not recognised'.format(boundary_type))
    #    
    
    #def __getitem__(self, name: int|str):
    #    """
    #    """
    #    try:
    #        index = self.cls._labels.index(name)
    #        #boundary_type = self.cls._type[index]
    #        #b_number = self.cls._number[index]
    #    except ValueError:
    #        raise KeyError(f'Invalid boundary name : {name}')
    #    #
    #    #if re.match(r"\b(node(s)?|coord(inate)?)\b", boundary_type, re.IGNORECASE):
    #    #    return self.cls._nodes[b_number]
    #    #elif 'elastic' == material_type :
    #    #    return self._elastic[mat_number]
    #    #else:
    #    #    raise IOError(f' boundary type {boundary_type} not recognised')
    #
    #
    def line(self):
        """Boundary condition along an edge"""
        raise NotImplemented()
    #
    #
    @property
    def points(self):
        """ """
        name = self._name
        index = self.cls._labels.index(name)
        fixity =  self.cls._fixity[index]
        #number = self.cls._number[index]
        items = self.cls._nitems[name]
        return BoundaryItem(*fixity, name, items)
    
    @points.setter
    def points(self, values:tuple|list):
        """Boundary condition inserted at support points"""
        index = self.cls._labels.index(self._name)
        if isinstance(values, list):
            for value in values:
                try: # try existing points
                    self.cls._nodes[value.name] = self.cls._fixity[index]
                    self.cls._nitems[self._name].append(value)
                except AttributeError: # new point
                    #print('-->')
                    pname = self.cls._points.get_new_point(values)
                    npoint = self.cls._points[pname]
                    self.cls._nodes[pname] = self.cls._fixity[index]
                    self.cls._nitems[self._name].append(npoint)
                    break
        
        elif isinstance(values, tuple):
            try:
                self.cls._nodes[values.name] = self.cls._fixity[index]
                self.cls._nitems[self._name].append(values)
            except AttributeError:
                pname = self.cls._points.get_new_point(values)
                npoint = self.cls._points[pname]
                self.cls._nodes[pname] = self.cls._fixity[index]
                self.cls._nitems[self._name].append(npoint)                
        else:
            raise IOError('Point data not valid')
        #
        #return self._nodes 
    #
    #
    def _set_item(self, b_name, b_type):
        """ """
        try:
            self._labels.index(b_name)
            raise IOError('   error boundary {:} already exist'.format(b_name))
        except ValueError:
            boundary_type = b_type
            self._labels.append(b_name)
            self._type.append(boundary_type)
            b_number = next(self.get_number())
            self._number.append(b_number)
        #
        return b_number
    #
    def get_number(self, start:int=1):
        """
        """
        try:
            n = max(self._number) + 1
        except ValueError:
            n = start
        #
        while True:
            yield n
            n += 1
    #
    #
    def __str__(self, units:str="si") -> str:
        """ """
        lenght = ' m'
        space = " "
        output = "\n"
        output += "{:}\n".format(80*"_")
        output += "\n"
        output += f"{33*space}BOUNDARIES\n"
        output += "\n"
        output += (f"Node {14*space} x {6*space} y {6*space} z {5*space} mx {5*space} my {5*space} mz {5*space} title")
        output += "\n"
        output += "{:}\n".format(80*".")
        output += "\n"
        for key, node in self._nodes.items():
            #if sum(node[:6]) == 0:
            #    continue
            output += node.__str__()
        return output
#
#
# -----------------------
# TODO: merge with slite
class BoundaryItem(NamedTuple):
    """
    """
    x: float
    y: float
    z: float
    rx: float
    ry: float
    rz: float
    #number: int
    name: str|None
    points:list
    
    #def __str__(self) -> str:
    #    if (name := self.name) == None:
    #        name = ""
    #    return "{:12d} {: 8.0f} {: 8.0f} {: 8.0f} {: 8.0f} {: 8.0f} {: 8.0f} {:>12s}\n"\
    #        .format(self.node, self.x, self.y, self.z, self.rx, self.ry, self.rz, name)
#
