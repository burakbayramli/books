# 
# Copyright (c) 2009-2023 fem2ufo
#
from __future__ import annotations
from dataclasses import dataclass
#from typing import Dict, List, Tuple, ClassVar, Iterable, Union
from collections.abc import Mapping
from collections import namedtuple
import math

# package imports
#from fem2ufo.f2u_model.feclass.mesh.mesh import Mesh
#from fem2ufo.f2u_model.femodel.mesh.boundary import Boundaries
#from fem2ufo.f2u_model.femodel.mesh.node import Nodes



#
class Spring(namedtuple('Spring', 'x y z mx my mz')):
    """
    """
    __slots__ = ()
    
    def __str__(self):
        return "{: 14.5f} {: 14.5f} {: 14.5f}".format(self.x, self.y, self.z)
#
#
@dataclass
class Joint:
    """
    FE concept joint
    
    Joints
        |_ name
        |_ number
        |_ type {shim}
        |_ node [nd[1], nd[2], nd[3],..., nd[n]]
        |_ chord [m[1], m[2]]
        |_ brace [m[1], m[2], m[3],..., m[n]]
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
    """
    #
    __slots__ = ('number', '_nodes', 'name', 'overlap',
                 'chord', 'brace', 'type', '_spring',
                 '_points', '_node')

    #_mesh = Mesh()

    def __init__(self, name: str, nodes) -> None:
        """
        """
        self.name:str = name
        self._nodes = nodes
        self.brace: list = []
        self.chord: list = []
        self.type: str = 'point'

    def get_name(self, number: int = None, name: str = None)-> str:
        """
        """
        if not number:
            number = self._node.number
        #
        if not name:
            name = self.name  # "jnt"
        #
        self.name = name + str(number).zfill(3) + '_' + str(self._node.name)
    #
    @property
    def spring(self):
        """
        """
        return self._spring
    
    @spring.setter
    def spring(self, spring)-> None:
        """
        """
        if isinstance(spring, (list, tuple)):
            self._spring = Spring._make(spring)
        elif isinstance(spring, dict):
            self._spring = Spring(**spring)
        else:
            raise Exception('   *** Spring input format not recognized')    
    #
    @property
    def node(self):
        """
        """
        try:
            return self._nodes[self._node]
        except Exception as e:
            raise KeyError(e)
    
    @node.setter
    def node(self, node_number: int)-> None:
        """
        """
        try:
            self._nodes[node_number]
            self._node = node_number
        except Exception as e:
            raise KeyError(e)

    #
    @property
    def boundary(self):
        """
        """
        return self._mesh.boundaries[self._node]

    @boundary.setter
    def boundary(self, fixity)-> None:
        """
        """
        self._mesh.boundaries[self._node] = fixity
    #
    # @property
    # def coordinates(self):
    #    """
    #    """
    #    
    #    return self._mesh.nodes[self._node]
    # @coordinates.setter
    # def coordinates(self):
    #    """
    #    """
    #    pass


#
class Connection(Mapping):
    """
    """
    __slots__ = ['_joints', '_points', '_nodes', '_boundaries']

    #_joints: Dict = {}

    def __init__(self, points) -> None:
        """
        """
        self._points = points
        self._joints: dict = {}

    def __setitem__(self, joint_name: str,
                    node_item: str|int|dict|list|tuple) -> None:
        """
        """
        #try:
        #self.map_joint_with_node(joint_name, node_item)
        #except KeyError:
        #    self._mesh.nodes[node_number] = coordinates
        #    self.map_joint_with_node(joint_name, node_number)
        #
        if isinstance(node_item, (str, int, dict)):
            self.map_joint_with_node(joint_name, node_item)
        else:
            node_number = len(self._joints) + 1
            self._nodes[node_number] = node_item
            self.map_joint_with_node(joint_name, node_number)

    def __getitem__(self, joint_name: str):
        """
        """
        try:
            return self._joints[joint_name]
        except KeyError:
            raise KeyError('Invalid key')

    #
    #
    def map_joint_with_node(self, joint_name: str, node_number: int) -> None:
        """
        """
        try:
            self._joints[joint_name]
            raise Exception('   error joint {:} already exist'.format(joint_name))
        except KeyError:        
            try: # check if point already exist
                _point = self._points[node_number]
                self._joints[joint_name] = Joint(joint_name, nodes=self._nodes)
                self._joints[joint_name].node = _point.node
            except KeyError:
                try: # check in node exist
                    self._nodes[node_number]
                    self._joints[joint_name] = Joint(joint_name, nodes=self._nodes)
                    self._joints[joint_name].node = node_number
                except IndexError:
                    raise KeyError('   *** error : Node {:} not found'.format(joint_name))
    #
    #
    #def get_point_by_node(self, node_number:int)-> bool:
    #    """ """
    #    node = [_point.name for _point in self._points.values()
    #            if _point.node.number == node_number]
    #    node
    #
    #
    def __len__(self) -> int:
        return len(self._joints)

    
    def __iter__(self)-> iter:
        """
        """
        return iter(self._joints)

    def __contains__(self, value) -> bool:
        return value in self._joints
    #
    def __delitem__(self, joint_name) -> None:
        """
        """
        #_joint = self._joints[joint_name]
        if 'link' in self._joints[joint_name].type:
            try:
                _node_number = self._joints[joint_name].node.number
                del self._boundaries[_node_number]
            except KeyError:
                pass
        del self._joints[joint_name]
#
#
#
@dataclass
class Point:
    """
    FE concept Point
    
    Points
        |_ name
        |_ number
        |_ type {shim}
        |_ node [nd[1], nd[2], nd[3],..., nd[n]]
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
    """
    __slots__ = ['number', 'name', '_node', '_nodes',
                 '_boundaries','_type']

    def __init__(self, name: str, nodes, boundaries) -> None:
        """
        """
        self.name:str = name
        self._nodes = nodes
        self._boundaries = boundaries
        self._type:str = "node"
    #
    @property
    def node(self):
        """
        """
        return self._nodes[self._node]

    @node.setter
    def node(self, node_number: int)-> None:
        """
        """
        try:
            self._nodes[node_number]
            self._node = node_number
        except KeyError:
            raise(f'   *** error : Node {node_number} not found')
    #
    @property
    def boundary(self):
        """
        """
        return self._boundaries[self._node]

    @boundary.setter
    def boundary(self, fixity: list|str)-> None:
        """
        """
        if isinstance(fixity, (list, tuple)):
            self._boundaries[self._node] = fixity
        elif isinstance(fixity, str):
            if 'fixed' in fixity.lower():
                self._boundaries[self._node] = [1, 1, 1, 1, 1, 1]
            elif 'pinned' in fixity.lower():
                self._boundaries[self._node] = [1, 1, 1, 1, 0, 0]
            else:
                raise IOError("**** error boundary not recognised")
        else:
            raise IOError("**** error boundary type not recognised")
    #
    @property
    def displacement(self):
        """
        """
        return self._boundaries[self._node]

    @displacement.setter
    def displacement(self, fixity:list)-> None:
        """
        """
        self._boundaries[self._node] = fixity
    #
    @property
    def type(self):
        """ """
        return self._type
    
    @type.setter
    def type(self, value:str):
        """ """
        self._type = value
    #
    # @property
    # def coordinates(self):
    #    """
    #    """
    #    
    #    return self._mesh.nodes[self._node]
    # @coordinates.setter
    # def coordinates(self):
    #    """
    #    """
    #    pass


#
class Points(Mapping):
    """
    """
    __slots__ = ['_points', '_nodes', '_boundaries']
    
    def __init__(self, nodes, boundaries) -> None:
        """
        """
        #self._mesh: ClassVar = mesh
        self._boundaries = boundaries
        self._nodes = nodes
        self._points: dict = {}
    
    def __setitem__(self, point_name: str, coordinates:tuple) -> None:
        """
        """
        node_number = len(self._points) + 1
        try:
            self.map_point_with_node(point_name, node_number)
        except KeyError:
            self._nodes[node_number] = coordinates
            self.map_point_with_node(point_name, node_number)
    
    def __getitem__(self, node_name: str):
        """
        """
        #try:
        return self._points[node_name]
        #except KeyError:
        #    raise KeyError

    #
    #
    def map_point_with_node(self, point_name: str, node_number: int) -> None:
        """
        """
        try:
            self._points[point_name]
            raise Exception('   error point {:} already exist'.format(point_name))
        except KeyError:        
            try:
                self._nodes[node_number]
                self._points[point_name] = Point(point_name, 
                                                 nodes=self._nodes,
                                                 boundaries= self._boundaries)
                self._points[point_name].node = node_number
            except IndexError:
                raise KeyError('   *** error : node {:} not found'.format(point_name))
    
    #
    #
    @property
    def system(self)-> str:
        """
        """
        return self._nodes.system
    @system.setter
    def system(self, coordinate_system:str)-> None:
        """
        """
        self._nodes.system = coordinate_system
    #
    #@property
    #def boundary(self):
    #    """ """
    #    return self._boundaries
    #
    #
    def __len__(self) -> int:
        return len(self._points)


    def __iter__(self)-> iter:
        """
        """
        return iter(self._points)

    def __contains__(self, value) -> bool:
        return value in self._points
    #
    def __delitem__(self, point_name) -> None:
        """
        """
        del self._points[point_name]
#    
#
def rect(r, theta):
    """theta in degrees

    returns tuple; (float, float); (x,y)
    """
    x = r * math.cos(math.radians(theta))
    y = r * math.sin(math.radians(theta))
    return x,y

def polar(x, y):
    """returns r, theta(degrees)
    """
    r = (x ** 2 + y ** 2) ** .5
    theta = math.degrees(math.atan2(y,x))
    return r, theta

class PointTest(object):
    def __init__(self, x=None, y=None, r=None, theta=None):
        """x and y or r and theta(degrees)
        """
        if x and y:
            self.c_polar(x, y)
        elif r and theta:
            self.c_rect(r, theta)
        else:
            raise ValueError('Must specify x and y or r and theta')
    def c_polar(self, x, y, f = polar):
        self._x = x
        self._y = y
        self._r, self._theta = f(self._x, self._y)
        self._theta_radians = math.radians(self._theta)
    def c_rect(self, r, theta, f = rect):
        """theta in degrees
        """
        self._r = r
        self._theta = theta
        self._theta_radians = math.radians(theta)
        self._x, self._y = f(self._r, self._theta)
    def setx(self, x):
        self.c_polar(x, self._y)
    def getx(self):
        return self._x
    x = property(fget = getx, fset = setx)
    def sety(self, y):
        self.c_polar(self._x, y)
    def gety(self):
        return self._y
    y = property(fget = gety, fset = sety)
    def setxy(self, x, y):
        self.c_polar(x, y)
    def getxy(self):
        return self._x, self._y
    xy = property(fget = getxy, fset = setxy)
    def setr(self, r):
        self.c_rect(r, self._theta)
    def getr(self):
        return self._r
    r = property(fget = getr, fset = setr)
    def settheta(self, theta):
        """theta in degrees
        """
        self.c_rect(self._r, theta)
    def gettheta(self):
        return self._theta
    theta = property(fget = gettheta, fset = settheta)
    def set_r_theta(self, r, theta):
        """theta in degrees
        """
        self.c_rect(r, theta)
    def get_r_theta(self):
        return self._r, self._theta
    r_theta = property(fget = get_r_theta, fset = set_r_theta)
    def __str__(self):
        return '({},{})'.format(self._x, self._y)
