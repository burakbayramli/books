# 
# Copyright (c) 2009-202 fem2ufo
#
# Python stdlib imports
from __future__ import annotations
from array import array
#from collections.abc import Mapping
import re
#from typing import NamedTuple


# package imports
from steelpy.f2uModel.mesh.process.boundary import BoundaryNode, BoundaryItem

#
class BoundaryNodes(BoundaryNode):
    """
    FE Fixity

    Boundary
        |_ type
        |_ constrain [x, y, z, mx, my, mz]

    **Parameters**:
      :name : coupling, pile
      :type : free, fix, dependent, prescribed, supernode, master
      :constrain :
            0- free
            1- fixed
            2- prescribed displacement, temperature, different from zero
            3- linear dependent
            4- supernode (link)
    """
    __slots__ = ['_x', '_y', '_z',
                 '_rx', '_ry', '_rz',
                 '_labels', '_number', '_title']

    def __init__(self) -> None:
        """
        """
        self._number : array = array('I', [])
        self._x : array = array('i', [])
        self._y : array = array('i', [])
        self._z : array = array('i', [])
        self._rx : array = array('i', [])
        self._ry : array = array('i', [])
        self._rz : array = array('i', [])
        #
        #self._labels: list[int|str] = []
        self._title : list[int|str] = []
        #
        super().__init__()
        #
        # fix_type = ['release', 'gap', 'prescribed', 'dependence',
        #            'link', 'master', 'rigid', 'constrain']

    def __setitem__(self, node_number:int|str,
                    value:list|tuple|dict|str) -> None:
        """
        1 : fix
        0 : free
        """
        try:
            # TODO : update data
            self._labels.index(node_number)
            raise Warning('    *** warning node {:} already exist'.format(node_number))
        except ValueError:
            if isinstance(value, dict):
                self._x.append(value['x'])
                self._y.append(value['y'])
                self._z.append(value['z'])
                self._rx.append(value['rx'])
                self._ry.append(value['ry'])
                self._rz.append(value['rz'])
                try:
                    title = value['title']
                except KeyError:
                    title = "NULL"
            else:
                title = self.setup_data(value)
            #
            self._labels.append(node_number)
            self._number.append(self._labels.index(node_number))
            self._title.append(title)
            # _type = _bound_type[_type]
    #
    #
    def __getitem__(self, node_number: None | str | int) -> tuple | bool:
        """
        """
        try:
            _index = self._labels.index(node_number)
            return BoundaryItem(x=self._x[_index], y=self._y[_index], z=self._z[_index],
                                rx=self._rx[_index], ry=self._ry[_index], rz=self._rz[_index],
                                number=self._number[_index], name=self._title[_index],
                                node=node_number)
        except ValueError:
            return False
            # raise IndexError

    #
    #
    def __delitem__(self, node_number: int) -> None:
        """
        """
        try:
            i = self._labels.index(node_number)
            self._labels.remove(node_number)
            self._number.pop(i)
            # self._type.pop(i)
            self._x.pop(i)
            self._y.pop(i)
            self._z.pop(i)
            self._rx.pop(i)
            self._ry.pop(i)
            self._rz.pop(i)
        except IndexError:
            # logging.warning('  ** boundary {:} does not exist'.format(node_number))
            raise Warning('  ** boundary {:} does not exist'.format(node_number))

    #
    #
    #
    def setup_data(self, value):
        """ """
        title = "NULL"
        if isinstance(value, str):
            value = self.get_boundary(value)

        elif len(value) == 1:
            if isinstance(value[0], str):
                value = self.get_boundary(value[0])
            else:
                newvalue = value[0]
                try:
                    1/len(newvalue)
                    value = []
                    for indx in range(6):
                        try:
                            value.append(newvalue[indx])
                        except KeyError:
                            value.append(0)
                except ZeroDivisionError:
                    raise IOError('*** error input format not recognized')
        else:
            try:
                value[6]
                title = value.pop()
            except AttributeError:
                title = value.name
            except IndexError:
                pass
        # update data
        self._x.append(value[0])
        self._y.append(value[1])
        self._z.append(value[2])
        self._rx.append(value[3])
        self._ry.append(value[4])
        self._rz.append(value[5])
        return title
    #
    #
    def update_data(self, value:list|tuple):
        """ """
        title = "NULL"
        if isinstance(value, list):
            node_name = value[0]
            if isinstance(value[1], str):
                value = self.get_boundary(value[1])
            else:
                1/0
        elif isinstance(value, BoundaryItem):
            node_name = value.node
            title = value.name
            value = value[:6]

        else:
            1/0
        #
        index = self._labels.index(node_name)
        number = self._number[index]
        # delete
        self.__delitem__(node_name)
        # update
        self._labels.insert(index, node_name)
        self._number.insert(index, number)
        self._title.insert(index, title)
        self._x.insert(index, value[0])
        self._y.insert(index, value[1])
        self._z.insert(index, value[2])
        self._rx.insert(index, value[3])
        self._ry.insert(index, value[4])
        self._rz.insert(index, value[5])
    #
    #
    def transposed(self):
        """ """
        data = [self._x, self._y, self._z,
                self._rx, self._ry, self._rz]
        data = list(map(list, zip(*data)))
        return data
    #
    #
#

