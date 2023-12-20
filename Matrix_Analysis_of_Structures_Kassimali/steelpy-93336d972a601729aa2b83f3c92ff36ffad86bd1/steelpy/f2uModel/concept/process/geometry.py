# 
# Copyright (c) 2009-2019 fem2ufo
#

# Python stdlib imports
from collections.abc import Mapping
from dataclasses import dataclass
from math import isclose
import logging
from typing import NamedTuple, Tuple, Dict, List
from array import array


# package imports

#
# -----------------------
#
class UnitVector(NamedTuple):
    """
    """
    x: float
    y: float
    z: float
    number: int
    type: str

    def __str__(self) -> str:
        return "{: 14.5f} {: 14.5f} {: 14.5f}".format(self.x, self.y, self.z)


#
class CosineMatrix(NamedTuple):
    """
    """
    c11: float
    c12: float
    c13: float
    #
    c21: float
    c22: float
    c23: float
    #
    c31: float
    c32: float
    c33: float
    #
    number: int
    type: str
    # def __str__(self) -> str:
    #    return "{: 14.5f} {: 14.5f} {: 14.5f} {: 14.5f} {: 14.5f} {: 14.5f}".format(self.x, self.y, self.z,
    #                                                                                self.mx, self.my, self.mz)


#
class DirectionCosines(Mapping):
    """
    FE element univector
    
    GuidePoint
        |_ cosine [vx, vy, vz,..., vn]
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
      :cosine: univector
    
    _type = 3 --> unit vector
            9 --> cosine matrix
    """
    __slots__ = ('_labels', '_number', '_type',
                 '_c11', '_c12', '_c13',
                 '_c21', '_c22', '_c23',
                 '_c31', '_c32', '_c33')

    #
    def __init__(self) -> None:
        """
        """
        self._labels = []
        self._number : List[int] = array('I', [])
        self._type : List[int] = array('I', [])
        #
        self._c11 : List[float] = array('f', [])
        self._c12 : List[float] = array('f', [])
        self._c13 : List[float] = array('f', [])
        #
        self._c21 : List[float] = array('f', [])
        self._c22 : List[float] = array('f', [])
        self._c23 : List[float] = array('f', [])
        #
        self._c31 : List[float] = array('f', [])
        self._c32 : List[float] = array('f', [])
        self._c33 : List[float] = array('f', [])

    def __setitem__(self, element_number: int, value: List) -> None:
        """
        """
        try:
            self._labels[element_number]
            print('    *** warning Direction Cosines {:} already exist'.format(element_number))
            1 / 0
            return
        except IndexError:
            self._labels.append(element_number)
            #_number = max(self._number) + 1
            self._number.append(element_number)
            if len(value) == 3:
                self._type.append(3)
                self._set_unit_vector(value)
            else:
                self._type.append(9)
                self._set_cosine_matrix(value)

    def __getitem__(self, element_number: int) -> Tuple:
        """
        """
        try:
            _index = self._labels.index(element_number)
            if self._type[_index] == 3:
                return UnitVector(self._c11[_index], self._c22[_index], self._c33[_index],
                                  number=self._number[_index], type=self._type[_index])
            else:
                return CosineMatrix(self._c11[_index], self._c12[_index], self._c13[_index],
                                    self._c21[_index], self._c22[_index], self._c23[_index],
                                    self._c31[_index], self._c32[_index], self._c33[_index],
                                    number=self._number[_index], type=self._type[_index])
        except ValueError:
            raise IndexError(' no unit cosine defined')
        # except IndexError:
        #    raise IndexError

    #
    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> float:
        return len(self._labels)

    def __delitem__(self, element_number: int) -> None:
        """
        """
        try:
            i = self._labels.index(element_number)
            self._labels.remove(element_number)
            self._number.pop(i)
            self._type.pop(i)
            #
            self._c11.pop(i)
            self._c12.pop(i)
            self._c13.pop(i)
            #
            self._c21.pop(i)
            self._c22.pop(i)
            self._c23.pop(i)
            #
            self._c31.pop(i)
            self._c32.pop(i)
            self._c33.pop(i)
            #
            # self._sets.pop(i)
        except IndexError:
            logging.warning(' delete -- Direction Cosine {:} does not exist'
                            .format(element_number))
            return

    #
    #
    def _set_unit_vector(self, value):
        """
        """
        if isinstance(value, list):
            self._c11.append(value[0])
            self._c22.append(value[1])
            self._c33.append(value[2])
        elif isinstance(value, dict):
            self._c11.append(value['x'])
            self._c22.append(value['y'])
            self._c33.append(value['z'])
        else:
            raise IOError('   *** error unit vector input format not recognized')

    #
    def _set_cosine_matrix(self, value):
        """
        """
        if isinstance(value, list):
            self._c11.append(value[0])
            self._c12.append(value[1])
            self._c13.append(value[2])
            #
            self._c21.append(value[3])
            self._c22.append(value[4])
            self._c33.append(value[5])
            #
            self._c31.append(value[6])
            self._c32.append(value[7])
            self._c33.append(value[8])
        else:
            raise IOError('   *** error cosine matrix input format not recognized')


#
# -----------------------
#
class OffSet(NamedTuple):
    """
    """
    x: float
    y: float
    z: float
    number: int
    type: List

    #
    @property
    def system(self) -> str:
        """
        return if Eccentricity in local or global coordinate system
        """
        if self.type == 1:
            return "local"
        else:
            return "global"
    # @system.setter
    # def system(self, coordinate_system:str) -> None:
    #    """
    #    set Eccentricity coordinate system
    #    """
    #  


#
class Eccentricities(Mapping):
    """
    Member eccentricity
    
    Eccentricities
        |_ case
        |_ eccentricity [x, y, z,..., n]
    
    **Parameters**:  
      :x
      :y
      :z
      :case: 
            1- global axis system
            2- local  system
    """
    __slots__ = ['_x', '_y', '_z', '_number',
                 '_labels', '_system', '_local_system']

    def __init__(self) -> None:
        """
        """
        self._x : List[float] = array('f', [])
        self._y : List[float] = array('f', [])
        self._z : List[float] = array('f', [])

        self._labels : List[int] = array('I', [])
        self._number : List[int] = array('I', [])
        self._system : List[int] = array('I', [])
        self._local_system : bool = False

    #
    def local_system(self):
        """
        """
        self._local_system = True

    #
    def __setitem__(self, eccentricity_number: int, 
                    values: List) -> None:
        """
        values : list = [x, y, z, system (optional, global default)]
        """
        try:
            self._labels[eccentricity_number]
            print('    *** warning eccentricity {:} already exist'.format(eccentricity_number))
            return
        except IndexError:
            self._labels.append(eccentricity_number)
            #_number = max(self._number) + 1
            self._number.append(eccentricity_number)
            if isinstance(values, (list, tuple)):
                self._x.append(values[0])
                self._y.append(values[1])
                self._z.append(values[2])
                try:
                    if 'local' in values[3].lower():
                        self._system.append(1)
                    else:
                        self._system.append(0)
                except IndexError:
                    if self._local_system:
                        self._system.append(1)
                    else:
                        self._system.append(0)

            elif isinstance(values, dict):
                self._x.append(values['x'])
                self._y.append(values['y'])
                self._z.append(values['z'])
                try:
                    if 'local' in values['system']:
                        self._system.append(1)
                    else:
                        self._system.append(0)
                except KeyError:
                    if self._local_system:
                        self._system.append(1)
                    else:
                        self._system.append(0)

            else:
                print('   *** error eccentricity input format not recognized')
        #
        self._local_system = False

    def __getitem__(self, eccentricity_number: int) -> Tuple:
        """
        """
        try:
            _index = self._labels.index(eccentricity_number)
            return OffSet(x=self._x[_index],
                          y=self._y[_index],
                          z=self._z[_index],
                          number=self._number[_index],
                          type=[self._system[_index]])
        except ValueError:
            raise KeyError('  eccentricity number: {:} not found'.format(eccentricity_number))
        # except IndexError:
        #    raise IndexError

    #
    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> float:
        return len(self._labels)

    def __delitem__(self, eccentricity_number: int) -> None:
        """
        """
        try:
            i = self._labels.index(eccentricity_number)
            self._labels.remove(eccentricity_number)
            self._number.pop(i)
            self._system.pop(i)
            self._x.pop(i)
            self._y.pop(i)
            self._z.pop(i)
        except IndexError:
            logging.warning(' delete -- eccentricity {:} does not exist'
                            .format(eccentricity_number))
            return

    @property
    def normal(self) -> float:
        """
        """
        return (self.x ** 2 + self.y ** 2 + self.z ** 2) ** 0.50

    #
    #
    def _set_eccentricity(self, values):
        """
        """
        eccentricity_number = len(self._labels) + 1
        self.__setitem__(eccentricity_number, values)
        return eccentricity_number


#
# -----------------------
#
class Fixity(NamedTuple):
    """
    """
    x: float
    y: float
    z: float
    mx: float
    my: float
    mz: float

    def __str__(self) -> str:
        return "{: 14.5f} {: 14.5f} {: 14.5f} {: 14.5f} {: 14.5f} {: 14.5f}".format(self.x, self.y, self.z,
                                                                                    self.mx, self.my, self.mz)


#
class Releases(Mapping):
    """
    FE elements releases
    
    Hinges
        |_ name
        |_ number
        |_ type
        |_ fix [x, y, z, mx, my, mz]
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
      :constrain : 
            0- free
            1- fixed
    """
    __slots__ = ['_x', '_y', '_z', '_mx', '_my', '_mz',
                 '_labels', '_number']

    #
    def __init__(self) -> None:
        """
        """
        self._labels: List = []
        self._number : List[int] = array('I', [])        
        self._x : List[float] = array('I', [])
        self._y : List[float] = array('I', [])
        self._z : List[float] = array('I', [])
        self._mx : List[float] = array('I', [])
        self._my : List[float] = array('I', [])
        self._mz : List[float] = array('I', [])
        # self.sets: Dict = {}
        # self.__setitem__('hinge_truss', [1, 1, 1, 1, 0, 0])
        # self._elements = elements
        #self._releases: Dict[str, Tuple] = {}
        #self._labels: Dict = {}

    def __setitem__(self, release_name: str, values: List) -> None:
        """
        """
        try:
            self._labels.index(release_name)
            raise Exception('    *** warning release {:} already exist'
                            .format(release_name))
        except ValueError:
            self._labels.append(release_name)
            _number = len(self._labels) #+ 1
            self._number.append(_number)
            #
            _releases = self._get_fixity(values)
            self._x.append(_releases[0])
            self._y.append(_releases[1])
            self._z.append(_releases[2])
            self._mx.append(_releases[3])
            self._my.append(_releases[4])
            self._mz.append(_releases[5])         
        #
        # try:
        #    self._releases[release_name]
        #    print('   *** warning release name already exist, values will be updated')
        # except KeyError:
        #    pass
        #self._releases[release_name] = Fixity(*_set_releases)
        # print('-->')

    def __getitem__(self, release_name: str) -> Tuple:
        """
        """
        try:
            #return self._releases[release_name]
            _index = self._labels.index(release_name)
            return Fixity(self._x[_index], self._y[_index], self._z[_index],
                          self._mx[_index], self._my[_index], self._mz[_index])
        except KeyError:
            raise KeyError
        # except IndexError:
        #    raise IndexError
    #
    def __iter__(self):
        """
        """
        #for _rel in self._releases.values():
        #    yield _rel
        return iter(self._number)
    
    def __delitem__(self, release_name: int) -> None:
        """
        """
        try:
            i = self._labels.index(release_name)
            self._number.pop(i)
            self._labels.pop(i)
            self._x.pop(i)
            self._y.pop(i)
            self._z.pop(i)
            self._mx.pop(i)
            self._my.pop(i)
            self._mz.pop(i)
        except IndexError:
            logging.warning(' delete -- release {:} does not exist'.format(release_name))
            return    
    
    def __len__(self) -> float:
        return len(self._number)
    
    def __contains__(self, value) -> bool:
        return value in self._labels    
    #
    # @property
    # def release(self):
    #    """
    #    """
    #    pass

    # @release.setter
    def _set_release(self, values):
        """
        """
        _set_releases = self._get_fixity(values)
        release_name = 'H_' + str(_set_releases[0]) + str(_set_releases[1]) + str(_set_releases[2]) \
                       + str(_set_releases[3]) + str(_set_releases[4]) + str(_set_releases[5])

        # if 'H_111111' in release_name:
        #    return None
        try:
            self._labels.index(release_name)
            #self._labels[release_name]
        except ValueError:
            self.__setitem__(release_name, values)
        return release_name

    #
    def _get_fixity(self, values):
        """
        """
        _freedom = {'x': 0, 'y': 1, 'z': 2, 'mx': 3, 'my': 4, 'mz': 5}
        _set_releases = [0, 0, 0, 0, 0, 0]
        if isinstance(values, list):
            for x, _value in enumerate(values):
                _set_releases[x] = _value

        elif isinstance(values, dict):
            for key, _value in values.items():
                _set_releases[_freedom[key]] = _value
        #
        # elif isinstance(value, str):
        #    print('here')
        #
        else:
            raise IOError('   *** release input format {:} not recognized'.format(values))

        return _set_releases
    #
    def get_name(self, number: int = None):
        """
        """
        # if not number:
        #    number = self.numbe
        rel = "".join(str(e) for e in self.fix)
        _name = 'HG' + str(number).zfill(3) + '_' + rel
        return _name
#
