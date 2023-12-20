# Copyright (c) 2022 steelpy
from __future__ import annotations
#
# Python stdlib imports
from array import array
from collections.abc import Mapping
import copy
from dataclasses import dataclass
#from dateutil.parser import parse
from datetime import datetime as dt, timedelta
from math import nan, isnan, ceil
import statistics

#
# package imports
from .dfprocess import get_dtype, rowcol
from .dftime import dtModule, get_date, string2number, get_timedelta
from steelpy.process.units.buckingham import Number

#
@dataclass
class BasicSeries(Mapping):
    __slots__ = [ '_serie', '_name', '_dtype', '_index']
    
    def __init__(self, data: list[float], 
                 name: int|float|str,
                 index: None|list = None, dtype=None) -> None:
        """Construct a new list object"""
        # get name
        self._name = name
        if isinstance(data, SeriesItem):
            #print('series')
            self._index = index
            if not self._index:
                self._index = data._index
            self._dtype = dtype
            if not self._dtype:
                self._dtype = data._dtype
            self._serie = data._serie
        else:
            self._index = index
            self._dtype = dtype
            if not self._dtype:
                self._dtype = get_dtype(data)
            # get data
            self._setup_serie(data)
            #print('--')
    #
    def _setup_serie(self, data):
        """ """
        if self._dtype == bool :
            self._serie: list[ bool ] = tuple(data)
        elif self._dtype == int:
            try:
                self._serie: array = array('q', list(map(int, data)))
            except TypeError:
                try:
                    self._serie = [nan if item == None else int(item) 
                                   for item in data]
                
                except TypeError:
                    self._serie = [nan if str(item) else int(item) 
                                   for item in data]
                
                except ValueError:
                    #self._dtype = str
                    #self._serie = list(map(str, data))
                    self._serie = data
            except ValueError:
                self._serie = data
            #print('--')
        elif self._dtype == str:
            #self._serie: list[ str ] = [item if item else nan for item in data]
            self._serie = [nan if str(item) == "" else item for item in data]
        elif self._dtype == dt:
            self._serie: list[ dt ] = [get_date(item) for item in data]
            #print('--')
        elif self._dtype == timedelta:
            self._serie: list[ timedelta ] = [get_timedelta(item) for item in data]
        elif self._dtype == float:
            try:
                try:
                    self._serie: array = array( 'd', list(map(float, data)))
                except TypeError:
                    self._serie: list = data
            except ValueError:
                try:
                    self._serie = [nan if str(item)=="" else float(item) for item in data]
                except ValueError:
                    #self._dtype = str
                    #self._serie = list(map(str, data))
                    self._serie: list = data
        else:
            self._serie: list[str|int|float] = data
    #    
    #
    def __iter__(self):
        """
        """
        return iter(self._serie)
    #
    # Return a string representation of self.
    def to_string(self) -> str:
        """
        :return:
        """
        steps = len(self._serie)
        index = [i for i in range(steps)]
        gapi = max(len(str(x)) for x in index)
        gaps = max(len(str(x)) for x in self._serie)
        output = ""
        if self._dtype == bool:
            for i in range(steps):
                output += "".join(f'{index[i]:>{gapi}} {str(self._serie[i]):>{gaps}}') + "\n"

        elif self._dtype == dt:
            for i in range(steps):
                try:
                    output += "".join(f"{index[i]:>{gapi}} {self._serie[i].strftime('%d %m %Y'):>{gaps}}") + "\n"
                except AttributeError:
                    output += "".join(f"{index[i]:>{gapi}} {str(self._serie[i]):>{gaps}}") + "\n"
        elif self._dtype == timedelta:
            # print('-->')
            days = [x.days for x in self._serie]
            gaps = max(len(str(x.days)) for x in self._serie)
            for i in range(steps):
                output += "".join(f'{index[i]:>{gapi}} {days[i]:>{gaps}} days') + "\n"

        else:
            for i in range(steps):
                output += "".join(f'{index[i]:>{gapi}} {self._serie[i]:>{gaps}}') + "\n"
        namesize = len(self._name)
        output += "".join(f"Name:{self._name:>{namesize}}")  # dtype:{self._dtype:>{12}
        return output
    #
    #
    #def __repr__(self):
    #    """Return a string representation of self."""
    #    args = ', '.join(repr(x) for x in self._serie)
    #    # return '[{}]'.format(args)
    #    return f'[{args} Name:{self._name} dtype:{self._dtype}]'
    #
    def __len__(self) -> int:
        """Return the dimension of self."""
        return len(self._serie)

    def __add__(self, other) -> list[float]:
        """Return the sum of self and daframe object other."""
        if not isinstance(other, SeriesItem):
            raise TypeError( "must be a dataframe class" )
        #
        # check if time series
        #if self._dtype == dt:
        #    print('---')
        #    1/0
        #elif self._dtype == timedelta:
        #    print('---')
        #    1/0        
        #else:
        result = [ x + y for x, y in zip(self._serie, other)]
        #except TypeError:
        #    result = []
        #    for idx, item in enumerate(self._serie):
        #        print(other[idx])
        #        result.append(item + other[idx])
        #xyx = SeriesItem( result, name=self._name, index=self._index, dtype=self._dtype)
        return SeriesItem(result, name=self._name, index=self._index, dtype=self._dtype)

    def __mul__(self, scalar) -> list[float]:
        """Return the product of self and numeric object alpha."""
        if isinstance(scalar, Number):
            dtype = object
        elif not isinstance(scalar, (float, int)):
            raise TypeError ( "must be a scalar" )
        else:
            dtype = self._dtype
        result = [ x * scalar for x in self._serie ]
        return SeriesItem(result, name=self._name, index=self._index, dtype=dtype)

    def __truediv__(self, scalar) -> list[float]:
        """Return the product of self and numeric object alpha."""
        if isinstance(scalar, Number):
            dtype = object
        if not isinstance(scalar, (float, int)):
            raise TypeError("must be a scalar")
        else:
            dtype = self._dtype
        result = [ x / scalar for x in self._serie ]
        return SeriesItem(result, name=self._name, index=self._index, dtype=dtype)
    #
    __rmul__ = __mul__
    #
    def __le__(self, other):
        """ """
        if isinstance(other, (int, str, int)):
            result = [False if item <= other else True for item in self._serie]
            return SeriesItem(result, name=self._name, index=self._index, dtype=self._dtype)
        elif isinstance(other, BasicSeries):
            1 / 0
            return self._serie <= other
        else:
            print('??')
            1 / 0
        1/0
    #
    def __eq__(self, other):
        """ """
        if isinstance(other, (int,str,int)):
            result = [True if item == other else False for item in self._serie]
            return SeriesItem(result, name=self._name, dtype=self._dtype)
        elif isinstance(other, BasicSeries):
            1/0
            return self._serie != other
        else:
            print('??')
            1/0    
    #
    def __ne__(self, other):
        """ """
        if isinstance(other, (int, str, int)):
            result = [False if item == other else True for item in self._serie]
            return SeriesItem(result, name=self._name, index=self._index, dtype=self._dtype)
        elif isinstance(other, BasicSeries):
            1/0
            return self._serie != other
        else:
            print('??')
            1/0    
    #
    #
    def maxabs(self):
        """ """
        mabs = min( self._serie )
        if max( self._serie ) > abs ( mabs ):
            mabs = max ( self._serie )
        return mabs

    #
    def max(self):
        """ """
        return max(self._serie)
    #
    def min(self):
        """ """
        return min(self._serie)    
    #
    def idxmax(self):
        """ """
        # max_value = max(self._serie)
        return [ index for index, value in enumerate ( self._serie )
                 if value == max ( self._serie ) ]
        # print('--')

    #
    def std(self):
        """standard deviation"""
        return statistics.stdev([0 if isnan(x) else x for x in self._serie])

    #
    def mean(self):
        """standard deviation / average"""
        return statistics.mean([0 if isnan(x) else x for x in self._serie])

    #
    def sum(self):
        """ sum"""
        #new = [0 if isnan(x) else x for x in self._serie]
        return sum([0 if isnan(x) else x for x in self._serie])

    #
    # ---------------------------------------
    #
    def tolist(self):
        """ """
        try:
            return self._serie.tolist()
        except AttributeError:
            return list(self._serie)
    #
    #def copy(self):
    #    """ """
    #    return copy.deepcopy(self._serie)
    #
    def fillna(self, value:float|int|list|None=None, 
               method:str|None=None):
        """Fill NA/NaN values using the specified method 
        method: pad / ffill: propagate last valid observation forward to next valid 
                backfill / bfill: use next valid observation to fill gap"""
        newdata = []
        #
        if method:
            if method in ['pad', 'ffill']:
                for idx, item in enumerate(self._serie):
                    try:
                        if isnan(item):
                            newdata.append(newdata[-1])
                        else:
                            newdata.append(item)
                    except TypeError:
                        newdata.append(item)
                #
                #test = [self._serie[idx-1] if isnan(item) else item
                #        for idx, item in enumerate(self._serie)]
                #test
                #1/0
                #self._setup_serie(newdata)
                dtype = self._dtype
            elif method in ['backfill', 'bfill']:
                1/0
            else:
                raise IOError(f"method {method} not valid")
        else:
            if isinstance(value, SeriesItem):
                for idx, item in enumerate(self._serie):
                    try:
                        if isnan(item):
                            newdata.append(value[idx])
                        else:
                            newdata.append(item)                        
                    except TypeError:
                        if item in ['NULL', 'null', '']:
                            newdata.append(value[idx])
                            continue
                        newdata.append(item)
                dtype = self._dtype
            else:
                for item in self._serie:
                    try:
                        if isnan(item):
                            newdata.append(value)
                        else:
                            newdata.append(item)
                    except TypeError:
                        if item in ['NULL', 'null', '']:
                            newdata.append(value)
                            continue
                        newdata.append(item)        
                dtype = self._dtype
        return SeriesItem(data=newdata, name=self._name, 
                          index=self._index) # , dtype=dtype
    #
    def apply(self, func, axis:int|str=0):
        """ 
        func: Function to apply to each column or row.  
        axis: Axis along which the function is applied.
              0 or ‘index’: apply function to each column.
              1 or ‘columns’: apply function to each row.
        """
        if axis != 0 : # or axis != "index":
            newdata = 1/0
        else:
            newdata = list(map(func, self._serie))
        #
        #xxx = SeriesItem(data=newdata, name=self._name, dtype=self._dtype)
        return SeriesItem(data=newdata, name=self._name, dtype=self._dtype)
    #
    def astype(self, dtype, copy=True):
        """ """
        newdata = [dtype(item) for item in self._serie]
        return SeriesItem(data=newdata, name=self._name, 
                          index=self._index, dtype=dtype)
    #
    def copy(self, deep:bool=True):
        """ """
        if deep:
            #return self
            return copy.deepcopy(self)
        return copy(self)
    #
    @property
    def iloc(self):
        """ Purely integer-location based indexing for selection by position."""
        #return self._serie[idx]
        return SerieIndex(self._serie)
    #    
    #@property
    #def loc(self):
    #    """ Access a group of rows and columns by label(s)"""
    #    return CellLabel(self._serie, self._index)
    #
    @property
    def dt(self):
        """ """
        return dtModule(self._serie)
    #
    def isin(self, values:list|dict):
        """Whether each element in the DataFrame is contained in values"""
        if isinstance(values, (list,tuple)):
            res = [True if item in values else False 
                   for item in self._serie]
        else :
            1/0
        return res
    #
    def between(self, left, right, inclusive:str ='both'):
        """ Return boolean Series equivalent to left <= series <= right 
        
        inclusive{“both”, “neither”, “left”, “right”}"""
        if inclusive =='both':
            newlist = [True if left <= item <= right else False
                       for item in self._serie]
        print('---')
        1/0
#
#
@dataclass
class SerieIndex:
    slots__ = [ '_serie']

    def __init__(self, data) -> None:
        """ """
        self._serie = data

    #
    def __setitem__(self, cell, value) -> None:
        """
        """
        if isinstance( cell, int):
            self._serie[cell] = value
        elif isinstance( cell, slice ):
            1/0
        elif isinstance( cell, tuple ):
            1/0
        else:
            1/0        

    #
    def __getitem__(self, cell):
        """
        a[start:stop]  # items start through stop-1
        a[start:]      # items start through the rest of the array
        a[:stop]       # items from the beginning through stop-1
        a[:]           # a copy of the whole array
        """
        #col = self._serie
        if isinstance( cell, int):
            return self._serie[cell]
        elif isinstance( cell, slice ):
            1/0
        elif isinstance( cell, tuple ):
            1/0
        else:
            1/0
#
#
@dataclass
class RowItem(BasicSeries):
    __slots__ = [ '_serie', '_index', '_name', '_dtype' ]
    
    def __init__(self, data: list, index: list,
                 name: None | int | float | str = None,
                 dtype=None) -> None:
        """Construct a new list object"""
        self._index = index
        super().__init__(data=data, name=name, index=index, dtype=dtype)
    #
    def __getitem__(self, index:int|str|list) -> float|int|str:
        """Return the ith index of self."""
        if isinstance(index, list):
            newlist = [self._index.index(item) for item in index]
            return [self._serie[item] for item in newlist]
        if isinstance(index, slice):
            start, stop, step = rowcol(index, maxsteps= len(self._serie))
            return [self._serie[index] for index in range(start, stop, step)]
        else:
            i = self._index.index(index)
            return self._serie[i]
    #
    def __getattr__(self, name):
        """ """
        if name in self.__slots__:
            return self[name]

        try:
            #return self._serie[name]
            return self.__getitem__(name)
        except KeyError:
            raise AttributeError(f"object has no attribute '{name}'")
    #
    #def __setattr__(self, name, value):
    #    """ """
    #    try:
    #        super().__setattr__(name, value)
    #    except AttributeError:
    #        self.__setitem__(name, value)
    #
    #
    #
    # Return a string representation of self.
    def __repr__(self) -> str:
        """
        :return:
        """
        steps = len(self._serie)
        index = self._index
        gapi = max(len( str(x)) for x in index)
        gaps = max(len( str(x)) for x in self._serie)
        output = ""
        for i in range(steps):
            output += "".join(f'{index[ i ]:>{gapi}} {self._serie[ i ]:>{gaps}}') + "\n"
        try:
            namesize = len(self._name)
        except TypeError:
            namesize = 4
        output += "".join(f"Name:{self._name:>{namesize}}, dtype: object")
        return output
#
#
@dataclass
class SeriesItem(BasicSeries):
    __slots__ = ['_serie', '_name', '_dtype'] # '_index'

    def __init__(self, data: list[ float ], 
                 name: int|float|str = "",
                 index: None|list = None, 
                 dtype=None) -> None:
        """Construct a new list object"""
        super().__init__(data=data, name=name, index=index, dtype=dtype)
        #print('--')
    #
    def __getitem__(self, index:int|str|list) -> float|int|str:
        """Return the ith index of self."""
        if isinstance(index, list):
            return [self._serie[item] for item in index]
        if isinstance(index, slice):
            start, stop, step = rowcol(index, maxsteps=len(self._serie))
            return [self._serie[index] for index in range(start, stop, step)]
        else:
            return self._serie[index]
    #
    #
    def __repr__(self) -> str:
        """
        :return:
        """
        return self.to_string()
    #
    #
    #    #return str(self._serie)
    #    output = [f"{self._name:}"]
    #    #if self._dtype == int:
    #    #    output.extend([f"{item: 6.0f}" for x, item in enumerate(self._serie)])
    #    #elif self._dtype == float:
    #    #    output.extend([f"{item: 1.4e}" for x, item in enumerate(self._serie)])
    #    #else:
    #    output.extend([f"{item}" for x, item in enumerate(self._serie)])
    #    gap = max([len(item) for item in output])
    #    output = [f'{item:>{gap}}' for item in output]
    #    return '\n'.join(map(str, output))
    # ---------------------------------------


#
#
def time_delta(data, unit=None, errors='raise'):
    """ """
    data = [string2number(item) for item in data]
    try:
        newdata = [timedelta(weeks=ceil(item)) for item in data]
    except:
        newdata = []
        for item in data:
            try:
                newdata.append(timedelta(weeks=ceil(item)))
            except:
                newdata.append(nan)
    return SeriesItem(newdata, dtype=timedelta)
    #print('---')
    #1/0
#
def to_timedelta(data, unit=None, errors='raise'):
    """ """
    try:
        #import numpy as np
        from pandas import to_timedelta
        #try:
        #    data = np.ceil(data)
        #except TypeError:
        #    data = data.fillna(0)
        return to_timedelta(data, unit=unit)
    except ModuleNotFoundError:
        return time_delta(data, unit=unit)
#
#
