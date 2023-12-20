# Copyright (c) 2022 steelpy
#
from __future__ import annotations
# Python stdlib imports
from collections import Counter
from itertools import groupby, chain, compress
import re
from math import nan
from datetime import datetime as dt, timedelta, date
#
# package imports
from .dftime import is_date, string2number


#
#
def all_equal(iterable):
    "Returns True if all the elements are equal to each other"
    g = groupby(iterable)
    return next(g, True) and not next(g, False)
#
def flatten(list_of_lists):
    "Flatten one level of nesting"
    return list(chain.from_iterable(list_of_lists))
#
def find_repeating(lsts):
    """ find repeat elements in list of list"""
    flat = flatten(lsts)
    return [item for item, count in Counter(flat).items() if count > 1]
#
def challenge(numbers, top=2):
    common = Counter(numbers).most_common(top)
    return (i for i, _ in common)
#
#
#
def urlify(text):
    """ """
    # Remove all non-word characters (everything except numbers and letters)
    text = re.sub(r"[^\w\s]", '', text)
    # Replace all runs of whitespace with a single dash
    #text = re.sub(r"\s+", '_', text)
    text = re.sub('[%s\s]+' % '-', '_', text)
    return text
#
#
def get_index(datasize, index):
    """ """
    # get index
    out_index = [x for x in range(datasize)]
    if index:
        if len(index) == datasize:
            out_index = index
        else:
            raise IOError('index =! data')
    return out_index
#
def getrowsindex(cell, data, index):
    """ """
    col = list( data.keys () )
    colstep = len( data[ col[ 0 ] ] )
    if isinstance(cell, slice):
        start, stop, step = rowcol(cell, maxsteps=colstep)
        if isinstance(stop, str):
            start = index.index(start)
            stop = index.index(stop)
            if stop < start:
                stop, start = start, stop
            stop += 1
        indices = [x for x in range(start, stop, step)]
    elif isinstance(cell, list):
        indices = [index.index(x) for x in cell]

    elif isinstance(cell, int):
        indices = [index.index(cell)]

    elif isinstance(cell, str):
        return [index.index(cell)]

    else:
        raise NotImplementedError()
    
    return indices
#
def getcellcols(cell, data, header):
    """ """
    colitems = list(data.keys())
    colstep = len(data[colitems[0]])
    if isinstance(cell, slice):
        col = {index: key for index, key in enumerate(data.keys())}
        # rows
        start, stop, step = rowcol(cell, maxsteps=colstep)
        if isinstance(stop, str):
            colindex = {key: index for index, key in enumerate(data.keys())}
            start = colindex[start]
            stop = colindex[stop]
            if stop < start:
                stop, start = start, stop
            stop += 1
        #else:
        return [col[x] for x in range(start, stop, step)]
    elif isinstance(cell, list):
        colheader = list(header.keys())
        if set(cell).isdisjoint(colheader):
            raise IOError('items not found')
        return cell
    elif isinstance(cell, int):
        colindex = {index: key for index, key in enumerate(data.keys())}
        return [colindex[cell]]
    else:
        return [cell]

#
#
#
def is_number(n):
    is_number = True
    try:
        # v type-casting the number here as `complex`, instead of `float`
        num = complex(n)
        is_number = num == num
    except ValueError:
        is_number = False
    except TypeError:
        is_number = nan
    return is_number   
#
#
#
def get_dtype(data):
    """ """
    #print('---', data)
    if all( [ type(val) == bool  for val in data ] ):
            _dtype = bool
    
    elif any( [ isinstance( val, dt ) for val in data ] ):
        _dtype = dt
        
    elif any( [ isinstance( val, timedelta ) for val in data ] ):
        _dtype = timedelta
    
    else:
        try:
            number = [is_number(n) for n in data]
            if all(number):
                if all( [ isinstance( val, int ) for val in data ] ):
                    _dtype = int
                #elif all( [ isinstance( val, float ) for val in data ] ):
                #    self._dtype = float
                else:
                    _dtype = float
            elif any(number):
                newdata2 = []
                for idx, n in enumerate(number):
                    if n:
                        newdata2.append(data[idx])
                    else:
                        n2 = string2number(data[idx])
                        if n2:
                            newdata2.append(n2)
                        else:
                            newdata2.append(nan)
                #newdata = [string2number(n) if n else nan for n in data]
                if any( [ isinstance( val, int ) for val in newdata2 ] ):
                    _dtype = int
                #elif all( [ isinstance( val, float ) for val in newdata ] ):
                #    _dtype = float
                else:
                    _dtype = float
            
            else:
                if all( [ isinstance( val, str ) for val in data ] ):
                    if any( [ is_date(val) for val in data ] ):
                        _dtype = dt
                    else:
                        _dtype = str
                #elif any( [ isinstance( val, (dt, timedelta) ) for val in data ] ):
                #    _dtype = dt
                #
                #elif any( [ isinstance( val, timedelta ) for val in data ] ):
                #    self._dtype = dt
                #    self._serie: List[ dt ] = data        
                
                #elif all( [ isinstance( val, float ) for val in data ] ):
                #    self._dtype = float
                else:
                    _dtype = str
        except TypeError:
            #xxx = [ is_date(val) for val in data ]
            if any( [ is_date(val) for val in data ] ):
                _dtype = dt
            else:
                1/0
    #
    #print('--->', _dtype)
    return _dtype
#
#
def rowcol(cell, maxsteps: int):
    """ """
    step = 1
    if cell.step:
        step = cell.step
    #
    stop = maxsteps
    if cell.stop:
        stop = cell.stop
    #
    start = 0
    if cell.start:
        start = cell.start
    #
    return start, stop, step

#
#
#
def isbool(data):
    """ """
    newdata = [isinstance(item, bool) for item in data]
    #dat = [isinstance(item, bool) for item in range(10)]
    if all(newdata):
        return True
    return False
#
def getbool(data, columns, header, bool_list):
    """ """
    return {header[item]: list(compress(data[item], bool_list))
            for item in columns}
#
#
