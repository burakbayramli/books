# Copyright (c) 2022 steelpy
#
from __future__ import annotations
# Python stdlib imports
#from math import ceil, nan
from dataclasses import dataclass
from collections.abc import Mapping
from collections import namedtuple, defaultdict #, Counter
import copy
from itertools import product # groupby, chain, compress
#
# package imports
from steelpy.process.units.buckingham import Number
from .dfprocess import get_index, urlify, rowcol, isbool, getbool
from .dfseries import SeriesItem, RowItem
from .dfprocess import getrowsindex, getcellcols, find_repeating



#
class DataFrameBasic(Mapping):
    __slots__ = ['_data', '_index', '_name', '_header']

    def __init__(self, data: None | list | dict = None,
                 columns: None | list = None,
                 index: None | list = None):
        """
        """
        self._data: dict = {}
        self._header: dict = {}
        self._index = index
        #
        if isinstance(data, dict):
            for key, col in data.items():
                index = get_index(datasize=len(col), index=self._index)
                try:
                    name = urlify(key)
                except TypeError:
                    if isinstance(key, (int,float)):
                        name = key
                    else:
                        raise IOError("input data not recognized")
                self._header[key] = name
                self._data[name] = SeriesItem(col, name=name)

        elif isinstance(data, (list, tuple)):
            if isinstance(data[0], list):
                data = list(map(list, zip(*data)))
                if not columns:
                    columns = [x for x in range(len(data))]
                for x, col in enumerate(data):
                    name = urlify(columns[x])
                    self._header[columns[x]] = name
                    index = get_index(datasize=len(col), index=self._index)
                    self._data[name] = SeriesItem(col, name=name)

            elif isinstance(data[0], dict):
                raise NotImplementedError('dictonary')

            else:
                data = list(map(list, zip(*data)))
                for x, col in enumerate(columns):
                    name = urlify(col)
                    self._header[col] = name
                    index = get_index(datasize=len(data[x]), index=self._index)
                    self._data[name] = SeriesItem(data[x], name=name)
            # for key, item in data.items():
            #    self._data[key] = SeriesItem(item)
        else:
            1 / 0
            self._data: dict = {}
        #
        # self._header = {item:item for item in self._data.keys()}
        self._index = index
        # if not index and data:
        #    col = list(data.keys())
        #    self._index = [x for x in range(len(data[col[0]]))]
        #    #print('--')

    #
    #
    def __getitem__(self, name: list | str) -> dict:
        """
        """
        if isinstance(name, (list, tuple)):
            newdf = {}
            for item in name:
                newname = self._header[item]
                newdf[item] = self._data[newname]
            return DataFrameBasic(newdf)
        else:
            newname = self._header[name]
            return self._data[newname]
            #

    def __setitem__(self, name:str, 
                    value: list|int|float|str|SeriesItem|DataFrameBasic) -> None:
        """
        """
        if isinstance(value, list):
            if len(value) == len(self._index):
                newname = urlify(name)
                self._header[name] = newname
                self._data[newname] = SeriesItem(value, name=newname)
            else:
                raise IOError("new list size not compatible")

        elif isinstance(value, (int, float, str, Number)):
            new_value = [value for _ in self._index]
            newname = urlify(name)
            self._header[name] = newname
            self._data[newname] = SeriesItem(new_value, name=newname)

        elif isinstance(value, SeriesItem):
            newname = urlify(name)
            self._header[name] = newname
            self._data[newname] = value

        elif isinstance(value, DataFrameBasic):
            for x, key in enumerate(name):
                newname = urlify(key)
                self._header[key] = newname
                self._data[newname] = value[key]
        else:
            raise IOError("input data not compatible")
            #

    def __getattr__(self, name):
        """ """
        if name in self.__slots__:
            return self[name]

        try:
            newname = self._header[name]
            return self._data[newname]
        except KeyError:
            raise AttributeError(f"object has no attribute '{name}'")

    #
    def __setattr__(self, name, value):
        """ """
        try:
            super().__setattr__(name, value)
        except AttributeError:
            self.__setitem__(name, value)

    #
    #
    def __repr__(self) -> str:
        """ """
        return self.to_string()

    #
    def __len__(self) -> int:
        """Return the dimension of self."""
        return len(self._data)

    def __iter__(self):
        """
        """
        return iter(self._data)

    def __contains__(self, value) -> bool:
        return value in self._header

    #
    # Return a string representation of self.
    #
    def drop(self):
        """ """
        1 / 0

    #
    def to_excel(self):
        """ """
        1 / 0

    #
    def to_string(self, buf: None = None, columns: None | list | str = None,
                  index: bool = True, max_rows: int = 100):
        """ Render a DataFrame to a console-friendly tabular output.
        buf :
        columns : The subset of columns to write. Writes all columns by default.
        index :  Whether to print index (row) labels.
        max_rows : Maximum number of rows to display in the console (100 default).
        """
        steps = min(len(self._index), max_rows)
        header = {item: key for key, item in self._header.items()}
        rows = [*([header[key]] + [str(item) for item in items[:steps]]
                  for key, items in self._data.items())]
        gaps = [max(len(str(x)) for x in item) for item in rows]
        gaps = [max(item, 6) for item in gaps]
        comp = [">" for _ in gaps]
        if index:
            index = [str(item) for item in self._index[:steps]]
            index.insert(0, "")
            rows.insert(0, index)
            gaps.insert(0, max(6, max(len(item) for item in index)))
            comp.insert(0, "<")
        rows = list(zip(*rows))
        #
        output = ""
        for row in rows:
            output += " ".join(f'{item:{comp[x]}{gaps[x]}}' for x, item in enumerate(row)) + "\n"
        return output

    #
    #
    def tabulate(self, df=None, headers=None, tablefmt: str = 'psql'):
        """
        df : The source dataframe
        headers : To denote that the keys of the dataframe needs to be used as table headrs
        tablefmt : To denote the dataframe needs to be printed as psql format
        """
        if not isinstance(df, DataFrameBasic):
            df = self
        #
        df_columns = df.columns.tolist()
        max_len_in_lst = lambda lst: len(sorted(lst, reverse=True, key=len)[0])
        align_center = lambda st, sz: "{0}{1}{0}".format(" " * (1 + (sz - len(st)) // 2), st)[:sz] if len(
            st) < sz else st
        align_right = lambda st, sz: "{0}{1} ".format(" " * (sz - len(st) - 1), st) if len(st) < sz else st
        max_col_len = max_len_in_lst(df_columns)
        max_val_len_for_col = dict([(col, max_len_in_lst(df.iloc[:, idx].astype('str')))
                                    for idx, col in enumerate(df_columns)])
        col_sizes = dict([(col, 2 + max(max_val_len_for_col.get(col, 0), max_col_len))
                          for col in df_columns])
        build_hline = lambda row: '+'.join(['-' * col_sizes[col]
                                            for col in row]).join(['+', '+'])
        build_data = lambda row, align: "|".join([align(str(val), col_sizes[df_columns[idx]])
                                                  for idx, val in enumerate(row)]).join(['|', '|'])
        hline = build_hline(df_columns)
        out = [hline, build_data(df_columns, align_center), hline]
        for _, row in df.iterrows():
            out.append(build_data(row.tolist(), align_right))
        out.append(hline)
        return "\n".join(out)

    #
    #
    @property
    def index(self):
        """ The index (row labels) of the DataFrame
        keys : label or list of labels/arrays
        """
        return self._index

    @index.setter
    def index(self, value):
        """ """
        if len(value) != len(self._index):
            raise IOError("index length not compatible")
        self._index = value

    #
    def set_index(self, keys: list, drop: bool = True, append: bool = False,
                  inplace: bool = False, verify_integrity: bool = False):
        """ Set the DataFrame index using existing columns"""
        1 / 0
    #
    def reindex(self):
        """ """
        1/0
    #
    @property
    def values(self):
        """ """
        # if not columns:
        columns = self._data.keys()
        try:
            new = [self._data[col].tolist() for col in columns]
        except AttributeError:
            new = [self._data[col] for col in columns]
        return list(zip(*new))

    #
    @property
    def columns(self):
        """ The column labels of the DataFrame."""
        col = self._header.keys()
        name = "Index"
        return SeriesItem(col, name=name, index=self._index)
    
    @columns.setter
    def columns(self, values:list):
        """ The column labels of the DataFrame."""
        if len(values) != len(self._header):
            raise IOError("index length not compatible")
        #
        new_dict = {}
        for idx, value in  enumerate(self._header.values()):
            new_key = values[idx]
            new_dict[new_key] = value
        self._header = new_dict
    #
    #
    @property
    def iloc(self):
        """ Purely integer-location based indexing for selection by position."""
        return CellIndex(self._data, header=self._header)

    #
    @property
    def loc(self):
        """ Access a group of rows and columns by label(s)"""
        return CellLabel(self._data, index=self._index, header=self._header)

    #
    #
    def iterrows(self) -> iter:
        """ Iterate over DataFrame rows as (index, Series) pairs"""
        col = list(self._data.keys())
        header = {item: key for key, item in self._header.items()}
        columns = [header[key] for key in self._data.keys()]
        steps = len(self._data[col[0]])
        for x in range(steps):
            data = [self._data[item][x] for item in col]
            #yield self._index[x], SeriesItem(data=data, name=x,
            #                                 index=columns, dtype=object)
            yield self._index[x], RowItem(data=data, name=x,
                                          index=columns, dtype=object)

    #
    def itertuples(self, index: bool = True, name: str = 'Stpy') -> iter:
        """ Iterate over DataFrame rows as namedtuples"""
        col = list(self._data.keys())
        if index:
            col.insert(0, "Index")
        tupleitem = namedtuple(name, col)
        #
        columns = list(self._data.keys())
        steps = len(self._data[columns[0]])
        for x in range(steps):
            data = [self._data[item][x] for item in columns]
            if index:
                data.insert(0, x)
                #yield tupleitem._make(data)
            yield tupleitem._make(data)

    #
    #
    def plot(self, x: list, y: list, kind: str = 'line'):
        """ """
        import matplotlib.pyplot as plt
        #
        y_axis = self._data[y]
        # if x :
        x_axis = self._data[x]
        # if kind.lower == 'line':
        plt.plot(x_axis, y_axis)
        #
        plt.show()

    #
    def astype(self, dtype: str | int | float, copy: bool = True):  # item,
        """ """
        col = list(self._data.keys())
        steps = len(self._data[col[0]])
        header = {item: key for key, item in self._header.items()}
        index = [self._index[x] for x in range(steps)]
        if isinstance(dtype, str):
            newdf = {header[item]: [str(self._data[item][x]) for x in index]
                     for item in col}
        elif isinstance(dtype, int):
            newdf = {header[item]: [int(self._data[item][x]) for x in index]
                     for item in col}
        elif isinstance(dtype, float):
            newdf = {header[item]: [float(self._data[item][x]) for x in index]
                     for item in col}
        else:
            raise NotImplementedError

        return DataFrameBasic(data=newdf, index=index)

    #
    def groupby(self, by: None | str | list[str] = None, axis=0, level=None,
                as_index=True, sort=True, group_keys=True):
        """ Group DataFrame using a mapper or by a Series of columns"""
        return DataFrameGroupBy(self._data, self._header, self._index,
                                by, axis, level, as_index, sort, group_keys)

    #
    def concat(self, objs, axis: int = 0, ignore_index=False):
        """ Concatenate DataFrame objects along a particular axis with optional set logic along the other axe
        axis : {0/'index', 1/'columns'}, default 0 """
        1 / 0

    #
    def apply(self, func, axis:int|str = 0,
              row:bool = False, result_type:str|None=None):
        """
        func: Function to apply to each column or row.
        axis: Axis along which the function is applied.
              0 or ‘index’: apply function to each column.
              1 or ‘columns’: apply function to each row.
        raw: Determines if row or column is passed as a Series or array object
             False : passes each row or column as a Series to the function
             True : the passed function will receive array objects instead
        result_type : {‘expand’, ‘reduce’, ‘broadcast’, None}
        """
        col = list(self._data.keys())
        steps = len(self._data[col[0]])
        index = [self._index[x] for x in range(steps)]
        #header = {item: key for key, item in self._header.items()}
        #
        if axis != 0:

            newdata = []
            for idx, row in self.iterrows():
                newdata.append(func(row))
            if result_type:
                1/0
        else:
            newdata = [item.apply(func) for key, item in self._data.items()]
            #newdf = {header[key]: item.apply(func)
            #         for key, item in self._data.items()}
        #
        if row:
            return newdata
        return SeriesItem(data=newdata, index=index)

    #
    def fillna(self, value: float | int | list | None = None,
               method: str | None = None):
        """ """
        newdata = {}
        for key, item in self._data.items():
            try:
                newdata[key] = item.fillna(value, method)
            except IndexError:
                newdata[key] = item
        # print('---')
        return DataFrameBasic(newdata)

    #
    def copy(self, deep: bool = True):
        """ """
        if deep:
            return self
            # return copy.deepcopy(self)
        return copy(self)

    #
    #
    def dropna(self, subset: list | tuple | None = None):
        """ """
        1 / 0

    #
    #
    @property
    def dt(self):
        """ """
        1 / 0

    #
    #
    def rename(self, index: dict | None = None, 
               columns: dict | None = None,
               axis: int | str = 0, inplace: bool = False):
        """

        :param index:
        :param columns:
        :param axis: 0 or 'index', 1 or 'columns'
        :param inplace: Whether to return a new DataFrame.
        :return:
        """
        if columns:
            new_dict = {}
            for key, value in zip(self._header.keys(), self._header.values()):
                new_key = columns.get(key, key)
                new_dict[new_key] = self._header[key]
        elif index:
            1 / 0
        else:
            raise NotImplementedError()
        #
        if inplace:
            self._header = new_dict
        else:
            1 / 0
    #
    def insert(self, loc:int, column:str|int|list, 
               value:list|tuple|SeriesItem, 
               allow_duplicates:bool=False):
        """ """
        if isinstance(value, SeriesItem):
            name = f"{value._name}{column}"
            self.join(value, rsuffix=column)
            header = {name:column}
            self.rename(columns=header, inplace=True)
        elif isinstance(value, (list,tuple)):
            name = column
            self.join(value, rsuffix=name)
            1/0
        else:
            raise IOError('data not recognized')        
        #self._header
        #1/0
    #
    def join(self, other:SeriesItem|DataFrameBasic|list, 
             on:str|list|None=None, how:str='left', 
             lsuffix:str='', rsuffix:str='', 
             sort:bool=False, validate:str|bool=None):
        """
        how: [‘left’, ‘right’, ‘outer’, ‘inner’]
        """
        if isinstance(other, DataFrameBasic):
            for x, key in enumerate(other):
                newname = f"{key}{rsuffix}"
                self._header[key] = newname
                self._data[newname] = other[key]
        elif isinstance(other, SeriesItem):
            name = f"{other._name}{rsuffix}"
            self._header[name] = name
            self._data[name] = other
        elif isinstance(other, (list,tuple)):
            other
            1/0
        else:
            raise IOError('data not recognized')
#
#
@dataclass
class DataFrameGroupBy(DataFrameBasic):
    __slots__ = ['_data', '_index', '_name', '_cls', '_header']

    def __init__(self, data, header: dict, index:list,
                 by: None | str | list[str] = None,
                 axis=0, level=None,
                 as_index=True, sort=True, group_keys=True):
        """ """
        self._cls = data
        self._header = header
        self._index = index
        if by:
            if isinstance(by, str):
                newdict = {by: defaultdict(list)}
                name = self._header[by]
                for x, item in enumerate(data[name]):
                    newdict[by][item].append(x)

            elif isinstance(by, list):
                newdict = {}
                for col in by:
                    newdict[col] = defaultdict(list)
                    name = self._header[col]
                    for x, item in enumerate(data[name]):
                        newdict[col][item].append(x)

            else:
                raise IOError('input data not valid')
            #
            #
            headers = list(newdict.keys())
            columns = [{key: item for key, item in newdict[name].items()}
                       for name in headers]
            colkeys = [list(item.keys()) for item in columns]
            colproduct = list(product(*colkeys))
            #
            newitems = defaultdict(list)
            for i, row in enumerate(colproduct):
                rowexpand = [columns[x][item] for x, item in enumerate(row)]
                steps = len(rowexpand)
                # first step
                resitems = rowexpand[0]
                decoy = rowexpand[:2]  # first step
                resitem = []
                for x in range(1, steps):
                    resitems = find_repeating(decoy)
                    if resitems:
                        decoy = rowexpand[x + 1:]
                        decoy.insert(0, resitems)
                        resitem.extend(resitems)
                    else:
                        resitem = []
                        break
                #
                if resitems:
                    newitems[row].extend(resitems)
                #
                # else:
                #    newitems[row[0]] = rowexpand[0]
            #
            #
            name = {key: key[0] if len(key) == 1 else key
                    for key in newitems.keys()}
            self._data = {name[key]: SeriesItem(col, name=name[key])
                          for key, col in newitems.items()}
            # self._data = newitems
            #
            # self._group = {key: SeriesItem(col, name=key) for key, col in newdict.items()}

    #
    @property
    def groups(self):
        """ """
        # name = {key: key[0] if len(key)==1 else key
        #        for key in self._cls.keys()}
        #
        # return {name[key]: SeriesItem(col, name=name[key])
        #        for key, col in self._cls.items()}
        return self._data

    #
    #
    def get_group(self, value):
        """ """
        header = {item: key for key, item in self._header.items()}
        indices = self._data[value].tolist()
        newdf = {header[key]: item[indices]
                 for key, item in self._cls.items()}
        return DataFrameBasic(data=newdf, index=indices)

    #
    #
    #
    def group_by(self, seqs, idx=0, merge=True):
        d = dict()
        for seq in seqs:
            k = seq[idx]
            v = d.get(k, tuple()) + (seq[:idx] + seq[idx + 1:] if merge else (seq[:idx] + seq[idx + 1:],))
            d.update({k: v})
        return d
    #
    # def groupby_unsorted(self, seq, key=lambda x: x):
    #    indexes = defaultdict(list)
    #    for i, elem in enumerate(seq):
    #        indexes[key(elem)].append(i)
    #    for k, idxs in indexes.items():
    #        yield k, (seq[i] for i in idxs)
    #
    def __repr__(self) -> str:
        """ """
        return self.to_string()

#
#
class CellIndex:
    slots__ = ['_data', '_header']

    def __init__(self, data, header: dict) -> None:
        """ """
        self._data = data
        self._header = header

    #
    def __setitem__(self, cell, value) -> None:
        """
        """
        1 / 0
        pass

    #
    def __getitem__(self, cell):
        """
        a[start:stop]  # items start through stop-1
        a[start:]      # items start through the rest of the array
        a[:stop]       # items from the beginning through stop-1
        a[:]           # a copy of the whole array
        """
        columns = list(self._data.keys())
        colstep = len(self._data[columns[0]])
        headers = {item: key for key, item in self._header.items()}
        #
        if isinstance(cell, slice):
            start, stop, step = rowcol(cell, maxsteps=len(columns))
            newdf = {headers[item]: [self._data[item][x] for x in range(start, stop, step)]
                     for item in columns}
            return DataFrameBasic(data=newdf)

        elif isinstance(cell, tuple):
            # Columns
            if isinstance(cell[1], slice):
                start, stop, step = rowcol(cell[1], maxsteps=len(columns))
                col = [columns[x] for x in range(start, stop, step)]
            else:
                if isinstance(cell[1], list):
                    col = [columns[x] for x in cell[1]]
                elif isinstance(cell[1], int):
                    col = [columns[cell[1]]]
                else:
                    raise NotImplementedError()
            #
            # Rows
            if isinstance(cell[0], slice):
                start, stop, step = rowcol(cell[0], maxsteps=colstep)
                newdf = {headers[item]: [self._data[item][x] for x in range(start, stop, step)]
                         for item in col}
            else:
                1 / 0
            #
            # print('??')
            # index = None
            return DataFrameBasic(data=newdf)
        else:
            return getcol(cell, columns, data=self._data, header=headers)
        # print('--')


#
#
class CellLabel:
    slots__ = ['_data', '_index', '_header']

    def __init__(self, data, index: list, header: dict) -> None:
        """ """
        self._data = data
        self._index = index
        self._header = header

    #
    def __setitem__(self, cell, value) -> None:
        """
        """
        pass

    def __getitem__(self, cell):
        """
        """
        columns = list(self._data.keys())
        headers = {item: key for key, item in self._header.items()}
        #
        if isinstance(cell, slice):
            indices = getrowsindex(cell, self._data, self._index)
            newdf = {headers[item]: [self._data[item][index] for index in indices]
                     for item in columns}
            return DataFrameBasic(data=newdf, index=indices)

        elif isinstance(cell, tuple):
            # Rows
            indices = getrowsindex(cell[0], self._data, self._index)
            # Columns
            columns = getcellcols(cell[1], self._data, header=self._header)
            newdf = {item: [self._data[self._header[item]][index] for index in indices]
                     for item in columns}
            return DataFrameBasic(data=newdf, index=indices)
        #
        elif isinstance(cell, list):
            if isbool(cell):
                bool_list = cell
                newdf = getbool(self._data, columns, bool_list)
                return DataFrameBasic(data=newdf)
            else:
                # Rows
                indices = [self._index.index(x) for x in cell]
                newdf = {headers[item]: [self._data[item][index] for index in indices]
                         for item in columns}
                return DataFrameBasic(data=newdf, index=indices)
        #
        elif isinstance(cell, (int, str)):
            indices = [self._index.index(cell)]
            newdf = {headers[item]: [self._data[item][index] for index in indices]
                     for item in columns}
            return DataFrameBasic(data=newdf, index=indices)
        #
        elif isinstance(cell, SeriesItem):
            if cell._dtype == bool:
                bool_list = cell.tolist()
                newdf = getbool(self._data, columns, headers, bool_list)
                return DataFrameBasic(data=newdf)
            else:
                1 / 0
            # print('---')
        #
        else:
            raise NotImplementedError()


#
def getcol(cell, col, data, header):
    """ """
    if isinstance(cell, list):
        newdf = {header[item]: [data[item][x] for x in cell]
                 for item in col}
        return DataFrameBasic(data=newdf, index=cell)
    elif isinstance(cell, int):
        name = 'Stpy'
        tupleitem = namedtuple(name, col)
        newdf = [data[item][cell] for item in col]
        return tupleitem._make(newdf)

