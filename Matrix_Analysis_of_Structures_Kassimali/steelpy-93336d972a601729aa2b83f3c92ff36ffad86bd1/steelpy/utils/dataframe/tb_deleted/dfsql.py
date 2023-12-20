# Copyright (c) 2022 steelpy
from __future__ import annotations
#
# Python stdlib imports
import os
import sqlite3 as sqlite3
#from dataclasses import dataclass
from collections.abc import Mapping
#from collections import namedtuple
from datetime import datetime as dt, timedelta

#
# package imports
from .dfmem import DataFrameBasic
from .dfprocess import get_index, urlify, get_dtype
from .dfseries import SeriesItem
#
#
#
#@dataclass
class DataFrameSQL(DataFrameBasic):
    __slots__ = [ '_data', '_index', '_table', '_dtype', 
                  '_columns', '_header']

    def __init__(self, data: None|list|dict = None,
                 columns: None|list = None,
                 index: None|list = None,
                 dtype: None|list = None,
                 db_name: str = "df"):
        """
        data : list|dict -
        columns : list - Column labels to use for resulting frame when data does not have them.
        index : list - Index to use for resulting frame.
        dtype : list[int, float, str] - Data type to force.
        """
        self._index = index
        #super().__init__(index=index)
        # set sqlite database
        if not db_name:
            1/0
        db_file = db_name + ".db"
        try:  # remove file if exist
            os.remove(db_file)
        except FileNotFoundError:
            pass
        self._data = DataBase(db_file)
        #
        self._header = {}
        colname = {}
        indices = []
        if isinstance(data, dict):
            newdata = [ ]
            if not columns:
                columns = list(data.keys())
            
            for idx, col in enumerate(data.values()):
                indices.append(get_index(datasize=len(col), index=self._index))
                name = urlify(columns[idx])
                self._header[columns[idx]] = name
                #self._data[ name ] = ( col, name, index )
                colname[name] = get_sql_type(col)
                newdata.append(col)
            #newdata.insert(0, indices[0])
            #newdata =  list(map(list, zip(*newdata)))
        elif isinstance(data, (list, tuple)):
            #
            if isinstance(data[0], list):
                newdata = list( map( list, zip( *data ) ) )
                if not columns:
                    columns = [x for x in range(len(newdata))]
                for x, col in enumerate( newdata ):
                    name = urlify(columns[x])
                    self._header[columns[x]] = name
                    #index = get_index ( datasize=len ( col ), index=self._index )
                    #self._data[ name ] = ( col, name, index )
                    indices.append(get_index( datasize=len(col), index=self._index ) )
                    colname[ name ] = get_sql_type( col )
                #newdata.insert(0, indices[0])
                #newdata =  list(map(list, zip(*newdata)))
            elif isinstance ( data[ 0 ], dict ):
                raise NotImplementedError ( 'dictonary' )

            else:
                1/0
                data = list(map(list, zip(*data)))
                for x, col in enumerate ( columns ):
                    name = urlify(col)
                    self._header[col] = name
                    index = get_index ( datasize=len ( data[ x ] ), index=self._index )
                    self._data[ name ] = ( data[ x ], name, index )
            # for key, item in data.items():
            #    self._data[key] = SeriesItem(item)
        else:
            1/0
            pass
        #
        #if self._index:
        #    newdata.insert( 0, self._index )
        #    index_type = get_type(self._index)
        if not self._index:
            self._index = indices[ 0 ]
            #newdata.insert ( 0, indices[ 0 ] )
            #index_type = get_type( index[ 0 ] )
        newdata.insert( 0, self._index )
        index_type = get_sql_type(self._index)        
        #
        newdata = list( map( list, zip ( *newdata ) ) )
        self._data.set_table(name="df", columns=colname, index=index_type)
        #
        headers = list(colname.keys())
        self._data.push_data(headers, newdata, index=index_type)
        #
        #print('--')
    #
    def __setitem__(self, name:str, value:list|int|float|str) -> None:
        """
        """
        indices = self._data.get("Indices")
        if isinstance(name, list):
            #print('---')
            input_data = []
            index = []
            for item in name:
                try: # existing data
                    self._data._columns[item]
                    input_data.append(self._data.update_column)
                    index.append(indices)
                except KeyError: # new data
                    input_data.append(self._data.insert_column)
                    index.append(["" for _ in indices])
        else:
            # get funtion
            try: # existing data
                self._data._columns[name]
                input_data = self._data.update_column
                index = indices
            except KeyError: # new data
                input_data = self._data.insert_column
                index = ["" for item in indices]
        #
        if isinstance( value, list ):
            1/0
            if len(value) == len(self._index):
                self._data[ name ] = SeriesItem ( value, name=name )
            else:
                raise IOError ( "new list size not compatible" )

        elif isinstance(value, (int, float, str)):
            1/0
            new_value = [ value for _ in self._index ]
            self._data[ name ] = SeriesItem( new_value, name=name )
        
        elif isinstance( value, SeriesItem ):
            #newdata = [(a,) for a in value]
            newdata = get_newdata(value._dtype, value, index)
            #if value._dtype == timedelta:
            #    #print('----')
            #    try:
            #        newdata =  [(a.days,b) if b != "" else (a,)
            #                    for a,b in zip(value,index)]
            #    except AttributeError:
            #        newdata =  [(a,b) if b != "" else (a,)
            #                    for a,b in zip(value,index)]                    
            #else:
            #    newdata =  [(a,b) if b != "" else (a,)
            #                for a,b in zip(value,index)]
            newname = {name:type_conversion(value._dtype)}
            input_data(newname, newdata)
        
        elif isinstance(value, DataFrame):
            for x, key in enumerate(name):
                serie = value[key]
                newdata = get_newdata(serie._dtype, serie, index[x])
                #newdata =  [(a,b) if b != "" else (a,)
                #            for a,b in zip(serie,index[x])]
                newname = {key:type_conversion(serie._dtype)}
                input_data[x](newname, newdata)                
            
        else:
            raise IOError ( "input data not compatible" )
    #
    #def __getitem__(self, name:list|str) -> dict:
    #    """
    #    """
    #    if isinstance(name, (list, tuple)):
    #        newdf = {}
    #        for item in name:
    #            newname = self._header[item]
    #            newdf[item] = self._data[newname]
    #        return DataFrameBasic(newdf)
    #    else:
    #        newname = self._header[name]
    #        return self._data[newname]
    #
    #
    #def __getattr__(self, name):
    #    """ """
    #    if name in self.__slots__:
    #        return self[name]
    #
    #    try:
    #        return self._data[name]
    #    except KeyError:
    #        raise AttributeError(f"object has no attribute '{name}'")
    #
    #def __setattr__(self, name, value):
    #    """ """
    #    try:
    #        super().__setattr__(name, value)
    #    except AttributeError:
    #        self.__setitem__(name, value)
    #
    @property
    def columns(self):
        """ The column labels of the DataFrame."""
        col = self._header.keys()
        name = "Index"
        return SeriesItem(col, name=name, index=self._index)
    
    @columns.setter
    def columns(self, value:list):
        """ """
        if len(value) == len(self._header):
            self._header = {value[idx] : item
                            for idx, item in enumerate(self._header.values())}
            #print('--')
        else:
            raise IOError('Data not compatible')
    #
#
#
#
class DataBase(Mapping):
    __slots__ = ['_conn', '_cursor', '_name', '_columns',
                 '_dtype', '_types']

    def __init__(self, db_file:None|str)-> None:
        """ """
        #self._name: list = []
        self._conn = None
        self._cursor = None
        if db_file:
            self.open(db_file)
        #
        self._types = {"INTEGER":int, "TEXT":str, "DECIMAL":float,
                       "TIMESTAMP":dt, "TIMEDELTA":timedelta,
                       "BOOLEAN":bool, "BLOB":bytes}        
    #
    #def __setitem__(self, index:str|int, values:list):
    #    """Construct a new list object"""
    #    data, name, index = values
    #    # get name
    #    self._name.append(name)
    #    # get index
    #    datasize = len(data)
    #    _index = [x for x in range(datasize)]
    #    if index:
    #        if len(index) == datasize:
    #            _index = index
    #        else:
    #            raise IOError('index =! data')
    #
    #
    def __getitem__(self, name):
        """Return the ith index of self."""
        index = self.get("Indices")
        if isinstance(name, (list, tuple)):
            newdf = {}
            dtype = []
            for item in name:
                dtype.append(self._dtype[name])
                newdf[item] = self.get(name)
            return DataFrame(newdf)
        else:
            col = self.get(name)
            dtype = self._dtype[name]
            return SeriesItem(col, name=name, index=index, dtype=dtype)
    #
    def __len__(self) -> int:
        """Return the dimension of self."""
        return len(self._columns)

    def __iter__(self):
        """
        """
        return iter(self._columns)

    def __contains__(self, value) -> bool:
        return value in self._columns
    #
    #
    #
    #
    def open(self,db_file:str):
        """ opens a new database connection"""
        try:
            self._conn = sqlite3.connect(db_file);
            self._cursor = self._conn.cursor()
        except sqlite3.Error as e:
            print("Error connecting to database!")
    #
    def get(self, columns:str, limit:None|int=None):
        """ fetch/query data from a database
        columns: The string of columns, comma-separated, to fetch.
        limit: a limit of items to fetch (option).
        """
        table = self._name
        query = f"SELECT {columns} from {table};"
        self._cursor.execute(query)      
        # fetch data
        rows = self._cursor.fetchall()
        #dtype = self._columns[columns]
        rows = [item[0] for item in rows]
        #rows = rows[len(rows)-limit if limit else 0:]
        #dtype = self._dtype[columns]
        #if dtype == "INTEGER":
        #    return 
        return rows[len(rows)-limit if limit else 0:]
    #
    def set_table(self, name:str, columns:dict, 
                  index:str|int|None=None) -> str:
        """Create new table"""
        # get types 
        self._dtype = {key:self._types[item] for key, item in columns.items()}  
        #
        newname = "tb_" + name
        table = f"CREATE TABLE IF NOT EXISTS {newname} ("
        if index:
            table += f"Indices {index} PRIMARY KEY NOT NULL, "
            self._dtype.update({"Indices":index})
        table += ", ".join([f"{key} {item}" for key, item in columns.items()])
        table += ");"
        # push table
        try:
            self._conn.execute(table)
        except sqlite3.Error as e:
            raise RuntimeError(e)
        #
        self._columns = columns
        self._name = newname
        #
    #
    def modify_table(self, name:str, dtype:str):
        """ Add new column"""
        # ALTER TABLE TableName ADD COLUMN COLNew CHAR(25)
        table_name = self._name
        table = f"ALTER TABLE {table_name} ADD COLUMN {name} {dtype}"
        #table += ", ".join([f"{key} {item}" for key, item in name.items()])
        #table += ");"        
        # push table
        try:
            self._conn.execute(table)
        except sqlite3.Error as e:
            raise RuntimeError(e)
        self._conn.commit()
    #
    def push_data(self, header, data, index:str|None=None):
        """push data to table"""
        #
        table_name = self._name
        #columns = list(self._columns.keys())
        #
        table = f"INSERT INTO {table_name} ("
        if index:
            table += f" Indices, "
        table += ", ".join([item for item in header])
        table += ") VALUES( "
        if index:
            table += "?, "
        table += ", ".join(["?" for item in header])
        table += ")"
        #sql = 'INSERT INTO tb_Nodes(name, type,\
        #                            x, y, z, r, theta, phi)\
        #                            VALUES(?,?,?,?,?,?,?,?)'
        #cur = conn.cursor ()
        try:
            self._conn.executemany(table, data)
        except sqlite3.Error as e:
            raise RuntimeError(e)
        self._conn.commit()
    #
    def insert_column(self, name:dict, data:list):
        """ """
        table_name = self._name
        # inser new column in existing table
        for key, item in name.items():
            self.modify_table(key, item)
        #
        #table = f"INSERT INTO {table_name} VALUES (?)"
        #
        header = list(name.keys())
        # UPDATE myTable SET formatteddate = (?)
        table = f"UPDATE {table_name} SET ("
        table += ", ".join([item for item in header])
        table += ") = ( "
        table += ", ".join(["?" for item in header])
        table += ")"        
        #
        try:
            self._conn.executemany(table, data)
            #cursor.executemany("INSERT INTO my_table VALUES (?)", [(a,) for a in listA])
        except sqlite3.Error as e:
            raise RuntimeError(e)
        self._conn.commit()        
        #
        self._columns.update(name)
        #self._dtype.update({key:type(item) for key, item in name.items()})
        self._dtype = update_type(name, self._dtype)
        #print('--')
    #
    def update_column(self, name:dict, data:list):
        """ """
        #index = self.get("indices")
        table_name = self._name
        header = list(name.keys())
        #
        table = f"UPDATE {table_name} SET "
        table += "=?, ".join([item for item in header])
        table += "=? WHERE Indices = ?"
        #
        try:
            self._conn.executemany(table, data)
            #cursor.executemany("INSERT INTO my_table VALUES (?)", [(a,) for a in listA])
        except sqlite3.Error as e:
            raise RuntimeError(e)
        self._conn.commit()
        #
        self._dtype = update_type(name, self._dtype)
        #self._dtype.update({key:type(item) for key, item in name.items()})
       #for key, item in name.items():
       #    if item == "TIMESTAMP":
       #        self._dtype.update({key:dt})
       #    elif item == "TIMEDELTA":
       #        self._dtype.update({key:timedelta})
       #    else:
       #        self._dtype.update({key:type(item)})
       #    #print(key, item, self._dtype[key])
       ##print('--')
#
def update_type(name, dtype):
    """ """
    for key, item in name.items():
        if item == "TIMESTAMP":
            dtype.update({key:dt})
        elif item == "TIMEDELTA":
            dtype.update({key:timedelta})
        else:
            dtype.update({key:type(item)})
    return dtype
#
#
#
def get_sql_type(data):
    """ """
    dtype = get_dtype(data)
    return type_conversion(dtype)
#
#
def type_conversion(dtype):
    """ """
    #if dtype == float:
    #    print('---')
    if dtype == int:
        return "INTEGER"
    elif dtype == str:
        return "TEXT"
    elif dtype == float:
        return "DECIMAL"
    elif dtype == dt:
        return "TIMESTAMP"
    elif dtype == timedelta:
        return "TIMEDELTA"
    elif dtype == bool:
        return "BOOLEAN"
    elif type == bytes:
        return "BLOB"
    elif type == object:
        return "BLOB"
    else:
        raise IOError(f'dtype not recognized {dtype}')
#
#
def get_newdata(dtype, value, index):
    """ """
    if dtype == timedelta:
        #print('----')
        try:
            newdata =  [(a.days,b) if b != "" else (a.days,)
                        for a,b in zip(value,index)]
        except AttributeError:
            newdata =  [(a,b) if b != "" else (a,)
                        for a,b in zip(value,index)]
    
    elif dtype == dt:
        try:
            newdata =  [(a.strftime('%Y/%m/%d'),b) if b != "" else (a.strftime('%Y/%m/%d'),)
                        for a,b in zip(value,index)]
        except AttributeError:
            newdata =  [(a,b) if b != "" else (a,)
                        for a,b in zip(value,index)]        
    
    else:
        newdata =  [(a,b) if b != "" else (a,)
                    for a,b in zip(value,index)]
    
    return newdata
#
#