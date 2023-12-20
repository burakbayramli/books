# Copyright (c) 2022 steelpy
from __future__ import annotations
#
# Python stdlib imports
from pathlib import Path
import re

# package imports
from .pylightxl.pylightxl import (readxl, readcsv, 
                                  writexl, writecsv,
                                  Database, 
                                  utility_address2index,
                                  utility_columnletter2num,
                                  utility_index2address)
from steelpy.utils.dataframe.main import DBframework
from .xlops import get_cell_name, get_cellcol_name

#
class ExcelInt:
    __slots__ = ['units', '_wb', '_ws', '_wb_name']

    def __init__(self, wb_name: str):
        """
        """
        my_pathlib = Path(wb_name)
        self._wb = readxl(my_pathlib)
        self._ws = Sheets(self._wb)
        self._wb_name = my_pathlib
    #
    @property
    def sheets(self):
        """ """
        return self._ws
    #
    @property
    def sheet_names(self):
        """ """
        return self._wb.ws_names
    #
    def save(self, wb_name:str|None =None):
        """ """
        if not wb_name:
            wb_name = "copy_" + self._wb_name
        writexl(db=self._wb, fn=wb_name)
#
#
class Sheets:
    __slots__ = ['units', '_wb', '_cells']

    def __init__(self, wb: str):
        """
        """
        self._wb = wb
        #self._cells = xlCells()
    #
    def __getitem__(self, ws_name: str):
        """
        """
        s_name = self._wb.ws_names
        if ws_name in s_name:
            #return self._wb.ws(ws=ws_name)
            return SheetOps(self._wb, ws_name)
        else:
            raise IOError(f"sheet {ws_name} not found")
    #
    #def __setitem__(self, ws_name: str, value) -> None:
    #    """
    #    """
    #    pass
    #
#
#
class SheetOps:
    __slots__ = ['_wb', '_ws_name', '_df']

    def __init__(self, wb, ws_name: str):
        """ ws : sheet"""
        self._wb = wb
        self._ws_name = ws_name
        self._df = DataFrameOps(wb=self._wb, ws_name=self._ws_name)
    #
    #
    def __setitem__(self, cell_name:float|int|str|tuple, 
                    value:int|float|str|tuple|list) -> None:
        """
        """
        # if re.search(r"\:", cell_name):
        # tokens = re.split(r'[:]', cell_name)
        # row_i, col_i = utility_address2index(tokens[0])
        # row_j, col_j = utility_address2index(tokens[1])
        # for i in range(row_i, row_j+1):
        #    for j in range(col_i, col_j+1):
        #        self._wb.ws(ws=self._ws_name).update_index(row=i, col=j, val=value)
        if isinstance(value, list):
            row_i, col_j = utility_address2index(cell_name)
            columns = len(value)
            if isinstance(value[0], list):
                rows =  len(value[0])
                for j in range(columns):  # columns
                    for i in range(rows):  # rows
                        print('---', i)
                        self._wb.ws(ws=self._ws_name).update_index(row=row_i + i,
                                                                   col=col_j + j,
                                                                   val=value[j][i])
            else:
                for j in range(columns):  # columns
                    self._wb.ws(ws=self._ws_name).update_index(row=row_i,
                                                               col=col_j + j,
                                                               val=value[j])
        else:
            #row, cname = utility_address2index(cell_name)
            #self._wb.ws(ws=self._ws_name).update_index(row=row, col=cname, val=value)
            cell_name = get_cell_name(cell_name)
            self._wb.ws(ws=self._ws_name).update_address(address=cell_name, val=value)

    #
    def __getitem__(self, cell_name: str):
        """
        """
        if re.search(r"\:", cell_name):
            return self._wb.ws(ws=self._ws_name).range(address=cell_name)
            #
            #tokens = re.split(r'[:]', cell_name)
            #row_i, col_i = utility_address2index(tokens[0])
            #row_j, col_j = utility_address2index(tokens[1])
            #for i in range(row_i, row_j + 1):
            #    for j in range(col_i, col_j + 1):
            #        #self._wb.ws(ws=self._ws_name).update_index(row=i, col=j, val=value)
            #        self._wb.ws(ws=self._ws_name).index(row=i, col=j)
        else:
            return self._wb.ws(ws=self._ws_name).address(address=cell_name)
            # row_id, col_id = utility_address2index(cell_name)
            # return self._wb.ws(ws=self._ws_name).index(row=row_id, col=col_id)
    #
    @property
    def column(self):
        """ """
        return xlColumn(self._wb, self._ws_name)
    #
    @property
    def row(self):
        """ """
        return xlRow(self._wb, self._ws_name)     
    #
    #
    def key(self, row:int|str|None=None,
            column:int|str|None=None):
        """ """
        #ws_name = self._ws_name
        #
        #if row:
        #    if isinstance(column, str):
        #        
        #
        #if column:
        #    if isinstance(column, str):
        return self._wb.ws(ws=self._ws_name).ssd(keyrows=row, keycols=column)
    #
    #
    def to_df(self, db_name:str|None=None,
              skiprows:int|list=0, names:str|list|None=None):
        """ convert sheet data to dataframe"""
        return self._df.get_df(db_name=db_name, skiprows=skiprows, 
                               names=names)
    #
    #
    #@property
    #def dataframe(self):
    #    """ """
    #    return self._df
    #    
    #
    def df_tool(self, index:int|bool=False, header:bool=False,
                dates:None=None):
        """ 
        index : When writing, include or exclude the index by setting it to True or False
        header : When writing, include or exclude the index and series names by setting it to True or False
        dates : 
        """
        return DataFrameOps(wb=self._wb, ws_name=self._ws_name,
                            index=index, header=header)
    #
#
#
#def get_data(sheet):
#    """ """
#    cell_obj = []
#    for row in sheet.rows:
#        cell_obj.append([])
#        for cell in row:
#            cell_obj[-1].append(cell.value)
#    return cell_obj
#
#
#class xlset:
#    __slots__ = ['_ws']
#
#    def __init__(self, ws):
#        """ ws : sheet"""
#        self._ws = ws
#
#    #
#    #
#    def __setitem__(self, cell_name: str, value: float) -> None:
#        """
#        """
#        self._ws.range(cell_name).value = value
#
#    #
#    def __getitem__(self, cell_name: int):
#        """
#        """
#        return self._ws.range(cell_name).value
#
#
class NewExcelFile:
    __slots__ = ['_wb', '_wb_name']

    def __init__(self, wb_name:str=None):
        """
        """
        self._wb = Database()
        if wb_name:
            self._wb_name = wb_name
    #
    #
    def sheet_active(self, ws_name: str):
        """ """
        self._wb.add_ws(ws=ws_name)
        return xlsetNew(wb=self._wb, ws_name=ws_name)

    #
    def close(self, file_name:str):
        """ """
        #fn = self._wb_name + ".xlsx"
        #fn = os.path.join(self._path, fn)
        #
        file_name = file_name+".xlsx"
        writexl(db=self._wb, fn=file_name)
#
#
class xlsetNew:
    __slots__ = ['_wb', '_ws_name']

    def __init__(self, wb, ws_name: str):
        """ ws : sheet"""
        self._wb = wb
        self._ws_name = ws_name
        #self._col2name = ColumnName()
    #
    #
    def __setitem__(self, cell_name:str|list, value:list|str|int|float) -> None:
        """
        """
        # if re.search(r"\:", cell_name):
        # tokens = re.split(r'[:]', cell_name)
        # row_i, col_i = utility_address2index(tokens[0])
        # row_j, col_j = utility_address2index(tokens[1])
        # for i in range(row_i, row_j+1):
        #    for j in range(col_i, col_j+1):
        #        self._wb.ws(ws=self._ws_name).update_index(row=i, col=j, val=value)
        if isinstance(value, list):
            if isinstance(cell_name, tuple):
                col_j = cell_name[0]
                row_i = cell_name[1]
            else:
                row_i, col_j = utility_address2index(cell_name)
            columns = len(value)
            if isinstance(value[0], list):
                rows =  len(value[0])
                for j in range(columns):  # columns
                    for i in range(rows):  # rows
                        #print('---', i)
                        self._wb.ws(ws=self._ws_name).update_index(row=row_i + i, 
                                                                   col=col_j + j,
                                                                   val=value[j][i])                        
            else:
                for j in range(columns):  # columns
                    self._wb.ws(ws=self._ws_name).update_index(row=row_i, 
                                                               col=col_j + j,
                                                               val=value[j])
        #elif isinstance(value, tuple):
        #    self.cell_name(col, row)
        #    
        else:
            self._wb.ws(ws=self._ws_name).update_address(address=cell_name, val=value)

    #
    def __getitem__(self, cell_name: str):
        """
        """
        if re.search(r"\:", cell_name):
            tokens = re.split(r'[:]', cell_name)
            row_i, col_i = utility_address2index(tokens[0])
            row_j, col_j = utility_address2index(tokens[1])
            for i in range(row_i, row_j + 1):
                for j in range(col_i, col_j + 1):
                    self._wb.ws(ws=self._ws_name).update_index(row=i, col=j, val=value)
        else:
            return self._wb.ws(ws=self._ws_name).index(address=cell_name)
            # row_id, col_id = utility_address2index(cell_name)
            # return self._wb.ws(ws=self._ws_name).index(row=row_id, col=col_id)
    #
    #def cell_name(self, column: int, row: int):
    #    """ return cell name"""
    #    col = self._col2name.get_column_name(column)
    #    name = f"{col}{row}".upper()
    #    return name
#
#
class xlColumn:
    __slots__ = ['_wb', '_ws_name']

    def __init__(self, wb, ws_name: str):
        """ ws : sheet"""
        self._wb = wb
        self._ws_name = ws_name
    #
    def __setitem__(self, col_name:float|int|str|tuple, 
                    value:int|float|str|tuple|list) -> None:
        """
        """
        col_name = get_cellcol_name(col_name)
        
        #if isinstance(col_name, int):
        #    cname = col_name
        #
        #elif isinstance(col_name, str):
        #    numbers = re.findall('[0-9]+', col_name)
        #    if numbers:
        #        row, cname = utility_address2index(col_name)
        #        #row, cname
        #    else:
        #        cname = utility_columnletter2num(col_name)
        #        row = 1
        #
        #else:
        #    raise IOError(f"column name : {col_name} not valid")
        #
        #self._wb.range(col_name).value = value
        row, cname = utility_address2index(col_name)
        for x, item in enumerate(value):
            row += x
            self._wb.ws(ws=self._ws_name).update_index(row=row, col=cname, val=item)

    #
    def __getitem__(self, col_name:int|str):
        """
        """
        if isinstance(col_name, int):
            return self._wb(ws=self._ws_name).col(col=col_name)
        
        elif isinstance(col_name, str):
            numbers = re.findall('[0-9]+', col_name)
            if numbers:
                row, cname = utility_address2index(col_name)
                col = self._wb.ws(ws=self._ws_name).col(col=cname)
                col = col[row-1:]
                return col
            else:
                cname = utility_columnletter2num(col_name)
                return self._wb.ws(ws=self._ws_name).col(col=cname)
        
        else:
            raise IOError(f"column name : {col_name} not valid")
#
#
class xlRow:
    __slots__ = ['_wb', '_ws_name']

    def __init__(self, wb, ws_name: str):
        """ ws : sheet"""
        self._wb = wb
        self._ws_name = ws_name
    #
    def __setitem__(self, row_name: str, value: float) -> None:
        """
        """
        if isinstance(row_name, int):
            row = row_name
        
        elif isinstance(row_name, str):
            numbers = re.findall('[0-9]+', row_name)
            if numbers:
                row, col = utility_address2index(row_name)
            else:
                col = utility_columnletter2num(row_name)
                row = 1            
            
        else:
            raise IOError(f"row name : {row_name} not valid")
        #
        for x, item in enumerate(value):
            col += x
            self._wb.ws(ws=self._ws_name).update_index(row=row, col=col, val=item)        

    #
    def __getitem__(self, row_name: int):
        """
        """
        if isinstance(row_name, int):
            return self._wb.ws(ws=self._ws_name).row(row=row_name)
        
        elif isinstance(row_name, str):
            numbers = re.findall('[0-9]+', row_name)
            if numbers:
                row, cname = utility_address2index(row_name)
                #col = self._wb.ws(ws=self._ws_name).row(row=row)
                #return col[cname-1:]
            else:
                cname = utility_columnletter2num(row_name)
                row = 1
            #
            col = self._wb.ws(ws=self._ws_name).row(row=row)
            return col[cname-1:]            
        else:
            raise IOError(f"row name : {row_name} not valid")
#
#
#
class CellName(dict):
    """
    """

    def __init__(self):
        import string
        super(CellName, self).__init__()
        self.alphabet = string.ascii_uppercase
        self.alphabet_size = len(self.alphabet)

    def __missing__(self, column_number:int):
        ret = self[column_number] = self.get_col_name(column_number)
        return ret

    def get_col_name(self, column_number):
        """ """
        if column_number <= self.alphabet_size:
            return self.alphabet[column_number - 1]
        else:
            return self.alphabet[int(((column_number - 1) / self.alphabet_size)) - 1] + self.alphabet[
                ((column_number - 1) % self.alphabet_size)]
    
    def get_col_number(self, col_name:str):
        """ """
        numbers = re.findall('[0-9]+', col_name)
        if numbers:
            row, cname = utility_address2index(col_name)
            #row, cname
        else:
            cname = utility_columnletter2num(col_name)
            row = 1
        return cname, row
    
    def cell_name(self, column: int, row: int):
        """ return cell name"""
        col = self.get_col_name(column)
        name = f"{col}{row}".upper()
        return name    
#
#
def get_row_column(data_list: list, header: str):
    """ """
    row = [x for x, item in enumerate(data_list) if header in item]
    col = [x for x, item in enumerate(data_list[row[0]]) if item == header]
    return [row[0], col[0]]
#
#
class DataFrameOps:
    __slots__ = ['_wb', '_ws_name', '_column',
                 '_index', '_header', '_df']

    def __init__(self, wb, ws_name:str,
                 index:int|bool=False, header:bool=False):
        """ ws : sheet"""
        self._wb = wb
        self._ws_name = ws_name
        self._index = index
        self._header = header        
        self._column = xlColumn(self._wb, self._ws_name)
        self._df = DBframework()
    #
    #
    def __setitem__(self, cell_name:str|int|tuple, df) -> None:
        """
        """
        cell_name = get_cell_name(cell_name)
        #
        if re.search(r"\:", cell_name):
            1/0
        else:
            # [row, col]
            index = utility_address2index(cell_name)
            for x, (key, value) in enumerate(df._header.items()):
                row = index[0]
                col = index[1] + x
                if self._header:
                    self._wb.ws(ws=self._ws_name).update_index(row=row, 
                                                               col=col, 
                                                               val=value)
                    row += 1
                address = utility_index2address(row, col)
                self._column[address] = df._data[value]
    # __call__
    def get_df(self, db_name:str|None=None,
               skiprows:int|list=0, names:str|list|None=None):
        """ """
        # 
        columns = []
        for col in self._wb.ws(ws=self._ws_name).cols:
            columns.append(col)
            #columns.append([string2number(item) for item in col])
        #
        if isinstance(skiprows, (list, tuple)):
            print('---')
            1/0
        else:
            if names:
                data = {names[idx]: col[skiprows+1:] 
                        for idx, col in enumerate(columns)}
            else:
                data = {col[skiprows]: col[skiprows+1:] 
                        for col in columns}
        #print('--')
        return self._df.DataFrame(data, columns=names)
    #
#
#