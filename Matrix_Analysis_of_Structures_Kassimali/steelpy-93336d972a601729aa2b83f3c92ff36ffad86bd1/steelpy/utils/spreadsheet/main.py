# Copyright (c) 2022 steelpy
from __future__ import annotations
#
# Python stdlib imports
#from typing import NamedTuple, Dict, List, Iterable, Union
import string

# package imports
from .xl_pyl import ExcelInt, NewExcelFile, CellName
from .xl_pkg import ExcelExt

#
#
class Spreadsheet:
    __slots__ = ['units', '_wb', '_wb_name',
                 '_wb_write', '_cells', 'cell_tools']
    tools:dict = {}
    
    def __init__(self):
        """
        """
        self._cells:dict = {}
        self.cell_tools = CellName()
    #
    def __setitem__(self, key, formula):
        if isinstance(formula, str) and formula[0] == '=':
            formula = formula[1:]
        else:
            formula = (formula,)
        self._cells[key] = formula
    
    def getformula(self, key):
        c = self._cells[key]
        if isinstance(c, str):
            return '=' + c
        return c[0]
    
    def __getitem__(self, key ):
        c = self._cells[key]
        if isinstance(c, str):
            return eval(c, Spreadsheet.tools, self)
        return c[0]    
    #
    def read_book(self, wb_name:str):
        """read workbook"""
        # read workbook
        try:
            #self._wb = ExcelExt(wb_name)
            self._wb = ExcelInt(wb_name)
        except PermissionError:
            self._wb = ExcelExt(wb_name)
        #
        self._wb_name = wb_name
        return self._wb
    #
    def write_book(self, wb_name:str|None = None):
        """Write excel"""
        # write workbook
        if wb_name:
            try:
                self._wb_write = ExcelExt(wb_name)
            except ModuleNotFoundError:
                self._wb_write = ExcelInt(wb_name)
        else:
            self._wb_write = NewExcelFile()     
        return self._wb_write
#
#

