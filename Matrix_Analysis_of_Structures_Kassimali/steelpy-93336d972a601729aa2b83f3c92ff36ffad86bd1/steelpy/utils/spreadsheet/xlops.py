# Copyright (c) 2022 steelpy
from __future__ import annotations
#
# Python stdlib imports
import re

# package imports
from steelpy.utils.spreadsheet.pylightxl.pylightxl import utility_index2address


def get_cell_name(cell_name:int|str|tuple) -> str:
    """ """
    if isinstance(cell_name, int): # Rows
        cell_name = f"A{cell_name}"
    
    elif isinstance(cell_name, str): # Cols
        if re.search(r"\:", cell_name):
            1/0        
        numbers = re.findall('[0-9]+', cell_name)
        if not numbers:
            cell_name = f"{cell_name}1"
            
    elif isinstance(cell_name, tuple):
        cell_name = utility_index2address(row=cell_name[1], col=cell_name[0])
        
    else:
        raise IOError(f"Cell name : {cell_name} not valid")
    
    return cell_name
#
#
def get_cellcol_name(cell_name:int|str|tuple) -> str:
    """ """
    if isinstance(cell_name, int): # Rows
        cell_name = f"{cell_name}1"
    
    elif isinstance(cell_name, str): # Cols
        if re.search(r"\:", cell_name):
            1/0        
        numbers = re.findall('[0-9]+', cell_name)
        if not numbers:
            cell_name = f"{cell_name}1"
            
    elif isinstance(cell_name, tuple):
        cell_name = utility_index2address(row=cell_name[1], col=cell_name[0])
        
    else:
        raise IOError(f"Cell name : {cell_name} not valid")
    
    return cell_name
#
#
def col_to_num(col_str):
    """ Convert base26 column string to number. """
    expn = 0
    col_num = 0
    for char in reversed(col_str):
        col_num += (ord(char) - ord('A') + 1) * 26**expn
        expn += 1
    return col_num
#
def letter2num(letters, zbase=False):
    """A = 1, C = 3 and so on. Convert spreadsheet style column enumeration to a number.
    
    Answers:
    A = 1, Z = 26, AA = 27, AZ = 52, ZZ = 702, AMJ = 1024

    >>> letter2num('A') == 1
    True
    >>> letter2num('Z') == 26
    True
    >>> letter2num('AZ') == 52
    True
    >>> letter2num('ZZ') == 702
    True
    >>> letter2num('AMJ') == 1024
    True
    >>> letter2num('AMJ', zbase=True) == 1023
    True
    >>> letter2num('A', zbase=True) == 0
    True
    """
    letters = letters.upper()
    res = 0
    weight = len(letters) - 1
    for i, c in enumerate(letters):
        res += (ord(c) - 64) * 26**(weight - i)
    if not zbase:
        return res
    return res - 1
#
#
class ColumnName(dict):
    """
    """
    def __init__(self):
        super(ColumnName, self).__init__()
        self.alphabet = string.ascii_uppercase
        self.alphabet_size = len(self.alphabet)

    def __missing__(self, column_number):
        ret = self[column_number] = self.get_column_name(column_number)
        return ret

    def get_column_name(self, column_number):
        if column_number <= self.alphabet_size:
            return self.alphabet[column_number - 1]
        else:
            return self.alphabet[int(((column_number - 1) / self.alphabet_size)) - 1] + self.alphabet[((column_number - 1) % self.alphabet_size)]
#