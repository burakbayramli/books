# 
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
# Python stdlib imports
import sys
from collections import defaultdict

# package imports
#


#
def check_input_file(file: str,
                     file_extension:str|bool=False):
    """
    """
    if not file:
        raise IOError(f"*** file {file} no found")
    
    file_name = file.split(".")
    
    if len(file_name) == 1:
        file_extension = file_extension.strip()
        file = file + '.' + file_extension
        extension = file_extension
    
    else:
        # check file extension
        extension = file_name[-1]
        extension = str(extension).lower()
        extension = str(extension).strip()
        #
        if file_extension:
            file_extension = file_extension.strip()
            if file_extension != extension:
                file = file.remove(extension, "")
                file = file.strip()
                file = file + '.' + file_extension
                extension = file_extension
    try:
        test_input = open(file)
        file_out = file
    except IOError:
        raise IOError(f"*** file {file} no found")
    #
    return file_out


#
#
def remove_file_extension(file):
    """
    """
    # check file extension
    file = str(file)
    _file_name = file.split(".")
    
    extension = _file_name[-1]
    extension = str(extension).lower()
    extension = str(extension).strip()    
    
    _file_name = _file_name[0]
    _file_name = _file_name.strip()
    
    return _file_name, extension
#
#
def read_raw_data(file, factor=1.0):
    """
    TH[0] : Time(sec)
    TH[1] : Accel(G)
    """
    # 
    _TH = []
    for line in open(file):

        # Jump empty lines
        if  not line: 
            continue
        # Jump commented lines
        if re.match(r"#|\*|'|\"|!|%|/{2}",line) : 
            continue
        # remove separators
        line = re.sub(r",|;|=|:|&|\n", " ", line)
        # ignore header lines
        if re.match(r"([a-z])", line, re.IGNORECASE) : 
            continue        
        #    Segment line
        keyword = line.split()
        #
        for x in range(len(keyword)):
            
            try:
                _TH[x].append(float(keyword[x])*factor)
            
            except IndexError:
                _TH.append([float(keyword[x])*factor])
        #
    #
    return _TH
    #
#
#
def split_file_name(file):
    """
    """
    file = str(file)
    fileName = file.split(".")
    #
    return fileName
    #
#
#
#
#
def read_cvs(file_name, read_data):
    """
    """
    import csv
    with open(file_name, 'r') as f:
        reader = csv.reader(f)
        csv_list = list(reader)
    #
    start = max(read_data['row']['start'] - 1, 0)
    end = len(csv_list)
    if read_data['row']['end']:
        end = int(read_data['row']['end']) - 1
    #
    list_column = read_data['column']['number']
    #new_list = [[l[i] for i in list_column] for l in csv_list[start:end]]
    name_column = read_data['column']['name']
    new_list = {}
    for x, i in enumerate(list_column):
        new_list[name_column[x]] = [float(l[i-1]) for l in csv_list[start:end]]
    #
    return new_list
#
def read_text(file_name, read_data, delimeter=None):
    """
    """
    start = max(read_data['row']['start'] - 1, 0)
    end = None
    if read_data['row']['end']:
        end = int(read_data['row']['end']) - 1    
    #
    text_list = [] 
    # dct = defaultdict(list)
    with open(file_name) as f:
        for x, line in enumerate(f):
            if x < start:
                continue
            #text, cat = line.rstrip("\n").split(delimeter)
            text_list.append(line.rstrip("\n").split(delimeter))
            #dct[cat].append(text)
    #
    list_column = read_data['column']['number']
    name_column = read_data['column']['name']
    #
    if not end:
        end = len(text_list)
    #
    new_list = {}
    for x, i in enumerate(list_column):
        new_list[name_column[x]] = [float(l[i-1]) for l in text_list[:end]]
    
    return new_list
#
def read_xlsx(file_name, sheet, read_data):
    """
    read xlsx excel file
    """
    import fem2ufo.process.modules.spreadsheet as spreadsheet
    #
    start = int(read_data['row']['start'])
    end = 2**20
    if read_data['row']['end']:
        end = int(read_data['row']['end'])
    #
    list_column = read_data['column']['number']
    name_column = read_data['column']['name']    
    #
    _name = spreadsheet.ColumnName()
    _column_name = []
    for _column in list_column:
        _column_name.append(_name[_column])
    #
    _book = spreadsheet.Workbook(file_name) #Open xlsx file
    
    try:
        _sheet = _book[sheet]
    except KeyError:
        print('    *** error spreadsheet sheet {:} not found\n'
              .format(sheet))
        ExitProg = input('Press Enter Key To Continue')
        sys.exit()
    #
    new_list = defaultdict(list)
    _flag = False
    for row, cells in _sheet.rowsIter():
        if row < start:
            continue
        #
        #print('row ',row,' cell ', cells[0].value)
        #
        _cells_name = {_cell.column: _cell for _cell in cells}
        #
        for x, _name in enumerate(_column_name):
            try:
                new_list[name_column[x]].append(float(_cells_name[_name].value))
            except ValueError:
                print('--- Reached end of file at row {:}'.format(row))
                _flag = True
                break
            except KeyError:
                #print('--- Reached end of column {:} at row {:}'
                #      .format(_name, row))
                continue
                #_flag = True
                #break            
            except IndexError:
                #print('--- Reached end of column {:} at row {:}'
                #      .format(_name, row))
                # _flag = True
                continue
        #
        if _flag or row == end:
            break
    #
    return new_list
#