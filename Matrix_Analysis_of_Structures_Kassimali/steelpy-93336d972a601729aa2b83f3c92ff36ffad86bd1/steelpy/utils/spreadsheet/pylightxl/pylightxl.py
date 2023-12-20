########################################################################################################
# SEC-00: PREFACE
########################################################################################################
"""
Title: pylightxl
Developed by: pydpiper
Version: 1.61
License: MIT

Copyright (c) 2019 Viktor Kis

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

Source: https://github.com/PydPiper/pylightxl

Documentation: https://pylightxl.readthedocs.io/en/latest/

Description: Pylightxl is a light-weight Microsoft Excel cell value reader/writer. Its strength over
existing libraries comes from the fact that pylightxl has zero non-standard libraries (zero-dependency),
supports python2-3, and its light-weight single file size makes it favorable to copy pylightxl into
your own projects for true zero-dependency. Please see documentation for full list of capabilities and
limitations.

Developers Notes:
    - always write test cases first
    - strive for simple/intuitive API interface
    - write docstrings with type annotations (unfortunately type-hints are not python2 compatible)
    - write documentation as a function is developed
    - zipfile from python 2.7.18 comes with zipfile 1.6 that doesnt come with file.seek method
      this is why readxl function open zip files 2x (once for namespace and once for tree)

Code Structure:
    - SEC-00: PREFACE
    - SEC-01: IMPORTS
    - SEC-02: READXL FUNCTIONS
    - SEC-03: WRITEXL FUNCTIONS
    - SEC-04: DATABASE FUNCTIONS
    - SEC-05: UTILITY FUNCTIONS

"""

########################################################################################################
# SEC-01: IMPORTS
########################################################################################################


import zipfile
import re
import os
import sys
import shutil
import warnings
from xml.etree import cElementTree as ET
import time
from datetime import datetime, timedelta

EXCEL_STARTDATE = datetime(1899,12,30)
MAX_XL_ROWS = 1048576
MAX_XL_COLS = 16384

########################################################################################################
# SEC-02: PYTHON2 COMPATIBILITY
########################################################################################################


# unicode is a python27 object that was merged into str in 3+, for compatibility it is redefined here
if sys.version_info[0] < 3:
    FileNotFoundError = IOError
    PermissionError = Exception
    WindowsError = Exception
    FileExistsError = Exception
    import cgi as html
    PYVER = 2
else:
    unicode = str
    WindowsError = Exception
    import html, pathlib, io
    from typing import Union, List, Dict, Iterable
    PYVER = 3

########################################################################################################
# SEC-03: READXL FUNCTIONS
########################################################################################################

def readxl(fn, ws=None):
    # type: (Union[str, pathlib.Path], Union[str,List[str]]) -> Database
    """Reads an xlsx or xlsm file and returns a pylightxl database

    :param fn: Excel file path, also supports Pathlib.Path object, as well as file-like object from with/open
    :type fn: Union[str, pathlib.Path]
    :param ws: sheetnames to read into the database, if not specified - all sheets are read
                entry support single ws name (ex: ws='sh1') or multi (ex: ws=['sh1', 'sh2']), defaults to None
    :type ws: Union[str,List[str]], optional
    :return: pylightxl Database 
    :rtype: Database
    """

    if type(ws) is str:
        ws = (ws,)

    # declare a db
    db = Database()

    fn = readxl_check_excelfile(fn)

    # {'ws': ws1: {'ws': str, 'rId': str, 'order': str, 'fn_ws': str}, ...
    #  'nr': {nr1: {'nr': str, 'ws': str, 'address': str}, ...}
    wb_rels = readxl_get_workbook(fn)

    for nr_dict in wb_rels['nr'].values():
        name = nr_dict['nr']
        worksheet = nr_dict['ws']
        address = nr_dict['address']
        # note that nr adds the ws if they are not already in the wb, thus it needs to be filtered here if the user didnt want those sheets then the nr are also not loaded
        if ws is None or worksheet in ws:
            db.add_nr(name=name, ws=worksheet, address=address)

    # get common string cell value table
    sharedString = readxl_get_sharedStrings(fn)
    # get styles for datetime parsing
    styles = readxl_get_styles(fn)

    # put the ws in order
    ordered_ws = {}
    for worksheet in wb_rels['ws'].keys():
        order = wb_rels['ws'][worksheet]['order']
        ordered_ws[order] = worksheet

    # scrape each sheet#.xml file
    if ws is None:
        # get all worksheets
        for order in sorted(ordered_ws.keys()):
            worksheet = ordered_ws[order]
            fn_ws = wb_rels['ws'][worksheet]['fn_ws']
            comments = readxl_get_ws_rels(fn, fn_ws)
            data = readxl_scrape(fn, fn_ws, sharedString, styles, comments)
            db.add_ws(ws=worksheet, data=data)
    else:
        # get only user specified worksheets
        # run through inputs and see if they are within the db read in
        for worksheet in ws:
            if worksheet not in wb_rels['ws'].keys():
                raise UserWarning('pylightxl - Sheetname ({}) is not in the workbook.'.format(worksheet))
        for order in sorted(ordered_ws.keys()):
            worksheet = ordered_ws[order]
            if worksheet in ws:
                fn_ws = wb_rels['ws'][worksheet]['fn_ws']
                comments = readxl_get_ws_rels(fn, fn_ws)
                data = readxl_scrape(fn, fn_ws, sharedString, styles, comments)
                db.add_ws(ws=worksheet, data=data)

    if 'pylightxlIOtemp_wb' in fn:
        os.remove(fn)

    return db


def readxl_check_excelfile(fn):
    # type: (Union[str, pathlib.Path]) -> str
    """Takes a file-path and raises error if the file is not found/unsupported.

    :param fn: Excel file path, also supports Pathlib.Path object, as well as file-like object from with/open
    :type fn: Union[str, pathlib.Path]
    :return: filename
    :rtype: str
    """

    # test for pathlib
    if 'pathlib' in str(type(fn)):
        fn = str(fn)
    # test for django already downloaded file
    elif 'path' in dir(fn):
        fn = fn.path
    # test for django stream only file or non-django open file object
    elif 'read' in dir(fn):
        with open('pylightxlIOtemp_wb.xlsx', 'wb') as f:
            f.write(fn.read())
        fn = 'pylightxlIOtemp_wb.xlsx'

    if type(fn) is not str:
        raise UserWarning('pylightxl - Incorrect file entry ({}).'.format(fn))

    if not os.path.isfile(fn):
        raise UserWarning('pylightxl - File ({}) does not exist.'.format(fn))

    extension = fn.split('.')[-1]

    if extension.lower() not in ['xlsx', 'xlsm']:
        raise UserWarning('pylightxl - Incorrect Excel file extension ({}). '
                         'File extension supported: .xlsx .xlsm'.format(extension))

    return fn


def readxl_get_workbook(fn):
    # type: (str) -> Dict[str, dict]
    """Takes a file-path for xl/workbook.xml and returns a list of sheetnames

    :param fn: Excel file path
    :type fn: str
    :return: {'ws': {ws1: {'ws': str, 'rId': str, 'order': str, 'fn_ws': str}}, ...
                'nr': {nr1: {'nr': str, 'ws': str, 'address': str}}, ...}
    :rtype: Dict[str, dict]
    """

    # {'ws': ws1: {'ws': str, 'rId': str, 'order': str, 'fn_ws': str}, ...
    #  'nr': {nr1: {'nr': str, 'ws': str, 'address': str}, ...}
    rv = {'ws': {}, 'nr': {}}

    # zip up the excel file to expose the xml files
    with zipfile.ZipFile(fn, 'r') as f_zip:

        with f_zip.open('xl/workbook.xml', 'r') as file:
            ns = utility_xml_namespace(file)
            for prefix, uri in ns.items():
                ET.register_namespace(prefix, uri)

        with f_zip.open('xl/workbook.xml', 'r') as file:
            tree = ET.parse(file)
            root = tree.getroot()

    wbrels = readxl_get_workbookxmlrels(fn)
    for tag_sheet in root.findall('./default:sheets/default:sheet', ns):
        name = tag_sheet.get('name')
        try:
            rId = tag_sheet.get('{' + ns['r'] + '}id')
        except KeyError:
            # the output of openpyxl can sometimes not write the schema for "r" relationship
            rId = tag_sheet.get('id')
        sheetId = int(re.sub('[^0-9]', '', rId))
        rv['ws'][name] = {'ws': name, 'rId': rId, 'order': sheetId, 'fn_ws': wbrels[rId]}

    for tag_sheet in root.findall('./default:definedNames/default:definedName', ns):
        name = tag_sheet.get('name')
        # for user friendly entry, the "$" for locked cell-locations are removed
        fulladdress = tag_sheet.text.replace('$', '')
        try:
            ws, address = fulladdress.split('!')
            # sometimes definedNames will have extra quate around it, remove these to prevent duplicate ws names
            ws = ws.replace("'","")
        except ValueError:
            msg = ('pylightxl - Ill formatted workbook.xml. '
                   'Skipping NamedRange not containing sheet reference (ex: "Sheet1!A1"): '
                   '{name} - {fulladdress}'.format(name=name, fulladdress=fulladdress))
            warnings.warn(msg, UserWarning)
            continue

        rv['nr'][name] = {'nr': name, 'ws': ws, 'address': address}

    return rv


def readxl_get_workbookxmlrels(fn):
    # type: (str) -> Dict[str, dict]
    """Takes a file-path for xl/_rels/workbook.xml.rels file and gets the sheet#.xml to rId relations

    :param fn: Excel file name
    :type fn: str
    :return: {rId: fn_ws,...}
    :rtype: Dict[str, dict]
    """

    # {rId: fn_ws,...}
    rv = {}

    # zip up the excel file to expose the xml files
    with zipfile.ZipFile(fn, 'r') as f_zip:

        with f_zip.open('xl/_rels/workbook.xml.rels', 'r') as file:
            ns = utility_xml_namespace(file)
            for prefix, uri in ns.items():
                ET.register_namespace(prefix, uri)

        with f_zip.open('xl/_rels/workbook.xml.rels', 'r') as file:
            tree = ET.parse(file)
            root = tree.getroot()

    for relationship in root.findall('./default:Relationship', ns):
        fn_ws = relationship.get('Target')
        # openpyxl write its xl/_rels/workbook.xml.rels file differently than excel itself. It adds on /xl/ at the start of the file path
        if fn_ws[:4] == '/xl/':
            fn_ws = fn_ws[4:]
        rId = relationship.get('Id')
        rv[rId] = fn_ws

    return rv


def readxl_get_sharedStrings(fn):
    # type: (str) -> Dict[str, dict]
    """Takes a file-path for xl/sharedStrings.xml and returns a dictionary of commonly used strings

    :param fn: Excel file name
    :type fn: str
    :return: dict of commonly used strings
    :rtype: Dict[str, dict]
    """

    sharedStrings = {}

    # zip up the excel file to expose the xml files
    with zipfile.ZipFile(fn, 'r') as f_zip:

        if 'xl/sharedStrings.xml' not in f_zip.NameToInfo.keys():
            return sharedStrings

        with f_zip.open('xl/sharedStrings.xml', 'r') as file:
            ns = utility_xml_namespace(file)
            for prefix, uri in ns.items():
                ET.register_namespace(prefix, uri)

        with f_zip.open('xl/sharedStrings.xml', 'r') as file:
            tree = ET.parse(file)
            root = tree.getroot()

    for i, tag_si in enumerate(root.findall('./default:si', ns)):
        tag_t = tag_si.findall('./default:r//default:t', ns)
        if tag_t:
            text = ''.join([tag.text for tag in tag_t if tag.text])
        else:
            text = tag_si.findall('./default:t', ns)[0].text
        sharedStrings.update({i: text})

    return sharedStrings


def readxl_get_styles(fn):
    # type: (str) -> Dict[int, str]
    """Takes a file-path for xl/styles.xml and returns a dictionary of cell formatting keys (example for dates)

    :param fn: Excel file name
    :type fn: str
    :return: dict of cell formatting keys
    :rtype: Dict[int, str]
    """

    styles = {0: '0'}

    # zip up the excel file to expose the xml files
    with zipfile.ZipFile(fn, 'r') as f_zip:

        if 'xl/styles.xml' not in f_zip.NameToInfo.keys():
            return styles

        with f_zip.open('xl/styles.xml', 'r') as file:
            ns = utility_xml_namespace(file)
            for prefix, uri in ns.items():
                ET.register_namespace(prefix, uri)

        with f_zip.open('xl/styles.xml', 'r') as file:
            tree = ET.parse(file)
            root = tree.getroot()

    custom_styles = {}
    try:
        tag_numFmts = root.findall('./default:numFmts', ns)[0]
    except IndexError:
        tag_numFmts = []
    num_chars = '0#?'
    date_chars = 'yd'  # 'm' is ambiguos
    time_chars = 'hs'
    for tag in tag_numFmts:
        fc = tag.get('formatCode').lower().split(';')[0]
        if fc == 'general':
            continue
        nnu = sum(fc.count(c) for c in num_chars)
        ndc = sum(fc.count(c) for c in date_chars)
        ntc = sum(fc.count(c) for c in time_chars)
        if nnu > ndc + ntc:
            pass
        elif ndc and ntc:
            custom_styles[tag.get('numFmtId')] = '22'
        elif ndc:
            custom_styles[tag.get('numFmtId')] = '14'
        elif ntc:
            custom_styles[tag.get('numFmtId')] = '18'


    for i, tag_cellXfs in enumerate(root.findall('./default:cellXfs', ns)[0]):
        numFmtId = tag_cellXfs.get('numFmtId')
        styles.update({i: custom_styles[numFmtId] if numFmtId in custom_styles else numFmtId})

    return styles


def readxl_get_ws_rels(fn, fn_ws):
    # type: (str, str) -> Dict[str, str]
    """Takes a file-path for xl/worksheets/sheet#.xml and returns a dict of cell data (comments)

    :param fn: Excel file name
    :type fn: str
    :param fn_ws: file path for worksheet (ex: xl/worksheets/sheet1.xml)
    :type fn_ws: str
    :return: dict of cell data (comments)
    :rtype: Dict[str, str]
    """

    rv = {}

    fn_ws_parts = fn_ws.split('/')
    fn_wsrels = '/'.join(fn_ws_parts[:-1]) + '/_rels/' + fn_ws_parts[-1] + '.rels'

    # zip up the excel file to expose the xml files
    with zipfile.ZipFile(fn, 'r') as f_zip:

        if 'xl/' + fn_wsrels not in f_zip.NameToInfo.keys():
            return rv

        with f_zip.open('xl/' + fn_wsrels, 'r') as file:
            ns = utility_xml_namespace(file)
            for prefix, uri in ns.items():
                ET.register_namespace(prefix, uri)

        with f_zip.open('xl/' + fn_wsrels, 'r') as file:
            tree = ET.parse(file)
            root = tree.getroot()

    comment_fn = ''
    for tag_rel in root.findall('./default:Relationship', ns):
        target = tag_rel.get('Target')
        if 'comments' in target:
            comment_fn = target.split('/')[-1]

    if comment_fn:
        # zip up the excel file to expose the xml files
        with zipfile.ZipFile(fn, 'r') as f_zip:

            with f_zip.open('xl/' + comment_fn, 'r') as file:
                ns = utility_xml_namespace(file)
                for prefix, uri in ns.items():
                    ET.register_namespace(prefix, uri)

            with f_zip.open('xl/' + comment_fn, 'r') as file:
                tree = ET.parse(file)
                root = tree.getroot()

        for tag_comment in root.findall('./default:commentList/default:comment', ns):
            celladdress = tag_comment.get('ref')
            comment = ''
            for tag_t in tag_comment.findall('.//default:t', ns):
                text = tag_t.text
                if '[Threaded comment]' in text:
                    text = text.split('Comment:\n')[1]
                comment += text
            rv[celladdress] = comment

    return rv


def readxl_scrape(fn, fn_ws, sharedString, styles, comments):
    # type: (str, str, dict, dict, dict) -> Dict[str, dict]
    """Takes a file-path for xl/worksheets/sheet#.xml and returns a dict of cell data

    :param fn: Excel file name
    :type fn: str
    :param fn_ws: file path for worksheet (ex: xl/worksheets/sheet1.xml)
    :type fn_ws: str
    :param sharedString: shared string dict lookup table from xl/sharedStrings.xml for string only cell values
    :type sharedString: dict
    :param styles: styles dict for date parsing
    :type styles: dict
    :param comments: comments dict
    :type comments: dict
    :return: dict of cell data {address: {'v': cell_val, 'f': cell_formula, 's': '', 'c': cell_comment}}
    :rtype: Dict[str, dict]
    """

    # {address: {'v': cell_val, 'f': cell_formula, 's': '', 'c': cell_comment}}
    data = {}

    # zip up the excel file to expose the xml files
    with zipfile.ZipFile(fn, 'r') as f_zip:

        with f_zip.open('xl/' + fn_ws, 'r') as file:
            ns = utility_xml_namespace(file)
            for prefix, uri in ns.items():
                ET.register_namespace(prefix, uri)

        with f_zip.open('xl/' + fn_ws, 'r') as file:
            tree = ET.parse(file)
            root = tree.getroot()

    for tag_cell in root.findall('./default:sheetData/default:row/default:c', ns):
        cell_address = tag_cell.get('r')
        # t="e" is for error cells "#N/A"
        # t="s" is for common strings
        # t="str" is for equation strings (ex: =A1 & "this")
        # t="b" is for bool, bool is not logged as a commonString in xml, 0 == FALSE, 1 == TRUE
        cell_type = tag_cell.get('t')
        cell_style = int(tag_cell.get('s')) if tag_cell.get('s') is not None else 0
        tag_val = tag_cell.find('./default:v', ns)
        cell_val = tag_val.text or '' if tag_val is not None else ''
        tag_formula = tag_cell.find('./default:f', ns)
        cell_formula = tag_formula.text or '' if tag_formula is not None else ''
        comment = comments[cell_address] if cell_address in comments.keys() else ''

        if all([entry == '' or entry is None for entry in [cell_val, cell_formula, comment]]):
            # this is a style only entry, currently we dont parse style therefore this data would unnecessarily stored
            continue

        if cell_type == 's':
            # commonString
            cell_val = sharedString[int(cell_val)]
        elif cell_type == 'b':
            # bool
            cell_val = True if cell_val == '1' else False
        elif cell_val == '' or cell_type == 'str' or cell_type == 'e':
            # cell is either empty, or is a str formula - leave cell_val as a string
            pass
        else:
            # int or float
            test_cell = cell_val if '-' not in cell_val else cell_val[1:]
            if test_cell.isdigit():
                cell_val = int(cell_val)
            else:
                cell_val = float(cell_val)
            st = styles[cell_style]
            if st in ['14', '15', '16', '17']:
                dt = EXCEL_STARTDATE + timedelta(cell_val)
                cell_val = dt.isoformat()[:10].replace('-', '/')
            elif st in ['18', '19', '20', '21']:
                partialday = cell_val % 1
                dt = EXCEL_STARTDATE + timedelta(2, round(partialday * 86400))
                cell_val = dt.strftime('%H:%M:%S')
            elif st in ['22']:
                partialday = cell_val % 1
                dt = EXCEL_STARTDATE + timedelta(int(cell_val), round(partialday * 86400))
                cell_val = dt.isoformat().replace('T', ' ').replace('-', '/')

        data.update({cell_address: {'v': cell_val, 'f': cell_formula, 's': '', 'c': comment}})

    return data


def readcsv(fn, delimiter=',', ws='Sheet1'):
    # type: (Union[str, pathlib.Path, io.StringIO], str, str) -> Database
    """Reads an xlsx or xlsm file and returns a pylightxl database

    :param fn: filename, pathlib, or stringIO object
    :type fn: str
    :param delimiter: csv file delimiter, defaults to ','
    :type delimiter: str, optional
    :param ws: worksheet name that the csv data will be stored in, defaults to 'Sheet1'
    :type ws: str, optional
    :return: pylightxl database
    :rtype: Database
    """

    # declare a db
    db = Database()

    # test that file entered was a valid excel file
    if 'pathlib' in str(type(fn)):
        fn = str(fn)

    # data = {'A1': data1, 'A2': data2...}
    data = {}

    try:
        f = fn if 'readline' in dir(fn) else open(fn, 'r')
    except Exception:
        f.close()
        raise Exception
    else:
        i_row = 0
        while True:
            i_row += 1

            line = f.readline()

            if not line:
                break

            line = line.replace('\n', '').replace('\r', '')

            items = line.split(delimiter)

            for i_col, item in enumerate(items, 1):
                address = utility_num2columnletters(i_col) + str(i_row)

                # data conditioning
                try:
                    if '.' in item:
                        item = float(item)
                    else:
                        item = int(item)
                except ValueError:
                    if 'true' in item.strip().lower():
                        item = True
                    elif 'false' in item.strip().lower():
                        item = False

                data[address] = {'v': item, 'f': None, 's': None}

    db.add_ws(ws, data)

    return db

########################################################################################################
# SEC-04: WRITEXL FUNCTIONS
########################################################################################################


def writexl(db, fn):
    # type: (Database, Union[str, pathlib.Path]) -> None
    """Writes an excel file from pylightxl.Database

    :param db: database contains sheetnames, and their data
    :type db: Database
    :param fn: file output path
    :type fn: Union[str, pathlib.path]
    """

    # test that file entered was a valid excel file
    if 'pathlib' in str(type(fn)):
        fn = str(fn)


    if not os.path.isfile(fn):
        # write to new excel
        writexl_new_writer(db, fn)
    else:
        # write to existing excel
        writexl_alt_writer(db, fn)

    # cleanup existing pylightxl temp files if an error occurred
    temp_folders = [folder for folder in os.listdir('.') if '_pylightxl_' in folder]
    for folder in temp_folders:
        try:
            shutil.rmtree(folder)
        except PermissionError:
            # windows sometimes messes up cleaning this up in python3
            time.sleep(1)
            os.system(r'rmdir /s /q {}'.format(folder))


def writexl_alt_writer(db, path):
    # type: (Database, str) -> None
    """Writes to an existing excel file. Only injects cell overwrites or new/removed sheets

    :param db: database contains sheetnames, and their data
    :type db: Database
    :param path: file output path
    :type path: str
    """

    filename = os.path.split(path)[-1]
    filename = filename if filename.split('.')[-1] == 'xlsx' else '.'.join(filename.split('.')[:-1] + ['xlsx'])
    temp_folder = '_pylightxl_' + filename

    # have to extract all first to modify
    with zipfile.ZipFile(path, 'r') as f:
        f.extractall(temp_folder)

    text = writexl_alt_app_text(db, temp_folder + '/docProps/app.xml')
    with open(temp_folder + '/docProps/app.xml', 'wb') as f:
        f.write(text.encode('utf-8'))


    # rename sheet#.xml to temp to prevent overwriting
    for file in os.listdir(temp_folder + '/xl/worksheets'):
        if '.xml' in file:
            old_name = temp_folder + '/xl/worksheets/' + file
            new_name = temp_folder + '/xl/worksheets/' + 'temp_' + file
            try:
                os.rename(old_name, new_name)
            except FileExistsError:
                os.remove('./' + new_name)
                os.rename(old_name, new_name)
    # get filename to xml rId associations
    sheetref = writexl_alt_getsheetref(path_wbrels=temp_folder + '/xl/_rels/workbook.xml.rels',
                                       path_wb=temp_folder + '/xl/workbook.xml')
    existing_sheetnames = [d['name'] for d in sheetref.values()]

    text = writexl_new_workbook_text(db)
    with open(temp_folder + '/xl/workbook.xml', 'wb') as f:
        f.write(text.encode('utf-8'))

    for shID, sheet_name in enumerate(db.ws_names, 1):
        if sheet_name in existing_sheetnames:
            # get the original sheet
            for subdict in sheetref.values():
                if subdict['name'] == sheet_name:
                    fn = 'temp_' + subdict['filename']

            # rewrite the sheet as if it was new
            text = writexl_new_worksheet_text(db, sheet_name)
            # feed altered text to new sheet based on db indexing order
            with open(temp_folder + '/xl/worksheets/sheet{}.xml'.format(shID), 'wb') as f:
                f.write(text.encode('utf-8'))
            # remove temp xml sheet file
            os.remove(temp_folder + '/xl/worksheets/{}'.format(fn))
        else:
            # this sheet is new, create a new sheet
            text = writexl_new_worksheet_text(db, sheet_name)
            with open(temp_folder + '/xl/worksheets/sheet{shID}.xml'.format(shID=shID), 'wb') as f:
                f.write(text.encode('utf-8'))

    # this has to come after sheets for db._sharedStrings to be populated
    text = writexl_new_workbookrels_text(db)
    with open(temp_folder + '/xl/_rels/workbook.xml.rels', 'wb') as f:
        f.write(text.encode('utf-8'))

    if os.path.isfile(temp_folder + '/xl/sharedStrings.xml'):
        # sharedStrings is always recreated from db._sharedStrings since all sheets are rewritten
        os.remove(temp_folder + '/xl/sharedStrings.xml')
    text = writexl_new_sharedStrings_text(db)
    with open(temp_folder + '/xl/sharedStrings.xml', 'wb') as f:
        f.write(text.encode('utf-8'))

    text = writexl_new_content_types_text(db)
    with open(temp_folder + '/[Content_Types].xml', 'wb') as f:
        f.write(text.encode('utf-8'))

    # cleanup files that would cause a "repair" workbook
    try:
        shutil.rmtree(temp_folder + '/xl/ctrlProps')
    except (FileNotFoundError, WindowsError):
        pass
    try:
        shutil.rmtree(temp_folder + '/xl/drawings')
    except (FileNotFoundError, WindowsError):
        pass
    try:
        shutil.rmtree(temp_folder + '/xl/printerSettings')
    except (FileNotFoundError, WindowsError):
        pass
    try:
        os.remove(temp_folder + '/xl/vbaProject.bin')
    except (FileNotFoundError, WindowsError):
        pass
    try:
        os.remove(temp_folder + '/docProps/custom.xml')
    except (FileNotFoundError, WindowsError):
        pass

    # remove existing file
    try:
        os.remove(path)
    except PermissionError:
        # file is open, adjust name and print warning
        print('pylightxl - Cannot write to existing file <{}> that is open in excel.'.format(filename))
        print('     New temporary file was written to <{}>'.format('new_' + filename))
        filename = 'new_' + filename

    # log old wd before changing it to temp folder for zipping
    exe_dir = os.getcwd()
    old_dir = os.path.split(os.path.abspath(path))[0]
    # wd must be changed to be within the temp folder to get zipfile to prevent the top level temp folder
    #  from being zipped as well
    os.chdir(temp_folder)
    with zipfile.ZipFile(filename, 'w') as f:
        for root, dirs, files in os.walk('.'):
            for file in files:
                # top level "with" statement already creates a excel file that is seen by os.walk
                #  this check skips that empty zip file from being zipped as well
                if file != filename:
                    f.write(os.path.join(root, file))
    # move the zipped up file out of the temp folder
    try:
        shutil.move(filename, old_dir)
    except Exception:
        os.remove(os.path.join(old_dir, filename))
        shutil.move(filename, old_dir)
    os.chdir(exe_dir)


def writexl_alt_app_text(db, filepath):
    # type: (Database, str) -> str
    """Takes a docProps/app.xml filepath and returns the updated xml text version of it.
    Updates:
        - HeadingPairs/vt:variant/vt:i4 "text" after Worksheets
        - TitlesOfParts/vt:vector named filed "size"
        - TitlesOfParts/vt:vector/vt:lpstr

    :param db: pylightxl database that contains data to update xml file
    :type db: Database
    :param filepath: file path for docProps/app.xml
    :type filepath: str
    :return: returns the updated xml text
    :rtype: str
    """

    # extract text from existing app.xml
    if PYVER == 3:
        with open(filepath, 'r', encoding='utf-8') as f:
            ns = utility_xml_namespace(f)
    else:
        # python2 does not support encoding, io.open does however it is extremely slow. if this creates a reading issue address it then.
        with open(filepath, 'r') as f:
            ns = utility_xml_namespace(f)
    for prefix, uri in ns.items():
        ET.register_namespace(prefix, uri)
    tree = ET.parse(filepath)
    root = tree.getroot()

    if db.nr_names == {}:
        # does not contain namedranges
        try:
            tag_vt_vector = root.find('./default:HeadingPairs//vt:vector', ns)
        except SyntaxError:
            # this occurs when excel file was created by another program like openpyxl
            # where not all information was written to docProps/app.xml
            return writexl_new_app_text(db)
        tag_vt_vector.clear()
        tag_vt_vector.set('size', '2')
        tag_vt_vector.set('baseType', 'variant')

        tag_vt_variant = ET.Element('vt:variant')
        tag_vt_vector.append(tag_vt_variant)
        tag_vt_lpstr = ET.Element('vt:lpstr')
        tag_vt_lpstr.text = 'Worksheets'
        tag_vt_variant.append(tag_vt_lpstr)

        tag_vt_variant = ET.Element('vt:variant')
        tag_vt_vector.append(tag_vt_variant)
        tag_vt_lpstr = ET.Element('vt:i4')
        tag_vt_lpstr.text = str(len(db.ws_names))
        tag_vt_variant.append(tag_vt_lpstr)

    else:
        # contains namedranges
        try:
            tag_vt_vector = root.find('./default:HeadingPairs//vt:vector', ns)
        except SyntaxError:
            # this occurs when excel file was created by another program like openpyxl
            # where not all information was written to docProps/app.xml
            return writexl_new_app_text(db)
        tag_vt_vector.clear()
        tag_vt_vector.set('size', '4')
        tag_vt_vector.set('baseType', 'variant')

        tag_vt_variant = ET.Element('vt:variant')
        tag_vt_vector.append(tag_vt_variant)
        tag_vt_lpstr = ET.Element('vt:lpstr')
        tag_vt_lpstr.text = 'Worksheets'
        tag_vt_variant.append(tag_vt_lpstr)

        tag_vt_variant = ET.Element('vt:variant')
        tag_vt_vector.append(tag_vt_variant)
        tag_vt_lpstr = ET.Element('vt:i4')
        tag_vt_lpstr.text = str(len(db.ws_names))
        tag_vt_variant.append(tag_vt_lpstr)

        tag_vt_variant = ET.Element('vt:variant')
        tag_vt_vector.append(tag_vt_variant)
        tag_vt_lpstr = ET.Element('vt:lpstr')
        tag_vt_lpstr.text = 'Named Ranges'
        tag_vt_variant.append(tag_vt_lpstr)

        tag_vt_variant = ET.Element('vt:variant')
        tag_vt_vector.append(tag_vt_variant)
        tag_vt_lpstr = ET.Element('vt:i4')
        tag_vt_lpstr.text = str(len(db.nr_names))
        tag_vt_variant.append(tag_vt_lpstr)

    # update: number of worksheets and named ranges for the workbook under "TitlesOfParts"
    # update: remove existing worksheet names, preserve named ranges, add new worksheet names
    tag_vt_vector = root.find('./default:TitlesOfParts//vt:vector', ns)
    tag_vt_vector.clear()
    tag_vt_vector.set('size', str(len(db.ws_names) + len(db.nr_names)))
    tag_vt_vector.set('baseType', 'lpstr')

    for sheet_name in db.ws_names:
        element = ET.Element('vt:lpstr')
        element.text = sheet_name

        tag_vt_vector.append(element)
    if db.nr_names != {}:
        for range_name in db.nr_names.keys():
            element = ET.Element('vt:lpstr')
            element.text = range_name

            tag_vt_vector.append(element)

    # reset default namespace
    ET.register_namespace('', ns['default'])

    # roll up entire xml file as text
    text = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n' + ET.tostring(root).decode()

    return text


def writexl_alt_getsheetref(path_wbrels, path_wb):
    # type: (str, str) -> Dict[int, dict]
    """Takes a file path for '/xl/_rels/workbook.xml.rels' and '/xl/workbook.xml' files
    and returns a dictionary relationship between sheetID, name and xml worksheet file path
        rId: xml's indexing between files (workbook.xml.rels defined rID to xml worksheet file path)
        sheetId: order of worksheet in the workbook as it appears in excel
        name: worksheet name as it appears in excel (user defined name)
        filename: xml worksheet file path

    :param path_wbrels: file path to '/xl/_rels/workbook.xml.rels'
    :type path_wbrels: str
    :param path_wb: file path to '/xl/workbook.xml'
    :type path_wb: str
    :return: dictionary of filenames {'rId#': {'name': str, 'filename': str, 'sheetId': int}}
    :rtype: Dict[int, dict]
    """

    sheetref = {}

    # -------------------------------------------------------------
    # get worksheet filenames and Ids
    with open(path_wbrels, 'r') as f:
        ns = utility_xml_namespace(f)
    for prefix, uri in ns.items():
        ET.register_namespace(prefix, uri)
    tree = ET.parse(path_wbrels)
    root = tree.getroot()

    for element in root.findall('./default:Relationship', ns):
        if 'worksheets/sheet' in element.get('Target'):
            rId = element.get('Id')
            filename = element.get('Target').split('/')[1].replace('"', '')
            sheetref.update({rId: {'sheetId': None, 'name': None, 'filename': filename}})

    # -------------------------------------------------------------
    # get custom worksheet names
    with open(path_wb, 'r') as f:
        ns = utility_xml_namespace(f)
    for prefix, uri in ns.items():
        ET.register_namespace(prefix, uri)
    tree = ET.parse(path_wb)
    root = tree.getroot()

    for element in root.findall('./default:sheets/default:sheet', ns):
        rId = element.get('{' + ns['r'] + '}id')
        sheetref[rId]['name'] = element.get('name')
        sheetref[rId]['sheetId'] = int(element.get('sheetId'))

    return sheetref


def writexl_new_writer(db, path):
    # type: (Database, str) -> None
    """Writes to a new excel file. The minimum xml parts are zipped together and converted to an .xlsx

    :param db: database contains sheetnames, and their data
    :type db: Database
    :param path: file output path
    :type path: str
    """

    filename = os.path.split(path)[-1]
    filename = filename if filename.split('.')[-1] == 'xlsx' else '.'.join(filename.split('.')[:-1] + ['xlsx'])
    path = '/'.join(os.path.split(path)[:-1])
    path = path + '/' + filename if path else filename

    with zipfile.ZipFile(path, 'w') as zf:
        text_rels = writexl_new_rels_text(db)
        zf.writestr('_rels/.rels', text_rels)

        text_app = writexl_new_app_text(db)
        zf.writestr('docProps/app.xml', text_app)

        text_core = writexl_new_core_text(db)
        zf.writestr('docProps/core.xml', text_core)

        text_workbook = writexl_new_workbook_text(db)
        zf.writestr('xl/workbook.xml', text_workbook)

        for shID, sheet_name in enumerate(db.ws_names, 1):
            text_worksheet = writexl_new_worksheet_text(db, sheet_name)
            zf.writestr('xl/worksheets/sheet{shID}.xml'.format(shID=shID), text_worksheet)

        if db._sharedStrings:
            text_sharedStrings = writexl_new_sharedStrings_text(db)
            zf.writestr('xl/sharedStrings.xml', text_sharedStrings)

        # this has to come after new_worksheet_text for db._sharedStrings to be populated
        text_workbookrels = writexl_new_workbookrels_text(db)
        zf.writestr('xl/_rels/workbook.xml.rels', text_workbookrels)

        # this has to come after new_worksheet_text for db._sharedStrings to be populated
        text_content_types = writexl_new_content_types_text(db)
        zf.writestr('[Content_Types].xml', text_content_types)


def writexl_new_rels_text(db):
    # type: (Database) -> str
    """Returns text for /_rels/.rels file

    :param db: pylightxl Database
    :type db: Database
    :return: text for /_rels/.rels file
    :rtype: str
    """

    # location: /_rels/.rels
    # inserts: -
    xml_base =  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n' \
                '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">\r\n' \
                    '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>\r\n' \
                    '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>\r\n' \
                    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>\r\n' \
                '</Relationships>'

    return xml_base


def writexl_new_app_text(db):
    # type: (Database) -> str
    """Returns /docProps/app.xml text

    :param db: database contains sheetnames, and their data
    :type db: Database
    :return: /docProps/app.xml text
    :rtype: str
    """

    # location: /docProps/app.xml
    # inserts: num_sheets, many_tag_vt
    #  note: sheet name order does not matter
    xml_base =  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n' \
                '<Properties xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes" xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties">\r\n' \
                '<Application>Microsoft Excel</Application>\r\n' \
                '<DocSecurity>0</DocSecurity>\r\n' \
                '<ScaleCrop>false</ScaleCrop>\r\n' \
                '<HeadingPairs>\r\n' \
                    '<vt:vector baseType="variant" size="{vector_size}">\r\n' \
                        '<vt:variant>\r\n' \
                            '<vt:lpstr>Worksheets</vt:lpstr>\r\n' \
                        '</vt:variant>\r\n' \
                        '<vt:variant>\r\n' \
                            '<vt:i4>{ws_size}</vt:i4>\r\n' \
                        '</vt:variant>\r\n' \
                        '{variant_tag_nr}' \
                    '</vt:vector>\r\n' \
               '</HeadingPairs>\r\n' \
               '<TitlesOfParts>\r\n' \
                   '<vt:vector baseType="lpstr" size="{vt_size}">\r\n' \
                       '{many_tag_vt}\r\n' \
                   '</vt:vector>\r\n' \
               '</TitlesOfParts>\r\n' \
               '<Company></Company>\r\n' \
               '<LinksUpToDate>false</LinksUpToDate>\r\n' \
               '<SharedDoc>false</SharedDoc>\r\n' \
               '<HyperlinksChanged>false</HyperlinksChanged>\r\n' \
               '<AppVersion>16.0300</AppVersion>\r\n' \
               '</Properties>'

    if db.nr_names != {}:
        variant_tag_nr = '<vt:variant><vt:lpstr>Named Ranges</vt:lpstr></vt:variant>\r\n' \
                         '<vt:variant><vt:i4>{nr_size}</vt:i4></vt:variant>\r\n'.format(nr_size=len(db.nr_names))
        vector_size = 4
    else:
        variant_tag_nr = ''
        vector_size = 2

    # location: single tag_sheet insert for xml_base
    # inserts: sheet_name
    tag_vt = '<vt:lpstr>{name}</vt:lpstr>\r\n'

    vt_size = len(db.ws_names) + len(db.nr_names)
    ws_size = len(db.ws_names)
    many_tag_vt = ''
    for sheet_name in db.ws_names:
        many_tag_vt += tag_vt.format(name=sheet_name)
    for range_name in db.nr_names.keys():
        many_tag_vt += tag_vt.format(name=range_name)

    rv = xml_base.format(vector_size=vector_size,
                         ws_size=ws_size,
                         variant_tag_nr=variant_tag_nr,
                         vt_size=vt_size,
                         many_tag_vt=many_tag_vt)

    return rv


def writexl_new_core_text(db):
    # type: (Database) -> str
    """Returns /docProps/core.xml text

    :param db: database contains sheetnames, and their data
    :type db: Database
    :return: /docProps/core.xml text
    :rtype: str
    """

    # location: /docProps/core.xml
    # inserts: -
    xml_base =  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n' \
                '<cp:coreProperties xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties">\r\n' \
                '<dc:creator>pylightxl</dc:creator>\r\n' \
                '<cp:lastModifiedBy>pylightxl</cp:lastModifiedBy>\r\n' \
                '<dcterms:created xsi:type="dcterms:W3CDTF">2019-12-27T01:35:28Z</dcterms:created>\r\n' \
                '<dcterms:modified xsi:type="dcterms:W3CDTF">2019-12-27T01:35:39Z</dcterms:modified>\r\n' \
                '</cp:coreProperties>'

    return xml_base


def writexl_new_workbookrels_text(db):
    # type: (Database) -> str
    """Returns /xl/_rels/workbook.xml.rels text

    :param db: database contains sheetnames, and their data
    :type db: Database
    :return: /xl/_rels/workbook.xml.rels text
    :rtype: str
    """

    # location: /xl/_rels/workbook.xml.rels
    # inserts: many_tag_sheets, tag_sharedStrings, tag_calcChain
    #   sheets first for rId# then theme > styles > sharedStrings
    #   note that theme, style, calcChain is not part of the stack. These don't need to be part of the base xml
    xml_base =  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n' \
                '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">\r\n' \
                    '{many_tag_sheets}\r\n' \
                    '{tag_sharedStrings}\r\n' \
                '</Relationships>'

    # location: single tag_sheet insert for xml_base
    # inserts: sheet_num
    #  note: rId is not the order of sheets, it just needs to match workbook.xml
    xml_tag_sheet = '<Relationship Target="worksheets/sheet{sheet_num}.xml" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Id="rId{sheet_num}"/>\r\n'

    # location: sharedStrings insert for xml_base
    # inserts: ID
    xml_tag_sharedStrings = '<Relationship Target="sharedStrings.xml" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Id="rId{ID}"/>\r\n'

    many_tag_sheets = ''
    for wsID, _ in enumerate(db.ws_names, 1):
        many_tag_sheets += xml_tag_sheet.format(sheet_num=wsID)
    if db._sharedStrings:
        # +1 to increment +1 from the last sheet ID
        tag_sharedStrings = xml_tag_sharedStrings.format(ID=len(db.ws_names)+1)
    else:
        tag_sharedStrings = ''

    rv = xml_base.format(many_tag_sheets=many_tag_sheets,
                         tag_sharedStrings=tag_sharedStrings)
    return rv


def writexl_new_workbook_text(db):
    # type: (Database) -> str
    """Returns xl/workbook.xml text

    :param db: database contains sheetnames, and their data
    :type db: Database
    :return: xl/workbook.xml text
    :rtype: str
    """

    # location: xl/workbook.xml
    # inserts: many_tag_sheets
    xml_base =  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n' \
                '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="x15 xr xr6 xr10 xr2" xmlns:x15="http://schemas.microsoft.com/office/spreadsheetml/2010/11/main" xmlns:xr="http://schemas.microsoft.com/office/spreadsheetml/2014/revision" xmlns:xr6="http://schemas.microsoft.com/office/spreadsheetml/2016/revision6" xmlns:xr10="http://schemas.microsoft.com/office/spreadsheetml/2016/revision10" xmlns:xr2="http://schemas.microsoft.com/office/spreadsheetml/2015/revision2">\r\n' \
                '<fileVersion appName="xl" lastEdited="7" lowestEdited="7" rupBuild="22228"/>\r\n' \
                '<workbookPr defaultThemeVersion="166925"/>\r\n' \
                    '<sheets>\r\n' \
                        '{many_tag_sheets}\r\n' \
                    '</sheets>\r\n' \
                '{xml_namedrange}' \
                    '<calcPr calcId="181029"/>\r\n' \
                '</workbook>'

    # location: worksheet tag for xml_base
    # inserts: name, sheet_id, order_id
    #   note id=rId# is referenced by .rels that points to the file locations of each sheet,
    #        it is also the sheet order number, name= is the custom name
    xml_tag_sheet = '<sheet name="{sheet_name}" sheetId="{order_id}" r:id="rId{ref_id}"/>\r\n'

    many_tag_sheets = ''
    for shID, sheet_name in enumerate(db.ws_names, 1):
        many_tag_sheets += xml_tag_sheet.format(sheet_name=sheet_name, order_id=shID, ref_id=shID)


    many_tag_nr = ''
    for name, address in db.nr_names.items():
        many_tag_nr += '<definedName name="{}">{}</definedName>\r\n'.format(name, address)

    if db.nr_names != {}:
        xml_namedrange = '<definedNames>{many_tag_nr}</definedNames>\r\n'.format(many_tag_nr=many_tag_nr)
    else:
        xml_namedrange = ''

    rv = xml_base.format(many_tag_sheets=many_tag_sheets, xml_namedrange=xml_namedrange)
    return rv


def writexl_new_worksheet_text(db, sheet_name):
    # type: (Database, str) -> str
    """Returns xl/worksheets/sheet#.xml text

    :param db: database contains sheetnames, and their data
    :type db: Database
    :param sheet_name: worksheet name
    :type sheet_name: str
    :return: xl/worksheets/sheet#.xml text
    :rtype: str
    """

    # dev note: the reason why db._sharedStrings is defined in here is to take advantage of single time
    #  looping through all of the cell data

    # row size and dyDescent are optional values

    # location: xl/worksheets/sheet#.xml
    # inserts: sizeAddress (ex: A1:B5, if empty then A1), many_tag_row
    xml_base =  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n' \
                '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="x14ac xr xr2 xr3" xmlns:x14ac="http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac" xmlns:xr="http://schemas.microsoft.com/office/spreadsheetml/2014/revision" xmlns:xr2="http://schemas.microsoft.com/office/spreadsheetml/2015/revision2" xmlns:xr3="http://schemas.microsoft.com/office/spreadsheetml/2016/revision3" xr:uid="{uid}">\r\n' \
                    '<dimension ref="{sizeAddress}"/>\r\n' \
                    '<sheetFormatPr defaultRowHeight="15" x14ac:dyDescent="0.25"/>\r\n' \
                    '<sheetData>\r\n' \
                        '{many_tag_row}\r\n' \
                    '</sheetData>\r\n' \
                    '<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>\r\n' \
                '</worksheet>'

    # location: row tag for xml_base
    # inserts: row_num (ex: 1), num_of_cr_tags (ex: 1:5), many_tag_cr
    xml_tag_row = '<row r="{row_num}" x14ac:dyDescent="0.25" spans="1:{num_of_cr_tags}">{many_tag_cr}</row>\r\n'

    # location: c r tag for xml_tag_row
    # inserts: address, str_option (t="s" for sharedStrings), val
    xml_tag_cr = '<c r="{address}" {str_option}><v>{val}</v></c>'

    ws_size = db.ws(sheet_name).size
    if ws_size == [0,0] or ws_size == [1,1]:
        sheet_size_address = 'A1'
    else:
        sheet_size_address = 'A1:' + utility_index2address(ws_size[0],ws_size[1])

    many_tag_row = ''
    for rowID, row in enumerate(db.ws(sheet_name).rows, 1):
        many_tag_cr = ''
        tag_cr = False
        num_of_cr_tags_counter = 0
        for colID, val in enumerate(row, 1):
            address = utility_index2address(rowID, colID)
            str_option = ''
            cell_formula = ''

            # empty cells are not stored in _data
            try:
                cell_formula = db.ws(sheet_name)._data[address]['f']
            except KeyError:
                pass

            # cell contains a formula
            if cell_formula:
                # cells containing formula must not have a type declaration or a <v> tag
                #   to calculate properly when excel is opened
                tag_formula = '<f>{f}</f>'.format(f=cell_formula)
                tag_formula = tag_formula.replace('&', '&amp;')
                tag_cr = True
                num_of_cr_tags_counter += 1
                many_tag_cr += '<c r="{address}">{tag_formula}</c>'.format(address=address,
                                                                           tag_formula=tag_formula)

            # cell value is string
            elif type(val) is str and val != '':
                str_option = 't="s"'
                try:
                    # replace val with its sharedStrings index,
                    #   note sharedString index does start at 0
                    val = db._sharedStrings.index(val)
                except ValueError:
                    db._sharedStrings.append(val.replace('&', '&amp;'))
                    val = db._sharedStrings.index(val.replace('&', '&amp;'))
                tag_cr = True
                num_of_cr_tags_counter += 1
                many_tag_cr += xml_tag_cr.format(address=address, str_option=str_option, val=val)

            # cell does not contain a formula, it is numeric
            elif val != '':
                # val is numeric
                tag_cr = True
                num_of_cr_tags_counter += 1
                many_tag_cr += xml_tag_cr.format(address=address, str_option=str_option, val=val)

        if tag_cr:
            many_tag_row += xml_tag_row.format(row_num=rowID, num_of_cr_tags=str(num_of_cr_tags_counter),
                                               many_tag_cr=many_tag_cr)

    # not 100% what uid does, but it is required for excel to open
    rv = xml_base.format(sizeAddress=sheet_size_address,
                         uid='2C7EE24B-C535-494D-AA97-0A61EE84BA40',
                         many_tag_row=many_tag_row)
    return rv


def writexl_new_sharedStrings_text(db):
    # type: (Database) -> str
    """Returns xl/sharedStrings.xml text

    :param db: database contains sheetnames, and their data
    :type db: Database
    :return: xl/sharedStrings.xml text
    :rtype: str
    """

    # location: xl/sharedStrings.xml
    # inserts: sharedString_len, many_tag_si
    xml_base =  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n' \
                '<sst uniqueCount="{sharedString_len}" count="{sharedString_len}" xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">\r\n' \
                    '{many_tag_si}\r\n' \
                '</sst>'

    # location: si tag for xml_base
    # inserts: space_preserve (xml:space="preserve"), val
    #   note leading and trailing spaces requires preserve tag: <t xml:space="preserve"> leadingspace</t>
    xml_tag_si = '<si><t {space_preserve}>{val}</t></si>\r\n'

    sharedString_len = len(db._sharedStrings)

    many_tag_si = ''
    for val in db._sharedStrings:
        if val[0] == ' ' or val[-1] == ' ':
            space_preserve = 'xml:space="preserve"'
        else:
            space_preserve = ''
        many_tag_si += xml_tag_si.format(space_preserve=space_preserve, val=html.escape(val))

    rv = xml_base.format(sharedString_len=sharedString_len, many_tag_si=many_tag_si)
    return rv


def writexl_new_content_types_text(db):
    # type: (Database) -> str
    """Returns [Content_Types].xml text

    :param db: database contains sheetnames, and their data
    :type db: Database
    :return: [Content_Types].xml text
    :rtype: str
    """

    # location: [Content_Types].xml
    # inserts: many_tag_sheets, tag_sharedStrings
    #  note calcChain is part of this but it is not necessary for excel to open
    xml_base =  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n' \
                '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">\r\n' \
                    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>\r\n' \
                    '<Default Extension="xml" ContentType="application/xml"/>\r\n' \
                    '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>\r\n' \
                    '{many_tag_sheets}\r\n' \
                    '{tag_sharedStrings}\r\n' \
                    '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>\r\n' \
                    '<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>\r\n' \
                '</Types>'


    xml_tag_sheet = '<Override PartName="/xl/worksheets/sheet{sheet_id}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>\r\n'

    xml_tag_sharedStrings = '<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>\r\n'

    many_tag_sheets = ''
    for sheet_id, _ in enumerate(db.ws_names, 1):
        many_tag_sheets += xml_tag_sheet.format(sheet_id=sheet_id)

    if db._sharedStrings:
        tag_sharedStrings = xml_tag_sharedStrings
    else:
        tag_sharedStrings = ''

    rv = xml_base.format(many_tag_sheets=many_tag_sheets,
                         tag_sharedStrings=tag_sharedStrings)

    return rv


def writecsv(db, fn, ws=(), delimiter=','):
    # type: (Database, Union[str, pathlib.Path, io.StringIO], Union[str, tuple], str) -> None
    """Writes a csv file from pylightxl database. For db that have more than one sheet, will write out,
    multiple files with the sheetname tagged on the end (ex: "fn_sh2.csv")

    :param db: pylightxl Database
    :type db: Database
    :param fn: output file name (without extension; ie. no '.csv')
    :type fn: Union[str, pathlib.Path, io.StringIO]
    :param ws: sheetname(s) to read into the database, if not specified - all sheets are read, defaults to ()
    :type ws: Union[str, tuple], optional
    :param delimiter: csv delimiter, defaults to ','
    :type delimiter: str, optional
    """

    # test that file entered was a valid excel file
    if 'pathlib' in str(type(fn)):
        fn = str(fn)

    if ws == ():
        # write all worksheets
        worksheets = db.ws_names
    else:
        # write only specified worksheets
        worksheets = (ws,) if type(ws) is str else ws

    for sheet in worksheets:
            # StringIO will keep on appending each worksheet to the same StringIO
            if '_io.StringIO' not in str(type(fn)):
                new_fn = fn + '_' + sheet + '.csv'

            try:
                if '_io.StringIO' not in str(type(fn)):
                    f = open(new_fn, 'w')
                else:
                    f = fn
            except PermissionError:
                # file is open, adjust name and print warning
                print('pylightxl - Cannot write to existing file <{}> that is open in excel.'.format(new_fn))
                print('     New temporary file was written to <{}>'.format('new_' + new_fn))
                new_fn = 'new_' + new_fn
                f = open(new_fn, 'w')
            finally:
                max_row, max_col = db.ws(sheet).size
                for r in range(1, max_row + 1):
                    row = []
                    for c in range(1, max_col + 1):
                        val = db.ws(sheet).index(r, c)
                        row.append(str(val))

                    if sys.version_info[0] < 3:
                        text = unicode(delimiter.join(row)).replace('\n','')
                        f.write(text)
                        f.write(unicode('\n'))
                    else:
                        text = delimiter.join(row).replace('\n','')
                        f.write(text)
                        f.write('\n')
                # dont close StringIO thats passed in by the user
                if '_io.StringIO' not in str(type(fn)):
                    f.close()


########################################################################################################
# SEC-05: DATABASE FUNCTIONS
########################################################################################################


class Database:

    def __init__(self):
        # keys are worksheet names, values are Workbook classes
        self._ws = {}
        self._sharedStrings = []
        # {order: ws}
        self._wsorder = {}

        # Named Ranges: checking for unique names and unique address for single worksheet
        # {unique_name: unique_address, ...}
        self._NamedRange = {}

    def __repr__(self):
        return 'pylightxl.Database'

    def ws(self, ws):
        # type: (str) -> Worksheet
        """Indexes worksheets within the database

        :param ws: worksheet name
        :type ws: str
        :return: pylightxl.Database.Worksheet class object
        :rtype: Worksheet
        """

        try:
            return self._ws[ws]
        except KeyError:
            raise UserWarning('pylightxl - Sheetname ({}) is not in the database'.format(ws))

    @property
    def ws_names(self):
        # type: () -> List[str]
        """Returns a list of database stored worksheet names

        :return: list of worksheet names
        :rtype: List[str]
        """

        rv = []

        for i in range(len(self._wsorder)):
            rv.append(self._wsorder[i+1])

        return rv

    def add_ws(self, ws, data=None):
        # type: (str, dict) -> None
        """Logs worksheet name and its data in the database

        :param ws: worksheet name
        :type ws: str
        :param data: dictionary of worksheet cell values (ex: {'A1': {'v':10,'f':'','s':'', 'c': ''}, 'A2': {'v':20,'f':'','s':'', 'c': ''}}), defaults to None
        :type data: dict, optional
        """

        if data is None:
            data = {'A1': {'v': '', 'f': '', 's': '', 'c': ''}}
        self._ws[ws] = Worksheet(data)
        if ws not in self._wsorder.values():
            self._wsorder[len(self._wsorder) + 1] = ws

    def remove_ws(self, ws):
        # type: (str) -> None
        """Removes a worksheet and its data from the database

        :param ws: worksheet name
        :type ws: str
        """

        try:
            del(self._ws[ws])
        except KeyError:
            pass

        try:
            # get the order of the ws
            order_index = list(self._wsorder.values()).index(ws)
            order = list(self._wsorder.keys())[order_index]
            # remove the ws from _wsorder
            del(self._wsorder[order])
            # decrease the order of all higher order ws's
            # 1 2 3 4 5
            # 1 x 3 4 5 = 4
            # range 2, 4
            for i in range(order, len(self._wsorder) + 1):
                self._wsorder.update({i: self._wsorder[i+1]})
            # remove the last index
            del(self._wsorder[len(self._wsorder)])
        except ValueError:
            # ws not in db
            pass

    def rename_ws(self, old, new):
        # type: (str, str) -> None
        """Renames an existing worksheet. Caution, renaming to an existing new worksheet name will overwrite

        :param old: old name
        :type old: str
        :param new: new name
        :type new: str
        """

        if new == "":
            raise UserWarning('pylightxl - sheetname should not be set to an empty string, excel will cause a warning when trying to open.')

        try:
            self._ws[new] = self._ws[old]
            del(self._ws[old])

            order_index = list(self._wsorder.values()).index(old)
            order = list(self._wsorder.keys())[order_index]
            if new in self._wsorder.values():
                # delete old out, keep existing name and order (this is a ws overwrite)
                del(self._wsorder[order])
                # update all high order ws's
                for i in range(order, len(self._wsorder) + 1):
                    self._wsorder.update({i: self._wsorder[i + 1]})
                # remove the last index
                del (self._wsorder[len(self._wsorder)])
            else:
                self._wsorder[order] = new
        except KeyError:
            pass

    def set_emptycell(self, val):
        # type: (Union[str,int,float]) -> None
        """Custom definition for how pylightxl returns an empty cell

        :param val: (default='') empty cell value
        :type val: Union[str,int,float]
        """

        for ws in self.ws_names:
            self.ws(ws).set_emptycell(val)

    def add_nr(self, name, ws, address):
        # type: (str, str, str) -> None
        """Add a NamedRange to the database. There can not be duplicate name or addresses. A named range
        that overlaps either the name or address will overwrite the database's existing NamedRange

        :param name: NamedRange name
        :type name: str
        :param ws: worksheet name
        :type ws: str
        :param address: range of address (single cell ex: "A1", range ex: "A1:B4")
        :type address: str
        """

        if ws not in self.ws_names:
            self.add_ws(ws)

        full_address = ws + '!' + address.replace('$', '')
        if full_address in self._NamedRange.values():
            # conflicting address, overwrite existing entry
            # get key for the full_address value
            key = list(self._NamedRange.keys())[list(self._NamedRange.values()).index(full_address)]
            # remove old key/value
            del(self._NamedRange[key])
            # add the new key/value
            self._NamedRange.update({name: full_address})
        else:
            # potentially a new entry or overwrite by name
            self._NamedRange.update({name: full_address})

    def remove_nr(self, name):
        # type: (str) -> None
        """Removes a Named Range from the database

        :param name: NamedRange name
        :type name: str
        """

        try:
            del(self._NamedRange[name])
        except KeyError:
            pass

    @property
    def nr_names(self):
        # type: () -> Dict[str, str]
        """Returns the dictionary of named ranges ex: {unique_name: unique_address, ...}

        :return: {unique_name: unique_address, ...}
        :rtype: Dict[str, str]
        """

        return self._NamedRange

    def nr(self, name, formula=False, output='v'):
        # type: (str, bool, str) -> List[list]
        """Returns the contents of a name range in a nest list form [row][col]

        :param name: NamedRange name
        :type name: str
        :param formula: flag to return the formula of this cell, defaults to False
        :type formula: bool, optional
        :param output: output request "v" for value, "f" for formula, "c" for comment, defaults to 'v'
        :type output: str, optional
        :return: nest list form [row][col]
        :rtype: List[list]
        """

        output = output.lower()
        if output not in ['v', 'f', 'c']:
            raise UserWarning('pylightxl - incorrect address(output={output}) argument. '
                              'Valid options = "v", "f", "c"'.format(output=output))

        if formula:
            print('DEPRECATION WARNING: address(formula=) argument has been replaced by address(output="f"). '
                  'Please update code base to use "output" argument')
            output = 'f'


        try:
            full_address = self._NamedRange[name]
        except KeyError:
            return [[]]

        ws, address = full_address.split('!')
        return self.ws(ws).range(address, output=output)

    def nr_loc(self, name):
        # type: (str) -> List[str]
        """Returns the worksheet and address loction of a named range

        :param name: NamedRange name
        :type name: str
        :return: [worksheet, address]
        :rtype: List[str]
        """

        try:
            full_address = self._NamedRange[name]
        except KeyError:
            return []

        ws, address = full_address.split('!')
        return [ws, address]

    def update_nr(self, name, val):
        # type: (str, Union[int,float,str]) -> None
        """Updates a NamedRange with a single value. Raises UserWarning if name not in workbook.

        :param name: NamedRange name
        :type name: str
        :param val: cell value; equations are string and must being with "="
        :type val: Union[int,float,str]
        """

        try:
            full_address = self._NamedRange[name]
        except KeyError:
            raise UserWarning('pylightxl - update_nr operation with name={name} is not in the workbook.'.format(name=name))
        
        ws, address = full_address.split('!')
        self.ws(ws).update_range(address, val)

class Worksheet():

    def __init__(self, data=None):
        # type: (dict) -> None
        """Takes a data dict of worksheet cell data (ex: {'A1': 1})

        :param data: worksheet cell data (ex: {'A1': 1}), defaults to None
        :type data: dict, optional
        """
        self._data = data if data != None else {}
        self.maxrow = 0
        self.maxcol = 0
        self._calc_size()
        self._emptycell = ''

    def __repr__(self):
        return 'pylightxl.Database.Worksheet'

    def _calc_size(self):
        """
        Calculates the size of the worksheet row/col. This only occurs on initialization

        :return: None (but this creates instance attributes maxrow/maxcol)
        """

        if self._data != {}:
            list_of_addresses = list(self._data.keys())

            list_of_chars = []
            list_of_nums = []
            for address in list_of_addresses:
                list_of_chars.append(''.join(filter(lambda x: x.isalpha(), address)))
                list_of_nums.append(int(''.join(filter(lambda x: unicode(x).isnumeric(), address))))
            self.maxrow = int(max(list_of_nums))
            # if all chars are the same length
            list_of_chars.sort(reverse=True)
            # if chars are different length
            list_of_chars.sort(key=len, reverse=True)
            self.maxcol = utility_address2index(list_of_chars[0]+"1")[1]
        else:
            self.maxrow = 0
            self.maxcol = 0

    def set_emptycell(self, val):
        # type: (Union[int, float, str]) -> None
        """Custom definition for how pylightxl returns an empty cell

        :param val: (default='') empty cell value
        :type val: Union[int, float, str]
        """

        self._emptycell = val

    @property
    def size(self):
        # type: () -> List[int]
        """Returns the size of the worksheet (row/col)

        :return: list of [maxrow, maxcol]
        :rtype: List[int]
        """

        return [self.maxrow, self.maxcol]

    def address(self, address, formula=False, output='v'):
        # type: (str, bool, str) -> Union[int, float, str, bool]
        """Takes an excel address and returns the worksheet stored value

        :param address: Excel address (ex: "A1")
        :type address: str
        :param formula: flag to return the formula of this cell, defaults to False
        :type formula: bool, optional
        :param output: output request "v" for value, "f" for formula, "c" for comment, defaults to 'v'
        :type output: str, optional
        :return: cell value
        :rtype: Union[int, float, str, bool]
        """

        address = address.replace('$', '')

        output = output.lower()
        if output not in ['v', 'f', 'c']:
            raise UserWarning('pylightxl - incorrect address(output={output}) argument. '
                              'Valid options = "v", "f", "c"'.format(output=output))

        if formula:
            print('DEPRECATION WARNING: address(formula=) argument has been replaced by address(output="f"). '
                  'Please update code base to use "output" argument')
            output = 'f'

        try:
            if output == 'v':
                rv = self._data[address]['v']
            elif output == 'f':
                rv = '=' + self._data[address]['f']
            else:
                rv = self._data[address]['c']
        except KeyError:
            # no data was parsed, return empty cell value
            rv = self._emptycell

        return rv

    def range(self, address, formula=False, output='v'):
        # type: (str, bool, str) -> List[List[Union[int, float, str, bool]]]
        """Takes a range (ex: "A1:A2") and returns a nested list [row][col]

        :param address: cell range (ex: "A1:A2", or "A1")
        :type address: str
        :param formula: returns the values if false, or formulas if true of cells, defaults to False
        :type formula: bool, optional
        :param output: output request "v" for value, "f" for formula, "c" for comment, defaults to 'v'
        :type output: str, optional
        :return: nested list [row][col] regardless if range is a single cell or a range
        :rtype: _type_
        """

        rv = []
        address = address.replace('$', '')

        output = output.lower()
        if output not in ['v', 'f', 'c']:
            raise UserWarning('pylightxl - incorrect range(output={output}) argument. '
                              'Valid options = "v", "f", "c"'.format(output=output))

        if formula:
            print('DEPRECATION WARNING: range(formula=) argument has been replaced by range(output="f"). '
                  'Please update code base to use "output" argument')
            output = 'f'

        if ':' in address:
            address_start, address_end = address.split(':')
            # check for entire row/col address
            if unicode(address_start).isnumeric() and unicode(address_end).isnumeric():
                # 1:1 is row 1, 1:3 is rows 1-3
                row_start, col_start = int(address_start), 1
                row_end, col_end = int(address_end), MAX_XL_COLS
            elif address_start.isalpha() and address_end.isalpha():
                # A:A is col A, A:C is col A-C
                row_start, col_start = 1, utility_columnletter2num(address_start)
                row_end, col_end = MAX_XL_ROWS, utility_columnletter2num(address_end)
            else:
                row_start, col_start = utility_address2index(address_start)
                row_end, col_end = utility_address2index(address_end)

            # +1 to include the end
            for n_row in range(row_start, row_end + 1):
                # -1 to drop index count from excel (start start at 1 to python at 0)
                # +1 to include the end
                if col_end <= self.size[1]:
                    row = self.row(n_row, output=output)[(col_start - 1):(col_end - 1 + 1)]
                else:
                    # add extra empty cells on since self.row will only return up to the size of ws data
                    row = self.row(n_row, output=output)[col_start - 1:]
                    while len(row) < col_end - (col_start - 1):
                        row.append(self._emptycell)
                rv.append(row)
        else:
            rv.append([self.address(address, output=output)])

        return rv

    def index(self, row, col, formula=False, output='v'):
        # type: (int, int, bool, str) -> Union[int, float, str, bool]
        """Takes an excel row and col starting at index 1 and returns the worksheet stored value

        :param row: row index (starting at 1)
        :type row: int
        :param col: col index (start at 1 that corresponds to column "A")
        :type col: int
        :param formula: flag to return the formula of this cell, defaults to False
        :type formula: bool, optional
        :param output: output request "v" for value, "f" for formula, "c" for comment, defaults to 'v'
        :type output: str, optional
        :return: cell value
        :rtype: Union[int, float, str, bool]
        """

        address = utility_index2address(row, col)

        output = output.lower()
        if output not in ['v', 'f', 'c']:
            raise UserWarning('pylightxl - incorrect index(output={output}) argument. '
                              'Valid options = "v", "f", "c"'.format(output=output))

        if formula:
            print('DEPRECATION WARNING: index(formula=) argument has been replaced by index(output="f"). '
                  'Please update code base to use "output" argument')
            output = 'f'

        return self.address(address, output=output)

    def update_index(self, row, col, val):
        # type: (int, int, Union[int, float, str, bool]) -> None
        """Update worksheet data via index

        :param row: row index
        :type row: int
        :param col: column index
        :type col: int
        :param val: cell value; equations are strings and must begin with "="
        :type val: Union[int, float, str, bool]
        """

        address = utility_index2address(row, col)
        self.maxcol = col if col > self.maxcol else self.maxcol
        self.maxrow = row if row > self.maxrow else self.maxrow
        # log formulas under formulas and trim off the '='
        if type(val) is str and len(val) != 0 and val[0] == '=':
            # overwrite existing cell val to be empty (it will calc when excel is opened)
            self._data.update({address: {'v': '', 'f': val[1:], 's': ''}})
        else:
            self._data.update({address: {'v': val, 'f': '', 's': ''}})

    def update_address(self, address, val):
        # type: (str, Union[int, float, str, bool]) -> None
        """Update worksheet data via address

        :param address: excel address (ex: "A1")
        :type address: str
        :param val: cell value; equations are strings and must begin with "="
        :type val: Union[int, float, str, bool]
        """

        address = address.replace('$', '')
        row, col = utility_address2index(address)
        self.maxcol = col if col > self.maxcol else self.maxcol
        self.maxrow = row if row > self.maxrow else self.maxrow
        # log formulas under formulas and trim off the '='
        if type(val) is str and len(val) != 0 and val[0] == '=':
            # overwrite existing cell val to be empty (it will calc when excel is opened)
            self._data.update({address: {'v': '', 'f': val[1:], 's': ''}})
        else:
            self._data.update({address: {'v': val, 'f': '', 's': ''}})

    def update_range(self, address, val):
        # type: (str, Union[int, float, str, bool]) -> None
        """Update worksheet data via address range with a single value

        :param address: excel address (ex: "A1:B3")
        :type address: str
        :param val: cell value; equations are strings and must begin with "="
        :type val: Union[int, float, str, bool]
        """

        if ':' in address:
            address_start, address_end = address.split(':')
            row_start, col_start = utility_address2index(address_start)
            row_end, col_end = utility_address2index(address_end)

            # +1 to include the end
            for n_row in range(row_start, row_end + 1):
                for n_col in range(col_start, col_end + 1):
                    self.update_index(n_row, n_col, val)
        else:
            self.update_address(address, val)

    def row(self, row, formula=False, output='v'):
        # type: (int, bool, str) -> List[Union[int, float, str, bool]]
        """Takes a row index input and returns a list of cell data

        :param row: row index (starting at 1)
        :type row: int
        :param formula: flag to return the formula of this cell, defaults to False
        :type formula: bool, optional
        :param output: output request "v" for value, "f" for formula, "c" for comment, defaults to 'v'
        :type output: str, optional
        :return: list of cell data
        :rtype: List[Union[int, float, str, bool]]
        """

        rv = []

        output = output.lower()
        if output not in ['v', 'f', 'c']:
            raise UserWarning('pylightxl - incorrect row(output={output}) argument. '
                              'Valid options = "v", "f", "c"'.format(output=output))

        if formula:
            print('DEPRECATION WARNING: row(formula=) argument has been replaced by row(output="f"). '
                  'Please update code base to use "output" argument')
            output = 'f'

        for c in range(1, self.maxcol + 1):
            val = self.index(row, c, output=output)
            rv.append(val)

        return rv

    def col(self, col, formula=False, output='v'):
        # type: (int, bool, str) -> List[Union[int, float, str, bool]]
        """Takes a col index input and returns a list of cell data

        :param col: col index (start at 1 that corresponds to column "A")
        :type col: int
        :param formula: flag to return the formula of this cell, defaults to False
        :type formula: bool, optional
        :param output: output request "v" for value, "f" for formula, "c" for comment, defaults to 'v'
        :type output: str, optional
        :return: list of cell data
        :rtype: List[Union[int, float, str, bool]]
        """

        rv = []

        output = output.lower()
        if output not in ['v', 'f', 'c']:
            raise UserWarning('pylightxl - incorrect col(output={output}) argument. '
                              'Valid options = "v", "f", "c"'.format(output=output))

        if formula:
            print('DEPRECATION WARNING: col(formula=) argument has been replaced by col(output="f"). '
                  'Please update code base to use "output" argument')
            output = 'f'

        for r in range(1, self.maxrow + 1):
            val = self.index(r, col, output=output)
            rv.append(val)

        return rv

    @property
    def rows(self):
        # type: () -> Iterable[List[Union[int, float, str, bool]]]
        """Returns a list of rows that can be iterated through

        :return: list of rows-lists (ex: [[11,12,13],[21,22,23]] for 2 rows with 3 columns of data
        :rtype: Iterable[List[Union[int, float, str, bool]]]
        """

        rv = []

        for r in range(1, self.maxrow + 1):
            rv.append(self.row(r))

        return iter(rv)

    @property
    def cols(self):
        # type: () -> Iterable[List[Union[int, float, str, bool]]]
        """Returns a list of cols that can be iterated through

        :return: list of cols-lists (ex: [[11,21],[12,22],[13,23]] for 2 rows with 3 columns of data
        :rtype: Iterable[List[Union[int, float, str, bool]]]
        """

        rv = []

        for c in range(1, self.maxcol + 1):
            rv.append(self.col(c))

        return iter(rv)

    def keycol(self, key, keyindex=1):
        # type: (Union[str,int,float,bool],int) -> List[Union[str,int,float,bool]]
        """Takes a column key value (value of any cell within keyindex row) and returns the entire column,
        no match returns an empty list

        :param key: any cell value within keyindex row (type sensitive)
        :type key: Union[str,int,float,bool]
        :param keyindex: option keyrow override. Must be >0 and smaller than worksheet size, defaults to 1
        :type keyindex: int, optional
        :return: list of the entire matched key column data (only first match is returned)
        :rtype: List[Union[str,int,float,bool]]
        """

        if not keyindex > 0 and not keyindex <= self.size[0]:
            raise UserWarning('pylightxl - keyindex ({}) entered must be >0 and <= worksheet size ({}.'.format(keyindex,self.size))

        # find first key match, get its column index and return col list
        for col_i in range(1, self.size[1] + 1):
            if key == self.index(keyindex, col_i):
                return self.col(col_i)
        return []

    def keyrow(self, key, keyindex=1):
        # type: (Union[str,int,float,bool],int) -> List[Union[str,int,float,bool]]
        """Takes a row key value (value of any cell within keyindex col) and returns the entire row,
        no match returns an empty list

        :param key: any cell value within keyindex col (type sensitive)
        :type key: Union[str,int,float,bool]
        :param keyindex: option keyrow override. Must be >0 and smaller than worksheet size, defaults to 1
        :type keyindex: int, optional
        :return: list of the entire matched key row data (only first match is returned)
        :rtype: List[Union[str,int,float,bool]]
        """

        if not keyindex > 0 and not keyindex <= self.size[1]:
            raise UserWarning('pylightxl - keyindex ({}) entered must be >0 and <= worksheet size ({}.'.format(keyindex,self.size))

        # find first key match, get its row index and return col list
        for row_i in range(1, self.size[0] + 1):
            if key == self.index(row_i, keyindex):
                return self.row(row_i)
        return []

    def ssd(self, keyrows='KEYROWS', keycols='KEYCOLS'):
        # type: (str, str) -> List[Dict[str,list]]
        """Runs through the worksheet and looks for "KEYROWS" and "KEYCOLS" flags in each cell to identify
        the start of a semi-structured data. A data table is read until an empty header is
        found by row or column. The search supports multiple tables.

        :param keyrows: a flag to indicate the start of keyrow's
                            cells below are read until an empty cell is reached, defaults to 'KEYROWS'
        :type keyrows: str, optional
        :param keycols: a flag to indicate the start of keycol's
                            cells to the right are read until an empty cell is reached, defaults to 'KEYCOLS'
        :type keycols: str, optional
        :return: list of data dict in the form of [{'keyrows': [], 'keycols': [], 'data': [[], ...]}, {...},]
        :rtype: List[Dict[str,list]]
        """

        # find the index of keyrow(s) and keycol(s) plural if there are multiple datasets - this is a fast loop downselect
        kr_colIDs = [colID for colID, col in enumerate(self.cols, 1) if keyrows in col or keyrows + keycols in col or keycols + keyrows in col]
        kc_rowIDs = [rowID for rowID, row in enumerate(self.rows, 1) if keycols in row or keyrows + keycols in row or keycols + keyrows in row]

        # look for duplicate key flags within rows/cols - this is a slower loop
        temp = []
        for row_id in range(1, self.maxrow):
            for col_id in kr_colIDs:
                cell = self.index(row_id, col_id)
                if cell != '' and type(cell) is str and keyrows in cell:
                    temp.append([row_id, col_id])
        kr_indexIDs = temp

        # for col_id in kr_colIDs:
        #     for row_id, cell in enumerate(self.col(col_id), 1):
        #         if cell != '' and type(cell) is str and keyrows in cell:
        #             temp.append([row_id, col_id])

        temp = []
        for row_id in kc_rowIDs:
            for col_id, cell in enumerate(self.row(row_id), 1):
                if cell != '' and type(cell) is str and keycols in cell:
                    temp.append([row_id, col_id])
        kc_indexIDs = temp

        if len(kr_indexIDs) != len(kc_indexIDs):
            raise UserWarning('pylightxl - keyrows != keycols most likely due to missing keyword '
                             'flag keyrow IDs: {}, keycol IDs: {}'.format(kr_indexIDs, kc_indexIDs))

        # datas structure: [{'keycols': ..., 'keyrows': ..., 'data'},...]
        datas = []
        dataset_i = 0
        for kr_indexID, kc_indexID in zip(kr_indexIDs, kc_indexIDs):

            r, c = 0, 1

            datas.append({'keyrows': [], 'keycols': [], 'data': []})

            # pull the column for keycol_ID
            kr_header = self.col(kr_indexID[c])[kr_indexID[r]:]
            # find the end for column header (by looking for empty cell)
            try:
                end_col_index = kr_header.index('')
            except ValueError:
                end_col_index = self.maxrow - kr_indexID[r]
            kr_end = end_col_index

            # pull the row for keyrow_ID
            kc_header = self.row(kc_indexID[r])[kc_indexID[c]:]
            # find the end for column header (by looking for empty cell)
            try:
                end_row_index = kc_header.index('')
            except ValueError:
                end_row_index = self.maxrow - kc_indexID[c]
            kc_end = end_row_index

            # truncate headers down
            datas[dataset_i]['keyrows'] = kr_header[:kr_end]
            datas[dataset_i]['keycols'] = kc_header[:kc_end]

            for row_i in range(kr_indexID[r] + 1, kr_indexID[r] + kr_end + 1):
                datas[dataset_i]['data'].append(self.row(row_i)[kc_indexID[c]:kc_indexID[c] + kc_end])
            dataset_i += 1

        return datas


########################################################################################################
# SEC-06: UTILITY FUNCTIONS
########################################################################################################


def utility_address2index(address):
    # type: (str) -> List[int]
    """Convert excel address to row/col index

    :param address: Excel address (ex: "A1")
    :type address: str
    :return: list of [row, col]
    :rtype: List[int]
    """

    if type(address) is not str:
        raise UserWarning('pylightxl - Address ({}) must be a string.'.format(address))
    if address == '':
        raise UserWarning('pylightxl - Address ({}) cannot be an empty str.'.format(address))

    address = address.upper()

    strVSnum = re.compile(r'[A-Z]+')
    try:
        colstr = strVSnum.findall(address)[0]
    except IndexError:
        raise UserWarning('pylightxl - Incorrect address ({}) entry. Address must be an alphanumeric '
                         'where the starting character(s) are alpha characters a-z'.format(address))

    if not colstr.isalpha():
        raise UserWarning('pylightxl - Incorrect address ({}) entry. Address must be an alphanumeric '
                         'where the starting character(s) are alpha characters a-z'.format(address))

    col = utility_columnletter2num(colstr)

    try:
        row = int(strVSnum.split(address)[1])
    except (IndexError, ValueError):
        raise UserWarning('pylightxl - Incorrect address ({}) entry. Address must be an alphanumeric '
                         'where the trailing character(s) are numeric characters 1-9'.format(address))

    return [row, col]


def utility_index2address(row, col):
    # type: (int, int) -> str
    """Converts index row/col to excel address

    :param row: row index (starting at 1)
    :type row: int
    :param col: col index (start at 1 that corresponds to column "A")
    :type col: int
    :return: str excel address
    :rtype: str
    """

    if type(row) is not int and type(row) is not float:
        raise UserWarning('pylightxl - Incorrect row ({}) entry. Row must either be a int or float'.format(row))
    if type(col) is not int and type(col) is not float:
        raise UserWarning('pylightxl - Incorrect col ({}) entry. Col must either be a int or float'.format(col))
    if row <= 0 or col <= 0:
        raise UserWarning('pylightxl - Row ({}) and Col ({}) entry cannot be less than 1'.format(row, col))

    # values over 26 are outside the A-Z range, reduce them
    colname = utility_num2columnletters(col)

    return colname + str(row)


def utility_columnletter2num(text):
    # type: (str) -> int
    """Takes excel column header string and returns the equivalent column count

    :param text: excel column (ex: 'AAA' will return 703)
    :type text: str
    :return: int of column count
    :rtype: int
    """

    letter_pos = len(text) - 1
    val = 0
    try:
        val = (ord(text[0].upper())-64) * 26 ** letter_pos
        next_val = utility_columnletter2num(text[1:])
        val = val + next_val
    except IndexError:
        return val
    return val


def utility_num2columnletters(num):
    # type: (int) -> str
    """Takes a column number and converts it to the equivalent excel column letters

    :param num: column number
    :type num: int
    :return: excel column letters
    :rtype: str
    """

    def pre_num2alpha(num):
        if num % 26 != 0:
            num = [num // 26, num % 26]
        else:
            num = [(num - 1) // 26, 26]

        if num[0] > 26:
            num = pre_num2alpha(num[0]) + [num[1]]
        else:
            num = list(filter(lambda x: False if x == 0 else True, num))

        return num

    return "".join(list(map(lambda x: chr(x + 64), pre_num2alpha(num))))


def utility_xml_namespace(file):
    # type: (str) -> Dict[str, str]
    """Takes an xml file and returns the root namespace as a dict

    :param file: xml file path
    :type file: str
    :return: dictionary of root namespace
    :rtype: Dict[str, str]
    """

    events = "start", "start-ns", "end-ns"

    ns_map = []

    for event, elem in ET.iterparse(file, events):
        if event == "start-ns":
            elem = ('default', elem[1]) if elem[0] == '' else elem
            ns_map.append(elem)
        if event == "start":
            break
    ns = dict(ns_map)
    if 'default' not in ns.keys():
        ns['default'] = ns['x']
    return ns
