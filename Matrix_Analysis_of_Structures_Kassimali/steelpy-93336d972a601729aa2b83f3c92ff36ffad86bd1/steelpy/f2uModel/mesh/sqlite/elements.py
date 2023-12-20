# 
# Copyright (c) 2009-2023 fem2ufo
#
# Python stdlib imports
from __future__ import annotations
#from dataclasses import dataclass
#from array import array
from collections import Counter, defaultdict
from collections.abc import Mapping
#from math import dist
from typing import NamedTuple
from itertools import chain
import re


# package imports
#from steelpy.f2uModel.mesh.sqlite.nodes import get_node
#from steelpy.material.matsql import get_materialSQL
#from steelpy.sections.main import get_sectionSQL
from steelpy.f2uModel.mesh.sqlite.process_sql import create_connection, create_table
#from steelpy.f2uModel.mesh.operations.elements  import (beam_Klocal, trans_3d_beam, Rmatrix)
#from steelpy.f2uModel.mesh.operations.elements import BeamElementSQL
from .beam import BeamSQL, get_connectivity
from steelpy.utils.dataframe.main import DBframework

#
#
class ElementsSQL(Mapping):
    __slots__ = ['_type', '_labels',
                 '_beams', '_plane', 'db_file'] # '_number', 
    
    def __init__(self, db_file:str, plane: NamedTuple, 
                 db_system:str="sqlite") -> None:
        """
        """
        self.db_file = db_file
        self._labels:list = []
        #self._number: array = array('i', [])
        self._type:list = []
        # create node table
        conn = create_connection(self.db_file)
        with conn:        
            self._create_table(conn)
        #
        # TODO: 
        self._plane = plane
        self._beams = BeamSQL(db_file=db_file,
                              labels=self._labels,
                              element_type=self._type, 
                              plane=self._plane)
    #
    #
    #@property
    #def _labels(self):
    #    """ """
    #    labels = self._beams._labels
    #    return labels    
    #
    def plane(self, plane: NamedTuple):
        """ """
        self._plane = plane
        self._beams._plane = self._plane
    #
    #
    def __setitem__(self, element_name: int|str, parameters: list) -> None:
        """
        parameters = ['beam', node1, node2, material, section, roll_angle]
        """
        try:
            self._labels.index(element_name)
            raise Exception('element {:} already exist'.format(element_name))
        except ValueError:
            element_type = parameters[0]
            # default
            #self._labels.append(element_name)
            #self._type.append(element_type)
            #
            if re.match(r"\b(shell(s)?|plate(s)?)\b", element_type, re.IGNORECASE):
                #return self._curve[mat_number]
                raise NotImplementedError('shell element tobe implemented')
            elif re.match(r"\b(beam)\b", element_type, re.IGNORECASE):
                self._beams[element_name] = parameters[1:]
            else:
                raise IOError(f' element type {element_type} not recognised')
            
    #
    def __getitem__(self, element_name: int|str):
        """
        """
        try:
            index = self._labels.index(element_name)
            element_type = self._type[index]
            # FIXME
            #element_type = self._beams._type[index]
        except ValueError:
            raise KeyError('Invalid element name : {:}'.format(element_name))        
        #
        if re.match(r"\b(shell(s)?|plate(s)?)\b", element_type, re.IGNORECASE):
            raise NotImplementedError('shell element tobe implemented')
        elif re.match(r"\b(beam)\b", element_type, re.IGNORECASE):
            return self._beams[element_name]
        else:
            raise IOError(f' element type {element_type} not recognised')    
    #
    #
    def __len__(self) -> int:
        return len(self._labels)

    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels
    #
    # TODO : get number from database
    def get_number(self, start:int=0):
        """
        """
        try:
            n = max(self._labels)
        except ValueError:
            n = start
        #
        while True:
            n += 1
            yield n
    #    
    #
    #def push_element(self, conn, element_number, parameters):
    #    """ """
    #    cur = conn.cursor()
    #    cur.execute("SELECT tb_Materials.name, tb_Materials.number FROM tb_Materials;")
    #    materials = cur.fetchall()
    #    materials = {item[0]:item[1] for item in materials}
    #    #
    #    #cur = conn.cursor()
    #    cur.execute("SELECT tb_Sections.name, tb_Sections.number FROM tb_Sections;")
    #    sections = cur.fetchall()
    #    sections = {item[0]:item[1] for item in sections}
    #    # connectivity
    #    push_connectivity(conn, element_number, parameters[1:3])
    #    #
    #    #try:
    #    roll_angle = parameters[5]
    #    #except IndexError:
    #    #    roll_angle = 0.0
    #    #print('-->')
    #    if (title := parameters[6]) == "NULL":
    #        title = None
    #    #
    #    project = (element_number, title, 
    #               parameters[0],
    #               materials[parameters[3]],
    #               sections[parameters[4]],
    #               roll_angle)
    #    #
    #    sql = 'INSERT INTO tb_Elements(name, title, type, material, section,\
    #                                   roll_angle)\
    #                                   VALUES(?,?,?,?,?,?)'
    #    #cur = conn.cursor()
    #    cur.execute(sql, project)
    #
    def _create_table(self, conn) -> None:
        """ """
        _table_elements = "CREATE TABLE IF NOT EXISTS tb_Elements(\
                            number INTEGER PRIMARY KEY NOT NULL,\
                            name TEXT NOT NULL,\
                            title TEXT,\
                            type TEXT NOT NULL,\
                            material_number INTEGER NOT NULL REFERENCES tb_Materials(number),\
                            section_number INTEGER NOT NULL REFERENCES tb_Sections(number),\
                            roll_angle DECIMAL);"
        #
        _table_connectivity = "CREATE TABLE IF NOT EXISTS tb_Connectivity(\
                                number INTEGER PRIMARY KEY NOT NULL,\
                                element_number INTEGER NOT NULL REFERENCES tb_Elements(number),\
                                node_number INTEGER REFERENCES tb_Nodes(name),\
                                node_end INTEGER NOT NULL);"
        #
        _table_univectors = "CREATE TABLE IF NOT EXISTS tb_DirectionCosines(\
                                number INTEGER PRIMARY KEY NOT NULL,\
                                element_number INTEGER NOT NULL REFERENCES tb_Elements(number),\
                                type TEXT NOT NULL);"
        #
        _table_offset = "CREATE TABLE IF NOT EXISTS tb_Eccentricities(\
                            number INTEGER PRIMARY KEY NOT NULL,\
                            element_number INTEGER NOT NULL REFERENCES tb_Elements(number),\
                            node_number INTEGER REFERENCES tb_Nodes(name),\
                            node_end INTEGER NOT NULL,\
                            system TEXT NOT NULL,\
                            x DECIMAL,\
                            y DECIMAL,\
                            z DECIMAL);"
        #
        #conn = create_connection(self.db_file)
        create_table(conn, _table_elements)
        create_table(conn, _table_connectivity)
        create_table(conn, _table_offset)
        create_table(conn, _table_univectors)
    #
    #def iter_elements(self, arraysize=1000):
    #    """
    #    """
    #    conn = create_connection(self.db_file)
    #    cur = conn.cursor()
    #    # TODO: check if direction cosines given
    #    cur.execute("SELECT tb_Elements.name, tb_Elements.number, tb_Elements.type,\
    #                tb_Elements.roll_angle, tb_Elements.material, tb_Elements.section\
    #                FROM tb_Elements;" )
    #    #
    #    try:
    #        while True:
    #            elements = cur.fetchmany(arraysize)
    #            if not elements:
    #                break
    #            for element in elements:
    #                #cur.execute("SELECT tb_Connectivity.node_end, tb_Connectivity.node_name\
    #                #            FROM tb_Connectivity\
    #                #            WHERE tb_Connectivity.element_name = {:};".format(element[0]))
    #                #row = cur.fetchall()
    #                #connodes = [x for _, x in sorted(row)]
    #                connodes = get_connectivity(conn, element[0])
    #                data = [*element[0:6], connodes, self.db_file]
    #                yield BeamElement(data)
    #    except Exception as e:
    #        print(e)
    #    finally:
    #        conn.close()
    #
    @property
    def get_connectivities(self):
        """ """
        conn = create_connection(self.db_file)
        cur = conn.cursor()
        cur.execute( "SELECT tb_Elements.name FROM tb_Elements;")
        elements = cur.fetchall()
        connodes = []
        for element in elements:
            #cur.execute("SELECT tb_Connectivity.node_end, tb_Connectivity.node_name\
            #            FROM tb_Connectivity\
            #            WHERE tb_Connectivity.element_name = {:};".format(member[0]))
            #row = cur.fetchall()
            #connodes.append([x for _,x in sorted(row)])
            connodes.append(get_connectivity(conn, element[0]))
        conn.close()
        return connodes
    #
    #
    def update_item(self, element_number:int, item:str, value:Union[float,int]):
        """ """
        conn = create_connection(self.db_file)
        with conn:
            update_element_item(conn, element_number, item, value)
            #conn.commit()
    #
    @property
    def get_free_nodes(self):
        """
        find nodes not sharing elements
        """
        connectivities = self.get_connectivities
        #connectivities = [conn for conn in connectivities.values()]
        #column
        flat = list(chain.from_iterable(connectivities))
        return [k for k, v in Counter(flat).items() if v == 1]
    #
    #@property
    def beams(self, values:None|list=None,
              df=None):
        """
        """
        if values:
            if isinstance(values, list):
                for item in values:
                    element_name = item[0]
                    try:
                        self._labels.index(element_name)
                        raise Exception('element {:} already exist'.format(element_name))
                    except ValueError:
                        element_type = 'beam'
                        mnumber = next(self.get_number())
                        # default
                        self._labels.append(element_name)
                        #self._number.append(mnumber)
                        self._type.append(element_type)
                        self._beams[element_name] = item[1:]
        #
        # dataframe input
        try:
            df.columns
            self._beams.df = df
        except AttributeError:
            pass
        #
        return self._beams
        #return ElementType(item_type='beam',
        #                   cls_type=self._beams, cls=self)
    #
    @property
    def df(self):
        """nodes in dataframe format"""
        #print('elements df out')
        conn = create_connection(self.db_file)
        with conn:        
            data = get_elements(conn)
        #
        header = ['name', 'type', 'material', 'section',
                  'node_1', 'node_2', 'node_3', 'node_4', 
                  'roll_angle', 'title']        
        return data[header]

    @df.setter
    def df(self, df):
        """nodes in dataframe format"""
        try:
            df.columns
            1 / 0
            #self._nodes.df =df
        except AttributeError:
            raise IOError('Node df not valid')    
#
#
class ElementType(Mapping):
    __slots__ =  ['_type', '_labels', '_number',
                  '_cls_type', '_item_type']
    
    def __init__(self, item_type: str, cls_type, cls):
        """
        """
        self._cls_type = cls_type
        self._item_type = item_type
        self._labels = cls._labels
        self._type = cls._type
        #self._number = cls._number
    #
    def __setitem__(self, item_name:str|int,
                    properties:list[str|float]) -> None:
        """
        item_name : element number
        properties : [material, section, node1, node2, roll_angle]
        """
        self._labels.append(item_name)
        self._type.append(self._item_type)
        #mat_number = next(self.get_number())
        #self._number.append(mat_number)
        self._cls_type[item_name] = properties
    
    def __getitem__(self, item_name:str|int):
        """
        """
        index = self._labels.index(item_name)
        #mat_number = self._number[index]        
        return self._cls_type[item_name]
    #
    #
    #
    def __len__(self) -> float:
        return len(self._cls_type._labels)

    def __iter__(self):
        """
        """
        return iter(self._cls_type._labels)

    def __contains__(self, value) -> bool:
        return value in self._cls_type._labels
    #
    #
    def __str__(self) -> str:
        """ """
        #print('--')
        return self._cls_type.__str__()
    #
    #
    @property
    def df(self):
        """ """
        return self._cls_type.df
    #
    #def get_number(self, start:int=1):
    #    """
    #    """
    #    try:
    #        n = max(self._number) + 1
    #    except ValueError:
    #        n = start
    #    #
    #    while True:
    #        yield n
    #        n += 1
    #
    #
    #@property
    def get_connectivities(self):
        """ """
        return self._cls_type.get_connectivities 
#
#
#def get_connectivity(conn, element_name):
#    """ """
#    cur = conn.cursor()
#    cur.execute("SELECT tb_Connectivity.node_end, tb_Connectivity.node_name\
#                FROM tb_Connectivity\
#                WHERE tb_Connectivity.element_name = {:};".format(element_name))
#    connodes = cur.fetchall()
#    return [x for _, x in sorted(connodes)]
#    #return connodes
#
#def push_connectivity(conn, element_name, connectivity):
#    """
#    """
#    cur = conn.cursor()
#    for x, node in enumerate(connectivity):
#        project = (element_name, node, x+1)
#        sql = 'INSERT INTO  tb_Connectivity(element_name,\
#                                            node_name, node_end)\
#                                            VALUES(?,?,?)'
#        cur.execute(sql, project)
#    #return cur.lastrowid
#
#
def update_element_item(conn, name, item, value):
    """ """
    project = (value, name)
    sql = 'UPDATE tb_Elements SET {:} = ? WHERE name = ?'.format(item)
    cur = conn.cursor()
    cur.execute(sql, project)
#
#
#def get_element_data(conn, element_name):
#    """ """
#    cur = conn.cursor()
#    cur.execute ("SELECT tb_Elements.name, tb_Elements.number, tb_Elements.type,\
#                tb_Elements.roll_angle, tb_Materials.name, tb_Sections.name, tb_Elements.title\
#                FROM tb_Elements, tb_Materials, tb_Sections\
#                WHERE tb_Elements.name = {:} \
#                AND tb_Elements.material = tb_Materials.number \
#                AND tb_Elements.section = tb_Sections.number;".format(element_name))
#    row = cur.fetchone()
#    #
#    connodes = get_connectivity(conn, element_name)
#    data = [*row[:6], connodes, row[-1]]
#    #conn.close ()
#    return data
#
#
def get_elements(conn):
    """ """
    db = DBframework()
    cur = conn.cursor()
    cur.execute ("SELECT tb_Elements.name, tb_Elements.type,\
                tb_Materials.name, tb_Sections.name, tb_Elements.roll_angle, tb_Elements.title\
                FROM tb_Elements, tb_Materials, tb_Sections\
                WHERE tb_Elements.material_number = tb_Materials.number \
                AND tb_Elements.section_number = tb_Sections.number;")
    rows = cur.fetchall()
    header = ['name', 'type', 'material', 'section', 'roll_angle', 'title']
    membdf = db.DataFrame(data=rows, columns=header)
    membdf.set_index('name', inplace=True)
    #
    connodes = get_connectivities(conn)
    conndf = db.DataFrame(data=connodes, columns=['name', 'nodes', 'end'])
    conndf = conndf.pivot(index='name', columns='end', values='nodes')
    #conndf.reset_index(inplace=True)
    #conndf.set_index('name')
    #conndf.rename(columns={1: 'node_1', 2: 'node_2'}, inplace=True)
    #
    membdf = membdf.join(conndf)
    membdf.reset_index(inplace=True)
    membdf.rename(columns={1: 'node_1', 2: 'node_2'}, inplace=True)
    membdf[['node_3', 'node_4']] = None
    #
    return membdf
#
def get_connectivities(conn):
    """ """
    cur = conn.cursor()
    cur.execute("SELECT tb_Elements.name, tb_Nodes.name, tb_Connectivity.node_end \
                FROM tb_Elements, tb_Nodes, tb_Connectivity \
                WHERE tb_Connectivity.element_number = tb_Elements.number \
                AND tb_Nodes.number = tb_Connectivity.node_number;")
    connodes = cur.fetchall()
    xxx = [x for i, j, x in sorted(connodes)]
    #memb = defaultdict(list)
    #for item in connodes:
    #    memb[item[0]].append()
    #return [x for _, x in sorted(connodes)]
    return connodes