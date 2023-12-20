#
# Copyright (c) 2009 steelpy
# 

# Python stdlib imports
from __future__ import annotations
#from dataclasses import dataclass
from typing import NamedTuple
#import re

# package imports
# steelpy.f2uModel
from steelpy.f2uModel.load.process.actions import SelfWeight
from steelpy.f2uModel.load.process.basic_load import BasicLoadBasic, LoadTypeBasic
from steelpy.f2uModel.load.sqlite.beam import BeamLoadItemSQL
from steelpy.f2uModel.load.sqlite.node import  NodeLoadItemSQL
from steelpy.f2uModel.load.sqlite.wave_load import WaveLoadItemSQL
#
#from steelpy.f2uModel.mesh.sqlite.beam import BeamItemSQL
# steelpy.f2uModel
from steelpy.f2uModel.mesh.sqlite.process_sql import create_connection, create_table
from steelpy.utils.dataframe.main import DBframework
#
# ---------------------------------
#
#
class BasicLoadSQL(BasicLoadBasic):
    """
    FE Load Cases

    LoadType
        |_ name
        |_ number
        |_ basic
        |_ combination_level
        |_ time_series
        |_
        |_ temperature

    **Parameters**:
      :number:  integer internal number
      :name:  string node external name
    """
    __slots__ = ['_labels', '_title','_number', 'gravity',
                 '_basic', 'db_file', '_plane']
    #
    def __init__(self, db_file:str, plane: NamedTuple) -> None:
        """
        """
        super().__init__()
        #
        self.db_file = db_file
        self._plane = plane
        #
        self._basic: dict = {}
        conn = create_connection(self.db_file)
        with conn: 
            self._create_table(conn)
    #
    #
    def __setitem__(self, load_name:int|str, load_title:str) -> None:
        """
        """
        #load_name = load_name
        try:
            self._labels.index(load_name)
            raise Warning('    *** warning load name {:} already exist'
                            .format(load_name))
        except ValueError:
            self._labels.append(load_name)
            self._title.append(load_title)
            #
            conn = create_connection(self.db_file)
            with conn:
                load_number = self._push_basic_load(conn, load_name, load_title)
            self._number.append(load_number)
            #
            self._basic[load_name] = LoadTypeSQL(name=load_name,
                                                 number=load_number,
                                                 title=load_title,
                                                 plane=self._plane, 
                                                 bd_file=self.db_file)
    #           
    def __getitem__(self, load_name: str|int):
        """
        """
        try:
            #load_name = load_name
            return self._basic[load_name]
        except KeyError:
            raise IOError("load case not defined")

    #
    def __delitem__(self, load_name: str|int):
        """
        """
        load_name = str(load_name)
        del self._basic[load_name]
    #
    #
    def _push_basic_load(self, conn, load_name:int|str, load_title:str):
        """ """
        #load_name = str(load_name)
        project = (load_name, load_title, "basic")
        sql = 'INSERT INTO tb_Load(name, title, type) VALUES(?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
        return cur.lastrowid
    #
    def _create_table(self, conn):
        """ """
        table_load = "CREATE TABLE IF NOT EXISTS tb_Load(\
                      number INTEGER PRIMARY KEY NOT NULL,\
                      name NOT NULL,\
                      title TEXT NOT NULL,\
                      type TEXT NOT NULL);"

        table_comb_load = "CREATE TABLE IF NOT EXISTS tb_LoadCombIndex(\
                            number INTEGER PRIMARY KEY NOT NULL,\
                            load_number INTEGER NOT NULL REFERENCES tb_Load(number),\
                            bl_number INTEGER REFERENCES tb_Load(number),\
                            lc_number INTEGER REFERENCES tb_Load(number),\
                            factor DECIMAL NOT NULL);"

        #conn = create_connection(self.db_file)
        create_table(conn, table_load)
        create_table(conn, table_comb_load)
    #
    #
    @property
    def df(self):
        """basic load df"""
        print('basic load in')
        db = DBframework()
        #
        conn = create_connection(self.db_file)
        with conn:        
            nodedf = node_load(conn, db)
            memgdf = member_load(conn, db)
        #
        #for name in self._labels:
        #    bload = self.__getitem__(name)
        #    nodedf = bload._node.df
        #    nodedf
        1 / 0
    
    @df.setter
    def df(self, value):
        """basic load df"""
        print('basic load out')
        1 / 0    
    #
#
#
def node_load(conn, db):
    """ """
    cur = conn.cursor()
    cur.execute("SELECT tb_Load.name, tb_Load.title, tb_Load.type,\
                 tb_Nodes.name, tb_LoadNode.* \
                 FROM tb_Load, tb_Nodes, tb_LoadNode\
                 WHERE tb_Load.number = tb_LoadNode.load_number \
                 AND tb_Nodes.number = tb_LoadNode.node_number \
                 AND tb_LoadNode.type = 'load' ")
    rows = cur.fetchall()
    #
    cols = ['load_name', 'load_title', 'load_type', 'node_name', 
            'number', 'load_number', 'element_name', 
            'node_number','load_comment', 'load_system', 'type',
            'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz',
            'x', 'y', 'z', 'rx', 'ry', 'rz']
    nodedf = db.DataFrame(data=rows, columns=cols)        
    #
    nodedf = nodedf[['load_name', 'load_type', 'load_name',
                     'load_system', 'load_comment', 'element_name',
                     'node_name', 'type',
                     'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz',
                     'x', 'y', 'z', 'rx', 'ry', 'rz']]
    #
    #
    return nodedf
#
#
def member_load(conn, db):
    """ """
    cur = conn.cursor()
    # line
    cur.execute("SELECT tb_Load.name, tb_Load.title, tb_Load.type,\
                 tb_Elements.name, tb_LoadBeamLine.* \
                 FROM tb_Load, tb_Elements, tb_LoadBeamLine \
                 WHERE tb_Load.number = tb_LoadBeamLine.load_number \
                 AND tb_Elements.number = tb_LoadBeamLine.element_number \
                 AND tb_LoadBeamLine.type = 'load' ")
    rows = cur.fetchall()
    #
    cols = ['load_name', 'load_title', 'load_type', 'element_name', 
            'number', 'load_number', 'element_number', 
            'load_comment', 'load_system', 'type',
            'L0', 'qx0', 'qy0', 'qz0',
            'L1', 'qx1', 'qy1', 'qz1',
            'BS', 'OTM', 'x', 'y', 'z']
    membdf = db.DataFrame(data=rows, columns=cols)      
    #
    membdf = membdf[['load_name', 'load_type', 'load_name',
                     'load_system', 'load_comment',
                     'element_name', 'type', 
                     'L0', 'qx0', 'qy0', 'qz0',
                     'L1', 'qx1', 'qy1', 'qz1',
                     'BS', 'OTM', 'x', 'y', 'z']].values    
    #
    return membdf
#
#
#
#
class LoadTypeSQL(LoadTypeBasic):
    """
    """
    __slots__ = ['_node', '_beam', '_selfweight', '_wave', 
                 'name', 'number', 'title', '_db_file']

    def __init__(self, name: str, number: int, title: str,
                 plane: NamedTuple, bd_file:str):
        """
        """
        super().__init__(name, number, title)
        #self.name = name       
        self._db_file = bd_file
        #
        self._node = NodeLoadItemSQL(load_name=self.name,
                                     db_file=self._db_file)
        #
        self._beam = BeamLoadItemSQL(load_name=self.name,
                                     plane=plane, 
                                     db_file=self._db_file)
        #
        self._wave = WaveLoadItemSQL(load_name=self.name,
                                     plane=plane, 
                                     db_file=self._db_file)
        #
        self._selfweight = SelfWeight()
    #
    #
#
