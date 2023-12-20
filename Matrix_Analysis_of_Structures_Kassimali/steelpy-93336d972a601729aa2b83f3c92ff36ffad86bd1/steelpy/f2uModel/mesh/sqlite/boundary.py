# Copyright (c) 2009 steelpy

# Python stdlib imports
from __future__ import annotations
#from array import array
#from collections.abc import Mapping
import re
#from typing import NamedTuple


# package imports
from steelpy.f2uModel.mesh.process.boundary import BoundaryItem, BoundaryNode
from steelpy.f2uModel.mesh.sqlite.process_sql import create_connection, create_table, check_nodes
from steelpy.utils.dataframe.main import DBframework
#
#
#
#
class BoundaryNodeSQL(BoundaryNode):
    
    __slots__ = ['_db_file', '_labels']
    
    def __init__(self, db_file:str):
        """
        """
        super().__init__()
        self._db_file = db_file
        # create node table
        conn = create_connection(self._db_file)
        with conn:        
            self._create_table(conn)
    #
    def __setitem__(self, node_name: int,
                    fixity:list|tuple|dict|str) -> None:
        """
        """
        conn = create_connection(self._db_file)
        with conn:  
            node = check_nodes(conn, node_name)       
        try:
            node_number = node[0]
        except TypeError:
            raise IOError(f"Node {node_name} not found")
        
        try:
            # TODO : update data option if needed?
            self._labels.index(node_name)
            raise Warning('    *** warning node {:} boundary already exist'.format(node_name))
        except ValueError:
            self._labels.append(node_name)
            fixity = self._get_fixity(fixity)
            conn = create_connection(self._db_file)
            with conn:
                self._push_boundary(conn, node_number, fixity)
        #
    #
    def __getitem__(self, node_name: int) -> tuple | bool:
        """
        """
        conn = create_connection(self._db_file)
        with conn:  
            node = check_nodes(conn, node_name)       
        try:
            node_number = node[0]
        except TypeError:
            raise IOError(f"Node {node_name} not found")
        
        try:
            self._labels.index(node_name)
            conn = create_connection (self._db_file)
            data = get_boundary(conn, node_number)
            return BoundaryItem(*data[3:], number=data[0],
                                name=data[1], node=node_name)
        except ValueError:
            return False
    #
    def _create_table(self, conn) -> None:
        """ """
        _table_boundary = "CREATE TABLE IF NOT EXISTS tb_Boundaries(\
                            number INTEGER PRIMARY KEY NOT NULL,\
                            title TEXT,\
                            node_number INTEGER NOT NULL REFERENCES tb_Nodes(name),\
                            x DECIMAL, y DECIMAL, z DECIMAL,\
                            rx DECIMAL, ry DECIMAL, rz DECIMAL);"
        #
        #conn = create_connection(self._db_file)
        create_table(conn, _table_boundary)
    #
    #
    def _push_boundary(self, conn, node_number: int, fixity: list):
        """
        """
        try:
            title = fixity[6]
        except IndexError:
            title = None

        project = (title, node_number, *fixity[:6])
        sql = 'INSERT INTO tb_Boundaries(title, node_number,\
                                         x, y, z, rx, ry, rz)\
                                         VALUES(?,?,?,?,?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
    #
    #
    #
    @property
    def df(self):
        """Boundary df"""
        #print('nodes df in')
        conn = create_connection(self._db_file)
        with conn:
            cur = conn.cursor()
            cur.execute ("SELECT tb_Nodes.name, \
                         tb_Boundaries.* \
                         FROM tb_Nodes, tb_Boundaries \
                         WHERE tb_Boundaries.node_number = tb_Nodes.number;")
            rows = cur.fetchall()
        #
        db = DBframework()
        header = ['name', 'number', 'title', 'node_number',
                  'ix', 'iy', 'iz', 'rx', 'ry', 'rz']
        boundf = db.DataFrame(data=rows, columns=header)
        #conn = create_connection(self._db_file)
        #return db.read_sql_query("SELECT * FROM tb_Boundaries", conn)
        header = ['name', 'ix', 'iy', 'iz', 'rx', 'ry', 'rz',  'title']        
        return boundf[header]
    
    @df.setter
    def df(self, df):
        """ """
        #
        columns = list(df.columns)
        header = {}
        for key in columns:
            if re.match(r"\b(id|name|node(s)?)\b", key, re.IGNORECASE):
                header[key] = 'name'
            
            elif re.match(r"\b(type)\b", key, re.IGNORECASE):
                header[key] = 'type'
            
            elif re.match(r"\b(title)\b", key, re.IGNORECASE):
                header[key] = 'title'
            #
            # displacement
            elif re.match(r"\b((i)?(\_|\-|\s*)?x)\b", key, re.IGNORECASE):
                header[key] = 'x'
            
            elif re.match(r"\b((i)?(\_|\-|\s*)?y)\b", key, re.IGNORECASE):
                header[key] = 'y'               
            
            elif re.match(r"\b((i)?(\_|\-|\s*)?z)\b", key, re.IGNORECASE):
                header[key] = 'z'
            #
            # rotation
            elif re.match(r"\b((r)?(\_|\-|\s*)?x)\b", key, re.IGNORECASE):
                header[key] = 'rx'
            
            elif re.match(r"\b((r)?(\_|\-|\s*)?y)\b", key, re.IGNORECASE):
                header[key] = 'ry'               
            
            elif re.match(r"\b((r)?(\_|\-|\s*)?z)\b", key, re.IGNORECASE):
                header[key] = 'rz'             
        #
        nodes = df[header.keys()].copy()
        nodes.rename(columns=header, inplace=True)
        #support.query("x != '' and y != '' and z != '' and rx != '' and ry != '' and rz != ''",
        #              inplace=True)        
        #
        for row in nodes.itertuples():
            #print(row)
            fixity=[row.x, row.y, row.z,
                    row.rx, row.ry, row.rz]
            if any(fixity):
                #print(fixity)
                self.__setitem__(node_name=row.name,
                                 fixity=fixity)
        #
        #
#
#
def get_boundary(conn, node_number, item:str="*"):
    """
    """
    #
    project = (node_number,)
    sql = 'SELECT {:} FROM tb_Boundaries WHERE node_number = ?'.format(item)
    cur = conn.cursor()
    cur.execute(sql, project)
    record = cur.fetchone()
    return record
#
#
#
class BoundarySQL:
    
    def __init__(self, db_file:str,
                 db_system:str="sqlite") -> None:
        """
        """
        self._nodes = BoundaryNodeSQL(db_file)
    #
    @property
    def node(self):
        """"""
        return self._nodes
    
    @node.setter
    def node(self, values):
        """"""
        for value in values:
            self._nodes[value[0]] = value[1:]
#
