#
# Copyright (c) 2009 steelpy
#
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
#from typing import NamedTuple
import re

# package imports
# steelpy.f2uModel
from steelpy.f2uModel.mesh.sqlite.process_sql import (create_connection, create_table,
                                                       get_load_data, check_nodes)
# steelpy.f2uModel.load
from ..process.nodes import (NodeLoadBasic, NodeItem,
                             get_nodal_load, PointNode)

#
from steelpy.utils.dataframe.main import DBframework
# ---------------------------------
#   
#
#
class NodeLoadItemSQL(Mapping):
    __slots__ = ['_name', '_labels', '_node', '_type', 
                 '_db_file']
    
    def __init__(self, load_name: int|float, db_file: str) -> None:
        """
        """
        self._db_file = db_file
        self._name = load_name
        #self._node = NodeItemSQL(self._name, self._db_file)
        self._node =  NodeLoadSQL(load_name, self._db_file)
        #
        self._labels = []
        self._type = []        
        # create node table
        conn = create_connection(self._db_file)
        with conn:        
            self._create_table(conn)
    #
    def __setitem__(self, node_name: int|str,
                    node_load: list) -> None:
        """
        """
        #1/0
        # get load data
        conn = create_connection(self._db_file)
        with conn:  
            node = check_nodes(conn, node_name)       
        try:
            node_number = node[0]
        except TypeError:
            raise IOError(f"Node {node_name} not found")         
        #
        # set element load        
        #load_type = node_load.pop(0)
        #load_number = next(self.get_number())
        self._labels.append(node_number)
        #
        #
        #bd_file = self._db_file
        #conn = create_connection(bd_file)
        #
        #if re.match(r"\b(point|load|node)\b", load_type, re.IGNORECASE):
        self._node[node_number] = node_load
        
        #elif re.match(r"\b(mass)\b", load_type, re.IGNORECASE):
        #    raise NotImplementedError(f'node mass')
        
        #elif re.match(r"\b(disp(lacement)?)\b", load_type, re.IGNORECASE):
        #    raise NotImplementedError(f'node displacement')
        
        #else:
        #    raise IOError(f'node load type {load_type} not recognized')        
    #
    def __getitem__(self, node_name:int|str) :
        """
        """
        # get load data
        conn = create_connection(self._db_file)
        with conn:  
            node = check_nodes(conn, node_name)
        #
        try:
            node_number = node[0]
            if not node_number in self._labels:
                self._labels.append(node_number)
            #
            return self._node(node_number)
            #return self._node[node_number]
        except TypeError:
            raise IOError(f"Node {node_name} not found")         
        #1 / 0
        #try:
        #    index = self._cls._index
        #    load_name = self._cls._labels[index]
        #    #load_title = self._title[index]
        #    bd_file = self._cls.bd_file
        #    #load_number = self._load_number.index(load_name)
        #    conn = create_connection(bd_file)
        #    #load_number = get_basic_load_number(conn, load_name)
        #    # get beam load
        #    with conn:
        #        node_load = self._get_node_load(conn, node_number, load_name)
        #    return node_load
        #except ValueError:
        #    return None
    #
    #
    #
    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> int:
        return len(self._labels)

    def __iter__(self):
        """
        """
        items = list(dict.fromkeys(self._labels))
        return iter(items)

    #
    def __str__(self, units: str = "si") -> str:
        """ """
        output = ""
        output += self._node.__str__()
        return output
    #    
    #
    #@property
    #def load(self):
    #    """
    #    """
    #    return self._node._load    
    #     
    #
    def _create_table(self, conn) -> None:
        """ """
        table_node_load = "CREATE TABLE IF NOT EXISTS tb_LoadNode(\
                            number INTEGER PRIMARY KEY NOT NULL,\
                            load_number INTEGER NOT NULL REFERENCES tb_Load(number),\
                            element_number INTEGER NOT NULL REFERENCES tb_Elements(number),\
                            node_number INTEGER NOT NULL REFERENCES tb_Nodes(number),\
                            title TEXT,\
                            system TEXT,\
                            type TEXT NOT NULL,\
                            fx DECIMAL,\
                            fy DECIMAL,\
                            fz DECIMAL,\
                            mx DECIMAL,\
                            my DECIMAL,\
                            mz DECIMAL,\
                            x DECIMAL,\
                            y DECIMAL,\
                            z DECIMAL,\
                            rx DECIMAL,\
                            ry DECIMAL,\
                            rz DECIMAL);"
        #
        create_table(conn, table_node_load)
    #
    def get_group_nodes(self):
        """
        """
        items = []
        set_items = set(self._labels)
        index = self._cls._index
        load_tile = self._cls._title[index]
        load_name = self._cls._labels[index]
        try:
            self._load_number.index(load_name)
            bd_file = self._cls.bd_file
            conn = create_connection(bd_file)
            load_number = get_basic_load_number(conn, load_name)
            for node_number in set_items:
                with conn:
                    sum_load = self.get_sum_column(conn, node_number, load_number)
                    items.append(PointNode(*sum_load, node_number,
                                            load_tile, 0, 0))
            return items
        except ValueError:
            return items
    #
    def get_sum_column(self, conn, node_name, load_number):
        """ """
        1 / 0
        project = (node_name, load_number,)
        sql = 'SELECT SUM(fx), SUM(fy), SUM(fz), SUM(mx), SUM(my), SUM(mz)\
               FROM tb_LoadNode WHERE node_number = ? AND load_number = ?'
        cur = conn.cursor()
        cur.execute( sql, project )
        record = cur.fetchone()
        return record
    #
    def update(self, other) -> None:
        """
        """
        pl = other._point
        try:
            index = pl._index
            #node_name = pl._labels[index]
            for x, node_name in enumerate(pl._labels):
                point_load = [pl._fx[x], pl._fy[x], pl._fz[x],
                              pl._mx[x], pl._my[x], pl._mz[x],
                              pl._title[x]]
                self.__setitem__(node_name, point_load)
        except AttributeError:
            pass        
        #print('----')
    #
    #
    #
    @property
    def df(self):
        """nodes in dataframe format"""
        db = DBframework()
        conn = create_connection(self._db_file)
        #
        with conn:
            cur = conn.cursor()
            cur.execute("SELECT tb_Load.*, \
                        tb_Nodes.name, \
                        tb_LoadNode.title, tb_LoadNode.system, tb_LoadNode.type, \
                        tb_LoadNode.fx, tb_LoadNode.fy, tb_LoadNode.fz, \
                        tb_LoadNode.mx, tb_LoadNode.my, tb_LoadNode.mz, \
                        tb_LoadNode.x, tb_LoadNode.y, tb_LoadNode.z, \
                        tb_LoadNode.rx, tb_LoadNode.ry, tb_LoadNode.rz \
                        FROM tb_Load, tb_Nodes, tb_LoadNode\
                        WHERE tb_LoadNode.load_number = tb_Load.number\
                        AND tb_LoadNode.node_number = tb_Nodes.number \
                        AND tb_Load.name = {:};".format(self._name))
            rows = cur.fetchall()
        #
        cols = ['load_number','load_name', 'load_title', 'load_type',
                'node_name',
                'load_comment', 'load_system', 'type', 
                'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz',
                'x', 'y', 'z', 'rx', 'ry', 'rz']
        df = db.DataFrame(data=rows, columns=cols)
        #df = db.read_sql_query("SELECT * FROM tb_LoadNode", conn)
        df = df[['load_name', 'load_type', 'load_number', 'load_system', 'load_comment',
                 'node_name', 'type', 'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz',
                 'x', 'y', 'z', 'rx', 'ry', 'rz']]
        return df
    
    @df.setter
    def df(self, value):
        """ """
        print('node load')
        1 / 0
    #     
#
#
# ---------------------------------
#
#
def get_basic_load_number(conn, load_name:int):
    """ """
    cur = conn.cursor()
    cur.execute("SELECT * FROM tb_Load\
                 WHERE name = {:} \
                 AND type = 'basic'".format(load_name))
    loads = cur.fetchone()
    return loads[0]
#
#
#
class NodeItemSQL(NodeItem):
    __slots__ = ['_load', '_displacement', '_mass', '_node_id',
                 '_bd_file']

    def __init__(self, load_name: str, bd_file: str):
        """
        """
        super().__init__()
        self._bd_file =  bd_file
        self._load = NodeLoadSQL(load_name, "load", self._bd_file)
        self._displacement = NodeLoadSQL(load_name, "displacement", self._bd_file)
        self._mass = NodeLoadSQL(load_name, "mass", self._bd_file)
    # 
#
#
class NodeLoadSQL(NodeLoadBasic):
    __slots__ = ['_labels', '_title', '_complex', 
                 '_system_flag', '_system', 
                 '_bd_file', '_name']
    
    def __init__(self, load_name: str,  bd_file: str) -> None:
        """
        """
        super().__init__()
        self._name = load_name
        self._bd_file = bd_file
    #
    #
    def __setitem__(self, node_name:int|str,
                    point_load: list) -> None:
        """
        """
        # get load data
        conn = create_connection(self._bd_file)
        with conn:  
            node = check_nodes(conn, node_name)
        #
        try:
            node_number = node[0]           
        except TypeError:
            raise IOError(f"Node {node_name} not found")         
        #
        self._labels.append(node_name)
        load_type = point_load.pop(0)
        #
        if isinstance(point_load, dict):
            point_load = get_nodal_load(point_load)
            title = point_load[-1]
            point_load.pop()
        elif isinstance(point_load[-1], str):
            title = point_load.pop()
            point_load = get_nodal_load(point_load)
        else:
            title = "NULL"
            point_load = get_nodal_load(point_load)
        #
        # Push to database
        #
        #if re.match(r"\b(point|load|node)\b", self._type, re.IGNORECASE):
        with conn:
            self._push_load(conn=conn,
                            node_number=node_number,
                            load_title=title,
                            node_load=point_load,
                            load_type=load_type)
        
        #elif re.match(r"\b(mass)\b", self._type, re.IGNORECASE):
        #    raise NotImplementedError(f'node mass')
        #
        #elif re.match(r"\b(disp(lacement)?)\b", self._type, re.IGNORECASE):
        #    raise NotImplementedError(f'node displacement')
        #
        #else:
        #    raise IOError(f'node load type {self._type} not recognized')    
    #
    def __getitem__(self, node_name:int|str) -> list:
        """
        """
        # get load data
        conn = create_connection(self._bd_file)
        #
        # Get database
        #
        #if re.match(r"\b(point|load|node)\b", self._type, re.IGNORECASE):
        with conn:
            nload = self._get_load(conn, node_name)
        
        #elif re.match(r"\b(mass)\b", self._type, re.IGNORECASE):
        #    raise NotImplementedError(f'node mass')
        #
        #elif re.match(r"\b(disp(lacement)?)\b", self._type, re.IGNORECASE):
        #    raise NotImplementedError(f'node displacement')
        #
        #else:
        #    raise IOError(f'node load type {self._type} not recognized')         
        #
        #
        #_points: list = []
        #for _index in _index_list:
        #    _points.append(PointNode(self._fx[_index], self._fy[_index], self._fz[_index],
        #                             self._mx[_index], self._my[_index], self._mz[_index],
        #                             self._labels[_index], self._title[_index],
        #                             self._system[_index], self._complex[_index], self._type))
        #
        return nload
    #
    #
    def _push_load(self, conn, node_number: int,
                   load_title: str, node_load: list[float],
                   load_type: str, system: str = "global"):
        """
        """
        #print("-->")
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]        
        #
        if re.match(r"\b(point|load|node)\b", load_type, re.IGNORECASE):
            project = (load_number, 'NULL', node_number,
                       load_title, system, load_type,
                       *node_load,
                       'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL')

        elif re.match(r"\b(mass)\b", load_type, re.IGNORECASE):
            project = (load_number, 'NULL', node_number,
                       load_title, system, load_type,
                       'NULL', 'NULL', 'NULL',
                       *node_load,
                       'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL')

        elif re.match(r"\b(disp(lacement)?)\b", load_type, re.IGNORECASE):
            project = (load_number, 'NULL', node_number,
                       load_title, system, load_type,
                       'NULL', 'NULL', 'NULL', 'NULL', 'NULL', 'NULL',
                       *node_load)

        else:
            raise IOError(f'node load type {load_type} not recognized')
        #
        # print('-->')
        #
        sql = 'INSERT INTO tb_LoadNode(load_number, element_number, node_number, \
                                        title, system, type, \
                                        fx, fy, fz, mx, my, mz,\
                                        x, y, z, rx, ry, rz)\
                                    VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
        #print('-->')
    #
    #
    def _get_load(self, conn, node_name:int|str):
        """ """
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]
        #
        node = check_nodes(conn, node_name)
        try:
            node_number = node[0]
        except TypeError:
            raise IOError(f"Node {node_name} not found")
        # Node load
        cur = conn.cursor()
        cur.execute("SELECT * FROM tb_LoadNode \
                     WHERE load_number = {:} \
                     AND node_number = {:}; "
                    .format(load_number, node_number))
        rows = cur.fetchall()
        node_load = []
        for row in rows:
            if row[6] in ['load', 'point', 'node']:
                data = [*row[7:13],                      # [fx, fy, fz, mx, my, mz]
                        node_name, row[4], self._name,   # [name, title, load_name]
                        0, 0, 'load']                    # [system, load_complex, load_type]
                #
                # [fx, fy, fz, mx, my, mz, name, title, load_name, system, load_complex, load_type]
                node_load.append(PointNode._make(data))
            else:
                1 / 0
        return node_load
    #
    #
    # -----------------------------------------
    #
    #
    def __call__(self, node_id):
        self._node_id = node_id
        return self    
    #
    #
    @property
    def load(self):
        """
        """
        1 / 0
        try:
            point_id = self._node_id
            return self._load[point_id]
        except :
            raise IndexError

    @load.setter
    def load(self, node_load: list):
        """
        Point Load
        """
        node_name = self._node_id
        #if isinstance(values, dict):
        #    self._load[node_name] = values
        #elif isinstance(values[0], list):
        #    for value in values:
        #        self._load[node_name] = value
        #else:
        #    self._load[node_name] = values
        node_load.insert(0, 'load')
        self.__setitem__(node_name, node_load)
    #    
    #
    #

    
#