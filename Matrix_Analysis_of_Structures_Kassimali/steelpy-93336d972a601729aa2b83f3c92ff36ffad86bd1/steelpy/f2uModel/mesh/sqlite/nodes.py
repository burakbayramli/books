# Copyright (c) 2009 steelpy

# Python stdlib imports
from __future__ import annotations
#from array import array
from collections.abc import Mapping
from math import isclose, dist
from typing import NamedTuple
import re

# package imports
#from steelpy.process.units.main import Units
# steelpy.f2uModel.mesh
from steelpy.f2uModel.mesh.process.nodes import (check_point_list, check_point_dic, 
                                                 get_coordinate_system, NodeBasic)
# steelpy
from steelpy.f2uModel.mesh.sqlite.process_sql import create_connection, create_table
from steelpy.utils.dataframe.main import DBframework


#
class NodeSQL(NodeBasic):
    """
    steelpy model node class


    Parameters
    ----------
    boundaries: object
        f2u boundary object


    Attributes
    ----------
    _labels : array
        node internal number
    _x : array
        coordinate x
    _y : array
        coordinate y
    _z : array
        coordinate y
    _sets : List[tuple]
        set with node/element
    """
    __slots__ = ['_system', 'db_file', '_labels']

    def __init__(self, db_file: str,
                 db_system:str="sqlite",
                 system:str = 'cartesian') -> None:
        """
        """
        super().__init__(system)
        self.db_file = db_file
        # create node table
        conn = create_connection(self.db_file)
        with conn:
            self._create_table(conn)
        #print('--> update labels?')
        #1 / 0
    #
    # ---------------------------------
    #
    def __setitem__(self, node_name: int|str,
                    coordinates: list[float]|dict[str, float]) -> None:
        """
        """
        try:
            self._labels.index(node_name)
            raise Exception('    *** warning point {:} already exist'
                            .format(node_name))
        except ValueError:
            coordinates = self._get_coordinates(coordinates)
            self._labels.append(node_name)
            #
            conn = create_connection(self.db_file)
            with conn:
                self._push_node(conn, node_name, coordinates)
                #conn.commit()
    #
    def __getitem__(self, node_name: int|str) -> tuple:
        """
        node_name : node number
        """
        try:
            self._labels.index(node_name)
            conn = create_connection(self.db_file)
            node = get_node(conn, node_name)
            return node
        except ValueError:
            raise IndexError('   *** node {:} does not exist'.format(node_name))    
    #
    # ------------------
    # SQL ops
    # ------------------
    #
    def _create_table(self, conn) -> None:
        """ """
        # conn = create_connection(self.db_file)
        _table_nodes = "CREATE TABLE IF NOT EXISTS tb_Nodes (\
                        number INTEGER PRIMARY KEY NOT NULL,\
                        name NOT NULL,\
                        type TEXT NOT NULL,\
                        x DECIMAL NOT NULL,\
                        y DECIMAL NOT NULL,\
                        z DECIMAL NOT NULL,\
                        r DECIMAL,\
                        theta DECIMAL,\
                        phi DECIMAL,\
                        idx INTEGER NOT NULL);"
        #
        create_table(conn, _table_nodes)
        
    #
    def _push_node(self, conn, node_name, coordinates):
        """
        Create a new project into the projects table
        """
        # get row number
        cur = conn.cursor()
        sql = 'SELECT max(number) from tb_Nodes;'
        if (idx := max(cur.execute(sql))[0]) == None:
            idx = 0
        #
        if self.system == 'cylindrical': # z, r, theta,
            project = (node_name, self.system,
                       None, None, *coordinates, None, idx)
        
        elif self.system == 'spherical': # r, theta, phi
            project = (node_name, self.system,
                       None, None, None, *coordinates, idx)
        
        else:
            project = (node_name, self.system,
                       *coordinates, None, None, None, idx)
        #
        sql = 'INSERT INTO tb_Nodes(name, type,\
                                    x, y, z, r, theta, phi, idx)\
                                    VALUES(?,?,?,?,?,?,?,?,?)'
        # push
        cur = conn.cursor()
        cur.execute(sql, project)
    #
    def _iscloseSQL(self, key:str, item:str, value:float,
                    rel_tol:float=1e-9, abs_tol:float=0.0)-> tuple:
        """ """
        sql = 'SELECT {:} FROM tb_Nodes WHERE ABS({:} - {:}) <= {:} * MAX(ABS({:}), ABS({:}), {:})'\
              .format(key, item, value, rel_tol, item, value, abs_tol)
        conn = create_connection(self.db_file)
        cur = conn.cursor()
        cur.execute(sql)
        records = cur.fetchall()
        return records
    #
    def _update_item(self, conn, name, item, value):
        """ """
        project = (value, name)
        sql = 'UPDATE tb_Nodes SET {:} = ? WHERE name = ?'.format(item)
        cur = conn.cursor()
        cur.execute(sql, project)
    #
    def _orderby(self):
        """
        """
        #print('--')
        conn = create_connection(self.db_file)
        sql = 'SELECT * FROM tb_Nodes ORDER BY number ASC'
        cur = conn.cursor()
        cur.execute(sql)
        conn.commit()
        conn.close()
    #
    # ------------------
    # ops
    # ------------------
    #
    def get_point_name(self, coordinates,
                       tol:float=0.01, rel_tol:float=1e-6) -> int:
        """
        tol: absolte tolerance in metres (0.010 m default)
        """
        # check if point already exist
        #try:
        #    #system = self.system # get_coordinate_system(self.system)
        #    #if isinstance(coordinates, system):
        #    return coordinates.name
        #except AttributeError:
        # get index of x coord location in existing database
        coord = self._get_coordinates(coordinates)
        #
        items = self._iscloseSQL(key='*', item='x', value=coord[0],
                                 abs_tol=tol, rel_tol=rel_tol)
        # check if y and z coord match
        if items:
            for item in items:
                if isclose(coord[1], item[4], abs_tol=tol, rel_tol=rel_tol):
                    if isclose(coord[2], item[5], abs_tol=tol, rel_tol=rel_tol):
                        return item[1]
        raise IOError('   error coordinate not found')
    #
    #
    def renumbering(self, new_numbers:list[int]):
        """ """
        indexes = [self._labels.index(node_name) 
                   for node_name in new_numbers]
        indexes = [(val, j + 1, ) for j, val in enumerate(indexes)]
        conn = create_connection(self.db_file)
        with conn:
            update_colum(conn, colname='idx', newcol=indexes)
            #
            #nodes = get_nodes(conn)
            #nindex = [item[1] for item in nodes]
            #nodes = [nodes[indx][1:] for indx in indexes]
            #update_table(conn, nodes)
            #conn.commit()
        #print('-->?')
    #
    #def update_number(self, node_name:int, value:Union[float,int]):
    #    """ """
    #    conn = create_connection(self.db_file)
    #    with conn:
    #        nodes = get_nodes(conn)
    #        nindex = [item[1] for item in nodes]
    #        row = nodes.pop(nindex.index(node_name))
    #        nodes.insert(value-1, row)
    #        update_table(conn, nodes)
    #        conn.commit()
    #
    def _get_maxmin(self):
        """
        """
        #
        def maxmin(head: str, col: str):
            #
            sql = 'SELECT {:}({:}) FROM tb_Nodes'.format(head.upper(), col)
            cur = conn.cursor()
            cur.execute(sql)
            record = cur.fetchone()
            return record[0]
        #
        conn = create_connection(self.db_file)
        with conn:
            max_x = maxmin(head='max', col='x')
            min_x = maxmin(head='min', col='x')
            #
            max_y = maxmin(head='max', col='y')
            min_y = maxmin(head='min', col='y')
            #
            max_z = maxmin(head='max', col='z')
            min_z = maxmin(head='min', col='z')
        return [max_x, max_y, max_z], [min_x, min_y, min_z]
    #
    #
    @property
    def df(self):
        """get node dataframe"""
        #print('nodes df in')
        db = DBframework()
        conn = create_connection(self.db_file)
        nodes = db.read_sql_query("SELECT * FROM tb_Nodes", conn)
        nodes.drop(columns=['number', 'idx'], inplace=True)
        return nodes
    
    @df.setter
    def df(self, df):
        """ """
        columns = list(df.columns)
        header = {}
        for key in columns:
            if re.match(r"\b(id|name|node(s)?)\b", key, re.IGNORECASE):
                header[key] = 'name'
            
            elif re.match(r"\b(x(\_|\-)?(coord(inate)?(s)?)?)\b", key, re.IGNORECASE):
                header[key] = 'x'
                try:
                    df[key] = df[key].apply(lambda x: x.value)
                except AttributeError:
                    pass
            
            elif re.match(r"\b(y(\_|\-)?(coord(inate)?(s)?)?)\b", key, re.IGNORECASE):
                header[key] = 'y'
                try:
                    df[key] = df[key].apply(lambda x: x.value)
                except AttributeError:
                    pass                
            
            elif re.match(r"\b(z(\_|\-)?(coord(inate)?(s)?)?)\b", key, re.IGNORECASE):
                header[key] = 'z'
                try:
                    df[key] = df[key].apply(lambda x: x.value)
                except AttributeError:
                    pass                
            
            #elif re.match(r"\b(title)\b", key, re.IGNORECASE):
            #    #header.append(key)
            #    header[key] = 'title'
        #
        #df.rename(columns=header, inplace=True)
        #
        nodes = df[header.keys()].copy()
        #nodes[["x", "y", "z"]] = df[coord].values.tolist()
        nodes['type'] = self._system
        nodes[['r', 'theta', 'phi']] = None
        #
        nodes.rename(columns=header, inplace=True)
        #
        #for row in nodes.itertuples():
        #    coordinates = self._get_coordinates([])
        #    self._labels.append(node_name)
        #
        conn = create_connection(self.db_file)
        #
        # get row number
        cur = conn.cursor()
        sql = 'SELECT max(number) from tb_Nodes;'
        if (idx := max(cur.execute(sql))[0]) == None:
            idx = 0
        #
        nodes['idx'] = [item + idx for item in list(nodes.index)]
        #
        with conn:
            nodes.to_sql('tb_Nodes', conn,
                         index_label=['name', 'type',
                                      'x', 'y', 'z', 'r',
                                      'theta', 'phi', 'idx'], 
                         if_exists='append', index=False)
        #
        #
        self._labels.extend(nodes['name'].tolist())
        #
#
#
#
def get_node(conn, node_name:int|str, item:str='*'):
    """ """
    with conn:
        data = get_item_table(conn, node_name, item)
    system = get_coordinate_system(data[2])
    return system(x=data[3], y=data[4], z=data[5],
                  name=node_name, number=data[0], 
                  index=data[0]-1)
    #return value
#
def get_item_table(conn, node_name, item):
    """ """
    project = (node_name,)
    sql = 'SELECT {:} FROM tb_Nodes WHERE name = ?'.format(item)
    cur = conn.cursor()
    cur.execute(sql, project)
    record = cur.fetchone()
    return record
#
def get_nodes(conn):
    """ """
    sql = 'SELECT * FROM tb_Nodes ORDER BY number ASC'
    cur = conn.cursor()
    cur.execute(sql)
    record = cur.fetchall()
    return record
#
#def update_table(conn, nodes):
#    """ """
#    # drop table
#    sql = 'DROP TABLE tb_Nodes'
#    cur = conn.cursor()
#    cur.execute(sql)
#    #
#    new_node_table(conn)
#    push_nodes(conn, nodes)
#
#
#def push_nodes(conn, nodes):
#    """
#    Create a new project into the projects table
#    :param conn:
#    :param project:
#
#    :return: project id
#    """
#    project = nodes
#    #project = [item[1:] for item in nodes]
#    # number = len(self._labels) - 1
#    #if csystem == 'cylindrical':  # z, r, theta,
#    #    project = (node_name, csystem,
#    #               None, None, *coordinates, None)
#    #
#    #elif csystem == 'spherical':  # r, theta, phi
#    #    project = (node_name, csystem,
#    #               None, None, None, *coordinates)
#    #
#    #else:
#    #    project = (node_name, csystem,
#    #               *coordinates, None, None, None)
#    #
#    sql = 'INSERT INTO tb_Nodes(name, type,\
#                                x, y, z, r, theta, phi)\
#                                VALUES(?,?,?,?,?,?,?,?)'
#    cur = conn.cursor()
#    cur.executemany(sql, project)
#
#
def update_colum(conn, colname: str, newcol: list):
    """update entire column values"""
    sql = f'UPDATE tb_Nodes SET {colname} = ? WHERE rowId = ?'
    cur = conn.cursor()
    cur.executemany(sql, newcol)
#    
#
#