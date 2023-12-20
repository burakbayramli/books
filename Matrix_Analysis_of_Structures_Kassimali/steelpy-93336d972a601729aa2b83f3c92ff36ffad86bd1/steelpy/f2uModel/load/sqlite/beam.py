#
# Copyright (c) 2009-2023 fem2ufo
# 

# Python stdlib imports
from __future__ import annotations
#from dataclasses import dataclass
from typing import NamedTuple
import re


# package imports
from steelpy.f2uModel.mesh.sqlite.process_sql import  check_nodes
# 
from steelpy.trave.beam.load.beam import (LineBeam, PointBeam,
                                          BeamLoadItem, BeamLoad)

from ..process.nodes import NodeLoadBasic, PointNode
from ..process.beam import BeamDistMaster
# steelpy.
from steelpy.f2uModel.mesh.sqlite.process_sql import (create_connection, create_table,
                                                       get_load_data, check_element)
from steelpy.f2uModel.mesh.sqlite.beam import BeamItemSQL
#
from steelpy.utils.dataframe.main import DBframework
#
# ---------------------------------
#
#
#
class BeamLoadItemSQL(BeamLoadItem):
    __slots__ = ['_labels','_name',  '_load', '_plane', 
                 '_plane', '_node_eq', '_db_file']

    def __init__(self, load_name: int|float,
                 plane: NamedTuple, db_file: str) -> None:
        """
        """
        super().__init__()
        #
        self._name = load_name
        self._db_file = db_file
        self._plane = plane
        #
        self._load = BeamLoadSQL(load_name=self._name,
                                 bd_file=self._db_file)
        #
        self._node_eq = BeamToNodeSQL(load_name=self._name, 
                                      db_file=self._db_file)
        #
        
    #
    def __setitem__(self, beam_name: int|str,
                    beam_load: list) -> None:
        """
        """
        conn = create_connection(self._db_file)
        with conn:
            beam =  BeamItemSQL(beam_name,
                                plane=self._plane, 
                                db_file=self._db_file)
            #beam = check_element(conn, beam_name)        
        try:
            beam.L
        except (TypeError, IndexError):
            raise IOError(f"beam {beam_name} not found")        
        #
        # TODO: check if _beam_id affects something else
        #self._beam_id = beam_name
        self._labels.append(beam_name)
        #
        if re.match(r"\b(point|node|mass)\b", str(beam_load[0]), re.IGNORECASE):
            self._load(beam).point =  beam_load[1:]
            #self._load._point[beam_name] = beam_load[1:]
            
        elif re.match(r"\b(line|udl)\b", str(beam_load[0]), re.IGNORECASE):
            self._load(beam).line = beam_load[1:]
            #self._load.line[beam_name] = beam_load[1:]
            
        else:
            raise IOError(f'Beam lod type {beam_load[0]} not implemented')

    #
    def __getitem__(self, beam_name: int | str):
        """
        """
        conn = create_connection(self._db_file)
        with conn:  
            #beam = check_element(conn, beam_name)
            beam =  BeamItemSQL(beam_name=beam_name,
                                plane=self._plane,
                                db_file=self._db_file)
        try:
            memb_type = beam.type # beam[3]
            if memb_type != 'beam':
                raise ValueError(f"element {beam_name} type {memb_type} not valid")
        except TypeError:
            raise IOError(f"beam {beam_name} not found")
        #
        #if not beam_name in self._labels:
        #if not beam_name in  self._labels:
        self._labels.append(beam_name)
        
        return self._load(beam=beam)
    #
    #
    def fer(self, beams):
        """ Return Fix End Reactions (FER) global system"""
        conn = create_connection(self._db_file)
        with conn:
            load_data = get_load_data(conn, self._name, load_type='basic')
            load_number = load_data[0]
        #
        ipart = ['NULL', 'NULL', 'NULL','NULL', 'NULL', 'NULL']
        res =[]
        res1 = self._load.fer(beams)
        for gnload in res1:
            try:
                1 / gnload[2]
                raise RuntimeError('node load in local system')
            except ZeroDivisionError:
                load_system = 'global'
                #
            res.extend([[load_number, gnload[1], load_system, gnload[3], gnload[4], *gnload[5], *ipart],
                        [load_number, gnload[1], load_system, gnload[3], gnload[6], *gnload[7], *ipart]])
        #print('--> get_end_forces')
        if res:
            with conn:  
                self._node_eq._push_node_load(conn, res)
    #
    #
    @property
    def df(self):
        """ beam load df"""
        print(' beam load')
        1 / 0
    
    @df.setter
    def df(self, data):
        """ """
        #
        conn = create_connection(self._db_file)
        #
        beams = data.groupby(['element_type']).get_group('beam')
        grpbeam = beams.groupby(['element_name', 'load_name'])
        #
        for key, item in grpbeam:
            subgrp = item.groupby('load_type')
            line = subgrp.get_group('line')
            #
            beam_name = key[0]
            with conn:  
                beam_data = check_element(conn, beam_name)
                load_data = get_load_data(conn, key[1], load_type='basic')
            #
            #
            #if not beam_name in self._labels:
            self._labels.append(beam_name)           
            #
            line['load_number'] = load_data[0]
            line['beam_number'] = beam_data[0]
            line['BS'] = 'NULL'
            line['OTM'] = 'NULL'
            line['x'] = 'NULL'
            line['y'] = 'NULL'
            line['z'] = 'NULL'
            #line['qz1i'] = 'NULL'
            #
            line = line[['load_number', 'beam_number',
                         'load_title', 'load_system', 
                         'L0', 'qx0', 'qy0', 'qz0',
                         'L1', 'qx1', 'qy1', 'qz1',
                         'BS', 'OTM', 'x', 'y', 'z']].values
            #line
            with conn:
                self._load._line._push_load(conn, beam_name=beam_name, load=line)
        #print('===')
        #1/0
    #    
#
#
class BeamLoadSQL(BeamLoad):
    __slots__ = ['_system_flag', #'_beam_id',
                 '_line', '_point', '_beam']

    def __init__(self, load_name: int|float, bd_file: str): #, beams
        """
        """
        super().__init__()
        self._line = BeamDistributedSQL(load_name=load_name, bd_file=bd_file)
        self._point = BeamPointSQL(load_name=load_name, bd_file=bd_file)
        #self._bd_file = bd_file
    #
    #
    def __call__(self, beam):
        """ """
        #self._beam_id = beam_name
        self._beam = beam # =  BeamItemSQL(beam_name, self._bd_file)
        return self    
    #
#
#    
#
#
#
#
# ---------------------------------
#
#
class BeamDistributedSQL(BeamDistMaster):
    __slots__ = ['_labels', '_title', '_index', '_complex',
                 '_system', '_bd_file'] # '_system_flag', 
    
    def __init__(self, load_name: int|float, bd_file: str) -> None:
        """
        """
        super().__init__()
        self._name = load_name
        self._bd_file =  bd_file
        # create node table
        conn = create_connection(self._bd_file)
        with conn:        
            self._create_table(conn)
    #
    def __setitem__(self, beam_name: int|str, line_load: list) -> None:
        """
        """
        load_source = line_load.pop(0)
        #
        conn = create_connection(self._bd_file)
        with conn:  
            beam = check_element(conn, beam_name)        
        try:
            beam_number = beam[0]
        except TypeError:
            raise IOError(f"beam {beam_name} not found")        
        # get load data
        # set element load
        self._labels.append(beam_name)
        title = line_load.pop()
        self._title.append(title)
        system = line_load.pop() #line_load[8]
        #
        # push to SQL
        bd_file = self._bd_file
        conn = create_connection(bd_file)
        with conn:
            self._push_beam_load(conn, beam_number, title,
                                 system, load_source, line_load)
        #print("-->")
    #
    def __getitem__(self, beam_name:int|str) -> list:
        """
        """
        conn = create_connection(self._bd_file)      
        with conn:
            udl = self._get_beam_load(conn, beam_name=beam_name, load_name=self._name)
        return udl
    #
    #
    #
    #@property
    #def name(self) -> str:
    #    """
    #    """
    #    return self._title[self._index]
    #
    #@name.setter
    #def name(self, load_name:str) -> None:
    #    """
    #    """
    #    index = self._cls._index
    #    load_name = self._cls._labels[index]
    #    bd_file = self._cls.bd_file
    #    conn = create_connection(bd_file)
    #    load_number = get_basic_load_number(conn, load_name)
    #    cur = conn.cursor()
    #    cur.execute("UPDATE tb_LoadBeamLine\
    #                 SET title = {:}\
    #                 WHERE load_number = {:}"
    #                .format(load_name, load_number))
    #    #try:
    #    #    self._title[self._index] = load_name
    #    #except AttributeError:
    #    #    #self.load_name = load_name
    #   #     raise IndexError("load name not found")
    #
    #
    #    
    #
    def _create_table(self, conn) -> None:
        """ """
        _table_element_line = "CREATE TABLE IF NOT EXISTS tb_LoadBeamLine(\
                                number INTEGER PRIMARY KEY NOT NULL,\
                                load_number INTEGER NOT NULL REFERENCES tb_Load(number),\
                                element_number INTEGER NOT NULL REFERENCES tb_Elements(number),\
                                title TEXT,\
                                system INTEGER NOT NULL,\
                                type TEXT NOT NULL,\
                                L0 DECIMAL,\
                                qx0 DECIMAL,\
                                qy0 DECIMAL,\
                                qz0 DECIMAL,\
                                L1 DECIMAL,\
                                qx1 DECIMAL,\
                                qy1 DECIMAL,\
                                qz1 DECIMAL,\
                                BS DECIMAL,\
                                OTM DECIMAL,\
                                x DECIMAL,\
                                y DECIMAL,\
                                z DECIMAL);"
        #
        #bd_file = self._bd_file
        #conn = create_connection(bd_file)
        create_table(conn, _table_element_line)
    #
    def _push_beam_load(self, conn, beam_number:int,
                        load_title:str, load_system:int,
                        load_source: str, 
                        udl:List[float]):
        """ """
        #beam = check_element(conn, beam_name)
        #beam_number = beam[0]        
        #print("-->")
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]
        #
        if load_source in ['wave']:
            raise NotImplemented
        else:
            project = (load_number, beam_number,
                       load_title, load_system,
                       load_source, 
                       udl[6], *udl[:3],
                       udl[7], *udl[3:6],
                       'NULL', 'NULL',
                       'NULL', 'NULL', 'NULL',)
        #
        sql = 'INSERT INTO tb_LoadBeamLine(load_number, element_number,\
                                            title, system, type,\
                                            L0, qx0, qy0, qz0,\
                                            L1, qx1, qy1, qz1, \
                                            BS, OTM, x, y, z)\
                                            VALUES(?,?,?,?,?,\
                                                   ?,?,?,?,?,?,?,\
                                                   ?,?,?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)      
    #
    def _push_load(self, conn, beam_name: str, load: list):
        """ """
        1 / 0
        self._labels.append(beam_name)
        #
        sql = 'INSERT INTO tb_LoadBeamLine(load_number, element_number,\
                                            title, system,\
                                            L0, qx0, qy0, qz0,\
                                            L1, qx1, qy1, qz1,\
                                            BS, OTM, x, y, z)\
                                            VALUES(?,?,?,?,\
                                                   ?,?,?,?,?,?,?,\
                                                   ?,?,?,?,?,?)'
        cur = conn.cursor()
        cur.executemany(sql, load)     
    #
    def _get_beam_load(self, conn, beam_name:int, load_name:int):
        """ """
        # get beam data
        beam = check_element(conn, beam_name)
        beam_number = beam[0]
        # beam line load
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]
        #
        cur = conn.cursor()
        cur.execute("SELECT tb_Load.name, tb_LoadBeamLine.*\
                    FROM tb_LoadBeamLine, tb_Load\
                    WHERE tb_LoadBeamLine.load_number = {:}\
                    AND tb_LoadBeamLine.load_number = tb_Load.number\
                    AND tb_LoadBeamLine.element_number = {:};"
                    .format(load_number, beam_number))
        rows = cur.fetchall()
        beam_line = [] # defaultdict(list)
        for row in rows:
            if row[6] in ['wave']:
                raise NotImplemented
            else:
                data = [*row[8:11], *row[12:15], row[7], row[11],
                        beam_name, row[4], row[0], # name, title, load_name,
                        row[5], 0, "Line Load"]    # system, load_complex, load_type
                beam_line.append(LineBeam._make(data))
        return beam_line
    #
    @property
    def df(self):
        """ """
        db = DBframework()
        conn = create_connection(self._bd_file)
        #
        with conn:
            cur = conn.cursor()
            cur.execute("SELECT tb_Load.*, \
                        tb_Elements.name, \
                        tb_LoadBeamLine.title, tb_LoadBeamLine.system,\
                        tb_LoadBeamLine.L0, tb_LoadBeamLine.qx0, tb_LoadBeamLine.qy0, tb_LoadBeamLine.qz0, \
                        tb_LoadBeamLine.L1, tb_LoadBeamLine.qx1, tb_LoadBeamLine.qy1, tb_LoadBeamLine.qz1 \
                        FROM tb_Load, tb_Elements, tb_LoadBeamLine \
                        WHERE tb_LoadBeamLine.load_number = tb_Load.number\
                        AND tb_LoadBeamLine.element_number = tb_Elements.number \
                        AND tb_Load.name = {:};".format(self._name))            
        rows = cur.fetchall()
        #
        #beam_load = []
        #for row in rows:
        #    data = [*row[:2]]
        #
        cols = ['load_number','load_name', 'load_title', 'load_type',
                'element_name',
                'load_comment', 'load_system',
                'L0', 'qx0', 'qy0', 'qz0',
                'L1', 'qx1', 'qy1', 'qz1']
        df = db.DataFrame(data=rows, columns=cols)
        #
        df = df[['load_name', 'load_type', 'load_number', 'load_system', 'load_comment',
                 'element_name',
                'L0', 'qx0', 'qy0', 'qz0',
                'L1', 'qx1', 'qy1', 'qz1']]
        #       
        #print('--->')
        return df 
    #
    #
#
#
class BeamPointSQL(NodeLoadBasic):
    __slots__ = ['_labels', '_title', '_complex', 
                 '_system_flag', '_system', 
                 '_bd_file', '_name']

    def __init__(self, load_name: int|float, bd_file: str) -> None:
        """
        """
        super().__init__()
        self._name = load_name
        self._bd_file =  bd_file
        # create node table
        conn = create_connection(self._bd_file)
        with conn:        
            self._create_table(conn)
    #
    def __setitem__(self, beam_name: int|str, point_load: list) -> None:
        """
        """
        point_type = point_load.pop(0)
        # get load data
        # set element load
        self._labels.append(beam_name)
        title = point_load.pop()
        self._title.append(title)
        system = point_load.pop() #line_load[8]        
        # 
        # push to SQL
        bd_file = self._bd_file
        conn = create_connection(bd_file)
        with conn:
            self._push_beam_load(conn, beam_name, title,
                                 system, point_type,
                                 point_load)
        # print("-->")
    #
    def __getitem__(self, beam_name:int|str)-> list:
        """
        """
        bd_file = self._bd_file
        # get beam load
        conn = create_connection(bd_file)
        with conn:
            pl = self._get_beam_load(conn, beam_name=beam_name, load_name=self._name)
        return pl
    #
    #
    def _create_table(self, conn) -> None:
        """ """
        _table_element_point = "CREATE TABLE IF NOT EXISTS tb_LoadBeamPoint(\
                                number INTEGER PRIMARY KEY NOT NULL,\
                                load_number INTEGER NOT NULL REFERENCES tb_Load(number),\
                                element_number INTEGER NOT NULL REFERENCES tb_Elements(number),\
                                title TEXT,\
                                system INTEGER NOT NULL,\
                                type TEXT NOT NULL,\
                                L0 DECIMAL,\
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
        create_table(conn, _table_element_point)
    #
    def _push_beam_load(self, conn, beam_name:int|str,
                        load_title: str, load_system: int,
                        point_type: str, 
                        point_load:list[float]):
        """ """
        #print("-->")
        beam = check_element(conn, beam_name)
        beam_number = beam[0]
        #
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]
        #
        #
        if re.match(r"\b(force)\b", point_type, re.IGNORECASE):
            project = (load_number, beam_number,
                       load_title, load_system,
                       point_type, 
                       point_load[6], *point_load[:6],
                       'NULL', 'NULL', 'NULL',
                       'NULL', 'NULL', 'NULL')
        else:
            raise NotImplemented
        #
        sql = 'INSERT INTO tb_LoadBeamPoint(load_number, element_number,\
                                            title, system, type, \
                                            L0, fx, fy, fz, mx, my, mz,\
                                            x, y, z, rx, ry, rz)\
                                            VALUES(?,?,?,?,?,\
                                                   ?,?,?,?,?,?,?,\
                                                   ?,?,?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
    #
    #
    def _get_beam_load(self, conn, beam_name:int, load_name:int):
        """ """
        # get beam data
        beam = check_element(conn, beam_name)
        beam_number = beam[0]
        # beam line load
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]
        #
        # beam line load
        cur = conn.cursor()
        cur.execute("SELECT tb_Load.name, tb_LoadBeamPoint.* \
                    FROM tb_LoadBeamPoint, tb_Load\
                    WHERE tb_LoadBeamPoint.load_number = {:}\
                    AND tb_LoadBeamPoint.load_number = tb_Load.number\
                    AND tb_LoadBeamPoint.element_number = {:};"
                    .format(load_number, beam_number))
        rows = cur.fetchall()
        beam_line = []
        for row in rows:
            if row[6] in ['force']:
                data = [*row[8:14], row[7],
                        beam_name, row[4], row[0],  # name, title, load_name,
                        row[5], 0, "Point Load"]    # system, load_complex, load_type
            beam_line.append(PointBeam._make(data))
        return beam_line
    #
    @property
    def df(self):
        """ """
        db = DBframework()
        conn = create_connection(self._bd_file)
        #
        with conn:
            cur = conn.cursor()
            cur.execute("SELECT tb_Load.*, \
                        tb_Elements.name, \
                        tb_LoadBeamPoint.title, tb_LoadBeamPoint.system,\
                        tb_LoadBeamPoint.L0, tb_LoadBeamPoint.fx, tb_LoadBeamPoint.fy, tb_LoadBeamPoint.fz, \
                        tb_LoadBeamPoint.mx, tb_LoadBeamPoint.my, tb_LoadBeamPoint.mz \
                        FROM tb_Load, tb_Elements, tb_LoadBeamPoint \
                        WHERE tb_LoadBeamPoint.load_number = tb_Load.number\
                        AND tb_LoadBeamPoint.element_number = tb_Elements.number \
                        AND tb_Load.name = {:};".format(self._name))            
        rows = cur.fetchall()
        #
        cols = ['load_number','load_name', 'load_title', 'load_type',
                'element_name',
                'load_comment', 'load_system',
                'L0', 'Fx', 'Fy', 'Fz',
                'Mx', 'My', 'Mz']
        df = db.DataFrame(data=rows, columns=cols)
        #
        df = df[['load_name', 'load_type', 'load_number', 'load_system', 'load_comment',
                 'element_name',
                 'L0', 'Fx', 'Fy', 'Fz',
                 'Mx', 'My', 'Mz']]
        #       
        #print('--->')
        return df
#    
#
class BeamToNodeSQL(NodeLoadBasic):
    __slots__ = ['_labels', '_title', '_complex', 
                '_system_flag', '_system', 
                '_db_file', '_name']
    def __init__(self, load_name: int|float, db_file: str) -> None:
        """
        """
        super().__init__()
        self._name = load_name
        self._db_file =  db_file
        # create node table
        conn = create_connection(self._db_file)
        with conn:        
            self._create_table(conn)
    #
        #
    def __setitem__(self, beam_name: int|str,
                    node_load: list[float]|dict[str,float]) -> None:
        """
        """
        # get load data
        # set element load
        self._labels.append(beam_name)
        #
        #
        # push to SQL
        bd_file = self._db_file
        conn = create_connection(bd_file)
        for node in node_load:
            title = node.pop()
            system = node.pop()
            with conn:
                self._push_beam_load(conn, beam_name, title,
                                     system, node)
        # print("-->")
    #
    def __getitem__(self, beam_name:int|str)-> list:
        """
        """
        bd_file = self._db_file
        # get beam load
        conn = create_connection(bd_file)
        with conn:
            pl = self._get_beam_load(conn, beam_name=beam_name, load_name=self._name)
        return pl
    #
    def _create_table(self, conn) -> None:
        """ """
        _table_element_point = "CREATE TABLE IF NOT EXISTS tb_LoadBeamToNode(\
                                number INTEGER PRIMARY KEY NOT NULL,\
                                load_number INTEGER NOT NULL REFERENCES tb_Load(number),\
                                title TEXT,\
                                system TEXT,\
                                element_number INTEGER NOT NULL REFERENCES tb_Elements(number),\
                                node_number INTEGER NOT NULL REFERENCES tb_Nodes(number),\
                                fx DECIMAL,\
                                fy DECIMAL,\
                                fz DECIMAL,\
                                mx DECIMAL,\
                                my DECIMAL,\
                                mz DECIMAL,\
                                fxi DECIMAL,\
                                fyi DECIMAL,\
                                fzi DECIMAL,\
                                mxi DECIMAL,\
                                myi DECIMAL,\
                                mzi DECIMAL);"
        #
        create_table(conn, _table_element_point)
    #
    def _push_beam_load(self, conn, beam_name:int|str,
                        load_title: str, load_system: int,
                        node_load:list[float]):
        """ """
        #print("-->")
        beam = check_element(conn, beam_name)
        beam_number = beam[0]
        #
        node_name = node_load.pop(0)
        node = check_nodes(conn, node_name)
        node_number = node[0]
        #
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]
        #
        try:
            1 / load_system
            raise RuntimeError('node load in local system')
        except ZeroDivisionError:
            lsystem = 'global'
        #
        project = (load_number, load_title, lsystem,
                   beam_number, node_number,
                   *node_load,
                   'NULL', 'NULL', 'NULL',
                   'NULL', 'NULL', 'NULL')
        #
        sql = 'INSERT INTO tb_LoadBeamToNode(load_number, title, system, \
                                            element_number,\
                                            node_number, fx, fy, fz, mx, my, mz,\
                                            fxi, fyi, fzi, mxi, myi, mzi)\
                                            VALUES(?,?,?,?,?,\
                                                   ?,?,?,?,?,?,?,\
                                                   ?,?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
    #
    def _push_node_load(self, conn, node_load:list):
        """ """
        #
        #project = (load_number, load_title, load_system,
        #           beam_number, node_number,
        #           *node_load,
        #           'NULL', 'NULL', 'NULL',
        #           'NULL', 'NULL', 'NULL')
        #
        sql = 'INSERT INTO tb_LoadBeamToNode(load_number, title, system, \
                                            element_number,\
                                            node_number, fx, fy, fz, mx, my, mz,\
                                            fxi, fyi, fzi, mxi, myi, mzi)\
                                            VALUES(?,?,?,?,?,\
                                                   ?,?,?,?,?,?,?,\
                                                   ?,?,?,?,?)'
        cur = conn.cursor()
        cur.executemany(sql, node_load)
    #
    def _get_beam_load(self, conn, beam_name:int, load_name:int):
        """ """
        # get beam data
        beam = check_element(conn, beam_name)
        beam_number = beam[0]
        # beam line load
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]
        #
        # beam line load
        cur = conn.cursor()
        cur.execute("SELECT tb_Load.title, tb_LoadBeamToNode.* \
                    FROM tb_LoadBeamPoint, tb_LoadBeamLine, tb_LoadBeamToNode, tb_Load\
                    WHERE tb_LoadBeamToNode.load_number = {:}\
                    AND tb_LoadBeamToNode.load_number = tb_Load.number\
                    AND tb_LoadBeamToNode.element_number = {:};"
                    .format(load_number, beam_number))
        rows = cur.fetchall()
        node_load = []
        1/0
        for row in rows:
            #data = [*row[7:10], *row[14:17], row[6], row[13],
            #        node_number, row[2], *row[4:6]]
            data = [*row[5:11],
                    #*row[2:4],
                    load_number, self._name, 
                    0, 0, self._type]
            node_load.append(PointNode._make(data))
        return node_load
    #
    @property
    def df(self):
        """nodes in dataframe format"""
        db = DBframework()
        conn = create_connection(self._db_file)
        #
        with conn:
            cur = conn.cursor()
            cur.execute("SELECT tb_Load.name, tb_Load.type, \
                        tb_Nodes.name, tb_Elements.name, \
                        tb_LoadBeamToNode.title, tb_LoadBeamToNode.system, \
                        tb_LoadBeamToNode.fx, tb_LoadBeamToNode.fy, tb_LoadBeamToNode.fz, \
                        tb_LoadBeamToNode.mx, tb_LoadBeamToNode.my, tb_LoadBeamToNode.mz \
                        FROM tb_Load, tb_Nodes, tb_Elements, tb_LoadBeamToNode\
                        WHERE tb_LoadBeamToNode.load_number = tb_Load.number\
                        AND tb_LoadBeamToNode.node_number = tb_Nodes.number \
                        AND tb_LoadBeamToNode.element_number = tb_Elements.number \
                        AND tb_Load.name = {:};".format(self._name))
            rows = cur.fetchall()
        #
        cols = ['load_name', 'load_type', 'node_name', 'element_name', 
                'load_comment', 'load_system',
                'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz']
        df = db.DataFrame(data=rows, columns=cols)
        #df = db.read_sql_query("SELECT * FROM tb_LoadNode", conn)
        df = df[['load_name', 'load_type', 'load_comment', 'load_system',
                 'element_name', 'node_name',
                 'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz']]
        return df    
#
#

