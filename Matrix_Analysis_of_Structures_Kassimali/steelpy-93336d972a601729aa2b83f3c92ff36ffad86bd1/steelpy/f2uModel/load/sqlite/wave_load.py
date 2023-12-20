#
# Copyright (c) 2009 steelpy
# 

# Python stdlib imports
from __future__ import annotations
#from array import array
#from collections.abc import Mapping
#from collections import defaultdict
#from dataclasses import dataclass
from typing import NamedTuple
#import re
#from operator import itemgetter
#from itertools import groupby

# package imports
# steelpy.f2uModel
#from steelpy.f2uModel.load.process.actions import SelfWeight
#from steelpy.f2uModel.load.process.basic_load import BasicLoadBasic, LoadTypeBasic
from steelpy.f2uModel.load.sqlite.beam import BeamToNodeSQL #, BeamLoadItemSQL 
#from steelpy.f2uModel.load.sqlite.node import  NodeLoadItemSQL
from steelpy.utils.math.operations import linspace
#
# steelpy.f2uModel.load
from steelpy.f2uModel.load.process.wave_load import WaveLoadItem
from steelpy.utils.math.operations import trnsload
#
from steelpy.f2uModel.mesh.sqlite.beam import BeamItemSQL
# steelpy.f2uModel
from steelpy.f2uModel.mesh.sqlite.process_sql import (create_connection, create_table,
                                                       get_load_data, check_element)
#
from steelpy.trave.beam.load.beam import LineBeam #, BeamLoad
#
#import numpy as np
import pandas as pd
from steelpy.utils.dataframe.main import DBframework
#
#
#
#
#
#@dataclass
class WaveLoadItemSQL(WaveLoadItem):
    """ """
    __slots__ = ['_seastate','_db_file', '_name',
                 '_load', '_criterion', '_plane']

    def __init__(self, load_name: int|str, plane: NamedTuple, db_file:str):
        """ """
        super().__init__(load_name=load_name)
        #
        self._plane = plane
        self._db_file = db_file
        #
        self._node_eq = BeamToNodeSQL(load_name=load_name, 
                                      db_file=self._db_file)
        #
        conn = create_connection(self._db_file)
        with conn:        
            self._create_table(conn)
            self._create_node_table(conn)
    #
    #
    #
    #
    def _create_table(self, conn) -> None:
        """ """
        _table_element_line = "CREATE TABLE IF NOT EXISTS tb_WaveLoad(\
                                number INTEGER PRIMARY KEY NOT NULL,\
                                load_number INTEGER NOT NULL REFERENCES tb_Load(number),\
                                element_number INTEGER NOT NULL REFERENCES tb_Elements(number),\
                                title TEXT,\
                                system INTEGER NOT NULL,\
                                L0 DECIMAL,\
                                qx0 DECIMAL,\
                                qy0 DECIMAL,\
                                qz0 DECIMAL,\
                                qx0i DECIMAL,\
                                qy0i DECIMAL,\
                                qz0i DECIMAL,\
                                L1 DECIMAL,\
                                qx1 DECIMAL,\
                                qy1 DECIMAL,\
                                qz1 DECIMAL,\
                                qx1i DECIMAL,\
                                qy1i DECIMAL,\
                                qz1i DECIMAL,\
                                BS DECIMAL,\
                                OTM DECIMAL,\
                                x DECIMAL,\
                                y DECIMAL,\
                                z DECIMAL);"
        #
        create_table(conn, _table_element_line)
    #
    def _push_load(self, conn, load: list):
        """ """
        #
        sql = 'INSERT INTO tb_WaveLoad(load_number, element_number,\
                                        title, system,\
                                        L0, qx0, qy0, qz0, qx0i, qy0i, qz0i,\
                                        L1, qx1, qy1, qz1, qx1i, qy1i, qz1i,\
                                        BS, OTM,\
                                        x, y, z)\
                                        VALUES(?,?,?,?,\
                                               ?,?,?,?,?,?,?,\
                                               ?,?,?,?,?,?,?,\
                                               ?,?,?,?,?)'
        cur = conn.cursor()
        cur.executemany(sql, load)
    #
    def get_wave_load(self, conn, load_name:int):
        """ """
        db = DBframework()      
        # get beam data
        #beam = check_element(conn, beam_name)
        #beam_number = beam[0]
        # beam line load
        load_data = get_load_data(conn, self._name, load_type='basic')
        load_number = load_data[0]
        #
        cur = conn.cursor()
        cur.execute("SELECT tb_Load.name, tb_Elements.name, tb_WaveLoad.*\
                    FROM tb_WaveLoad, tb_Elements, tb_Load\
                    WHERE tb_WaveLoad.load_number = {:}\
                    AND tb_WaveLoad.element_number = tb_Elements.number\
                    AND tb_WaveLoad.load_number = tb_Load.number;"
                    .format(load_number))
        rows = cur.fetchall()
        #
        cols = ['load_name', 'element_name', 'number', 'load_number', 'element_number', 
                'load_comment', 'load_system',
                'L0', 'qx0', 'qy0', 'qz0', 'qx0i', 'qy0i', 'qz0i',
                'L1', 'qx1', 'qy1', 'qz1', 'qx1i', 'qy1i', 'qz1i',
                'BS', 'OTM',
                'x', 'y', 'z']        
        df = db.DataFrame(data=rows, columns=cols)
        df = df[['load_name', 'load_number', 
                 'load_comment', 'load_system',
                 'element_name', 'element_number', 
                 'L0', 'qx0', 'qy0', 'qz0',
                 'L1', 'qx1', 'qy1', 'qz1',
                 'BS', 'OTM',
                 'x', 'y', 'z']]
        return df
    #
    def get_elements(self,conn):
        """ """
        cur = conn.cursor()
        cur.execute ("SELECT * FROM tb_Elements;")
        row = cur.fetchall()
        return row
    #
    #
    #
    # -----------------------------------------------
    #
    #
    def _create_node_table(self, conn) -> None:
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
    #
    def _push_node_load(self, conn,
                        node_load:list[float]):
        """ """
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
    #
    # -----------------------------------------------
    #
    def process(self, load_name: int | str):
        """ """
        try:
            index = self._labels.index(load_name)
        except ValueError:
            raise KeyError('Invalid load name : {:}'.format(load_name))        
        print('# Calculating wave beam forces')
        #index = self._labels.index(load_name)
        design_load = self._design_load[index]
        seastate = self._seastate[index]
        #wave = self.__getitem__(load_name)
        sname = f'{seastate.name}_{seastate.title}'
        sload = seastate.load()
        #
        df_bload = self.get_beam_load(sname, sload)
        #
        #grpbeam = df_bload.groupby(['element_type', 'element_name'])
        #
        self.df(data=df_bload)
        #
        print('# End process')
    #
    #
    def get_beam_load(self, wname, wload):
        """ """
        #
        conn = create_connection(self._db_file)
        with conn:
            labels = self.get_elements(conn)
        #
        df_bload = None
        for idx, key in enumerate(labels):
            beam = BeamItemSQL(key[1],
                               plane=self._plane,
                               db_file=self._db_file)
            Fwave = wload.Fwave(beam=beam)
            df_load = Fwave.df
            df_load['load_name'] = self._name
            df_load['load_title'] = wname
            #df_load['load_title'] = df_load.apply(lambda row: f"{wname}_{round(row.x, 2)}_{round(row.y, 2)}_{round(row.z, 2)}", axis=1)
            df_load['load_system'] = 'local'
            try:
                1/idx
                df_bload = pd.concat([df_bload, df_load], ignore_index=True)
            except ZeroDivisionError:
                df_bload = df_load
            #
            # process to select wave point based on user request
            #
            #Fx, Fy, OTM = udl.span_loading()
            #indmax = Fx.argmax(dim='length').values
            #vmax = Fx.idxmax(dim='length').values
            #print('')
            #print('Total combined force [kN-m]')
            #print(f'Fx ={np.max(Fx) / 1000: 1.3e}, Fy={np.max(Fy) / 1000: 1.3e}, OTM={np.max(OTM)/1000: 1.3e}')
            #print('---')
            #
            #Fx.sel(x=0).plot.line(hue='z')
            #plt.show()            
            #
        #1 / 0
        return df_bload
    #
    # -----------------------------------------------
    #
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
            line['load_number'] = load_data[0]
            line['beam_number'] = beam_data[0]
            line['qx0i'] = 'NULL'
            line['qy0i'] = 'NULL'
            line['qz0i'] = 'NULL'
            line['qx1i'] = 'NULL'
            line['qy1i'] = 'NULL'
            line['qz1i'] = 'NULL'
            #
            line = line[['load_number', 'beam_number',
                         'load_title', 'load_system', 
                         'L0', 'qx0', 'qy0', 'qz0', 'qx0i', 'qy0i', 'qz0i',
                         'L1', 'qx1', 'qy1', 'qz1', 'qx1i', 'qy1i', 'qz1i',
                         'BS', 'OTM', 
                         'x', 'y', 'z']].values
            #line
            with conn:
                self._push_load(conn, load=line)
        #print('===')
        #1/0
    #
    #
    def fer(self):
        """ Return Fix End Reactions (FER) global system"""
        #beams = self._f2u_beams
        conn = create_connection(self._db_file)       
        for load_name in set(self._labels):
            with conn: 
                bldf = self.get_wave_load(conn, load_name=load_name)
            #
            # ------------------------------------------
            # TODO : select coordinate for design load
            #
            waveinput = self.__getitem__(load_name=load_name)
            design_load = waveinput.design_load.split("_")
            criteria = waveinput.criterion
            #
            value_type = design_load[0]
            load_type = design_load[1]
            #
            #
            #grpm = bldf.groupby(['element_name','y'])
            grpwave = bldf.groupby(['element_name','y'])[['BS', 'OTM']].sum()
            #
            if criteria == 'global':
                # FIXME
                1 / 0
            else: # default 
                grpm = grpwave[load_type].abs().groupby('element_name')
            #
            if value_type.lower() == 'max':
                idx = grpm.idxmax()
            else:
                1 / 0
            #
            bldf.set_index(['element_name','y'], inplace=True)
            grpm = bldf.loc[idx]
            #
            #
            # ----------------------------------------
            #
            ipart = ['NULL', 'NULL', 'NULL','NULL', 'NULL', 'NULL']
            res = []
            for item in grpm.itertuples():
                element_name = item.Index[0]
                beam =  BeamItemSQL(element_name,
                                    plane=self._plane, 
                                    db_file=self._db_file)
                node1, node2 = beam.nodes
                #
                #for item in items.itertuples():
                #
                data = [item.qx0, item.qy0, item.qz0,
                        item.qx1, item.qy1, item.qz1,
                        item.L0, item.L1,
                        element_name, item.load_comment,
                        item.load_name, item.load_system,
                        1, 'Line Load']
                load = LineBeam._make(data)
                gnload = load.fer_beam(L=beam.L)
                # load local system to global 
                gnload = [*gnload[4], *gnload[5]]
                lnload = trnsload(gnload, beam.T3D())
                #
                res.extend([[item.load_number, item.load_comment, 
                             'global', beam.number,
                             node1.number, *lnload[:6], *ipart],
                            [item.load_number, item.load_comment, 
                             'global', beam.number,
                             node2.number, *lnload[6:], *ipart]])
                #
                #
                #self._load.df(data=items)
                #res = self._load(beam=beam).fer()
                #
                #1 / 0
                #for gnload in res:
                #    self._node_eq[key[0]] = [[end_nodes[0], *gnload[4], gnload[1], gnload[2]],
                #                             [end_nodes[1], *gnload[5], gnload[1], gnload[2]]]
                #
            #
            with conn: 
                self._push_node_load(conn, res)
            #
            #print('---')
        #print('--> get_end_forces')
        #1 / 0    
    #
    #
    # -----------------------------------------------
    #
    def beam_load(self, steps:int = 10):
        """ """
        conn = create_connection(self._db_file)
        #beamfun = defaultdict(list)
        beamfun = []
        for load_name in set(self._labels):
            with conn: 
                bldf = self.get_wave_load(conn, load_name=load_name)
            #
            # TODO : select coordinate for design load
            #
            grpm = bldf.groupby(['element_name'])
            #
            for key, items in grpm:
                beam = BeamItemSQL(key[0],
                                   plane=self._plane, 
                                   db_file=self._db_file)
                mat = beam.material
                sec = beam.section.properties()
                Lsteps = linspace(start=0, stop=beam.L, num=steps+1, endpoint=True)
                #
                for item in items.itertuples():
                    data = [item.qx0, item.qy0, item.qz0,
                            item.qx1, item.qy1, item.qz1,
                            item.L0, item.L1,
                            item.element_name, item.load_comment,
                            item.load_name, item.load_system,
                            1, 'Line Load']
                    bitem = LineBeam._make(data)
                    lout = bitem.Fx(x=Lsteps, L=beam.L,
                                    E=mat.E, G=mat.G, 
                                    Iy=sec.Iy, Iz=sec.Iy,
                                    J=sec.J, Cw=sec.Cw, Area=sec.area)
                    #beamfun[key[0]].extend(lout)
                    beamfun.extend(lout)
                #
        #print('---')
        return beamfun
    #
    #
#