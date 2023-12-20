#
# Copyright (c) 2009-2023 fem2ufo
# 

# Python stdlib imports
from __future__ import annotations
#from array import array
#from collections.abc import Mapping
#from collections import defaultdict
#from dataclasses import dataclass
#from typing import NamedTuple
#import re
#from operator import itemgetter
#from itertools import groupby

# package imports
from steelpy.f2uModel.load.process.wave_load import WaveLoadItem
from steelpy.utils.math.operations import linspace
#
# steelpy.f2uModel.load
from .beam import BeamToNodeIM
from steelpy.utils.math.operations import trnsload
#
from steelpy.trave.beam.load.beam import LineBeam #, BeamLoad
#
import pandas as pd
from steelpy.utils.dataframe.main import DBframework
#
#
#
#
#@dataclass
class WaveLoadItemIM(WaveLoadItem):
    """ """
    __slots__ = ['_seastate', '_name', '_load', '_criterion']

    def __init__(self, load_name: int|str):
        """ """
        super().__init__(load_name=load_name)
        #
        self._node_eq = BeamToNodeIM(load_name=load_name)
        #
    #
    #
    #
    #
    def get_wave_load(self, conn, load_name:int):
        """ """
        db = DBframework()      
        # get beam data
        #beam = check_element(conn, beam_name)
        #beam_number = beam[0]
        # beam line load
        #
        cols = ['load_name', 'element_name', 'number', 'load_number', 'element_number', 
                'load_comment', 'load_system',
                'L_end1', 'qx1', 'qy1', 'qz1', 'qx1i', 'qy1i', 'qz1i',
                'L_end2', 'qx2', 'qy2', 'qz2', 'qx2i', 'qy2i', 'qz2i',
                'x', 'y', 'z']        
        df = db.DataFrame(data=rows, columns=cols)
        df = df[['load_name', 'load_number', 
                 'load_comment', 'load_system',
                 'element_name', 'element_number', 
                 'L_end1', 'qx1', 'qy1', 'qz1',
                 'L_end2', 'qx2', 'qy2', 'qz2',
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
        conn = create_connection(self._bd_file)
        with conn:
            labels = self.get_elements(conn)
        #
        df_bload = None
        for idx, key in enumerate(labels):
            beam = BeamItemSQL(key[1], self._bd_file)
            udl = wload.wave_force(beam=beam)
            df_load = udl.line()
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
        return df_bload        
    #
    # -----------------------------------------------
    #
    def df(self, data):
        """ """
        #
        conn = create_connection(self._bd_file)
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
        #conn = create_connection(self._bd_file)       
        for load_name in set(self._labels):
            #with conn: 
            #    bldf = self.get_wave_load(conn, load_name=load_name)
            #
            # TODO : select coordinate for design load
            #
            grpm = bldf.groupby(['element_name'])
            #
            ipart = ['NULL', 'NULL', 'NULL','NULL', 'NULL', 'NULL']
            res = []
            for key, items in grpm:
                beam =  BeamItemSQL(key[0], self._bd_file)
                node1,node2 = beam.nodes
                #
                for item in items.itertuples():
                    #
                    data = [item.qx1, item.qy1, item.qz1,
                            item.qx2, item.qy2, item.qz2,
                            item.L_end1, item.L_end2,
                            item.element_name, item.load_comment,
                            item.load_name, item.load_system,
                            1, 'Line Load']
                    load = LineBeam._make(data)
                    gnload = load.fer_beam(L=beam.L)
                    # load local system to global 
                    gnload = [*gnload[4], *gnload[5]]
                    lnload = trnsload(gnload, beam.T())
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
        conn = create_connection(self._bd_file)
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
                beam = BeamItemSQL(key[0], self._bd_file)
                mat = beam.material
                sec = beam.section.properties()
                Lsteps = linspace(start=0, stop=beam.L, num=steps+1, endpoint=True)
                #
                for item in items.itertuples():
                    data = [item.qx1, item.qy1, item.qz1,
                            item.qx2, item.qy2, item.qz2,
                            item.L_end1, item.L_end2,
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