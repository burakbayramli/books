# 
# Copyright (c) 2019-2023 steelpy
#
# 
# Python stdlib imports
from __future__ import annotations
#from array import array
from collections.abc import Mapping
#from collections import defaultdict
from collections import Counter, defaultdict
#from dataclasses import dataclass
from typing import NamedTuple
#from math import prod

# package imports
from steelpy.f2uModel.load.sqlite.basic_load import BasicLoadSQL
# steelpy.f2uModel.load
from ..process.operations import duplicates, indices
from ..process.combination import LoadCombinationBasic
# steelpy
from steelpy.f2uModel.mesh.sqlite.process_sql import create_connection
from steelpy.utils.dataframe.main import DBframework
#
#
class LoadCombSQL(LoadCombinationBasic):
    
    __slots__ = ['_labels', '_title', '_number', 'bd_file', '_plane', 
                 '_index', '_basic', '_combination', '_metocean']
    #
    def __init__(self, bd_file:str, plane: NamedTuple):
        """
        """
        super().__init__()
        #
        self.bd_file = bd_file
        self._plane=plane
        #
        self._basic = BasicLoadSQL(db_file=self.bd_file,
                                   plane=self._plane)
        self._combination = {} #LoadCombinationSQL(self)
    #
    def __setitem__(self, load_name:int|str, load_title:str) -> None:
        """
        """
        load_name = str(load_name)
        try:
            self._labels.index(load_name)
            raise Exception('    *** warning load combination name {:} already exist'
                            .format(load_name))
        except ValueError:
            self._labels.append(load_name)
            self._title.append(load_title)
            load_number = next(self.get_number())
            self._number.append(load_number)
            #conn = create_connection(self.bd_file)
            #
            self._combination[load_name] = CombTypeSQL(name=load_name,
                                                       title=load_title,
                                                       bd_file=self.bd_file)
            #
            self._combination[load_name].number = load_number
            #
            #with conn:
            #    load_number = self._push_load_combination(conn, load_name, load_title)
            #    self._number.append(load_number)
            #    conn.commit()
    #
    def __getitem__(self, load_name:str|int):
        """
        """
        load_name = str(load_name)
        try:
            self._index = self._labels.index(load_name)
            #return CombTypeSQL(self)
            return self._combination[load_name]
        except ValueError:
            raise IOError("load combination {:} not defined".format(load_name))
    #
    #
    def to_basic2(self):
        """ """
        #
        db = DBframework()
        dftemp = []
        #
        conn = create_connection(self.bd_file)
        cur = conn.cursor()
        cur.execute ( "SELECT load_number, bl_number, factor\
                      FROM tb_LoadCombIndex\
                      WHERE bl_number IS NOT NULL")
        basic_loads = cur.fetchall()
        # get basic load
        blc = defaultdict(list)
        for basic in basic_loads:
            blc[basic[0]].append([basic[1], basic[2]])
        # get load combination and convert to basic loads
        cur.execute ( "SELECT load_number, lc_number, factor\
                      FROM tb_LoadCombIndex\
                      WHERE lc_number IS NOT NULL")
        comb_loads = cur.fetchall()
        for comb in comb_loads:
            for bl in blc[comb[1]]:
                blc[comb[0]].append([bl[0], bl[1]*comb[2]])
        # organize basic load in user load name
        load_list = get_load_list(conn)
        lnumber = {item[0]:item[2] for item in load_list}
        basic_loads = defaultdict(list)
        for key, item in blc.items():
            lname = lnumber[key]
            tlist = list(zip(*item))
            dup = duplicates(tlist[0])
            dup = indices(tlist[0], dup)
            if dup:
                # duplicates
                for name, load in dup.items():
                    blname = lnumber[name]
                    factor = [tlist[1][index] for index in load]
                    #basic_loads[key].append([name, sum(factor)])
                    basic_loads[ lname ].append( [ blname, sum ( factor ) ] )
                    #dftemp.append([key, item.number, 'combination', item.title, blname, sum(factor)])
                # singles
                sload = set(tlist[0]) - set(dup)
                for name in sload:
                    blname = lnumber[name]
                    index = tlist[0].index(name)
                    #basic_loads[key].append([name, tlist[1][index]])
                    basic_loads[lname].append([blname, tlist[1][index]])
            else:
                #basic_loads[key] = item
                basic_loads[lname] = [[lnumber[_bl[0]], _bl[1]] for _bl in item]
                #dftemp.extend([[key, item.number, 'combination', item.title, lnumber[_bl[0]], _bl[1]]
                #               for _bl in item])
        #
        #header = ['load_name', 'load_number','load_type', 'load_title', 'basic_load', 'factor']
        #dfcomb = db.DataFrame(data=dftemp, columns=header, index=None)
        #return dfcomb #, basic_loads
        return basic_loads
    #
    #def solve_combinations(self, basic_res, memb_force):
    #    """
    #    """
    #    comb_res = {}
    #    memb_comb = {}
    #    bloads = self.to_basic()
    #    for lcomb, comb in bloads.items():
    #        #lcomb = load_combination[cname].title
    #        beam_load = {}
    #        for bname, factor in comb:
    #            try:
    #                comb_res[lcomb] += basic_res[bname] * factor
    #            except KeyError:
    #                comb_res[lcomb] = basic_res[bname] * factor
    #            #
    #            for mname, member in memb_force[bname].items():
    #                try:
    #                    beam_load[mname] += member * factor
    #                except KeyError:
    #                    beam_load[mname] =  member * factor
    #        memb_comb[lcomb] = beam_load
    #    return comb_res, memb_comb
#     
#
class CombTypeSQL:
    """
    """
    __slots__ = ['name', 'number', 'title', 
                 '_basic', '_metocean', '_combination',
                 'bd_file']

    def __init__(self, name:str, title:str, bd_file:str):
        """
        """
        #self._cls = cls
        self.name = name
        self.title = title
        self.bd_file = bd_file
        #
        self._basic = BasicCombSQL(bl_type="basic",
                                   comb_name=self.name, 
                                   bd_file=self.bd_file)
        self._metocean = BasicCombSQL(bl_type="metocean",
                                      comb_name=self.name,
                                      bd_file=self.bd_file)
        self._combination = BasicCombSQL(bl_type="combination",
                                         comb_name=self.name,
                                         bd_file=self.bd_file)        
        #
        conn = create_connection(self.bd_file)
        with conn:
            self._push_load_combination(conn, name, title)
    #
    def _push_load_combination(self, conn, load_name:int, load_title:str):
        """ """
        #
        project = (load_name, load_title, "combination")
        sql = 'INSERT INTO tb_Load(name, title, type) VALUES(?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
        conn.commit() 
        return cur.lastrowid
    #
    @property
    def combination(self):
        """
        """
        return self._combination

    #
    @property
    def basic(self):
        """
        """
        return self._basic
    
    #
    @property
    def metocean(self):
        """
        """
        return self._metocean    
#
#
class BasicCombSQL(Mapping):
    """
    FE Metocean Combination Class
    """
    __slots__ = ['_labels', '_type', '_number', 'bd_file', '_comb_name']

    def __init__(self, bl_type:str, comb_name:str|int, bd_file:str):
        """
        """
        self._comb_name = comb_name
        self._labels: list[str,int] = []
        self._number: list[str,int] = []
        self._type = bl_type
        self.bd_file = bd_file

    #
    def __setitem__(self, load_name:int|str, factor: float) -> None:
        """
        """
        load_name = str(load_name)
        #
        self._labels.append(load_name)
        #self._number.append(comb_name)
        #
        conn = create_connection(self.bd_file)
        with conn:
            if self._type == 'basic':
                try:
                    bl_number = get_basic_number(conn, load_name)
                    lc_number = None
                except TypeError:
                    raise IOError(f' Basic Load {load_name} not found')
                
            elif self._type == 'combination':
                try:
                    bl_number = None
                    lc_number = get_comb_number(conn, load_name)
                except TypeError:
                    raise IOError(f' Load Combination {load_name} not found')            
            else:
                1 / 0
            #
            load_number = get_comb_number(conn, self._comb_name)
            self._push_combination(conn, load_number, bl_number, lc_number, factor)
            
    #
    def __getitem__(self, load_name:int|str):
        """
        """
        load_name = str(load_name)
        try:
            #index = self._cls._index
            comb_name = self._comb_name
            #load_title = self._title[index]
            # load_number = self._load_number.index(load_name)
            bd_file = self.bd_file
            conn = create_connection(bd_file)

            # get beam load
            with conn:
                comb_number = get_comb_number( conn, comb_name )
                if self._type == 'basic':
                    basicn = get_basic_number(conn, load_name)                
                    factor = get_basic_factor(conn, comb_number, basicn)
                
                elif self._type == 'combination':
                    combn = get_comb_number(conn, load_name)                
                    factor = get_comb_factor(conn, comb_number, combn)
                
                else:
                    1 / 0
            return factor
        except TypeError:
            raise KeyError(f"{load_name}")

    #
    def _push_combination(self, conn, load_number:int, bl_name:int,
                          lc_name:int, factor:float):
        """
        """
        #
        #
        project = (load_number, bl_name, lc_name, factor)
        sql = 'INSERT INTO tb_LoadCombIndex(\
               load_number, bl_number, lc_number, factor)\
               VALUES(?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)
        conn.commit()
    #
    @property
    def load_type(self):
        """
        """
        return self._type
    #
    def __len__(self) -> int:
        return len(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels
    #
    def __iter__(self):
        """
        """
        return iter(self._labels)
#
#
class LoadCombinationSQL(Mapping):
    
    __slots__ = ['_cls', '_labels', '_number']

    def __init__(self, cls):
        """
        """
        self._cls = cls
        self._labels: list[str|int] = []
        self._number: list[str|int] = []
    #
    def __setitem__(self, load_name:int, factor: float) -> None:
        """
        """
        index = self._cls._index
        #comb_title = self._cls._title[index]
        comb_name = self._cls._labels[index]
        conn = create_connection(self._cls.bd_file)
        load_number = get_comb_number(conn, comb_name)
        with conn:
            self._push_combination(conn, load_number,
                                   load_name, factor)
            #conn.commit()
        #
        self._labels.append(load_name)
        self._number.append(comb_name)
    #
    def __getitem__(self, load_name:int):
        """
        """
        try:
            index = self._cls._index
            comb_name = self._cls._labels[index]
            bd_file = self._cls.bd_file
            conn = create_connection(bd_file)
            comb_number = get_comb_number(conn, comb_name)
            basic_number = get_comb_number(conn, load_name)
            # get beam load
            with conn:
                factor = self._get_comb_load_factor(conn, comb_number, basic_number)
            return factor
        except TypeError:
            raise KeyError(f"{load_name}")
    #
    def _push_combination(self, conn, load_number:int,
                            lc_number:int, factor:float):
        """
        """
        try:
            comb_number = get_comb_number(conn, lc_number)
        except TypeError:
            raise IOError(f"combination {lc_number} not found")
        #
        project = (load_number,  None, comb_number, factor)
        sql = 'INSERT INTO tb_LoadCombIndex(\
               load_number, bl_number, lc_number, factor)\
               VALUES(?,?,?,?)'
        cur = conn.cursor()
        cur.execute(sql, project)    
    #
    def _get_comb_load_factor(self, conn, load_number, basic_number):
        """ """
        cur = conn.cursor()
        cur.execute("SELECT * FROM tb_LoadCombIndex\
                     WHERE load_number = {:} \
                     AND lc_number = {:}".format(load_number, basic_number))
        loads = cur.fetchone()
        return loads[4]
    #
    def __len__(self) -> int:
        return len(self._labels)
    #
    def __contains__(self, value) -> bool:
        return value in self._labels
    #
    def __iter__(self):
        """
        """
        index = self._cls._index
        comb_name = self._cls._labels[index]
        comb_load = [self._labels[x] for x, item in enumerate(self._number)
                      if item == comb_name]
        #comb = set(self._labels)
        return iter(comb_load)
#   
#
def get_load_list(conn):
    """ """
    cur = conn.cursor()
    cur.execute("SELECT * FROM tb_Load")
    loads = cur.fetchall()
    return loads
#
#
def get_comb_number(conn, load_name:int|str):
    """ """
    cur = conn.cursor()
    cur.execute("SELECT * FROM tb_Load\
                 WHERE name = {:} \
                 AND type = 'combination'".format(load_name))
    loads = cur.fetchone()
    return loads[0]
#
def get_basic_number(conn, load_name:int):
    """ """
    cur = conn.cursor()
    cur.execute("SELECT * FROM tb_Load\
                 WHERE name = {:} \
                 AND type = 'basic'".format(load_name))
    loads = cur.fetchone()
    return loads[0]
#
def get_basic_factor(conn, load_number, basic_number):
    """ """
    cur = conn.cursor ()
    cur.execute ( "SELECT * FROM tb_LoadCombIndex\
                 WHERE load_number = {:} \
                 AND bl_number = {:}".format(load_number, basic_number))
    loads = cur.fetchone ()
    return loads[4]
#
def get_comb_factor(conn, load_number, lc_number):
    """ """
    cur = conn.cursor ()
    cur.execute ( "SELECT * FROM tb_LoadCombIndex\
                 WHERE load_number = {:} \
                 AND lc_number = {:}".format(load_number, lc_number))
    loads = cur.fetchone ()
    return loads[4]